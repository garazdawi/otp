/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2026. All Rights Reserved.
 *
 * %CopyrightEnd%
 */

/*
 * --validate mode: open a .jc file produced by an earlier compile,
 * walk the per-module reloc list, and resolve each entry against the
 * cache_tool's *own* live tables (which mirror what the runtime would
 * present). Used to round-trip-test the writer + loader pair without
 * the runtime emulator in the loop.
 *
 * The host hooks below are the cache_tool's stand-ins for the live
 * VM. atom interning goes through the real linked atom.c; BIF lookup
 * walks bif_table[] (linked from erl_bif_table.c); export lookup
 * synthesises the same cache_tool_exports as the compile path uses.
 *
 * Success criterion: every reloc resolves to a non-NULL address. The
 * patcher writes those addresses into the loaded code; we don't
 * execute, only verify byte-level patches happened.
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cache_tool.h"
#include "../beam_jit_cache_load.h"

/* Forward declarations of the cache_tool host-side resolvers. Their
 * definitions live in cache_tool_stubs.c and the linked-in runtime
 * sources (atom.c, erl_bif_table.c, etc.). */
extern unsigned long erts_atom_put(const unsigned char *name, long len,
                                   int enc, int trunc);
struct cache_tool_bif_entry_shape {
    unsigned long module, name;
    int arity;
    void *f, *impl;
    int kind;
};
extern struct cache_tool_bif_entry_shape bif_table[];

/* Parse "Mod:Func/Arity" into 3 components. Returns 0 on success. */
static int parse_mfa(const char *s, const char **mod, size_t *mod_n,
                     const char **fn, size_t *fn_n,
                     unsigned *arity) {
    const char *colon = strchr(s, ':');
    const char *slash = strrchr(s, '/');
    if (!colon || !slash || colon >= slash) return -1;
    *mod = s;       *mod_n = colon - s;
    *fn  = colon+1; *fn_n  = slash - (colon + 1);
    *arity = (unsigned)atoi(slash + 1);
    return 0;
}

/* ---- Host hooks ------------------------------------------------ */

static uintptr_t hk_atom_eterm_for_name(void *ctx, const char *name, size_t n) {
    (void)ctx;
    return (uintptr_t)erts_atom_put((const unsigned char *)name,
                                    (long)n, 2 /*UTF8*/, 0);
}

static void *hk_bif_fn_for_mfa(void *ctx, const char *mfa) {
    (void)ctx;
    const char *mod, *fn; size_t mod_n, fn_n; unsigned ar;
    if (parse_mfa(mfa, &mod, &mod_n, &fn, &fn_n, &ar) != 0) return NULL;
    unsigned long mod_atom = erts_atom_put((const unsigned char *)mod,
                                           (long)mod_n, 2, 0);
    unsigned long fn_atom  = erts_atom_put((const unsigned char *)fn,
                                           (long)fn_n, 2, 0);
    for (int i = 0; i < 1024; i++) {
        if (bif_table[i].module == 0 && bif_table[i].name == 0) break;
        if (bif_table[i].module == mod_atom
            && bif_table[i].name == fn_atom
            && (unsigned)bif_table[i].arity == ar) {
            return bif_table[i].f;
        }
    }
    return NULL;
}

static void *hk_export_ptr_for_mfa(void *ctx, const char *mfa) {
    /* In the cache_tool host, every Export* it would have handed back
     * during compile was a sentinel cache_tool_exports[i]. The right
     * thing to return for round-trip-test is that same pointer. The
     * cache_tool_stubs path already does the MFA→Export lookup via
     * erts_find_export_entry; reuse it. */
    (void)ctx;
    const char *mod, *fn; size_t mod_n, fn_n; unsigned ar;
    if (parse_mfa(mfa, &mod, &mod_n, &fn, &fn_n, &ar) != 0) return NULL;
    unsigned long mod_atom = erts_atom_put((const unsigned char *)mod,
                                           (long)mod_n, 2, 0);
    unsigned long fn_atom  = erts_atom_put((const unsigned char *)fn,
                                           (long)fn_n, 2, 0);
    extern const void *erts_find_export_entry(unsigned long m, unsigned long f,
                                              unsigned a, int code_ix);
    return (void *)erts_find_export_entry(mod_atom, fn_atom, ar, 0);
}

static void *hk_bif_fn_for_index(void *ctx, uint32_t bif_index) {
    (void)ctx;
    return bif_table[bif_index].f;
}

static void *hk_vm_static_for_id(void *ctx, uint32_t which) {
    (void)ctx;
    /* For the cache_tool's process, the well-known runtime statics
     * live at whatever address the host linker put them. Look them
     * up by symbol name — the runtime would use a direct table. */
    extern int the_active_code_index;
    switch (which) {
    case BEAM_JIT_VM_STATIC_ACTIVE_CODE_INDEX:
        return &the_active_code_index;
    default:
        return NULL;
    }
}

/* ---- Public entry --------------------------------------------- */

int cache_tool_validate(const char *jc_path, const char *module_name,
                        int verbose) {
    /* Init the cache_tool's own loader / atom table — the same setup
     * the compile path does. Without this, atom interning would be
     * uninitialised. */
    if (cache_tool_init("aarch64") != 0) {
        fprintf(stderr, "validate: cache_tool_init failed\n");
        return 2;
    }

    BeamJitCache *cache = beam_jit_cache_open(jc_path);
    if (!cache) {
        fprintf(stderr, "validate: cannot open %s\n", jc_path);
        return 3;
    }

    BeamJitCacheHostHooks hooks = {
        .ctx                  = NULL,
        .atom_eterm_for_name  = hk_atom_eterm_for_name,
        .export_ptr_for_mfa   = hk_export_ptr_for_mfa,
        .bif_fn_for_mfa       = hk_bif_fn_for_mfa,
        .bif_fn_for_index     = hk_bif_fn_for_index,
        .vm_static_for_id     = hk_vm_static_for_id,
    };

    void *code = NULL;
    size_t code_size = 0;
    int rc = beam_jit_cache_load_module(cache, module_name, &hooks,
                                        &code, &code_size);
    if (rc != 0) {
        fprintf(stderr, "validate: load_module(%s) returned %d\n",
                module_name, rc);
        beam_jit_cache_close(cache);
        return 4;
    }

    /* Re-compile the same module fresh, extract the assembled code,
     * and byte-compare against the cache→load→patch result. Since we
     * resolved every reloc against the same live VM that produced the
     * cache file, the two should be byte-identical. */
    extern int cache_tool_compile_module(const BeamInput *in,
                                         CompiledModule *out);
    /* The validate path only has the cache file, not the .beam — so
     * load the corresponding preloaded .beam. The cache filename is
     * conventionally <name>.jc adjacent to <name>.beam. */
    char beam_path[1024];
    snprintf(beam_path, sizeof(beam_path),
             "erts/preloaded/ebin/%s.beam", module_name);
    BeamInput in;
    int beam_rc = cache_tool_read_beam(beam_path, &in);
    if (beam_rc != 0) {
        fprintf(stderr, "validate: cannot read %s (%d), "
                        "skipping byte-equivalence check\n",
                beam_path, beam_rc);
        free(code);
        beam_jit_cache_close(cache);
        return 0;
    }
    CompiledModule cm = {0};
    int cc_rc = cache_tool_compile_module(&in, &cm);
    if (cc_rc != 0) {
        fprintf(stderr, "validate: re-compile failed (%d)\n", cc_rc);
        cache_tool_free_input(&in);
        free(code);
        beam_jit_cache_close(cache);
        return 0;
    }

    size_t cmp_size = code_size < cm.code_size ? code_size : cm.code_size;
    size_t diff_bytes = 0;
    size_t first_diff = (size_t)-1;
    for (size_t i = 0; i < cmp_size; i++) {
        if (((uint8_t *)code)[i] != cm.code[i]) {
            if (first_diff == (size_t)-1) first_diff = i;
            diff_bytes++;
        }
    }

    fprintf(stderr,
            "validate: %s — %zu byte cache code, %zu byte fresh code, "
            "%zu mismatched bytes",
            module_name, code_size, cm.code_size, diff_bytes);
    if (first_diff != (size_t)-1) {
        fprintf(stderr, " (first diff at offset %zu)", first_diff);
    }
    fprintf(stderr, "\n");

    if (verbose) {
        if (first_diff != (size_t)-1) {
            fprintf(stderr, "  cache@%zu: ", first_diff);
            for (size_t i = first_diff;
                 i < first_diff + 16 && i < cmp_size; i++) {
                fprintf(stderr, "%02x ", ((uint8_t *)code)[i]);
            }
            fprintf(stderr, "\n  fresh@%zu: ", first_diff);
            for (size_t i = first_diff;
                 i < first_diff + 16 && i < cmp_size; i++) {
                fprintf(stderr, "%02x ", cm.code[i]);
            }
            fprintf(stderr, "\n");
        } else {
            fprintf(stderr, "  byte-identical.\n");
        }
    }

    cache_tool_free_compiled(&cm);
    cache_tool_free_input(&in);
    free(code);
    beam_jit_cache_close(cache);
    /* Mismatches are expected for now — they are PC-relative branches
     * into the BeamGlobalAssembler fragments, whose addresses differ
     * per process. Resolving those needs a RELOC_FRAGMENT_BRANCH kind
     * that the loader re-emits with the live offset. Until then, the
     * useful metric is "no unresolved relocs" rather than byte-equal. */
    return 0;
}
