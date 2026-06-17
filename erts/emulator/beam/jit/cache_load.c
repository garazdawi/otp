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
 * Runtime cache loader: installs a preloaded module directly from
 * its embedded .jc cache, bypassing the per-op transform + asmjit
 * emit path.
 *
 * Stage 1 of this file handles the simplest preloaded modules
 * (prim_eval, etc.): no on_load, no literals, no string table.
 * Stage 2 will extend to literals + strings + the rest.
 *
 * The flow:
 *   1. Caller (erts_preload_module_from_cache) has already parsed
 *      the BEAM into stp->beam and run beam_load_prepare_emit (so
 *      stp->load_hdr holds the prepared header template).
 *   2. We allocate a JIT region the size of the cached code blob,
 *      copy the bytes in, then reconstruct the BeamCodeHeader at
 *      the start (the cache zeroed it).
 *   3. Walk the cache's reloc list, resolving each symbolic ref
 *      against the live VM's atom/export/BIF/fragment tables and
 *      patching the corresponding code slot.
 *   4. Hand back to erts_finish_loading via the usual stp fields.
 */

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>

#define ERTS_WANT_BREAK_HANDLING

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "erl_binary.h"
#include "atom.h"
#include "export.h"
#include "bif.h"
#include "beam_load.h"
#include "beam_file.h"
#include "jit/beam_jit_cache_load.h"
#include "jit/beam_asm.h"

extern int beamasm_install_cache_code(const unsigned char *code,
                                      size_t code_size,
                                      const void **executable_region,
                                      void **writable_region);
extern void *beamasm_fragment_addr_for_name(const char *name);

/* ---- Host hooks against the live VM tables --------------------- */

struct CacheLoadCtx {
    LoaderState *stp;
};

static uintptr_t hk_atom_eterm_for_name(void *ctx, const char *name,
                                        size_t n) {
    (void)ctx;
    return (uintptr_t)erts_atom_put((const byte *)name, (Sint)n,
                                    ERTS_ATOM_ENC_UTF8, 0);
}

/* Parse "Mod:Func/Arity" into atoms + arity. Returns 0 on success. */
static int parse_mfa(const char *s, Eterm *mod, Eterm *fn, unsigned *ar) {
    const char *colon = strchr(s, ':');
    const char *slash = strrchr(s, '/');
    if (!colon || !slash || colon >= slash) return -1;
    *mod = erts_atom_put((const byte *)s, (Sint)(colon - s),
                         ERTS_ATOM_ENC_UTF8, 0);
    *fn  = erts_atom_put((const byte *)(colon + 1),
                         (Sint)(slash - (colon + 1)),
                         ERTS_ATOM_ENC_UTF8, 0);
    *ar  = (unsigned)atoi(slash + 1);
    return 0;
}

static void *hk_export_ptr_for_mfa(void *ctx, const char *mfa) {
    (void)ctx;
    Eterm mod, fn; unsigned ar;
    if (parse_mfa(mfa, &mod, &fn, &ar) != 0) return NULL;
    /* Allocate (or look up) the export entry. */
    return (void *)erts_export_put(mod, fn, ar);
}

extern const struct erts_bif_entry_t {
    Eterm module, name;
    int arity;
    void *f, *impl;
    int kind;
} bif_table_real[]; /* placeholder; we use BifEntry below */

static void *hk_bif_fn_for_mfa(void *ctx, const char *mfa) {
    (void)ctx;
    Eterm mod, fn; unsigned ar;
    if (parse_mfa(mfa, &mod, &fn, &ar) != 0) return NULL;
    extern BifEntry bif_table[];
    for (int i = 0; i < BIF_SIZE; i++) {
        if (bif_table[i].module == mod
            && bif_table[i].name == fn
            && (unsigned)bif_table[i].arity == ar) {
            return (void *)bif_table[i].f;
        }
    }
    return NULL;
}

static void *hk_bif_fn_for_index(void *ctx, uint32_t bif_index) {
    (void)ctx;
    extern BifEntry bif_table[];
    if (bif_index >= BIF_SIZE) return NULL;
    return (void *)bif_table[bif_index].f;
}

static void *hk_fragment_addr_for_name(void *ctx, const char *name) {
    (void)ctx;
    return beamasm_fragment_addr_for_name(name);
}

static uintptr_t hk_literal_eterm_for_index(void *ctx, uint32_t idx_u) {
    /* Stage 1: not implemented. Returning 0 signals "unresolved" to
     * the loader. Modules with literals will fail to fully patch and
     * the caller falls back to the regular path. */
    (void)ctx; (void)idx_u;
    return 0;
}

static const void *hk_byte_ptr_addr(void *ctx, uint32_t offset) {
    /* Stage 1: not implemented (no preloaded module needs BYTE_PTR
     * patches with the current set — prim_eval has zero of them). */
    (void)ctx; (void)offset;
    return NULL;
}

static void *hk_vm_static_for_id(void *ctx, uint32_t which) {
    (void)ctx;
    switch (which) {
    case BEAM_JIT_VM_STATIC_ACTIVE_CODE_INDEX:
        return &the_active_code_index;
    default:
        return NULL;
    }
}

/* Runtime-side static-symbol table (mirrors cache_tool_static_table.c
 * for symbols dlsym can't see). Stage 1: empty. Symbols that need it
 * (apply3_mfa, erts_msgq_*, function-local statics) won't resolve.
 * For prim_eval all 9 RUNTIME_FN symbols are external-linkage so
 * dlsym alone suffices. */
extern void *cache_runtime_static_symbol_addr(const char *name);

static void *hk_runtime_fn_for_symbol(void *ctx, const char *symbol) {
    (void)ctx;
    void *p = dlsym(RTLD_DEFAULT, symbol);
    if (p) return p;
    return cache_runtime_static_symbol_addr(symbol);
}

/* ---- The loader proper ----------------------------------------- */

static uint32_t read_u32_le(const uint8_t *p) {
    return (uint32_t)p[0]
         | ((uint32_t)p[1] << 8)
         | ((uint32_t)p[2] << 16)
         | ((uint32_t)p[3] << 24);
}

int erts_load_cache_module(LoaderState *stp, BeamJitCache *cache,
                           BeamJitCacheModule *m) {
    /* The previously-run erts_prepare_loading already allocated a JIT
     * region via beamasm_codegen and filled it with asmjit-emitted
     * code that's process-specific only at reloc-covered sites.
     * Overlay the cache bytes on top of the same region: asmjit and
     * the cache produce identical byte offsets for everything else
     * (the cache is a recorded asmjit run of the same .beam through
     * the same emitter), so function/label offsets line up.
     *
     * Skip the BeamCodeHeader region — asmjit already populated it
     * with this-process pointers (literal_area, line_table, etc.)
     * that the cache zeroed out. */
    if ((size_t)stp->loaded_size != m->code_size) {
        if (getenv("ERL_CACHE_TRACE")) {
            fprintf(stderr,
                    "  cache load: size mismatch — asmjit %u, cache %zu\n",
                    (unsigned)stp->loaded_size, (size_t)m->code_size);
        }
        const char *dump_mod = getenv("ERL_CACHE_DUMP_RUNTIME");
        if (dump_mod && m->name && strcmp(m->name, dump_mod) == 0) {
            char path[256];
            snprintf(path, sizeof(path), "/tmp/runtime_%s.bin", m->name);
            FILE *f = fopen(path, "wb");
            if (f) {
                fwrite(stp->writable_region, 1, stp->loaded_size, f);
                fclose(f);
                fprintf(stderr,
                        "  wrote runtime asmjit code (%u bytes) to %s\n",
                        (unsigned)stp->loaded_size, path);
            }
        }
        return -1;
    }

    /* Compute the header region size and skip it in the overlay. */
    size_t hdr_bytes = offsetof(BeamCodeHeader, functions)
                       + sizeof(BeamInstr) * stp->codev_size;
    /* Round up to 8 to match the codegen's padding. */
    hdr_bytes = (hdr_bytes + 7) & ~(size_t)7;
    if (hdr_bytes > m->code_size) return -1;

    /* The cache code section starts at offset 0 of m->code, and the
     * asmjit code also has BeamCodeHeader at offset 0 of
     * stp->writable_region. After hdr_bytes, both are the runtime
     * code; overlay the cache code there. */
    uint8_t *code_rw = (uint8_t *)stp->writable_region;
    sys_memcpy(code_rw + hdr_bytes,
               m->code + hdr_bytes,
               m->code_size - hdr_bytes);

    const char *exec_base = (const char *)stp->executable_region;

    /* 3. Walk the reloc list, resolving each via the live-VM hooks
     *    and writing the patched value into the writable region. */
    BeamJitCacheHostHooks hooks = {
        .ctx                     = NULL,
        .atom_eterm_for_name     = hk_atom_eterm_for_name,
        .export_ptr_for_mfa      = hk_export_ptr_for_mfa,
        .bif_fn_for_mfa          = hk_bif_fn_for_mfa,
        .bif_fn_for_index        = hk_bif_fn_for_index,
        .vm_static_for_id        = hk_vm_static_for_id,
        .fragment_addr_for_name  = hk_fragment_addr_for_name,
        .literal_eterm_for_index = hk_literal_eterm_for_index,
        .byte_ptr_addr           = hk_byte_ptr_addr,
        .runtime_fn_for_symbol   = hk_runtime_fn_for_symbol,
    };

    /* Use the same loader walker we wrote for the validator. It
     * expects to call beam_jit_cache_load_module which takes a cache
     * and a module name; we already have the module, so reach into
     * the per-entry loop directly. For now, since reusing that
     * function would re-copy the code, just run the reloc loop here
     * by walking m->relocs[]. */
    size_t patched = 0, unresolved = 0;
    for (size_t i = 0; i < m->reloc_count; i++) {
        const BeamJitReloc *r = &m->relocs[i];
        uintptr_t value = 0;
        int resolved = 0;

        /* Resolve the symbolic ref. Largely mirrors the validator's
         * dispatch in beam_jit_cache_load.c; kept inline here for
         * stage 1. */
        switch (r->kind) {
        case BEAM_JIT_RELOC_ATOM: {
            if (r->symbolic_ref >= m->atom_count) break;
            uint32_t str_idx = read_u32_le(m->atom_indices
                                           + r->symbolic_ref * 4);
            extern uint32_t beam_jit_cache_strtab_count(const BeamJitCache *);
            extern const char *beam_jit_cache_strtab_at(const BeamJitCache *,
                                                        uint32_t);
            if (str_idx >= beam_jit_cache_strtab_count(cache)) break;
            const char *name = beam_jit_cache_strtab_at(cache, str_idx);
            value = hooks.atom_eterm_for_name(hooks.ctx, name, strlen(name));
            resolved = 1;
            break;
        }
        case BEAM_JIT_RELOC_BIF:
        case BEAM_JIT_RELOC_EXPORT: {
            if (r->symbolic_ref >= m->mfa_count) break;
            uint32_t str_idx = read_u32_le(m->mfa_indices
                                           + r->symbolic_ref * 4);
            extern uint32_t beam_jit_cache_strtab_count(const BeamJitCache *);
            extern const char *beam_jit_cache_strtab_at(const BeamJitCache *,
                                                        uint32_t);
            if (str_idx >= beam_jit_cache_strtab_count(cache)) break;
            const char *s = beam_jit_cache_strtab_at(cache, str_idx);
            if (s[0] == '<' && strncmp(s, "<bif:", 5) == 0) {
                uint32_t bif_idx = (uint32_t)atoi(s + 5);
                value = (uintptr_t)hooks.bif_fn_for_index(hooks.ctx, bif_idx);
            } else if (r->kind == BEAM_JIT_RELOC_EXPORT) {
                value = (uintptr_t)hooks.export_ptr_for_mfa(hooks.ctx, s);
            } else {
                value = (uintptr_t)hooks.bif_fn_for_mfa(hooks.ctx, s);
            }
            resolved = 1;
            break;
        }
        case BEAM_JIT_RELOC_FRAGMENT_BRANCH: {
            if (r->symbolic_ref >= m->mfa_count) break;
            uint32_t str_idx = read_u32_le(m->mfa_indices
                                           + r->symbolic_ref * 4);
            extern uint32_t beam_jit_cache_strtab_count(const BeamJitCache *);
            extern const char *beam_jit_cache_strtab_at(const BeamJitCache *,
                                                        uint32_t);
            if (str_idx >= beam_jit_cache_strtab_count(cache)) break;
            const char *s = beam_jit_cache_strtab_at(cache, str_idx);
            if (strncmp(s, "<frag:", 6) != 0) break;
            char name[256];
            size_t nl = strlen(s);
            if (nl < 7 || nl - 6 >= sizeof(name)) break;
            sys_memcpy(name, s + 6, nl - 7);
            name[nl - 7] = 0;
            value = (uintptr_t)hooks.fragment_addr_for_name(hooks.ctx, name);
            resolved = (value != 0);
            break;
        }
        case BEAM_JIT_RELOC_RUNTIME_FN: {
            if (r->symbolic_ref >= m->mfa_count) break;
            uint32_t str_idx = read_u32_le(m->mfa_indices
                                           + r->symbolic_ref * 4);
            extern uint32_t beam_jit_cache_strtab_count(const BeamJitCache *);
            extern const char *beam_jit_cache_strtab_at(const BeamJitCache *,
                                                        uint32_t);
            if (str_idx >= beam_jit_cache_strtab_count(cache)) break;
            const char *s = beam_jit_cache_strtab_at(cache, str_idx);
            if (strncmp(s, "<rfn:", 5) != 0) break;
            char name[256];
            size_t nl = strlen(s);
            if (nl < 6 || nl - 5 >= sizeof(name)) break;
            sys_memcpy(name, s + 5, nl - 6);
            name[nl - 6] = 0;
            value = (uintptr_t)hooks.runtime_fn_for_symbol(hooks.ctx, name);
            resolved = (value != 0);
            if (!resolved && getenv("ERL_CACHE_TRACE")) {
                fprintf(stderr, "    rfn miss: %s\n", name);
            }
            break;
        }
        case BEAM_JIT_RELOC_INTRA_LABEL:
            value = (uintptr_t)exec_base + r->symbolic_ref;
            resolved = 1;
            break;
        case BEAM_JIT_RELOC_VM_STATIC:
            value = (uintptr_t)hooks.vm_static_for_id(hooks.ctx,
                                                     r->symbolic_ref);
            resolved = (value != 0);
            break;
        default:
            break;
        }

        if (!resolved) {
            if (getenv("ERL_CACHE_TRACE")) {
                fprintf(stderr,
                        "    unresolved kind=%u off=%u sym=%u\n",
                        (unsigned)r->kind, r->code_offset, r->symbolic_ref);
            }
            unresolved++;
            continue;
        }

        /* Apply the patch — aarch64 BL re-emit for FRAGMENT_BRANCH
         * width=4, MOVZ+MOVK*N rewrite for the rest. Same as the
         * validator's tail. */
        if (r->kind == BEAM_JIT_RELOC_FRAGMENT_BRANCH
            && r->imm_width == 4
            && r->code_offset + 4 <= m->code_size) {
            uintptr_t pc = (uintptr_t)(exec_base + r->code_offset);
            intptr_t delta = (intptr_t)(value - pc);
            if (delta >= -(intptr_t)(1 << 27)
                && delta < (intptr_t)(1 << 27)
                && (delta & 3) == 0) {
                int32_t imm26 = (int32_t)(delta / 4);
                uint32_t insn = 0x94000000u
                    | ((uint32_t)imm26 & 0x03ffffffu);
                sys_memcpy(code_rw + r->code_offset, &insn, 4);
                patched++;
            } else {
                unresolved++;
            }
            continue;
        }

        if (r->imm_width == 8
            && r->code_offset + 8 <= m->code_size) {
            sys_memcpy(code_rw + r->code_offset, &value, 8);
            patched++;
        } else if (r->imm_width >= 4 && r->imm_width <= 16
                   && (r->imm_width & 3) == 0
                   && r->code_offset + r->imm_width <= m->code_size) {
            uint32_t *insns = (uint32_t *)(code_rw + r->code_offset);
            unsigned n = r->imm_width / 4;
            uint32_t Rd = insns[0] & 0x1f;
            for (unsigned k = 0; k < n; k++) {
                uint16_t slice = (uint16_t)((value >> (16 * k)) & 0xffff);
                uint32_t opcode_top = (k == 0) ? 0xD2800000u
                                               : 0xF2800000u;
                uint32_t shift_field = (uint32_t)k << 21;
                insns[k] = opcode_top | shift_field
                         | ((uint32_t)slice << 5) | Rd;
            }
            patched++;
        } else {
            unresolved++;
        }
    }

    if (getenv("ERL_CACHE_TRACE")) {
        fprintf(stderr,
                "  cache load: %zu relocs total, %zu patched, "
                "%zu unresolved\n",
                m->reloc_count, patched, unresolved);
    }

    if (unresolved > 0) return -2;

    /* Done — stp->executable_region / writable_region / code_hdr
     * were already set up by erts_prepare_loading; we just overlaid
     * the cache bytes on top. erts_finish_loading takes it from here. */
    return 0;
}

/* Lightweight strtab accessors so cache_load.c doesn't need
 * to know BeamJitCache's internals. Implemented in
 * beam_jit_cache_load.c. */
uint32_t beam_jit_cache_strtab_count(const BeamJitCache *c);
const char *beam_jit_cache_strtab_at(const BeamJitCache *c, uint32_t i);
