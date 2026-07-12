/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2026. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */

/*
 * T2-Full tier-2 JIT: retention copy/free/accounting. See t2_retain.h.
 */

#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "erl_alloc.h"
#include "beam_file.h"
#include "module.h"
#include "beam_asm.h" /* erts_jit_t2_force */

#include "t2_retain.h"
#include "t2_pctab.h"

static erts_atomic_t t2_retained_bytes;

int erts_t2_enabled(void) {
    static int enabled = -1;

    /* +JT2enable implies retention (PLAN/T2FULL/08 §5); the flag is
     * parsed before any module loads, so reading it here is stable. */
    if (erts_jit_t2_force) {
        return 1;
    }

    if (enabled < 0) {
        const char *env = getenv("T2_RETAIN");
        enabled = (env != NULL && env[0] == '1') ? 1 : 0;
    }

    return enabled;
}

int erts_t2_tier_enabled(void) {
#if !defined(__aarch64__)
    /* The T2 backend emits on aarch64 only: off it, tier-up could
     * only arm counters and queue work with no possible install
     * (PLAN/T2FULL/16 §5 latent risk). Retention itself stays
     * available everywhere for the arch-independent mid-end
     * (reconstruction/build/isel testing). */
    return 0;
#else
    /* Counter-triggered tier-up is the tier's default mode
     * (PLAN/T2FULL/09 §1): on whenever the tier is enabled, except
     * under +JT2enable, which is the forced compile-everything mode
     * and needs no counters. */
    return erts_t2_enabled() && !erts_jit_t2_force;
#endif
}

Uint32 erts_t2_tier_threshold(void) {
    static Uint32 thr = 0;

    if (thr == 0) {
        const char *env = getenv("T2_TIER_THRESHOLD");

        if (env != NULL && env[0] != '\0') {
            Sint64 v = atoll(env);

            thr = (v <= 0 || v >= (Sint64)ERTS_T2_TIER_PENDING)
                          ? (ERTS_T2_TIER_PENDING - 1)
                          : (Uint32)v;
        } else {
            /* 05 §15.1's base. */
            thr = 1000;
        }
    }
    return thr;
}

/* Was the flat threshold forced with T2_TIER_THRESHOLD? (Testing and
 * the tax legs need determinism; the size term is skipped then.) */
static int t2_tier_threshold_forced(void) {
    static int forced = -1;

    if (forced < 0) {
        const char *env = getenv("T2_TIER_THRESHOLD");
        forced = (env != NULL && env[0] != '\0') ? 1 : 0;
    }
    return forced;
}

/* The per-function trip threshold (05 s15.1): base * sqrt(size + 1),
 * size = the function's generic-op count from the eligibility scan (a
 * stable proxy for the IR-op count the formula names -- the IR does
 * not exist before the first compile). The recompile and
 * cache-pressure terms stay deferred with the recompile machinery
 * (P2 commit 10). */
Uint32 erts_t2_tier_threshold_for(Uint32 size) {
    Uint32 base = erts_t2_tier_threshold();
    Uint32 root;
    Uint64 scaled;

    if (t2_tier_threshold_forced()) {
        return base;
    }

    /* Integer sqrt(size + 1). */
    {
        Uint32 x = size + 1;
        Uint32 r = 0, b = 1u << 15;

        while (b > 0) {
            Uint32 t = r + b;

            if ((Uint64)t * t <= (Uint64)x) {
                r = t;
            }
            b >>= 1;
        }
        root = r == 0 ? 1 : r;
    }

    scaled = (Uint64)base * root;
    if (scaled >= (Uint64)ERTS_T2_TIER_PENDING) {
        scaled = ERTS_T2_TIER_PENDING - 1;
    }
    return (Uint32)scaled;
}

/* Diagnostics on stderr for every retention decision (T2_DEBUG=1). */
static int t2_debug_enabled(void) {
    static int enabled = -1;

    if (enabled < 0) {
        const char *env = getenv("T2_DEBUG");
        enabled = (env != NULL && env[0] == '1') ? 1 : 0;
    }

    return enabled;
}

void erts_t2_init(void) {
    erts_atomic_init_nob(&t2_retained_bytes, 0);
    erts_t2_tier_init();
}

UWord erts_t2_retained_sz(void) {
    return (UWord)erts_atomic_read_nob(&t2_retained_bytes);
}

void erts_t2_account_bytes(Sint delta) {
    erts_atomic_add_nob(&t2_retained_bytes, (erts_aint_t)delta);
}

static size_t align_up(size_t size) {
    return (size + (sizeof(UWord) - 1)) & ~(sizeof(UWord) - 1);
}

ErtsT2RetainedCode *erts_t2_prepare(BeamFile *beam) {
    ErtsT2RetainedCode *ret;
    Uint32 *bitmap;
    Uint32 *installs = NULL;
    Uint32 *loops = NULL;
    Uint32 *arities = NULL;
    Uint32 *sizes = NULL;
    int any_eligible = 0;
    int on_load = 0;

    size_t atoms_size, imports_size, types_size, literals_size, bitmap_size;
    size_t lambdas_size;
    size_t code_size, total;
    size_t offset;
    byte *base;

    arities = erts_alloc(ERTS_ALC_T_T2_CODE,
                         (beam->code.function_count > 0
                                  ? beam->code.function_count
                                  : 1) *
                                 sizeof(Uint32));
    sizes = erts_alloc(ERTS_ALC_T_T2_CODE,
                       (beam->code.function_count > 0
                                ? beam->code.function_count
                                : 1) *
                               sizeof(Uint32));
    bitmap = erts_t2_eligibility_scan(beam,
                                      &any_eligible,
                                      &installs,
                                      &loops,
                                      &on_load,
                                      arities,
                                      sizes);
    if (t2_debug_enabled()) {
        erts_fprintf(stderr,
                     "t2_retain: module=%T functions=%d any_eligible=%d\n",
                     beam->module,
                     beam->code.function_count,
                     any_eligible);
    }
    if (!any_eligible) {
        if (bitmap != NULL) {
            erts_free(ERTS_ALC_T_T2_CODE, bitmap);
        }
        if (installs != NULL) {
            erts_free(ERTS_ALC_T_T2_CODE, installs);
        }
        if (loops != NULL) {
            erts_free(ERTS_ALC_T_T2_CODE, loops);
        }
        erts_free(ERTS_ALC_T_T2_CODE, arities);
        erts_free(ERTS_ALC_T_T2_CODE, sizes);
        return NULL;
    }

    atoms_size = beam->atoms.count * sizeof(Eterm);
    imports_size = beam->imports.count * sizeof(BeamFile_ImportEntry);
    types_size = beam->types.count * sizeof(BeamType);
    literals_size = beam->static_literals.count * sizeof(Eterm);
    bitmap_size = ((beam->code.function_count + 31) / 32) * sizeof(Uint32);
    lambdas_size = beam->lambdas.count * sizeof(ErtsT2Lambda);
    code_size = beam->code.size;

    /* One allocation: header, then the word-aligned tables, then the
     * unaligned code bytes last. */
    total = align_up(sizeof(ErtsT2RetainedCode)) + align_up(atoms_size) +
            align_up(imports_size) + align_up(types_size) +
            align_up(literals_size) + 3 * align_up(bitmap_size) +
            align_up(lambdas_size) + code_size;

    base = erts_alloc(ERTS_ALC_T_T2_CODE, total);
    ret = (ErtsT2RetainedCode *)base;
    sys_memset(ret, 0, sizeof(ErtsT2RetainedCode));

    offset = align_up(sizeof(ErtsT2RetainedCode));

    ret->atoms = (Eterm *)(base + offset);
    ret->atom_count = beam->atoms.count;
    sys_memcpy(ret->atoms, beam->atoms.entries, atoms_size);
    offset += align_up(atoms_size);

    ret->imports = (BeamFile_ImportEntry *)(base + offset);
    ret->import_count = beam->imports.count;
    sys_memcpy(ret->imports, beam->imports.entries, imports_size);
    offset += align_up(imports_size);

    ret->types = (BeamType *)(base + offset);
    ret->type_count = beam->types.count;
    ret->types_fallback = beam->types.fallback;
    sys_memcpy(ret->types, beam->types.entries, types_size);
    offset += align_up(types_size);

    /* Slots only; the values are filled by erts_t2_retain_commit once
     * beamfile_move_literals has produced literal-area terms. */
    ret->literal_map = (Eterm *)(base + offset);
    ret->literal_count = beam->static_literals.count;
    sys_memset(ret->literal_map, 0, literals_size);
    offset += align_up(literals_size);

    ret->eligible_bitmap = (Uint32 *)(base + offset);
    sys_memcpy(ret->eligible_bitmap, bitmap, bitmap_size);
    offset += align_up(bitmap_size);

    ret->install_bitmap = (Uint32 *)(base + offset);
    sys_memcpy(ret->install_bitmap, installs, bitmap_size);
    offset += align_up(bitmap_size);

    ret->loop_bitmap = (Uint32 *)(base + offset);
    sys_memcpy(ret->loop_bitmap, loops, bitmap_size);
    offset += align_up(bitmap_size);

    /* Lambda meta (P2 commit 8): make_fun3 decode + the intrinsics'
     * fun-body resolution. Entry pointers are captured at finalize
     * (erts_t2_retain_lambda_entry). */
    ret->lambdas = NULL;
    ret->lambda_count = beam->lambdas.count;
    if (beam->lambdas.count > 0) {
        Sint32 li;

        ret->lambdas = (ErtsT2Lambda *)(base + offset);
        for (li = 0; li < beam->lambdas.count; li++) {
            const BeamFile_LambdaEntry *le = &beam->lambdas.entries[li];

            ret->lambdas[li].label = le->label;
            ret->lambdas[li].arity = le->arity;
            ret->lambdas[li].num_free = le->num_free;
            ret->lambdas[li].fun_entry = NULL;
        }
    }
    offset += align_up(lambdas_size);

    /* The input binary that code.data points into is only guaranteed
     * to be alive here, during prepare; this copy is the reason this
     * function cannot run at finalize. */
    ret->code = base + offset;
    ret->code_size = beam->code.size;
    ret->function_count = beam->code.function_count;
    ret->label_count = beam->code.label_count;
    ret->max_opcode = beam->code.max_opcode;
    sys_memcpy(ret->code, beam->code.data, code_size);
    offset += code_size;

    ASSERT(offset == total);
    ret->bytes = total;

    /* Tier-up profile block (P2 commit 9): allocated here, before T1
     * codegen, so the profiling sequences can bake record addresses.
     * Only under counter-triggered tier-up (never with +JT2enable's
     * forced compile-at-load, which needs no counters), and only when
     * some installable function has the loop shape (a buildable-only
     * loop would just degrade at isel; never arm it). */
    if (erts_t2_tier_enabled() && !on_load) {
        int any_loop = 0;
        Sint32 i;

        for (i = 0; i < beam->code.function_count; i++) {
            if ((installs[i / 32] & (((Uint32)1) << (i % 32))) &&
                (loops[i / 32] & (((Uint32)1) << (i % 32)))) {
                any_loop = 1;
                break;
            }
        }

        if (any_loop) {
            size_t psize = (size_t)beam->code.function_count *
                           ERTS_T2_PROFILE_STRIDE;
            byte *pbase = erts_alloc(ERTS_ALC_T_T2_CODE, psize);

            sys_memset(pbase, 0, psize);
            for (i = 0; i < beam->code.function_count; i++) {
                ErtsT2Profile *rec =
                        (ErtsT2Profile *)(pbase +
                                          (size_t)i *
                                                  ERTS_T2_PROFILE_STRIDE);

                rec->fn_index = (Uint32)i;
                rec->arity = arities[i];
                rec->module = beam->module;
                if ((installs[i / 32] & (((Uint32)1) << (i % 32))) &&
                    (loops[i / 32] & (((Uint32)1) << (i % 32)))) {
                    rec->threshold =
                            erts_t2_tier_threshold_for(sizes[i]);
                }
            }
            ret->profiles = (ErtsT2Profile *)pbase;
            ret->bytes += psize;
        }
    }

    erts_free(ERTS_ALC_T_T2_CODE, bitmap);
    erts_free(ERTS_ALC_T_T2_CODE, installs);
    erts_free(ERTS_ALC_T_T2_CODE, loops);
    erts_free(ERTS_ALC_T_T2_CODE, arities);
    erts_free(ERTS_ALC_T_T2_CODE, sizes);

    return ret;
}

void erts_t2_retain_lambda_entry(ErtsT2RetainedCode *ret,
                                 int i,
                                 const void *fun_entry) {
    if (ret == NULL || ret->lambdas == NULL || i < 0 ||
        i >= ret->lambda_count) {
        return;
    }
    ret->lambdas[i].fun_entry = fun_entry;
}

void erts_t2_retain_commit(ErtsT2RetainedCode *ret,
                           BeamFile *beam,
                           struct erl_module_instance *inst_p) {
    Sint32 i;

    ASSERT(inst_p->t2_retained == NULL);
    ASSERT(ret->literal_count == beam->static_literals.count);

    /* beamfile_move_literals has run: entry values are literal-area
     * terms that stay valid until the module instance is purged. */
    for (i = 0; i < ret->literal_count; i++) {
        ASSERT(beam->static_literals.entries[i].heap_fragments == NULL);
        ret->literal_map[i] = beam->static_literals.entries[i].value;
    }

    inst_p->t2_retained = ret;
    erts_atomic_add_nob(&t2_retained_bytes, (erts_aint_t)ret->bytes);

    if (t2_debug_enabled()) {
        erts_fprintf(stderr,
                     "t2_retain: module=%T bytes=%lu total_retained=%lu\n",
                     beam->module,
                     (unsigned long)ret->bytes,
                     (unsigned long)erts_t2_retained_sz());
    }

    /* T2_BUILD=1 corpus testing runs from beam_load_finalize_code (after
     * the PC side table is built, which the T2_ISEL=1 coverage sweep
     * consults); see asm_load.c. */
}

void erts_t2_retained_free(ErtsT2RetainedCode *ret) {
    if (ret != NULL) {
        if (ret->profiles != NULL) {
            erts_free(ERTS_ALC_T_T2_CODE, ret->profiles);
        }
        erts_free(ERTS_ALC_T_T2_CODE, ret);
    }
}

void erts_t2_release(struct erl_module_instance *inst_p) {
    ErtsT2RetainedCode *ret = inst_p->t2_retained;

    if (ret == NULL) {
        return;
    }

    /* Free + un-account the separately-allocated PC side table first. */
    erts_t2_pctab_free(ret);

    erts_atomic_add_nob(&t2_retained_bytes, -(erts_aint_t)ret->bytes);
    if (ret->profiles != NULL) {
        erts_free(ERTS_ALC_T_T2_CODE, ret->profiles);
    }
    erts_free(ERTS_ALC_T_T2_CODE, ret);

    inst_p->t2_retained = NULL;
}
