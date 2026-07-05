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
 * T2-Full tier-2 JIT: dynamic install / jettison (see t2_install.h for
 * the design summary; PLAN/T2FULL/08 §3-§4, PLAN/T2/06 §§1-3, §5.1-5.2).
 */

#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "erl_alloc.h"
#include "erl_process.h"
#include "erl_thr_progress.h"
#include "code_ix.h"
#include "module.h"
#include "export.h"
#include "beam_asm.h"

#include "t2_install.h"
#include "t2_ranges.h"

#if defined(BEAMASM) && defined(__aarch64__)

/* The pristine patchable prologue word at L_f+4: `b next`, jumping over
 * the `bl i_breakpoint_trampoline_shared` (see erts_asm_bp_enable,
 * beam_asm.h, and emit_i_breakpoint_trampoline). Asserted bit-exactly,
 * never assumed. */
#    define T2_PROLOGUE_B_NEXT ((Uint32)0x14000002)

/* aarch64 `b` reach: signed imm26 in words = +-128MB. */
#    define T2_B_RANGE ((SWord)1 << 27)

/* The near-side bridge veneer (T2/06 §2.7(b)): an absolute indirect
 * jump through x14 (SUPER_TMP -- the only scratch register that is
 * dead at L_f+4 for every arity; x15/x16 hold XREG4/XREG5). Mirrors
 * BeamModuleAssembler::emit_veneer's `ldr/br` shape. */
#    define T2_BRIDGE_LDR_X14 ((Uint32)0x5800004E) /* ldr x14, .+8  */
#    define T2_BRIDGE_BR_X14 ((Uint32)0xD61F01C0)  /* br  x14       */
#    define T2_BRIDGE_SIZE 16

static erts_atomic_t t2_installed_bytes;
static int t2_install_initialized = 0;

static void t2_install_init(void) {
    if (!t2_install_initialized) {
        erts_atomic_init_nob(&t2_installed_bytes, 0);
        t2_install_initialized = 1;
    }
}

UWord erts_t2_installed_sz(void) {
    if (!t2_install_initialized) {
        return 0;
    }
    return (UWord)erts_atomic_read_nob(&t2_installed_bytes);
}

/* Everything below mutates prologues / install lists and must be
 * serialized the same way the trace/NIF prologue writers are. */
static void t2_assert_write_permission(void) {
    ERTS_LC_ASSERT(erts_initialized == 0 ||
                   erts_thr_progress_is_blocking() ||
                   erts_has_code_mod_permission());
}

/* ------------------------------------------------------------------ *
 * Deferred span release (tombstoning)                                *
 * ------------------------------------------------------------------ */

typedef struct {
    ErtsCodeBarrier barrier;
    void *spans[2];
    size_t sizes[2];
} ErtsT2BarrierFree;

#    ifdef DEBUG
/* By construction no live c_p->i ever points into a P1 blob (the entry
 * stub routes yields through i_test_yield_shared with ARG3 = L_f, so
 * the stored resume PC is the T1 body; PLAN/T2FULL/08 §4). Verify that
 * before the memory is released. */
static void t2_assert_no_resume_pc_in(const void *start, size_t size) {
    int i, max = erts_ptab_max(&erts_proc);
    const char *lo = (const char *)start;
    const char *hi = lo + size;

    for (i = 0; i < max; i++) {
        Process *p = erts_pix2proc(i);
        const char *ip;

        if (p == NULL) {
            continue;
        }
        ip = (const char *)p->i;
        ERTS_ASSERT(!(ip >= lo && ip < hi) &&
                    "tombstoned T2 blob still referenced by c_p->i");
    }
}
#    endif

static void t2_barrier_free(void *vbf) {
    ErtsT2BarrierFree *bf = (ErtsT2BarrierFree *)vbf;
    int i;

    for (i = 0; i < 2; i++) {
        if (bf->spans[i] != NULL) {
#    ifdef DEBUG
            t2_assert_no_resume_pc_in(bf->spans[i], bf->sizes[i]);
#    endif
            beamasm_t2_jit_release(bf->spans[i]);
        }
    }

    erts_free(ERTS_ALC_T_T2_CODE, bf);
}

void erts_t2_free_spans_after_barrier(void *span1,
                                      size_t size1,
                                      void *span2,
                                      size_t size2) {
    ErtsT2BarrierFree *bf = erts_alloc(ERTS_ALC_T_T2_CODE,
                                       sizeof(ErtsT2BarrierFree));

    bf->spans[0] = span1;
    bf->sizes[0] = size1;
    bf->spans[1] = span2;
    bf->sizes[1] = size2;

    /* Runs after thread progress + instruction barriers on all
     * schedulers: no scheduler can still be executing inside the spans
     * (P1 blobs hold no CPs/resume PCs, so in-flight invocations leave
     * on their next call/return/side-exit and, with the prologue
     * reverted before this is scheduled, can never re-enter). */
    erts_schedule_code_barrier_cleanup(&bf->barrier,
                                       t2_barrier_free,
                                       bf,
                                       size1 + size2);
}

/* ------------------------------------------------------------------ *
 * Reach                                                              *
 * ------------------------------------------------------------------ */

static int t2_b_reaches(const void *from, const void *to) {
    SWord diff = (SWord)((const char *)to - (const char *)from);

    return diff >= -T2_B_RANGE && diff < T2_B_RANGE && (diff & 3) == 0;
}

static Uint32 t2_encode_b(const void *from, const void *to) {
    SWord diff = (SWord)((const char *)to - (const char *)from);

    ASSERT(t2_b_reaches(from, to));
    return (Uint32)0x14000000 | (((Uint32)(diff >> 2)) & 0x03FFFFFFu);
}

/* ------------------------------------------------------------------ *
 * Install                                                            *
 * ------------------------------------------------------------------ */

ErtsT2InstallResult erts_t2_install(struct erl_module_instance *mi,
                                    const ErtsCodeInfo *ci_exec,
                                    const void *blob_entry,
                                    const void *blob_base,
                                    size_t blob_size) {
    const void *patch_exec;
    const void *reach_target;
    void *bridge_rx = NULL, *bridge_rw = NULL;
    ErtsT2Install *inst;
    Uint32 new_word;
    int was_sealed;

    t2_assert_write_permission();
    t2_install_init();

    ASSERT(mi != NULL && ci_exec != NULL && blob_entry != NULL);

    patch_exec = (const void *)((const char *)erts_codeinfo_to_code(ci_exec) +
                                sizeof(Uint32));

    /* --- the six preconditions of T2/06 §2.6, all under the lock --- */

    /* (0) The function's own MFA must not be a BIF: the loader's
     * is_mfa_bif transform loaded a call_bif_mfa trampoline at L_f,
     * not the chunk body a T2 blob was compiled from (the body is
     * emitted only as dead code after the trampoline). Installing
     * would replace the real BIF with its erlang:error/nif_error
     * stub fallback. Belt-and-braces to the isel-side rejection. */
    {
        const Export *self_ep =
                erts_active_export_entry(ci_exec->mfa.module,
                                         ci_exec->mfa.function,
                                         ci_exec->mfa.arity);

        if (self_ep != NULL && self_ep->bif_number >= 0) {
            return ERTS_T2_INSTALL_REJECTED_STATE;
        }
    }

    /* (2) No breakpoint/NIF flag; (4) not trace-patterned (a staged
     * GenericBp exists before the flag is set; both writers hold the
     * same permission, but check defensively against missed
     * wakeups). Strict mutual exclusion -- T2/06 §2.4. */
    if (erts_asm_bp_get_flags(ci_exec) != ERTS_ASM_BP_FLAG_NONE ||
        ci_exec->gen_bp != NULL) {
        return ERTS_T2_INSTALL_REJECTED_BP;
    }

    /* (1) The prologue is in the unmodified T1 form: the +4 word is
     * bit-exactly `b next`. Anything else means trace/NIF claimed the
     * prologue or a concurrent T2 install slipped by (impossible under
     * the permission, but asserted, never assumed). */
    if (*(const Uint32 *)patch_exec != T2_PROLOGUE_B_NEXT) {
        return ERTS_T2_INSTALL_REJECTED_PROLOGUE;
    }

    /* Duplicate install (the +4 check above already implies this; keep
     * the list consistent regardless). */
    for (inst = mi->t2_installs; inst != NULL; inst = inst->next) {
        if (inst->ci == ci_exec) {
            return ERTS_T2_INSTALL_REJECTED_DUP;
        }
    }

    /* (3) Watched-module generations: P1 compiles synchronously under
     * the same permission hold that installs, and the blob's cross-tier
     * addresses bind only the module instance itself (pctab T1 PCs, its
     * own function entries) plus permanent Export entries -- there is
     * no build-to-install window in which they can change. (6) The
     * pctab was consulted by isel; a function without one never reaches
     * emission. */

    /* (5) Reach: direct `b`, else a bridge veneer if *it* is in range,
     * else reject (map §3: the JitAllocator takes no address hint, so
     * co-location is not guaranteed; reject-if-far is the bounded,
     * behavior-preserving fallback). */
    if (t2_b_reaches(patch_exec, blob_entry)) {
        reach_target = blob_entry;
    } else {
        if (beamasm_t2_jit_alloc(&bridge_rx, &bridge_rw, T2_BRIDGE_SIZE) !=
            0) {
            return ERTS_T2_INSTALL_REJECTED_REACH;
        }
        if (!t2_b_reaches(patch_exec, bridge_rx)) {
            beamasm_t2_jit_release(bridge_rx);
            return ERTS_T2_INSTALL_REJECTED_REACH;
        }
        reach_target = bridge_rx;
    }

    new_word = t2_encode_b(patch_exec, reach_target);

    /* --- commit ---------------------------------------------------- */

    inst = erts_alloc(ERTS_ALC_T_T2_CODE, sizeof(ErtsT2Install));
    inst->ci = ci_exec;
    inst->mfa = ci_exec->mfa;
    inst->patch_addr = patch_exec;
    inst->patched_word = new_word;
    inst->blob_entry = blob_entry;
    inst->blob_base = blob_base;
    inst->blob_size = blob_size;
    inst->bridge_base = bridge_rx;
    inst->bridge_size = bridge_rx != NULL ? T2_BRIDGE_SIZE : 0;

    /* Register the blob range first so a PC inside the blob resolves to
     * the MFA from the instant it becomes reachable. */
    if (!erts_t2_register_blob(blob_base,
                               (ErtsCodePtr)((const char *)blob_base +
                                             blob_size),
                               &inst->mfa,
                               NULL)) {
        erts_free(ERTS_ALC_T_T2_CODE, inst);
        if (bridge_rx != NULL) {
            beamasm_t2_jit_release(bridge_rx);
        }
        return ERTS_T2_INSTALL_REJECTED_STATE;
    }

    was_sealed = !mi->unsealed;

    if (was_sealed) {
        erts_unseal_module(mi);
    } else {
        /* The loader holds the module unsealed, but a preceding blob
         * emission flipped this thread out of JIT-write mode (Apple
         * single-map W^X); flip it back. The loader's final
         * erts_seal_module provides the whole-module flush + reseal. */
        beamasm_unseal_module(mi->executable_region,
                              mi->writable_region,
                              mi->code_length);
    }

    if (bridge_rx != NULL) {
        Uint32 *w = (Uint32 *)bridge_rw;

        w[0] = T2_BRIDGE_LDR_X14;
        w[1] = T2_BRIDGE_BR_X14;
        sys_memcpy(&w[2], &blob_entry, sizeof(blob_entry));

        beamasm_flush_icache(bridge_rx, T2_BRIDGE_SIZE);
    }

    {
        /* The single 4-byte aligned store -- the entire install
         * surface (T2/06 §2.2). A CPU racing the store executes either
         * the old T1 path or the new T2 path; both are correct. */
        Uint32 volatile *rw_p =
                (Uint32 volatile *)erts_writable_code_ptr(mi,
                                                          patch_exec);

        *rw_p = new_word;
    }

    inst->next = mi->t2_installs;
    mi->t2_installs = inst;

    erts_atomic_add_nob(&t2_installed_bytes,
                        (erts_aint_t)(blob_size + inst->bridge_size));

    if (was_sealed) {
        /* Flush + reseal, then make sure every scheduler issues an
         * instruction barrier before the memory can be repurposed. */
        erts_seal_module(mi);
        erts_t2_free_spans_after_barrier(NULL, 0, NULL, 0);
    }

    return ERTS_T2_INSTALL_OK;
}

/* ------------------------------------------------------------------ *
 * Jettison                                                           *
 * ------------------------------------------------------------------ */

/* Unlink + revert one install; returns the unlinked node. The caller
 * has dealt with sealing and MUST, after any flush/reseal, schedule
 * the deferred span release (t2_release_node) -- the DEBUG barrier
 * discipline requires the code barrier to be scheduled *after* the
 * last icache flush of the operation. */
static ErtsT2Install *t2_jettison_one(struct erl_module_instance *mi,
                                      ErtsT2Install **prevp,
                                      int revert_prologue) {
    ErtsT2Install *inst = *prevp;

    if (revert_prologue) {
        Uint32 volatile *rw_p =
                (Uint32 volatile *)erts_writable_code_ptr(mi,
                                                          inst->patch_addr);

        ASSERT(*(const Uint32 *)inst->patch_addr == inst->patched_word);
        *rw_p = T2_PROLOGUE_B_NEXT;
    }

    erts_t2_deregister_blob(inst->blob_base);

    erts_atomic_add_nob(&t2_installed_bytes,
                        -(erts_aint_t)(inst->blob_size + inst->bridge_size));

    *prevp = inst->next;
    inst->next = NULL;
    return inst;
}

/* Schedule the node's spans for release behind a code barrier and free
 * the node. The blob (and its bridge -- a racing caller may sit right
 * on the bridge's `br`) stays mapped until every scheduler has passed
 * the barrier. */
static void t2_release_node(ErtsT2Install *inst) {
    erts_t2_free_spans_after_barrier((void *)inst->blob_base,
                                     inst->blob_size,
                                     inst->bridge_base,
                                     inst->bridge_size);
    erts_free(ERTS_ALC_T_T2_CODE, inst);
}

int erts_t2_jettison_function(struct erl_module_instance *mi,
                              const ErtsCodeInfo *ci_exec) {
    ErtsT2Install **prevp;

    t2_assert_write_permission();

    for (prevp = &mi->t2_installs; *prevp != NULL;
         prevp = &(*prevp)->next) {
        if ((*prevp)->ci == ci_exec) {
            int was_sealed = !mi->unsealed;
            ErtsT2Install *inst;

            if (was_sealed) {
                erts_unseal_module(mi);
            } else {
                /* Ensure JIT-write mode; see erts_t2_install. */
                beamasm_unseal_module(mi->executable_region,
                                      mi->writable_region,
                                      mi->code_length);
            }

            inst = t2_jettison_one(mi, prevp, 1);

            if (was_sealed) {
                erts_seal_module(mi);
            }

            /* After the flush: the scheduled code barrier both frees
             * the spans and satisfies the barrier the flush requires. */
            t2_release_node(inst);
            return 1;
        }
    }

    return 0;
}

void erts_t2_jettison_instance(struct erl_module_instance *mi,
                               int revert_prologue) {
    ErtsT2Install *chain = NULL;
    int was_sealed;

    t2_assert_write_permission();

    if (mi->t2_installs == NULL) {
        return;
    }

    was_sealed = !mi->unsealed;

    if (revert_prologue) {
        if (was_sealed) {
            erts_unseal_module(mi);
        } else {
            beamasm_unseal_module(mi->executable_region,
                                  mi->writable_region,
                                  mi->code_length);
        }
    }

    while (mi->t2_installs != NULL) {
        ErtsT2Install *inst = t2_jettison_one(mi,
                                              &mi->t2_installs,
                                              revert_prologue);

        inst->next = chain;
        chain = inst;
    }

    if (revert_prologue && was_sealed) {
        erts_seal_module(mi);
    }

    while (chain != NULL) {
        ErtsT2Install *inst = chain;

        chain = inst->next;
        t2_release_node(inst);
    }
}

#else /* !BEAMASM || !__aarch64__ */

/* Tier-2 install is aarch64-only in P1 (x86 lands in P7); provide inert
 * stubs so shared call sites need no arch conditions beyond the cheap
 * t2_installs guard (always NULL here). */

UWord erts_t2_installed_sz(void) {
    return 0;
}

ErtsT2InstallResult erts_t2_install(struct erl_module_instance *mi,
                                    const ErtsCodeInfo *ci_exec,
                                    const void *blob_entry,
                                    const void *blob_base,
                                    size_t blob_size) {
    (void)mi;
    (void)ci_exec;
    (void)blob_entry;
    (void)blob_base;
    (void)blob_size;
    return ERTS_T2_INSTALL_REJECTED_STATE;
}

int erts_t2_jettison_function(struct erl_module_instance *mi,
                              const ErtsCodeInfo *ci_exec) {
    (void)mi;
    (void)ci_exec;
    return 0;
}

void erts_t2_jettison_instance(struct erl_module_instance *mi,
                               int revert_prologue) {
    (void)mi;
    (void)revert_prologue;
}

void erts_t2_free_spans_after_barrier(void *span1,
                                      size_t size1,
                                      void *span2,
                                      size_t size2) {
    (void)span1;
    (void)size1;
    (void)span2;
    (void)size2;
}

#endif
