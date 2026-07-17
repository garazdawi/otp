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
#include "big.h"
#include "erl_process.h"
#include "erl_thr_progress.h"
#include "code_ix.h"
#include "module.h"
#include "export.h"
#include "beam_asm.h"

#include "t2_install.h"
#include "t2_ranges.h"
#include "t2_pctab.h"
#include "t2_retain.h"

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
/* Number of installed blobs carrying cross-module deps (P2 commit 8):
 * lets the hot trace/delete hooks skip the module-table walk when no
 * intrinsic blob exists. Mutated under code-mod permission only. */
static erts_atomic_t t2_deps_installed;
static int t2_install_initialized = 0;

static void t2_install_init(void) {
    if (!t2_install_initialized) {
        erts_atomic_init_nob(&t2_installed_bytes, 0);
        erts_atomic_init_nob(&t2_deps_installed, 0);
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
 * Two-phase blob retirement (P2 commit 5)                            *
 * ------------------------------------------------------------------ *
 * A blob with back-edge resume stubs may be named by yielded
 * processes' c_p->i, so its release needs more than the plain
 * barrier free:
 *
 *   jettison (sync, code-mod permission):
 *       revert the prologue (no new invocations), write the in-blob
 *       tombstone flags (a resume in flight from here on demotes
 *       itself to the T1 body), keep the range registered, schedule
 *       phase 1 behind a code barrier;
 *   phase 1 (after thread progress + isb on all schedulers — no
 *       scheduler still executes inside the blob, so no NEW back-edge
 *       yield can store a blob PC):
 *       translate every in-span c_p->i to the resume table's T1
 *       demote target (the fresh-call semantics of 08 §4.5: the saved
 *       vector is a valid fresh call, so the target is the T1 entry
 *       body), then schedule phase 2 behind a second barrier;
 *   phase 2 (second barrier: any process that had already fetched the
 *       stale stub PC at phase-1 time has since executed the — still
 *       mapped, tombstoned — stub and demoted itself):
 *       assert no c_p->i remains in the span, deregister the range
 *       (frees the resume table) and release the memory.
 *
 * Writing another process's c_p->i is an aligned word store; the only
 * competing writer is the owner scheduler at schedule-out (post-
 * barrier that value is never a PC into this blob), and the only
 * reader is schedule-in, which either sees the translated T1 address
 * or the stub address — the latter demotes via the tombstone. */

typedef struct {
    ErtsCodeBarrier barrier;
    int phase;
    void *blob_base;
    size_t blob_size;
    void *bridge_base;
    size_t bridge_size;
    /* Owned by the still-registered range until phase 2 deregisters. */
    const ErtsT2ResumeTab *rtab;
} ErtsT2RetireBlob;

static void t2_retire_phase(void *vrb) {
    ErtsT2RetireBlob *rb = (ErtsT2RetireBlob *)vrb;

    if (rb->phase == 1) {
        int i, max = erts_ptab_max(&erts_proc);
        const char *lo = (const char *)rb->blob_base;
        const char *hi = lo + rb->blob_size;

        for (i = 0; i < max; i++) {
            Process *p = erts_pix2proc(i);
            const char *ip;

            if (p == NULL) {
                continue;
            }
            ip = (const char *)p->i;
            if (ip >= lo && ip < hi) {
                /* Exactly the blob's resume PCs can be parked in
                 * c_p->i (P2 commit 5); translate per entry (P2
                 * commit 8: targets differ by demote class). */
                Uint32 k;
                ErtsCodePtr target = NULL;

                for (k = 0; k < rb->rtab->count; k++) {
                    if ((Uint32)(ip - lo) == rb->rtab->entries[k].offset) {
                        target = rb->rtab->entries[k].t1_demote;
                        break;
                    }
                }
                if (target == NULL) {
                    erts_exit(ERTS_ABORT_EXIT,
                              "t2 retire: c_p->i %p in blob %p is not a "
                              "registered resume PC\n",
                              ip,
                              lo);
                }
                p->i = target;
            }
        }

        rb->phase = 2;
        erts_schedule_code_barrier_cleanup(&rb->barrier,
                                           t2_retire_phase,
                                           rb,
                                           0);
        return;
    }

#    ifdef DEBUG
    t2_assert_no_resume_pc_in(rb->blob_base, rb->blob_size);
#    endif

    erts_t2_deregister_blob(rb->blob_base); /* frees the resume tab */
    beamasm_t2_jit_release(rb->blob_base);
    if (rb->bridge_base != NULL) {
        beamasm_t2_jit_release(rb->bridge_base);
    }
    erts_free(ERTS_ALC_T_T2_CODE, rb);
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
                                    size_t blob_size,
                                    void *blob_rw,
                                    const ErtsT2ResumeEntry *resume_points,
                                    Uint32 resume_count,
                                    const void *const *dep_hdrs,
                                    Uint32 dep_count) {
    const void *patch_exec;
    const void *reach_target;
    void *bridge_rx = NULL, *bridge_rw = NULL;
    ErtsT2Install *inst;
    ErtsT2ResumeTab *rtab = NULL;
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

    /* (0.5) The blob must not depend on more foreign modules than the
     * fixed dep_hdrs array can hold. A transitively-inlined wrapper chain
     * (t2_intrinsics.cpp, P1c) can in principle bake addresses from more
     * distinct modules than ERTS_T2_MAX_DEP_HDRS; recording only a prefix
     * would leave the un-recorded dependencies unable to jettison the blob
     * when they are reloaded/traced/purged, so a baked T1 address could
     * dangle. Refuse the install (the function stays on T1) rather than
     * track the deps incompletely. */
    if (dep_count > ERTS_T2_MAX_DEP_HDRS) {
        return ERTS_T2_INSTALL_REJECTED_STATE;
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
    inst->blob_rw = blob_rw;
    inst->bridge_base = bridge_rx;
    inst->bridge_size = bridge_rx != NULL ? T2_BRIDGE_SIZE : 0;

    /* Cross-module dependencies (P2 commit 8). */
    inst->dep_count = 0;
    {
        Uint32 d;

        ASSERT(dep_count <= ERTS_T2_MAX_DEP_HDRS);
        for (d = 0; d < dep_count && d < ERTS_T2_MAX_DEP_HDRS; d++) {
            inst->dep_hdrs[d] = dep_hdrs[d];
            inst->dep_count++;
        }
    }

    /* The resume table (P2 commit 5): every recovered loop's back-edge
     * resume PC, blob-relative, each with its own T1 translation
     * target (P2 commit 8) — the emitter resolved it per demote class:
     * the post-yield entry body for self-recursion back edges (a
     * translated resume then charges no extra reduction — fresh-call
     * semantics, 08 §4.5), the intrinsic call site's T1 PC for callee
     * back edges. */
    if (resume_count > 0) {
        Uint32 i;

        rtab = erts_alloc(ERTS_ALC_T_T2_CODE,
                          sizeof(ErtsT2ResumeTab) +
                                  (resume_count - 1) *
                                          sizeof(ErtsT2ResumeEntry));
        rtab->count = resume_count;
        rtab->flag_back = (Uint32)erts_t2_test_yield_return_offset();
        for (i = 0; i < resume_count; i++) {
            rtab->entries[i] = resume_points[i];
        }
    }
    inst->resume_tab = rtab;

    /* Register the blob range first so a PC inside the blob resolves to
     * the MFA from the instant it becomes reachable. */
    if (!erts_t2_register_blob(blob_base,
                               (ErtsCodePtr)((const char *)blob_base +
                                             blob_size),
                               &inst->mfa,
                               (const void *)mi->code_hdr,
                               rtab)) {
        if (rtab != NULL) {
            erts_free(ERTS_ALC_T_T2_CODE, rtab);
        }
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
    if (inst->dep_count > 0) {
        erts_atomic_add_nob(&t2_deps_installed, 1);
    }

    if (was_sealed) {
        /* Flush + reseal, then make sure every scheduler issues an
         * instruction barrier before the memory can be repurposed. */
        erts_seal_module(mi);
        erts_t2_free_spans_after_barrier(NULL, 0, NULL, 0);
    }

    /* Name the blob for `perf` so tier-2 samples resolve to
     * "$T2:Module:Function/Arity" instead of a bare address. A T2 blob
     * lives in its own allocator span, outside the T1 module range that
     * beamasm registers, so it needs its own entry. No-op unless
     * +JPperf is active. */
    {
        char t2_perf_name[256];

        erts_snprintf(t2_perf_name,
                      sizeof(t2_perf_name),
                      "T2:%T:%T/%u",
                      inst->mfa.module,
                      inst->mfa.function,
                      (unsigned)inst->mfa.arity);
        beamasm_t2_register_perf(t2_perf_name, blob_base, blob_size);
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

    if (inst->resume_tab != NULL) {
        /* Tombstone every resume stub (P2 commit 5): a process that
         * resumes into the blob from here on demotes itself to the T1
         * body. Data words read by the stub's ldr; plain coherent
         * stores through the writable alias suffice (the caller's
         * unseal put this thread in JIT-write mode where required),
         * and the retire barriers order them against any resume. The
         * range stays registered until retire phase 2 — the c_p->i
         * translation pass still needs it. */
        Uint32 i;

        for (i = 0; i < inst->resume_tab->count; i++) {
            Uint64 volatile *flag =
                    (Uint64 volatile *)((char *)inst->blob_rw +
                                        inst->resume_tab->entries[i].offset -
                                        inst->resume_tab->flag_back);

            *flag = 1;
        }
    } else {
        erts_t2_deregister_blob(inst->blob_base);
    }

    erts_atomic_add_nob(&t2_installed_bytes,
                        -(erts_aint_t)(inst->blob_size + inst->bridge_size));
    if (inst->dep_count > 0) {
        erts_atomic_add_nob(&t2_deps_installed, -1);
    }

    *prevp = inst->next;
    inst->next = NULL;
    return inst;
}

/* Schedule the node's spans for release behind a code barrier and free
 * the node. The blob (and its bridge -- a racing caller may sit right
 * on the bridge's `br`) stays mapped until every scheduler has passed
 * the barrier. A blob with resume stubs goes through the two-phase
 * retire (tombstones were set by t2_jettison_one; phase 1 translates
 * yielded c_p->i, phase 2 asserts-empty, deregisters and frees). */
static void t2_release_node(ErtsT2Install *inst) {
    if (inst->resume_tab != NULL) {
        ErtsT2RetireBlob *rb = erts_alloc(ERTS_ALC_T_T2_CODE,
                                          sizeof(ErtsT2RetireBlob));

        rb->phase = 1;
        rb->blob_base = (void *)inst->blob_base;
        rb->blob_size = inst->blob_size;
        rb->bridge_base = inst->bridge_base;
        rb->bridge_size = inst->bridge_size;
        rb->rtab = inst->resume_tab;

        erts_schedule_code_barrier_cleanup(&rb->barrier,
                                           t2_retire_phase,
                                           rb,
                                           inst->blob_size +
                                                   inst->bridge_size);
        erts_free(ERTS_ALC_T_T2_CODE, inst);
        return;
    }

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

int erts_t2_function_prologue_claimed(const ErtsCodeInfo *ci_exec) {
    /* L_f + 4: the patchable `b next` word in the T1 prologue (the word
     * erts_t2_install rewrites to reach the blob, and precondition (1)
     * validates against T2_PROLOGUE_B_NEXT). A racy single-word read; a
     * concurrent install/jettison stores one aligned word, so we observe
     * the old or the new value, both meaningful. */
    const Uint32 *patch =
            (const Uint32 *)((const char *)erts_codeinfo_to_code(ci_exec) +
                             sizeof(Uint32));

    return *patch != T2_PROLOGUE_B_NEXT;
}

void erts_t2_jettison_instance(struct erl_module_instance *mi,
                               int revert_prologue) {
    ErtsT2Install *chain = NULL;
    ErtsT2Install *scan;
    int was_sealed;
    int need_write;

    t2_assert_write_permission();

    if (mi->t2_installs == NULL) {
        return;
    }

    /* The purge path (revert_prologue == 0) still needs JIT-write mode
     * when any blob carries resume stubs: their tombstone flags are
     * written through the blob's writable alias, which on single-map
     * platforms (Apple Silicon) requires the thread write mode the
     * module unseal provides. */
    need_write = revert_prologue;
    for (scan = mi->t2_installs; scan != NULL && !need_write;
         scan = scan->next) {
        if (scan->resume_tab != NULL) {
            need_write = 1;
        }
    }

    was_sealed = !mi->unsealed;

    if (need_write) {
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

    if (need_write && was_sealed) {
        erts_seal_module(mi);
    }

    while (chain != NULL) {
        ErtsT2Install *inst = chain;

        chain = inst->next;
        t2_release_node(inst);
    }
}

/* Jettison every blob (across all modules, both instances) that
 * recorded `code_hdr` as a dependency (P2 commit 8). The caller holds
 * code modification permission, which serializes this against
 * install/jettison/trace/load. Rare path (module delete/trace/NIF
 * load), so the full module-table walk is fine. */
void erts_t2_jettison_deps(const void *code_hdr) {
    ErtsCodeIndex code_ix;
    int i, num;

    t2_assert_write_permission();

    if (code_hdr == NULL || !t2_install_initialized ||
        erts_atomic_read_nob(&t2_deps_installed) == 0) {
        return;
    }

    code_ix = erts_active_code_ix();
    num = module_code_size(code_ix);

    for (i = 0; i < num; i++) {
        Module *modp = module_code(i, code_ix);
        int inst_i;

        if (modp == NULL) {
            continue;
        }

        for (inst_i = 0; inst_i < 2; inst_i++) {
            struct erl_module_instance *mi = inst_i == 0 ? &modp->curr
                                                         : &modp->old;
            int again = 1;

            /* erts_t2_jettison_function unlinks from the list; restart
             * the scan after each hit. */
            while (again) {
                ErtsT2Install *inst;

                again = 0;
                for (inst = mi->t2_installs; inst != NULL;
                     inst = inst->next) {
                    Uint32 d;
                    int dep = 0;

                    for (d = 0; d < inst->dep_count; d++) {
                        if (inst->dep_hdrs[d] == code_hdr) {
                            dep = 1;
                            break;
                        }
                    }
                    if (dep) {
                        erts_t2_jettison_function(mi, inst->ci);
                        again = 1;
                        break;
                    }
                }
            }
        }
    }
}

/* ------------------------------------------------------------------ *
 * Counter self-disarm (P2 commit 10)                                 *
 * ------------------------------------------------------------------ */

/* aarch64 `b #imm` over `bytes` (forward, word-aligned). */
static Uint32 t2_encode_b_fwd(Uint32 bytes) {
    ASSERT(bytes >= 4 && bytes % 4 == 0 && (bytes >> 2) < (1u << 26));
    return (Uint32)0x14000000 | (Uint32)(bytes >> 2);
}

/* Patch one armed profiling sequence to a single `b` over it. The
 * store is one aligned instruction word: a racing executor runs either
 * the old sequence or the branch — both valid. The caller holds code
 * modification permission (worker path) or load permission with the
 * module unsealed (forced-disarm path). */
static void t2_profile_disarm_one(struct erl_module_instance *mi,
                                  ErtsT2Profile *rec,
                                  int module_unsealed) {
    Uint32 volatile *rw_p;

    if (rec->seq_addr == NULL || rec->seq_size < 4) {
        return;
    }

    if (!module_unsealed) {
        erts_unseal_module(mi);
    }

    rw_p = (Uint32 volatile *)erts_writable_code_ptr(mi, rec->seq_addr);
    *rw_p = t2_encode_b_fwd(rec->seq_size);

    if (!module_unsealed) {
        /* Flush + reseal, then require an instruction barrier on all
         * schedulers before anything depends on the disarmed state
         * (mirrors the install path's discipline). */
        erts_seal_module(mi);
        erts_t2_free_spans_after_barrier(NULL, 0, NULL, 0);
    }

    rec->seq_addr = NULL;
}

void erts_t2_profile_disarm(struct erl_module_instance *mi,
                            struct ErtsT2Profile *rec) {
    t2_assert_write_permission();
    t2_profile_disarm_one(mi, rec, 0);
}

int erts_t2_tier_disarm_forced(void) {
    static int forced = -1;

    if (forced < 0) {
        const char *env = getenv("T2_TIER_DISARM");
        forced = (env != NULL && env[0] == '1') ? 1 : 0;
    }
    return forced;
}

void erts_t2_disarm_module_profiles(struct erl_module_instance *mi,
                                    struct ErtsT2RetainedCode *ret) {
    Sint32 i;

    if (ret == NULL || ret->profiles == NULL) {
        return;
    }
    for (i = 0; i < ret->function_count; i++) {
        ErtsT2Profile *rec =
                (ErtsT2Profile *)((char *)ret->profiles +
                                  (size_t)i * ERTS_T2_PROFILE_STRIDE);

        /* The loader holds the module unsealed; its final
         * erts_seal_module provides the flush. */
        t2_profile_disarm_one(mi, rec, 1);
    }
}

/* ------------------------------------------------------------------ *
 * P2 commit 5 introspection (erts_debug:get_internal_state)          *
 * ------------------------------------------------------------------ */

Eterm erts_t2_debug_in_blob(Process *p, Eterm pid) {
    Process *rp;

    (void)p;

    if (!is_internal_pid(pid)) {
        return am_undefined;
    }
    rp = erts_proc_lookup(pid);
    if (rp == NULL) {
        return am_undefined;
    }
    /* A racy read of another process's saved instruction pointer:
     * meaningful for a yielded process (the debug harness suspends /
     * polls), advisory otherwise. */
    return erts_t2_find_blob(rp->i) != NULL ? am_true : am_false;
}

Eterm erts_t2_debug_yield_stats(Process *p) {
    Eterm *hp = HAlloc(p, 4);

    return TUPLE3(hp,
                  erts_make_integer((Uint)erts_t2_backedge_yields, p),
                  erts_make_integer((Uint)erts_t2_backedge_resumes, p),
                  erts_make_integer((Uint)erts_t2_callsite_deopts, p));
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
                                    size_t blob_size,
                                    void *blob_rw,
                                    const ErtsT2ResumeEntry *resume_points,
                                    Uint32 resume_count,
                                    const void *const *dep_hdrs,
                                    Uint32 dep_count) {
    (void)mi;
    (void)ci_exec;
    (void)blob_entry;
    (void)blob_base;
    (void)blob_size;
    (void)blob_rw;
    (void)resume_points;
    (void)resume_count;
    (void)dep_hdrs;
    (void)dep_count;
    return ERTS_T2_INSTALL_REJECTED_STATE;
}

void erts_t2_jettison_deps(const void *code_hdr) {
    (void)code_hdr;
}

void erts_t2_profile_disarm(struct erl_module_instance *mi,
                            struct ErtsT2Profile *rec) {
    (void)mi;
    (void)rec;
}

int erts_t2_tier_disarm_forced(void) {
    return 0;
}

void erts_t2_disarm_module_profiles(struct erl_module_instance *mi,
                                    struct ErtsT2RetainedCode *ret) {
    (void)mi;
    (void)ret;
}

Eterm erts_t2_debug_in_blob(Process *p, Eterm pid) {
    (void)p;
    (void)pid;
    return am_undefined;
}

Eterm erts_t2_debug_yield_stats(Process *p) {
    Eterm *hp = HAlloc(p, 4);

    return TUPLE3(hp, make_small(0), make_small(0), make_small(0));
}

int erts_t2_jettison_function(struct erl_module_instance *mi,
                              const ErtsCodeInfo *ci_exec) {
    (void)mi;
    (void)ci_exec;
    return 0;
}

int erts_t2_function_prologue_claimed(const ErtsCodeInfo *ci_exec) {
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
