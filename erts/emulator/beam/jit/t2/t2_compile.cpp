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
 * T2-Full tier-2 JIT: the compile pipeline driver (PLAN/T2FULL/08 §5).
 *
 * Chains the P0/P1 stages -- SSA build -> identity isel -> LIR verify
 * -> installable-blob emission -> prologue install -- for one function
 * at a time. Consumers:
 *
 *   - the erts_debug:get_internal_state({t2_install|t2_jettison|
 *     t2_installed, M, F, A}) debug hooks (this file), used by the
 *     install-wave tests;
 *   - the +JT2enable synchronous compile-at-load driver.
 *
 * A failure at any stage degrades to T1 for that function -- never an
 * error, never an aborted load.
 */

/* config.h first: it defines the HAVE_* feature macros the headers below
 * rely on. */
#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

/* C++ standard headers MUST precede the ERTS headers below. erl_term.h
 * defines function-like macros make_boxed/make_list/make_tuple (the term
 * constructors); once those are in scope they collide with and corrupt
 * libstdc++'s std::make_tuple / std::make_pair declarations pulled in by
 * <functional>/<tuple>. Including the C++ headers -- and the t2 .hpp files
 * that transitively include them -- first keeps the std names intact before
 * the macros are defined. (This is the same ordering the other t2 .cpp files
 * use, e.g. t2_isel.cpp including t2_isel.hpp ahead of its extern "C" block.)
 */
#include <ctime>
#include <string>

#include "t2_hir.hpp"
#include "t2_isel.hpp"
#include "t2_loop.hpp"
#include "t2_lir.hpp"
#include "t2_emit.hpp"
#include "t2_spec.hpp"
#include "t2_inline.hpp"
#include "t2_intrinsics.hpp"

extern "C"
{
#include "sys.h"
#include "global.h"
#include "big.h"
#include "code_ix.h"
#include "module.h"
#include "export.h"
#include "beam_code.h"
#include "beam_asm.h"

#include "t2_retain.h"
#include "t2_install.h"
#include "t2_ranges.h"
}

using namespace erts_t2;

namespace {

    /* Outcome of one function's trip through the pipeline; the failure
     * cases name the stage for diagnostics/statistics. */
    enum class T2CompileStatus {
        Installed,
        IselUnsupported, /* outside the P1 identity table (expected)   */
        EmitFailed,
        RejectedBp,
        RejectedProloguePatched,
        RejectedReach,
        RejectedDup,
        RejectedState,
        RejectedGate /* install-quality gate: the blob would not beat T1
                      * (P2.6 blocker B; PLAN/T2FULL/10)               */
    };

    T2CompileStatus map_install_result(ErtsT2InstallResult r) {
        switch (r) {
        case ERTS_T2_INSTALL_OK:
            return T2CompileStatus::Installed;
        case ERTS_T2_INSTALL_REJECTED_BP:
            return T2CompileStatus::RejectedBp;
        case ERTS_T2_INSTALL_REJECTED_PROLOGUE:
            return T2CompileStatus::RejectedProloguePatched;
        case ERTS_T2_INSTALL_REJECTED_REACH:
            return T2CompileStatus::RejectedReach;
        case ERTS_T2_INSTALL_REJECTED_DUP:
            return T2CompileStatus::RejectedDup;
        case ERTS_T2_INSTALL_REJECTED_STATE:
        default:
            return T2CompileStatus::RejectedState;
        }
    }

    /* ---- P2.6 blocker B: install-quality gate ----------------------
     *
     * A static, compile-time decision (PLAN/T2FULL/10). Diagnosis found
     * that T2 beats T1 *only when it removes work* (inlines a call, fuses
     * unboxed arithmetic, or fuses a per-byte bs loop into a scan run);
     * a blob that re-emits T1's ops plus speculation guards can only
     * tie-or-lose the warm steady state. Two taxes make the "tie" a loss:
     *   - a retained non-tail call lowers as `mov x30,cont; br target`
     *     (demote-on-return), defeating the CPU return-stack predictor
     *     (~3.5 ns/call vs T1's `bl`); measured 3-4x on body recursion;
     *   - a wide `select_val` lowers as a linear scan where T1 emits a
     *     binary search (i_select_val_bins).
     * A runtime A/B timer is unsafe (effectful functions can't be re-run
     * to benchmark), so the gate is purely static: it reads the signals
     * off the post-isel + post-regalloc LIR, all byproducts of passes
     * that already ran. Refuse to install anything that is not a clear
     * win — the "never slower than T1" floor. */
    struct T2InstallSignals {
        unsigned calls_inlined;    /* leaf-inline or foldl-class intrinsic:
                                    * a callee was erased                 */
        unsigned calls_retained;   /* Call+CallExt: non-tail generic calls
                                    * that demote-on-return (the tax)     */
        unsigned fused_arith;      /* AddSmall+SubSmall: unboxed speculated
                                    * arithmetic                          */
        unsigned bs_scan;          /* raw byte-aligned bs match ops
                                    * (StartMatch/BsMatch/…): present in
                                    * BOTH a fused scan-run and a slow
                                    * un-fused per-byte match loop        */
        unsigned scan_runs;        /* fused bs scan-run regions the
                                    * emitter actually laid down (memo 10's
                                    * `bs_scan_runs`): the ONLY bs shape
                                    * that beats T1. bs_scan>=1 with
                                    * scan_runs==0 is the un-fused per-byte
                                    * loser (lex_wl:classify/4)            */
        unsigned switch_max_cases; /* widest Switch (T2 lowers linear;
                                    * T1 binary-searches)                 */
        unsigned spec_guards;      /* SpeculateSmall                       */
        unsigned spills;           /* the placement allocator keeps every
                                    * value in an X/Y home, so it cannot
                                    * spill in the classic sense (verified
                                    * T2_RA_DUMP=0); kept for the trace    */
        unsigned blob_size;        /* emitted bytes (trace only)           */
    };

    void t2_collect_install_signals(const T2LirFunction &lir,
                                    bool leaf_inlined,
                                    unsigned scan_runs,
                                    unsigned blob_size,
                                    T2InstallSignals *s) {
        unsigned intrinsic_loop = 0;

        s->calls_inlined = 0;
        s->calls_retained = 0;
        s->fused_arith = 0;
        s->bs_scan = 0;
        s->scan_runs = scan_runs;
        s->switch_max_cases = 0;
        s->spec_guards = 0;
        s->spills = 0;
        s->blob_size = blob_size;

        for (const T2LirBlock &b : lir.blocks) {
            for (const T2LirOp &op : b.ops) {
                switch (op.kind) {
                case T2LirKind::Call:
                case T2LirKind::CallExt:
                    /* Non-tail, demote-on-return. CallBif (light BIF) is
                     * NOT counted: it returns into the blob, so it does
                     * not carry the return-predictor tax. Tail calls are
                     * the loop back-edge, not a tax. */
                    s->calls_retained++;
                    break;
                case T2LirKind::AddSmall:
                case T2LirKind::SubSmall:
                    s->fused_arith++;
                    break;
                case T2LirKind::SpeculateSmall:
                    s->spec_guards++;
                    break;
                case T2LirKind::StartMatch:
                case T2LirKind::BsMatch:
                case T2LirKind::BsGetTail:
                case T2LirKind::BsTestTail:
                    s->bs_scan++;
                    break;
                case T2LirKind::Switch:
                    if (op.num_cases > s->switch_max_cases) {
                        s->switch_max_cases = op.num_cases;
                    }
                    break;
                case T2LirKind::ReductionCheckCallee:
                case T2LirKind::ChargeReds:
                case T2LirKind::DemoteCallee:
                    /* foldl-class intrinsic markers: the lists call it
                     * stands in for is erased. The markers recur per
                     * error edge, so count presence, not occurrences. */
                    intrinsic_loop++;
                    break;
                default:
                    break;
                }
            }
        }

        s->calls_inlined =
                (leaf_inlined ? 1u : 0u) + (intrinsic_loop ? 1u : 0u);
    }

    /* T2_INSTALL_GATE=0 disables the gate (install everything, the pre-B
     * behaviour, for before/after measurement). Default ON. */
    bool t2_install_gate_on(void) {
        static int on = -1;

        if (on < 0) {
            const char *env = getenv("T2_INSTALL_GATE");
            on = (env != NULL && env[0] == '0') ? 0 : 1;
        }
        return on;
    }

    /* True to INSTALL, false to REJECT. On reject `*reason` names the
     * dominating signal (trace only). */
    bool t2_install_gate_accept(const T2InstallSignals &s,
                                const char **reason) {
        /* The bs "work eliminated" signal is the *fused scan-run*
         * (memo 10's bs_scan_runs), NOT any bs match op. A multi-clause
         * byte classifier lowers to raw bs match ops (bs_scan >= 1) that
         * the fused-scan-loop emitter refuses to admit (scan_runs == 0):
         * it re-emits T1's per-byte match 1:1 and is measured slower
         * (lex_wl:classify/4: T1 193 us -> T2 268 us, +38%). Requiring a
         * real scan-run here is memo 10's original intent; the drift to
         * bs_scan >= 1 was the false-accept. */
        bool bs_unfused = s.bs_scan >= 1 && s.scan_runs == 0;
        bool eliminated =
                s.calls_inlined >= 1 || s.fused_arith >= 2 || s.scan_runs >= 1;
        /* An un-fused per-byte bs loop is a *tax*, not merely a missing
         * win: it can trip the eliminated-work arm on the side (its
         * mutually-exclusive per-clause increments over-count fused_arith
         * -- lex_wl statically shows 3 add_small, only one per byte), yet
         * the slow bs loop dominates. Disqualify it outright so the
         * never-slower floor holds regardless of which arm accepted. */
        bool disqualified = s.calls_retained >= 1 || s.switch_max_cases > 4 ||
                            bs_unfused ||
                            (s.spec_guards >= 1 && s.fused_arith == 0);

        if (!eliminated) {
            if (reason != NULL) {
                *reason = "no work eliminated (T1 re-emit)";
            }
            return false;
        }
        if (disqualified) {
            if (reason != NULL) {
                if (s.calls_retained >= 1) {
                    *reason = "retained non-tail call";
                } else if (s.switch_max_cases > 4) {
                    *reason = "wide switch (linear scan)";
                } else if (bs_unfused) {
                    *reason = "un-fused per-byte bs loop (slower than T1)";
                } else {
                    *reason = "speculation without fusion";
                }
            }
            return false;
        }
        return true;
    }

    /* Facts observed by the tier-up profile (P2 commit 9): the default
     * source's type-chunk reasoning, narrowed by the recorded
     * non-small observations — an argument seen non-small would deopt
     * every entry, so its speculation is withheld. */
    class T2ProfileFactSource : public T2FactSource {
    public:
        T2ProfileFactSource(const T2Function &fn, const ErtsT2Profile *rec)
                : dflt(fn), nonsmall(rec != nullptr ? rec->nonsmall : 0) {
        }

        bool speculate_param_small(uint32_t idx) const override {
            if (idx < 4 && (nonsmall & (1u << idx)) != 0) {
                return false;
            }
            return dflt.speculate_param_small(idx);
        }

    private:
        T2DefaultFactSource dflt;
        Uint32 nonsmall;
    };

    /* Lower + emit + install one built HIR function against the loaded
     * module instance. Called from inside the builder's emit callback
     * (the module decode is still alive). `profile` (may be null) is
     * the function's tier-up record, plugged into the speculation
     * inserter as its fact source. */
    T2CompileStatus t2_compile_install_one(
            T2Function &hir,
            const ErtsT2RetainedCode *ret,
            const BeamCodeHeader *code_hdr,
            struct erl_module_instance *mi,
            std::string *diag,
            const ErtsT2Profile *profile = nullptr) {
        T2IselContext ctx;
        T2LirFunction lir;
        T2EmitResult blob;
        std::string err;
        bool leaf_inlined = false; /* the leaf inliner erased a local call
                                    * (install-quality gate signal)        */

        ctx.ret = ret;
        ctx.code_hdr = (const void *)code_hdr;

        /* Loop recovery (PLAN/T2FULL/09 §4, work-order item 2):
         * rewrite local self-recursive tail calls into back edges.
         * Install pipeline only — a standalone debug-exec blob has
         * no patched prologue for the back-edge demote's L_f
         * contract. A recovered function is re-validated in full,
         * re-proving the sync/frame/home state model on the
         * rewritten IR; any failure degrades to T1, loudly. */
        {
            bool recovered = false;
            bool intrinsified = false;

            if (!t2_loop_recover(hir, &recovered, &err)) {
                if (diag) {
                    *diag = err;
                }
                return T2CompileStatus::IselUnsupported;
            }
            if (recovered && !t2_validate(hir, &err)) {
                if (diag) {
                    *diag = "post-recovery validate: " + err;
                }
                return T2CompileStatus::IselUnsupported;
            }

            /* lists intrinsics (P2 commit 8): expand recognized
             * foldl-class call sites into in-blob loops. Install
             * pipeline only (the demote contracts bake T1 addresses,
             * exactly like loop recovery). Independent of self-
             * recursion; re-validated in full below. */
            if (!t2_intrinsics(hir,
                               ret,
                               (const void *)code_hdr,
                               &intrinsified,
                               &err)) {
                if (diag) {
                    *diag = "intrinsics: " + err;
                }
                return T2CompileStatus::IselUnsupported;
            }
            if (intrinsified && getenv("T2_INTRIN_DUMP") != NULL) {
                std::string d = t2_dump(hir);

                erts_fprintf(stderr,
                             "t2_intrinsics dump %T:%T/%u\n",
                             hir.module,
                             hir.function,
                             (unsigned)hir.arity);
                fwrite(d.data(), 1, d.size(), stderr);
            }
            if (intrinsified && !t2_validate(hir, &err)) {
                if (diag) {
                    *diag = "post-intrinsics validate: " + err;
                }
                return T2CompileStatus::IselUnsupported;
            }

            if (recovered || intrinsified) {
                T2LoopInfo li;
                bool rewritten = false;

                t2_loop_info(hir, &li);

                /* Local leaf inlining + loop shape-up (P2 commit 6):
                 * splice the loop's single leaf callee, drop the frame
                 * the call forced, and preserve the re-call vector.
                 * The spliced arithmetic is window-only (rung-1
                 * re-call deopt: the enclosing iteration covers the
                 * inlined body), so it depends on the speculation pass
                 * below — both ride the same T2_NO_SPEC lever, plus
                 * T2_NO_INLINE for isolating the inliner. The CFG is
                 * unchanged (single-block callees), so `li` stays
                 * valid. */
                if (getenv("T2_NO_SPEC") == NULL &&
                    getenv("T2_NO_INLINE") == NULL) {
                    bool inlined = false;

                    if (!t2_inline_leaf(hir, li, ret, &inlined, &err)) {
                        if (diag) {
                            *diag = "inliner: " + err;
                        }
                        return T2CompileStatus::IselUnsupported;
                    }
                    if (inlined) {
                        /* The spliced callee's code now lives in this
                         * blob: a breakpoint anywhere in the own
                         * instance (the callee included) must kill it
                         * (P2 commit 8 closes this wave-6 hole via the
                         * dependency registry). */
                        bool have = false;

                        for (const void *d : hir.dep_hdrs) {
                            have |= d == (const void *)code_hdr;
                        }
                        if (!have) {
                            hir.dep_hdrs.push_back((const void *)code_hdr);
                        }
                        rewritten = true;
                        leaf_inlined = true;
                    }
                }

                /* Speculation insertion (P2 commit 4): entry-type +
                 * loop-carried "observed small" speculation with
                 * flag-checked arithmetic and fused guards. Every
                 * conversion is re-proven below by the full validator
                 * (born-checked: run_speculation_checks) and the
                 * window validator; any failure degrades to T1,
                 * loudly. T2_NO_SPEC=1 forces the identity pipeline
                 * (the A/B lever for the measurement legs). */
                if (getenv("T2_NO_SPEC") == NULL) {
                    T2ProfileFactSource facts(hir, profile);
                    bool spec_changed = false;

                    if (!t2_speculate(hir,
                                      li,
                                      ret,
                                      facts,
                                      &spec_changed,
                                      &err)) {
                        if (diag) {
                            *diag = "speculation: " + err;
                        }
                        return T2CompileStatus::IselUnsupported;
                    }
                    rewritten |= spec_changed;
                }

                /* LICM-lite (P2 commit 8): hoist loop-invariant
                 * window-shaped guards from loop headers to
                 * preheaders. Runs after the speculation inserter (its
                 * guards are the candidates); the CFG is unchanged, so
                 * `li` stays valid, and a hoisted guard lands under
                 * the entry-window rule the window validator below
                 * enforces. */
                if (getenv("T2_NO_SPEC") == NULL) {
                    bool hoisted = false;

                    if (!t2_licm_lite(hir, li, &hoisted, &err)) {
                        if (diag) {
                            *diag = "licm: " + err;
                        }
                        return T2CompileStatus::IselUnsupported;
                    }
                    rewritten |= hoisted;
                }

                if (rewritten && !t2_validate(hir, &err)) {
                    if (diag) {
                        *diag = "post-rewrite validate: " + err;
                    }
                    return T2CompileStatus::IselUnsupported;
                }

                /* Re-execution-window legality (PLAN/T2/08 §4.2): the
                 * clean-prefix rule for window-shaped deopt guards;
                 * vacuous when speculation inserted nothing, enforced
                 * from the first relaxed blob on so the corruption
                 * class cannot land unnoticed. */
                if (!t2_validate_windows(hir, li, &err)) {
                    if (diag) {
                        *diag = err;
                    }
                    return T2CompileStatus::IselUnsupported;
                }
            }
        }

        if (!t2_isel(hir, ctx, lir, &err)) {
            if (diag) {
                *diag = err;
            }
            return T2CompileStatus::IselUnsupported;
        }

        const ErtsCodeInfo *ci = code_hdr->functions[hir.fn_index];
        const void *l_f = (const void *)erts_codeinfo_to_code(ci);

        lir.t1_entry = l_f;

        if (!t2_regalloc(lir, &err)) {
            if (diag) {
                *diag = err;
            }
            return T2CompileStatus::IselUnsupported;
        }

        bool dump = getenv("T2_DUMP") != nullptr;
        std::string disasm;

        if (!t2_emit_blob_install(lir,
                                  l_f,
                                  &blob,
                                  &err,
                                  dump ? &disasm : nullptr)) {
            if (diag) {
                *diag = err;
            }
            return T2CompileStatus::EmitFailed;
        }

        if (dump) {
            erts_fprintf(stderr,
                         "t2_compile: %T:%T/%u L_f=%p\n%s\n--- disasm ---\n",
                         hir.module,
                         hir.function,
                         (unsigned)hir.arity,
                         l_f,
                         t2_lir_dump(lir).c_str());
            fwrite(disasm.data(), 1, disasm.size(), stderr);
        }

        /* Install-quality gate (P2.6 blocker B; PLAN/T2FULL/10). The
         * blob is fully emitted but not yet reachable: decide, purely
         * from the static signals, whether it can beat T1. On reject,
         * free it through the same barrier-scheduled path the install-
         * failure branch uses (the emission flushed this thread's icache)
         * and return terminal — the tier worker disarms the record so it
         * never retries (a rejected shape cannot improve without new
         * code). */
        if (t2_install_gate_on()) {
            T2InstallSignals sig;
            const char *reason = "";

            t2_collect_install_signals(lir,
                                       leaf_inlined,
                                       blob.scan_runs,
                                       (unsigned)blob.size,
                                       &sig);

            if (!t2_install_gate_accept(sig, &reason)) {
                if (getenv("T2_DEBUG") != NULL ||
                    getenv("T2_INSTALL_TRACE") != NULL) {
                    erts_fprintf(stderr,
                                 "t2_gate: REJECT %T:%T/%u -- %s "
                                 "(inl=%u ret=%u fus=%u bs=%u run=%u sw=%u "
                                 "spec=%u sz=%u)\n",
                                 hir.module,
                                 hir.function,
                                 (unsigned)hir.arity,
                                 reason,
                                 sig.calls_inlined,
                                 sig.calls_retained,
                                 sig.fused_arith,
                                 sig.bs_scan,
                                 sig.scan_runs,
                                 sig.switch_max_cases,
                                 sig.spec_guards,
                                 sig.blob_size);
                }
                erts_t2_free_spans_after_barrier((void *)blob.base,
                                                 blob.size,
                                                 NULL,
                                                 0);
                return T2CompileStatus::RejectedGate;
            }
        }

        std::vector<ErtsT2ResumeEntry> rpoints;

        rpoints.reserve(blob.resume_points.size());
        for (const T2EmitResult::ResumePoint &pt : blob.resume_points) {
            ErtsT2ResumeEntry e;

            e.offset = pt.offset;
            e.t1_demote = (ErtsCodePtr)pt.t1_demote;
            rpoints.push_back(e);
        }

        ErtsT2InstallResult res = erts_t2_install(
                mi,
                ci,
                blob.entry,
                blob.base,
                blob.size,
                blob.rw_base,
                rpoints.empty() ? NULL : rpoints.data(),
                (Uint32)rpoints.size(),
                hir.dep_hdrs.empty() ? NULL : hir.dep_hdrs.data(),
                (Uint32)hir.dep_hdrs.size());

        if (res != ERTS_T2_INSTALL_OK) {
            /* The blob was never reachable by anyone, but its emission
             * flushed the icache on this thread; route the release
             * through the code-barrier free so the barrier the flush
             * requires is scheduled (DEBUG-asserted at thread
             * progress). */
            erts_t2_free_spans_after_barrier((void *)blob.base,
                                             blob.size,
                                             NULL,
                                             0);
        }

        return map_install_result(res);
    }

} /* anonymous namespace */

/* ------------------------------------------------------------------ *
 * +JT2enable synchronous compile-at-load driver (map §5)             *
 * ------------------------------------------------------------------ */

/* Cumulative statistics. Loads are serialized by load permission (and
 * early boot is single-threaded), so plain counters suffice; reads
 * from the t2_stats debug hook are advisory. */
static struct {
    Uint64 modules;          /* modules the driver processed           */
    Uint64 functions;        /* eligible functions built OK            */
    Uint64 installed;        /* blobs installed                        */
    Uint64 isel_unsupported; /* outside the P1 identity table          */
    Uint64 emit_failed;
    Uint64 install_rejected; /* bp/prologue/reach/dup/state            */
    Uint64 gate_rejected;    /* install-quality gate refusals (P2.6 B) */
    Uint64 build_failed;     /* SSA build/validate failures            */
    Uint64 compile_ns;       /* wall time inside the driver            */
} t2_stats;

/* Bisection knobs (debugging aids for the identity gate):
 *   T2_INSTALL_LIMIT=N   install at most N blobs process-wide (the
 *                        pipeline still runs; only the install is
 *                        skipped past the limit);
 *   T2_INSTALL_TRACE=1   print every installed MFA, in order.
 * Unset means unlimited / quiet. */
static Sint64 t2_install_limit() {
    static Sint64 limit = -2;

    if (limit == -2) {
        const char *env = getenv("T2_INSTALL_LIMIT");
        limit = (env != NULL && env[0] != '\0') ? (Sint64)atoll(env) : -1;
    }
    return limit;
}

static int t2_install_trace() {
    static int on = -1;

    if (on < 0) {
        const char *env = getenv("T2_INSTALL_TRACE");
        on = (env != NULL && env[0] == '1') ? 1 : 0;
    }
    return on;
}

/* Raw monotonic ns, deliberately independent of the ERTS time
 * subsystem: the driver also runs while preloaded modules load, long
 * before erl_init finishes. */
static Uint64 t2_now_ns() {
    struct timespec ts;

    if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) {
        return 0;
    }
    return (Uint64)ts.tv_sec * UINT64_C(1000000000) + (Uint64)ts.tv_nsec;
}

extern "C" void erts_t2_compile_module(const struct ErtsT2RetainedCode *ret,
                                       const void *code_hdr_v,
                                       struct erl_module_instance *mi) {
    const BeamCodeHeader *code_hdr = (const BeamCodeHeader *)code_hdr_v;
    Uint64 t0;
    std::string err;
    int build_failures = 0;

    if (!erts_jit_t2_force || ret == NULL || code_hdr == NULL || mi == NULL) {
        return;
    }

    t0 = t2_now_ns();

    (void)t2_build_each(
            ret,
            [&](T2Function &hir) {
                Sint64 limit = t2_install_limit();

                t2_stats.functions++;

                if (limit >= 0 && (Sint64)t2_stats.installed >= limit) {
                    return;
                }

                switch (t2_compile_install_one(hir,
                                               ret,
                                               code_hdr,
                                               mi,
                                               nullptr)) {
                case T2CompileStatus::Installed:
                    t2_stats.installed++;
                    if (t2_install_trace()) {
                        erts_fprintf(stderr,
                                     "t2_install: %4lu %T:%T/%u\n",
                                     (unsigned long)t2_stats.installed,
                                     hir.module,
                                     hir.function,
                                     (unsigned)hir.arity);
                    }
                    break;
                case T2CompileStatus::IselUnsupported:
                    t2_stats.isel_unsupported++;
                    break;
                case T2CompileStatus::EmitFailed:
                    t2_stats.emit_failed++;
                    break;
                case T2CompileStatus::RejectedGate:
                    t2_stats.gate_rejected++;
                    break;
                default:
                    t2_stats.install_rejected++;
                    break;
                }
            },
            &build_failures,
            &err);

    t2_stats.build_failed += (Uint64)build_failures;
    t2_stats.modules++;
    t2_stats.compile_ns += t2_now_ns() - t0;
}

/* ------------------------------------------------------------------ *
 * Tier-up worker entry (P2 commit 9; called from t2_tier.c under      *
 * code-modification permission)                                       *
 * ------------------------------------------------------------------ */

extern "C" unsigned erts_t2_tier_compile_batch(Eterm module,
                                               const Uint32 *fn_indices,
                                               unsigned n,
                                               unsigned *rejected_out) {
    Module *modp = erts_get_module(module, erts_active_code_ix());
    const ErtsT2RetainedCode *ret;
    const BeamCodeHeader *code_hdr;
    struct erl_module_instance *mi;
    unsigned installed = 0;
    unsigned rejected = 0;

    if (rejected_out != NULL) {
        *rejected_out = 0;
    }

    if (modp == NULL || modp->curr.code_hdr == NULL ||
        modp->curr.t2_retained == NULL) {
        /* Purged/reloaded between trip and compile: drop the batch
         * (the new instance's own counters retrip). */
        return 0;
    }

    mi = &modp->curr;
    ret = mi->t2_retained;
    code_hdr = mi->code_hdr;

    if (ret->profiles == NULL) {
        return 0;
    }

    (void)t2_build_selected(
            ret,
            fn_indices,
            n,
            [&](T2Function &hir) {
                const ErtsT2Profile *rec =
                        (const ErtsT2Profile *)((const char *)ret->profiles +
                                                (size_t)hir.fn_index *
                                                        ERTS_T2_PROFILE_STRIDE);
                std::string diag;
                T2CompileStatus st = t2_compile_install_one(hir,
                                                            ret,
                                                            code_hdr,
                                                            mi,
                                                            &diag,
                                                            rec);

                if (st == T2CompileStatus::Installed) {
                    installed++;
                    if (t2_install_trace()) {
                        erts_fprintf(stderr,
                                     "t2_tier: installed %T:%T/%u\n",
                                     hir.module,
                                     hir.function,
                                     (unsigned)hir.arity);
                    }
                } else if (st == T2CompileStatus::RejectedGate) {
                    rejected++;
                } else if (getenv("T2_DEBUG") != NULL) {
                    erts_fprintf(stderr,
                                 "t2_tier: %T:%T/%u failed: %s\n",
                                 hir.module,
                                 hir.function,
                                 (unsigned)hir.arity,
                                 diag.c_str());
                }
                /* Success or failure, the record stays pending: a
                 * failed shape cannot improve without new code
                 * (PLAN/T2/08 §4.6's permanent-demote rule for static
                 * exits, applied to compile rejections). Either way
                 * the outcome is terminal — patch the T1 profiling
                 * sequence out (P2 commit 10): an installed function's
                 * demote/resume paths, and a failed function's every
                 * T1 entry, then cost one taken branch instead of the
                 * counter sequence. */
                erts_t2_profile_disarm(
                        mi,
                        (ErtsT2Profile *)((char *)ret->profiles +
                                          (size_t)hir.fn_index *
                                                  ERTS_T2_PROFILE_STRIDE));
            },
            nullptr);

    /* Functions the builder skipped (decode/build/validate failures)
     * never reach the emit callback above, but their outcome is just
     * as terminal — their records sit pending and would pay the armed
     * sequence forever. Disarm every requested record that still has
     * one. */
    {
        unsigned i;

        for (i = 0; i < n; i++) {
            if (fn_indices[i] < (Uint32)ret->function_count) {
                erts_t2_profile_disarm(
                        mi,
                        (ErtsT2Profile *)((char *)ret->profiles +
                                          (size_t)fn_indices[i] *
                                                  ERTS_T2_PROFILE_STRIDE));
            }
        }
    }

    if (rejected_out != NULL) {
        *rejected_out = rejected;
    }
    return installed;
}

extern "C" Eterm erts_t2_debug_stats(Process *p) {
    /* Nine fields -> past the TUPLE8 macro; build the tuple by hand. */
    Eterm *hp = HAlloc(p, 1 + 9);
    Eterm tup = make_tuple(hp);

    hp[0] = make_arityval(9);
    hp[1] = make_small((Uint)t2_stats.modules);
    hp[2] = make_small((Uint)t2_stats.functions);
    hp[3] = make_small((Uint)t2_stats.installed);
    hp[4] = make_small((Uint)t2_stats.isel_unsupported);
    hp[5] = make_small((Uint)t2_stats.emit_failed);
    hp[6] = make_small((Uint)t2_stats.install_rejected);
    hp[7] = make_small((Uint)t2_stats.gate_rejected);
    hp[8] = make_small((Uint)t2_stats.build_failed);
    hp[9] = make_small((Uint)(t2_stats.compile_ns / 1000));
    return tup;
}

/* ------------------------------------------------------------------ *
 * Debug hooks (erts_debug:get_internal_state; code mod permission is *
 * seized by the erl_bif_info.c wrapper)                              *
 * ------------------------------------------------------------------ */

namespace {

    /* Locate the active instance + retained tables for `mod`. */
    bool t2_debug_context(Eterm mod,
                          struct erl_module_instance **mi_out,
                          const ErtsT2RetainedCode **ret_out,
                          const BeamCodeHeader **hdr_out) {
        Module *modp = erts_get_module(mod, erts_active_code_ix());

        if (modp == NULL || modp->curr.code_hdr == NULL) {
            return false;
        }

        *mi_out = &modp->curr;
        *ret_out = modp->curr.t2_retained;
        *hdr_out = modp->curr.code_hdr;
        return true;
    }

    /* The ErtsCodeInfo of F/A in `hdr`, or NULL. Walks the function
     * table so unexported functions are found too. */
    const ErtsCodeInfo *t2_find_ci(const BeamCodeHeader *hdr,
                                   Eterm func,
                                   Uint arity) {
        for (Uint i = 0; i < (Uint)hdr->num_functions; i++) {
            const ErtsCodeInfo *ci = hdr->functions[i];

            if (ci->mfa.function == func && ci->mfa.arity == arity) {
                return ci;
            }
        }
        return NULL;
    }

    Eterm t2_error2(Process *p, Eterm reason) {
        Eterm *hp = HAlloc(p, 3);
        return TUPLE2(hp, am_error, reason);
    }

    Eterm t2_status_atom(T2CompileStatus st) {
        switch (st) {
        case T2CompileStatus::Installed:
            return am_ok;
        case T2CompileStatus::IselUnsupported:
            return ERTS_MAKE_AM("isel_unsupported");
        case T2CompileStatus::EmitFailed:
            return ERTS_MAKE_AM("emit_failed");
        case T2CompileStatus::RejectedBp:
            return ERTS_MAKE_AM("rejected_bp");
        case T2CompileStatus::RejectedProloguePatched:
            return ERTS_MAKE_AM("rejected_prologue");
        case T2CompileStatus::RejectedReach:
            return ERTS_MAKE_AM("rejected_reach");
        case T2CompileStatus::RejectedDup:
            return ERTS_MAKE_AM("rejected_dup");
        case T2CompileStatus::RejectedGate:
            return ERTS_MAKE_AM("rejected_gate");
        case T2CompileStatus::RejectedState:
        default:
            return ERTS_MAKE_AM("rejected_state");
        }
    }

} /* anonymous namespace */

extern "C" Eterm erts_t2_debug_install(Process *p,
                                       Eterm mod,
                                       Eterm func,
                                       Eterm arity) {
    struct erl_module_instance *mi;
    const ErtsT2RetainedCode *ret;
    const BeamCodeHeader *hdr;

    if (!is_atom(mod) || !is_atom(func) || !is_small(arity) ||
        signed_val(arity) < 0) {
        return am_undefined;
    }
    if (!t2_debug_context(mod, &mi, &ret, &hdr) || ret == NULL) {
        return am_undefined;
    }

    T2CompileStatus st = T2CompileStatus::RejectedState;
    std::string build_err;
    bool ran = false;

    T2BuildStatus bst = t2_build_for_debug(
            ret,
            func,
            (unsigned)unsigned_val(arity),
            [&](T2Function &hir) {
                ran = true;
                std::string diag;

                st = t2_compile_install_one(hir, ret, hdr, mi, &diag);
                if (st != T2CompileStatus::Installed &&
                    getenv("T2_DEBUG") != NULL) {
                    erts_fprintf(stderr,
                                 "t2_install %T:%T/%u failed: %s\n",
                                 hir.module,
                                 hir.function,
                                 (unsigned)hir.arity,
                                 diag.c_str());
                }
            },
            &build_err);

    switch (bst) {
    case T2BuildStatus::Ok:
        break;
    case T2BuildStatus::NotFound:
        return t2_error2(p, ERTS_MAKE_AM("not_found"));
    case T2BuildStatus::NotEligible:
        return t2_error2(p, ERTS_MAKE_AM("not_eligible"));
    case T2BuildStatus::Failed:
    default:
        return t2_error2(p, ERTS_MAKE_AM("build_failed"));
    }

    if (!ran) {
        return t2_error2(p, ERTS_MAKE_AM("build_failed"));
    }
    if (st == T2CompileStatus::Installed) {
        return am_ok;
    }
    return t2_error2(p, t2_status_atom(st));
}

extern "C" Eterm erts_t2_debug_jettison(Process *p,
                                        Eterm mod,
                                        Eterm func,
                                        Eterm arity) {
    struct erl_module_instance *mi;
    const ErtsT2RetainedCode *ret;
    const BeamCodeHeader *hdr;
    const ErtsCodeInfo *ci;

    (void)p;

    if (!is_atom(mod) || !is_atom(func) || !is_small(arity) ||
        signed_val(arity) < 0) {
        return am_undefined;
    }
    if (!t2_debug_context(mod, &mi, &ret, &hdr)) {
        return am_undefined;
    }

    ci = t2_find_ci(hdr, func, (Uint)unsigned_val(arity));
    if (ci == NULL) {
        return am_undefined;
    }

    if (erts_t2_jettison_function(mi, ci)) {
        return am_ok;
    }
    return ERTS_MAKE_AM("not_installed");
}

extern "C" Eterm erts_t2_debug_installed(Process *p,
                                         Eterm mod,
                                         Eterm func,
                                         Eterm arity) {
    struct erl_module_instance *mi;
    const ErtsT2RetainedCode *ret;
    const BeamCodeHeader *hdr;
    const ErtsCodeInfo *ci;
    const ErtsT2Install *inst;

    if (!is_atom(mod) || !is_atom(func) || !is_small(arity) ||
        signed_val(arity) < 0) {
        return am_undefined;
    }
    if (!t2_debug_context(mod, &mi, &ret, &hdr)) {
        return am_undefined;
    }

    ci = t2_find_ci(hdr, func, (Uint)unsigned_val(arity));
    if (ci == NULL) {
        return am_undefined;
    }

    for (inst = mi->t2_installs; inst != NULL; inst = inst->next) {
        if (inst->ci == ci) {
            /* Exercise the t2_ranges population: a PC in the blob's
             * interior must resolve back to this function's MFA. */
            ErtsCodePtr mid = (ErtsCodePtr)((const char *)inst->blob_base +
                                            (inst->blob_size / 2));
            const ErtsT2Blob *blob = erts_t2_find_blob(mid);
            Eterm find_mfa = am_undefined;
            Eterm start = erts_make_integer((Uint)(UWord)inst->blob_base, p);
            Eterm *hp;

            if (blob != NULL) {
                hp = HAlloc(p, 4);
                find_mfa = TUPLE3(hp,
                                  blob->mfa.module,
                                  blob->mfa.function,
                                  make_small(blob->mfa.arity));
            }

            hp = HAlloc(p, 4);
            return TUPLE3(hp,
                          start,
                          make_small((Uint)inst->blob_size),
                          find_mfa);
        }
    }

    return am_false;
}
