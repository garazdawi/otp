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

#include <ctime>
#include <string>

extern "C"
{
#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

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

#include "t2_hir.hpp"
#include "t2_isel.hpp"
#include "t2_loop.hpp"
#include "t2_lir.hpp"
#include "t2_emit.hpp"
#include "t2_spec.hpp"

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
        RejectedState
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

    /* Lower + emit + install one built HIR function against the loaded
     * module instance. Called from inside the builder's emit callback
     * (the module decode is still alive). */
    T2CompileStatus t2_compile_install_one(T2Function &hir,
                                           const ErtsT2RetainedCode *ret,
                                           const BeamCodeHeader *code_hdr,
                                           struct erl_module_instance *mi,
                                           std::string *diag) {
        T2IselContext ctx;
        T2LirFunction lir;
        T2EmitResult blob;
        std::string err;

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
            if (recovered) {
                T2LoopInfo li;

                t2_loop_info(hir, &li);

                /* Speculation insertion (P2 commit 4): entry-type +
                 * loop-carried "observed small" speculation with
                 * flag-checked arithmetic and fused guards. Every
                 * conversion is re-proven below by the full validator
                 * (born-checked: run_speculation_checks) and the
                 * window validator; any failure degrades to T1,
                 * loudly. T2_NO_SPEC=1 forces the identity pipeline
                 * (the A/B lever for the measurement legs). */
                if (getenv("T2_NO_SPEC") == NULL) {
                    T2DefaultFactSource facts(hir);
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
                    if (spec_changed && !t2_validate(hir, &err)) {
                        if (diag) {
                            *diag = "post-speculation validate: " + err;
                        }
                        return T2CompileStatus::IselUnsupported;
                    }
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

        ErtsT2InstallResult res =
                erts_t2_install(mi,
                                ci,
                                blob.entry,
                                blob.base,
                                blob.size,
                                blob.rw_base,
                                blob.resume_offsets.empty()
                                        ? NULL
                                        : blob.resume_offsets.data(),
                                (Uint32)blob.resume_offsets.size());

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

    if (!erts_jit_t2_force || ret == NULL || code_hdr == NULL ||
        mi == NULL) {
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

extern "C" Eterm erts_t2_debug_stats(Process *p) {
    Eterm *hp = HAlloc(p, 9);

    return TUPLE8(hp,
                  make_small((Uint)t2_stats.modules),
                  make_small((Uint)t2_stats.functions),
                  make_small((Uint)t2_stats.installed),
                  make_small((Uint)t2_stats.isel_unsupported),
                  make_small((Uint)t2_stats.emit_failed),
                  make_small((Uint)t2_stats.install_rejected),
                  make_small((Uint)t2_stats.build_failed),
                  make_small((Uint)(t2_stats.compile_ns / 1000)));
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
                st = t2_compile_install_one(hir, ret, hdr, mi, nullptr);
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
