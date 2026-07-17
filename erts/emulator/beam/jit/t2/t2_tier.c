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
 * T2-Full tier-2 JIT: profile-driven tier-up (P2 commit 9;
 * PLAN/T2FULL/09 §1, PLAN/T2/05 §15).
 *
 * The T1 profiling sequence (arm/instr_common.cpp,
 * emit_t2_profile_sequence) trips into erts_t2_profile_trip when an
 * eligible loop function's call counter crosses its threshold on
 * scheduler 1. The trip applies the stability rule, marks the record
 * pending, pushes a compile job onto a small ring and kicks the single
 * worker, which runs under code-modification permission
 * (erts_try_seize_code_mod_permission_aux — the same permission the
 * debug install hook holds) and drains the ring, compiling + installing
 * each function through the standard t2_compile_install_one pipeline
 * with the profile's observed facts plugged into the speculation
 * inserter. Per-module decode caching happens one level down
 * (t2_build_selected builds all queued functions of one module from a
 * single decode).
 *
 * Off the hot path (P2.6 blocker A): the tripping scheduler only
 * enqueues the job, marks the record pending and schedules the
 * compile as misc aux work on a normal scheduler other than
 * scheduler 1 (t2_tier_compiler_tid) — it never compiles inline and
 * returns to running Erlang immediately. The aux callback
 * (t2_tier_seize_and_run) is a canonical re-seizing trampoline (cf.
 * load_nif_1st_finisher / trace_session_destroy_aux in erl_nif.c /
 * erl_bif_trace.c): the aux callback does NOT inherit the permission,
 * it (re-)acquires code-mod permission and only then runs the worker
 * body, which compiles + installs + disarms under that permission and
 * releases it.
 *
 * A dirty CPU scheduler was ruled out (05 §15.3's original target):
 * dirty schedulers cannot run aux work at all (erl_process.c:
 * ERTS_INTERNAL_ERROR "Executing aux work on a dirty scheduler.") and
 * code-mod permission can only be released from a normal scheduler
 * (release_code_permission schedules queued aux waiters via
 * erts_schedule_misc_aux_work((int)esdp->no,...) under
 * ASSERT(esdp->type == ERTS_SCHED_NORMAL)). Since the install + disarm
 * steps assert code-mod permission (t2_install.c), the
 * permission-holding work must stay on a normal scheduler; routing it
 * to a dirty scheduler would require splitting codegen from install
 * into a detached-blob handoff (address-stability-across-reload risk)
 * plus a process/NIF dispatch harness — larger and racier for no gain
 * on the measured hot path, since the profiled process runs on
 * scheduler 1 and the compile now lands on a different normal
 * scheduler in parallel. Only one worker can ever run (the permission
 * is exclusive), so asmjit's allocator needs no extra locking, exactly
 * as §15.3 prescribes.
 */

#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "erl_alloc.h"
#include "big.h"
#include "beam_file.h"
#include "code_ix.h"
#include "module.h"
#include "erl_process.h"
#include "erl_map.h" /* is_flatmap / is_hashmap subtag constants */
#include "erl_fun.h" /* ErlFunThing / fun identity for #2a target profiling */

#include "t2_retain.h"
#include "t2_install.h" /* erts_t2_function_prologue_claimed (caller dedup) */

/* The shared throwaway record (see t2_retain.h): the store target for
 * every scheduler but scheduler 1. Threshold ~0 never trips. One full
 * stride so neighbouring data never shares its line. */
static union {
    ErtsT2Profile p;
    char pad__[ERTS_T2_PROFILE_STRIDE];
} t2_throwaway = {{.threshold = ERTS_T2_TIER_PENDING}};

UWord erts_t2_profile_throwaway_addr(void) {
    return (UWord)&t2_throwaway.p;
}

/* ------------------------------------------------------------------ *
 * The compile queue (05 §15.3): a small ring; overflow drops the job  *
 * and rearms the counter (it retrips later).                          *
 * ------------------------------------------------------------------ */

#define T2_TIER_QUEUE_SIZE 256

typedef struct {
    Eterm module;
    Uint32 fn_index;
    ErtsT2Profile *rec; /* identity check against the live retention */
} T2TierJob;

static struct {
    erts_mtx_t lock;
    T2TierJob jobs[T2_TIER_QUEUE_SIZE];
    unsigned head; /* pop position  */
    unsigned tail; /* push position */
    int worker_kicked;
} t2_tier_q;

/* Statistics (advisory; sched-1/worker writes). */
static struct {
    Uint64 trips;
    Uint64 stability_resets;
    Uint64 enqueued;
    Uint64 dropped;
    Uint64 compiled;
    Uint64 installed;
    Uint64 failed;
    Uint64 rejected; /* install-quality gate refusals (P2.6 blocker B) */
    Uint64 caller_enqueued; /* caller-directed tier-up enqueues (task #91) */
} t2_tier_stats;

void erts_t2_tier_init(void) {
    erts_mtx_init(&t2_tier_q.lock,
                  "t2_tier_queue",
                  NIL,
                  ERTS_LOCK_FLAGS_PROPERTY_STATIC |
                          ERTS_LOCK_FLAGS_CATEGORY_GENERIC);
}

static void t2_tier_worker(void *arg);
static void t2_tier_seize_and_run(void *arg);

/* Which normal scheduler runs the compile. misc aux work tids
 * 1..erts_no_schedulers map one-to-one to the normal scheduler threads
 * (0 and >erts_no_schedulers are aux threads, which must not be used:
 * releasing code-mod permission asserts a normal scheduler). P2.5
 * confines profiling/tripping to scheduler 1, so route the compile to
 * a different normal scheduler when one exists; the profiled process on
 * scheduler 1 then never pauses for it. */
static int t2_tier_compiler_tid(void) {
    Uint n = erts_no_schedulers;

    return (n >= 2) ? 2 : 1;
}

/* Aux-work trampoline: the tripping scheduler schedules this on a
 * normal scheduler so the compile+install runs off the hot path. Per
 * the code-mod aux protocol the callback does NOT inherit the
 * permission — it must (re-)seize. On failure it has re-enqueued
 * itself and runs again when the current holder releases; only when it
 * holds the permission does it run the worker body (which releases it).
 */
static void t2_tier_seize_and_run(void *arg) {
    (void)arg;

    if (!erts_try_seize_code_mod_permission_aux(t2_tier_seize_and_run,
                                                NULL)) {
        return;
    }

    t2_tier_worker(NULL);
}

static void t2_tier_kick(void) {
    int kick = 0;

    erts_mtx_lock(&t2_tier_q.lock);
    if (!t2_tier_q.worker_kicked && t2_tier_q.head != t2_tier_q.tail) {
        t2_tier_q.worker_kicked = 1;
        kick = 1;
    }
    erts_mtx_unlock(&t2_tier_q.lock);

    if (kick) {
        /* Off the hot path: hand the compile+install to a normal
         * scheduler's aux-work slot and return to running Erlang
         * immediately. worker_kicked stays 1 until the worker drains
         * the ring empty, so concurrent trips enqueue without
         * re-scheduling (no lost or double compile). */
        erts_schedule_misc_aux_work(t2_tier_compiler_tid(),
                                    t2_tier_seize_and_run,
                                    NULL);
    }
}

/* Classify a sampled argument into a single ERTS_T2_TY_* bit. Ordered
 * by the cheapest / most common tests first (small ints and conses
 * dominate the loop-shaped hot set). is_list is a non-empty list (cons
 * cell); is_nil is []; both distinct so the consumer can speculate an
 * empty-or-nonempty entry. Every term maps to exactly one bit, so the
 * OR-accumulation in the trip handler yields "how many distinct classes
 * has this argument been" -- one bit == monomorphic. */
static ERTS_INLINE Uint16 t2_sample_type_bit(Eterm t) {
    if (is_small(t))     return ERTS_T2_TY_SMALL;
    if (is_nil(t))       return ERTS_T2_TY_NIL;
    if (is_list(t))      return ERTS_T2_TY_CONS;
    if (is_atom(t))      return ERTS_T2_TY_ATOM;
    if (is_tuple(t))     return ERTS_T2_TY_TUPLE;
    if (is_big(t))       return ERTS_T2_TY_BIG;
    if (is_flatmap(t))   return ERTS_T2_TY_MAP_FLAT;
    if (is_hashmap(t))   return ERTS_T2_TY_MAP_HASH;
    if (is_float(t))     return ERTS_T2_TY_FLOAT;
    if (is_bitstring(t)) return ERTS_T2_TY_BINARY;
    if (is_any_fun(t))   return ERTS_T2_TY_FUN;
    return ERTS_T2_TY_OTHER;
}

/* Record the identity of a fun observed in a fun-typed entry argument
 * (02 §7.5). One monomorphic-target slot per function, locked to the
 * first argument position a fun was seen in: the first fun wins the
 * slot, a second distinct fun in that same position marks it POLY,
 * funs in other positions are ignored (single-slot measurement).
 * fun_flags accumulates whether env-free (inlinable) funs or closures
 * have been seen. */
static ERTS_INLINE void t2_sample_fun(ErtsT2Profile *p, Uint32 arg, Eterm t) {
    const ErlFunThing *ft = (const ErlFunThing *)fun_val(t);
    const void *id = (const void *)ft->entry.disp;

    p->fun_flags |= (fun_num_free(ft) == 0) ? ERTS_T2_FUNF_ENVFREE
                                            : ERTS_T2_FUNF_CLOSURE;
    if (p->seen_fun == NULL) {
        p->seen_fun = id;
        p->fun_arg = arg;
    } else if (p->seen_fun != ERTS_T2_FUN_POLY && arg == p->fun_arg &&
               p->seen_fun != id) {
        p->seen_fun = ERTS_T2_FUN_POLY;
    }
}

/* Monomorphic map-shape sampling (S1b.3b, map_monomorphic_design.md):
 * record the tagged keys-tuple pointer of a flatmap entry argument,
 * single-slot like the fun target above. The first flatmap shape seen
 * wins the slot; a second distinct shape in that same argument marks it
 * POLY; flatmaps in other argument positions are ignored. The specializer
 * bakes a monomorphic shape as an O(1) guard replacing the key scan (a
 * wrong guess deopts, never a wrong result). */
static ERTS_INLINE void t2_sample_map(ErtsT2Profile *p, Uint32 arg, Eterm t) {
    Eterm keys = ((flatmap_t *) flatmap_val(t))->keys;

    if (p->map_shape == (Eterm)0) {
        p->map_shape = keys;
        p->map_shape_arg = arg;
    } else if (p->map_shape != ERTS_T2_MAP_SHAPE_POLY &&
               arg == p->map_shape_arg && p->map_shape != keys) {
        p->map_shape = ERTS_T2_MAP_SHAPE_POLY;
    }
}

/* Continuous-sampling measurement mode (#2a), gated by the same
 * T2_PROFILE_BUILDABLE flag that arms buildable loops: never tier up,
 * just keep recording the type/target samples. Cached (the flag is
 * fixed for the VM's life); racy init is idempotent. */
static ERTS_INLINE int t2_measure_mode(void) {
    static int m = -1;
    if (m < 0) {
        m = (getenv("T2_PROFILE_BUILDABLE") != NULL);
    }
    return m;
}

/* Caller-directed tier-up (task #91) is ON by default; T2_NO_CALLER_TIERUP
 * disables it (the lever, cached like t2_measure_mode; racy init is
 * idempotent). */
static ERTS_INLINE int t2_caller_tierup_enabled(void) {
    static int m = -1;
    if (m < 0) {
        m = (getenv("T2_NO_CALLER_TIERUP") == NULL);
    }
    return m;
}

/* T2_CALLER_TRACE: one-time de-risking trace of the caller resolution
 * (confirms c_p->stop[0] is the tripping loop's frame CP). Cached. */
static ERTS_INLINE int t2_caller_trace(void) {
    static int m = -1;
    if (m < 0) {
        m = (getenv("T2_CALLER_TRACE") != NULL);
    }
    return m;
}

/* Caller-directed tier-up (task #91): when a hot, loop-shaped callee \p p
 * trips, ALSO enqueue its immediate caller so caller-side transforms
 * (P1 fold-inline, the foldl intrinsic) fire on the function holding the
 * call site. Under natural tier-up the counter trips on the cold callee
 * (e.g. lists:foldl or a driver loop), never on the caller, so those
 * sites otherwise stay unrealized.
 *
 * The trip fragment's added Update::eStack synced c_p->stop to E, so
 * c_p->stop[0] is the caller's return address: aarch64 uses the one-word
 * ERTS_FRAME_LAYOUT_RA frame (make_cp is identity), and the T1
 * breakpoint trampoline pushes the CP via enter_erlang_frame just before
 * this profiling sequence runs. Resolve it to an MFA + code-header
 * function index and enqueue {module, fn_index} on the same ring the
 * callee uses (rec = NULL: the worker keys only on module + fn_index).
 *
 * Best-effort and IN ADDITION to the callee: a caller that is
 * unresolved, in a non-retained module, self-recursive, or already
 * prologue-claimed (installed) is simply skipped. One level only; a
 * tail-called site already yields two-levels-up naturally. */
static void t2_tier_enqueue_caller(Process *c_p, ErtsT2Profile *p) {
    ErtsCodePtr caller_cp;
    const ErtsCodeMFA *cmfa;
    Eterm caller_module;
    Module *cmodp;
    const BeamCodeHeader *code_hdr;
    Uint32 fn_index;
    UWord nf, i;
    unsigned next;

    if (!t2_caller_tierup_enabled()) {
        return;
    }

    caller_cp = (ErtsCodePtr)c_p->stop[0];
    cmfa = erts_find_function_from_pc(caller_cp);

    if (t2_caller_trace()) {
        if (cmfa == NULL) {
            erts_fprintf(stderr,
                         "t2_caller: callee=%T fn=%u cp=%p caller=<none>\n",
                         p->module,
                         (unsigned)p->fn_index,
                         (void *)caller_cp);
        } else {
            erts_fprintf(stderr,
                         "t2_caller: callee=%T fn=%u cp=%p caller=%T:%T/%u\n",
                         p->module,
                         (unsigned)p->fn_index,
                         (void *)caller_cp,
                         cmfa->module,
                         cmfa->function,
                         (unsigned)cmfa->arity);
        }
    }

    if (cmfa == NULL) {
        return; /* CP resolves to nothing (BIF/NIF/stub caller) */
    }

    caller_module = cmfa->module;
    cmodp = erts_get_module(caller_module, erts_active_code_ix());
    if (cmodp == NULL || cmodp->curr.t2_retained == NULL ||
        cmodp->curr.code_hdr == NULL) {
        return; /* non-retained caller: nothing to compile from */
    }
    code_hdr = cmodp->curr.code_hdr;

    /* The caller's index in the code-header function table: the entry
     * whose &functions[i]->mfa is the resolved cmfa (exactly the pointer
     * erts_find_function_from_pc returned). This enumeration is the one
     * the profile block and erts_t2_tier_compile_batch use
     * (t2_compile.cpp indexes code_hdr->functions[fn_index]). */
    fn_index = (Uint32)-1;
    nf = code_hdr->num_functions;
    for (i = 0; i < nf; i++) {
        if (&code_hdr->functions[i]->mfa == cmfa) {
            fn_index = (Uint32)i;
            break;
        }
    }
    if (fn_index == (Uint32)-1) {
        return; /* caller_cp in a stale/other instance than curr; skip */
    }

    /* Don't self-enqueue on self-recursion (the callee's own record
     * already tiers it up). */
    if (caller_module == p->module && fn_index == p->fn_index) {
        return;
    }

    /* Dedup: the caller has no PENDING record, so gate the re-enqueue on
     * whether its prologue is already claimed (installed). Without it,
     * every callee trip would re-enqueue an already-compiled caller (the
     * worker would rebuild it and reject the install as DUP). */
    if (erts_t2_function_prologue_claimed(code_hdr->functions[fn_index])) {
        return;
    }

    erts_mtx_lock(&t2_tier_q.lock);
    next = (t2_tier_q.tail + 1) % T2_TIER_QUEUE_SIZE;
    if (next == t2_tier_q.head) {
        /* Full: drop (high-water rule, 05 §15.3); a later callee trip
         * retries. */
        t2_tier_stats.dropped++;
        erts_mtx_unlock(&t2_tier_q.lock);
        return;
    }
    t2_tier_q.jobs[t2_tier_q.tail].module = caller_module;
    t2_tier_q.jobs[t2_tier_q.tail].fn_index = fn_index;
    t2_tier_q.jobs[t2_tier_q.tail].rec = NULL;
    t2_tier_q.tail = next;
    t2_tier_stats.caller_enqueued++;
    erts_mtx_unlock(&t2_tier_q.lock);
}

void erts_t2_profile_trip(Process *c_p,
                          ErtsT2Profile *p,
                          Eterm a0,
                          Eterm a1,
                          Eterm a2,
                          Eterm a3) {
    Eterm args[4];
    Uint32 mask = 0;
    Uint32 i;

    ERTS_CT_ASSERT(sizeof(ErtsT2Profile) <= ERTS_T2_PROFILE_STRIDE);

    if (p == &t2_throwaway.p ||
        p->threshold == ERTS_T2_TIER_PENDING || p->threshold == 0) {
        return; /* spurious (throwaway / already pending) */
    }

    t2_tier_stats.trips++;

    /* Sample the argument types at the trip (the hot sequence carries
     * only the counter; a T1 self-recursive loop runs it per
     * iteration, so every hot instruction is a tax — see
     * emit_t2_profile_sequence). Sampling is sound: a wrong "always
     * small" guess costs a deopt, never a wrong result, and the
     * stability retries below demand consecutive identical samples. */
    args[0] = a0;
    args[1] = a1;
    args[2] = a2;
    args[3] = a3;
    for (i = 0; i < ERTS_T2_PROFILE_ARGS && i < p->arity; i++) {
        Uint16 tb = t2_sample_type_bit(args[i]);
        if (!is_small(args[i])) {
            mask |= (Uint32)1 << i;
        }
        p->seen_types[i] |= tb;
        if (tb == ERTS_T2_TY_FUN) {
            t2_sample_fun(p, i, args[i]);
        } else if (tb == ERTS_T2_TY_MAP_FLAT) {
            t2_sample_map(p, i, args[i]);
        }
    }
    p->nonsmall |= mask;

    /* Measurement mode (#2a): a pure continuous sampler. Keep observing
     * every trip so the monomorphic-target slot accumulates a robust
     * mono/poly verdict, and never enqueue a compile (buildable-only
     * loops would just degrade at isel). Reset the counter and return. */
    if (t2_measure_mode()) {
        p->count = 0;
        return;
    }

    /* Profile stability (05 §15.2, simplified to the observed
     * non-small mask): an argument whose smallness is still changing
     * gets more time — reset the counter, keep the threshold, at most
     * 5 retries. */
    if (p->nonsmall != p->snapshot && p->retries < 5) {
        p->snapshot = p->nonsmall;
        p->retries++;
        p->count = 0;
        t2_tier_stats.stability_resets++;
        return;
    }

    erts_mtx_lock(&t2_tier_q.lock);
    {
        unsigned next = (t2_tier_q.tail + 1) % T2_TIER_QUEUE_SIZE;

        if (next == t2_tier_q.head) {
            /* Full: drop; the counter retrips later (high-water rule,
             * 05 §15.3). */
            t2_tier_stats.dropped++;
            erts_mtx_unlock(&t2_tier_q.lock);
            p->count = 0;
            return;
        }
        t2_tier_q.jobs[t2_tier_q.tail].module = p->module;
        t2_tier_q.jobs[t2_tier_q.tail].fn_index = p->fn_index;
        t2_tier_q.jobs[t2_tier_q.tail].rec = p;
        t2_tier_q.tail = next;
        t2_tier_stats.enqueued++;
    }
    erts_mtx_unlock(&t2_tier_q.lock);

    /* Caller-directed tier-up (task #91): the callee is now committed to
     * compile; also tier up its immediate caller so the caller-side
     * transforms fire on the function that actually holds the call site.
     * In addition to the callee; best-effort. */
    t2_tier_enqueue_caller(c_p, p);

    /* Pending sentinel: suppress duplicate enqueues (09 §1(b)). The
     * writer is scheduler 1 (us); the worker only ever writes the
     * threshold of records it dequeued, serialized by the permission. */
    p->threshold = ERTS_T2_TIER_PENDING;

    t2_tier_kick();
}

/* Worker body; runs with code-modification permission held. */
static void t2_tier_worker(void *arg) {
    (void)arg;

    for (;;) {
        T2TierJob batch[T2_TIER_QUEUE_SIZE];
        unsigned n = 0;
        unsigned i;

        erts_mtx_lock(&t2_tier_q.lock);
        while (t2_tier_q.head != t2_tier_q.tail &&
               n < T2_TIER_QUEUE_SIZE) {
            batch[n++] = t2_tier_q.jobs[t2_tier_q.head];
            t2_tier_q.head = (t2_tier_q.head + 1) % T2_TIER_QUEUE_SIZE;
        }
        if (n == 0) {
            t2_tier_q.worker_kicked = 0;
            erts_mtx_unlock(&t2_tier_q.lock);
            break;
        }
        erts_mtx_unlock(&t2_tier_q.lock);

        /* Per-module batches: one retained-code decode per module
         * (n is small; the quadratic grouping is noise). */
        for (i = 0; i < n; i++) {
            Uint32 idxs[T2_TIER_QUEUE_SIZE];
            unsigned m = 0;
            unsigned installed;
            unsigned rejected = 0;
            unsigned j;

            if (batch[i].module == THE_NON_VALUE) {
                continue; /* consumed by an earlier group */
            }
            idxs[m++] = batch[i].fn_index;
            for (j = i + 1; j < n; j++) {
                if (batch[j].module == batch[i].module) {
                    idxs[m++] = batch[j].fn_index;
                    batch[j].module = THE_NON_VALUE;
                }
            }

            t2_tier_stats.compiled += m;
            installed = erts_t2_tier_compile_batch(batch[i].module,
                                                   idxs,
                                                   m,
                                                   &rejected);
            t2_tier_stats.installed += installed;
            t2_tier_stats.rejected += rejected;
            t2_tier_stats.failed += m - installed - rejected;
        }
    }

    erts_release_code_mod_permission();

    /* A trip may have raced the drain: re-kick if needed. */
    t2_tier_kick();
}

Eterm erts_t2_tier_stats_term(Process *p) {
    /* Nine fields -> past the TUPLE8 macro; build the tuple by hand.
     * Append-only ordering: caller_enqueued (task #91) is the new tail
     * field. */
    Eterm *hp = HAlloc(p, 1 + 9);

    hp[0] = make_arityval(9);
    hp[1] = make_small((Uint)t2_tier_stats.trips);
    hp[2] = make_small((Uint)t2_tier_stats.stability_resets);
    hp[3] = make_small((Uint)t2_tier_stats.enqueued);
    hp[4] = make_small((Uint)t2_tier_stats.dropped);
    hp[5] = make_small((Uint)t2_tier_stats.compiled);
    hp[6] = make_small((Uint)t2_tier_stats.installed);
    hp[7] = make_small((Uint)t2_tier_stats.failed);
    hp[8] = make_small((Uint)t2_tier_stats.rejected);
    hp[9] = make_small((Uint)t2_tier_stats.caller_enqueued);
    return make_tuple(hp);
}

/* Diagnostic census (debug-only) of the armed profiling records — the
 * population that pays the steady-state tax until it trips (P2.5). Walks
 * the active instance of every loaded module and buckets each armed
 * record by how far its scheduler-1 counter climbed toward its trip
 * threshold. With the P2.5 sampling gate the counter advances in STRIDE
 * steps, so a bucket's `count` still approximates the sched-1 entry
 * count (quantized), which is what we want here.
 *
 * Buckets (by count c vs threshold t):
 *   zero:     c == 0        (never sampled on sched 1 — legitimately cold)
 *   vcold:    0 < c < t/10  (entered, nowhere near tripping)
 *   approach: t/10 <= c < t (climbing; a lower/size-flat threshold or
 *                            better counting might have tripped these)
 *
 * Returned flat tuple (all integers), for the t2_profile_census debug
 * BIF:
 *   { Armed, TrippedOrPending, Unarmed,
 *     N_zero,     Dist_zero,     Thr_zero,
 *     N_vcold,    Dist_vcold,    Thr_vcold,
 *     N_approach, Dist_approach, Thr_approach }
 * where per bucket: N = record count, Dist = sum(threshold - count)
 * (distance still to trip), Thr = sum(threshold) (a size proxy, since
 * threshold = base*sqrt(size+1)). */
Eterm erts_t2_profile_census_term(Process *p) {
    ErtsCodeIndex code_ix = erts_active_code_ix();
    int i, num = module_code_size(code_ix);
    Uint64 armed = 0, tripped = 0, unarmed = 0;
    Uint64 n[3] = {0, 0, 0};
    Uint64 dist[3] = {0, 0, 0};
    Uint64 thr[3] = {0, 0, 0};
    Uint64 vals[12];
    Eterm *hp;
    int k;

    for (i = 0; i < num; i++) {
        Module *modp = module_code(i, code_ix);
        ErtsT2RetainedCode *ret;
        Sint32 f;

        if (modp == NULL) {
            continue;
        }
        /* Active instance only: the tax is paid by the running code. */
        ret = modp->curr.t2_retained;
        if (ret == NULL || ret->profiles == NULL) {
            continue;
        }
        for (f = 0; f < ret->function_count; f++) {
            ErtsT2Profile *rec =
                    (ErtsT2Profile *)((char *)ret->profiles +
                                      (size_t)f * ERTS_T2_PROFILE_STRIDE);
            Uint32 t = rec->threshold;
            Uint32 c = rec->count;
            int b;

            if (t == 0) {
                unarmed++;
                continue;
            }
            if (t == ERTS_T2_TIER_PENDING) {
                tripped++; /* tripped: cross-check against tier_stats */
                continue;
            }
            armed++;
            if (c == 0) {
                b = 0;
            } else if (c < t / 10) {
                b = 1;
            } else {
                b = 2;
            }
            n[b]++;
            dist[b] += (c < t) ? (Uint64)(t - c) : 0;
            thr[b] += (Uint64)t;
        }
    }

    vals[0] = armed;
    vals[1] = tripped;
    vals[2] = unarmed;
    for (k = 0; k < 3; k++) {
        vals[3 + k * 3] = n[k];
        vals[4 + k * 3] = dist[k];
        vals[5 + k * 3] = thr[k];
    }

    /* Materialize the (possibly bignum) integers first, then the tuple. */
    {
        Eterm terms[12];

        for (k = 0; k < 12; k++) {
            terms[k] = erts_make_integer((Uint)vals[k], p);
        }
        hp = HAlloc(p, 1 + 12);
        hp[0] = make_arityval(12);
        for (k = 0; k < 12; k++) {
            hp[1 + k] = terms[k];
        }
        return make_tuple(hp);
    }
}
