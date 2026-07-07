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

#include "t2_retain.h"

/* The shared throwaway record (see t2_retain.h): the store target for
 * every scheduler but scheduler 1. Threshold ~0 never trips. One full
 * stride so neighbouring data never shares its line. */
static union {
    ErtsT2Profile p;
    char pad__[ERTS_T2_PROFILE_STRIDE];
} t2_throwaway = {{0, ERTS_T2_TIER_PENDING, 0, 0, 0, 0, 0, 0}};

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

void erts_t2_profile_trip(ErtsT2Profile *p,
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
    for (i = 0; i < 4 && i < p->arity; i++) {
        if (!is_small(args[i])) {
            mask |= (Uint32)1 << i;
        }
    }
    p->nonsmall |= mask;

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
    Eterm *hp = HAlloc(p, 9);

    return TUPLE8(hp,
                  make_small((Uint)t2_tier_stats.trips),
                  make_small((Uint)t2_tier_stats.stability_resets),
                  make_small((Uint)t2_tier_stats.enqueued),
                  make_small((Uint)t2_tier_stats.dropped),
                  make_small((Uint)t2_tier_stats.compiled),
                  make_small((Uint)t2_tier_stats.installed),
                  make_small((Uint)t2_tier_stats.failed),
                  make_small((Uint)t2_tier_stats.rejected));
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
