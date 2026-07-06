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
 * Deviation from 05 §15.3 recorded: the worker body runs in the
 * code-mod-permission aux callback (a normal-scheduler aux job), not on
 * a dirty CPU scheduler — the permission machinery serializes it and
 * the measured per-function compile cost (~25-130 us) is a GC-pause-
 * class stall; routing the callback through a dirty scheduler is a
 * follow-up. Only one worker can ever run (the permission is
 * exclusive), so asmjit's allocator needs no extra locking, exactly as
 * §15.3 prescribes.
 */

#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "erl_alloc.h"
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
} t2_tier_stats;

void erts_t2_tier_init(void) {
    erts_mtx_init(&t2_tier_q.lock,
                  "t2_tier_queue",
                  NIL,
                  ERTS_LOCK_FLAGS_PROPERTY_STATIC |
                          ERTS_LOCK_FLAGS_CATEGORY_GENERIC);
}

static void t2_tier_worker(void *arg);

static void t2_tier_kick(void) {
    int kick = 0;

    erts_mtx_lock(&t2_tier_q.lock);
    if (!t2_tier_q.worker_kicked && t2_tier_q.head != t2_tier_q.tail) {
        t2_tier_q.worker_kicked = 1;
        kick = 1;
    }
    erts_mtx_unlock(&t2_tier_q.lock);

    if (kick) {
        /* Seized immediately -> run here; otherwise the callback runs
         * when the current holder releases. */
        if (erts_try_seize_code_mod_permission_aux(t2_tier_worker, NULL)) {
            t2_tier_worker(NULL);
        }
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
                                                   m);
            t2_tier_stats.installed += installed;
            t2_tier_stats.failed += m - installed;
        }
    }

    erts_release_code_mod_permission();

    /* A trip may have raced the drain: re-kick if needed. */
    t2_tier_kick();
}

Eterm erts_t2_tier_stats_term(Process *p) {
    Eterm *hp = HAlloc(p, 8);

    return TUPLE7(hp,
                  make_small((Uint)t2_tier_stats.trips),
                  make_small((Uint)t2_tier_stats.stability_resets),
                  make_small((Uint)t2_tier_stats.enqueued),
                  make_small((Uint)t2_tier_stats.dropped),
                  make_small((Uint)t2_tier_stats.compiled),
                  make_small((Uint)t2_tier_stats.installed),
                  make_small((Uint)t2_tier_stats.failed));
}
