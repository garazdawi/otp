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
 * T2-Full tier-2 JIT: blob range class. See t2_ranges.h for the design.
 *
 * A single sorted-by-start interval array, guarded by a mutex. Blobs do
 * not overlap, so binary search finds the unique containing blob. Unlike
 * beam_ranges there is no per-code_ix staging: a blob's lifetime is
 * individual (per-blob jettison), so registration and removal mutate the
 * one live array directly.
 */

#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_alloc.h"
#include "beam_code.h"

#include "t2_ranges.h"

Uint64 erts_t2_backedge_yields;
Uint64 erts_t2_backedge_resumes;
Uint64 erts_t2_callsite_deopts;
Uint64 erts_t2_rollback_deopts;
Uint64 erts_t2_range_deopts;

static ErtsT2Blob *blobs;   /* sorted by start */
static Sint blob_count;
static Sint blob_allocated;
static erts_mtx_t blobs_mtx;
static erts_atomic_t blobs_bytes;

void erts_t2_ranges_init(void) {
    blobs = NULL;
    blob_count = 0;
    blob_allocated = 0;
    erts_mtx_init(&blobs_mtx,
                  "t2_ranges",
                  NIL,
                  ERTS_LOCK_FLAGS_PROPERTY_STATIC |
                          ERTS_LOCK_FLAGS_CATEGORY_GENERIC);
    erts_atomic_init_nob(&blobs_bytes, 0);
}

UWord erts_t2_blobs_sz(void) {
    return (UWord)erts_atomic_read_nob(&blobs_bytes);
}

/* Binary search for the first index whose blob->start >= key. Caller holds
 * the mutex. */
static Sint lower_bound(ErtsCodePtr key) {
    Sint lo = 0, hi = blob_count;

    while (lo < hi) {
        Sint mid = lo + (hi - lo) / 2;

        if (blobs[mid].start < key) {
            lo = mid + 1;
        } else {
            hi = mid;
        }
    }
    return lo;
}

static void ensure_capacity(Sint need) {
    Sint new_alloc;
    ErtsT2Blob *new_blobs;
    UWord old_bytes, new_bytes;

    if (need <= blob_allocated) {
        return;
    }

    new_alloc = blob_allocated == 0 ? 8 : blob_allocated * 2;
    while (new_alloc < need) {
        new_alloc *= 2;
    }

    old_bytes = (UWord)blob_allocated * sizeof(ErtsT2Blob);
    new_bytes = (UWord)new_alloc * sizeof(ErtsT2Blob);

    new_blobs = erts_realloc(ERTS_ALC_T_T2_CODE, blobs, new_bytes);
    blobs = new_blobs;
    blob_allocated = new_alloc;

    erts_atomic_add_nob(&blobs_bytes, (erts_aint_t)(new_bytes - old_bytes));
}

int erts_t2_register_blob(ErtsCodePtr start,
                          ErtsCodePtr end,
                          const ErtsCodeMFA *mfa,
                          const void *code_hdr,
                          ErtsT2ResumeTab *resume_tab) {
    Sint idx;
    int ok = 1;

    if (start >= end) {
        return 0;
    }

    erts_mtx_lock(&blobs_mtx);

    idx = lower_bound(start);

    /* Reject a duplicate start or any overlap with an adjacent range. */
    if (idx < blob_count && blobs[idx].start == start) {
        ok = 0;
    } else if (idx < blob_count && end > blobs[idx].start) {
        ok = 0; /* overlaps the range to the right */
    } else if (idx > 0 && start < blobs[idx - 1].end) {
        ok = 0; /* overlaps the range to the left */
    }

    if (ok) {
        ensure_capacity(blob_count + 1);
        sys_memmove(&blobs[idx + 1],
                    &blobs[idx],
                    (blob_count - idx) * sizeof(ErtsT2Blob));
        blobs[idx].start = start;
        blobs[idx].end = end;
        blobs[idx].mfa = *mfa;
        blobs[idx].code_hdr = code_hdr;
        blobs[idx].resume_tab = resume_tab;
        blob_count++;
    }

    erts_mtx_unlock(&blobs_mtx);
    return ok;
}

int erts_t2_deregister_blob(ErtsCodePtr start) {
    Sint idx;
    int found = 0;

    erts_mtx_lock(&blobs_mtx);

    idx = lower_bound(start);
    if (idx < blob_count && blobs[idx].start == start) {
        if (blobs[idx].resume_tab != NULL) {
            erts_free(ERTS_ALC_T_T2_CODE, blobs[idx].resume_tab);
        }
        sys_memmove(&blobs[idx],
                    &blobs[idx + 1],
                    (blob_count - idx - 1) * sizeof(ErtsT2Blob));
        blob_count--;
        found = 1;
    }

    erts_mtx_unlock(&blobs_mtx);
    return found;
}

const ErtsT2Blob *erts_t2_find_blob(ErtsCodePtr pc) {
    const ErtsT2Blob *res = NULL;
    Sint idx;

    erts_mtx_lock(&blobs_mtx);

    /* lower_bound(pc) is the first blob starting at or after pc; the
     * containing blob (if any) is that one when it starts exactly at pc,
     * else the one before. */
    idx = lower_bound(pc);
    if (idx < blob_count && blobs[idx].start == pc) {
        res = &blobs[idx];
    } else if (idx > 0 && pc >= blobs[idx - 1].start &&
               pc < blobs[idx - 1].end) {
        res = &blobs[idx - 1];
    }

    erts_mtx_unlock(&blobs_mtx);
    return res;
}

/* ------------------------------------------------------------------ *
 * Self-test (T2_SELFTEST)                                            *
 * ------------------------------------------------------------------ */

#define T2R_CHECK(Cond)                                                        \
    do {                                                                       \
        if (!(Cond)) {                                                         \
            erts_fprintf(stderr,                                               \
                         "t2 ranges selftest failure at %s:%d: %s\n",          \
                         __FILE__,                                             \
                         __LINE__,                                             \
                         #Cond);                                               \
            return __LINE__;                                                   \
        }                                                                      \
    } while (0)

int erts_t2_ranges_selftest(void) {
    /* Synthetic addresses; never dereferenced. Three non-overlapping
     * ranges registered out of order to exercise sorted insertion. */
    byte *b1 = (byte *)0x100000;
    byte *b2 = (byte *)0x200000;
    byte *b3 = (byte *)0x300000;
    ErtsCodeMFA m1 = {am_erlang, am_ok, 1};
    ErtsCodeMFA m2 = {am_erlang, am_error, 2};
    ErtsCodeMFA m3 = {am_erlang, am_true, 0};
    const ErtsT2Blob *f;
    UWord sz0 = erts_t2_blobs_sz();

    ErtsT2ResumeTab *tab = erts_alloc(ERTS_ALC_T_T2_CODE,
                                      sizeof(ErtsT2ResumeTab));

    tab->count = 1;
    tab->flag_back = 24;
    tab->entries[0].offset = 0x40;
    tab->entries[0].t1_demote = (ErtsCodePtr)(b1 + 0x18);

    T2R_CHECK(erts_t2_register_blob(b2, b2 + 0x1000, &m2, NULL, NULL));
    T2R_CHECK(erts_t2_register_blob(b1, b1 + 0x1000, &m1, NULL, NULL));
    T2R_CHECK(erts_t2_register_blob(b3, b3 + 0x1000, &m3, NULL, tab));

    /* Duplicate start and an overlapping range must be rejected. */
    T2R_CHECK(!erts_t2_register_blob(b1, b1 + 0x10, &m1, NULL, NULL));
    T2R_CHECK(!erts_t2_register_blob(b1 + 0x100, b1 + 0x2000, &m1, NULL,
                                     NULL));

    /* Interior, boundary, and gap lookups. */
    f = erts_t2_find_blob(b1);
    T2R_CHECK(f != NULL && f->mfa.function == am_ok);
    f = erts_t2_find_blob(b1 + 0xfff);
    T2R_CHECK(f != NULL && f->mfa.function == am_ok);
    f = erts_t2_find_blob(b1 + 0x1000); /* one past end: no blob */
    T2R_CHECK(f == NULL);
    f = erts_t2_find_blob(b3 + 0x10);
    T2R_CHECK(f != NULL && f->mfa.arity == 0 && f->resume_tab == tab);
    T2R_CHECK(f->resume_tab->count == 1 &&
              f->resume_tab->entries[0].offset == 0x40);
    f = erts_t2_find_blob((byte *)0x50000); /* below all */
    T2R_CHECK(f == NULL);
    f = erts_t2_find_blob((byte *)0x400000); /* above all */
    T2R_CHECK(f == NULL);

    T2R_CHECK(erts_t2_blobs_sz() >= sz0);

    /* Remove the middle, confirm the others still resolve and it does not. */
    T2R_CHECK(erts_t2_deregister_blob(b2));
    T2R_CHECK(erts_t2_find_blob(b2 + 0x10) == NULL);
    T2R_CHECK(erts_t2_find_blob(b1) != NULL);
    T2R_CHECK(erts_t2_find_blob(b3 + 0x10) != NULL);
    T2R_CHECK(!erts_t2_deregister_blob(b2)); /* already gone */

    /* Clean up so the self-test leaves no residue (deregistration
     * frees b3's resume tab). */
    T2R_CHECK(erts_t2_deregister_blob(b1));
    T2R_CHECK(erts_t2_deregister_blob(b3));

    return 0;
}
