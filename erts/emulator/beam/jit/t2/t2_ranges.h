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
 * T2-Full tier-2 JIT: blob range registration (PLAN/T2FULL/07 §5).
 *
 * When a tier-2 blob is installed (P1+), its executable range must be
 * translatable back to an MFA + resume info for c_p->i resume-PC
 * translation and resume-stub MFA/line lookup (T2/08 §4.3). beam_ranges
 * cannot host these: after find_range, erts_lookup_function_info casts
 * `rp->start` to a BeamCodeHeader* and walks hdr->functions / line_table
 * (beam_ranges.c:260), which a T2 blob does not have -- every existing
 * lookup would misinterpret it. And a blob's lifetime follows per-blob
 * jettison (trace enable, watchpoint, eviction), decoupled from the
 * module code_ix staging that drives erts_start_staging_ranges, so
 * folding blobs in would tie eviction to code-load transactions.
 *
 * Hence a separate, parallel sorted-interval class keyed on blob
 * {start,end}, with its own lookup returning a T2 blob descriptor
 * directly (never a BeamCodeHeader).
 *
 * P0 scope: the design + the register/lookup/deregister API + accounting,
 * exercised only by a self-test (T2_SELFTEST). No blobs exist to register
 * yet; population arrives with P1 install. The current implementation is a
 * mutex-guarded sorted array; the lockless thread-progress discipline that
 * beam_ranges uses for its `mid` cache is deferred to when the lookup is
 * actually on a hot path.
 */

#ifndef ERL_T2_RANGES_H__
#define ERL_T2_RANGES_H__

#include "sys.h"
#include "beam_code.h" /* ErtsCodePtr, ErtsCodeMFA */

/* Descriptor for one installed tier-2 blob's executable range. */
typedef struct {
    ErtsCodePtr start;   /* first byte of the blob                       */
    ErtsCodePtr end;     /* one byte past the blob                       */
    ErtsCodeMFA mfa;     /* originating function                         */
    void *resume_tab;    /* opaque resume table; NULL until P1 populates */
} ErtsT2Blob;

/* One-time init (mutex + accounting). Called from beamasm_init(). */
void erts_t2_ranges_init(void);

/* Register a blob's range. \p start must be unique and \p start < \p end;
 * ranges must not overlap. Returns 1 on success, 0 on a bad/overlapping
 * range. \p mfa is copied. */
int erts_t2_register_blob(ErtsCodePtr start,
                          ErtsCodePtr end,
                          const ErtsCodeMFA *mfa,
                          void *resume_tab);

/* Remove the blob registered at \p start. Returns 1 if removed, 0 if no
 * blob starts there. */
int erts_t2_deregister_blob(ErtsCodePtr start);

/* Find the blob whose range contains \p pc (start <= pc < end), or NULL.
 * The returned pointer is valid until the next register/deregister. */
const ErtsT2Blob *erts_t2_find_blob(ErtsCodePtr pc);

/* Bytes held by the blob range array, for accounting. */
UWord erts_t2_blobs_sz(void);

/* Registration/lookup/removal self-test over synthetic ranges. Returns 0
 * on success. Run from beamasm_init() under T2_SELFTEST. */
int erts_t2_ranges_selftest(void);

#endif /* ERL_T2_RANGES_H__ */
