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

/* The resume table (P2 commit 5; PLAN/T2FULL/09 §5, PLAN/T2/08 §4.5).
 * One per blob with recovered loops: the sorted, blob-relative offsets
 * of every back-edge resume PC (the addresses a back-edge yield stores
 * into c_p->i), the fixed distance back from a resume PC to its
 * in-blob tombstone flag word, and a PER-ENTRY T1 translation target
 * (P2 commit 8): the state saved at ANY back-edge yield is a valid
 * fresh-call argument vector, but its target differs by demote class —
 * a self-recursion back edge translates to the function's own
 * L_f + TEST_YIELD_RETURN_OFFSET (the post-yield T1 body, so the
 * translated resume charges no extra reduction), an intrinsic
 * (callee-demote) back edge to the intrinsic call site's T1 PC, which
 * re-executes call_ext over the saved callee vector. Owned by the
 * ranges class: freed on deregistration. */
typedef struct {
    Uint32 offset;         /* blob-relative resume PC                   */
    ErtsCodePtr t1_demote; /* c_p->i translation target for this entry  */
} ErtsT2ResumeEntry;

typedef struct {
    Uint32 count;
    Uint32 flag_back;      /* resume PC - flag word distance (bytes)    */
    ErtsT2ResumeEntry entries[1]; /* count entries, offset-ascending    */
} ErtsT2ResumeTab;

/* Descriptor for one installed tier-2 blob's executable range. */
typedef struct {
    ErtsCodePtr start;   /* first byte of the blob                       */
    ErtsCodePtr end;     /* one byte past the blob                       */
    ErtsCodeMFA mfa;     /* originating function                         */
    const void *code_hdr; /* owning instance's BeamCodeHeader — the
                           * stable instance identity (survives the
                           * curr->old struct copy at delete_module);
                           * check_process_code compares against it     */
    ErtsT2ResumeTab *resume_tab; /* NULL when the blob has no loops     */
} ErtsT2Blob;

/* Back-edge yield/resume counters (racy, monitoring only): incremented
 * by blob code at the cold yield setup / resume stub. */
extern Uint64 erts_t2_backedge_yields;
extern Uint64 erts_t2_backedge_resumes;

/* Callsite-class deopt counter (racy, monitoring only): incremented by
 * the cold fail trampoline of every T2_OP_SPEC_CALLSITE side exit (the
 * maps:fold specialization's "re-execute the erased call" deopts), so
 * deopt storms are visible — no re-tier machinery exists. */
extern Uint64 erts_t2_callsite_deopts;

/* One-time init (mutex + accounting). Called from beamasm_init(). */
void erts_t2_ranges_init(void);

/* Register a blob's range. \p start must be unique and \p start < \p end;
 * ranges must not overlap. Returns 1 on success, 0 on a bad/overlapping
 * range. \p mfa is copied; \p resume_tab ownership transfers on success
 * (deregistration frees it). */
int erts_t2_register_blob(ErtsCodePtr start,
                          ErtsCodePtr end,
                          const ErtsCodeMFA *mfa,
                          const void *code_hdr,
                          ErtsT2ResumeTab *resume_tab);

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
