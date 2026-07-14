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
 * T2-Full tier-2 JIT: foldl-class lists intrinsics (P2 commit 8;
 * PLAN/T2FULL/09 §8, PLAN/T2/04 §10.4, PLAN/T2/08 §4).
 *
 * Recognizes monomorphic non-tail `call_ext` sites to the
 * tail-recursive lists higher-order wrappers —
 *
 *     lists:foldl/3   lists:foreach/2   lists:all/2   lists:any/2
 *
 * — whose fun argument is an SSA-constant, environment-free MakeFun of
 * the same module (§10.4's "literal fun"), and replaces the call with a
 * hand-ported expansion of the wrapper + its recursive helper
 * (`sys_core_fold_lists.erl`'s template, built directly as HIR — ported,
 * not generated), with the literal fun's body spliced in by constant
 * propagation. lists:map (and every body-recursive shape, foldr
 * included) is NOT expressible under rung-1 re-call deopt and stays a
 * plain call — it defers to P3's framestates.
 *
 * The loop's deopt/yield state is always the CALLEE's fresh-call
 * vector — foldl at element k IS foldl(F, Acc_k, Rest_k) — and every
 * exit to T1 enters real lists code:
 *
 *   - the back edge is a ReductionCheck of the callee demote class
 *     (T2_OP_RC_CALLEE): its yield saves the vector under the helper's
 *     own MFA and resumes into the loop; its tombstone/jettison path
 *     re-enters T1 lists code / the call site;
 *   - the improper-list and bad-fun-result edges are DemoteCallee
 *     transfers into the wrapper/helper body, which re-executes the
 *     iteration and raises the byte-identical error
 *     (case_clause@wrapper for a bad list head, function_clause@helper
 *     past it — exactly T1's shapes);
 *   - window deopts of the spliced fun body (flag-checked arithmetic)
 *     take the same helper-body route (T2_OP_WINDOW_CALLEE).
 *
 * Reduction identity: the expansion charges exactly what T1's
 * wrapper/helper/fun call chain charges (1 at the wrapper entry, 2 per
 * element, +1 on all/any's early exit), at the same boundaries, so
 * process_info(_, reductions) and the yield schedule stay
 * T1-identical on every non-deopting path.
 *
 * Effect discipline (PLAN/T2/08 §10): only effect-free fun bodies are
 * admitted — one re-execution window per iteration; anything effectful
 * (calls, BIFs) or allocating (GcTest) rejects the site, which then
 * stays a plain call_ext. Conservative rejection, never a wrong result.
 */

#ifndef _JIT_T2_INTRINSICS_HPP
#define _JIT_T2_INTRINSICS_HPP

#include <string>

#include "t2_hir.hpp"
#include "t2_loop.hpp"

struct ErtsT2RetainedCode;

namespace erts_t2 {

    /* Expand every recognized lists-intrinsic call site in `fn`.
     * `code_hdr` is the enclosing instance's BeamCodeHeader (recorded
     * as a dependency when a fun body is inlined). Sets *changed when
     * any site was expanded; the caller then re-validates + re-runs
     * loop analysis and the window validator. A site that fails
     * admission is left exactly as it was. Returns false with *err
     * only on an internal inconsistency (caller degrades the function
     * to T1). Install-mode only (the demote contracts bake T1
     * addresses). T2_NO_INTRIN=1 disables the pass. */
    bool t2_intrinsics(T2Function &fn,
                       const ErtsT2RetainedCode *ret,
                       const void *code_hdr,
                       bool *changed,
                       std::string *err);

    /* LICM-lite (PLAN/T2FULL/09 §8, PLAN/T2/04 §10.6 note): hoist
     * loop-invariant, pure, never-faulting ops and window-shaped
     * SpeculateType guards whose operands are all defined outside the
     * loop from the loop HEADER block to the preheader. For a
     * recovered self-recursion loop the preheader is the entry block,
     * so a hoisted guard falls under the entry-window rule the window
     * validator enforces (params-and-guards-only prefix). The
     * intrinsic expansions place their invariant guards (the
     * accumulator's entry guard) in the preheader by construction;
     * this pass covers what recovery/speculation leave in headers.
     * Sets *changed when anything moved. */
    bool t2_licm_lite(T2Function &fn,
                      const T2LoopInfo &li,
                      bool *changed,
                      std::string *err);

    /* P-C increments A1 + A2 (PLAN/T2FULL/14 §4): verbatim xN unroll
     * of the two simplest cursor-IV loops — a skip-count byte scanner
     * whose latch is exactly {cursor advance, bs_sync, acc += const}
     * (`cnt/2` shape; no reads), and (A2) a read-and-sum byte scanner
     * whose latch prepends one byte read and accumulates it instead:
     * {bs_base, bs_read, cursor advance, bs_sync, acc += byte}
     * (`sumt/2` shape). Inserts a fast path: a new bounds check block
     * (one bs_ensure of N*stride bits) branching to a new N-wide latch
     * holding N byte-for-byte copies of the per-byte body — same
     * homes, flags and sync-map shape with the threaded SSA values
     * substituted (for a reading loop: ONE hoisted base projection,
     * then per copy the read, advance, sync and add, the k-th add's
     * map naming byte_k in the read home) — sharing ONE reduction
     * check and ONE back edge; the original 1-wide body is kept
     * untouched as the remainder. Because every copy is verbatim, a
     * deopt at any byte re-enters T1 at the same beam op PC with the
     * same register contract as the 1-wide loop — deopt-correct by
     * construction, no new deopt shapes.
     *
     * P-C increment B1 (the ROLL-BACK deopt): for the skip-count (A1)
     * shape the default is a FUSED fast-path latch instead of the
     * verbatim copies — the N accumulator adds collapse to ONE checked
     * `acc + N*C` (T2_OP_ROLLBACK) placed BEFORE one N*stride advance
     * and ONE bs_sync, so at its overflow deopt the cursor is still
     * un-advanced and the accumulator un-committed. The op carries the
     * loop header's start_match beam_idx and a clone of its sync map
     * (the pre-iteration clause-entry state), so the deopt redispatches
     * T1 at the header (ERTS_T2_PC_EFFECT — `ret` is consulted up
     * front; no entry, no fusion) and T1 re-executes all N iterations
     * from the pre-add accumulator, producing the small->bignum result
     * byte-identically. The fused reduction check charges N (one per
     * fused iteration, task #46), keeping reduction counts T1-exact.
     * T2_NO_FUSE=1 (or T2_FUSE=0) falls back to the A1 verbatim FL;
     * read-and-sum (A2) shapes always stay verbatim (B2 fuses those).
     *
     * N = 64/stride (T2_UNROLL_N overrides, clamped to 1..16; N <= 1
     * is a no-op). The recognizer bails — the pass makes no change —
     * on ANY shape mismatch: nested loops, multiple latches, more
     * than one read, a read the accumulator does not consume, a
     * non-byte read, home aliasing, extra ops. Sets *changed when a
     * loop was unrolled; the caller re-validates and re-runs loop
     * analysis (the CFG changed). T2_NO_UNROLL=1 disables the pass;
     * T2_UNROLL_TRACE=1 logs accepts and bail reasons. */
    bool t2_unroll(T2Function &fn,
                   const T2LoopInfo &li,
                   const ErtsT2RetainedCode *ret,
                   bool *changed,
                   std::string *err);

} /* namespace erts_t2 */

#endif /* _JIT_T2_INTRINSICS_HPP */
