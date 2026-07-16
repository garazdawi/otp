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
 * T2-Full tier-2 JIT: the standard optimization suite over inlined HIR
 * (Stage 3; PLAN/T2FULL/census/stage3_opts_design.md).
 *
 * Once a hot caller has an inlined body in its blob (the maps:fold
 * fast loop, the lists intrinsics, the leaf inliner), this module runs
 * the classic cleanup passes as a fixpoint so the inlined body is as
 * tight as a hand-written loop:
 *
 *   3a  DCE                 — dead pure-value elimination (kills the
 *                             dead flatmap_key_at when the fun ignores
 *                             its key argument) + dead-phi elimination.
 *   3b  constant folding    — copy-of-constant rematerialization and
 *       + copy propagation    all-constant add_small/sub_small folds;
 *                             home-aware operand forwarding through
 *                             register copies, and single-input-phi
 *                             collapse (the count_pos copy chains).
 *   3c  CSE / GVN-lite      — value-number pure ops; merge only when
 *                             the survivor is provably available in
 *                             its canonical home at every rewritten
 *                             use and the victim is invisible to every
 *                             sync map (sync maps are uses).
 *   3d  make_fun sinking    — partial DCE: sink the slow-path-only
 *                             MakeFun (+ its argument Copy and its
 *                             share of the entry GcTest) into the slow
 *                             block, so the fast path neither
 *                             allocates the fun nor GC-tests for it.
 *                             The fast path's callsite-class deopts
 *                             lose their physical call boundary (the
 *                             fun in X0) and are re-classed to the
 *                             entry-window class (T2_OP_SPEC_ENTRY):
 *                             re-execute the whole invocation from the
 *                             function's own T1 entry body — legal
 *                             only when every path from entry to the
 *                             deopt is effect-/frame-/charge-free and
 *                             X0..arity-1 are never written (the
 *                             entry-recall rule in t2_validate_windows
 *                             re-proves it).
 *
 * Every rewrite respects the deopt/home model (design doc §"Hard
 * constraints"): homes are real register moves, sync maps are uses,
 * and the output must re-validate under both t2_validate and
 * t2_validate_windows or the caller degrades the function to T1.
 *
 * Levers: T2_NO_OPT (master, checked by the caller), T2_NO_DCE,
 * T2_NO_CONSTFOLD, T2_NO_COPYPROP, T2_NO_CSE, T2_NO_SINK per sub-pass.
 * The post-pass HIR is dumped by the caller under the +JT2dump STAGES
 * facet ("hir after opt").
 */

#ifndef _JIT_T2_OPT_HPP
#define _JIT_T2_OPT_HPP

#include <string>

#include "t2_hir.hpp"
#include "t2_loop.hpp"

namespace erts_t2 {

    /* Side-effect-free, never-faulting op kinds whose unused results
     * may be eliminated and whose identical instances may be merged
     * (design doc §"Hard constraints" pt 4). AddSmall/SubSmall are
     * included deliberately: their overflow deopt guard is dead
     * exactly when the result is (removing a dead guard changes no
     * data flow and re-executing the erased call would produce the
     * same downstream state). MakeFun is NOT pure — it allocates and
     * is only removable via the sinking transform. */
    bool t2_op_is_pure(T2OpKind kind);

    /* Run the fixpoint of the sub-passes above on `fn`. `li` is the
     * loop side data computed on the current (unchanged) CFG; the pass
     * never adds or removes blocks or edges, so `li` stays valid for
     * the window validator afterwards. Sets *changed when anything
     * was rewritten; the caller then re-validates in full. Returns
     * false with *err only on an internal inconsistency (the caller
     * degrades the function to T1). */
    bool t2_opt(T2Function &fn,
                const T2LoopInfo &li,
                bool *changed,
                std::string *err);

} /* namespace erts_t2 */

#endif /* _JIT_T2_OPT_HPP */
