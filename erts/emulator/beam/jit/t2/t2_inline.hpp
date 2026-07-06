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
 * T2-Full tier-2 JIT: local leaf inlining + loop shape-up (P2 commit 6;
 * PLAN/T2FULL/09 §6, PLAN/T2/03 §2, PLAN/T2/08 §4.2).
 *
 * The MVP class (`diff/2` fused into `total/2`): a small, call-free,
 * frame-free, single-block local callee is spliced into its call site
 * inside a recovered loop. Rung-1 deopt discipline — no framestates, no
 * CPs: the inlined ops carry no T1 PC of their own, so every fallible
 * inlined op is flagged T2_OP_INLINED and MUST become a window-shaped
 * speculative op (the enclosing iteration's re-execution covers the
 * inlined body); if the speculation pass cannot convert one, the whole
 * function fails validation and stays on T1 — never a wrong result.
 *
 * Inlining removes the call — the loop's only effect boundary — so the
 * whole iteration becomes one speculation window. The shape-up half
 * then restores the MVP's loop shape:
 *
 *   - copy propagation through the frame Copies the call forced
 *     (values parked in Y slots across the call read their roots
 *     directly once the call is gone);
 *   - frame elision (the Allocate/Deallocate pair and the dead fill
 *     Copies vanish; nothing in the call-free body needs a frame);
 *   - re-call-vector preservation: iteration-local values decoded into
 *     X0..arity-1 (argument setup for the erased call) are retargeted
 *     to fresh X slots so the loop-header phi homes stay intact from
 *     the header to every window guard — the clean-prefix rule the
 *     window validator enforces. The latch materializations (the ops
 *     whose results are the back edge's phi inputs) keep their homes.
 *
 * Admission is deliberately strict (one call in the loop, one balanced
 * frame pair around it, whitelisted callee ops, no GcTest in the
 * caller loop) so that a spliced function always converts; anything
 * else is left exactly as it was. The rewritten function is re-proven
 * in full by the HIR validator's register-state walk, the speculation
 * fact checker and the window validator before lowering.
 */

#ifndef _JIT_T2_INLINE_HPP
#define _JIT_T2_INLINE_HPP

#include <string>

#include "t2_hir.hpp"
#include "t2_loop.hpp"

struct ErtsT2RetainedCode;

namespace erts_t2 {

    /* Inline eligible leaf calls inside `fn`'s recovered loops and
     * shape the loops up (see above). `ret` provides the retained
     * module for building callee SSA. Sets *changed when anything was
     * rewritten (caller then re-validates after the speculation pass).
     * Returns false with *err only on an internal inconsistency; a
     * site that fails admission is simply left alone. */
    bool t2_inline_leaf(T2Function &fn,
                        const T2LoopInfo &li,
                        const ErtsT2RetainedCode *ret,
                        bool *changed,
                        std::string *err);

} /* namespace erts_t2 */

#endif /* _JIT_T2_INLINE_HPP */
