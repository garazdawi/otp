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
 * T2-Full tier-2 JIT: instruction selection + register allocation
 * (PLAN/T2FULL/08 §1).
 *
 * Isel walks the HIR ops 1:1 to LIR ops, emitting operands as
 * PhysLoc::phys(value_id) placeholders. Regalloc runs the P1
 * *sync-everything* policy: every op boundary is a sync point, so each
 * SSA value is pinned to a canonical BEAM slot for its whole (one-op)
 * live range and the placeholders are rewritten to Xn/Yn.
 *
 * P1 scope note (a real, source-grounded constraint): the P0 HIR builder
 * reconstructs SSA and does *not* record which BEAM register/slot each
 * value originally occupied. For a leaf function whose only slots are the
 * parameter X registers and the return value (X0), the canonical slots
 * are fully recoverable from the calling convention (param i -> X_i,
 * return -> X0), which is what these passes implement. A function with
 * values live across a call (needing specific Y slots + T1's exact
 * allocate/trim/deallocate frame) needs register-origin metadata the HIR
 * abstracts away; t2_regalloc reports that case as unsupported rather
 * than emit a mis-framed body.
 */

#ifndef _JIT_T2_ISEL_HPP
#define _JIT_T2_ISEL_HPP

#include <string>

#include "t2_hir.hpp"
#include "t2_lir.hpp"

namespace erts_t2 {

    /* Lower HIR to LIR (operands as value-id placeholders). Returns false
     * with *err set on an unsupported HIR op. */
    bool t2_isel(const T2Function &hir, T2LirFunction &lir, std::string *err);

    /* Assign canonical slots (sync-everything) and compute per-op GC live
     * counts, rewriting placeholders in place. Returns false with *err set
     * when the function needs frame/Y-slot placement P1's leaf allocator
     * does not reconstruct. */
    bool t2_regalloc(T2LirFunction &lir, std::string *err);

} /* namespace erts_t2 */

#endif /* _JIT_T2_ISEL_HPP */
