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
 * T2-Full tier-2 JIT: loop analysis + self-tail-recursion loop recovery
 * (PLAN/T2/04 §10.5, PLAN/T2/08 §3 class 1, PLAN/T2FULL/09 §4).
 *
 * Loop structure is *side data* in the LoopInfo style — never an IR
 * construct. t2_loop_info builds dominators, finds back-edges, and
 * groups blocks into natural loops (merged per header). t2_loop_recover
 * rewrites a self-tail-recursive function's recursive tail calls into
 * back-jumps to a synthesized loop header, which is what creates the
 * CFG loops the analysis (and later LICM / the re-execution-window
 * validator) sees.
 */

#ifndef _JIT_T2_LOOP_HPP
#define _JIT_T2_LOOP_HPP

#include <cstdint>
#include <string>
#include <vector>

#include "t2_hir.hpp"

namespace erts_t2 {

    constexpr uint32_t T2_NO_LOOP_BLOCK = 0xFFFFFFFFu;

    /* One natural loop (PLAN/T2/04 §10.5). Loops sharing a header are
     * merged (one loop, several latches). */
    struct T2Loop {
        uint32_t header = T2_NO_LOOP_BLOCK;
        /* The unique out-of-loop predecessor of the header whose only
         * successor is the header, or T2_NO_LOOP_BLOCK. For a recovered
         * self-recursion loop this is the function entry block (the
         * synthesized preheader). */
        uint32_t preheader = T2_NO_LOOP_BLOCK;
        std::vector<uint32_t> latches; /* in-loop preds of the header  */
        std::vector<uint32_t> exits;   /* loop blocks with an edge out */
        std::vector<uint32_t> body;    /* all loop blocks, incl header */
        /* Nesting parent (index into T2LoopInfo::loops), or -1. */
        int32_t parent = -1;
    };

    struct T2LoopInfo {
        std::vector<T2Loop> loops;
    };

    /* Dominators -> back-edges -> block groups over the finalized CFG.
     * Unreachable blocks are ignored. */
    void t2_loop_info(const T2Function &fn, T2LoopInfo *out);

    /* Self-tail-recursion loop recovery (the back-edge-as-internal-
     * branch transform). For every *local* recursive tail call to the
     * function itself: synthesize a loop header holding one phi per
     * parameter (entry values from the function entry block — the
     * preheader — updated values from each latch), move the entry
     * block's non-Param body there, and replace each recursive
     * TailCall with a ReductionCheck (carrying the tail call's sync
     * map: the fresh-call argument vector, the back-edge reduction
     * charge point) followed by a back-jump to the header.
     *
     * Module-qualified self-calls (TailCallExt) are deliberately NOT
     * recovered: T1 dispatches them through the export table every
     * iteration, so they pick up new code immediately after a module
     * upgrade — an internal back-jump would delay that to the next
     * yield, an observable hot-code-loading difference. Local tail
     * calls never re-dispatch in T1 either, so the back-jump is
     * observably identical for them.
     *
     * Returns false with *err on an internal inconsistency (the caller
     * must leave the function on T1). *recovered reports whether the
     * transform applied; when it did, the caller re-validates the
     * function (t2_validate) before lowering. */
    bool t2_loop_recover(T2Function &fn, bool *recovered, std::string *err);

} /* namespace erts_t2 */

#endif /* _JIT_T2_LOOP_HPP */
