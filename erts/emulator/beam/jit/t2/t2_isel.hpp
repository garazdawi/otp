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
 * T2-Full tier-2 JIT: instruction selection + LIR verification
 * (PLAN/T2FULL/08 §1–§2).
 *
 * P1 identity isel walks the HIR 1:1 to LIR ops with *concrete*
 * canonical slots taken from the decoded homes the builder recorded
 * (dst_reg/operand_regs; see t2_hir.hpp). The sync-everything policy is
 * therefore satisfied by construction: every value sits in the register
 * T1 would keep it in at every boundary, and the HIR validator has
 * already proven each sync map against the walked register state.
 *
 * Cross-tier addresses are resolved here, against the loaded module:
 *
 *   - non-tail calls get their CP (the T1 post-call continuation) from
 *     the pctab CONT entry of the call's decode ordinal — never a T2
 *     address (PLAN/T2/08 §4.3);
 *   - gc_bif arithmetic with a {f,0} fail gets its side-exit PC from
 *     the pctab EFFECT entry of its own ordinal (T1 re-executes the op
 *     and raises; T2 never raises);
 *   - decoded error exits (badmatch/case_end/if_end) get theirs from
 *     the pctab ERROR entry; the shared function_clause exit branches
 *     to the function's func_info (its ErtsCodeInfo, whose first word
 *     is a valid `bl <raise function_clause>` on aarch64);
 *   - local call/tail-call targets resolve by MFA against the code
 *     header's function table; external ones through the export entry
 *     (dispatched via the active code index at run time);
 *   - call_ext to a *light* BIF lowers to CallBif (T1's
 *     call_light_bif through the T2 fragment): the site's own T1 PC
 *     (pctab BIF entry) is the yield-resume/raise address and the
 *     T1 continuation (pctab CONT entry) the trap/trace CP, so no
 *     blob address ever reaches c_p->i or the stack; tail shapes
 *     mirror T1's transform (BIF with frame intact, then
 *     deallocate + return).
 *
 * A missing pctab entry, a heavy-BIF or loader-transformed call
 * target, or any shape outside the identity table is a clean
 * "unsupported" failure — the function simply stays T1.
 */

#ifndef _JIT_T2_ISEL_HPP
#define _JIT_T2_ISEL_HPP

#include <string>

#include "t2_hir.hpp"
#include "t2_lir.hpp"

struct ErtsT2RetainedCode;

namespace erts_t2 {

    /* Cross-tier resolution context. `code_hdr` is the loaded module's
     * BeamCodeHeader (its functions[] array of ErtsCodeInfo*, in the
     * same chunk order as the retained function index). Passed as
     * void* to keep this header free of beam_code.h. */
    struct T2IselContext {
        const ErtsT2RetainedCode *ret = nullptr;
        const void *code_hdr = nullptr; /* const BeamCodeHeader * */

        /* True when isel runs inside the loader (beam_load_finalize_code,
         * holding code-load permission), so it may mirror T1's load-time
         * erts_export_get_or_make_stub for a not-yet-loaded remote callee.
         * Defaults on for the corpus sweep / compile-at-load driver, which
         * hold that permission; the standalone debug-exec BIF turns it off
         * (it must never mutate the staging export table). */
        bool allow_stub = true;
    };

    /* Lower HIR to LIR with concrete canonical slots and resolved
     * cross-tier addresses. Returns false with *err set on any op or
     * shape outside the P1 identity table. Requires hir.sync_complete. */
    bool t2_isel(const T2Function &hir,
                 const T2IselContext &ctx,
                 T2LirFunction &lir,
                 std::string *err);

    /* LIR verifier (the P1 stand-in for register allocation; the pin
     * API becomes a real allocator in P2): checks that every operand is
     * a concrete slot or immediate, terminators are last, CFG edges are
     * in range, and cross-tier addresses are present where required. */
    bool t2_regalloc(T2LirFunction &lir, std::string *err);

} /* namespace erts_t2 */

#endif /* _JIT_T2_ISEL_HPP */
