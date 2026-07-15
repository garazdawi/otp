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
 * T2-Full tier-2 JIT: the speculation-insertion pass (P2 commit 4;
 * PLAN/T2FULL/09 §§2-3, PLAN/T2/03 §9, PLAN/T2/08 §4.4).
 *
 * Replaces generic Add/Sub gc_bif arithmetic inside recovered loops
 * with flag-checked AddSmall/SubSmall (compute with flag-setting
 * instructions, branch on overflow to the side exit, commit after —
 * "deopt before the *commit*", T1's own small-int fast-path pattern
 * with the type checks hoisted out), guarded by SpeculateType tag-bit
 * tests. The one-untag trick (PLAN/T2/03 §9.4: clearing one operand's
 * tag bits makes tagged+cleared preserve the tag through add/sub) is
 * folded into the AddSmall/SubSmall emitters exactly as in T1's
 * arm/instr_arith.cpp, so no untagged machine word ever exists as an
 * SSA value — the untagged-interval corruption class stays empty by
 * construction (the armed allocator/validator rules keep guarding it).
 *
 * Two deopt shapes, chosen per op (see T2_OP_SPEC_BOUNDARY in
 * t2_hir.hpp):
 *
 *   - window: side exit to the function's T1 entry body; T1 re-executes
 *     the whole iteration from the fresh-call vector (the loop-header
 *     phi homes X0..arity-1). Requires a clean prefix — no effect, no
 *     frame op, no X0..arity-1 write before the op — enforced by
 *     t2_validate_windows.
 *   - boundary: side exit to the op's own T1 EFFECT PC; T1 re-executes
 *     just that op from its sync-map state. Legal anywhere the pctab
 *     has the entry; used after effects (e.g. arithmetic on a call
 *     result), where the window shape is illegal.
 *
 * Guard fusion (the MVP's AND rule, PLAN/T2/08 §4.4 closing): guards
 * needed at one deopt anchor are emitted as a single multi-operand
 * SpeculateType whose lowering ANDs the values and requires every
 * small-tag bit — the combined predicate holds only when every input
 * satisfies every bit.
 *
 * Facts come from a T2FactSource so the profile slots (P2 commit 9)
 * can plug in without touching the pass. The profile-less default
 * speculates "observed small" on function-entry arguments that feed
 * loop-carried arithmetic phis, unless the Type chunk excludes small
 * integers. Type-chunk *proofs* (integer-only with a small range)
 * need no guard at all. A function where no facts allow speculation is
 * left untouched — the identity transform stays byte-identical.
 */

#ifndef _JIT_T2_SPEC_HPP
#define _JIT_T2_SPEC_HPP

#include <string>

#include "t2_hir.hpp"
#include "t2_loop.hpp"

struct ErtsT2RetainedCode;

namespace erts_t2 {

    /* The fact source the speculation inserter consults. P2 commit 4
     * ships the profile-less default below; commit 9's entry-type
     * profile slots implement this interface over observed data. */
    class T2FactSource {
    public:
        virtual ~T2FactSource() = default;

        /* May the pass speculate (guard + assume) that function-entry
         * argument `idx` is a small integer? Returning false leaves
         * consumers of that argument on the generic path. */
        virtual bool speculate_param_small(uint32_t idx) const = 0;

        /* The observed entry type-class bitmask (ERTS_T2_TY_*, t2_retain.h)
         * for function-entry argument `idx`, unioned across profile trips,
         * or 0 when there is no profile evidence (forced mode, an unsampled
         * argument, or the profile-less default). Exactly one bit set means
         * the argument was monomorphically that class. Consumers treat 0 as
         * "no evidence" and fall back to the AOT Type chunk — profile
         * narrows, never contradicts (02 §1). The default returns 0. */
        virtual uint16_t param_seen_types(uint32_t idx) const {
            (void)idx;
            return 0;
        }
    };

    /* Profile-less default: speculate on every entry argument whose
     * seeded type does not exclude a small integer ("where the entry
     * types allow"). */
    class T2DefaultFactSource : public T2FactSource {
    public:
        explicit T2DefaultFactSource(const T2Function &fn);
        bool speculate_param_small(uint32_t idx) const override;

    private:
        std::vector<uint8_t> allow;
    };

    /* Run the pass over a recovered function. `ret` supplies the pctab
     * for boundary-deopt availability checks (an op without an EFFECT
     * entry simply stays generic). Sets *changed when any op was
     * converted or any guard inserted; the caller must then re-run
     * t2_validate and t2_validate_windows. Returns false with *err only
     * on an internal inconsistency (caller degrades the function to
     * T1). */
    bool t2_speculate(T2Function &fn,
                      const T2LoopInfo &li,
                      const ErtsT2RetainedCode *ret,
                      const T2FactSource &facts,
                      bool *changed,
                      std::string *err);

} /* namespace erts_t2 */

#endif /* _JIT_T2_SPEC_HPP */
