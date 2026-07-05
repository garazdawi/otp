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
 * T2-Full tier-2 JIT: LIR verification (PLAN/T2FULL/08 §1).
 *
 * The P1 policy is *sync-everything*, and under the identity transform
 * it is satisfied by construction: isel places every value in its
 * decoded canonical BEAM slot (the homes the builder recorded and the
 * HIR validator proved against the per-sync-point register maps), so
 * there is nothing left to allocate. What remains — and what this pass
 * provides — is the backend-side verification that the LIR the emitter
 * is about to trust is well-formed: every operand is a concrete
 * canonical slot or an immediate (no placeholder ever reaches
 * emission), terminators sit last and only last, CFG edges are in
 * range, and every op that transfers to T1 carries its resolved T1
 * address. P2 replaces this with a real Wimmer-style allocator that
 * relaxes non-sync-point boundaries; the entry point is kept stable
 * for that reason.
 */

#include "t2_isel.hpp"

namespace erts_t2 {

    namespace {

        struct Verify {
            T2LirFunction &lir;
            std::string *err;

            bool fail(const std::string &msg) {
                if (err) {
                    *err = msg;
                }
                return false;
            }

            bool check_src(const T2LirSrc &s, const char *what) {
                if (s.is_const) {
                    return true;
                }
                if (!s.loc.is_slot()) {
                    return fail(std::string(what) +
                                ": operand is not a canonical slot or "
                                "immediate");
                }
                return true;
            }

            bool check_block_ref(uint32_t id, const char *what) {
                if (id == T2_LIR_NO_BLOCK || id >= lir.blocks.size()) {
                    return fail(std::string(what) +
                                ": successor block out of range");
                }
                return true;
            }

            bool run() {
                for (const T2LirBlock &b : lir.blocks) {
                    for (size_t i = 0; i < b.ops.size(); i++) {
                        const T2LirOp &op = b.ops[i];
                        const char *name = t2_lir_kind_name(op.kind);
                        bool is_term = t2_lir_kind_is_terminator(op.kind) ||
                                       op.kind == T2LirKind::Call ||
                                       op.kind == T2LirKind::CallExt ||
                                       /* folded guards absorb the branch */
                                       op.succ_else != T2_LIR_NO_BLOCK;

                        if (op.kind == T2LirKind::Invalid) {
                            return fail("invalid LIR op");
                        }

                        for (uint8_t s = 0; s < op.num_srcs; s++) {
                            if (!check_src(op.srcs[s], name)) {
                                return false;
                            }
                        }
                        for (uint32_t s = 0; s < op.num_srcs_ext; s++) {
                            if (op.pool_first + s >= lir.src_pool.size()) {
                                return fail("operand pool out of range");
                            }
                            if (!check_src(lir.src_pool[op.pool_first + s],
                                           name)) {
                                return false;
                            }
                        }
                        if (!op.dst.is_none() && !op.dst.is_slot()) {
                            return fail(std::string(name) +
                                        ": destination is not a canonical "
                                        "slot");
                        }
                        if (!op.dst2.is_none() && !op.dst2.is_slot()) {
                            return fail(std::string(name) +
                                        ": second destination is not a "
                                        "canonical slot");
                        }

                        /* Edges. */
                        if (op.succ_then != T2_LIR_NO_BLOCK &&
                            !check_block_ref(op.succ_then, name)) {
                            return false;
                        }
                        if (op.succ_else != T2_LIR_NO_BLOCK &&
                            !check_block_ref(op.succ_else, name)) {
                            return false;
                        }
                        if (op.kind == T2LirKind::Switch) {
                            if (op.first_case + op.num_cases >
                                lir.switch_cases.size()) {
                                return fail("switch case pool out of range");
                            }
                            for (uint32_t c = 0; c < op.num_cases; c++) {
                                if (!check_block_ref(
                                            lir.switch_cases[op.first_case + c]
                                                    .target,
                                            name)) {
                                    return false;
                                }
                            }
                            if (!check_block_ref(op.default_target, name)) {
                                return false;
                            }
                        }

                        /* Cross-tier addresses. */
                        switch (op.kind) {
                        case T2LirKind::Call:
                        case T2LirKind::CallExt:
                            if (op.t1_pc_cont == nullptr) {
                                return fail("call without a T1 continuation");
                            }
                            /* fall through */
                        case T2LirKind::TailCall:
                        case T2LirKind::TailCallExt:
                            if (op.kind == T2LirKind::Call ||
                                op.kind == T2LirKind::TailCall) {
                                if (op.target == nullptr) {
                                    return fail("local call without a "
                                                "resolved target");
                                }
                            } else if (op.exp == nullptr) {
                                return fail("remote call without an export");
                            }
                            break;
                        case T2LirKind::SideExit:
                            if (op.t1_pc_fail == nullptr) {
                                return fail("side exit without a T1 PC");
                            }
                            break;
                        case T2LirKind::CallBif:
                            /* Both T1 addresses and both callee handles
                             * must be resolved: the site (yield/error),
                             * the continuation (trap/trace CP), the
                             * export and the BIF C function. */
                            if (op.t1_pc_fail == nullptr) {
                                return fail("bif call without a T1 site");
                            }
                            if (op.t1_pc_cont == nullptr) {
                                return fail("bif call without a T1 "
                                            "continuation");
                            }
                            if (op.exp == nullptr || op.target == nullptr) {
                                return fail("bif call without a resolved "
                                            "export/function");
                            }
                            break;
                        default:
                            break;
                        }

                        /* Terminators (and the ops that absorb them, or end
                         * the T2 region: non-tail calls demote-on-return)
                         * must be last in the block. */
                        if (is_term && i + 1 != b.ops.size() &&
                            op.kind != T2LirKind::Call &&
                            op.kind != T2LirKind::CallExt) {
                            return fail(std::string(name) +
                                        ": terminator not last in block");
                        }
                    }
                }

                return true;
            }
        };

    } /* anonymous namespace */

    bool t2_regalloc(T2LirFunction &lir, std::string *err) {
        Verify v{lir, err};
        return v.run();
    }

} /* namespace erts_t2 */
