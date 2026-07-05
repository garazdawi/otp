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
 * T2-Full tier-2 JIT: register allocation (PLAN/T2FULL/08 §1).
 *
 * The P1 policy is *sync-everything*: every op boundary is a sync point,
 * so no value's live range spans a register across ops -- allocation is a
 * trivial per-value canonical-slot pin. The intervals/pin API is real (a
 * per-value slot map + a per-op live-set giving the GC live count); P2
 * relaxes it by demoting non-sync-point boundaries so ranges start living
 * in registers across ops.
 *
 * Commit-2 scope: leaf, single-block straight-line bodies (parameters in
 * X0..X(arity-1), the return value in X0). Functions with control flow,
 * calls, or values live across a call need Y-slot + T1-frame placement
 * that the reconstructed HIR does not carry; those are reported
 * unsupported here rather than mis-allocated.
 */

#include "t2_isel.hpp"

#include <vector>

namespace erts_t2 {

    namespace {

        constexpr int T2_REGALLOC_MAX_X = 64;

        bool is_arith(T2LirKind k) {
            switch (k) {
            case T2LirKind::Add:
            case T2LirKind::Sub:
            case T2LirKind::Mul:
            case T2LirKind::IDiv:
            case T2LirKind::Rem:
            case T2LirKind::Band:
            case T2LirKind::Bor:
            case T2LirKind::Bxor:
            case T2LirKind::Bsl:
            case T2LirKind::Bsr:
            case T2LirKind::Bnot:
            case T2LirKind::Neg:
            case T2LirKind::GuardBif:
                return true;
            default:
                return false;
            }
        }

        struct RegAlloc {
            T2LirFunction &lir;
            std::string *err;
            std::vector<int32_t> slot;     /* value id -> X slot, or -1 */
            std::vector<int32_t> last_use; /* value id -> flat op idx, or -1 */

            bool fail(const char *msg) {
                if (err) {
                    *err = msg;
                }
                return false;
            }

            bool supported() {
                if (lir.blocks.size() != 1) {
                    return fail("multi-block function unsupported in P1 leaf "
                                "regalloc (commit 3)");
                }
                for (const T2LirBlock &b : lir.blocks) {
                    for (const T2LirOp &op : b.ops) {
                        if (t2_lir_kind_is_terminator(op.kind) &&
                            op.kind != T2LirKind::Return) {
                            return fail("non-return terminator unsupported "
                                        "in P1 leaf regalloc");
                        }
                        if (!op.dst.is_none() && op.dst.is_yreg()) {
                            return fail("Y-slot result unsupported in P1 leaf "
                                        "regalloc");
                        }
                    }
                }
                return true;
            }

            void note_use(const T2LirSrc &s, int idx) {
                if (!s.is_const && s.loc.is_phys()) {
                    last_use[s.loc.num] = idx;
                }
            }

            /* Rewrite a placeholder into its assigned canonical slot. */
            bool lower_loc(PhysLoc &l) {
                if (l.is_phys()) {
                    int32_t s = slot[l.num];
                    if (s < 0) {
                        return fail("value used before slot assignment");
                    }
                    l = PhysLoc::xreg((uint16_t)s);
                }
                return true;
            }

            bool run() {
                if (!supported()) {
                    return false;
                }

                uint32_t nv = lir.num_values;
                slot.assign(nv, -1);
                last_use.assign(nv, -1);

                T2LirBlock &b = lir.blocks[0];

                /* Pass 1: last use of every value. */
                int idx = 0;
                for (const T2LirOp &op : b.ops) {
                    for (uint8_t i = 0; i < op.num_srcs; i++) {
                        note_use(op.srcs[i], idx);
                    }
                    idx++;
                }

                /* Parameters occupy their X slot from function entry. */
                std::vector<bool> x_used(T2_REGALLOC_MAX_X, false);
                for (uint32_t v = 0; v < nv; v++) {
                    if (lir.param_x[v] >= 0) {
                        if (lir.param_x[v] >= T2_REGALLOC_MAX_X) {
                            return fail("parameter X slot out of range");
                        }
                        slot[v] = lir.param_x[v];
                        x_used[slot[v]] = true;
                    }
                }

                /* Pass 2: assign result slots + GC live counts. */
                idx = 0;
                for (T2LirOp &op : b.ops) {
                    if (is_arith(op.kind)) {
                        int live = 0;
                        for (int s = 0; s < T2_REGALLOC_MAX_X; s++) {
                            if (x_used[s]) {
                                live = s + 1;
                            }
                        }
                        op.arity = (uint32_t)live;
                    }

                    /* Reclaim X slots of src values whose last use is here
                     * (their slot may back this op's own result). */
                    for (uint8_t i = 0; i < op.num_srcs; i++) {
                        const T2LirSrc &s = op.srcs[i];
                        if (!s.is_const && s.loc.is_phys()) {
                            uint32_t vid = s.loc.num;
                            if (last_use[vid] == idx && slot[vid] >= 0) {
                                x_used[slot[vid]] = false;
                            }
                        }
                    }

                    if (!op.dst.is_none() && op.dst.is_phys()) {
                        uint32_t vid = op.dst.num;
                        int s = 0;
                        while (s < T2_REGALLOC_MAX_X && x_used[s]) {
                            s++;
                        }
                        if (s >= T2_REGALLOC_MAX_X) {
                            return fail("out of X slots in P1 leaf regalloc");
                        }
                        slot[vid] = s;
                        x_used[s] = true;
                    }

                    idx++;
                }

                /* Pass 3: rewrite placeholders to canonical slots. */
                for (T2LirOp &op : b.ops) {
                    if (!lower_loc(op.dst)) {
                        return false;
                    }
                    for (uint8_t i = 0; i < op.num_srcs; i++) {
                        if (!op.srcs[i].is_const &&
                            !lower_loc(op.srcs[i].loc)) {
                            return false;
                        }
                    }
                }

                /* The return value must be in X0 (return convention). If it
                 * landed elsewhere, insert an explicit move ahead of the
                 * return terminator. */
                if (!b.ops.empty()) {
                    T2LirOp &ret = b.ops.back();
                    if (ret.kind == T2LirKind::Return && ret.num_srcs == 1 &&
                        !ret.srcs[0].is_const && ret.srcs[0].loc.is_xreg() &&
                        ret.srcs[0].loc.num != 0) {
                        T2LirOp mv;
                        mv.kind = T2LirKind::Move;
                        mv.dst = PhysLoc::xreg(0);
                        mv.num_srcs = 1;
                        mv.srcs[0] = ret.srcs[0];
                        ret.srcs[0] = T2LirSrc::slot(PhysLoc::xreg(0));
                        b.ops.insert(b.ops.end() - 1, mv);
                    }
                }

                return true;
            }
        };

    } /* anonymous namespace */

    bool t2_regalloc(T2LirFunction &lir, std::string *err) {
        RegAlloc ra{lir, err};
        return ra.run();
    }

} /* namespace erts_t2 */
