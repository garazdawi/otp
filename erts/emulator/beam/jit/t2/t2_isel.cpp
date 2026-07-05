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
 * T2-Full tier-2 JIT: instruction selection (HIR -> LIR). See t2_isel.hpp.
 */

#include "t2_isel.hpp"

namespace erts_t2 {

    namespace {

        /* HIR arithmetic/bitwise op -> LIR kind. Returns Invalid if the op
         * is not a generic arithmetic op. */
        T2LirKind arith_kind(T2OpKind k) {
            switch (k) {
            case T2OpKind::Add: return T2LirKind::Add;
            case T2OpKind::Sub: return T2LirKind::Sub;
            case T2OpKind::Mul: return T2LirKind::Mul;
            case T2OpKind::IDiv: return T2LirKind::IDiv;
            case T2OpKind::Rem: return T2LirKind::Rem;
            case T2OpKind::Band: return T2LirKind::Band;
            case T2OpKind::Bor: return T2LirKind::Bor;
            case T2OpKind::Bxor: return T2LirKind::Bxor;
            case T2OpKind::Bsl: return T2LirKind::Bsl;
            case T2OpKind::Bsr: return T2LirKind::Bsr;
            case T2OpKind::Bnot: return T2LirKind::Bnot;
            case T2OpKind::Neg: return T2LirKind::Neg;
            default: return T2LirKind::Invalid;
            }
        }

        struct Isel {
            const T2Function &hir;
            T2LirFunction &lir;
            std::string *err;

            bool fail(const char *msg) {
                if (err) {
                    *err = msg;
                }
                return false;
            }

            /* A value operand becomes a phys(value_id) placeholder. */
            T2LirSrc value_src(const T2Value *v) {
                return T2LirSrc::slot(PhysLoc::phys((uint16_t)v->id));
            }

            PhysLoc value_dst(const T2Value *v) {
                return PhysLoc::phys((uint16_t)v->id);
            }

            bool emit_op(T2LirBlock &b, const T2Op *op) {
                T2LirOp lop;
                lop.beam_idx = op->beam_idx;

                /* Generic arithmetic (Fail -> op's T1 PC; side-exit). */
                T2LirKind ak = arith_kind(op->kind);
                if (ak != T2LirKind::Invalid) {
                    lop.kind = ak;
                    lop.dst = value_dst(op->result);
                    lop.mfa_m = op->mfa_m;
                    lop.mfa_f = op->mfa_f;
                    lop.num_srcs = (uint8_t)op->num_operands;
                    if (lop.num_srcs > T2_LIR_MAX_SRCS) {
                        return fail("arith op has too many operands");
                    }
                    for (uint16_t i = 0; i < op->num_operands; i++) {
                        lop.srcs[i] = value_src(op->operands[i]);
                    }
                    b.ops.push_back(lop);
                    return true;
                }

                switch (op->kind) {
                case T2OpKind::Param:
                    /* Establishes value = X(param index); no code. */
                    if (op->result->id >= lir.num_values) {
                        return fail("param value id out of range");
                    }
                    lir.param_x[op->result->id] = (int32_t)op->index;
                    return true;

                default:
                    return fail("unsupported HIR op in P1 isel "
                                "(commit 2 covers leaf arithmetic)");
                }
            }

            bool emit_terminator(T2LirBlock &b, const T2Op *t) {
                if (t == nullptr) {
                    return fail("block without terminator");
                }
                switch (t->kind) {
                case T2OpKind::Return: {
                    T2LirOp lop;
                    lop.kind = T2LirKind::Return;
                    lop.beam_idx = t->beam_idx;
                    if (t->num_operands != 1) {
                        return fail("return without a single value");
                    }
                    lop.num_srcs = 1;
                    lop.srcs[0] = value_src(t->operands[0]);
                    b.ops.push_back(lop);
                    return true;
                }
                default:
                    return fail("unsupported terminator in P1 isel "
                                "(commit 2 covers return)");
                }
            }

            bool run() {
                lir.module = hir.module;
                lir.function = hir.function;
                lir.arity = hir.arity;
                lir.num_values = (uint32_t)hir.values.size();
                lir.param_x.assign(lir.num_values, -1);

                for (const T2BasicBlock *hb : hir.blocks) {
                    if (hb->phis_head != nullptr) {
                        return fail("phi nodes unsupported in P1 isel "
                                    "(commit 3 / needs frame placement)");
                    }

                    T2LirBlock &lb = lir.new_block();

                    for (const T2Op *op = hb->ops_head; op != nullptr;
                         op = op->next) {
                        if (!emit_op(lb, op)) {
                            return false;
                        }
                    }

                    if (!emit_terminator(lb, hb->terminator)) {
                        return false;
                    }
                }

                return true;
            }
        };

    } /* anonymous namespace */

    bool t2_isel(const T2Function &hir, T2LirFunction &lir, std::string *err) {
        Isel isel{hir, lir, err};
        return isel.run();
    }

} /* namespace erts_t2 */
