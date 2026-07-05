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

extern "C"
{
#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "code_ix.h"
#include "export.h"
#include "beam_code.h"

#include "t2_retain.h"
#include "t2_pctab.h"
}

#include <vector>

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

        /* HIR type test / comparison -> LIR kind (all lower to a reused
         * T1 emitter with Fail redirected to the in-blob else-block). */
        T2LirKind guard_kind(T2OpKind k) {
            switch (k) {
            case T2OpKind::IsInteger: return T2LirKind::IsInteger;
            case T2OpKind::IsFloat: return T2LirKind::IsFloat;
            case T2OpKind::IsNumber: return T2LirKind::IsNumber;
            case T2OpKind::IsAtom: return T2LirKind::IsAtom;
            case T2OpKind::IsBoolean: return T2LirKind::IsBoolean;
            case T2OpKind::IsTuple: return T2LirKind::IsTuple;
            case T2OpKind::IsList: return T2LirKind::IsList;
            case T2OpKind::IsNonemptyList: return T2LirKind::IsNonemptyList;
            case T2OpKind::IsNil: return T2LirKind::IsNil;
            case T2OpKind::IsBinary: return T2LirKind::IsBinary;
            case T2OpKind::IsBitstring: return T2LirKind::IsBitstring;
            case T2OpKind::IsMap: return T2LirKind::IsMap;
            case T2OpKind::IsPid: return T2LirKind::IsPid;
            case T2OpKind::IsPort: return T2LirKind::IsPort;
            case T2OpKind::IsReference: return T2LirKind::IsReference;
            case T2OpKind::IsTaggedTuple: return T2LirKind::IsTaggedTuple;
            case T2OpKind::TestArity: return T2LirKind::TestArity;
            case T2OpKind::CmpEqExact: return T2LirKind::CmpEqExact;
            case T2OpKind::CmpNeExact: return T2LirKind::CmpNeExact;
            case T2OpKind::CmpEq: return T2LirKind::CmpEq;
            case T2OpKind::CmpNe: return T2LirKind::CmpNe;
            case T2OpKind::CmpLt: return T2LirKind::CmpLt;
            case T2OpKind::CmpLe: return T2LirKind::CmpLe;
            case T2OpKind::CmpGt: return T2LirKind::CmpGt;
            case T2OpKind::CmpGe: return T2LirKind::CmpGe;
            default: return T2LirKind::Invalid;
            }
        }

        bool is_const_kind(T2OpKind k) {
            switch (k) {
            case T2OpKind::ConstInt:
            case T2OpKind::ConstAtom:
            case T2OpKind::ConstNil:
            case T2OpKind::ConstLiteral:
                return true;
            default:
                return false;
            }
        }

        struct Isel {
            const T2Function &hir;
            const T2IselContext &ctx;
            T2LirFunction &lir;
            std::string *err;

            /* Per-value use counts, for the guard-fold pattern checks. */
            std::vector<uint32_t> uses;

            bool fail(const std::string &msg) {
                if (err) {
                    *err = msg;
                }
                return false;
            }

            bool fail_op(const T2Op *op, const char *msg) {
                return fail(std::string(msg) + " (" +
                            t2_op_kind_name(op->kind) + ")");
            }

            /* ---- slots and operands ----------------------------------- */

            static PhysLoc reg_loc(int32_t reg) {
                if (t2_reg_is_y(reg)) {
                    return PhysLoc::yreg((uint16_t)t2_reg_index(reg));
                }
                return PhysLoc::xreg((uint16_t)t2_reg_index(reg));
            }

            /* A boxed ConstLiteral term is safe to embed in a standalone
             * blob as a bare immediate (a tagged pointer into the literal
             * area) only when it is a *persistent static* literal of this
             * module: the retained literal_map keeps it alive for as long
             * as the module — and therefore the blob — is loaded. Dynamic
             * literals (bignum immediates synthesized during this decode;
             * the builder recorded index 0 for them, PLAN/T2FULL/07 §"read
             * TAG_q") live only in the transient decode view and must never
             * be embedded. A T2 blob cannot use ArgLiteral either: the JIT
             * literal ArgVal defers to a loader patch (beam_asm_module.cpp
             * emit_constant embeds LLONG_MAX + records literals[i].patches),
             * which standalone blobs never run — so the retained term is the
             * only usable handle. Returns the retained term on success. */
            bool safe_literal_term(const T2Op *def, Eterm *out) {
                if (def->kind != T2OpKind::ConstLiteral || ctx.ret == nullptr) {
                    return false;
                }
                if (def->index >= (Uint32)ctx.ret->literal_count) {
                    return false;
                }
                Eterm term = ctx.ret->literal_map[def->index];
                if (term != def->imm_term) {
                    /* Index-0 placeholder for a dynamic literal, or a stale
                     * mismatch: not the persistent static literal. */
                    return false;
                }
                *out = term;
                return true;
            }

            /* The canonical home of operand i of `op`: its decoded source
             * register when it came from one, otherwise the operand must
             * be an inline-able constant. */
            bool src_of(const T2Op *op, uint16_t i, T2LirSrc *out) {
                int32_t reg = op->operand_regs != nullptr
                                      ? op->operand_regs[i]
                                      : T2_REG_NONE;

                if (reg != T2_REG_NONE) {
                    *out = T2LirSrc::slot(reg_loc(reg));
                    return true;
                }

                const T2Op *def = op->operands[i]->def;

                if (!is_const_kind(def->kind)) {
                    return fail_op(op,
                                   "operand without a canonical home is not "
                                   "a constant");
                }
                Eterm term = def->kind == T2OpKind::ConstInt
                                     ? make_small(def->imm_int)
                                     : def->imm_term;
                if (!is_immed(term)) {
                    /* Boxed literal operand: only a persistent static
                     * literal may be materialized as a bare immediate (a
                     * tagged literal-area pointer). Correct for value-carrying
                     * contexts (move, put_list/put_tuple2 elements, mixed
                     * arithmetic — the tagged pointer IS the term). Callers
                     * that need term-deep semantics (exact/ordering compares)
                     * reject boxed operands separately (guard_reject_boxed). */
                    if (!safe_literal_term(def, &term)) {
                        return fail_op(op,
                                       "boxed literal operand unsupported in P1 "
                                       "commit-3 isel");
                    }
                }
                *out = T2LirSrc::immediate(term);
                return true;
            }

            /* Type tests and comparisons must never receive a boxed literal
             * as a bare immediate: emit_is_eq_exact/… only take the term-deep
             * path for an ArgLiteral (loader-resolved), and fall back to a
             * *shallow pointer* compare for a plain immediate — wrong for a
             * boxed term. A T2 blob has no ArgLiteral, so reject and let the
             * function stay T1 for these sites. */
            bool guard_has_boxed_literal(const T2Op *op) {
                for (uint16_t i = 0; i < op->num_operands; i++) {
                    bool from_reg = op->operand_regs != nullptr &&
                                    op->operand_regs[i] != T2_REG_NONE;
                    if (from_reg) {
                        continue;
                    }
                    const T2Op *def = op->operands[i]->def;
                    if (!is_const_kind(def->kind)) {
                        continue;
                    }
                    Eterm term = def->kind == T2OpKind::ConstInt
                                         ? make_small(def->imm_int)
                                         : def->imm_term;
                    if (!is_immed(term)) {
                        return true;
                    }
                }
                return false;
            }

            bool fill_srcs(const T2Op *op, T2LirOp *lop) {
                if (op->num_operands > T2_LIR_MAX_SRCS) {
                    return fail_op(op, "too many operands for inline srcs");
                }
                lop->num_srcs = (uint8_t)op->num_operands;
                for (uint16_t i = 0; i < op->num_operands; i++) {
                    if (!src_of(op, i, &lop->srcs[i])) {
                        return false;
                    }
                }
                return true;
            }

            /* ---- cross-tier resolution -------------------------------- */

            const BeamCodeHeader *code_hdr() const {
                return (const BeamCodeHeader *)ctx.code_hdr;
            }

            const void *pc_lookup(uint32_t beam_idx, ErtsT2PcKind kind) {
                return (const void *)erts_t2_pc_lookup_kind(ctx.ret,
                                                            hir.fn_index,
                                                            beam_idx,
                                                            kind);
            }

            /* Local call target: MFA -> the function's T1 entry (the
             * patchable prologue, where T1's own `bl` lands). */
            const void *local_target(Eterm f, uint32_t arity) {
                const BeamCodeHeader *hdr = code_hdr();

                for (Sint i = 0; i < hdr->num_functions; i++) {
                    const ErtsCodeInfo *ci = hdr->functions[i];

                    if (ci->mfa.function == f && ci->mfa.arity == arity) {
                        return (const void *)erts_codeinfo_to_code(ci);
                    }
                }
                return nullptr;
            }

            /* The function's own func_info: on aarch64 the ErtsCodeInfo's
             * first word is a valid `bl <i_func_info_shared>`, so branching
             * to it raises function_clause exactly as T1's guard fails do. */
            const void *func_info_target() {
                const BeamCodeHeader *hdr = code_hdr();

                if ((Sint)hir.fn_index >= hdr->num_functions) {
                    return nullptr;
                }

                const ErtsCodeInfo *ci = hdr->functions[hir.fn_index];

                /* Chunk order and header order must agree (drift check). */
                if (ci->mfa.function != hir.function ||
                    ci->mfa.arity != hir.arity) {
                    return nullptr;
                }
                return (const void *)ci;
            }

            /* External call target: the export entry, dispatched through
             * the active code index at run time (identical to T1's
             * i_call_ext). BIF targets are rejected — T1 lowers those to
             * inlined BIF calls with different reduction accounting, which
             * the identity tier must not approximate. */
            bool export_target(const T2Op *op, const Export **out) {
                const Export *ep = erts_active_export_entry(op->mfa_m,
                                                            op->mfa_f,
                                                            op->index);

                if (ep == nullptr) {
                    return fail_op(op, "no export entry for remote target");
                }
                if (ep->bif_number >= 0) {
                    return fail_op(op,
                                   "call_ext to a BIF unsupported in P1 "
                                   "commit-3 isel");
                }
                *out = ep;
                return true;
            }

            /* ---- guard folding ---------------------------------------- */

            /* True iff `op`'s boolean result is consumed exactly once, by
             * this block's Branch terminator. */
            bool feeds_branch(const T2Op *op) {
                const T2Op *term = op->block->terminator;

                return term != nullptr && term->kind == T2OpKind::Branch &&
                       term->num_operands == 1 &&
                       term->operands[0] == op->result &&
                       uses[op->result->id] == 1;
            }

            /* ---- op lowering ------------------------------------------ */

            /* Returns false on failure; sets *consumed_terminator when the
             * lowered op absorbed the block's Branch terminator. */
            bool emit_op(T2LirBlock &b,
                         const T2Op *op,
                         const T2Op **skip_until,
                         bool *consumed_terminator) {
                T2LirOp lop;

                lop.beam_idx = op->beam_idx;

                /* Generic arithmetic (gc_bif): fail edge in-blob when the
                 * builder's Succeeded/Branch follows; else side exit to the
                 * op's own T1 EFFECT site. */
                T2LirKind ak = arith_kind(op->kind);
                if (ak != T2LirKind::Invalid) {
                    lop.kind = ak;
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "arith result without a home");
                    }
                    lop.dst = reg_loc(op->dst_reg);
                    lop.mfa_m = op->mfa_m;
                    lop.mfa_f = op->mfa_f;
                    lop.live = op->live;
                    if (!fill_srcs(op, &lop)) {
                        return false;
                    }

                    const T2Op *succ = op->next;
                    if (succ != nullptr && succ->kind == T2OpKind::Succeeded &&
                        succ->num_operands == 1 &&
                        succ->operands[0] == op->result &&
                        feeds_branch(succ)) {
                        const T2Op *term = op->block->terminator;

                        lop.succ_then = term->succ_then->id;
                        lop.succ_else = term->succ_else->id;
                        *skip_until = succ; /* consume the Succeeded */
                        *consumed_terminator = true;
                    } else if (succ != nullptr &&
                               succ->kind == T2OpKind::Succeeded) {
                        return fail_op(op,
                                       "succeeded not consumed by the block "
                                       "branch");
                    } else {
                        lop.t1_pc_fail =
                                pc_lookup(op->beam_idx, ERTS_T2_PC_EFFECT);
                        if (lop.t1_pc_fail == nullptr) {
                            return fail_op(op,
                                           "no EFFECT pctab entry for arith "
                                           "side exit");
                        }
                    }
                    b.ops.push_back(lop);
                    return true;
                }

                /* Type tests / comparisons: must fold into the Branch. */
                T2LirKind gk = guard_kind(op->kind);
                if (gk != T2LirKind::Invalid) {
                    if (!feeds_branch(op) || op->next != nullptr) {
                        return fail_op(op,
                                       "guard result not consumed by this "
                                       "block's branch");
                    }
                    if (guard_has_boxed_literal(op)) {
                        return fail_op(op,
                                       "boxed literal operand in comparison/"
                                       "test unsupported (needs literal-pool "
                                       "deep compare) in P1 isel");
                    }
                    const T2Op *term = op->block->terminator;

                    lop.kind = gk;
                    lop.imm = (Sint64)op->index;   /* arity for TestArity /
                                                    * IsTaggedTuple      */
                    lop.imm_term = op->imm_term;   /* IsTaggedTuple tag  */
                    if (!fill_srcs(op, &lop)) {
                        return false;
                    }
                    lop.succ_then = term->succ_then->id;
                    lop.succ_else = term->succ_else->id;
                    *consumed_terminator = true;
                    b.ops.push_back(lop);
                    return true;
                }

                switch (op->kind) {
                case T2OpKind::Param:
                    /* Argument already in its X register at entry. */
                    if (op->dst_reg != (int32_t)op->index) {
                        return fail_op(op, "param home mismatch");
                    }
                    if (op->result->id < lir.num_values) {
                        lir.param_x[op->result->id] = (int32_t)op->index;
                    }
                    return true;

                case T2OpKind::ConstInt:
                case T2OpKind::ConstAtom:
                case T2OpKind::ConstNil:
                case T2OpKind::ConstLiteral:
                    /* Materialized only when decoded into a register
                     * (init_yregs kills, move-of-constant); pure operand
                     * constants emit nothing. */
                    if (op->dst_reg == T2_REG_NONE) {
                        return true;
                    }
                    {
                        Eterm term = op->kind == T2OpKind::ConstInt
                                             ? make_small(op->imm_int)
                                             : op->imm_term;

                        if (!is_immed(term) &&
                            !safe_literal_term(op, &term)) {
                            return fail_op(op,
                                           "boxed literal move unsupported "
                                           "in P1 commit-3 isel");
                        }
                        lop.kind = T2LirKind::Move;
                        lop.dst = reg_loc(op->dst_reg);
                        lop.num_srcs = 1;
                        lop.srcs[0] = T2LirSrc::immediate(term);
                        b.ops.push_back(lop);
                    }
                    return true;

                case T2OpKind::Copy: {
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "copy without a destination home");
                    }

                    /* A flagged pair is a decoded swap: reads precede
                     * writes; emit fused. */
                    if (op->flags & T2_OP_PAIR_HEAD) {
                        const T2Op *tail = op->next;

                        if (tail == nullptr || tail->kind != T2OpKind::Copy ||
                            tail->dst_reg == T2_REG_NONE) {
                            return fail_op(op, "dangling swap pair");
                        }
                        lop.kind = T2LirKind::Swap;
                        lop.dst = reg_loc(op->dst_reg);
                        lop.dst2 = reg_loc(tail->dst_reg);
                        b.ops.push_back(lop);
                        *skip_until = tail;
                        return true;
                    }

                    lop.kind = T2LirKind::Move;
                    lop.dst = reg_loc(op->dst_reg);
                    lop.num_srcs = 1;
                    if (!src_of(op, 0, &lop.srcs[0])) {
                        return false;
                    }
                    b.ops.push_back(lop);
                    return true;
                }

                case T2OpKind::GetHd:
                case T2OpKind::GetTl: {
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "list access without a home");
                    }

                    if (op->flags & T2_OP_PAIR_HEAD) {
                        /* Decoded get_list: destinations may alias the
                         * source; emit fused (emit_get_list handles all
                         * overlaps, as T1 does). */
                        const T2Op *tail = op->next;

                        if (op->kind != T2OpKind::GetHd || tail == nullptr ||
                            tail->kind != T2OpKind::GetTl ||
                            tail->dst_reg == T2_REG_NONE) {
                            return fail_op(op, "dangling get_list pair");
                        }
                        lop.kind = T2LirKind::GetList;
                        lop.dst = reg_loc(op->dst_reg);
                        lop.dst2 = reg_loc(tail->dst_reg);
                        lop.num_srcs = 1;
                        if (!src_of(op, 0, &lop.srcs[0])) {
                            return false;
                        }
                        b.ops.push_back(lop);
                        *skip_until = tail;
                        return true;
                    }

                    lop.kind = op->kind == T2OpKind::GetHd ? T2LirKind::GetHd
                                                           : T2LirKind::GetTl;
                    lop.dst = reg_loc(op->dst_reg);
                    lop.num_srcs = 1;
                    if (!src_of(op, 0, &lop.srcs[0])) {
                        return false;
                    }
                    b.ops.push_back(lop);
                    return true;
                }

                case T2OpKind::GetTupleElement:
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "tuple access without a home");
                    }
                    lop.kind = T2LirKind::GetTupleElement;
                    lop.dst = reg_loc(op->dst_reg);
                    lop.imm = (Sint64)op->index;
                    lop.num_srcs = 1;
                    if (!src_of(op, 0, &lop.srcs[0])) {
                        return false;
                    }
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::MakeList:
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "put_list without a home");
                    }
                    lop.kind = T2LirKind::MakeList;
                    lop.dst = reg_loc(op->dst_reg);
                    if (!fill_srcs(op, &lop)) {
                        return false;
                    }
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::MakeTuple: {
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "put_tuple2 without a home");
                    }
                    lop.kind = T2LirKind::MakeTuple;
                    lop.dst = reg_loc(op->dst_reg);
                    if (op->num_operands <= T2_LIR_MAX_SRCS) {
                        if (!fill_srcs(op, &lop)) {
                            return false;
                        }
                    } else {
                        lop.pool_first = (uint32_t)lir.src_pool.size();
                        lop.num_srcs_ext = op->num_operands;
                        for (uint16_t i = 0; i < op->num_operands; i++) {
                            T2LirSrc s;

                            if (!src_of(op, i, &s)) {
                                return false;
                            }
                            lir.src_pool.push_back(s);
                        }
                    }
                    b.ops.push_back(lop);
                    return true;
                }

                case T2OpKind::GcTest:
                    lop.kind = T2LirKind::GcTest;
                    lop.imm = (Sint64)op->index; /* heap words */
                    lop.live = op->live;
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::Allocate:
                    lop.kind = T2LirKind::Allocate;
                    lop.imm = (Sint64)op->index; /* stack slots */
                    lop.imm2 = op->imm_int;      /* fused heap words */
                    lop.live = op->live;
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::Deallocate:
                    lop.kind = T2LirKind::Deallocate;
                    lop.imm = (Sint64)op->index;
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::Trim:
                    lop.kind = T2LirKind::Trim;
                    lop.imm = (Sint64)op->index;
                    lop.imm2 = op->imm_int;
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::Call:
                case T2OpKind::CallExt: {
                    bool is_ext = op->kind == T2OpKind::CallExt;

                    lop.kind = is_ext ? T2LirKind::CallExt : T2LirKind::Call;
                    lop.mfa_m = op->mfa_m;
                    lop.mfa_f = op->mfa_f;
                    lop.arity = op->index;
                    lop.dst = PhysLoc::xreg(0);

                    /* The CP: the T1 post-call continuation of this call's
                     * decode ordinal — never a T2 address (08 §4.3). The
                     * callee returns into T1; the rest of the invocation
                     * runs T1 (demote-on-return). */
                    lop.t1_pc_cont = pc_lookup(op->beam_idx, ERTS_T2_PC_CONT);
                    if (lop.t1_pc_cont == nullptr) {
                        return fail_op(op,
                                       "no CONT pctab entry for the call's "
                                       "T1 continuation");
                    }

                    if (is_ext) {
                        const Export *ep;

                        if (!export_target(op, &ep)) {
                            return false;
                        }
                        lop.exp = (const void *)ep;
                    } else {
                        lop.target = local_target(op->mfa_f, op->index);
                        if (lop.target == nullptr) {
                            return fail_op(op, "local call target not found");
                        }
                    }
                    b.ops.push_back(lop);
                    return true;
                }

                default:
                    return fail_op(op, "unsupported HIR op in P1 isel");
                }
            }

            bool emit_terminator(T2LirBlock &b, const T2Op *t) {
                T2LirOp lop;

                if (t == nullptr) {
                    return fail("block without terminator");
                }
                lop.beam_idx = t->beam_idx;

                switch (t->kind) {
                case T2OpKind::Return:
                    lop.kind = T2LirKind::Return;
                    if (t->num_operands != 1) {
                        return fail("return without a single value");
                    }
                    lop.num_srcs = 1;
                    if (!src_of(t, 0, &lop.srcs[0])) {
                        return false;
                    }
                    if (!lop.srcs[0].is_const &&
                        lop.srcs[0].loc != PhysLoc::xreg(0)) {
                        return fail("return value not in x0");
                    }
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::Jump:
                    lop.kind = T2LirKind::Jump;
                    lop.succ_then = t->succ_then->id;
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::Switch: {
                    lop.kind = T2LirKind::Switch;
                    lop.num_srcs = 1;
                    if (!src_of(t, 0, &lop.srcs[0])) {
                        return false;
                    }
                    lop.first_case = (uint32_t)lir.switch_cases.size();
                    lop.num_cases = t->num_cases;
                    for (uint32_t i = 0; i < t->num_cases; i++) {
                        if (!is_immed(t->cases[i].value)) {
                            return fail("boxed switch value unsupported in "
                                        "P1 commit-3 isel");
                        }
                        lir.switch_cases.push_back(T2LirSwitchCase{
                                t->cases[i].value,
                                t->cases[i].target->id});
                    }
                    lop.default_target = t->default_target->id;
                    b.ops.push_back(lop);
                    return true;
                }

                case T2OpKind::TailCall:
                case T2OpKind::TailCallExt: {
                    bool is_ext = t->kind == T2OpKind::TailCallExt;

                    /* Error exits lower to a side exit: an unconditional
                     * branch to a T1 PC. T2 never raises (surprise #7). */
                    if (t->flags & T2_OP_ERR_EXIT_SHARED) {
                        lop.kind = T2LirKind::SideExit;
                        lop.t1_pc_fail = func_info_target();
                        if (lop.t1_pc_fail == nullptr) {
                            return fail("cannot resolve func_info for the "
                                        "shared error exit");
                        }
                        b.ops.push_back(lop);
                        return true;
                    }
                    if (t->flags & T2_OP_ERR_EXIT_OP) {
                        /* The `NotInX=cy` transform splits a leading move
                         * off badmatch/case_end, so the recorded T1 PC
                         * assumes an X-register source; only that shape
                         * side-exits exactly. */
                        if (t->num_operands == 1 &&
                            (t->operand_regs == nullptr ||
                             !t2_reg_is_x(t->operand_regs[0]))) {
                            return fail_op(t,
                                           "error-exit source not in an X "
                                           "register");
                        }
                        lop.kind = T2LirKind::SideExit;
                        lop.t1_pc_fail =
                                pc_lookup(t->beam_idx, ERTS_T2_PC_ERROR);
                        if (lop.t1_pc_fail == nullptr) {
                            return fail_op(t,
                                           "no ERROR pctab entry for the "
                                           "error exit");
                        }
                        b.ops.push_back(lop);
                        return true;
                    }
                    if (t->flags & T2_OP_GARBAGE_DEALLOC) {
                        return fail_op(t,
                                       "garbage-dealloc transfer unsupported "
                                       "in P1 isel (no-return call)");
                    }

                    lop.kind = is_ext ? T2LirKind::TailCallExt
                                      : T2LirKind::TailCall;
                    lop.mfa_m = t->mfa_m;
                    lop.mfa_f = t->mfa_f;
                    lop.arity = t->index;
                    if (is_ext) {
                        const Export *ep;

                        if (!export_target(t, &ep)) {
                            return false;
                        }
                        lop.exp = (const void *)ep;
                    } else {
                        /* Self-recursion included: identity only, the
                         * transfer goes to the (own) T1 entry — no
                         * back-edge optimization in P1. */
                        lop.target = local_target(t->mfa_f, t->index);
                        if (lop.target == nullptr) {
                            return fail_op(t, "local tail target not found");
                        }
                    }
                    b.ops.push_back(lop);
                    return true;
                }

                default:
                    return fail(std::string("unsupported terminator in P1 "
                                            "isel (") +
                                t2_op_kind_name(t->kind) + ")");
                }
            }

            bool run() {
                if (!hir.sync_complete) {
                    return fail("P1 isel requires builder sync metadata");
                }

                lir.module = hir.module;
                lir.function = hir.function;
                lir.arity = hir.arity;
                lir.num_values = (uint32_t)hir.values.size();
                lir.param_x.assign(lir.num_values, -1);

                if (ctx.ret == nullptr || ctx.code_hdr == nullptr) {
                    return fail("isel context incomplete");
                }

                /* Use counts for the guard-fold checks. */
                uses.assign(hir.values.size(), 0);
                for (const T2BasicBlock *hb : hir.blocks) {
                    auto count = [&](const T2Op *op) {
                        for (uint16_t i = 0; i < op->num_operands; i++) {
                            uses[op->operands[i]->id]++;
                        }
                    };
                    for (const T2Op *p = hb->phis_head; p != nullptr;
                         p = p->next) {
                        count(p);
                    }
                    for (const T2Op *op = hb->ops_head; op != nullptr;
                         op = op->next) {
                        count(op);
                    }
                    if (hb->terminator != nullptr) {
                        count(hb->terminator);
                    }
                }

                for (const T2BasicBlock *hb : hir.blocks) {
                    T2LirBlock &lb = lir.new_block();
                    bool consumed_terminator = false;

                    /* Phis are no-ops under identity placement: every
                     * predecessor materialized the value in the phi's
                     * home register (proven by the HIR validator's
                     * merge-slot walk). */

                    for (const T2Op *op = hb->ops_head; op != nullptr;
                         op = op->next) {
                        const T2Op *skip_until = nullptr;

                        if (!emit_op(lb, op, &skip_until,
                                     &consumed_terminator)) {
                            return false;
                        }
                        if (skip_until != nullptr) {
                            op = skip_until;
                        }
                    }

                    if (!consumed_terminator &&
                        !emit_terminator(lb, hb->terminator)) {
                        return false;
                    }
                }

                return true;
            }
        };

    } /* anonymous namespace */

    bool t2_isel(const T2Function &hir,
                 const T2IselContext &ctx,
                 T2LirFunction &lir,
                 std::string *err) {
        Isel isel{hir, ctx, lir, err, {}};
        return isel.run();
    }

} /* namespace erts_t2 */
