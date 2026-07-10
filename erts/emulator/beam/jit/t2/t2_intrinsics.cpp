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
 * T2-Full tier-2 JIT: foldl-class lists intrinsics + LICM-lite. See
 * t2_intrinsics.hpp for the design summary.
 *
 * The expansion template (rotated: the nil/cons dispatch sits on both
 * loop entry edges, so the loop body always sees a cons and one fun
 * body copy suffices). For lists:foldl/3 — the general shape; foreach/
 * all/any differ only in arity, exit values, the early-exit dispatch
 * and the extra wrapper->helper charge (foreach):
 *
 *   B_pre    (the call's block, call removed):
 *              ... original ops ...
 *              RC#1 {callee=lists:foldl/3 (wrapper), charge 1,
 *                    map = the call's own sync map (F,A0,L0 + frame)}
 *              jump B_chk
 *   B_chk:     is_nil L0     -> B_exit0 | B_chk2
 *   B_chk2:    is_cons L0    -> B_grd   | B_dm_entry
 *   B_dm_entry: DemoteCallee {wrapper body, (F,A0,L0)}
 *                 (T1's wrapper raises case_clause for a bad head)
 *   B_grd:     [entry guards: SpeculateType A0 -> wrapper body]
 *              jump B_body
 *   B_body:    acc = phi(A0, rv')   @x1
 *              l   = phi(L0, t')    @x2
 *              h = get_hd l; t = get_tl l        (fresh X homes)
 *              <fun body, spliced; guards/flag-checked arith deopt
 *               to B_dm_iter's contract via T2_OP_WINDOW_CALLEE
 *               (helper body + CP push); error edges -> B_dm_iter>
 *              ... -> B_ret
 *   B_ret:     rv = phi(returns)                 (fresh X home)
 *              rv' = copy rv @x1 ; t' = copy t @x2
 *              RC#2 {callee=lists:foldl_1/3 (helper), charge 2,
 *                    map = (F, rv', t') + frame}
 *              jump B_lat
 *   B_lat:     is_nil t'     -> B_exit1 | B_lat2
 *   B_lat2:    is_cons t'    -> B_body (back edge) | B_dm_next
 *   B_dm_next: DemoteCallee {helper body, (F, rv', t')}
 *                 (T1's helper raises function_clause for a bad tail)
 *   B_dm_iter: DemoteCallee {helper body, (F, acc, l)}
 *                 (re-execute the iteration as a fresh helper call)
 *   B_exit0:   copy A0 @x0 ; jump B_join
 *   B_exit1:   copy rv' @x0 ; jump B_join
 *   B_join:    res = phi(exits) @x0
 *              ... original post-call ops + terminator ...
 *
 * Reduction identity against lists.erl's shapes (wrapper `foldl`
 * dispatching to helper `foldl_1`, both charging one reduction per
 * call, the fun one per element):
 *
 *   n elements, no yield:  1 (RC#1 = wrapper entry)
 *                        + 3n (RC#2 per element = fun entry + fun
 *                              return dispatch + next self call — the
 *                              JIT charges returns too)
 *                        + 1 (exit edge = the chain's single return)
 *                        = T1's 2 + 3n exactly (the final RC#2 pays
 *                          for the helper's [] clause call, as T1
 *                          does); n = 0 charges 2, as T1 does.
 *   all/any early exit at element k: 1 + 3(k-1) + 3 (ChargeReds on
 *     the exit edge = fun entry + fun return + chain return)
 *     = 3k + 1, T1-exact.
 *   foreach: an extra RC {helper, charge 1} follows RC#1 — T1's
 *     wrapper is a plain tail-dispatch (no case), so entry+dispatch
 *     charge 2 before the first list inspection, and a bad list head
 *     raises function_clause@foreach_1, not case_clause: B_dm_entry
 *     targets the HELPER for foreach.
 *   yields: RC#1/RC#2 route through i_test_yield_shared under the
 *     wrapper/helper MFA with the callee vector saved — position,
 *     charge and introspection identical to T1 yielding at the
 *     wrapper/helper entry; resume re-enters the loop.
 */

#include "t2_intrinsics.hpp"

extern "C"
{
#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "big.h"
#include "code_ix.h"
#include "module.h"
#include "beam_code.h"

#include "t2_retain.h"
#include "t2_pctab.h"
}

#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace erts_t2 {

    namespace {

        constexpr uint32_t T2_INTRIN_MAX_FUN_OPS = 24;
        constexpr uint32_t T2_INTRIN_MAX_FUN_BLOCKS = 8;

        enum class IClass { Foldl, Foreach, All, Any };

        struct IntrinsicKind {
            IClass cls;
            const char *wrapper;
            const char *helper;
            uint32_t call_arity; /* == the fresh-call vector length */
            uint32_t fun_arity;
        };

        const IntrinsicKind t2_intrinsic_kinds[] = {
                {IClass::Foldl, "foldl", "foldl_1", 3, 2},
                {IClass::Foreach, "foreach", "foreach_1", 2, 1},
                {IClass::All, "all", "all_1", 2, 1},
                {IClass::Any, "any", "any_1", 2, 1},
        };

        bool intrin_disabled() {
            static const bool off = getenv("T2_NO_INTRIN") != nullptr;
            return off;
        }

        bool intrin_trace() {
            static const bool on = getenv("T2_INTRIN_TRACE") != nullptr;
            return on;
        }

        const T2Value *resolve_copies(const T2Value *v) {
            for (int depth = 0; depth < 64; depth++) {
                if (v->def == nullptr || v->def->kind != T2OpKind::Copy) {
                    break;
                }
                v = v->def->operands[0];
            }
            return v;
        }

        bool is_const_def(const T2Value *v) {
            if (v->def == nullptr) {
                return false;
            }
            switch (v->def->kind) {
            case T2OpKind::ConstInt:
            case T2OpKind::ConstAtom:
            case T2OpKind::ConstNil:
            case T2OpKind::ConstLiteral:
                return true;
            default:
                return false;
            }
        }

        bool const_is_small(const T2Value *v) {
            const T2Op *def = v->def;

            if (def == nullptr) {
                return false;
            }
            if (def->kind == T2OpKind::ConstInt) {
                return IS_SSMALL(def->imm_int);
            }
            return false;
        }

        void unlink_op(T2BasicBlock *b, T2Op *op) {
            if (op->prev != nullptr) {
                op->prev->next = op->next;
            } else {
                b->ops_head = op->next;
            }
            if (op->next != nullptr) {
                op->next->prev = op->prev;
            } else {
                b->ops_tail = op->prev;
            }
            op->prev = op->next = nullptr;
        }

        template<typename F>
        void for_each_op(T2Function &fn, F f) {
            for (T2BasicBlock *b : fn.blocks) {
                for (T2Op *phi = b->phis_head; phi != nullptr;
                     phi = phi->next) {
                    f(phi);
                }
                for (T2Op *op = b->ops_head; op != nullptr; op = op->next) {
                    f(op);
                }
                if (b->terminator != nullptr) {
                    f(b->terminator);
                }
            }
        }

        void replace_value(T2Function &fn, T2Value *from, T2Value *to) {
            for_each_op(fn, [&](T2Op *op) {
                for (uint16_t i = 0; i < op->num_operands; i++) {
                    if (op->operands[i] == from) {
                        op->operands[i] = to;
                    }
                }
                if (op->sync != nullptr) {
                    T2SyncMap *m = op->sync;

                    for (uint32_t i = 0; i < m->x_live; i++) {
                        if (m->x[i] == from) {
                            m->x[i] = to;
                        }
                    }
                    for (int32_t i = 0; m->frame_size != T2_NO_FRAME &&
                                        i < m->frame_size;
                         i++) {
                        if (m->y[i] == from) {
                            m->y[i] = to;
                        }
                    }
                }
            });
        }

        struct Expander {
            T2Function &fn;
            const ErtsT2RetainedCode *ret;
            const void *own_code_hdr;
            std::string *err;
            bool changed = false;

            bool fail(const char *what) {
                if (err != nullptr) {
                    *err = std::string("intrinsics: ") + what;
                }
                return false;
            }

            /* ---- resolution ------------------------------------------ */

            struct Callee {
                Eterm mfa_m = NIL;
                Eterm wrapper_f = NIL;
                Eterm helper_f = NIL;
                const void *wrapper_lf = nullptr;
                const void *helper_lf = nullptr;
                const void *lists_hdr = nullptr;
            };

            static const void *find_lf(const BeamCodeHeader *hdr,
                                       Eterm f,
                                       uint32_t arity) {
                for (Uint i = 0; i < hdr->num_functions; i++) {
                    const ErtsCodeInfo *ci = hdr->functions[i];

                    if (ci->mfa.function == f && ci->mfa.arity == arity) {
                        return (const void *)erts_codeinfo_to_code(ci);
                    }
                }
                return nullptr;
            }

            bool resolve_callee(const IntrinsicKind &k, Callee *out) {
                Eterm am_lists_mod = ERTS_MAKE_AM("lists");
                Module *lm = erts_get_module(am_lists_mod,
                                             erts_active_code_ix());

                if (lm == nullptr || lm->curr.code_hdr == nullptr) {
                    return false;
                }

                const BeamCodeHeader *hdr =
                        (const BeamCodeHeader *)lm->curr.code_hdr;

                out->mfa_m = am_lists_mod;
                out->wrapper_f = erts_atom_put((const byte *)k.wrapper,
                                               sys_strlen(k.wrapper),
                                               ERTS_ATOM_ENC_LATIN1,
                                               1);
                out->helper_f = erts_atom_put((const byte *)k.helper,
                                              sys_strlen(k.helper),
                                              ERTS_ATOM_ENC_LATIN1,
                                              1);
                out->wrapper_lf = find_lf(hdr, out->wrapper_f, k.call_arity);
                out->helper_lf = find_lf(hdr, out->helper_f, k.call_arity);
                out->lists_hdr = (const void *)hdr;
                return out->wrapper_lf != nullptr &&
                       out->helper_lf != nullptr;
            }

            /* ---- fun body admission + clone --------------------------- */

            /* Everything about the spliced fun body: cloned blocks (in
             * fn), the entry block's clone target, the return join. */
            struct FunBody {
                /* Value -> canonical home for operand_regs synthesis. */
                std::unordered_map<const T2Value *, int32_t> home;
                uint32_t next_x;

                T2BasicBlock *entry = nullptr; /* first cloned block   */
                T2BasicBlock *ret_join = nullptr;
                T2Value *ret_phi = nullptr;

                /* Ops needing speculation conversion, in order. */
                std::vector<T2Op *> arith;
            };

            int32_t fresh_home(FunBody &fb) {
                return t2_xreg(fb.next_x++);
            }

            static bool is_err_block(const T2BasicBlock *b) {
                return b != nullptr && b->terminator != nullptr &&
                       (b->terminator->flags &
                        (T2_OP_ERR_EXIT_SHARED | T2_OP_ERR_EXIT_OP)) != 0 &&
                       b->phis_head == nullptr;
            }

            static bool fun_op_ok(const T2Op *op) {
                if ((op->flags &
                     (T2_OP_ERR_EXIT_OP | T2_OP_GARBAGE_DEALLOC)) != 0) {
                    return false;
                }
                if (op->dst_reg != T2_REG_NONE &&
                    !t2_reg_is_x(op->dst_reg)) {
                    return false;
                }
                switch (op->kind) {
                case T2OpKind::ConstInt:
                case T2OpKind::ConstAtom:
                case T2OpKind::ConstNil:
                case T2OpKind::ConstLiteral:
                case T2OpKind::Copy:
                case T2OpKind::IsInteger:
                case T2OpKind::IsFloat:
                case T2OpKind::IsNumber:
                case T2OpKind::IsAtom:
                case T2OpKind::IsBoolean:
                case T2OpKind::IsTuple:
                case T2OpKind::IsList:
                case T2OpKind::IsNonemptyList:
                case T2OpKind::IsNil:
                case T2OpKind::IsMap:
                case T2OpKind::IsPid:
                case T2OpKind::IsPort:
                case T2OpKind::IsReference:
                case T2OpKind::IsFunction:
                case T2OpKind::IsTaggedTuple:
                case T2OpKind::TestArity:
                case T2OpKind::CmpEqExact:
                case T2OpKind::CmpNeExact:
                case T2OpKind::CmpEq:
                case T2OpKind::CmpNe:
                case T2OpKind::CmpLt:
                case T2OpKind::CmpLe:
                case T2OpKind::CmpGt:
                case T2OpKind::CmpGe:
                case T2OpKind::GetTupleElement:
                case T2OpKind::GetHd:
                case T2OpKind::GetTl:
                    return true;
                case T2OpKind::Add:
                case T2OpKind::Sub:
                    /* Convertible flag-checked shape only: a plain
                     * 2-operand body-context arith (no Succeeded
                     * follower — guard-context arith is rejected). */
                    return op->num_operands == 2 && op->result != nullptr &&
                           op->dst_reg != T2_REG_NONE &&
                           (op->next == nullptr ||
                            op->next->kind != T2OpKind::Succeeded);
                default:
                    return false;
                }
            }

            /* Mutation-free admission (pass 1 of splice_fun): can this
             * callee be spliced at all? Called BEFORE the caller is
             * rewritten, so a rejected site stays a plain call_ext and
             * the function keeps compiling exactly as before. */
            /* Is a fun return value provably boolean? (a value-
             * producing comparison or a true/false constant, through
             * Copies.) all/any require this so the bad-result edge —
             * T1's case_clause, whose MFA differs between the wrapper
             * (element 1) and the helper (later elements) — is
             * unreachable rather than approximated. */
            static bool ret_is_bool(const T2Value *v) {
                const T2Value *root = resolve_copies(v);
                const T2Op *def = root->def;

                if (def == nullptr) {
                    return false;
                }
                switch (def->kind) {
                case T2OpKind::CmpEqExact:
                case T2OpKind::CmpNeExact:
                case T2OpKind::CmpEq:
                case T2OpKind::CmpNe:
                case T2OpKind::CmpLt:
                case T2OpKind::CmpLe:
                case T2OpKind::CmpGt:
                case T2OpKind::CmpGe:
                case T2OpKind::IsInteger:
                case T2OpKind::IsFloat:
                case T2OpKind::IsNumber:
                case T2OpKind::IsAtom:
                case T2OpKind::IsBoolean:
                case T2OpKind::IsTuple:
                case T2OpKind::IsList:
                case T2OpKind::IsNonemptyList:
                case T2OpKind::IsNil:
                case T2OpKind::IsMap:
                case T2OpKind::IsPid:
                case T2OpKind::IsPort:
                case T2OpKind::IsReference:
                case T2OpKind::IsFunction:
                    return true;
                case T2OpKind::ConstAtom:
                    return def->imm_term == am_true ||
                           def->imm_term == am_false;
                default:
                    return false;
                }
            }

            static bool admit_fun(const T2Function &callee,
                                  uint32_t fun_arity,
                                  bool need_bool_returns) {
                if (callee.blocks.empty() ||
                    callee.blocks.size() > T2_INTRIN_MAX_FUN_BLOCKS ||
                    callee.arity != fun_arity) {
                    return false;
                }

                uint32_t nops = 0;
                bool any_return = false;

                for (size_t i = 0; i < callee.blocks.size(); i++) {
                    const T2BasicBlock *b = callee.blocks[i];

                    if (is_err_block(b)) {
                        continue;
                    }
                    for (const T2Op *op = b->ops_head; op != nullptr;
                         op = op->next) {
                        if (op->kind == T2OpKind::Param) {
                            if (i != 0) {
                                return false;
                            }
                            continue;
                        }
                        if (!fun_op_ok(op)) {
                            return false;
                        }
                        if (op->operand_regs != nullptr) {
                            for (uint16_t j = 0; j < op->num_operands;
                                 j++) {
                                if (op->operand_regs[j] != T2_REG_NONE &&
                                    !t2_reg_is_x(op->operand_regs[j])) {
                                    return false;
                                }
                            }
                        }
                        /* Flag-checked conversion cannot swallow a
                         * non-small constant operand. */
                        if (op->kind == T2OpKind::Add ||
                            op->kind == T2OpKind::Sub) {
                            for (uint16_t j = 0; j < op->num_operands;
                                 j++) {
                                const T2Value *v = op->operands[j];

                                if (is_const_def(v) &&
                                    !const_is_small(v)) {
                                    return false;
                                }
                            }
                        }
                        nops++;
                    }
                    const T2Op *t = b->terminator;

                    if (t == nullptr) {
                        return false;
                    }
                    switch (t->kind) {
                    case T2OpKind::Return:
                        any_return = true;
                        if (need_bool_returns &&
                            !ret_is_bool(t->operands[0])) {
                            return false;
                        }
                        break;
                    case T2OpKind::Jump:
                    case T2OpKind::Branch:
                    case T2OpKind::Switch:
                        break;
                    default:
                        return false;
                    }
                }
                return any_return && nops <= T2_INTRIN_MAX_FUN_OPS;
            }

            /* Clone the (validated, eligible) fun implementation into
             * `fn`. `args` are the values standing in for the fun's
             * parameters, `arg_homes` the X slots they occupy at the
             * splice point. On success fills `fb`; on admission failure
             * returns false with fb untouched (site left alone; *err
             * NOT set). `demote` is the in-loop demote block every
             * error edge retargets to. */
            bool splice_fun(const T2Function &callee,
                            const std::vector<T2Value *> &args,
                            const std::vector<int32_t> &arg_homes,
                            T2BasicBlock *demote,
                            uint32_t first_free_x,
                            FunBody *fb_out) {
                FunBody fb;

                fb.next_x = first_free_x;

                if (callee.blocks.empty() ||
                    callee.blocks.size() > T2_INTRIN_MAX_FUN_BLOCKS ||
                    callee.arity != args.size()) {
                    return false;
                }

                /* Pass 1: admit + classify blocks. */
                std::vector<int> kind(callee.blocks.size(), 0);
                /* 0 = clone, 1 = error block (retarget edges) */
                uint32_t nops = 0;

                for (size_t i = 0; i < callee.blocks.size(); i++) {
                    const T2BasicBlock *b = callee.blocks[i];

                    if (is_err_block(b)) {
                        kind[i] = 1;
                        continue;
                    }
                    for (const T2Op *op = b->ops_head; op != nullptr;
                         op = op->next) {
                        if (op->kind == T2OpKind::Param) {
                            if (i != 0) {
                                return false;
                            }
                            continue;
                        }
                        if (!fun_op_ok(op)) {
                            return false;
                        }
                        if (op->operand_regs != nullptr) {
                            for (uint16_t j = 0; j < op->num_operands;
                                 j++) {
                                if (op->operand_regs[j] != T2_REG_NONE &&
                                    !t2_reg_is_x(op->operand_regs[j])) {
                                    return false;
                                }
                            }
                        }
                        nops++;
                    }
                    const T2Op *t = b->terminator;

                    if (t == nullptr) {
                        return false;
                    }
                    switch (t->kind) {
                    case T2OpKind::Return:
                    case T2OpKind::Jump:
                    case T2OpKind::Branch:
                    case T2OpKind::Switch:
                        break;
                    default:
                        return false;
                    }
                }
                if (nops > T2_INTRIN_MAX_FUN_OPS) {
                    return false;
                }

                /* Pass 2: create the clone blocks. */
                std::unordered_map<const T2BasicBlock *, T2BasicBlock *>
                        bmap;

                for (size_t i = 0; i < callee.blocks.size(); i++) {
                    if (kind[i] == 0) {
                        bmap[callee.blocks[i]] = fn.new_block();
                    }
                }
                fb.entry = bmap.at(callee.blocks[0]);
                fb.ret_join = fn.new_block();

                auto target_of = [&](T2BasicBlock *cb) -> T2BasicBlock * {
                    auto it = bmap.find(cb);

                    if (it != bmap.end()) {
                        return it->second;
                    }
                    return demote; /* error blocks collapse onto demote */
                };

                /* Pass 3: clone values/ops. */
                std::unordered_map<const T2Value *, T2Value *> vmap;

                /* Params: the leading run of block 0. */
                {
                    const T2Op *op = callee.blocks[0]->ops_head;
                    uint32_t seen = 0;

                    for (; op != nullptr && op->kind == T2OpKind::Param;
                         op = op->next) {
                        if (op->index >= args.size()) {
                            return false;
                        }
                        vmap[op->result] = args[op->index];
                        fb.home[args[op->index]] = arg_homes[op->index];
                        seen++;
                    }
                    if (seen != callee.arity) {
                        return false;
                    }
                }

                struct RetEdge {
                    T2BasicBlock *from;
                    T2Value *val;
                };
                std::vector<RetEdge> rets;

                /* Clone phis first (their inputs are filled after ops
                 * so forward refs resolve). */
                struct PendingPhi {
                    const T2Op *src;
                    T2Op *clone;
                };
                std::vector<PendingPhi> phis;

                for (size_t i = 0; i < callee.blocks.size(); i++) {
                    if (kind[i] != 0) {
                        continue;
                    }
                    const T2BasicBlock *b = callee.blocks[i];
                    T2BasicBlock *nb = bmap.at(b);

                    for (const T2Op *phi = b->phis_head; phi != nullptr;
                         phi = phi->next) {
                        T2Op *np = fn.new_phi(nb, phi->type);

                        np->dst_reg = fresh_home(fb);
                        np->beam_idx = 0;
                        np->flags = T2_OP_INLINED;
                        vmap[phi->result] = np->result;
                        fb.home[np->result] = np->dst_reg;
                        phis.push_back({phi, np});
                    }
                }

                auto mapped = [&](const T2Value *v) -> T2Value * {
                    auto it = vmap.find(v);

                    return it == vmap.end() ? nullptr : it->second;
                };

                for (size_t i = 0; i < callee.blocks.size(); i++) {
                    if (kind[i] != 0) {
                        continue;
                    }
                    const T2BasicBlock *b = callee.blocks[i];
                    T2BasicBlock *nb = bmap.at(b);

                    for (const T2Op *op = b->ops_head; op != nullptr;
                         op = op->next) {
                        if (op->kind == T2OpKind::Param) {
                            continue;
                        }

                        T2Op *cl = fn.new_op(nb, op->kind, op->type);
                        std::vector<T2Value *> ins;

                        for (uint16_t j = 0; j < op->num_operands; j++) {
                            T2Value *mv = mapped(op->operands[j]);

                            if (mv == nullptr) {
                                return fail("fun operand escapes the "
                                            "value map");
                            }
                            ins.push_back(mv);
                        }
                        fn.set_operands(cl, ins);

                        cl->imm_int = op->imm_int;
                        cl->imm_term = op->imm_term;
                        cl->mfa_m = op->mfa_m;
                        cl->mfa_f = op->mfa_f;
                        cl->index = op->index;
                        cl->bif_num = op->bif_num;
                        cl->live = op->live;
                        cl->flags = T2_OP_INLINED |
                                    (op->flags & T2_OP_TUPLE_ARITY_FUSED);
                        cl->sync = nullptr;
                        cl->beam_idx = 0;

                        if (op->result != nullptr) {
                            vmap[op->result] = cl->result;
                            if (op->dst_reg != T2_REG_NONE) {
                                cl->dst_reg = fresh_home(fb);
                                fb.home[cl->result] = cl->dst_reg;
                            } else {
                                cl->dst_reg = T2_REG_NONE;
                            }
                        }

                        /* Operand homes: the mapped values' homes at
                         * the splice point (constants have none). */
                        if (op->num_operands > 0) {
                            cl->operand_regs =
                                    fn.arena.alloc_array<int32_t>(
                                            op->num_operands);
                            for (uint16_t j = 0; j < op->num_operands;
                                 j++) {
                                auto hit = fb.home.find(cl->operands[j]);

                                cl->operand_regs[j] =
                                        hit != fb.home.end()
                                                ? hit->second
                                                : T2_REG_NONE;
                            }
                        }

                        if (op->kind == T2OpKind::Add ||
                            op->kind == T2OpKind::Sub) {
                            fb.arith.push_back(cl);
                        }
                    }

                    /* Terminator. */
                    const T2Op *t = b->terminator;

                    switch (t->kind) {
                    case T2OpKind::Return: {
                        T2Value *rv = mapped(t->operands[0]);

                        if (rv == nullptr) {
                            return fail("fun return escapes the map");
                        }
                        rets.push_back({nb, rv});
                        fn.emit_jump(nb, fb.ret_join);
                        break;
                    }
                    case T2OpKind::Jump:
                        fn.emit_jump(nb, target_of(t->succ_then));
                        break;
                    case T2OpKind::Branch: {
                        T2Value *cond = mapped(t->operands[0]);

                        if (cond == nullptr) {
                            return fail("fun branch cond escapes the map");
                        }
                        fn.emit_branch(nb,
                                       cond,
                                       target_of(t->succ_then),
                                       target_of(t->succ_else));
                        break;
                    }
                    case T2OpKind::Switch: {
                        T2Value *v = mapped(t->operands[0]);

                        if (v == nullptr) {
                            return fail("fun switch value escapes the "
                                        "map");
                        }
                        T2Op *sw = fn.new_op(nb,
                                             T2OpKind::Switch,
                                             T2Type::none());

                        fn.set_operands(sw, {v});
                        sw->operand_regs =
                                fn.arena.alloc_array<int32_t>(1);
                        {
                            auto hit = fb.home.find(v);

                            sw->operand_regs[0] = hit != fb.home.end()
                                                          ? hit->second
                                                          : T2_REG_NONE;
                        }
                        sw->flags = t->flags & T2_OP_SWITCH_ARITY;
                        sw->num_cases = t->num_cases;
                        sw->cases = fn.arena.alloc_array<T2SwitchCase>(
                                t->num_cases);
                        for (uint32_t c = 0; c < t->num_cases; c++) {
                            sw->cases[c].value = t->cases[c].value;
                            sw->cases[c].target =
                                    target_of(t->cases[c].target);
                        }
                        sw->default_target = target_of(t->default_target);
                        sw->beam_idx = 0;
                        break;
                    }
                    default:
                        return false;
                    }
                }

                /* Phi inputs. */
                for (const PendingPhi &pp : phis) {
                    std::vector<T2Value *> vals;
                    std::vector<T2BasicBlock *> preds;

                    for (uint16_t j = 0; j < pp.src->num_operands; j++) {
                        T2Value *mv = mapped(pp.src->operands[j]);
                        auto bit = bmap.find(pp.src->phi_blocks[j]);

                        if (mv == nullptr || bit == bmap.end()) {
                            return fail("fun phi input escapes the map");
                        }
                        vals.push_back(mv);
                        preds.push_back(bit->second);
                        /* The input must sit in the phi's home at the
                         * predecessor's exit: materialize with a Copy
                         * unless it already lives there. */
                    }
                    /* Insert per-edge Copies into the phi home. */
                    for (size_t j = 0; j < vals.size(); j++) {
                        T2Op *cp = fn.new_op(preds[j],
                                             T2OpKind::Copy,
                                             vals[j]->type);
                        /* new_op appended at tail — but the tail is the
                         * terminator? new_op appends to ops list, the
                         * terminator is stored separately, so the Copy
                         * lands before the terminator in emission
                         * order. */
                        fn.set_operands(cp, {vals[j]});
                        cp->dst_reg = pp.clone->dst_reg;
                        cp->flags = T2_OP_INLINED;
                        cp->operand_regs =
                                fn.arena.alloc_array<int32_t>(1);
                        {
                            auto hit = fb.home.find(vals[j]);

                            cp->operand_regs[0] = hit != fb.home.end()
                                                          ? hit->second
                                                          : T2_REG_NONE;
                        }
                        fb.home[cp->result] = cp->dst_reg;
                        vals[j] = cp->result;
                    }
                    fn.set_phi_inputs(pp.clone, vals, preds);
                }

                /* Return join: single-return funs (the common case)
                 * bypass the phi — the return value flows directly and
                 * the caller may commit it straight into a loop home.
                 * Multi-return funs get one phi over per-edge copies. */
                if (rets.size() == 1) {
                    fb.ret_phi = rets[0].val;
                    *fb_out = std::move(fb);
                    return true;
                }
                {
                    T2Op *rp = fn.new_phi(fb.ret_join, T2Type::any());

                    rp->dst_reg = fresh_home(fb);
                    rp->flags = T2_OP_INLINED;
                    fb.home[rp->result] = rp->dst_reg;

                    std::vector<T2Value *> vals;
                    std::vector<T2BasicBlock *> preds;

                    for (RetEdge &re : rets) {
                        T2Op *cp = fn.new_op(re.from,
                                             T2OpKind::Copy,
                                             re.val->type);

                        fn.set_operands(cp, {re.val});
                        cp->dst_reg = rp->dst_reg;
                        cp->flags = T2_OP_INLINED;
                        cp->operand_regs =
                                fn.arena.alloc_array<int32_t>(1);
                        {
                            auto hit = fb.home.find(re.val);

                            cp->operand_regs[0] = hit != fb.home.end()
                                                          ? hit->second
                                                          : T2_REG_NONE;
                        }
                        fb.home[cp->result] = cp->dst_reg;
                        vals.push_back(cp->result);
                        preds.push_back(re.from);
                    }
                    if (vals.empty()) {
                        return false; /* no return: not a leaf shape */
                    }
                    fn.set_phi_inputs(rp, vals, preds);
                    fb.ret_phi = rp->result;
                }

                *fb_out = std::move(fb);
                return true;
            }

            /* ---- speculation conversion for the spliced body ---------- *
             * Convert every cloned Add/Sub to the flag-checked shape,
             * guarding operands that are not provably small. Values
             * provable here: small constants, previously converted
             * results, values already guarded in this body, and the
             * accumulator phi when its entry input gets a preheader
             * guard and every fun return value is provably small (the
             * induction closes over the latch input; the typed-state
             * validator re-proves all of it). Returns false when some
             * operand cannot be proven or guarded — the site is then
             * abandoned (caller discards the expansion attempt). */
            bool convert_arith(FunBody &fb,
                               T2Value *acc_phi, /* null for arity-1 funs */
                               bool *need_acc_entry_guard,
                               const void *helper_lf,
                               uint32_t call_beam_idx) {
                std::unordered_set<const T2Value *> proven;

                *need_acc_entry_guard = false;

                /* Accumulator induction: legal iff every return value
                 * is a small constant or a converted arith result. */
                bool acc_inductive = acc_phi != nullptr;

                if (acc_phi != nullptr) {
                    /* fb.ret_phi's inputs are Copies of return values;
                     * check their roots. */
                    const T2Op *rp = fb.ret_phi->def;

                    for (uint16_t i = 0; i < rp->num_operands; i++) {
                        const T2Value *root =
                                resolve_copies(rp->operands[i]);

                        bool ok = const_is_small(root) ||
                                  (root->def != nullptr &&
                                   (root->def->kind == T2OpKind::Add ||
                                    root->def->kind == T2OpKind::Sub) &&
                                   !fb.arith.empty());

                        if (!ok) {
                            acc_inductive = false;
                        }
                    }
                }

                for (T2Op *op : fb.arith) {
                    std::vector<T2Value *> guards;

                    for (uint16_t i = 0; i < 2; i++) {
                        T2Value *v = op->operands[i];

                        if (proven.count(v) != 0 ||
                            t2_type_proves_small(v->type)) {
                            continue;
                        }
                        if (is_const_def(v)) {
                            if (!const_is_small(v)) {
                                return false; /* non-small constant */
                            }
                            continue;
                        }
                        if (v->def != nullptr &&
                            v->def->kind == T2OpKind::Phi &&
                            acc_phi != nullptr && v == acc_phi &&
                            acc_inductive) {
                            /* Preheader entry guard + induction. */
                            *need_acc_entry_guard = true;
                            proven.insert(v);
                            continue;
                        }
                        guards.push_back(v);
                    }

                    if (!guards.empty()) {
                        T2Op *g = fn.new_op(op->block,
                                            T2OpKind::SpeculateType,
                                            T2Type::none());

                        fn.set_operands(g, guards);
                        g->operand_regs = fn.arena.alloc_array<int32_t>(
                                guards.size());
                        for (size_t j = 0; j < guards.size(); j++) {
                            auto hit = fb.home.find(guards[j]);

                            if (hit == fb.home.end()) {
                                return false;
                            }
                            g->operand_regs[j] = hit->second;
                            proven.insert(guards[j]);
                        }
                        g->flags = T2_OP_INLINED | T2_OP_WINDOW_CALLEE;
                        g->imm_int = (Sint64)(UWord)helper_lf;
                        g->beam_idx = call_beam_idx;

                        /* new_op appended g at the block tail; move it
                         * to sit directly before the arith op. */
                        move_before(op->block, op, g);
                    }

                    op->kind = op->kind == T2OpKind::Add
                                       ? T2OpKind::AddSmall
                                       : T2OpKind::SubSmall;
                    op->sync = nullptr;
                    op->flags |= T2_OP_INLINED | T2_OP_WINDOW_CALLEE;
                    op->imm_int = (Sint64)(UWord)helper_lf;
                    op->beam_idx = call_beam_idx;
                    proven.insert(op->result);
                }

                return true;
            }

            /* Move `op` (freshly appended at b's tail by new_op) to sit
             * directly before `before` in the same block. */
            static void move_before(T2BasicBlock *b,
                                    T2Op *before,
                                    T2Op *op) {
                ASSERT(b->ops_tail == op && op->next == nullptr);
                unlink_op(b, op);
                op->prev = before->prev;
                op->next = before;
                if (before->prev != nullptr) {
                    before->prev->next = op;
                } else {
                    b->ops_head = op;
                }
                before->prev = op;
            }

            /* ---- sync-map synthesis ----------------------------------- */

            T2SyncMap *make_map(const std::vector<T2Value *> &xs,
                                const T2SyncMap *frame_from) {
                T2SyncMap *m = fn.arena.create<T2SyncMap>();

                m->x_live = (uint32_t)xs.size();
                m->x = fn.arena.alloc_array<T2Value *>(xs.size());
                for (size_t i = 0; i < xs.size(); i++) {
                    m->x[i] = xs[i];
                }
                m->frame_size = frame_from->frame_size;
                m->y = frame_from->y;
                return m;
            }

            /* ---- per-op helpers --------------------------------------- */

            T2Op *new_rc(T2BasicBlock *b,
                         const Callee &ce,
                         Eterm f,
                         const void *lf,
                         uint32_t arity,
                         uint32_t charge,
                         T2SyncMap *map,
                         uint32_t beam_idx) {
                T2Op *rc = fn.new_op(b,
                                     T2OpKind::ReductionCheck,
                                     T2Type::none());

                fn.set_operands(rc, {});
                rc->flags = T2_OP_RC_CALLEE;
                rc->mfa_m = ce.mfa_m;
                rc->mfa_f = f;
                rc->live = arity;
                rc->imm_int = (Sint64)(UWord)lf;
                rc->index = charge - 1; /* isel: imm = 1 + index */
                rc->sync = map;
                rc->beam_idx = beam_idx;
                return rc;
            }

            void new_demote(T2BasicBlock *b,
                            const Callee &ce,
                            Eterm f,
                            const void *lf,
                            uint32_t arity,
                            T2SyncMap *map,
                            uint32_t beam_idx) {
                T2Op *dm = fn.new_op(b,
                                     T2OpKind::DemoteCallee,
                                     T2Type::none());

                fn.set_operands(dm, {});
                dm->mfa_m = ce.mfa_m;
                dm->mfa_f = f;
                dm->live = arity;
                dm->imm_int = (Sint64)(UWord)lf;
                dm->sync = map;
                dm->beam_idx = beam_idx;
            }

            T2Value *new_copy(T2BasicBlock *b,
                              T2Value *src,
                              int32_t src_home,
                              int32_t dst_home,
                              uint32_t beam_idx) {
                T2Op *cp = fn.new_op(b, T2OpKind::Copy, src->type);

                fn.set_operands(cp, {src});
                cp->dst_reg = dst_home;
                cp->operand_regs = fn.arena.alloc_array<int32_t>(1);
                cp->operand_regs[0] = src_home;
                cp->beam_idx = beam_idx;
                return cp->result;
            }

            /* A type-test + branch pair ending block `b`. */
            void new_test_branch(T2BasicBlock *b,
                                 T2OpKind test,
                                 T2Value *v,
                                 int32_t v_home,
                                 T2BasicBlock *then_b,
                                 T2BasicBlock *else_b,
                                 uint32_t beam_idx) {
                T2Op *t = fn.new_op(b, test, T2Type::of(BEAM_TYPE_ATOM));

                fn.set_operands(t, {v});
                t->operand_regs = fn.arena.alloc_array<int32_t>(1);
                t->operand_regs[0] = v_home;
                t->beam_idx = beam_idx;
                fn.emit_branch(b, t->result, then_b, else_b);
                b->terminator->beam_idx = beam_idx;
            }

            /* ---- one site --------------------------------------------- */

            void trace_reject(const T2Op *call, const char *why) {
                if (intrin_trace()) {
                    erts_fprintf(stderr,
                                 "t2_intrinsics: %T:%T/%u lists:%T/%u "
                                 "rejected: %s\n",
                                 fn.module,
                                 fn.function,
                                 (unsigned)fn.arity,
                                 call->mfa_f,
                                 (unsigned)call->index,
                                 why);
                }
            }

            bool expand_site(T2Op *call, const IntrinsicKind &k) {
                Callee ce;

                if (call->sync == nullptr ||
                    call->sync->x_live != k.call_arity) {
                    trace_reject(call, "no call-shaped sync map");
                    return true; /* leave the site alone */
                }

                /* The fun argument: an SSA-constant, env-free MakeFun
                 * of this module. */
                const T2Value *froot = resolve_copies(call->operands[0]);
                const T2Op *mf = froot->def;

                if (mf == nullptr || mf->kind != T2OpKind::MakeFun ||
                    mf->live != 0 /* num_free */ || mf->imm_int < 0) {
                    trace_reject(call, "fun arg is not an env-free "
                                       "SSA-constant MakeFun");
                    return true;
                }
                if (ret->lambdas == NULL ||
                    mf->index >= (uint32_t)ret->lambda_count) {
                    trace_reject(call, "lambda out of range");
                    return true;
                }
                {
                    const ErtsT2Lambda *lam = &ret->lambdas[mf->index];

                    if ((uint32_t)(lam->arity - lam->num_free) !=
                        k.fun_arity) {
                        trace_reject(call, "fun arity mismatch");
                        return true;
                    }
                }

                /* The impl function must be eligible for building. */
                {
                    uint32_t idx = (uint32_t)mf->imm_int;

                    if (idx >= (uint32_t)ret->function_count ||
                        (ret->eligible_bitmap[idx / 32] &
                         (((Uint32)1) << (idx % 32))) == 0) {
                        trace_reject(call, "fun impl not eligible");
                        return true;
                    }
                }

                /* Cross-tier addresses must exist for this site. */
                if (erts_t2_pc_lookup_kind(ret,
                                           fn.fn_index,
                                           call->beam_idx,
                                           ERTS_T2_PC_CALL) == 0 ||
                    erts_t2_pc_lookup_kind(ret,
                                           fn.fn_index,
                                           call->beam_idx,
                                           ERTS_T2_PC_CONT) == 0) {
                    trace_reject(call, "no CALL/CONT pctab entries");
                    return true;
                }

                if (!resolve_callee(k, &ce)) {
                    trace_reject(call, "lists wrapper/helper not "
                                       "resolvable");
                    return true;
                }

                /* Pre-admit the fun body BEFORE any rewrite: a site
                 * whose fun cannot be spliced (calls, allocation, ...)
                 * must stay a plain call_ext with the function
                 * compiling exactly as before. */
                {
                    uint32_t impl_idx = (uint32_t)mf->imm_int;
                    bool admitted = false;
                    std::string berr;

                    if (!t2_build_selected(
                                ret,
                                &impl_idx,
                                1,
                                [&](T2Function &callee) {
                                    admitted = admit_fun(
                                            callee,
                                            k.fun_arity,
                                            k.cls == IClass::All ||
                                                    k.cls == IClass::Any);
                                },
                                &berr)) {
                        return true; /* decode trouble: leave alone */
                    }
                    if (!admitted) {
                        trace_reject(call, "fun body not admissible");
                        return true;
                    }
                }

                uint32_t bi = call->beam_idx;
                T2SyncMap *cmap = call->sync;

                /* Vector values + homes at the boundary. */
                T2Value *fv = cmap->x[0];
                T2Value *a0 = k.cls == IClass::Foldl ? cmap->x[1] : nullptr;
                T2Value *l0 = k.cls == IClass::Foldl ? cmap->x[2]
                                                     : cmap->x[1];
                int32_t lx = k.cls == IClass::Foldl ? t2_xreg(2)
                                                    : t2_xreg(1);

                /* ---- blocks ------------------------------------------ */

                T2BasicBlock *b_pre = call->block;
                T2BasicBlock *b_join = fn.new_block();

                /* Split: everything after the call moves to b_join;
                 * the original terminator too. */
                {
                    T2Op *after = call->next;

                    if (after != nullptr) {
                        after->prev = nullptr;
                        b_join->ops_head = after;
                        b_join->ops_tail = b_pre->ops_tail;
                        for (T2Op *q = after; q != nullptr; q = q->next) {
                            q->block = b_join;
                        }
                        call->next = nullptr;
                        b_pre->ops_tail = call;
                    }
                    b_join->terminator = b_pre->terminator;
                    if (b_join->terminator != nullptr) {
                        b_join->terminator->block = b_join;
                    }
                    b_pre->terminator = nullptr;

                    /* Phis in former successors now come from b_join. */
                    for (T2BasicBlock *b : fn.blocks) {
                        for (T2Op *phi = b->phis_head; phi != nullptr;
                             phi = phi->next) {
                            if (phi->phi_blocks == nullptr) {
                                continue;
                            }
                            for (uint16_t i = 0; i < phi->num_operands;
                                 i++) {
                                if (phi->phi_blocks[i] == b_pre) {
                                    phi->phi_blocks[i] = b_join;
                                }
                            }
                        }
                    }

                    unlink_op(b_pre, call);
                }

                T2BasicBlock *b_preb = k.cls == IClass::Foreach
                                               ? fn.new_block()
                                               : nullptr;
                T2BasicBlock *b_chk = fn.new_block();
                T2BasicBlock *b_chk2 = fn.new_block();
                T2BasicBlock *b_grd = fn.new_block();
                T2BasicBlock *b_dm_entry = fn.new_block();
                T2BasicBlock *b_body = fn.new_block();
                T2BasicBlock *b_dm_iter = fn.new_block();

                /* ---- preheader charges ------------------------------- */

                /* RC#1: the wrapper's entry charge; state = the call's
                 * own boundary map. */
                new_rc(b_pre,
                       ce,
                       ce.wrapper_f,
                       ce.wrapper_lf,
                       k.call_arity,
                       1,
                       cmap,
                       bi);
                fn.emit_jump(b_pre,
                             b_preb != nullptr ? b_preb : b_chk);
                b_pre->terminator->beam_idx = bi;

                if (b_preb != nullptr) {
                    /* foreach: the wrapper's tail-dispatch to the
                     * helper charges before any list inspection. */
                    new_rc(b_preb,
                           ce,
                           ce.helper_f,
                           ce.helper_lf,
                           k.call_arity,
                           1,
                           cmap,
                           bi);
                    fn.emit_jump(b_preb, b_chk);
                    b_preb->terminator->beam_idx = bi;
                }

                /* ---- loop phis (created early: values needed below) -- */

                T2Op *acc_phi_op = nullptr;
                T2Op *l_phi_op;

                if (k.cls == IClass::Foldl) {
                    acc_phi_op = fn.new_phi(b_body, T2Type::any());
                    acc_phi_op->dst_reg = t2_xreg(1);
                }
                l_phi_op = fn.new_phi(b_body, T2Type::any());
                l_phi_op->dst_reg = lx;

                /* ---- fun body (spliced early so admission can still
                 *      abandon the site — nothing below is reachable
                 *      until the preheader branches are wired). ------- */

                T2Value *hv = nullptr;
                T2Value *tv = nullptr;
                FunBody fb;
                bool spliced = false;
                bool splice_hard_err = false;

                {
                    /* Only the head is extracted up front; the tail
                     * extraction sinks below the fun body (into the
                     * charge block), where it can write the list
                     * phi's own home — every deopt needing the old
                     * list value fires before it. */
                    uint32_t first_free = k.call_arity;
                    T2Op *gh = fn.new_op(b_body,
                                         T2OpKind::GetHd,
                                         T2Type::any());

                    fn.set_operands(gh, {l_phi_op->result});
                    gh->dst_reg = t2_xreg(first_free);
                    gh->operand_regs = fn.arena.alloc_array<int32_t>(1);
                    gh->operand_regs[0] = lx;
                    gh->beam_idx = bi;
                    hv = gh->result;

                    std::vector<T2Value *> args;
                    std::vector<int32_t> arg_homes;

                    if (k.fun_arity == 2) {
                        args = {hv, acc_phi_op->result};
                        arg_homes = {t2_xreg(first_free), t2_xreg(1)};
                    } else {
                        args = {hv};
                        arg_homes = {t2_xreg(first_free)};
                    }

                    uint32_t impl_idx = (uint32_t)mf->imm_int;
                    std::string berr;

                    bool built = t2_build_selected(
                            ret,
                            &impl_idx,
                            1,
                            [&](T2Function &callee) {
                                FunBody tmp;

                                if (splice_fun(callee,
                                               args,
                                               arg_homes,
                                               b_dm_iter,
                                               first_free + 1,
                                               &tmp)) {
                                    fb = std::move(tmp);
                                    spliced = true;
                                } else if (err != nullptr &&
                                           !err->empty()) {
                                    splice_hard_err = true;
                                }
                            },
                            &berr);

                    if (!built || splice_hard_err) {
                        /* Internal inconsistency mid-rewrite: the block
                         * split already happened, so degrade the whole
                         * function loudly. */
                        if (err != nullptr && err->empty()) {
                            *err = "intrinsics: fun body build failed: " +
                                   berr;
                        }
                        return false;
                    }
                    if (!spliced) {
                        /* Admission said no — but the block split is
                         * done. Fail the function to T1 (conservative;
                         * the site itself simply isn't expanded on the
                         * next profile-driven compile either). */
                        if (err != nullptr) {
                            *err = "intrinsics: fun body not admissible "
                                   "(site abandoned post-split)";
                        }
                        return false;
                    }
                }

                /* Speculation conversion for the spliced body. */
                bool need_acc_guard = false;

                if (!convert_arith(fb,
                                   acc_phi_op != nullptr
                                           ? acc_phi_op->result
                                           : nullptr,
                                   &need_acc_guard,
                                   ce.helper_lf,
                                   bi)) {
                    if (err != nullptr) {
                        *err = "intrinsics: fun arithmetic not "
                               "convertible (site abandoned post-split)";
                    }
                    return false;
                }

                /* b_body falls through into the fun entry clone. */
                fn.emit_jump(b_body, fb.entry);
                b_body->terminator->beam_idx = bi;

                /* ---- preheader checks -------------------------------- */

                T2BasicBlock *b_exit0 = fn.new_block();

                new_test_branch(b_chk,
                                T2OpKind::IsNil,
                                l0,
                                lx,
                                b_exit0,
                                b_chk2,
                                bi);
                new_test_branch(b_chk2,
                                T2OpKind::IsNonemptyList,
                                l0,
                                lx,
                                b_grd,
                                b_dm_entry,
                                bi);

                /* Entry demote: the wrapper's case_clause shape — except
                 * foreach, whose wrapper has no case (the helper raises
                 * function_clause). */
                if (k.cls == IClass::Foreach) {
                    new_demote(b_dm_entry,
                               ce,
                               ce.helper_f,
                               ce.helper_lf,
                               k.call_arity,
                               cmap,
                               bi);
                } else {
                    new_demote(b_dm_entry,
                               ce,
                               ce.wrapper_f,
                               ce.wrapper_lf,
                               k.call_arity,
                               cmap,
                               bi);
                }

                /* Entry guards (LICM-lite by construction: the
                 * accumulator's invariant type guard lives here, in the
                 * preheader, and deopts to the WRAPPER body — nothing
                 * has executed yet). */
                if (need_acc_guard) {
                    T2Op *g = fn.new_op(b_grd,
                                        T2OpKind::SpeculateType,
                                        T2Type::none());

                    fn.set_operands(g, {a0});
                    g->operand_regs = fn.arena.alloc_array<int32_t>(1);
                    g->operand_regs[0] = t2_xreg(1);
                    g->flags = T2_OP_WINDOW_CALLEE;
                    g->imm_int = (Sint64)(UWord)ce.wrapper_lf;
                    g->beam_idx = bi;
                }
                fn.emit_jump(b_grd, b_body);
                b_grd->terminator->beam_idx = bi;

                /* ---- iteration demote (fun error edges / window) ----- */

                {
                    std::vector<T2Value *> xs;

                    xs.push_back(fv);
                    if (acc_phi_op != nullptr) {
                        xs.push_back(acc_phi_op->result);
                    }
                    xs.push_back(l_phi_op->result);
                    new_demote(b_dm_iter,
                               ce,
                               ce.helper_f,
                               ce.helper_lf,
                               k.call_arity,
                               make_map(xs, cmap),
                               bi);
                }

                /* ---- latch / early-exit dispatch ---------------------- */

                T2BasicBlock *b_exit1 = fn.new_block();
                T2BasicBlock *b_lat = fn.new_block();
                T2BasicBlock *b_lat2 = fn.new_block();
                T2BasicBlock *b_dm_next = fn.new_block();
                T2BasicBlock *b_exit_early = nullptr;
                T2BasicBlock *b_cont = nullptr; /* holds copies + RC#2 */

                if (k.cls == IClass::All || k.cls == IClass::Any) {
                    /* switch rv: continue | early-exit | demote. */
                    b_exit_early = fn.new_block();
                    b_cont = fn.new_block();

                    T2Op *sw = fn.new_op(fb.ret_join,
                                         T2OpKind::Switch,
                                         T2Type::none());

                    fn.set_operands(sw, {fb.ret_phi});
                    sw->operand_regs = fn.arena.alloc_array<int32_t>(1);
                    sw->operand_regs[0] = fb.ret_phi->def->dst_reg;
                    sw->num_cases = 2;
                    sw->cases = fn.arena.alloc_array<T2SwitchCase>(2);
                    if (k.cls == IClass::All) {
                        sw->cases[0].value = am_true;
                        sw->cases[0].target = b_cont;
                        sw->cases[1].value = am_false;
                        sw->cases[1].target = b_exit_early;
                    } else {
                        sw->cases[0].value = am_true;
                        sw->cases[0].target = b_exit_early;
                        sw->cases[1].value = am_false;
                        sw->cases[1].target = b_cont;
                    }
                    sw->default_target = b_dm_iter;
                    sw->beam_idx = bi;

                    /* Early exit: the fun call's entry + return and
                     * the wrapper/helper chain's own return — what T1
                     * pays on this path. */
                    {
                        T2Op *cr = fn.new_op(b_exit_early,
                                             T2OpKind::ChargeReds,
                                             T2Type::none());

                        fn.set_operands(cr, {});
                        cr->imm_int = 3;
                        cr->beam_idx = bi;
                    }
                } else {
                    b_cont = fb.ret_join;
                }

                /* b_cont: materialize the next-iteration vector, charge,
                 * dispatch. The accumulator commits straight into its
                 * loop home when the fun's single return value is a
                 * flag-checked arith result that ends the block feeding
                 * this one (its commit already happens after its own
                 * deopt check, and nothing after it re-reads the old
                 * accumulator); otherwise one Copy. The tail extraction
                 * always lands here, writing the list phi's home
                 * directly — every deopt needing the old list fired
                 * earlier. */
                T2Value *rv_next = nullptr; /* @x1 for foldl */
                T2Value *t_next;            /* @lx            */

                if (k.cls == IClass::Foldl) {
                    T2Op *rd = fb.ret_phi->def;

                    if (rd != nullptr &&
                        (rd->kind == T2OpKind::AddSmall ||
                         rd->kind == T2OpKind::SubSmall) &&
                        rd->block->ops_tail == rd &&
                        rd->block->terminator != nullptr &&
                        rd->block->terminator->kind == T2OpKind::Jump &&
                        rd->block->terminator->succ_then == fb.ret_join) {
                        rd->dst_reg = t2_xreg(1);
                        rv_next = fb.ret_phi;
                    } else {
                        rv_next = new_copy(b_cont,
                                           fb.ret_phi,
                                           fb.ret_phi->def->dst_reg,
                                           t2_xreg(1),
                                           bi);
                    }
                }

                {
                    T2Op *gt = fn.new_op(b_cont,
                                         T2OpKind::GetTl,
                                         T2Type::any());

                    fn.set_operands(gt, {l_phi_op->result});
                    gt->dst_reg = lx;
                    gt->operand_regs = fn.arena.alloc_array<int32_t>(1);
                    gt->operand_regs[0] = lx;
                    gt->beam_idx = bi;
                    tv = gt->result;
                    t_next = tv;
                }

                T2SyncMap *next_map;
                {
                    std::vector<T2Value *> xs;

                    xs.push_back(fv);
                    if (rv_next != nullptr) {
                        xs.push_back(rv_next);
                    }
                    xs.push_back(t_next);
                    next_map = make_map(xs, cmap);
                }

                /* Per-element charge: the fun call's entry check, the
                 * fun's return dispatch, and the self tail call into
                 * the next iteration — exactly T1's three (the JIT
                 * charges returns too). */
                new_rc(b_cont,
                       ce,
                       ce.helper_f,
                       ce.helper_lf,
                       k.call_arity,
                       3,
                       next_map,
                       bi);
                fn.emit_jump(b_cont, b_lat);
                b_cont->terminator->beam_idx = bi;

                new_test_branch(b_lat,
                                T2OpKind::IsNil,
                                t_next,
                                lx,
                                b_exit1,
                                b_lat2,
                                bi);
                new_test_branch(b_lat2,
                                T2OpKind::IsNonemptyList,
                                t_next,
                                lx,
                                b_body,
                                b_dm_next,
                                bi);

                new_demote(b_dm_next,
                           ce,
                           ce.helper_f,
                           ce.helper_lf,
                           k.call_arity,
                           next_map,
                           bi);

                /* ---- loop phi inputs ---------------------------------- */

                if (acc_phi_op != nullptr) {
                    fn.set_phi_inputs(acc_phi_op,
                                      {a0, rv_next},
                                      {b_grd, b_lat2});
                }
                fn.set_phi_inputs(l_phi_op,
                                  {l0, t_next},
                                  {b_grd, b_lat2});

                /* ---- exits + result join ------------------------------ */

                std::vector<T2Value *> join_vals;
                std::vector<T2BasicBlock *> join_preds;
                int32_t res_home = call->dst_reg != T2_REG_NONE
                                           ? call->dst_reg
                                           : t2_xreg(0);

                /* Every non-demote exit pays the wrapper/helper
                 * chain's single return dispatch, as T1 does. */
                auto exit_charge = [&](T2BasicBlock *b) {
                    T2Op *cr = fn.new_op(b,
                                         T2OpKind::ChargeReds,
                                         T2Type::none());

                    fn.set_operands(cr, {});
                    cr->imm_int = 1;
                    cr->beam_idx = bi;
                };

                exit_charge(b_exit0);
                exit_charge(b_exit1);

                auto exit_const = [&](T2BasicBlock *b, Eterm atom) {
                    T2Op *c = fn.new_op(b,
                                        T2OpKind::ConstAtom,
                                        T2Type::of(BEAM_TYPE_ATOM));

                    fn.set_operands(c, {});
                    c->imm_term = atom;
                    c->beam_idx = bi;
                    return new_copy(b, c->result, T2_REG_NONE, res_home,
                                    bi);
                };

                switch (k.cls) {
                case IClass::Foldl:
                    join_vals.push_back(new_copy(b_exit0,
                                                 a0,
                                                 t2_xreg(1),
                                                 res_home,
                                                 bi));
                    join_preds.push_back(b_exit0);
                    join_vals.push_back(new_copy(b_exit1,
                                                 rv_next,
                                                 t2_xreg(1),
                                                 res_home,
                                                 bi));
                    join_preds.push_back(b_exit1);
                    break;
                case IClass::Foreach:
                    join_vals.push_back(exit_const(b_exit0, am_ok));
                    join_preds.push_back(b_exit0);
                    join_vals.push_back(exit_const(b_exit1, am_ok));
                    join_preds.push_back(b_exit1);
                    break;
                case IClass::All:
                    join_vals.push_back(exit_const(b_exit0, am_true));
                    join_preds.push_back(b_exit0);
                    join_vals.push_back(exit_const(b_exit1, am_true));
                    join_preds.push_back(b_exit1);
                    join_vals.push_back(exit_const(b_exit_early,
                                                   am_false));
                    join_preds.push_back(b_exit_early);
                    break;
                case IClass::Any:
                    join_vals.push_back(exit_const(b_exit0, am_false));
                    join_preds.push_back(b_exit0);
                    join_vals.push_back(exit_const(b_exit1, am_false));
                    join_preds.push_back(b_exit1);
                    join_vals.push_back(exit_const(b_exit_early,
                                                   am_true));
                    join_preds.push_back(b_exit_early);
                    break;
                }

                for (T2BasicBlock *eb : join_preds) {
                    fn.emit_jump(eb, b_join);
                    eb->terminator->beam_idx = bi;
                }

                {
                    T2Op *res = fn.new_phi(b_join, T2Type::any());

                    res->dst_reg = res_home;
                    res->beam_idx = bi;
                    fn.set_phi_inputs(res, join_vals, join_preds);
                    replace_value(fn, call->result, res->result);
                }

                /* Dependencies: the lists instance (baked helper/wrapper
                 * addresses) + our own instance (the inlined fun body —
                 * a breakpoint on the fun implementation must kill this
                 * blob). */
                auto add_dep = [&](const void *hdr) {
                    for (const void *d : fn.dep_hdrs) {
                        if (d == hdr) {
                            return;
                        }
                    }
                    fn.dep_hdrs.push_back(hdr);
                };
                add_dep(ce.lists_hdr);
                add_dep(own_code_hdr);

                if (intrin_trace()) {
                    erts_fprintf(stderr,
                                 "t2_intrinsics: %T:%T/%u expanded "
                                 "lists:%T/%u (beam_idx %u)\n",
                                 fn.module,
                                 fn.function,
                                 (unsigned)fn.arity,
                                 ce.wrapper_f,
                                 (unsigned)k.call_arity,
                                 (unsigned)bi);
                }

                changed = true;
                return true;
            }

            bool run() {
                if (fn.blocks.empty() || !fn.sync_complete ||
                    ret == nullptr) {
                    return true;
                }

                /* Collect sites first: the rewrite splits blocks. */
                struct Site {
                    T2Op *call;
                    const IntrinsicKind *k;
                };
                std::vector<Site> sites;
                Eterm am_lists_mod = ERTS_MAKE_AM("lists");

                for (T2BasicBlock *b : fn.blocks) {
                    for (T2Op *op = b->ops_head; op != nullptr;
                         op = op->next) {
                        if (op->kind != T2OpKind::CallExt ||
                            op->flags != 0 ||
                            op->mfa_m != am_lists_mod) {
                            continue;
                        }
                        for (const IntrinsicKind &k : t2_intrinsic_kinds) {
                            if (op->index == k.call_arity &&
                                op->mfa_f ==
                                        erts_atom_put(
                                                (const byte *)k.wrapper,
                                                sys_strlen(k.wrapper),
                                                ERTS_ATOM_ENC_LATIN1,
                                                1)) {
                                sites.push_back({op, &k});
                                break;
                            }
                        }
                    }
                }

                if (intrin_trace()) {
                    erts_fprintf(stderr,
                                 "t2_intrinsics: %T:%T/%u scan: %u "
                                 "site(s)\n",
                                 fn.module,
                                 fn.function,
                                 (unsigned)fn.arity,
                                 (unsigned)sites.size());
                }

                for (Site &s : sites) {
                    if (!expand_site(s.call, *s.k)) {
                        return false;
                    }
                }

                if (changed) {
                    fn.finalize();
                }
                return true;
            }
        };

    } /* anonymous namespace */

    bool t2_intrinsics(T2Function &fn,
                       const ErtsT2RetainedCode *ret,
                       const void *code_hdr,
                       bool *changed,
                       std::string *err) {
        *changed = false;

        if (intrin_disabled()) {
            return true;
        }

        Expander ex{fn, ret, code_hdr, err};

        if (!ex.run()) {
            return false;
        }
        *changed = ex.changed;
        return true;
    }

    /* ------------------------------------------------------------------ *
     * LICM-lite (see t2_intrinsics.hpp)                                   *
     * ------------------------------------------------------------------ */

    bool t2_licm_lite(T2Function &fn,
                      const T2LoopInfo &li,
                      bool *changed,
                      std::string *err) {
        (void)err;
        *changed = false;

        for (const T2Loop &loop : li.loops) {
            if (loop.preheader == T2_NO_LOOP_BLOCK) {
                continue;
            }

            std::unordered_set<uint32_t> in_loop(loop.body.begin(),
                                                 loop.body.end());
            T2BasicBlock *hdr = fn.blocks[loop.header];
            T2BasicBlock *pre = fn.blocks[loop.preheader];

            /* Values defined inside the loop (phis included). */
            std::unordered_set<const T2Value *> loop_defs;

            for (uint32_t bid : loop.body) {
                const T2BasicBlock *b = fn.blocks[bid];

                for (const T2Op *phi = b->phis_head; phi != nullptr;
                     phi = phi->next) {
                    loop_defs.insert(phi->result);
                }
                for (const T2Op *op = b->ops_head; op != nullptr;
                     op = op->next) {
                    if (op->result != nullptr) {
                        loop_defs.insert(op->result);
                    }
                }
            }

            /* Hoist window-shaped SpeculateType guards from the HEADER
             * block whose operands are all loop-invariant. The header
             * dominates every latch, so the guard executes on every
             * iteration today; being invariant and pure, executing it
             * once in the preheader is observably identical (its deopt
             * re-executes from the entry vector, which the preheader
             * boundary still holds — for a recovered self-loop the
             * preheader IS the entry block, so the entry-window rule
             * applies and the window validator re-checks it). Callee-
             * class guards (intrinsic loops) are placed in their
             * preheader at construction and never appear here. */
            T2Op *op = hdr->ops_head;

            while (op != nullptr) {
                T2Op *next = op->next;

                if (op->kind == T2OpKind::SpeculateType &&
                    (op->flags & T2_OP_SPEC_BOUNDARY) == 0 &&
                    (op->flags & T2_OP_WINDOW_CALLEE) == 0) {
                    bool invariant = true;

                    for (uint16_t i = 0; i < op->num_operands; i++) {
                        if (loop_defs.count(op->operands[i]) != 0) {
                            invariant = false;
                            break;
                        }
                    }
                    if (invariant) {
                        unlink_op(hdr, op);
                        /* Append to the preheader's body (before its
                         * terminator, which is stored separately). */
                        op->block = pre;
                        op->prev = pre->ops_tail;
                        op->next = nullptr;
                        if (pre->ops_tail != nullptr) {
                            pre->ops_tail->next = op;
                        } else {
                            pre->ops_head = op;
                        }
                        pre->ops_tail = op;
                        *changed = true;
                    }
                }
                op = next;
            }
        }

        return true;
    }

} /* namespace erts_t2 */
