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

        /* The maps:fold flatmap specialization (Stage 1;
         * PLAN/T2FULL/census/mapsfold_design.md) has its own lever so
         * the lists intrinsics stay unaffected by A/B runs. */
        bool maps_intrin_disabled() {
            static const bool off = getenv("T2_NO_MAPS_INTRIN") != nullptr;
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

        /* Append a detached op (unlinked, or a former terminator) to
         * the tail of b's body list. */
        void append_op(T2BasicBlock *b, T2Op *op) {
            op->block = b;
            op->prev = b->ops_tail;
            op->next = nullptr;
            if (b->ops_tail != nullptr) {
                b->ops_tail->next = op;
            } else {
                b->ops_head = op;
            }
            b->ops_tail = op;
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

        /* Replace every use of `from` (operands + sync maps) with `to`.
         * `except` skips one op — the result phi whose own input must
         * keep referencing the original value. */
        void replace_value(T2Function &fn,
                           T2Value *from,
                           T2Value *to,
                           const T2Op *except = nullptr) {
            for_each_op(fn, [&](T2Op *op) {
                if (op == except) {
                    return;
                }
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
                    for (int32_t i = 0;
                         m->frame_size != T2_NO_FRAME && i < m->frame_size;
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
                Module *lm =
                        erts_get_module(am_lists_mod, erts_active_code_ix());

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
                return out->wrapper_lf != nullptr && out->helper_lf != nullptr;
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
                if ((op->flags & (T2_OP_ERR_EXIT_OP | T2_OP_GARBAGE_DEALLOC)) !=
                    0) {
                    return false;
                }
                if (op->dst_reg != T2_REG_NONE && !t2_reg_is_x(op->dst_reg)) {
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
                            for (uint16_t j = 0; j < op->num_operands; j++) {
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
                            for (uint16_t j = 0; j < op->num_operands; j++) {
                                const T2Value *v = op->operands[j];

                                if (is_const_def(v) && !const_is_small(v)) {
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
                        if (need_bool_returns && !ret_is_bool(t->operands[0])) {
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
                            for (uint16_t j = 0; j < op->num_operands; j++) {
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
                std::unordered_map<const T2BasicBlock *, T2BasicBlock *> bmap;

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
                            cl->operand_regs = fn.arena.alloc_array<int32_t>(
                                    op->num_operands);
                            for (uint16_t j = 0; j < op->num_operands; j++) {
                                auto hit = fb.home.find(cl->operands[j]);

                                cl->operand_regs[j] = hit != fb.home.end()
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
                        T2Op *sw =
                                fn.new_op(nb, T2OpKind::Switch, T2Type::none());

                        fn.set_operands(sw, {v});
                        sw->operand_regs = fn.arena.alloc_array<int32_t>(1);
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
                            sw->cases[c].target = target_of(t->cases[c].target);
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
                        cp->operand_regs = fn.arena.alloc_array<int32_t>(1);
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
                        cp->operand_regs = fn.arena.alloc_array<int32_t>(1);
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
            /* spec_flags/spec_imm/spec_sync select the deopt CLASS of
             * the converted ops: the lists intrinsics pass
             * T2_OP_WINDOW_CALLEE with the helper L_f (re-execute the
             * iteration as a fresh helper call, no sync map); the
             * maps:fold expansion passes T2_OP_SPEC_CALLSITE with the
             * call-boundary sync map (re-execute the erased call). */
            bool convert_arith(FunBody &fb,
                               T2Value *acc_phi, /* null for arity-1 funs */
                               bool *need_acc_entry_guard,
                               uint16_t spec_flags,
                               Sint64 spec_imm,
                               T2SyncMap *spec_sync,
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
                        const T2Value *root = resolve_copies(rp->operands[i]);

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
                        g->operand_regs =
                                fn.arena.alloc_array<int32_t>(guards.size());
                        for (size_t j = 0; j < guards.size(); j++) {
                            auto hit = fb.home.find(guards[j]);

                            if (hit == fb.home.end()) {
                                return false;
                            }
                            g->operand_regs[j] = hit->second;
                            proven.insert(guards[j]);
                        }
                        g->flags = T2_OP_INLINED | spec_flags;
                        g->imm_int = spec_imm;
                        g->sync = spec_sync;
                        g->beam_idx = call_beam_idx;

                        /* new_op appended g at the block tail; move it
                         * to sit directly before the arith op. */
                        move_before(op->block, op, g);
                    }

                    op->kind = op->kind == T2OpKind::Add ? T2OpKind::AddSmall
                                                         : T2OpKind::SubSmall;
                    op->sync = spec_sync;
                    op->flags |= T2_OP_INLINED | spec_flags;
                    op->imm_int = spec_imm;
                    op->beam_idx = call_beam_idx;
                    proven.insert(op->result);
                }

                return true;
            }

            /* Move `op` (freshly appended at b's tail by new_op) to sit
             * directly before `before` in the same block. */
            static void move_before(T2BasicBlock *b, T2Op *before, T2Op *op) {
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
                T2Op *rc =
                        fn.new_op(b, T2OpKind::ReductionCheck, T2Type::none());

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
                T2Op *dm = fn.new_op(b, T2OpKind::DemoteCallee, T2Type::none());

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
                                 "t2_intrinsics: %T:%T/%u callee %T:%T/%u "
                                 "rejected: %s\n",
                                 fn.module,
                                 fn.function,
                                 (unsigned)fn.arity,
                                 call->mfa_m,
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
                    trace_reject(call,
                                 "fun arg is not an env-free "
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

                    if ((uint32_t)(lam->arity - lam->num_free) != k.fun_arity) {
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
                    trace_reject(call,
                                 "lists wrapper/helper not "
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
                T2Value *l0 = k.cls == IClass::Foldl ? cmap->x[2] : cmap->x[1];
                int32_t lx = k.cls == IClass::Foldl ? t2_xreg(2) : t2_xreg(1);

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
                            for (uint16_t i = 0; i < phi->num_operands; i++) {
                                if (phi->phi_blocks[i] == b_pre) {
                                    phi->phi_blocks[i] = b_join;
                                }
                            }
                        }
                    }

                    unlink_op(b_pre, call);
                }

                T2BasicBlock *b_preb =
                        k.cls == IClass::Foreach ? fn.new_block() : nullptr;
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
                fn.emit_jump(b_pre, b_preb != nullptr ? b_preb : b_chk);
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
                    T2Op *gh =
                            fn.new_op(b_body, T2OpKind::GetHd, T2Type::any());

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
                                } else if (err != nullptr && !err->empty()) {
                                    splice_hard_err = true;
                                }
                            },
                            &berr);

                    if (!built || splice_hard_err) {
                        /* Internal inconsistency mid-rewrite: the block
                         * split already happened, so degrade the whole
                         * function loudly. */
                        if (err != nullptr && err->empty()) {
                            *err = "intrinsics: fun body build failed: " + berr;
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
                                   acc_phi_op != nullptr ? acc_phi_op->result
                                                         : nullptr,
                                   &need_acc_guard,
                                   T2_OP_WINDOW_CALLEE,
                                   (Sint64)(UWord)ce.helper_lf,
                                   nullptr,
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
                    T2Op *gt =
                            fn.new_op(b_cont, T2OpKind::GetTl, T2Type::any());

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
                fn.set_phi_inputs(l_phi_op, {l0, t_next}, {b_grd, b_lat2});

                /* ---- exits + result join ------------------------------ */

                std::vector<T2Value *> join_vals;
                std::vector<T2BasicBlock *> join_preds;
                int32_t res_home = call->dst_reg != T2_REG_NONE ? call->dst_reg
                                                                : t2_xreg(0);

                /* Every non-demote exit pays the wrapper/helper
                 * chain's single return dispatch, as T1 does. */
                auto exit_charge = [&](T2BasicBlock *b) {
                    T2Op *cr =
                            fn.new_op(b, T2OpKind::ChargeReds, T2Type::none());

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
                    return new_copy(b, c->result, T2_REG_NONE, res_home, bi);
                };

                switch (k.cls) {
                case IClass::Foldl:
                    join_vals.push_back(
                            new_copy(b_exit0, a0, t2_xreg(1), res_home, bi));
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
                    join_vals.push_back(exit_const(b_exit_early, am_false));
                    join_preds.push_back(b_exit_early);
                    break;
                case IClass::Any:
                    join_vals.push_back(exit_const(b_exit0, am_false));
                    join_preds.push_back(b_exit0);
                    join_vals.push_back(exit_const(b_exit1, am_false));
                    join_preds.push_back(b_exit1);
                    join_vals.push_back(exit_const(b_exit_early, am_true));
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

            /* ---- maps:fold flatmap specialization (Stage 1) ----------- *
             *
             * Expansion template (design §2.4). The slow edge is the
             * ORIGINAL call moved to b_slow — non-flatmap is the
             * general case, not an error — and every fast-path deopt
             * re-executes that call via its own T1 PC
             * (ERTS_T2_PC_CALL, class T2_OP_SPEC_CALLSITE) from the
             * call-boundary sync map, which the fast path provably
             * leaves intact (effect-free, alloc-free, no write below
             * cmap->x_live nor to any Y slot). The fun's error edges
             * jump to b_slow directly: re-running the whole fold on T1
             * raises the byte-identical exception (the fast path had
             * no observable effects).
             *
             *   b_pre:  ...orig ops, call removed
             *           IsFlatmapBounded(Map) -> b_fast | b_slow
             *   b_slow: <the original CallExt/TailCallExt, orig cmap>
             *           [jump b_join   (non-tail only)]
             *   b_fast: n = FlatmapSize(Map)          @nx (invariant)
             *           FoldBudget(n)  [uncharged deopt -> CALL PC]
             *           [SpeculateType(A0) if induction applies]
             *           acc0 = Copy A0                @x3
             *           i0   = 0                      @x4
             *           jump b_head
             *   b_head: acc = phi(acc0, acc')         @x3
             *           i   = phi(i0, i')             @x4
             *           CmpLt(i, n) -> b_body | b_exit
             *   b_body: k = FlatmapKeyAt(Map, i)      @x5
             *           v = FlatmapValAt(Map, i)      @x6
             *           <spliced fun body, args (k, v, acc), fresh
             *            homes from x7; converted arith + guards are
             *            callsite-class; error edges -> b_slow>
             *   b_ret:  acc' = ret (committed @x3)
             *           i'   = AddSmall(i, 1)         @x4
             *           jump b_head
             *   b_exit: res_f = Copy acc              @res_home
             *           Return res_f       (tail)  |  jump b_join
             *   b_join: res = phi(res_s, res_f)       @res_home
             *           ...orig post-call ops         (non-tail only)
             *
             * Reductions: T1's whole fold->iterator->next chain costs
             * a + b*n (measured; see the constants below), charged as
             * ONE FoldBudget batch — legal because the fast loop is
             * bounded by MAP_SMALL_MAP_LIMIT and cannot yield, exactly
             * like T1's map_next materializing all n elements in one
             * non-yielding BIF. A deopt AFTER the budget was paid
             * re-charges on the T1 side (accepted, documented
             * deviation — reduction accounting only, term results are
             * unaffected; the fast path re-executes from the original
             * arguments, so the partial fold is simply discarded). */

            /* T1 charge model for maps:fold/3 over a flatmap of size n
             * (measured via process_info(reductions) deltas around a
             * tail-wrapped call = 10 + 5n; asserted by the
             * reduction-identity test). Decomposed: the wrapper's own
             * entry charge (1) — which the T2 entry stub also pays —
             * leaves the erased call's CHAIN cost at 9 + 5n (callee
             * entry through the chain's final return). A body site's
             * budget is exactly that chain cost; a tail site charges
             * one less, because the blob's own dispatch_return (1)
             * replaces the chain's final return. Verified empirically
             * for both shapes (mapsfold_cal): T1 tail 10+5n / body
             * 11+5n == T2 with these constants. */
            static constexpr Sint64 T2_MAPS_FOLD_R_CHAIN_CONST = 9;
            static constexpr Sint64 T2_MAPS_FOLD_R_PER_ELEM = 5;

            bool expand_maps_fold_site(T2Op *call) {
                bool is_tail = call->kind == T2OpKind::TailCallExt;

                if (call->sync == nullptr || call->sync->x_live != 3) {
                    trace_reject(call, "no call-shaped sync map");
                    return true; /* leave the site alone */
                }

                /* The fun argument: an SSA-constant, env-free MakeFun
                 * of this module (same admission as the lists
                 * intrinsics). */
                const T2Value *froot = resolve_copies(call->operands[0]);
                const T2Op *mf = froot->def;

                if (mf == nullptr || mf->kind != T2OpKind::MakeFun ||
                    mf->live != 0 /* num_free */ || mf->imm_int < 0) {
                    trace_reject(call,
                                 "fun arg is not an env-free "
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

                    if ((uint32_t)(lam->arity - lam->num_free) != 3) {
                        trace_reject(call, "fun arity mismatch");
                        return true;
                    }
                }
                {
                    uint32_t idx = (uint32_t)mf->imm_int;

                    if (idx >= (uint32_t)ret->function_count ||
                        (ret->eligible_bitmap[idx / 32] &
                         (((Uint32)1) << (idx % 32))) == 0) {
                        trace_reject(call, "fun impl not eligible");
                        return true;
                    }
                }

                /* Cross-tier addresses. Every deopt branches to the
                 * site's own T1 CALL PC; a non-tail slow edge also
                 * needs the CONT (isel requires it to lower the
                 * retained CallExt). Tail sites record CALL only. */
                if (erts_t2_pc_lookup_kind(ret,
                                           fn.fn_index,
                                           call->beam_idx,
                                           ERTS_T2_PC_CALL) == 0) {
                    trace_reject(call, "no CALL pctab entry");
                    return true;
                }
                if (!is_tail && erts_t2_pc_lookup_kind(ret,
                                                       fn.fn_index,
                                                       call->beam_idx,
                                                       ERTS_T2_PC_CONT) == 0) {
                    trace_reject(call, "no CONT pctab entry");
                    return true;
                }

                /* A fused call_ext_last decodes as Deallocate +
                 * TailCallExt, but its T1 CALL PC names the FUSED
                 * instruction (which deallocates again) — deopting
                 * there after our Deallocate ran would pop the frame
                 * twice. Only the frameless call_ext_only shape is
                 * admitted for tail sites. */
                if (is_tail) {
                    const T2Op *last = call->block->ops_tail;

                    if (call->sync->frame_size != T2_NO_FRAME ||
                        (last != nullptr &&
                         last->kind == T2OpKind::Deallocate &&
                         last->beam_idx == call->beam_idx)) {
                        trace_reject(call,
                                     "call_ext_last shape (fused "
                                     "dealloc) unsupported");
                        return true;
                    }
                }

                /* Pre-admit the fun body BEFORE any rewrite. */
                {
                    uint32_t impl_idx = (uint32_t)mf->imm_int;
                    bool admitted = false;
                    std::string berr;

                    if (!t2_build_selected(
                                ret,
                                &impl_idx,
                                1,
                                [&](T2Function &callee) {
                                    admitted = admit_fun(callee, 3, false);
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
                T2Value *a0 = cmap->x[1];
                T2Value *mapv = cmap->x[2];

                /* ---- CFG surgery -------------------------------------- */

                T2BasicBlock *b_pre = call->block;
                T2BasicBlock *b_join = nullptr;

                if (!is_tail) {
                    b_join = fn.new_block();

                    /* Split: everything after the call moves to b_join,
                     * the original terminator too (mirrors the lists
                     * expansion's split). */
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

                    for (T2BasicBlock *b : fn.blocks) {
                        for (T2Op *phi = b->phis_head; phi != nullptr;
                             phi = phi->next) {
                            if (phi->phi_blocks == nullptr) {
                                continue;
                            }
                            for (uint16_t i = 0; i < phi->num_operands; i++) {
                                if (phi->phi_blocks[i] == b_pre) {
                                    phi->phi_blocks[i] = b_join;
                                }
                            }
                        }
                    }

                    unlink_op(b_pre, call);
                } else {
                    /* The call IS the terminator; no successors, no
                     * post-call ops. */
                    ASSERT(b_pre->terminator == call);
                    b_pre->terminator = nullptr;
                }

                T2BasicBlock *b_slow = fn.new_block();
                T2BasicBlock *b_fast = fn.new_block();
                T2BasicBlock *b_head = fn.new_block();
                T2BasicBlock *b_body = fn.new_block();
                T2BasicBlock *b_exit = fn.new_block();

                /* The original call, verbatim, on the slow edge. */
                if (is_tail) {
                    call->block = b_slow;
                    b_slow->terminator = call;
                } else {
                    append_op(b_slow, call);
                    fn.emit_jump(b_slow, b_join);
                    b_slow->terminator->beam_idx = bi;
                }

                /* b_pre: the shape guard dispatching fast | slow. */
                {
                    T2Op *g = fn.new_op(b_pre,
                                        T2OpKind::IsFlatmapBounded,
                                        T2Type::of(BEAM_TYPE_ATOM));

                    fn.set_operands(g, {mapv});
                    g->operand_regs = fn.arena.alloc_array<int32_t>(1);
                    g->operand_regs[0] = t2_xreg(2);
                    g->beam_idx = bi;
                    fn.emit_branch(b_pre, g->result, b_fast, b_slow);
                    b_pre->terminator->beam_idx = bi;
                }

                /* b_fast: the invariant size + the whole-fold budget.
                 * (The loop-entry copies land below, after the splice
                 * fixes the free-X frontier.) */
                T2Op *size_op;
                T2Op *budget;

                {
                    size_op = fn.new_op(b_fast,
                                        T2OpKind::FlatmapSize,
                                        T2Type::any());
                    fn.set_operands(size_op, {mapv});
                    size_op->operand_regs = fn.arena.alloc_array<int32_t>(1);
                    size_op->operand_regs[0] = t2_xreg(2);
                    size_op->beam_idx = bi;

                    budget = fn.new_op(b_fast,
                                       T2OpKind::FoldBudget,
                                       T2Type::none());
                    fn.set_operands(budget, {size_op->result});
                    budget->operand_regs = fn.arena.alloc_array<int32_t>(1);
                    budget->sync = cmap;
                    budget->flags = T2_OP_SPEC_CALLSITE;
                    budget->imm_int =
                            T2_MAPS_FOLD_R_CHAIN_CONST - (is_tail ? 1 : 0);
                    budget->index = (uint32_t)T2_MAPS_FOLD_R_PER_ELEM;
                    budget->beam_idx = bi;
                }

                /* ---- loop phis (values needed by the fun splice) ------ */

                T2Op *acc_phi = fn.new_phi(b_head, T2Type::any());
                T2Op *i_phi = fn.new_phi(b_head, T2Type::any());

                acc_phi->dst_reg = t2_xreg(3);
                acc_phi->beam_idx = bi;
                i_phi->dst_reg = t2_xreg(4);
                i_phi->beam_idx = bi;

                /* b_head: bound check (i and n are always smalls; the
                 * generic emit_is_lt takes its small-small fast path). */
                T2Op *cmp;

                {
                    cmp = fn.new_op(b_head,
                                    T2OpKind::CmpLt,
                                    T2Type::of(BEAM_TYPE_ATOM));
                    fn.set_operands(cmp, {i_phi->result, size_op->result});
                    cmp->operand_regs = fn.arena.alloc_array<int32_t>(2);
                    cmp->operand_regs[0] = t2_xreg(4);
                    cmp->beam_idx = bi;
                    fn.emit_branch(b_head, cmp->result, b_body, b_exit);
                    b_head->terminator->beam_idx = bi;
                }

                /* b_body: element loads + the spliced fun body. */
                T2Value *kv;
                T2Value *vv;

                {
                    T2Op *k_op = fn.new_op(b_body,
                                           T2OpKind::FlatmapKeyAt,
                                           T2Type::any());

                    fn.set_operands(k_op, {mapv, i_phi->result});
                    k_op->operand_regs = fn.arena.alloc_array<int32_t>(2);
                    k_op->operand_regs[0] = t2_xreg(2);
                    k_op->operand_regs[1] = t2_xreg(4);
                    k_op->dst_reg = t2_xreg(5);
                    k_op->beam_idx = bi;
                    kv = k_op->result;

                    T2Op *v_op = fn.new_op(b_body,
                                           T2OpKind::FlatmapValAt,
                                           T2Type::any());

                    fn.set_operands(v_op, {mapv, i_phi->result});
                    v_op->operand_regs = fn.arena.alloc_array<int32_t>(2);
                    v_op->operand_regs[0] = t2_xreg(2);
                    v_op->operand_regs[1] = t2_xreg(4);
                    v_op->dst_reg = t2_xreg(6);
                    v_op->beam_idx = bi;
                    vv = v_op->result;
                }

                FunBody fb;
                bool spliced = false;
                bool splice_hard_err = false;

                {
                    std::vector<T2Value *> args = {kv, vv, acc_phi->result};
                    std::vector<int32_t> arg_homes = {t2_xreg(5),
                                                      t2_xreg(6),
                                                      t2_xreg(3)};
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
                                               b_slow, /* error edges */
                                               7,
                                               &tmp)) {
                                    fb = std::move(tmp);
                                    spliced = true;
                                } else if (err != nullptr && !err->empty()) {
                                    splice_hard_err = true;
                                }
                            },
                            &berr);

                    if (!built || splice_hard_err) {
                        if (err != nullptr && err->empty()) {
                            *err = "maps intrinsic: fun body build "
                                   "failed: " +
                                   berr;
                        }
                        return false;
                    }
                    if (!spliced) {
                        if (err != nullptr) {
                            *err = "maps intrinsic: fun body not "
                                   "admissible (site abandoned "
                                   "post-split)";
                        }
                        return false;
                    }
                }

                /* Speculation conversion: callsite class — every guard
                 * and flag-checked arith deopts by re-executing the
                 * erased call from cmap. */
                bool need_acc_guard = false;

                if (!convert_arith(fb,
                                   acc_phi->result,
                                   &need_acc_guard,
                                   T2_OP_SPEC_CALLSITE,
                                   0,
                                   cmap,
                                   bi)) {
                    if (err != nullptr) {
                        *err = "maps intrinsic: fun arithmetic not "
                               "convertible (site abandoned post-split)";
                    }
                    return false;
                }

                fn.emit_jump(b_body, fb.entry);
                b_body->terminator->beam_idx = bi;

                /* The invariant size's home: the first X slot past the
                 * spliced body's frontier — written once in b_fast,
                 * never touched by the loop. */
                int32_t nx = t2_xreg(fb.next_x);

                size_op->dst_reg = nx;
                budget->operand_regs[0] = nx;
                cmp->operand_regs[1] = nx;

                /* b_fast tail: optional accumulator entry guard, then
                 * the loop-entry copies. */
                if (need_acc_guard) {
                    T2Op *g = fn.new_op(b_fast,
                                        T2OpKind::SpeculateType,
                                        T2Type::none());

                    fn.set_operands(g, {a0});
                    g->operand_regs = fn.arena.alloc_array<int32_t>(1);
                    g->operand_regs[0] = t2_xreg(1);
                    g->flags = T2_OP_SPEC_CALLSITE;
                    g->sync = cmap;
                    g->beam_idx = bi;
                }

                T2Value *acc_in =
                        new_copy(b_fast, a0, t2_xreg(1), t2_xreg(3), bi);
                T2Value *i0 = fn.emit_const_int(b_fast, 0);

                i0->def->dst_reg = t2_xreg(4);
                i0->def->beam_idx = bi;

                T2Value *one = fn.emit_const_int(b_fast, 1);

                one->def->beam_idx = bi;

                fn.emit_jump(b_fast, b_head);
                b_fast->terminator->beam_idx = bi;

                /* b_ret (= fb.ret_join): commit acc', bump i', loop.
                 * The accumulator commits straight into its loop home
                 * when the fun's single return value is a flag-checked
                 * arith result ending the block that feeds ret_join
                 * (same direct-commit rule as the lists expansion);
                 * otherwise one Copy. */
                T2Value *acc_next;

                {
                    T2Op *rd = fb.ret_phi->def;

                    if (rd != nullptr &&
                        (rd->kind == T2OpKind::AddSmall ||
                         rd->kind == T2OpKind::SubSmall) &&
                        rd->block->ops_tail == rd &&
                        rd->block->terminator != nullptr &&
                        rd->block->terminator->kind == T2OpKind::Jump &&
                        rd->block->terminator->succ_then == fb.ret_join) {
                        rd->dst_reg = t2_xreg(3);
                        acc_next = fb.ret_phi;
                    } else {
                        auto hit = fb.home.find(fb.ret_phi);
                        int32_t src_home = hit != fb.home.end()
                                                   ? hit->second
                                                   : fb.ret_phi->def->dst_reg;

                        acc_next = new_copy(fb.ret_join,
                                            fb.ret_phi,
                                            src_home,
                                            t2_xreg(3),
                                            bi);
                    }
                }

                T2Value *i_next;

                {
                    T2Op *inc = fn.new_op(fb.ret_join,
                                          T2OpKind::AddSmall,
                                          T2Type::any());

                    fn.set_operands(inc, {i_phi->result, one});
                    inc->operand_regs = fn.arena.alloc_array<int32_t>(2);
                    inc->operand_regs[0] = t2_xreg(4);
                    inc->operand_regs[1] = T2_REG_NONE;
                    inc->dst_reg = t2_xreg(4);
                    inc->flags = T2_OP_SPEC_CALLSITE;
                    inc->sync = cmap;
                    inc->beam_idx = bi;
                    i_next = inc->result;
                }
                fn.emit_jump(fb.ret_join, b_head);
                fb.ret_join->terminator->beam_idx = bi;

                fn.set_phi_inputs(acc_phi,
                                  {acc_in, acc_next},
                                  {b_fast, fb.ret_join});
                fn.set_phi_inputs(i_phi, {i0, i_next}, {b_fast, fb.ret_join});

                /* b_exit: the fast result, joined with the slow edge. */
                int32_t res_home = !is_tail && call->dst_reg != T2_REG_NONE
                                           ? call->dst_reg
                                           : t2_xreg(0);
                T2Value *res_f = new_copy(b_exit,
                                          acc_phi->result,
                                          t2_xreg(3),
                                          res_home,
                                          bi);

                if (is_tail) {
                    T2Op *ret_op =
                            fn.new_op(b_exit, T2OpKind::Return, T2Type::none());

                    fn.set_operands(ret_op, {res_f});
                    ret_op->operand_regs = fn.arena.alloc_array<int32_t>(1);
                    ret_op->operand_regs[0] = res_home;
                    ret_op->sync = make_map({res_f}, cmap);
                    ret_op->beam_idx = bi;
                } else {
                    fn.emit_jump(b_exit, b_join);
                    b_exit->terminator->beam_idx = bi;

                    T2Op *res = fn.new_phi(b_join, T2Type::any());

                    res->dst_reg = res_home;
                    res->beam_idx = bi;
                    fn.set_phi_inputs(res,
                                      {call->result, res_f},
                                      {b_slow, b_exit});
                    replace_value(fn, call->result, res->result, res);
                }

                /* Only the own-module dep (the inlined fun body): the
                 * slow edge dispatches through the maps export at
                 * runtime, so nothing of the maps module is baked in. */
                {
                    bool have = false;

                    for (const void *d : fn.dep_hdrs) {
                        have |= d == own_code_hdr;
                    }
                    if (!have) {
                        fn.dep_hdrs.push_back(own_code_hdr);
                    }
                }

                if (intrin_trace()) {
                    erts_fprintf(stderr,
                                 "t2_intrinsics: %T:%T/%u expanded "
                                 "maps:fold/3 (%s site, beam_idx %u)\n",
                                 fn.module,
                                 fn.function,
                                 (unsigned)fn.arity,
                                 is_tail ? "tail" : "body",
                                 (unsigned)bi);
                }

                changed = true;
                return true;
            }

            bool run() {
                if (fn.blocks.empty() || !fn.sync_complete || ret == nullptr) {
                    return true;
                }

                /* Collect sites first: the rewrite splits blocks. */
                struct Site {
                    T2Op *call;
                    const IntrinsicKind *k;
                };
                std::vector<Site> sites;
                std::vector<T2Op *> maps_sites;
                Eterm am_lists_mod = ERTS_MAKE_AM("lists");
                Eterm am_maps_mod = ERTS_MAKE_AM("maps");
                Eterm am_fold_fn = ERTS_MAKE_AM("fold");

                for (T2BasicBlock *b : fn.blocks) {
                    for (T2Op *op = b->ops_head; op != nullptr; op = op->next) {
                        if (op->kind != T2OpKind::CallExt || op->flags != 0) {
                            continue;
                        }
                        if (op->mfa_m == am_maps_mod &&
                            op->mfa_f == am_fold_fn && op->index == 3 &&
                            !maps_intrin_disabled()) {
                            maps_sites.push_back(op);
                            continue;
                        }
                        if (op->mfa_m != am_lists_mod) {
                            continue;
                        }
                        for (const IntrinsicKind &k : t2_intrinsic_kinds) {
                            if (op->index == k.call_arity &&
                                op->mfa_f ==
                                        erts_atom_put((const byte *)k.wrapper,
                                                      sys_strlen(k.wrapper),
                                                      ERTS_ATOM_ENC_LATIN1,
                                                      1)) {
                                sites.push_back({op, &k});
                                break;
                            }
                        }
                    }

                    /* maps:fold tail sites (call_ext_only): the call is
                     * the block terminator. */
                    T2Op *t = b->terminator;

                    if (t != nullptr && t->kind == T2OpKind::TailCallExt &&
                        t->flags == 0 && t->mfa_m == am_maps_mod &&
                        t->mfa_f == am_fold_fn && t->index == 3 &&
                        !maps_intrin_disabled()) {
                        maps_sites.push_back(t);
                    }
                }

                if (intrin_trace()) {
                    erts_fprintf(stderr,
                                 "t2_intrinsics: %T:%T/%u scan: %u lists "
                                 "site(s), %u maps site(s)\n",
                                 fn.module,
                                 fn.function,
                                 (unsigned)fn.arity,
                                 (unsigned)sites.size(),
                                 (unsigned)maps_sites.size());
                }

                for (Site &s : sites) {
                    if (!expand_site(s.call, *s.k)) {
                        return false;
                    }
                }

                for (T2Op *site : maps_sites) {
                    if (!expand_maps_fold_site(site)) {
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
                    (op->flags & T2_OP_WINDOW_CALLEE) == 0 &&
                    (op->flags & T2_OP_SPEC_CALLSITE) == 0) {
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
