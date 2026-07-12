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

        /* P1a general caller-driven call-site specialization
         * (PLAN/T2FULL/census/p1_design.md): its own lever + trace so
         * the hand-coded expanders stay unaffected by A/B runs. */
        bool p1_disabled() {
            static const bool off = getenv("T2_NO_P1") != nullptr;
            return off;
        }

        bool p1_trace() {
            static const bool on = getenv("T2_P1_TRACE") != nullptr;
            return on;
        }

        /* P1c-2 (tolerant wrapper classification): its own kill switch
         * on top of T2_NO_P1, so P1a/P1c-1 A/B runs stay unaffected.
         * Gates BOTH the tolerant-build retry of a NotEligible chain
         * callee AND the widened error-leaf shapes (Opaque, noreturn
         * erlang:error tail calls) in p1_is_err_block. */
        bool p1c2_disabled() {
            static const bool off = getenv("T2_NO_P1C2") != nullptr;
            return off;
        }

        /* P1 inner re-dispatch (the mid-list deopt fidelity fix): the
         * structural/type side exits of an inlined loop land in the
         * TERMINAL loop function's body (foldl_1-equivalent) instead of
         * re-invoking the outermost original call, so a mid-list
         * malformed input raises the byte-identical exception and the
         * deopt re-charges no wrapper entries. Its own kill switch on
         * top of T2_NO_P1 reverts to the outermost re-dispatch for A/B
         * runs. */
        bool p1_inner_disabled() {
            static const bool off = getenv("T2_NO_P1_INNER") != nullptr;
            return off;
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

            /* ========================================================= *
             * P1a: general caller-driven call-site specialization       *
             * (PLAN/T2FULL/census/p1_design.md).                        *
             *                                                           *
             * At a hot tail call to a cross-module M:F/A where (a) some *
             * argument resolves to a literal env-free MakeFun of this   *
             * module and (b) M:F/A is a self-recursive loop recoverable *
             * by t2_loop_recover, the callee's recovered loop is CLONED *
             * into the caller, its params bound to the call's boundary  *
             * vector, the per-element CallFun devirtualized by splicing *
             * the fun body, and the callee's frame dropped. Deopt is    *
             * the new RE-DISPATCH shape (T2_OP_SPEC_REDISPATCH /        *
             * T2_OP_TAIL_SITE): every side exit re-invokes the generic  *
             * callee with the LOOP-CARRIED vector. Under the identity   *
             * permutation the mid-list exits land INSIDE the TERMINAL   *
             * loop function — guards enter its T1 body past the entry   *
             * check (the back edge pre-charged it) and the error edges  *
             * demote into it (DemoteCallee at a tail site) — exactly    *
             * where generic execution would be, with T1-exact           *
             * reductions and error shapes; a first-element structural   *
             * miss is pre-screened at the boundary and re-executes the  *
             * OUTERMOST original call (a genuine TailCallExt), whose    *
             * wrapper chain owns that error's shape (T2_NO_P1_INNER     *
             * reverts every exit to the outermost form: a branch to     *
             * the call site's own T1 CALL PC over the current           *
             * X0..ar-1). The RC_CALLEE back edge's yield stays in T2    *
             * and its tombstone/parked translation re-enters the        *
             * callee (timeslice never deopts). No shape guard and no    *
             * slow edge: the callee's own case dispatch is the loop     *
             * entry, and re-dispatch IS the generic fallback.           *
             * ========================================================= */

            static constexpr uint32_t T2_P1_MAX_CALLEE_OPS = 48;
            static constexpr uint32_t T2_P1_MAX_ARITY = 4;

            /* ===================================================== *
             * P1c-1: transitive wrapper inlining                    *
             * (PLAN/T2FULL/census/p1c_design.md).                   *
             *                                                       *
             * The idiomatic folds are wrappers, not loops:          *
             * lists:foldl/3 guards is_function(F,2), peels the      *
             * first element and tail-calls the self-recursive       *
             * foldl_1/3. The P1 trigger therefore descends          *
             * transitively: a callee that is not itself a           *
             * recoverable loop is classified as a THIN WRAPPER —    *
             * small, non-loop, with a unique fun-passing tail call  *
             * behind admissible guards — and the trigger re-runs on *
             * the exposed target with the argument permutation      *
             * composed (bounded depth, visited set).                *
             *                                                       *
             * The wrapper body is never cloned. Its guards are      *
             * either statically discharged (is_function on the      *
             * literal fun) or subsumed by the loop's own cons/nil   *
             * dispatch; its peel is exactly one loop iteration and  *
             * its nil path exactly the loop's nil exit — both       *
             * checked structurally against the terminal loop — so   *
             * wrapper(v) == loop(perm(v)) on every input the        *
             * inlined fast path handles, and every input it does    *
             * NOT handle re-dispatches from an unconsumed state.    *
             * The terminal loop is bound directly to the ORIGINAL   *
             * call's boundary vector through the composed           *
             * permutation. Mid-list deopts land inside the TERMINAL *
             * loop function (the inner re-dispatch, identity        *
             * permutation only — see the P1a block above); the      *
             * entry pre-screen, yield and tombstone target the      *
             * OUTERMOST original M:F/A — the only exported member   *
             * of the chain, and a correct continuation from any     *
             * loop state by the congruence above. The loop entry RC *
             * charges the wrapper chain's fixed entry reductions:   *
             * +1 per NON-peeling level (a peeling wrapper's fun     *
             * call + loop handoff costs exactly one loop            *
             * iteration's charge, which the extra inlined iteration *
             * already pays).                                        *
             *                                                       *
             * Under a non-identity permutation the clone relabels   *
             * every x home below the arity through the permutation, *
             * so the loop-carried vector is PHYSICALLY resident in  *
             * the outer argument positions — the P1 backend's sync  *
             * maps are pin sets over identity placement, never move *
             * scripts (t2_regalloc.cpp).                            *
             * ===================================================== */

            static constexpr uint32_t T2_P1_MAX_DEPTH = 3;
            static constexpr uint32_t T2_P1_MAX_WRAPPER_OPS = 32;

            /* One classified wrapper level: the fun-passing tail
             * call's target, the argument source mapping, and the
             * congruence constraints the level imposes on the
             * terminal loop. All positions are in the NEXT level's
             * argument space. */
            struct P1Wrap {
                Eterm m = NIL, f = NIL;
                uint32_t arity = 0;
                uint32_t src[T2_P1_MAX_ARITY] = {0}; /* arg j <- param */
                uint32_t fun_pos = 0;
                bool peel = false;
                int32_t peel_acc = -1;    /* arg = the CallFun result */
                int32_t peel_list = -1;   /* arg = GetTl(list param)  */
                int32_t peel_hd_op = -1;  /* CallFun op: GetHd(list)  */
                int32_t peel_acc_op = -1; /* CallFun op: the acc      */
                bool nil_ret = false;
                int32_t nil_cond = -1;    /* the IsNil'd (list) param */
                int32_t nil_ret_pos = -1; /* the returned (acc) param */
                uint32_t list_tested = 0; /* spine list-guard mask    */
                bool has_leaves = false;
                uint32_t leaf_mask = 0; /* bit p: every err/opaque
                                         * leaf excludes cons AND nil
                                         * on param p                 */
            };

            /* The chain's accumulated constraints, remapped into the
             * current (deepest) level's argument space at each
             * descent step. */
            struct P1Pending {
                bool peel = false;
                int32_t peel_acc = -1, peel_list = -1;
                int32_t peel_hd_op = -1, peel_acc_op = -1;
                bool nil_ret = false;
                int32_t nil_cond = -1, nil_ret_pos = -1;
                uint32_t list_tested = 0;
                bool has_leaves = false;
                uint32_t leaf_mask = 0;
            };

            /* Resolve through Copies AND trivial single-input phis
             * (Braun leftovers in once-unsealed blocks). The wrapper
             * is classified, never cloned, so seeing through its
             * phis is safe. */
            static const T2Value *resolve_thru(const T2Value *v) {
                for (int depth = 0; depth < 64; depth++) {
                    const T2Op *d = v->def;

                    if (d != nullptr &&
                        (d->kind == T2OpKind::Copy ||
                         (d->kind == T2OpKind::Phi && d->num_operands == 1))) {
                        v = d->operands[0];
                        continue;
                    }
                    break;
                }
                return v;
            }

            static int32_t p1_param_index(
                    const std::vector<const T2Value *> &params,
                    const T2Value *v) {
                for (size_t i = 0; i < params.size(); i++) {
                    if (params[i] == v) {
                        return (int32_t)i;
                    }
                }
                return -1;
            }

            /* next-space mask from a current-space mask: next bit j
             * holds iff current bit src[j] does. */
            static uint32_t p1_remap_mask(uint32_t mask,
                                          const uint32_t *src,
                                          uint32_t ar) {
                uint32_t out = 0;

                for (uint32_t j = 0; j < ar; j++) {
                    if ((mask & (1u << src[j])) != 0) {
                        out |= 1u << j;
                    }
                }
                return out;
            }

            /* A guard condition on a wrapper CFG path: a list-shape
             * test on one of the wrapper's own parameters. */
            struct P1Cond {
                uint32_t param;
                T2OpKind kind;
                bool pol;
            };

            struct P1WalkState {
                const T2Function &callee;
                std::vector<const T2Value *> params;
                uint32_t fun_idx;
                uint32_t fun_arity;
                const char **why;

                const T2Op *callfun = nullptr;
                const T2Op *tailcall = nullptr;
                bool tc_crossed_peel = false;
                uint32_t tc_list_tested = 0;
                bool nil_ret = false;
                uint32_t nil_cond = 0, nil_ret_param = 0;
                bool has_leaves = false;
                uint32_t leaf_mask = ~0u;
                std::unordered_set<const T2BasicBlock *> counted;
                uint32_t region_ops = 0;
                uint32_t steps = 0;
            };

            /* An err/opaque leaf is admissible iff the terminal
             * loop's list parameter can neither be a cons nor nil
             * under the leaf's path conditions — such inputs never
             * take the inlined fast path (the loop's own dispatch
             * re-dispatches them from an unconsumed state), so the
             * leaf's generic behavior is reproduced exactly. Which
             * parameter is the list is only known at the terminal
             * loop; record the exclusion mask and check it there. */
            static bool p1_leaf(P1WalkState &st,
                                const std::vector<P1Cond> &conds) {
                uint32_t excl_cons = 0, excl_nil = 0;

                for (const P1Cond &c : conds) {
                    uint32_t bit = 1u << c.param;

                    if ((c.kind == T2OpKind::IsNonemptyList && !c.pol) ||
                        (c.kind == T2OpKind::IsList && !c.pol) ||
                        (c.kind == T2OpKind::IsNil && c.pol)) {
                        excl_cons |= bit;
                    }
                    if ((c.kind == T2OpKind::IsNil && !c.pol) ||
                        (c.kind == T2OpKind::IsNonemptyList && c.pol) ||
                        (c.kind == T2OpKind::IsList && !c.pol)) {
                        excl_nil |= bit;
                    }
                }
                st.leaf_mask &= excl_cons & excl_nil;
                st.has_leaves = true;
                return true;
            }

            /* DFS over the wrapper CFG from the entry, following
             * only admissible edges. BEAM function bodies are DAGs
             * (loops are calls; recovery said this one has none), so
             * the walk terminates; the step budget caps pathological
             * diamond fan-out. */
            bool p1_walk_wrapper(P1WalkState &st,
                                 const T2BasicBlock *b,
                                 std::vector<P1Cond> conds,
                                 bool crossed_peel) {
                if (++st.steps > 128) {
                    *st.why = "wrapper region too branchy";
                    return false;
                }
                if (p1_is_err_block(b)) {
                    return p1_leaf(st, conds);
                }
                for (const T2Op *phi = b->phis_head; phi != nullptr;
                     phi = phi->next) {
                    if (phi->num_operands != 1) {
                        *st.why = "wrapper has a real merge";
                        return false;
                    }
                }

                bool entry = b == st.callee.blocks[0];
                uint32_t nops = 0;

                for (const T2Op *op = b->ops_head; op != nullptr;
                     op = op->next) {
                    if (op->kind == T2OpKind::Param) {
                        if (!entry) {
                            *st.why = "wrapper param outside the entry";
                            return false;
                        }
                        continue;
                    }
                    if (op->kind == T2OpKind::Allocate ||
                        op->kind == T2OpKind::Deallocate) {
                        nops++;
                        continue; /* never cloned; frames stay T1's */
                    }
                    if (op->kind == T2OpKind::CallFun) {
                        if (st.callfun != nullptr && st.callfun != op) {
                            *st.why = "wrapper applies the fun twice";
                            return false;
                        }
                        if (op->flags != 0 || op->sync == nullptr ||
                            op->index != st.fun_arity ||
                            op->num_operands != (uint16_t)(st.fun_arity + 1) ||
                            resolve_thru(op->operands[st.fun_arity]) !=
                                    st.params[st.fun_idx]) {
                            *st.why = "wrapper fun application shape "
                                      "not admissible";
                            return false;
                        }
                        st.callfun = op;
                        crossed_peel = true;
                        nops++;
                        continue;
                    }
                    if (!p1_callee_op_ok(op)) {
                        /* Opaque block: classified inputs never take
                         * it on the fast path (leaf mask checked at
                         * the terminal loop); do not look further. */
                        return p1_leaf(st, conds);
                    }
                    nops++;
                }
                if (st.counted.insert(b).second) {
                    st.region_ops += nops;
                    if (st.region_ops > T2_P1_MAX_WRAPPER_OPS) {
                        *st.why = "wrapper region too large";
                        return false;
                    }
                }

                const T2Op *t = b->terminator;

                if (t == nullptr) {
                    *st.why = "wrapper block without terminator";
                    return false;
                }
                switch (t->kind) {
                case T2OpKind::Jump:
                    return p1_walk_wrapper(st,
                                           t->succ_then,
                                           std::move(conds),
                                           crossed_peel);
                case T2OpKind::Branch: {
                    const T2Op *test = resolve_thru(t->operands[0])->def;

                    if (test == nullptr) {
                        return p1_leaf(st, conds);
                    }
                    if (test->kind == T2OpKind::IsFunction &&
                        resolve_thru(test->operands[0]) ==
                                st.params[st.fun_idx]) {
                        /* Statically discharged on the literal fun:
                         * only the taken edge is live for the
                         * specialized inputs. */
                        return p1_walk_wrapper(st,
                                               test->index == st.fun_arity
                                                       ? t->succ_then
                                                       : t->succ_else,
                                               std::move(conds),
                                               crossed_peel);
                    }
                    if (test->kind == T2OpKind::IsList ||
                        test->kind == T2OpKind::IsNonemptyList ||
                        test->kind == T2OpKind::IsNil) {
                        int32_t p =
                                p1_param_index(st.params,
                                               resolve_thru(test->operands[0]));

                        if (p < 0) {
                            return p1_leaf(st, conds);
                        }

                        std::vector<P1Cond> c1 = conds;

                        c1.push_back({(uint32_t)p, test->kind, true});
                        if (!p1_walk_wrapper(st,
                                             t->succ_then,
                                             std::move(c1),
                                             crossed_peel)) {
                            return false;
                        }
                        conds.push_back({(uint32_t)p, test->kind, false});
                        return p1_walk_wrapper(st,
                                               t->succ_else,
                                               std::move(conds),
                                               crossed_peel);
                    }
                    return p1_leaf(st, conds);
                }
                case T2OpKind::Return: {
                    /* Admissible only as the nil/acc exit: return of
                     * a parameter behind an IsNil=true guard on one
                     * parameter, with no fun application crossed. */
                    int32_t pr = p1_param_index(st.params,
                                                resolve_thru(t->operands[0]));
                    int32_t pl = -1;

                    for (const P1Cond &c : conds) {
                        if (c.kind == T2OpKind::IsNil && c.pol) {
                            if (pl >= 0 && pl != (int32_t)c.param) {
                                pl = -2;
                                break;
                            }
                            pl = (int32_t)c.param;
                        }
                    }
                    if (pr < 0 || pl < 0 || crossed_peel) {
                        *st.why = "wrapper return path is not the "
                                  "nil/acc exit";
                        return false;
                    }
                    if (st.nil_ret && (st.nil_cond != (uint32_t)pl ||
                                       st.nil_ret_param != (uint32_t)pr)) {
                        *st.why = "wrapper nil returns disagree";
                        return false;
                    }
                    st.nil_ret = true;
                    st.nil_cond = (uint32_t)pl;
                    st.nil_ret_param = (uint32_t)pr;
                    return true;
                }
                case T2OpKind::TailCall:
                case T2OpKind::TailCallExt: {
                    if (t->flags != 0 || t->sync == nullptr) {
                        *st.why = "wrapper tail call carries flags or "
                                  "no sync";
                        return false;
                    }
                    if (st.tailcall != nullptr && st.tailcall != t) {
                        *st.why = "wrapper has two tail calls";
                        return false;
                    }
                    for (const P1Cond &c : conds) {
                        /* A nil-positive guard before the tail call
                         * would send cons inputs elsewhere. */
                        if ((c.kind == T2OpKind::IsNil && c.pol) ||
                            (c.kind == T2OpKind::IsNonemptyList && !c.pol) ||
                            (c.kind == T2OpKind::IsList && !c.pol)) {
                            *st.why = "wrapper tail call behind a "
                                      "non-cons guard";
                            return false;
                        }
                        if ((c.kind == T2OpKind::IsList ||
                             c.kind == T2OpKind::IsNonemptyList) &&
                            c.pol) {
                            st.tc_list_tested |= 1u << c.param;
                        }
                    }
                    st.tailcall = t;
                    st.tc_crossed_peel |= crossed_peel;
                    return true;
                }
                default:
                    return p1_leaf(st, conds);
                }
            }

            /* Classify a non-loop callee as a thin fun-passing
             * wrapper. Read-only; fills *out on success. */
            bool p1_classify_wrapper(const T2Function &callee,
                                     uint32_t fun_idx,
                                     uint32_t fun_arity,
                                     P1Wrap *out,
                                     const char **why) {
                *why = "wrapper shape not admissible";

                uint32_t ar = callee.arity;

                if (ar == 0 || ar > T2_P1_MAX_ARITY || callee.blocks.empty() ||
                    fun_idx >= ar) {
                    return false;
                }

                std::vector<const T2Value *> params(ar, nullptr);

                for (const T2Op *op = callee.blocks[0]->ops_head;
                     op != nullptr && op->kind == T2OpKind::Param;
                     op = op->next) {
                    if (op->index >= ar || params[op->index] != nullptr) {
                        *why = "wrapper params not canonical";
                        return false;
                    }
                    params[op->index] = op->result;
                }
                for (uint32_t i = 0; i < ar; i++) {
                    if (params[i] == nullptr) {
                        *why = "wrapper params not canonical";
                        return false;
                    }
                }

                P1WalkState st{callee, params, fun_idx, fun_arity, why};

                if (!p1_walk_wrapper(st, callee.blocks[0], {}, false)) {
                    return false;
                }
                if (st.tailcall == nullptr) {
                    *why = "no fun-passing tail call on an admissible "
                           "spine";
                    return false;
                }

                const T2Op *tc = st.tailcall;

                if (tc->index != ar || tc->num_operands != ar) {
                    *why = "wrapper tail call changes arity";
                    return false;
                }

                /* Argument sources: pass-through params, or the
                 * one-iteration peel (CallFun result + GetTl). */
                int32_t peel_acc = -1, peel_list = -1;
                int32_t fun_pos = -1;
                uint32_t p_list = 0;
                uint32_t src[T2_P1_MAX_ARITY] = {0};

                for (uint32_t j = 0; j < ar; j++) {
                    const T2Value *root = resolve_thru(tc->operands[j]);
                    int32_t p = p1_param_index(params, root);

                    if (p >= 0) {
                        src[j] = (uint32_t)p;
                        if ((uint32_t)p == fun_idx) {
                            fun_pos = (int32_t)j;
                        }
                        continue;
                    }
                    if (st.callfun != nullptr && root == st.callfun->result) {
                        if (peel_acc >= 0) {
                            *why = "wrapper passes the fun result twice";
                            return false;
                        }
                        peel_acc = (int32_t)j;
                        continue;
                    }
                    if (root->def != nullptr &&
                        root->def->kind == T2OpKind::GetTl) {
                        int32_t q = p1_param_index(
                                params,
                                resolve_thru(root->def->operands[0]));

                        if (q < 0 || peel_list >= 0) {
                            *why = "wrapper tail advance not from a "
                                   "parameter";
                            return false;
                        }
                        peel_list = (int32_t)j;
                        p_list = (uint32_t)q;
                        src[j] = (uint32_t)q;
                        continue;
                    }
                    *why = "wrapper tail-call argument neither "
                           "pass-through nor peel";
                    return false;
                }

                bool peel = peel_acc >= 0 || peel_list >= 0;
                int32_t hd_op = -1, acc_op = -1;
                uint32_t p_acc = 0;

                if (peel) {
                    if (peel_acc < 0 || peel_list < 0 ||
                        st.callfun == nullptr || !st.tc_crossed_peel) {
                        *why = "wrapper peel incomplete";
                        return false;
                    }
                    for (uint32_t k = 0; k < fun_arity; k++) {
                        const T2Value *r =
                                resolve_thru(st.callfun->operands[k]);

                        if (r->def != nullptr &&
                            r->def->kind == T2OpKind::GetHd &&
                            p1_param_index(params,
                                           resolve_thru(r->def->operands[0])) ==
                                    (int32_t)p_list) {
                            if (hd_op >= 0) {
                                *why = "wrapper peel reads two heads";
                                return false;
                            }
                            hd_op = (int32_t)k;
                            continue;
                        }

                        int32_t q = p1_param_index(params, r);

                        if (q < 0 || acc_op >= 0) {
                            *why = "wrapper peel operands are not "
                                   "head + accumulator";
                            return false;
                        }
                        acc_op = (int32_t)k;
                        p_acc = (uint32_t)q;
                    }
                    if (hd_op < 0 || acc_op < 0) {
                        *why = "wrapper peel operands are not head + "
                               "accumulator";
                        return false;
                    }
                    src[peel_acc] = p_acc;
                } else if (st.callfun != nullptr) {
                    *why = "wrapper drops the fun application result";
                    return false;
                }

                /* The mapping must be a bijection (re-dispatch
                 * reconstructs the OUTER argument vector from the
                 * loop-carried state; a dropped or duplicated
                 * position would be unreconstructible). */
                uint32_t seen = 0;

                for (uint32_t j = 0; j < ar; j++) {
                    if (src[j] >= ar || (seen & (1u << src[j])) != 0) {
                        *why = "wrapper argument mapping is not a "
                               "bijection";
                        return false;
                    }
                    seen |= 1u << src[j];
                }
                if (fun_pos < 0 || src[fun_pos] != fun_idx) {
                    *why = "fun not passed through as an argument";
                    return false;
                }

                uint32_t s_inv[T2_P1_MAX_ARITY] = {0};

                for (uint32_t j = 0; j < ar; j++) {
                    s_inv[src[j]] = j;
                }

                out->m = tc->mfa_m;
                out->f = tc->mfa_f;
                out->arity = ar;
                for (uint32_t j = 0; j < ar; j++) {
                    out->src[j] = src[j];
                }
                out->fun_pos = (uint32_t)fun_pos;
                out->peel = peel;
                out->peel_acc = peel_acc;
                out->peel_list = peel_list;
                out->peel_hd_op = hd_op;
                out->peel_acc_op = acc_op;
                out->nil_ret = st.nil_ret;
                out->nil_cond = st.nil_ret ? (int32_t)s_inv[st.nil_cond] : -1;
                out->nil_ret_pos =
                        st.nil_ret ? (int32_t)s_inv[st.nil_ret_param] : -1;
                out->list_tested = p1_remap_mask(st.tc_list_tested, src, ar);
                out->has_leaves = st.has_leaves;
                out->leaf_mask =
                        st.has_leaves
                                ? p1_remap_mask(st.leaf_mask & ((1u << ar) - 1),
                                                src,
                                                ar)
                                : 0;
                return true;
            }

            /* Terminal congruence: the chain's accumulated wrapper
             * constraints (in the loop's argument space) against the
             * ADMITTED loop's structure. This is what makes skipping
             * the wrapper bodies sound: peel == loop step, wrapper
             * nil exit == loop nil exit, wrapper list guards on the
             * loop's list param, err/opaque leaves confined to
             * inputs the loop's own dispatch re-dispatches. */
            bool p1_chain_congruent(const T2Function &callee,
                                    const T2Loop &loop,
                                    uint32_t fun_idx,
                                    int32_t acc_idx,
                                    const P1Pending &pend,
                                    const char **why) {
                *why = "wrapper chain not congruent with the loop";

                uint32_t ar = callee.arity;
                const T2BasicBlock *ch = callee.blocks[loop.header];
                uint32_t latch_id = loop.latches[0];

                std::vector<const T2Op *> phis(ar, nullptr);

                for (const T2Op *phi = ch->phis_head; phi != nullptr;
                     phi = phi->next) {
                    phis[t2_reg_index(phi->dst_reg)] = phi;
                }

                auto latch_input = [&](const T2Op *phi) -> const T2Value * {
                    for (uint16_t i = 0; i < phi->num_operands; i++) {
                        if (phi->phi_blocks[i]->id == latch_id) {
                            return phi->operands[i];
                        }
                    }
                    return nullptr;
                };

                const T2Op *callfun = nullptr;

                for (const T2Op *op = callee.blocks[latch_id]->ops_head;
                     op != nullptr;
                     op = op->next) {
                    if (op->kind == T2OpKind::CallFun) {
                        callfun = op;
                    }
                }

                /* The loop's list position: the phi advanced by
                 * GetTl of itself. */
                int32_t ll = -1;

                for (uint32_t i = 0; i < ar; i++) {
                    if (i == fun_idx) {
                        continue;
                    }
                    const T2Value *lv = latch_input(phis[i]);

                    if (lv == nullptr) {
                        continue;
                    }
                    lv = resolve_thru(lv);
                    if (lv->def != nullptr &&
                        lv->def->kind == T2OpKind::GetTl &&
                        resolve_thru(lv->def->operands[0]) == phis[i]->result) {
                        if (ll >= 0) {
                            *why = "loop advances two list params";
                            return false;
                        }
                        ll = (int32_t)i;
                    }
                }

                bool need_ll = pend.peel || pend.nil_ret ||
                               pend.list_tested != 0 || pend.has_leaves;

                if (need_ll && ll < 0) {
                    *why = "loop has no list-advancing param";
                    return false;
                }

                if (pend.peel) {
                    if (acc_idx < 0 || callfun == nullptr ||
                        pend.peel_acc != acc_idx || pend.peel_list != ll) {
                        *why = "wrapper peel does not match the loop "
                               "step";
                        return false;
                    }
                    if ((uint32_t)pend.peel_hd_op >=
                                (uint32_t)(callfun->num_operands - 1) ||
                        (uint32_t)pend.peel_acc_op >=
                                (uint32_t)(callfun->num_operands - 1)) {
                        *why = "wrapper peel does not match the loop "
                               "step";
                        return false;
                    }

                    const T2Value *hd =
                            resolve_thru(callfun->operands[pend.peel_hd_op]);

                    if (hd->def == nullptr ||
                        hd->def->kind != T2OpKind::GetHd ||
                        resolve_thru(hd->def->operands[0]) !=
                                phis[ll]->result) {
                        *why = "loop step head operand differs from "
                               "the peel";
                        return false;
                    }
                    if (resolve_thru(callfun->operands[pend.peel_acc_op]) !=
                        phis[acc_idx]->result) {
                        *why = "loop step acc operand differs from "
                               "the peel";
                        return false;
                    }
                }

                if (pend.nil_ret) {
                    if (acc_idx < 0 || pend.nil_cond != ll ||
                        pend.nil_ret_pos != acc_idx) {
                        *why = "wrapper nil return does not match the "
                               "loop exit";
                        return false;
                    }
                    for (const T2BasicBlock *b : callee.blocks) {
                        if (p1_is_err_block(b) || b->terminator == nullptr ||
                            b->terminator->kind != T2OpKind::Return) {
                            continue;
                        }
                        if (resolve_thru(b->terminator->operands[0]) !=
                            phis[acc_idx]->result) {
                            *why = "loop exit does not return the "
                                   "accumulator";
                            return false;
                        }
                    }
                }

                if (ll >= 0 && (pend.list_tested & ~(1u << ll)) != 0) {
                    *why = "wrapper list guard on a non-list param";
                    return false;
                }
                if (pend.has_leaves &&
                    (ll < 0 || (pend.leaf_mask & (1u << ll)) == 0)) {
                    *why = "wrapper error paths not confined to "
                           "non-list inputs";
                    return false;
                }
                return true;
            }

            void p1_reject(const T2Op *call, const char *why) {
                if (p1_trace()) {
                    erts_fprintf(stderr,
                                 "t2_p1: %T:%T/%u site %T:%T/%u rejected: "
                                 "%s\n",
                                 fn.module,
                                 fn.function,
                                 (unsigned)fn.arity,
                                 call->mfa_m,
                                 call->mfa_f,
                                 (unsigned)call->index,
                                 why);
                }
            }

            /* Ops admissible in the cloned callee loop, besides the
             * structural ones checked separately (the one CallFun, the
             * one Allocate/Deallocate pair, the recovery RC, Params in
             * the entry block, phis in the header). Deliberately
             * narrower than fun_op_ok: no Add/Sub (a callee-body
             * arith would carry a callee-relative sync/beam_idx that
             * means nothing in the caller) and no ConstLiteral (a
             * dynamic literal's term dies with the callee decode). */
            static bool p1_callee_op_ok(const T2Op *op) {
                if ((op->flags & ~(uint16_t)T2_OP_PAIR_HEAD) != 0) {
                    return false;
                }
                switch (op->kind) {
                case T2OpKind::ConstInt:
                case T2OpKind::ConstAtom:
                case T2OpKind::ConstNil:
                case T2OpKind::Copy:
                case T2OpKind::GetHd:
                case T2OpKind::GetTl:
                case T2OpKind::GetTupleElement:
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
                    return true;
                default:
                    return false;
                }
            }

            /* An error block from the P1 cloner's point of view: the
             * terminator raises (T2_OP_ERR_EXIT_*). Unlike splice_fun's
             * is_err_block it tolerates phis — the Braun builder leaves
             * trivial single-input phis in shared raise blocks, and the
             * cloner drops the whole block (error edges collapse onto
             * the re-dispatch block), phis included.
             *
             * P1c-2 widens the shape (behind its lever) with two more
             * not-the-fast-path leaves, both consumed the same way —
             * inputs reaching them are excluded from the inlined path
             * by the leaf-confinement mask (classification) or their
             * edges collapse onto the re-dispatch block (clone):
             *
             * - Opaque: a tolerant partial build's degrade marker
             *   (classification only; a tolerant callee never commits).
             * - A hand-rolled noreturn raise: a plain tail call to
             *   erlang:error/1,2 (flags 0) never returns — Elixir
             *   spells its function_clause exits this way instead of
             *   case_end — so the block behaves exactly like a flagged
             *   error exit. */
            static bool p1_is_err_block(const T2BasicBlock *b) {
                if (b == nullptr || b->terminator == nullptr) {
                    return false;
                }

                const T2Op *t = b->terminator;

                if ((t->flags & (T2_OP_ERR_EXIT_SHARED | T2_OP_ERR_EXIT_OP)) !=
                    0) {
                    return true;
                }
                if (p1c2_disabled()) {
                    return false;
                }
                return t->kind == T2OpKind::Opaque ||
                       (t->kind == T2OpKind::TailCallExt && t->flags == 0 &&
                        t->mfa_m == am_erlang && t->mfa_f == am_error &&
                        (t->index == 1 || t->index == 2));
            }

            /* Read-only admission of the RECOVERED callee. Fills the
             * accumulator param index (the phi whose latch input is the
             * CallFun's result; -1 when none) — everything else is
             * re-derived structurally by the clone pass. */
            bool p1_admit_callee(const T2Function &callee,
                                 const T2LoopInfo &cli,
                                 uint32_t fun_idx,
                                 uint32_t fun_arity,
                                 int32_t *acc_idx_out,
                                 const char **why) {
                *why = "callee shape not admissible";

                if (cli.loops.size() != 1 || cli.loops[0].latches.size() != 1 ||
                    cli.loops[0].preheader != 0 ||
                    cli.loops[0].header >= callee.blocks.size()) {
                    *why = "callee is not a single-latch recovered loop";
                    return false;
                }

                const T2Loop &loop = cli.loops[0];
                const T2BasicBlock *ce = callee.blocks[0];
                const T2BasicBlock *ch = callee.blocks[loop.header];
                uint32_t latch_id = loop.latches[0];
                uint32_t ar = callee.arity;

                /* Entry block: only Params + the jump to the header. */
                for (const T2Op *op = ce->ops_head; op != nullptr;
                     op = op->next) {
                    if (op->kind != T2OpKind::Param) {
                        *why = "callee entry block has non-Param ops";
                        return false;
                    }
                }
                if (ce->terminator == nullptr ||
                    ce->terminator->kind != T2OpKind::Jump ||
                    ce->terminator->succ_then != ch) {
                    *why = "callee entry does not jump to the loop header";
                    return false;
                }

                /* Header phis: exactly one per param, homed X0..ar-1. */
                std::vector<const T2Op *> phis(ar, nullptr);
                for (const T2Op *phi = ch->phis_head; phi != nullptr;
                     phi = phi->next) {
                    if (phi->dst_reg == T2_REG_NONE ||
                        !t2_reg_is_x(phi->dst_reg) ||
                        t2_reg_index(phi->dst_reg) >= ar ||
                        phis[t2_reg_index(phi->dst_reg)] != nullptr) {
                        *why = "callee header phis are not the param vector";
                        return false;
                    }
                    phis[t2_reg_index(phi->dst_reg)] = phi;
                }
                for (uint32_t i = 0; i < ar; i++) {
                    if (phis[i] == nullptr) {
                        *why = "callee header phi missing";
                        return false;
                    }
                }

                /* Latch input per phi. */
                auto latch_input = [&](const T2Op *phi) -> const T2Value * {
                    for (uint16_t i = 0; i < phi->num_operands; i++) {
                        if (phi->phi_blocks[i]->id == latch_id) {
                            return phi->operands[i];
                        }
                    }
                    return nullptr;
                };

                /* The fun param must be loop-invariant: its latch input
                 * resolves (through copies) back to the phi itself. */
                {
                    const T2Value *lv = latch_input(phis[fun_idx]);

                    if (lv == nullptr ||
                        resolve_thru(lv) != phis[fun_idx]->result) {
                        *why = "callee fun param is not loop-invariant";
                        return false;
                    }
                }

                /* Walk every block: census the structural ops, check
                 * the whitelist. Error blocks are admissible exits. */
                const T2Op *callfun = nullptr;
                const T2Op *alloc = nullptr;
                const T2Op *dealloc = nullptr;
                const T2Op *rc = nullptr;
                uint32_t nops = 0;

                for (const T2BasicBlock *b : callee.blocks) {
                    if (b == ce || p1_is_err_block(b)) {
                        continue;
                    }
                    if (b != ch) {
                        /* The Braun builder leaves trivial single-input
                         * phis in once-unsealed blocks; the clone
                         * forwards them through the value map. Real
                         * merges outside the header are out of P1a
                         * scope. */
                        for (const T2Op *phi = b->phis_head; phi != nullptr;
                             phi = phi->next) {
                            if (phi->num_operands != 1) {
                                *why = "callee has a merge outside the "
                                       "loop header";
                                return false;
                            }
                        }
                    }
                    for (const T2Op *op = b->ops_head; op != nullptr;
                         op = op->next) {
                        nops++;
                        switch (op->kind) {
                        case T2OpKind::IsFunction: {
                            /* P1c-2: an IsFunction inside the admitted
                             * loop (Elixir's inlined-foldl helper
                             * guards is_function2 on its nil exit) has
                             * no isel lowering, so it is admissible
                             * only when the commit can statically
                             * discharge it on the literal fun: the
                             * tested value must be the fun param's
                             * phi, the tested arity the literal fun's,
                             * and the result consumed by exactly one
                             * direct Branch condition (the clone drops
                             * the op and rewires that branch to its
                             * taken edge). */
                            if (p1c2_disabled() || op->flags != 0 ||
                                op->index != fun_arity ||
                                resolve_thru(op->operands[0]) !=
                                        phis[fun_idx]->result) {
                                *why = "callee IsFunction not "
                                       "dischargeable on the literal "
                                       "fun";
                                return false;
                            }

                            uint32_t uses = 0, branch_uses = 0;

                            for (const T2BasicBlock *ub : callee.blocks) {
                                for (const T2Op *u = ub->phis_head;
                                     u != nullptr;
                                     u = u->next) {
                                    for (uint16_t i = 0; i < u->num_operands;
                                         i++) {
                                        uses += u->operands[i] == op->result;
                                    }
                                }
                                for (const T2Op *u = ub->ops_head; u != nullptr;
                                     u = u->next) {
                                    for (uint16_t i = 0; i < u->num_operands;
                                         i++) {
                                        uses += u->operands[i] == op->result;
                                    }
                                }

                                const T2Op *u = ub->terminator;

                                if (u == nullptr) {
                                    continue;
                                }
                                for (uint16_t i = 0; i < u->num_operands; i++) {
                                    uses += u->operands[i] == op->result;
                                }
                                branch_uses += u->kind == T2OpKind::Branch &&
                                               u->operands[0] == op->result;
                            }
                            if (uses != 1 || branch_uses != 1) {
                                *why = "callee IsFunction result "
                                       "escapes its guard branch";
                                return false;
                            }
                            continue;
                        }
                        case T2OpKind::CallFun:
                            if (callfun != nullptr || op->flags != 0 ||
                                op->sync == nullptr || b->id != latch_id ||
                                op->index != fun_arity) {
                                *why = "callee CallFun census failed";
                                return false;
                            }
                            callfun = op;
                            continue;
                        case T2OpKind::Allocate:
                            if (alloc != nullptr || b->id != latch_id ||
                                op->imm_int != 0) {
                                *why = "callee frame shape not admissible";
                                return false;
                            }
                            alloc = op;
                            continue;
                        case T2OpKind::Deallocate:
                            if (dealloc != nullptr || b->id != latch_id) {
                                *why = "callee frame shape not admissible";
                                return false;
                            }
                            dealloc = op;
                            continue;
                        case T2OpKind::ReductionCheck:
                            if (rc != nullptr || op->flags != 0 ||
                                op->sync == nullptr || b->id != latch_id) {
                                *why = "callee back edge not admissible";
                                return false;
                            }
                            rc = op;
                            continue;
                        default:
                            break;
                        }
                        if (!p1_callee_op_ok(op)) {
                            *why = "callee op outside the P1a whitelist";
                            return false;
                        }
                    }
                    const T2Op *t = b->terminator;

                    if (t == nullptr) {
                        *why = "callee block without terminator";
                        return false;
                    }
                    switch (t->kind) {
                    case T2OpKind::Jump:
                    case T2OpKind::Branch:
                        break;
                    case T2OpKind::Return:
                        if (t->num_operands != 1 ||
                            t->operand_regs == nullptr ||
                            t->operand_regs[0] != t2_xreg(0)) {
                            *why = "callee return value not in x0";
                            return false;
                        }
                        break;
                    default:
                        *why = "callee terminator outside the P1a set";
                        return false;
                    }
                }

                if (callfun == nullptr || rc == nullptr ||
                    (alloc == nullptr) != (dealloc == nullptr) ||
                    (alloc != nullptr && alloc->index != dealloc->index) ||
                    nops > T2_P1_MAX_CALLEE_OPS) {
                    *why = "callee census failed";
                    return false;
                }

                /* The CallFun's fun operand (last) must be the fun
                 * param's phi, through copies. */
                if (callfun->num_operands != fun_arity + 1 ||
                    resolve_thru(
                            callfun->operands[callfun->num_operands - 1]) !=
                            phis[fun_idx]->result) {
                    *why = "callee CallFun does not apply the fun param";
                    return false;
                }

                /* Order within the latch block: [alloc <] callfun
                 * [< dealloc] < rc. */
                {
                    int state = alloc == nullptr ? 1 : 0;

                    for (const T2Op *op = callee.blocks[latch_id]->ops_head;
                         op != nullptr;
                         op = op->next) {
                        if (op == alloc && state == 0) {
                            state = 1;
                        } else if (op == callfun && state == 1) {
                            state = 2;
                        } else if (op == dealloc && state == 2) {
                            state = 3;
                        } else if (op == rc &&
                                   state == (alloc == nullptr ? 2 : 3)) {
                            state = 4;
                        } else if (op == alloc || op == callfun ||
                                   op == dealloc || op == rc) {
                            *why = "callee latch op order not admissible";
                            return false;
                        }
                    }
                    if (state != 4) {
                        *why = "callee latch op order not admissible";
                        return false;
                    }
                }

                /* The accumulator param: the phi whose latch input
                 * resolves to the CallFun's result. */
                *acc_idx_out = -1;
                for (uint32_t i = 0; i < ar; i++) {
                    if (i == fun_idx) {
                        continue;
                    }
                    const T2Value *lv = latch_input(phis[i]);

                    if (lv != nullptr && resolve_thru(lv) == callfun->result) {
                        *acc_idx_out = (int32_t)i;
                    }
                }

                return true;
            }

            /* The clone + specialization pass. Runs inside the callee
             * build callback (the callee HIR is alive), AFTER all
             * read-only admission passed; any failure here is a hard
             * error that degrades the whole function to T1, loudly. */
            bool p1_commit(T2Op *call,
                           T2Function &callee,
                           const T2Loop &loop,
                           uint32_t fun_idx,
                           int32_t acc_idx,
                           const T2Op *mf,
                           Eterm outer_m,
                           Eterm outer_f,
                           const void *outer_lf,
                           Eterm inner_m,
                           Eterm inner_f,
                           const void *inner_lf,
                           const std::vector<const void *> &chain_hdrs,
                           const uint32_t *perm,
                           uint32_t rc1_extra) {
                uint32_t bi = call->beam_idx;
                T2SyncMap *cmap = call->sync;
                uint32_t ar = callee.arity;
                T2BasicBlock *ce = callee.blocks[0];
                T2BasicBlock *ch = callee.blocks[loop.header];
                uint32_t latch_id = loop.latches[0];

                /* Under a non-identity permutation (a transitive
                 * chain that reorders arguments) the clone relabels
                 * every x home below the arity, so the loop-carried
                 * vector is PHYSICALLY resident in the OUTER argument
                 * positions — sync maps are pins over identity
                 * placement, never move scripts. Homes at or above
                 * the arity are iteration-locals; the re-homing pass
                 * moves them anyway. */
                auto rn = [&](int32_t reg) -> int32_t {
                    if (reg != T2_REG_NONE && t2_reg_is_x(reg) &&
                        t2_reg_index(reg) < ar) {
                        return t2_xreg(perm[t2_reg_index(reg)]);
                    }
                    return reg;
                };

                /* ---- callee structure (re-derived; admission holds) -- */

                std::vector<const T2Op *> phis(ar, nullptr);
                for (const T2Op *phi = ch->phis_head; phi != nullptr;
                     phi = phi->next) {
                    phis[t2_reg_index(phi->dst_reg)] = phi;
                }

                auto latch_input = [&](const T2Op *phi) -> T2Value * {
                    for (uint16_t i = 0; i < phi->num_operands; i++) {
                        if (phi->phi_blocks[i]->id == latch_id) {
                            return phi->operands[i];
                        }
                    }
                    return nullptr;
                };

                /* ---- inner re-dispatch: the loop-carried list -------- *
                 * The terminal loop dispatches cons/nil on exactly one
                 * of its params (the list); the inner re-dispatch's
                 * entry pre-screen tests the same param at the
                 * boundary. A callee whose list tests do not name one
                 * unique param phi falls back to the outermost
                 * re-dispatch (inner_lf = null). */
                int32_t list_idx = -1;

                if (inner_lf != nullptr) {
                    bool ambiguous = false;

                    for (const T2BasicBlock *b : callee.blocks) {
                        if (b == ce || p1_is_err_block(b)) {
                            continue;
                        }
                        for (const T2Op *op = b->ops_head; op != nullptr;
                             op = op->next) {
                            if (op->kind != T2OpKind::IsNonemptyList &&
                                op->kind != T2OpKind::IsNil) {
                                continue;
                            }
                            const T2Value *tv = resolve_thru(op->operands[0]);

                            for (uint32_t i = 0; i < ar; i++) {
                                if (phis[i] == nullptr ||
                                    tv != phis[i]->result) {
                                    continue;
                                }
                                if (list_idx >= 0 && list_idx != (int32_t)i) {
                                    ambiguous = true;
                                }
                                list_idx = (int32_t)i;
                            }
                        }
                    }
                    if (list_idx < 0 || ambiguous) {
                        if (p1_trace()) {
                            erts_fprintf(stderr,
                                         "t2_p1: %T:%T/%u inner re-dispatch "
                                         "off (no unique list param in "
                                         "%T:%T/%u), outermost kept\n",
                                         fn.module,
                                         fn.function,
                                         (unsigned)fn.arity,
                                         inner_m,
                                         inner_f,
                                         (unsigned)ar);
                        }
                        inner_lf = nullptr;
                    }
                }

                const bool inner = inner_lf != nullptr;

                /* ---- clone ------------------------------------------- */

                std::unordered_map<const T2Value *, T2Value *> vmap;
                std::unordered_map<const T2BasicBlock *, T2BasicBlock *> bmap;
                std::vector<T2Op *> clones; /* cloned body ops, in order */
                std::vector<T2Op *> cl_phis(ar, nullptr);
                T2Op *ccf = nullptr; /* the cloned CallFun               */
                T2Op *crc = nullptr; /* the cloned back-edge RC          */

                T2BasicBlock *b_grd = fn.new_block();
                T2BasicBlock *b_err = fn.new_block();
                /* Inner mode splits the error funnel: in-loop error
                 * edges (mid-list, by the entry pre-screen) demote into
                 * the TERMINAL loop function's body (b_err_in), while
                 * the pre-screen's own miss — a malformed argument the
                 * generic wrapper chain would have examined first —
                 * keeps the outermost re-dispatch (b_err). b_ok is the
                 * pre-screen's reconvergence: the loop header's single
                 * entry predecessor. */
                T2BasicBlock *b_err_in = inner ? fn.new_block() : nullptr;
                T2BasicBlock *b_ok = inner ? fn.new_block() : b_grd;

                /* Params + the invariant fun phi bind to the boundary
                 * vector (identical to the operands; the validator
                 * asserted it). */
                for (const T2Op *op = ce->ops_head;
                     op != nullptr && op->kind == T2OpKind::Param;
                     op = op->next) {
                    vmap[op->result] = cmap->x[perm[op->index]];
                }
                vmap[phis[fun_idx]->result] = cmap->x[perm[fun_idx]];

                /* Block skeleton (entry + error blocks are not cloned;
                 * error edges collapse onto the re-dispatch block). */
                for (const T2BasicBlock *b : callee.blocks) {
                    if (b == ce || p1_is_err_block(b)) {
                        continue;
                    }
                    bmap[b] = fn.new_block();
                }
                auto target_of = [&](T2BasicBlock *cb) -> T2BasicBlock * {
                    auto it = bmap.find(cb);

                    return it != bmap.end() ? it->second
                                            : (inner ? b_err_in : b_err);
                };

                /* Header phis (except the fun's). */
                for (uint32_t i = 0; i < ar; i++) {
                    if (i == fun_idx) {
                        continue;
                    }
                    T2Op *np = fn.new_phi(bmap.at(ch), phis[i]->type);

                    np->dst_reg = t2_xreg(perm[i]);
                    np->flags = T2_OP_INLINED;
                    np->beam_idx = bi;
                    vmap[phis[i]->result] = np->result;
                    cl_phis[i] = np;
                }

                auto mapped = [&](const T2Value *v) -> T2Value * {
                    auto it = vmap.find(v);

                    return it == vmap.end() ? nullptr : it->second;
                };

                /* Body ops, header block first (its defs dominate the
                 * loop), then the rest in index order. */
                std::vector<const T2BasicBlock *> order;

                order.push_back(ch);
                for (const T2BasicBlock *b : callee.blocks) {
                    if (b == ce || b == ch || p1_is_err_block(b)) {
                        continue;
                    }
                    order.push_back(b);
                }

                for (const T2BasicBlock *b : order) {
                    T2BasicBlock *nb = bmap.at(b);

                    /* Trivial single-input phis (Braun leftovers in
                     * once-unsealed blocks) forward through the value
                     * map; their home must already be the input's home
                     * (it is, by Braun construction — checked). */
                    if (b != ch) {
                        for (const T2Op *phi = b->phis_head; phi != nullptr;
                             phi = phi->next) {
                            T2Value *mv = mapped(phi->operands[0]);

                            if (mv == nullptr) {
                                return fail("p1: trivial phi input escapes "
                                            "the value map");
                            }
                            if (mv->def == nullptr ||
                                mv->def->dst_reg != rn(phi->dst_reg)) {
                                return fail("p1: trivial phi home differs "
                                            "from its input's home");
                            }
                            vmap[phi->result] = mv;
                        }
                    }

                    for (const T2Op *op = b->ops_head; op != nullptr;
                         op = op->next) {
                        /* The callee's frame is dropped by construction:
                         * the erased CallFun was the only reason for it
                         * (admission pinned the one balanced pair). */
                        if (op->kind == T2OpKind::Allocate ||
                            op->kind == T2OpKind::Deallocate) {
                            continue;
                        }
                        /* Statically discharged on the literal fun
                         * (admission pinned the shape); the consuming
                         * branch is rewired to its taken edge below
                         * and the op vanishes — IsFunction has no isel
                         * lowering. */
                        if (op->kind == T2OpKind::IsFunction) {
                            continue;
                        }

                        T2Op *cl = fn.new_op(nb, op->kind, op->type);
                        std::vector<T2Value *> ins;

                        for (uint16_t i = 0; i < op->num_operands; i++) {
                            T2Value *mv = mapped(op->operands[i]);

                            if (mv == nullptr) {
                                return fail("p1: callee operand escapes "
                                            "the value map");
                            }
                            ins.push_back(mv);
                        }
                        fn.set_operands(cl, ins);
                        if (op->operand_regs != nullptr) {
                            cl->operand_regs = fn.arena.alloc_array<int32_t>(
                                    op->num_operands);
                            for (uint16_t i = 0; i < op->num_operands; i++) {
                                cl->operand_regs[i] = rn(op->operand_regs[i]);
                            }
                        }
                        cl->imm_int = op->imm_int;
                        cl->imm_term = op->imm_term;
                        cl->mfa_m = op->mfa_m;
                        cl->mfa_f = op->mfa_f;
                        cl->index = op->index;
                        cl->bif_num = op->bif_num;
                        cl->live = op->live;
                        cl->dst_reg = rn(op->dst_reg);
                        /* PAIR_HEAD dropped: the re-homing below moves
                         * every pair destination off its source, so the
                         * clones are safe as plain singles. */
                        cl->flags = T2_OP_INLINED |
                                    (op->flags & T2_OP_TUPLE_ARITY_FUSED);
                        cl->sync = nullptr;
                        cl->beam_idx = bi;

                        if (op->result != nullptr) {
                            vmap[op->result] = cl->result;
                        }
                        if (op->kind == T2OpKind::CallFun) {
                            ccf = cl;
                        }
                        if (op->kind == T2OpKind::ReductionCheck) {
                            crc = cl;
                        }
                        clones.push_back(cl);
                    }

                    const T2Op *t = b->terminator;

                    switch (t->kind) {
                    case T2OpKind::Jump:
                        fn.emit_jump(nb, target_of(t->succ_then));
                        break;
                    case T2OpKind::Branch: {
                        const T2Op *test = resolve_thru(t->operands[0])->def;

                        if (test != nullptr &&
                            test->kind == T2OpKind::IsFunction) {
                            /* Statically discharged: admission pinned
                             * the test to the fun phi with the literal
                             * fun's own arity — always TRUE. */
                            fn.emit_jump(nb, target_of(t->succ_then));
                            break;
                        }

                        T2Value *cond = mapped(t->operands[0]);

                        if (cond == nullptr) {
                            return fail("p1: callee branch cond escapes "
                                        "the value map");
                        }
                        fn.emit_branch(nb,
                                       cond,
                                       target_of(t->succ_then),
                                       target_of(t->succ_else));
                        break;
                    }
                    case T2OpKind::Return: {
                        T2Value *rv = mapped(t->operands[0]);

                        if (rv == nullptr) {
                            return fail("p1: callee return escapes the "
                                        "value map");
                        }
                        T2Op *ret_op =
                                fn.new_op(nb, T2OpKind::Return, T2Type::none());

                        fn.set_operands(ret_op, {rv});
                        ret_op->operand_regs = fn.arena.alloc_array<int32_t>(1);
                        ret_op->operand_regs[0] = t2_xreg(0);
                        ret_op->sync = make_map({rv}, cmap);
                        ret_op->beam_idx = bi;
                        break;
                    }
                    default:
                        return fail("p1: callee terminator outside the "
                                    "admitted set");
                    }
                    nb->terminator->beam_idx = bi;
                }
                if (ccf == nullptr || crc == nullptr) {
                    return fail("p1: cloned CallFun/RC vanished");
                }

                /* ---- keep-sets for the re-homing pass ----------------- */

                /* Latch materializations: the (mapped) values feeding
                 * the phis' latch inputs keep the phi homes. */
                std::unordered_set<const T2Value *> keep;
                std::vector<T2Value *> latch_vec(ar, nullptr);

                for (uint32_t i = 0; i < ar; i++) {
                    if (i == fun_idx) {
                        latch_vec[i] = cmap->x[perm[fun_idx]];
                        continue;
                    }
                    T2Value *mv = mapped(latch_input(phis[i]));

                    if (mv == nullptr) {
                        return fail("p1: latch input escapes the value map");
                    }
                    if (mv->def == nullptr ||
                        mv->def->dst_reg != t2_xreg(perm[i])) {
                        return fail("p1: latch materialization is not in "
                                    "the phi home");
                    }
                    keep.insert(mv);
                    latch_vec[i] = mv;
                }
                /* Return-value materializations keep X0: the return
                 * convention pins x0 regardless of the permutation,
                 * so un-relabel them (exit-path only — the loop's
                 * phis never write x0's occupant mid-iteration). */
                for (const auto &e : bmap) {
                    const T2Op *t = e.second->terminator;

                    if (t != nullptr && t->kind == T2OpKind::Return) {
                        const T2Value *rv = t->operands[0];

                        if (rv->def == nullptr ||
                            rv->def->dst_reg != rn(t2_xreg(0))) {
                            return fail("p1: return value is not in x0");
                        }
                        if (rv->def->dst_reg != t2_xreg(0)) {
                            if (keep.count(rv) != 0) {
                                return fail("p1: return value doubles as "
                                            "a latch input under a "
                                            "permutation");
                            }
                            rv->def->dst_reg = t2_xreg(0);
                        }
                        keep.insert(rv);
                    }
                }

                /* ---- re-home iteration-locals off X0..ar-1 and Y ------ */

                uint32_t next_free = ar;

                for (const T2Op *cl : clones) {
                    if (cl->dst_reg != T2_REG_NONE &&
                        t2_reg_is_x(cl->dst_reg) &&
                        t2_reg_index(cl->dst_reg) >= next_free) {
                        next_free = t2_reg_index(cl->dst_reg) + 1;
                    }
                }

                for (T2Op *cl : clones) {
                    if (cl->result == nullptr || cl->dst_reg == T2_REG_NONE ||
                        keep.count(cl->result) != 0) {
                        continue;
                    }
                    if (t2_reg_is_x(cl->dst_reg) &&
                        t2_reg_index(cl->dst_reg) >= ar) {
                        continue;
                    }

                    int32_t nr = t2_xreg(next_free++);

                    cl->dst_reg = nr;
                    for (T2Op *u : clones) {
                        if (u->operand_regs == nullptr) {
                            continue;
                        }
                        for (uint16_t i = 0; i < u->num_operands; i++) {
                            if (u->operands[i] == cl->result) {
                                u->operand_regs[i] = nr;
                            }
                        }
                    }
                    for (const auto &e : bmap) {
                        T2Op *t = e.second->terminator;

                        if (t == nullptr || t->operand_regs == nullptr) {
                            continue;
                        }
                        for (uint16_t i = 0; i < t->num_operands; i++) {
                            if (t->operands[i] == cl->result) {
                                t->operand_regs[i] = nr;
                            }
                        }
                    }
                }

                /* ---- devirtualize: splice the fun body at the CallFun - */

                T2BasicBlock *cb = ccf->block; /* the cloned latch      */
                T2BasicBlock *b_lat2 = fn.new_block();

                {
                    T2Op *after = ccf->next;

                    if (after != nullptr) {
                        after->prev = nullptr;
                        b_lat2->ops_head = after;
                        b_lat2->ops_tail = cb->ops_tail;
                        for (T2Op *q = after; q != nullptr; q = q->next) {
                            q->block = b_lat2;
                        }
                        ccf->next = nullptr;
                        cb->ops_tail = ccf;
                    }
                    b_lat2->terminator = cb->terminator;
                    if (b_lat2->terminator != nullptr) {
                        b_lat2->terminator->block = b_lat2;
                    }
                    cb->terminator = nullptr;
                }

                uint32_t fun_arity = ccf->num_operands - 1;
                std::vector<T2Value *> fargs;
                std::vector<int32_t> farg_homes;

                for (uint32_t j = 0; j < fun_arity; j++) {
                    T2Value *root = const_cast<T2Value *>(
                            resolve_copies(ccf->operands[j]));

                    if (is_const_def(root)) {
                        fargs.push_back(root);
                        farg_homes.push_back(T2_REG_NONE);
                    } else if (root->def != nullptr &&
                               root->def->dst_reg != T2_REG_NONE) {
                        fargs.push_back(root);
                        farg_homes.push_back(root->def->dst_reg);
                    } else {
                        fargs.push_back(ccf->operands[j]);
                        farg_homes.push_back(ccf->operand_regs != nullptr
                                                     ? ccf->operand_regs[j]
                                                     : T2_REG_NONE);
                    }
                }

                FunBody fb;
                bool spliced = false;
                bool splice_hard_err = false;

                {
                    uint32_t impl_idx = (uint32_t)mf->imm_int;
                    std::string berr;
                    bool built = t2_build_selected(
                            ret,
                            &impl_idx,
                            1,
                            [&](T2Function &impl) {
                                FunBody tmp;

                                if (splice_fun(impl,
                                               fargs,
                                               farg_homes,
                                               b_err,
                                               next_free,
                                               &tmp)) {
                                    fb = std::move(tmp);
                                    spliced = true;
                                } else if (err != nullptr && !err->empty()) {
                                    splice_hard_err = true;
                                }
                            },
                            &berr);

                    if (!built || splice_hard_err || !spliced) {
                        if (err != nullptr && err->empty()) {
                            *err = "p1: fun body splice failed "
                                   "(site abandoned post-clone): " +
                                   berr;
                        }
                        return false;
                    }
                }

                fn.emit_jump(cb, fb.entry);
                cb->terminator->beam_idx = bi;
                fn.emit_jump(fb.ret_join, b_lat2);
                fb.ret_join->terminator->beam_idx = bi;

                /* The fun's return value replaces the CallFun's result
                 * everywhere (the latch accumulator copy included);
                 * re-point the readers' operand homes at its real home. */
                {
                    int32_t ret_home = T2_REG_NONE;
                    auto hit = fb.home.find(fb.ret_phi);

                    if (hit != fb.home.end()) {
                        ret_home = hit->second;
                    } else if (fb.ret_phi->def != nullptr) {
                        ret_home = fb.ret_phi->def->dst_reg;
                    }
                    replace_value(fn, ccf->result, fb.ret_phi);
                    for (T2Op *u : clones) {
                        if (u == ccf || u->operand_regs == nullptr) {
                            continue;
                        }
                        for (uint16_t i = 0; i < u->num_operands; i++) {
                            if (u->operands[i] == fb.ret_phi) {
                                u->operand_regs[i] = ret_home;
                            }
                        }
                    }
                    unlink_op(cb, ccf);
                }

                /* ---- loop phi inputs (before dead-copy reaping: the
                 *      latch materializations' uses must be visible) --- */

                for (uint32_t i = 0; i < ar; i++) {
                    if (i == fun_idx) {
                        continue;
                    }
                    fn.set_phi_inputs(cl_phis[i],
                                      {cmap->x[perm[i]], latch_vec[i]},
                                      {b_ok, b_lat2});
                }

                /* ---- reap dead cloned copies (the fun's save/restore
                 *      round-trip through the dropped frame, the arg
                 *      shuffle the splice bypassed) ---------------------- */

                {
                    bool again = true;

                    while (again) {
                        again = false;
                        for (T2Op *cl : clones) {
                            if (cl->kind != T2OpKind::Copy ||
                                cl->block == nullptr || cl->result == nullptr) {
                                continue;
                            }

                            uint32_t uses = 0;

                            for_each_op(fn, [&](T2Op *u) {
                                for (uint16_t i = 0; i < u->num_operands; i++) {
                                    if (u->operands[i] == cl->result) {
                                        uses++;
                                    }
                                }
                                if (u->sync != nullptr) {
                                    const T2SyncMap *m = u->sync;

                                    for (uint32_t i = 0; i < m->x_live; i++) {
                                        if (m->x[i] == cl->result) {
                                            uses++;
                                        }
                                    }
                                    for (int32_t i = 0;
                                         m->frame_size != T2_NO_FRAME &&
                                         i < m->frame_size;
                                         i++) {
                                        if (m->y[i] == cl->result) {
                                            uses++;
                                        }
                                    }
                                }
                            });
                            if (uses == 0) {
                                unlink_op(cl->block, cl);
                                cl->block = nullptr;
                                again = true;
                            }
                        }
                    }
                }

                /* ---- the frame is gone: nothing may touch Y ----------- */

                for (const T2Op *cl : clones) {
                    if (cl->block == nullptr) {
                        continue; /* reaped */
                    }
                    if (cl->dst_reg != T2_REG_NONE &&
                        t2_reg_is_y(cl->dst_reg)) {
                        return fail("p1: live Y write after frame drop");
                    }
                    if (cl->operand_regs != nullptr) {
                        for (uint16_t i = 0; i < cl->num_operands; i++) {
                            if (cl->operand_regs[i] != T2_REG_NONE &&
                                t2_reg_is_y(cl->operand_regs[i])) {
                                return fail("p1: live Y read after frame "
                                            "drop");
                            }
                        }
                    }
                }

                /* ---- speculation: re-dispatch class ------------------- */

                T2SyncMap *vec_map =
                        make_map(p1_outer_vec(cl_phis, cmap, fun_idx, ar, perm),
                                 cmap);
                bool need_acc_guard = false;

                /* Inner mode carries the terminal loop function's L_f
                 * on the guards (imm_int, the window-callee pattern):
                 * their side exits enter its body past the entry check
                 * — the back edge pre-charged this iteration's entry —
                 * so T1 re-executes the iteration inside the REAL loop
                 * function with T1-exact reductions and frames. The
                 * identity permutation (checked by the caller) makes
                 * the loop-carried vector the terminal function's own
                 * fresh-call vector, already pinned in X0..ar-1. */
                if (!convert_arith(fb,
                                   acc_idx >= 0 ? cl_phis[acc_idx]->result
                                                : nullptr,
                                   &need_acc_guard,
                                   T2_OP_SPEC_REDISPATCH,
                                   inner ? (Sint64)(UWord)inner_lf : 0,
                                   vec_map,
                                   bi)) {
                    if (err != nullptr) {
                        *err = "p1: fun arithmetic not convertible (site "
                               "abandoned post-clone)";
                    }
                    return false;
                }

                /* ---- back edge: RC_CALLEE (tail), +2 for the erased
                 *      fun call (fun entry + fun return dispatch).
                 *      Yield/tombstone re-enter the OUTERMOST original
                 *      callee over the outer-ordered continuation
                 *      vector (physically resident by the relabeled
                 *      homes) — the wrapper chain is a correct
                 *      continuation from any loop state. ------------- */

                crc->flags = T2_OP_RC_CALLEE | T2_OP_TAIL_SITE;
                crc->mfa_m = outer_m;
                crc->mfa_f = outer_f;
                crc->live = ar;
                crc->imm_int = (Sint64)(UWord)outer_lf;
                crc->index += 2;
                {
                    std::vector<T2Value *> crc_vec(ar, nullptr);

                    for (uint32_t i = 0; i < ar; i++) {
                        crc_vec[perm[i]] = latch_vec[i];
                    }
                    crc->sync = make_map(crc_vec, cmap);
                }

                /* ---- preheader: entry guard + the erased call's own
                 *      entry charge (RC#1) ------------------------------ */

                if (need_acc_guard && acc_idx >= 0) {
                    T2Op *g = fn.new_op(b_grd,
                                        T2OpKind::SpeculateType,
                                        T2Type::none());

                    fn.set_operands(g, {cmap->x[perm[acc_idx]]});
                    g->operand_regs = fn.arena.alloc_array<int32_t>(1);
                    g->operand_regs[0] = t2_xreg(perm[acc_idx]);
                    g->flags = T2_OP_INLINED | T2_OP_SPEC_REDISPATCH;
                    g->sync = cmap;
                    g->beam_idx = bi;
                }
                {
                    T2Op *rc1 = fn.new_op(b_grd,
                                          T2OpKind::ReductionCheck,
                                          T2Type::none());

                    fn.set_operands(rc1, {});
                    rc1->flags = T2_OP_RC_CALLEE | T2_OP_TAIL_SITE;
                    rc1->mfa_m = outer_m;
                    rc1->mfa_f = outer_f;
                    rc1->live = ar;
                    rc1->imm_int = (Sint64)(UWord)outer_lf;
                    /* charge 1 (the callee's entry check) + 1 per
                     * NON-peeling wrapper level: a peeling wrapper's
                     * erased fun call + loop handoff is exactly one
                     * loop iteration's charge, which the extra
                     * inlined iteration already pays. */
                    rc1->index = rc1_extra;
                    rc1->sync = cmap;
                    rc1->beam_idx = bi;
                }
                if (inner) {
                    /* Entry pre-screen (inner mode): a first-element
                     * structural miss must re-execute the OUTERMOST
                     * call — the generic wrapper chain examines the
                     * argument first and raises ITS shape (case_clause
                     * at the wrapper for a non-list) — so the boundary
                     * cons/nil test peels that case off before the
                     * loop. Past it, any in-loop structural miss is
                     * mid-list by construction and demotes into the
                     * terminal loop function, exactly where generic
                     * execution would be. The screen lives in its own
                     * blocks past b_grd: the entry stub's rc1 keeps
                     * its trailing jump (the yield-resume target), so
                     * a resume at the stub re-runs the screen over the
                     * restored boundary vector. */
                    T2BasicBlock *b_scr = fn.new_block();
                    T2BasicBlock *b_chk2 = fn.new_block();
                    T2Value *lv = cmap->x[perm[list_idx]];
                    int32_t lh = t2_xreg(perm[list_idx]);

                    fn.emit_jump(b_grd, b_scr);
                    b_grd->terminator->beam_idx = bi;
                    new_test_branch(b_scr,
                                    T2OpKind::IsNonemptyList,
                                    lv,
                                    lh,
                                    b_ok,
                                    b_chk2,
                                    bi);
                    new_test_branch(b_chk2,
                                    T2OpKind::IsNil,
                                    lv,
                                    lh,
                                    b_ok,
                                    b_err,
                                    bi);
                    fn.emit_jump(b_ok, bmap.at(ch));
                    b_ok->terminator->beam_idx = bi;
                } else {
                    fn.emit_jump(b_grd, bmap.at(ch));
                    b_grd->terminator->beam_idx = bi;
                }

                /* ---- the re-dispatch exit: a genuine tail call to the
                 *      generic callee. Fallback mode funnels every
                 *      error edge here over the loop-carried vector;
                 *      inner mode reaches it only from the entry
                 *      pre-screen, where the phis do not dominate — the
                 *      re-dispatch is the untouched call boundary
                 *      itself (nothing before the pre-screen writes
                 *      it). ------------------------------------------- */

                {
                    T2Op *rd = fn.new_op(b_err,
                                         T2OpKind::TailCallExt,
                                         T2Type::none());
                    std::vector<T2Value *> rvec;

                    if (inner) {
                        for (uint32_t i = 0; i < ar; i++) {
                            rvec.push_back(cmap->x[i]);
                        }
                    } else {
                        rvec = p1_outer_vec(cl_phis, cmap, fun_idx, ar, perm);
                    }
                    fn.set_operands(rd, rvec);
                    rd->operand_regs = fn.arena.alloc_array<int32_t>(ar);
                    for (uint32_t i = 0; i < ar; i++) {
                        rd->operand_regs[i] = t2_xreg(i);
                    }
                    rd->mfa_m = outer_m;
                    rd->mfa_f = outer_f;
                    rd->index = ar;
                    rd->sync = inner ? cmap : vec_map;
                    rd->beam_idx = bi;
                }

                /* ---- the inner demote (error edges, inner mode): a
                 *      mid-list structural miss transfers into the
                 *      terminal loop function's body over its own
                 *      fresh-call vector — the entry charge was paid by
                 *      the back edge, so T1 re-executes the iteration
                 *      and raises the byte-identical error (the
                 *      intrinsic tier's helper demote, at a TAIL site:
                 *      no CP push) ------------------------------------- */

                if (inner) {
                    T2Op *dm = fn.new_op(b_err_in,
                                         T2OpKind::DemoteCallee,
                                         T2Type::none());

                    fn.set_operands(dm, {});
                    dm->mfa_m = inner_m;
                    dm->mfa_f = inner_f;
                    dm->live = ar;
                    dm->imm_int = (Sint64)(UWord)inner_lf;
                    dm->flags = T2_OP_TAIL_SITE;
                    dm->sync = vec_map;
                    dm->beam_idx = bi;
                }

                /* ---- caller wiring ------------------------------------ */

                ASSERT(fn.blocks[call->block->id] == call->block &&
                       call->block->terminator == call);
                {
                    T2BasicBlock *b_pre = call->block;

                    b_pre->terminator = nullptr;
                    fn.emit_jump(b_pre, b_grd);
                    b_pre->terminator->beam_idx = bi;
                }

                /* Dependencies: EVERY module in the transitive chain
                 * (each one's structure is baked into this blob) and
                 * our own instance (the inlined fun body). */
                {
                    auto add_dep = [&](const void *hdr) {
                        for (const void *d : fn.dep_hdrs) {
                            if (d == hdr) {
                                return;
                            }
                        }
                        fn.dep_hdrs.push_back(hdr);
                    };
                    for (const void *hdr : chain_hdrs) {
                        add_dep(hdr);
                    }
                    add_dep(own_code_hdr);
                }

                if (p1_trace() || intrin_trace()) {
                    erts_fprintf(stderr,
                                 "t2_p1: %T:%T/%u inlined %T:%T/%u (tail "
                                 "site, fun arg %u, beam_idx %u, chain "
                                 "reds +%u)\n",
                                 fn.module,
                                 fn.function,
                                 (unsigned)fn.arity,
                                 callee.module,
                                 callee.function,
                                 (unsigned)ar,
                                 (unsigned)fun_idx,
                                 (unsigned)bi,
                                 (unsigned)rc1_extra);
                }

                changed = true;
                return true;
            }

            /* The loop-carried vector in OUTER argument order: the
             * phis (each physically homed at its outer position by
             * the relabeled clone), with the invariant fun slot
             * holding the caller's boundary fun value. */
            static std::vector<T2Value *> p1_outer_vec(
                    const std::vector<T2Op *> &cl_phis,
                    const T2SyncMap *cmap,
                    uint32_t fun_idx,
                    uint32_t ar,
                    const uint32_t *perm) {
                std::vector<T2Value *> v(ar, nullptr);

                for (uint32_t i = 0; i < ar; i++) {
                    v[perm[i]] = i == fun_idx ? cmap->x[perm[i]]
                                              : cl_phis[i]->result;
                }
                return v;
            }

            /* One P1 site: recognition + admission, then commit. A
             * `true` return with changed untouched means the site was
             * (silently or tracedly) left alone. */
            bool expand_p1_site(T2Op *call) {
                if (call->kind != T2OpKind::TailCallExt) {
                    p1_reject(call, "body site (P1a implements tail sites)");
                    return true;
                }
                if (call->sync == nullptr ||
                    call->sync->x_live != call->index || call->index == 0 ||
                    call->index > T2_P1_MAX_ARITY) {
                    p1_reject(call, "no call-shaped sync map");
                    return true;
                }
                if (call->mfa_m == fn.module) {
                    p1_reject(call,
                              "own-module callee (instance not "
                              "committed at load time)");
                    return true;
                }

                /* Only the frameless call_ext_only shape (mirror the
                 * maps tail-site rule: a fused call_ext_last's T1 CALL
                 * PC deallocates again). */
                {
                    const T2Op *last = call->block->ops_tail;

                    if (call->sync->frame_size != T2_NO_FRAME ||
                        (last != nullptr &&
                         last->kind == T2OpKind::Deallocate &&
                         last->beam_idx == call->beam_idx)) {
                        p1_reject(call,
                                  "call_ext_last shape (fused dealloc) "
                                  "unsupported");
                        return true;
                    }
                }

                /* A statically-known fun among the arguments: an
                 * env-free SSA-constant MakeFun of this module. */
                int32_t fun_idx = -1;
                const T2Op *mf = nullptr;

                for (uint16_t i = 0; i < call->num_operands; i++) {
                    const T2Value *root = resolve_copies(call->operands[i]);
                    const T2Op *def = root->def;

                    if (def != nullptr && def->kind == T2OpKind::MakeFun &&
                        def->live == 0 /* num_free */ && def->imm_int >= 0) {
                        fun_idx = (int32_t)i;
                        mf = def;
                        break;
                    }
                }
                if (mf == nullptr) {
                    p1_reject(call, "no literal env-free MakeFun argument");
                    return true;
                }
                if (ret->lambdas == NULL ||
                    mf->index >= (uint32_t)ret->lambda_count) {
                    p1_reject(call, "lambda out of range");
                    return true;
                }

                uint32_t fun_arity;
                {
                    const ErtsT2Lambda *lam = &ret->lambdas[mf->index];

                    fun_arity = (uint32_t)(lam->arity - lam->num_free);
                }
                {
                    uint32_t idx = (uint32_t)mf->imm_int;

                    if (idx >= (uint32_t)ret->function_count ||
                        (ret->eligible_bitmap[idx / 32] &
                         (((Uint32)1) << (idx % 32))) == 0) {
                        p1_reject(call, "fun impl not eligible");
                        return true;
                    }
                }

                /* Every deopt branches to the site's own T1 CALL PC. */
                if (erts_t2_pc_lookup_kind(ret,
                                           fn.fn_index,
                                           call->beam_idx,
                                           ERTS_T2_PC_CALL) == 0) {
                    p1_reject(call, "no CALL pctab entry");
                    return true;
                }

                /* The callee: loaded with retention, T1 entry resolved. */
                Module *cm =
                        erts_get_module(call->mfa_m, erts_active_code_ix());

                if (cm == nullptr || cm->curr.code_hdr == nullptr ||
                    cm->curr.t2_retained == nullptr) {
                    p1_reject(call, "callee module not loaded/retained");
                    return true;
                }

                const BeamCodeHeader *chdr =
                        (const BeamCodeHeader *)cm->curr.code_hdr;
                const ErtsT2RetainedCode *cret = cm->curr.t2_retained;
                const void *clf = find_lf(chdr, call->mfa_f, call->index);

                if (clf == nullptr) {
                    p1_reject(call, "callee T1 entry not found");
                    return true;
                }

                /* Pre-admit the fun body (mutation-free). */
                {
                    uint32_t impl_idx = (uint32_t)mf->imm_int;
                    bool admitted = false;
                    std::string berr;

                    if (!t2_build_selected(
                                ret,
                                &impl_idx,
                                1,
                                [&](T2Function &impl) {
                                    admitted =
                                            admit_fun(impl, fun_arity, false);
                                },
                                &berr)) {
                        return true; /* decode trouble: leave alone */
                    }
                    if (!admitted) {
                        p1_reject(call, "fun body not admissible");
                        return true;
                    }
                }

                /* ---- transitive descent (P1c-1) -------------------- *
                 * Build the callee's HIR (the retained-code path the
                 * debug BIFs use) and classify: a recoverable
                 * self-recursive loop commits — bound to the ORIGINAL
                 * call's boundary through the composed permutation —
                 * while a thin fun-passing wrapper exposes its tail
                 * call and the trigger re-runs on that target.
                 * Bounded depth, visited set; anything else stays
                 * generic. */
                uint32_t ar = call->index;
                Eterm cur_m = call->mfa_m;
                Eterm cur_f = call->mfa_f;
                const ErtsT2RetainedCode *cur_ret = cret;
                const BeamCodeHeader *cur_hdr = chdr;
                uint32_t perm[T2_P1_MAX_ARITY];
                uint32_t fun_pos = (uint32_t)fun_idx;
                uint32_t wrappers = 0;
                uint32_t peels = 0;
                P1Pending pend;
                std::vector<const void *> chain_hdrs{(const void *)chdr};
                std::vector<std::pair<Eterm, Eterm>> visited{{cur_m, cur_f}};

                for (uint32_t i = 0; i < ar; i++) {
                    perm[i] = i;
                }

                for (;;) {
                    bool committed = false;
                    bool hard_err = false;
                    bool wrapped = false;
                    bool tolerant = false;
                    P1Wrap wr;
                    const char *why = "callee build/admission failed";
                    std::string berr;

                    auto classify = [&](T2Function &callee) {
                        bool recovered = false;
                        std::string rerr;

                        if (!t2_loop_recover(callee, &recovered, &rerr)) {
                            why = "callee loop recovery failed";
                            return;
                        }
                        if (recovered) {
                            if (tolerant) {
                                /* A tolerant (partial) build is
                                 * classification-only: a cloned
                                 * loop must come from a fully
                                 * eligible build. */
                                why = "tolerant-built callee "
                                      "recovered a loop (commit "
                                      "requires full eligibility)";
                                return;
                            }
                            if (!t2_validate(callee, &rerr)) {
                                why = "callee invalid "
                                      "post-recovery";
                                return;
                            }

                            T2LoopInfo cli;

                            t2_loop_info(callee, &cli);

                            int32_t acc_idx = -1;

                            if (!p1_admit_callee(callee,
                                                 cli,
                                                 fun_pos,
                                                 fun_arity,
                                                 &acc_idx,
                                                 &why)) {
                                return;
                            }
                            if (wrappers > 0 &&
                                !p1_chain_congruent(callee,
                                                    cli.loops[0],
                                                    fun_pos,
                                                    acc_idx,
                                                    pend,
                                                    &why)) {
                                return;
                            }

                            /* Inner re-dispatch target: the TERMINAL
                             * loop function's own T1 entry, admissible
                             * only under the identity permutation (the
                             * loop-carried vector must BE its
                             * fresh-call vector, no move script). A
                             * reordering chain or an unresolved entry
                             * keeps the outermost re-dispatch. */
                            const void *inner_lf = nullptr;

                            if (!p1_inner_disabled()) {
                                bool ident = true;

                                for (uint32_t i = 0; i < ar; i++) {
                                    ident &= perm[i] == i;
                                }
                                if (ident) {
                                    inner_lf = find_lf(cur_hdr,
                                                       cur_f,
                                                       (unsigned)ar);
                                }
                                if (inner_lf == nullptr && p1_trace()) {
                                    erts_fprintf(stderr,
                                                 "t2_p1: %T:%T/%u inner "
                                                 "re-dispatch off for terminal "
                                                 "%T:%T/%u (%s), outermost "
                                                 "kept\n",
                                                 fn.module,
                                                 fn.function,
                                                 (unsigned)fn.arity,
                                                 cur_m,
                                                 cur_f,
                                                 (unsigned)ar,
                                                 ident ? "no T1 entry"
                                                       : "permuted chain");
                                }
                            }

                            if (!p1_commit(call,
                                           callee,
                                           cli.loops[0],
                                           fun_pos,
                                           acc_idx,
                                           mf,
                                           call->mfa_m,
                                           call->mfa_f,
                                           clf,
                                           cur_m,
                                           cur_f,
                                           inner_lf,
                                           chain_hdrs,
                                           perm,
                                           wrappers - peels)) {
                                hard_err = true;
                                return;
                            }
                            committed = true;
                            return;
                        }

                        /* Not a loop: try the wrapper shape. */
                        if (wrappers >= T2_P1_MAX_DEPTH) {
                            why = "wrapper chain exceeds the "
                                  "transitive depth bound";
                            return;
                        }
                        if (!p1_classify_wrapper(callee,
                                                 fun_pos,
                                                 fun_arity,
                                                 &wr,
                                                 &why)) {
                            return;
                        }
                        wrapped = true;
                    };

                    T2BuildStatus bst = t2_build_for_debug(cur_ret,
                                                           cur_f,
                                                           (unsigned)ar,
                                                           classify,
                                                           &berr);

                    if (bst == T2BuildStatus::NotEligible && !p1c2_disabled()) {
                        /* P1c-2: the chain callee is only PARTIALLY
                         * buildable (a multi-clause function whose
                         * sibling clauses use unsupported ops, e.g.
                         * 'Elixir.Enum':reduce/3's map/Range clauses
                         * next to its is_list fold clause). Retry with
                         * the tolerant build: unsupported regions
                         * degrade to Opaque leaves the wrapper walk
                         * treats as not-the-fast-path, subject to the
                         * same leaf-confinement congruence as flagged
                         * error exits. Wrapper classification only —
                         * the tolerant reject above keeps a recovered
                         * loop from committing. */
                        tolerant = true;
                        if (p1_trace()) {
                            erts_fprintf(stderr,
                                         "t2_p1: %T:%T/%u tolerant build "
                                         "of chain callee %T:%T/%u\n",
                                         fn.module,
                                         fn.function,
                                         (unsigned)fn.arity,
                                         cur_m,
                                         cur_f,
                                         (unsigned)ar);
                        }
                        bst = t2_build_for_p1(cur_ret,
                                              cur_f,
                                              (unsigned)ar,
                                              classify,
                                              &berr);
                    }

                    if (hard_err) {
                        if (p1_trace() && err != nullptr) {
                            erts_fprintf(stderr,
                                         "t2_p1: %T:%T/%u hard error: "
                                         "%s\n",
                                         fn.module,
                                         fn.function,
                                         (unsigned)fn.arity,
                                         err->c_str());
                        }
                        return false; /* *err set by p1_commit */
                    }
                    if (bst != T2BuildStatus::Ok) {
                        p1_reject(call,
                                  "chain callee not buildable "
                                  "(eligibility/retention)");
                        return true;
                    }
                    if (committed) {
                        return true;
                    }
                    if (!wrapped) {
                        p1_reject(call, why);
                        return true;
                    }

                    /* Descend into the exposed tail-call target. */
                    if (wr.arity != ar) {
                        p1_reject(call, "wrapper changes arity");
                        return true;
                    }
                    if (wr.m == fn.module) {
                        p1_reject(call,
                                  "own-module callee in the chain "
                                  "(instance not committed at load "
                                  "time)");
                        return true;
                    }
                    {
                        bool cycle = false;

                        for (const auto &v : visited) {
                            if (v.first == wr.m && v.second == wr.f) {
                                cycle = true;
                                break;
                            }
                        }
                        if (cycle) {
                            p1_reject(call, "wrapper chain cycles");
                            return true;
                        }
                    }
                    visited.emplace_back(wr.m, wr.f);

                    /* Remap the pending constraints into the next
                     * level's argument space, then merge this
                     * level's. */
                    uint32_t s_inv[T2_P1_MAX_ARITY] = {0};

                    for (uint32_t j = 0; j < ar; j++) {
                        s_inv[wr.src[j]] = j;
                    }
                    if (pend.peel) {
                        pend.peel_acc = (int32_t)s_inv[pend.peel_acc];
                        pend.peel_list = (int32_t)s_inv[pend.peel_list];
                    }
                    if (pend.nil_ret) {
                        pend.nil_cond = (int32_t)s_inv[pend.nil_cond];
                        pend.nil_ret_pos = (int32_t)s_inv[pend.nil_ret_pos];
                    }
                    pend.list_tested =
                            p1_remap_mask(pend.list_tested, wr.src, ar);
                    if (pend.has_leaves) {
                        pend.leaf_mask =
                                p1_remap_mask(pend.leaf_mask, wr.src, ar);
                    }

                    if (wr.peel) {
                        if (pend.peel && (pend.peel_acc != wr.peel_acc ||
                                          pend.peel_list != wr.peel_list ||
                                          pend.peel_hd_op != wr.peel_hd_op ||
                                          pend.peel_acc_op != wr.peel_acc_op)) {
                            p1_reject(call, "chain peels disagree");
                            return true;
                        }
                        pend.peel = true;
                        pend.peel_acc = wr.peel_acc;
                        pend.peel_list = wr.peel_list;
                        pend.peel_hd_op = wr.peel_hd_op;
                        pend.peel_acc_op = wr.peel_acc_op;
                    }
                    if (wr.nil_ret) {
                        if (pend.nil_ret &&
                            (pend.nil_cond != wr.nil_cond ||
                             pend.nil_ret_pos != wr.nil_ret_pos)) {
                            p1_reject(call, "chain nil exits disagree");
                            return true;
                        }
                        pend.nil_ret = true;
                        pend.nil_cond = wr.nil_cond;
                        pend.nil_ret_pos = wr.nil_ret_pos;
                    }
                    pend.list_tested |= wr.list_tested;
                    if (wr.has_leaves) {
                        pend.leaf_mask =
                                pend.has_leaves
                                        ? (pend.leaf_mask & wr.leaf_mask)
                                        : wr.leaf_mask;
                        pend.has_leaves = true;
                    }

                    /* Compose the outer permutation and track the
                     * fun through the chain. */
                    {
                        uint32_t np[T2_P1_MAX_ARITY];

                        for (uint32_t j = 0; j < ar; j++) {
                            np[j] = perm[wr.src[j]];
                        }
                        for (uint32_t j = 0; j < ar; j++) {
                            perm[j] = np[j];
                        }
                    }
                    fun_pos = wr.fun_pos;
                    wrappers++;
                    peels += wr.peel ? 1 : 0;

                    if (wr.m != cur_m) {
                        Module *nm =
                                erts_get_module(wr.m, erts_active_code_ix());

                        if (nm == nullptr || nm->curr.code_hdr == nullptr ||
                            nm->curr.t2_retained == nullptr) {
                            p1_reject(call,
                                      "chain module not "
                                      "loaded/retained");
                            return true;
                        }
                        cur_ret = nm->curr.t2_retained;
                        cur_hdr = (const BeamCodeHeader *)nm->curr.code_hdr;

                        const void *nh = (const void *)nm->curr.code_hdr;
                        bool have = false;

                        for (const void *h : chain_hdrs) {
                            if (h == nh) {
                                have = true;
                                break;
                            }
                        }
                        if (!have) {
                            chain_hdrs.push_back(nh);
                        }
                    }
                    cur_m = wr.m;
                    cur_f = wr.f;

                    if (p1_trace()) {
                        erts_fprintf(stderr,
                                     "t2_p1: %T:%T/%u descending into "
                                     "%T:%T/%u (depth %u%s)\n",
                                     fn.module,
                                     fn.function,
                                     (unsigned)fn.arity,
                                     cur_m,
                                     cur_f,
                                     (unsigned)ar,
                                     (unsigned)wrappers,
                                     wr.peel ? ", peel" : "");
                    }
                }
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
                std::vector<T2Op *> p1_sites;
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
                    } else if (t != nullptr &&
                               t->kind == T2OpKind::TailCallExt &&
                               t->flags == 0 && !p1_disabled()) {
                        /* P1a general trigger (tail sites): any
                         * cross-module tail call not claimed by the
                         * hand-coded recognizers; expand_p1_site does
                         * the structural screening. */
                        p1_sites.push_back(t);
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

                for (T2Op *site : p1_sites) {
                    if (!expand_p1_site(site)) {
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
