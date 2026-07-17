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
 *               to B_dm_iter's contract via WindowCallee-shape
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

        /* P1b (the local pre-chain): a LOCAL (call_only) tail site
         * whose own-module pass-through wrapper chain is peeled into
         * the caller, exposing the cross-module fold call the literal
         * fun actually reaches (pt-style apply_fold(F,L) ->
         * lists:foldl(F,0,L)). Its own kill switch on top of T2_NO_P1
         * so the committed P1a/P1c behavior stays unaffected by A/B
         * runs. */
        bool p1_local_disabled() {
            static const bool off = getenv("T2_NO_P1_LOCAL") != nullptr;
            return off;
        }

        /* P2 loop unboxing (tag elimination,
         * PLAN/T2FULL/census/opt_landscape.md P2): keep the
         * loop-carried accumulator / induction variable as RAW-IN-HOME
         * words (tag-cleared smalls, T2_OP_RAW_MODE) across the
         * expanded fold loops, tagging only at loop exits and — via
         * T2Op::raw_mask — on every deopt/yield cold path. T2_NO_P2
         * disables the whole rewrite INCLUDING the single-return
         * accumulator-induction fix in convert_arith that feeds it, so
         * a lever-off run reproduces the pre-P2 code byte-for-byte
         * (the exact same-binary A/B the gates require). */
        bool unbox_disabled() {
            static const bool off = getenv("T2_NO_P2") != nullptr;
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

        /* Is `v` provably an immediate (never a boxed pointer)? Used to
         * gate closure inlining: a captured free var rides into the
         * devirtualized loop through a reserved X slot that is NOT a GC
         * root, so a boxed capture would go stale across a GC/yield. Only
         * small integers, atoms, nil, and typed-small SSA values are
         * safe; ConstLiteral (boxed tuple/binary/bignum literals) and
         * anything not provably small is not. */
        bool value_is_immediate(const T2Value *v) {
            v = resolve_copies(v);
            const T2Op *def = v->def;

            if (def != nullptr) {
                switch (def->kind) {
                case T2OpKind::ConstInt:
                    return IS_SSMALL(def->imm_int);
                case T2OpKind::ConstAtom:
                case T2OpKind::ConstNil:
                    return true;
                default:
                    break;
                }
            }
            return t2_type_proves_small(v->type);
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
             * WindowCallee-shape with the helper L_f (re-execute the
             * iteration as a fresh helper call, no sync map); the
             * maps:fold expansion passes Callsite-shape with the
             * call-boundary sync map (re-execute the erased call). */
            bool convert_arith(FunBody &fb,
                               T2Value *acc_phi, /* null for arity-1 funs */
                               bool *need_acc_entry_guard,
                               uint32_t spec_flags,
                               T2DeoptShape spec_shape,
                               Sint64 spec_imm,
                               T2SyncMap *spec_sync,
                               uint32_t call_beam_idx) {
                std::unordered_set<const T2Value *> proven;

                *need_acc_entry_guard = false;

                /* Accumulator induction: legal iff every return value
                 * is a small constant or a converted arith result. */
                bool acc_inductive = acc_phi != nullptr;

                if (acc_phi != nullptr) {
                    const T2Op *rp = fb.ret_phi->def;

                    if (!unbox_disabled() && rp != nullptr &&
                        rp->kind != T2OpKind::Phi) {
                        /* Single-return fast path (splice_fun): ret_phi
                         * IS the return value, not a phi over the
                         * return values — so the induction test must
                         * examine ret_phi's own root, not the operands
                         * of whatever op computed it (a sum fun's
                         * Add(elem, acc) is inductive; its ELEM operand
                         * is irrelevant). Pre-P2 this mis-classified
                         * every single-return arith fun as
                         * non-inductive, forcing a per-iteration
                         * accumulator guard; gated behind the P2 lever
                         * so lever-off reproduces that code exactly. */
                        const T2Value *root = resolve_copies(fb.ret_phi);

                        acc_inductive = const_is_small(root) ||
                                        (root->def != nullptr &&
                                         (root->def->kind == T2OpKind::Add ||
                                          root->def->kind == T2OpKind::Sub) &&
                                         !fb.arith.empty());
                    } else {
                        /* fb.ret_phi's inputs are Copies of return
                         * values; check their roots. */
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
                        g->deopt_shape = spec_shape;
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
                    op->deopt_shape = spec_shape;
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

            /* ---- P2 loop unboxing (tag elimination) ------------------- *
             *
             * Rewrite one loop-carried accumulator to the RAW-IN-HOME
             * representation (tag-cleared small, T2_OP_RAW_MODE; see
             * t2_hir.hpp): the loop-entry seed untags once (UntagInt),
             * the converted flag-checked arith operates raw with the
             * bit-identical overflow deopt, and the value is re-tagged
             * only at loop exits (Copy -> TagInt) and — via
             * T2Op::raw_mask, consumed by the emitter's cold paths —
             * at every deopt/yield/demote boundary whose sync map
             * names it. Planned in FULL before anything mutates: any
             * use outside the closed set below aborts the plan and the
             * loop stays tagged (transparency dominates the win).     */

            struct UnboxPlan {
                std::unordered_set<const T2Value *> raw;
                std::vector<T2Op *> flip;   /* ops gaining RAW_MODE     */
                std::vector<T2Op *> to_tag; /* exit Copies -> TagInt    */
                std::vector<std::pair<T2Op *, uint32_t>> masks;
            };

            /* True when `op` is a deopt/yield/demote class whose
             * emission re-tags masked homes in its cold path. */
            static bool unbox_maskable(const T2Op *op) {
                switch (op->kind) {
                case T2OpKind::SpeculateType:
                case T2OpKind::AddSmall:
                case T2OpKind::SubSmall:
                case T2OpKind::ReductionCheck:
                case T2OpKind::DemoteCallee:
                case T2OpKind::FoldBudget:
                case T2OpKind::Call:
                case T2OpKind::CallExt:
                case T2OpKind::TailCall:
                case T2OpKind::TailCallExt:
                    return true;
                default:
                    return false;
                }
            }

            /* Plan the raw value set from the accumulator phi through
             * the converted arith chain (and the latch copies), then
             * verify every use of every raw value is accounted for.
             * Returns false (nothing mutated) when a use escapes. */
            bool plan_acc_unbox(T2Op *acc_phi,
                                const std::vector<T2Op *> &arith,
                                UnboxPlan *plan) {
                std::unordered_set<const T2Op *> arith_set(arith.begin(),
                                                           arith.end());
                std::unordered_set<const T2Op *> flip_set, tag_set;
                std::vector<const T2Value *> work;

                auto add_raw = [&](const T2Value *v) {
                    if (plan->raw.insert(v).second) {
                        work.push_back(v);
                    }
                };
                auto add_flip = [&](T2Op *o) {
                    if (flip_set.insert(o).second) {
                        plan->flip.push_back(o);
                    }
                };
                auto feeds_acc_phi = [&](const T2Value *v) {
                    for (uint16_t i = 0; i < acc_phi->num_operands; i++) {
                        if (acc_phi->operands[i] == v) {
                            return true;
                        }
                    }
                    return false;
                };

                add_raw(acc_phi->result);

                while (!work.empty()) {
                    const T2Value *v = work.back();
                    bool ok = true;

                    work.pop_back();
                    for_each_op(fn, [&](T2Op *op) {
                        if (!ok) {
                            return;
                        }

                        /* Operand uses. */
                        for (uint16_t i = 0; ok && i < op->num_operands; i++) {
                            if (op->operands[i] != v) {
                                continue;
                            }
                            if (op == acc_phi) {
                                continue; /* the phi itself */
                            }
                            switch (op->kind) {
                            case T2OpKind::AddSmall:
                            case T2OpKind::SubSmall:
                                /* Only the chain this plan owns. */
                                if (arith_set.count(op) != 0) {
                                    add_flip(op);
                                    add_raw(op->result);
                                } else {
                                    ok = false;
                                }
                                break;
                            case T2OpKind::Copy:
                                if (feeds_acc_phi(op->result)) {
                                    /* Latch materialization: a raw
                                     * move. */
                                    add_flip(op);
                                    add_raw(op->result);
                                } else if (tag_set.insert(op).second) {
                                    /* Exit copy: re-tag instead. */
                                    plan->to_tag.push_back(op);
                                }
                                break;
                            case T2OpKind::FoldBudget:
                                /* Reads the same value from either
                                 * representation. */
                                break;
                            case T2OpKind::Call:
                            case T2OpKind::CallExt:
                            case T2OpKind::TailCall:
                            case T2OpKind::TailCallExt:
                                /* The re-dispatch consumes the raw home
                                 * as a real argument; its emission
                                 * re-tags in place first. */
                                if (op->operand_regs == nullptr ||
                                    !t2_reg_is_x(op->operand_regs[i]) ||
                                    t2_reg_index(op->operand_regs[i]) >= 32) {
                                    ok = false;
                                } else {
                                    plan->masks.emplace_back(
                                            op,
                                            (uint32_t)1 << t2_reg_index(
                                                    op->operand_regs[i]));
                                }
                                break;
                            default:
                                ok = false; /* not raw-aware: abort */
                                break;
                            }
                        }

                        /* Sync-map mentions: the op's cold path must be
                         * able to re-tag the named home. */
                        if (ok && op->sync != nullptr) {
                            const T2SyncMap *m = op->sync;

                            for (uint32_t i = 0; ok && i < m->x_live; i++) {
                                if (m->x[i] != v) {
                                    continue;
                                }
                                if (i >= 32 || !unbox_maskable(op)) {
                                    ok = false;
                                } else {
                                    plan->masks.emplace_back(op,
                                                             (uint32_t)1 << i);
                                }
                            }
                            for (int32_t i = 0;
                                 ok && m->frame_size != T2_NO_FRAME &&
                                 i < m->frame_size;
                                 i++) {
                                if (m->y[i] == v) {
                                    ok = false; /* raw never in Y */
                                }
                            }
                        }
                    });
                    if (!ok) {
                        return false;
                    }
                }

                /* The latch input must have gone raw (the entry input
                 * is the caller's boundary value — it gets the
                 * UntagInt seed); exactly one of the two. */
                if (acc_phi->num_operands != 2) {
                    return false;
                }
                {
                    bool r0 = plan->raw.count(acc_phi->operands[0]) != 0;
                    bool r1 = plan->raw.count(acc_phi->operands[1]) != 0;

                    if (r0 == r1) {
                        return false;
                    }
                }
                return true;
            }

            /* Commit a successful plan: flags, exit re-tags, masks.
             * The caller wires the entry UntagInt seed itself (its
             * placement is template-specific). */
            void apply_acc_unbox(UnboxPlan &plan, T2Op *acc_phi) {
                acc_phi->flags |= T2_OP_RAW_MODE;
                for (T2Op *o : plan.flip) {
                    o->flags |= T2_OP_RAW_MODE;
                }
                for (T2Op *o : plan.to_tag) {
                    o->kind = T2OpKind::TagInt;
                }
                for (auto &mb : plan.masks) {
                    mb.first->raw_mask |= mb.second;
                }
            }

            /* The entry seed: an UntagInt of `entry_val` into the
             * accumulator home, appended to `b` (before its
             * terminator), replacing the phi's entry-edge input. */
            T2Value *unbox_entry_seed(T2Op *acc_phi,
                                      T2Value *entry_val,
                                      int32_t entry_home,
                                      T2BasicBlock *b,
                                      uint32_t bi) {
                T2Op *u = fn.new_op(b, T2OpKind::UntagInt, T2Type::any());

                fn.set_operands(u, {entry_val});
                u->operand_regs = fn.arena.alloc_array<int32_t>(1);
                u->operand_regs[0] = entry_home;
                u->dst_reg = acc_phi->dst_reg;
                u->flags = T2_OP_RAW_MODE;
                u->beam_idx = bi;

                for (uint16_t i = 0; i < acc_phi->num_operands; i++) {
                    if (acc_phi->operands[i] == entry_val) {
                        acc_phi->operands[i] = u->result;
                    }
                }
                return u->result;
            }

            /* ---- CFG surgery ------------------------------------------ */

            /* Split a block at a non-tail call: everything after the
             * call — the terminator included — moves to a fresh JOIN
             * block, phi edges from the split block retarget to it,
             * and the call is unlinked (its block pointer survives).
             * The caller owns re-terminating the split block and
             * re-homing the call's result into the join. */
            T2BasicBlock *split_at_call(T2Op *call) {
                T2BasicBlock *b_pre = call->block;
                T2BasicBlock *b_join = fn.new_block();
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
                return b_join;
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
                T2BasicBlock *b_join = split_at_call(call);

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
                /* Block-id range of the spliced fun body (the window-
                 * class deopt ops live there; P2 loop unboxing masks
                 * them). */
                size_t fbb0 = 0, fbb1 = 0;

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

                    fbb0 = fn.blocks.size();
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
                    fbb1 = fn.blocks.size();

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
                                   0,
                                   T2DeoptShape::WindowCallee,
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
                    g->flags = 0;
                    g->deopt_shape = T2DeoptShape::WindowCallee;
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

                /* ---- P2 loop unboxing: the accumulator ---------------- *
                 * The window-class deopts in the spliced body re-execute
                 * the iteration as a fresh helper call over X0..2 — a
                 * physical contract on the register file. With a raw
                 * accumulator in X1 each such op gets the fresh-call
                 * sync map (making the contract explicit for the
                 * validators) and the re-tag mask, so its trampoline
                 * restores the term before T1 runs. The demote maps,
                 * the RC#2 back edge and the exit copies are covered by
                 * the generic plan (masks / TagInt rewrites). */
                if (!unbox_disabled() && acc_phi_op != nullptr &&
                    need_acc_guard) {
                    UnboxPlan plan;

                    if (plan_acc_unbox(acc_phi_op, fb.arith, &plan)) {
                        T2SyncMap *wmap = make_map(
                                {fv, acc_phi_op->result, l_phi_op->result},
                                cmap);

                        for (size_t bx = fbb0; bx < fbb1; bx++) {
                            for (T2Op *op = fn.blocks[bx]->ops_head;
                                 op != nullptr;
                                 op = op->next) {
                                if (op->deopt_shape !=
                                    T2DeoptShape::WindowCallee) {
                                    continue;
                                }
                                switch (op->kind) {
                                case T2OpKind::SpeculateType:
                                case T2OpKind::AddSmall:
                                case T2OpKind::SubSmall:
                                    op->sync = wmap;
                                    op->raw_mask |= (uint32_t)1 << 1;
                                    break;
                                default:
                                    break;
                                }
                            }
                        }

                        apply_acc_unbox(plan, acc_phi_op);
                        unbox_entry_seed(acc_phi_op, a0, t2_xreg(1), b_grd, bi);
                        erts_t2_opt_stats.p2_acc_unboxed++;

                        if (intrin_trace()) {
                            erts_fprintf(stderr,
                                         "t2_intrinsics: %T:%T/%u foldl "
                                         "acc unboxed (beam_idx %u)\n",
                                         fn.module,
                                         fn.function,
                                         (unsigned)fn.arity,
                                         (unsigned)bi);
                        }
                    }
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

                /* A committed inlined fold site (this hand-coded lists
                 * recognizer only ever fires on CallExt — i.e. body /
                 * non-tail sites; the tail lists:foldl and pass-through
                 * wrapper sites commit via p1_commit). Bumps the shared
                 * P1SitesInlined counter exactly once, in parity with
                 * the p1_commit tail/wrapper path. */
                erts_t2_opt_stats.p1_sites_inlined++;

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
                if (p1_trace() || intrin_trace()) {
                    erts_fprintf(stderr,
                                 "t2_p1: %T:%T/%u inlined lists:%T/%u (%s "
                                 "site, beam_idx %u)\n",
                                 fn.module,
                                 fn.function,
                                 (unsigned)fn.arity,
                                 ce.wrapper_f,
                                 (unsigned)k.call_arity,
                                 call->kind == T2OpKind::TailCallExt ? "tail"
                                                                     : "body",
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
             * (ERTS_T2_PC_CALL, class Callsite-shape) from the
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
                    /* Split: everything after the call moves to b_join,
                     * the original terminator too (shared with the
                     * lists expansion and the P1 body path). */
                    b_join = split_at_call(call);
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
                    budget->flags = 0;
                    budget->deopt_shape = T2DeoptShape::Callsite;
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
                                   0,
                                   T2DeoptShape::Callsite,
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
                    g->flags = 0;
                    g->deopt_shape = T2DeoptShape::Callsite;
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
                    inc->flags = 0;
                    inc->deopt_shape = T2DeoptShape::Callsite;
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

                /* ---- P2 loop unboxing --------------------------------- *
                 * Every fast-path deopt in this template is callsite-
                 * class (restart the erased call from cmap, which never
                 * names the loop-carried values), so the loop-internal
                 * representation is free: no re-tag mask is ever needed
                 * here — a raw word in x3/x4/nx is invisible to T1 (the
                 * re-executed call's Live is 3, so neither T1 nor GC
                 * reads above x2).                                      *
                 *   - induction + bound (i @x4, n @nx): always raw —
                 *     template-owned, defensively checked against any
                 *     sync-map mention; the per-iteration bound check
                 *     becomes one raw compare and the element loads
                 *     index raw.
                 *   - accumulator (acc @x3): raw when the generic plan
                 *     admits it (inductive sum shape).                  */
                if (!unbox_disabled()) {
                    bool iv_clean = true;
                    const T2Value *ivs[] = {i_phi->result,
                                            i_next,
                                            i0,
                                            size_op->result};

                    for_each_op(fn, [&](T2Op *op) {
                        if (op->sync == nullptr) {
                            return;
                        }
                        for (uint32_t i = 0; i < op->sync->x_live; i++) {
                            for (const T2Value *v : ivs) {
                                iv_clean &= op->sync->x[i] != v;
                            }
                        }
                        for (int32_t i = 0;
                             op->sync->frame_size != T2_NO_FRAME &&
                             i < op->sync->frame_size;
                             i++) {
                            for (const T2Value *v : ivs) {
                                iv_clean &= op->sync->y[i] != v;
                            }
                        }
                    });
                    if (iv_clean) {
                        i_phi->flags |= T2_OP_RAW_MODE;
                        i0->def->flags |= T2_OP_RAW_MODE;
                        i_next->def->flags |= T2_OP_RAW_MODE;
                        size_op->flags |= T2_OP_RAW_MODE;
                        cmp->flags |= T2_OP_RAW_MODE;
                        kv->def->flags |= T2_OP_RAW_MODE;
                        vv->def->flags |= T2_OP_RAW_MODE;
                        erts_t2_opt_stats.p2_iv_unboxed++;
                    }

                    UnboxPlan plan;

                    if (need_acc_guard &&
                        plan_acc_unbox(acc_phi, fb.arith, &plan)) {
                        apply_acc_unbox(plan, acc_phi);
                        /* The loop-entry copy (a0 @x1 -> @x3) becomes
                         * the untag seed: same shape, tag cleared. The
                         * entry guard above proved a0 small. */
                        acc_in->def->kind = T2OpKind::UntagInt;
                        acc_in->def->flags |= T2_OP_RAW_MODE;
                        erts_t2_opt_stats.p2_acc_unboxed++;

                        if (intrin_trace()) {
                            erts_fprintf(stderr,
                                         "t2_intrinsics: %T:%T/%u maps:fold "
                                         "acc unboxed (beam_idx %u)\n",
                                         fn.module,
                                         fn.function,
                                         (unsigned)fn.arity,
                                         (unsigned)bi);
                        }
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
             * At a hot call (tail or body) to a cross-module M:F/A      *
             * where (a) some argument resolves to a literal env-free    *
             * MakeFun of this module and (b) M:F/A is a self-recursive  *
             * loop recoverable by t2_loop_recover, the callee's         *
             * recovered loop is CLONED into the caller, its params      *
             * bound to the call's boundary vector, the per-element      *
             * CallFun devirtualized by splicing the fun body, and the   *
             * callee's frame dropped. Deopt is the RE-DISPATCH shape    *
             * (Redispatch-shape; T2_OP_TAIL_SITE at a tail site):  *
             * every side exit re-invokes the generic callee with the    *
             * LOOP-CARRIED vector. Under the identity permutation the   *
             * mid-list exits land INSIDE the TERMINAL loop function —   *
             * guards enter its T1 body past the entry check (the back   *
             * edge pre-charged it; a body site's trampoline pushes the  *
             * site's T1 CONT as the CP the skipped prologue would have  *
             * pushed) and the error edges demote into it (DemoteCallee: *
             * no CP push at a tail site, CONT pushed at a body site) —  *
             * exactly where generic execution would be, with T1-exact   *
             * reductions and error shapes; a first-element structural   *
             * miss is pre-screened at the boundary and re-executes the  *
             * OUTERMOST original call, whose wrapper chain owns that    *
             * error's shape (T2_NO_P1_INNER reverts every exit to the   *
             * outermost form). The RC_CALLEE back edge's yield stays    *
             * in T2 and its tombstone/parked translation re-enters the  *
             * callee (timeslice never deopts). No shape guard and no    *
             * slow edge: the callee's own case dispatch is the loop     *
             * entry, and re-dispatch IS the generic fallback.           *
             *                                                           *
             * Site shapes. TAIL (call_ext_only): the cloned loop's      *
             * Return IS the caller's return, and the outermost          *
             * re-dispatch is a genuine TailCallExt. BODY (CallExt):     *
             * the ops after the call move to a JOIN block               *
             * (split_at_call, as the maps/lists expanders do); every    *
             * cloned Return materializes the fold result in X0 (the    *
             * call's dst) and jumps to the join, where a result phi     *
             * replaces the call's value; the outermost re-dispatch is   *
             * a resident CallExt over the loop-carried vector whose CP  *
             * is the site's T1 CONT — the callee returns into T1 and    *
             * the rest of the invocation runs there (demote-on-return,  *
             * the maps slow-edge shape), so its phi input is            *
             * structural, never taken at runtime.                       *
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

            /* ===================================================== *
             * P1b: the LOCAL pass-through pre-chain.                *
             *                                                       *
             * A LOCAL (call_only) tail site never specializes in    *
             * place — the wrapper's own fun argument is a variable  *
             * (`pt:apply_fold/2 site lists:foldl/3 rejected: no     *
             * literal MakeFun`). Instead, the SITE that passes the  *
             * literal fun peels the own-module wrapper: classify it *
             * as a straight guardless pass-through spine to exactly *
             * one tail call whose arguments are parameter           *
             * pass-throughs or immediate constants (arity MAY       *
             * change — the classic "seed the accumulator" shape),   *
             * compose the bindings across local levels, and treat   *
             * the first cross-module target as the site to          *
             * specialize. The boundary rewrite happens only at      *
             * commit time (transactional: a rejected site keeps its *
             * original local call, byte-identical behavior and      *
             * reductions).                                          *
             *                                                       *
             * Deopt discipline: the peeled site's exposed boundary  *
             * is WIDER than the T1 call instruction the imm==0      *
             * re-dispatch deopts branch to, so a peeled site        *
             * commits only in INNER re-dispatch mode (every in-loop *
             * side exit enters the terminal loop function's own     *
             * body) with a statically-small entry accumulator (the  *
             * conditional entry guard — the one remaining imm==0    *
             * deopt — is then never live at runtime: it tests a     *
             * compile-time small constant). Yield/tombstone/entry   *
             * pre-screen already target the EXPOSED callee's own T1 *
             * entry (outer_lf) or re-invoke it for real (b_err), so *
             * they are peel-agnostic.                               *
             * ===================================================== */

            /* One argument of the exposed call: a wrapper parameter
             * pass-through or an immediate constant the wrapper
             * materializes (ConstInt/ConstAtom/ConstNil only — a
             * ConstLiteral's term dies with the callee decode). */
            struct P1LocalArg {
                int32_t param = -1; /* >= 0: param index; -1: const */
                T2OpKind ckind = T2OpKind::ConstNil;
                Sint64 cint = 0;
                Eterm cterm = NIL;
            };

            struct P1LocalWrap {
                bool ext = false;
                Eterm m = NIL, f = NIL;
                uint32_t arity = 0;
                P1LocalArg args[T2_P1_MAX_ARITY];
            };

            /* Classify an own-module callee as a THIN LOCAL
             * PASS-THROUGH wrapper: a straight spine from the entry
             * to exactly one flag-free tail call whose arguments are
             * parameter pass-throughs or immediate constants. The
             * only admissible branch is an is_function test on the
             * fun parameter, statically discharged on the literal
             * fun (a wrong-arity test would raise for it: reject).
             * No CallFun (no peel), no frame, no returning path:
             * anything else stays generic. Read-only. */
            bool p1_classify_local_wrapper(const T2Function &callee,
                                           uint32_t fun_idx,
                                           uint32_t fun_arity,
                                           P1LocalWrap *out,
                                           const char **why) {
                *why = "local wrapper shape not admissible";

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
                        *why = "local wrapper params not canonical";
                        return false;
                    }
                    params[op->index] = op->result;
                }
                for (uint32_t i = 0; i < ar; i++) {
                    if (params[i] == nullptr) {
                        *why = "local wrapper params not canonical";
                        return false;
                    }
                }

                const T2BasicBlock *b = callee.blocks[0];
                const T2Op *tc = nullptr;
                uint32_t ops_seen = 0;
                uint32_t steps = 0;

                while (tc == nullptr) {
                    if (++steps > T2_P1_MAX_WRAPPER_OPS) {
                        *why = "local wrapper spine too long";
                        return false;
                    }
                    for (const T2Op *phi = b->phis_head; phi != nullptr;
                         phi = phi->next) {
                        if (phi->num_operands != 1) {
                            *why = "local wrapper has a real merge";
                            return false;
                        }
                    }

                    bool entry = b == callee.blocks[0];

                    for (const T2Op *op = b->ops_head; op != nullptr;
                         op = op->next) {
                        if (op->kind == T2OpKind::Param) {
                            if (!entry) {
                                *why = "local wrapper param outside the "
                                       "entry";
                                return false;
                            }
                            continue;
                        }
                        if ((op->flags & ~(uint16_t)T2_OP_PAIR_HEAD) != 0) {
                            *why = "local wrapper op carries flags";
                            return false;
                        }
                        switch (op->kind) {
                        case T2OpKind::Copy:
                        case T2OpKind::ConstInt:
                        case T2OpKind::ConstAtom:
                        case T2OpKind::ConstNil:
                            break;
                        case T2OpKind::IsFunction:
                            if (resolve_thru(op->operands[0]) !=
                                params[fun_idx]) {
                                *why = "local wrapper tests a non-fun "
                                       "value";
                                return false;
                            }
                            break;
                        default:
                            *why = "local wrapper body op not admissible";
                            return false;
                        }
                        if (++ops_seen > T2_P1_MAX_WRAPPER_OPS) {
                            *why = "local wrapper too large";
                            return false;
                        }
                    }

                    const T2Op *t = b->terminator;

                    if (t == nullptr) {
                        *why = "local wrapper block without terminator";
                        return false;
                    }
                    switch (t->kind) {
                    case T2OpKind::Jump:
                        b = t->succ_then;
                        break;
                    case T2OpKind::Branch: {
                        const T2Op *test = resolve_thru(t->operands[0])->def;

                        if (test == nullptr ||
                            test->kind != T2OpKind::IsFunction ||
                            resolve_thru(test->operands[0]) !=
                                    params[fun_idx] ||
                            test->index != fun_arity) {
                            *why = "local wrapper branch is not the "
                                   "statically discharged fun guard";
                            return false;
                        }
                        b = t->succ_then;
                        break;
                    }
                    case T2OpKind::TailCall:
                    case T2OpKind::TailCallExt:
                        if (t->flags != 0 || t->sync == nullptr) {
                            *why = "local wrapper tail call carries "
                                   "flags or no sync";
                            return false;
                        }
                        tc = t;
                        break;
                    default:
                        *why = "local wrapper is not a straight "
                               "pass-through spine";
                        return false;
                    }
                }

                uint32_t ar2 = tc->index;

                if (ar2 == 0 || ar2 > T2_P1_MAX_ARITY ||
                    tc->num_operands != (uint16_t)ar2) {
                    *why = "local wrapper target arity out of bounds";
                    return false;
                }

                int32_t fpos = -1;

                for (uint32_t j = 0; j < ar2; j++) {
                    const T2Value *root = resolve_thru(tc->operands[j]);
                    int32_t p = p1_param_index(params, root);

                    if (p >= 0) {
                        out->args[j].param = p;
                        if ((uint32_t)p == fun_idx) {
                            if (fpos >= 0) {
                                *why = "local wrapper passes the fun "
                                       "twice";
                                return false;
                            }
                            fpos = (int32_t)j;
                        }
                        continue;
                    }

                    const T2Op *def = root->def;

                    if (def != nullptr && (def->kind == T2OpKind::ConstInt ||
                                           def->kind == T2OpKind::ConstAtom ||
                                           def->kind == T2OpKind::ConstNil)) {
                        out->args[j].param = -1;
                        out->args[j].ckind = def->kind;
                        out->args[j].cint = def->imm_int;
                        out->args[j].cterm = def->imm_term;
                        continue;
                    }
                    *why = "local wrapper argument neither pass-through "
                           "nor immediate constant";
                    return false;
                }
                if (fpos < 0) {
                    *why = "fun not passed through the local wrapper";
                    return false;
                }

                out->ext = tc->kind == T2OpKind::TailCallExt;
                out->m = tc->mfa_m;
                out->f = tc->mfa_f;
                out->arity = ar2;
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
            /* The terminal loop's dispatch list param: the unique
             * param phi the loop's cons/nil tests name, or -1 when
             * none/ambiguous. Shared by the commit (the inner
             * re-dispatch's entry pre-screen tests it) and the local
             * pre-chain admission (which REQUIRES inner mode, so it
             * pre-checks the same condition before the boundary
             * rewrite). */
            static int32_t p1_unique_list_param(const T2Function &callee,
                                                const T2Loop &loop) {
                uint32_t ar = callee.arity;
                const T2BasicBlock *ce = callee.blocks[0];
                const T2BasicBlock *ch = callee.blocks[loop.header];
                std::vector<const T2Op *> phis(ar, nullptr);

                for (const T2Op *phi = ch->phis_head; phi != nullptr;
                     phi = phi->next) {
                    if (phi->dst_reg == T2_REG_NONE ||
                        !t2_reg_is_x(phi->dst_reg) ||
                        t2_reg_index(phi->dst_reg) >= ar) {
                        continue; /* admission excludes this */
                    }
                    phis[t2_reg_index(phi->dst_reg)] = phi;
                }

                int32_t list_idx = -1;

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
                            if (phis[i] == nullptr || tv != phis[i]->result) {
                                continue;
                            }
                            if (list_idx >= 0 && list_idx != (int32_t)i) {
                                return -1; /* ambiguous */
                            }
                            list_idx = (int32_t)i;
                        }
                    }
                }
                return list_idx;
            }

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
                const bool is_tail = call->kind == T2OpKind::TailCallExt;
                const uint16_t site_flag = is_tail ? T2_OP_TAIL_SITE : 0;

                /* BODY site: split the caller at the call up front —
                 * the ops after the call (the original terminator
                 * included) move to the JOIN block the cloned loop's
                 * exits and the re-dispatch edge jump into. */
                T2BasicBlock *b_pre = call->block;
                T2BasicBlock *b_join = is_tail ? nullptr : split_at_call(call);

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
                    list_idx = p1_unique_list_param(callee, loop);
                    if (list_idx < 0) {
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
                std::vector<T2Value *> ret_vals; /* per cloned Return    */
                std::vector<T2BasicBlock *> ret_blocks;
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
                        if (is_tail) {
                            T2Op *ret_op = fn.new_op(nb,
                                                     T2OpKind::Return,
                                                     T2Type::none());

                            fn.set_operands(ret_op, {rv});
                            ret_op->operand_regs =
                                    fn.arena.alloc_array<int32_t>(1);
                            ret_op->operand_regs[0] = t2_xreg(0);
                            ret_op->sync = make_map({rv}, cmap);
                            ret_op->beam_idx = bi;
                        } else {
                            /* BODY site: the fold result feeds the
                             * join's result phi instead of returning
                             * (the X0 pin below is the phi home). The
                             * exit edge charges the erased chain's
                             * final return — T1 pays one return
                             * dispatch more at a body site than at a
                             * tail site (the maps expander's constants
                             * document the same split); the deopt
                             * paths return through T1 and pay it
                             * there. */
                            T2Op *cr = fn.new_op(nb,
                                                 T2OpKind::ChargeReds,
                                                 T2Type::none());

                            fn.set_operands(cr, {});
                            cr->imm_int = 1;
                            cr->beam_idx = bi;
                            fn.emit_jump(nb, b_join);
                        }
                        ret_vals.push_back(rv);
                        ret_blocks.push_back(nb);
                        break;
                    }
                    default:
                        return fail("p1: callee terminator outside the "
                                    "admitted set");
                    }
                    nb->terminator->beam_idx = bi;
                }
                if (ccf == nullptr || crc == nullptr || ret_vals.empty()) {
                    return fail("p1: cloned CallFun/RC/Return vanished");
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
                /* Return-value materializations keep X0 — the return
                 * convention (tail) and the join phi's home = the
                 * call's dst (body) both pin x0 regardless of the
                 * permutation, so un-relabel them (exit-path only —
                 * the loop's phis never write x0's occupant
                 * mid-iteration). */
                for (const T2Value *rv : ret_vals) {
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
                /* (value, reserved-slot home) of each non-constant free
                 * var: a loop-entry SpeculateSmall guard (emitted at
                 * b_grd below) redispatches the whole site to the generic
                 * callee if the capture is not a small at runtime. */
                std::vector<std::pair<T2Value *, int32_t>> fv_guards;

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

                /* ---- closure free vars: the fun impl's trailing params *
                 * A closure impl has arity decl+num_free; the call_fun
                 * only supplied the decl value args above, so append the
                 * MakeFun's captured env values as the remaining params.
                 *
                 * They are loop-invariant caller values, but their home
                 * slots die AT the MakeFun — the compiler reuses the env
                 * source slot the instant it captures (verified: make_fun3
                 * reads AND writes the same x slot). So a captured value
                 * is NOT resident anywhere at the loop. Capture each
                 * non-constant free var with a Copy inserted BEFORE the
                 * MakeFun (where the source slot still holds it) into a
                 * fresh X slot placed above every slot the function uses:
                 * the loop never writes it, so it rides invariant across
                 * every iteration (the ReductionCheck is not an X-file
                 * clobber, so no frame home is required). A constant free
                 * var needs no slot. On deopt the generic call_fun
                 * re-reads the env from the fun value F — kept live by the
                 * re-dispatch vector — so free vars never enter a sync
                 * map.
                 *
                 * The reserved slot is above the sync-map live prefix, so
                 * the GC/yield walker never scans it: a BOXED capture
                 * would go stale across an in-loop GC. A loop-entry
                 * SpeculateSmall guard (b_grd, below) pins every
                 * non-constant capture to a small — an immediate, hence
                 * GC-invariant in any slot — and redispatches the site to
                 * the generic callee otherwise. Constant captures reached
                 * commit only if admission proved them immediate. */
                if (mf->live > 0) {
                    uint32_t rbase = next_free;

                    for_each_op(fn, [&](const T2Op *o) {
                        auto bump = [&](int32_t r) {
                            if (r != T2_REG_NONE && t2_reg_is_x(r) &&
                                t2_reg_index(r) + 1 > rbase) {
                                rbase = t2_reg_index(r) + 1;
                            }
                        };
                        bump(o->dst_reg);
                        if (o->operand_regs != nullptr) {
                            for (uint16_t i = 0; i < o->num_operands; i++) {
                                bump(o->operand_regs[i]);
                            }
                        }
                    });

                    T2Op *mfop = const_cast<T2Op *>(mf);
                    T2BasicBlock *mfb = mfop->block;

                    for (uint32_t k = 0; k < mf->live; k++) {
                        T2Value *fv = const_cast<T2Value *>(
                                resolve_copies(mf->operands[k]));

                        if (is_const_def(fv)) {
                            fargs.push_back(fv);
                            farg_homes.push_back(T2_REG_NONE);
                            continue;
                        }

                        int32_t dst_home = t2_xreg(rbase + k);
                        int32_t src_home =
                                mf->operand_regs != nullptr
                                        ? mf->operand_regs[k]
                                        : (mf->operands[k]->def != nullptr
                                                   ? mf->operands[k]->def->dst_reg
                                                   : T2_REG_NONE);
                        T2Op *cp = fn.new_op(mfb,
                                             T2OpKind::Copy,
                                             mf->operands[k]->type);

                        fn.set_operands(cp, {mf->operands[k]});
                        cp->dst_reg = dst_home;
                        cp->operand_regs = fn.arena.alloc_array<int32_t>(1);
                        cp->operand_regs[0] = src_home;
                        cp->beam_idx = mfop->beam_idx;

                        /* new_op appended at the block tail; relink it
                         * immediately before the MakeFun. */
                        unlink_op(mfb, cp);
                        cp->block = mfb;
                        cp->prev = mfop->prev;
                        cp->next = mfop;
                        if (mfop->prev != nullptr) {
                            mfop->prev->next = cp;
                        } else {
                            mfb->ops_head = cp;
                        }
                        mfop->prev = cp;

                        fargs.push_back(cp->result);
                        farg_homes.push_back(dst_home);
                        fv_guards.push_back({cp->result, dst_home});
                    }

                    next_free = rbase + mf->live;
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
                 *      shuffle the splice bypassed). Keep-set values
                 *      are pinned state (latch materializations, the
                 *      return values — a BODY site's result phi is not
                 *      built yet, so its inputs have no visible use
                 *      here) ------------------------------------------- */

                {
                    bool again = true;

                    while (again) {
                        again = false;
                        for (T2Op *cl : clones) {
                            if (cl->kind != T2OpKind::Copy ||
                                cl->block == nullptr || cl->result == nullptr ||
                                keep.count(cl->result) != 0) {
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
                 * function with T1-exact reductions and frames. At a
                 * BODY site (no T2_OP_TAIL_SITE) the deopt trampoline
                 * additionally pushes the site's T1 CONT as the CP the
                 * skipped callee prologue would have pushed
                 * (fill_spec_cont, t2_isel.cpp). The identity
                 * permutation (checked by the caller) makes the
                 * loop-carried vector the terminal function's own
                 * fresh-call vector, already pinned in X0..ar-1. */
                if (!convert_arith(fb,
                                   acc_idx >= 0 ? cl_phis[acc_idx]->result
                                                : nullptr,
                                   &need_acc_guard,
                                   site_flag,
                                   T2DeoptShape::Redispatch,
                                   inner ? (Sint64)(UWord)inner_lf : 0,
                                   vec_map,
                                   bi)) {
                    if (err != nullptr) {
                        *err = "p1: fun arithmetic not convertible (site "
                               "abandoned post-clone)";
                    }
                    return false;
                }

                /* ---- back edge: RC_CALLEE, +2 for the erased fun
                 *      call (fun entry + fun return dispatch).
                 *      Yield/tombstone re-enter the OUTERMOST original
                 *      callee over the outer-ordered continuation
                 *      vector (physically resident by the relabeled
                 *      homes) — the wrapper chain is a correct
                 *      continuation from any loop state. A BODY site's
                 *      tombstone demote pushes the site's T1 CONT
                 *      (isel fills it when T2_OP_TAIL_SITE is off). -- */

                crc->flags = T2_OP_RC_CALLEE | site_flag;
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
                    g->flags = T2_OP_INLINED | site_flag;
                    g->deopt_shape = T2DeoptShape::Redispatch;
                    g->sync = cmap;
                    g->beam_idx = bi;
                }

                /* Loop-entry small-guards for the non-constant closure
                 * captures: each rides the loop in a reserved slot that
                 * the GC/yield walker never scans, so it must be an
                 * immediate. SpeculateSmall pins it to a small (redispatch
                 * to the generic callee otherwise). A single entry check
                 * suffices — the capture is loop-invariant. */
                for (const auto &fvg : fv_guards) {
                    T2Op *g = fn.new_op(b_grd,
                                        T2OpKind::SpeculateType,
                                        T2Type::none());

                    fn.set_operands(g, {fvg.first});
                    g->operand_regs = fn.arena.alloc_array<int32_t>(1);
                    g->operand_regs[0] = fvg.second;
                    g->flags = T2_OP_INLINED | site_flag;
                    g->deopt_shape = T2DeoptShape::Redispatch;
                    g->sync = cmap;
                    g->beam_idx = bi;
                }
                {
                    T2Op *rc1 = fn.new_op(b_grd,
                                          T2OpKind::ReductionCheck,
                                          T2Type::none());

                    fn.set_operands(rc1, {});
                    rc1->flags = T2_OP_RC_CALLEE | site_flag;
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

                /* ---- the re-dispatch exit: the generic callee,
                 *      invoked for real. Fallback mode funnels every
                 *      error edge here over the loop-carried vector;
                 *      inner mode reaches it only from the entry
                 *      pre-screen, where the phis do not dominate — the
                 *      re-dispatch is the untouched call boundary
                 *      itself (nothing before the pre-screen writes
                 *      it). A TAIL site tail-transfers (a genuine
                 *      TailCallExt); a BODY site is a resident CallExt
                 *      whose CP is the site's T1 CONT — the callee
                 *      returns into T1 and the rest of the invocation
                 *      runs there (demote-on-return, the maps
                 *      slow-edge shape), so its edge into the join is
                 *      structural, never taken at runtime. ----------- */

                {
                    T2Op *rd = fn.new_op(
                            b_err,
                            is_tail ? T2OpKind::TailCallExt : T2OpKind::CallExt,
                            is_tail ? T2Type::none() : T2Type::any());
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

                    if (!is_tail) {
                        rd->live = ar;
                        rd->dst_reg = t2_xreg(0);
                        fn.emit_jump(b_err, b_join);
                        b_err->terminator->beam_idx = bi;

                        /* The join's result phi replaces the erased
                         * call's value: the loop's nil exits feed it
                         * in X0 (the call's dst, pinned above), the
                         * re-dispatch edge is the structural input. */
                        T2Op *res = fn.new_phi(b_join, T2Type::any());

                        res->dst_reg = t2_xreg(0);
                        res->beam_idx = bi;
                        ret_vals.push_back(rd->result);
                        ret_blocks.push_back(b_err);
                        fn.set_phi_inputs(res, ret_vals, ret_blocks);
                        replace_value(fn, call->result, res->result, res);
                    }
                }

                /* ---- the inner demote (error edges, inner mode): a
                 *      mid-list structural miss transfers into the
                 *      terminal loop function's body over its own
                 *      fresh-call vector — the entry charge was paid by
                 *      the back edge, so T1 re-executes the iteration
                 *      and raises the byte-identical error (the
                 *      intrinsic tier's helper demote: no CP push at a
                 *      TAIL site, the site's T1 CONT pushed at a BODY
                 *      site) -------------------------------------------- */

                if (inner) {
                    T2Op *dm = fn.new_op(b_err_in,
                                         T2OpKind::DemoteCallee,
                                         T2Type::none());

                    fn.set_operands(dm, {});
                    dm->mfa_m = inner_m;
                    dm->mfa_f = inner_f;
                    dm->live = ar;
                    dm->imm_int = (Sint64)(UWord)inner_lf;
                    dm->flags = site_flag;
                    dm->sync = vec_map;
                    dm->beam_idx = bi;
                }

                /* ---- caller wiring (a body site's b_pre lost its
                 *      terminator to the split) ------------------------ */

                if (is_tail) {
                    ASSERT(fn.blocks[b_pre->id] == b_pre &&
                           b_pre->terminator == call);
                    b_pre->terminator = nullptr;
                }
                fn.emit_jump(b_pre, b_grd);
                b_pre->terminator->beam_idx = bi;

                /* ---- P2 loop unboxing: the accumulator ---------------- *
                 * The generic plan covers everything here: the
                 * re-dispatch-class spec ops carry vec_map (which names
                 * the acc phi — masked), the RC_CALLEE back edge's map
                 * names the acc latch copy (masked; its yield stub
                 * re-tags and its resume re-clears), the inner demote /
                 * fallback re-dispatch carry vec_map (masked), and the
                 * cloned return copies become TagInt. The entry seed
                 * needs its own block on the entry edge: the preheader
                 * ends with rc1 — whose LIR contract requires the
                 * trailing back-jump, and whose yield RESUME re-enters
                 * through this edge, re-establishing the raw
                 * representation for free. */
                if (!unbox_disabled() && acc_idx >= 0 && need_acc_guard) {
                    UnboxPlan plan;
                    T2BasicBlock *hdr = bmap.at(ch);
                    T2BasicBlock *edge = b_ok; /* == b_grd in fallback */

                    if (edge->terminator != nullptr &&
                        edge->terminator->kind == T2OpKind::Jump &&
                        edge->terminator->succ_then == hdr &&
                        plan_acc_unbox(cl_phis[acc_idx], fb.arith, &plan)) {
                        T2BasicBlock *b_seed = fn.new_block();

                        edge->terminator->succ_then = b_seed;
                        fn.emit_jump(b_seed, hdr);
                        b_seed->terminator->beam_idx = bi;
                        for (T2Op *phi = hdr->phis_head; phi != nullptr;
                             phi = phi->next) {
                            for (uint16_t i = 0; i < phi->num_operands; i++) {
                                if (phi->phi_blocks[i] == edge) {
                                    phi->phi_blocks[i] = b_seed;
                                }
                            }
                        }

                        apply_acc_unbox(plan, cl_phis[acc_idx]);
                        unbox_entry_seed(cl_phis[acc_idx],
                                         cmap->x[perm[acc_idx]],
                                         t2_xreg(perm[acc_idx]),
                                         b_seed,
                                         bi);
                        erts_t2_opt_stats.p2_acc_unboxed++;

                        if (p1_trace() || intrin_trace()) {
                            erts_fprintf(stderr,
                                         "t2_p1: %T:%T/%u inlined loop "
                                         "acc unboxed (beam_idx %u)\n",
                                         fn.module,
                                         fn.function,
                                         (unsigned)fn.arity,
                                         (unsigned)bi);
                        }
                    }
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

                erts_t2_opt_stats.p1_sites_inlined++;

                if (p1_trace() || intrin_trace()) {
                    erts_fprintf(stderr,
                                 "t2_p1: %T:%T/%u inlined %T:%T/%u (%s "
                                 "site, fun arg %u, beam_idx %u, chain "
                                 "reds +%u)\n",
                                 fn.module,
                                 fn.function,
                                 (unsigned)fn.arity,
                                 callee.module,
                                 callee.function,
                                 (unsigned)ar,
                                 is_tail ? "tail" : "body",
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
                bool is_local = call->kind == T2OpKind::TailCall;
                bool is_tail = call->kind == T2OpKind::TailCallExt || is_local;

                if (!is_tail && call->kind != T2OpKind::CallExt) {
                    p1_reject(call, "not a call_ext/call_ext_only site");
                    return true;
                }
                if (call->sync == nullptr ||
                    call->sync->x_live != call->index || call->index == 0 ||
                    call->index > T2_P1_MAX_ARITY) {
                    p1_reject(call, "no call-shaped sync map");
                    return true;
                }
                if (is_local) {
                    /* P1b: a LOCAL (call_only) tail site is admissible
                     * only as a pass-through pre-chain head, and a
                     * peeled site commits only in inner re-dispatch
                     * mode (see the pre-chain block above). */
                    if (p1_local_disabled() || p1_inner_disabled()) {
                        p1_reject(call, "local pre-chain disabled");
                        return true;
                    }
                } else if (call->mfa_m == fn.module) {
                    p1_reject(call,
                              "own-module callee (instance not "
                              "committed at load time)");
                    return true;
                }

                /* Tail sites: only the frameless call_ext_only shape
                 * (mirror the maps tail-site rule: a fused
                 * call_ext_last's T1 CALL PC deallocates again). */
                if (is_tail) {
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
                 * SSA-constant MakeFun of this module. A closure
                 * (num_free > 0) is admitted too — its captured env
                 * values ride into the devirtualized loop as the fun
                 * impl's trailing params (p1_commit's free-var append). */
                int32_t fun_idx = -1;
                const T2Op *mf = nullptr;

                for (uint16_t i = 0; i < call->num_operands; i++) {
                    const T2Value *root = resolve_copies(call->operands[i]);
                    const T2Op *def = root->def;

                    if (def != nullptr && def->kind == T2OpKind::MakeFun &&
                        def->imm_int >= 0) {
                        fun_idx = (int32_t)i;
                        mf = def;
                        break;
                    }
                }
                if (mf == nullptr) {
                    p1_reject(call, "no literal SSA-constant MakeFun argument");
                    return true;
                }
                if (ret->lambdas == NULL ||
                    mf->index >= (uint32_t)ret->lambda_count) {
                    p1_reject(call, "lambda out of range");
                    return true;
                }

                /* Closure captures ride into the devirtualized loop
                 * through a reserved X slot that is not a GC root
                 * (p1_commit's free-var append), so a boxed capture would
                 * go stale across a GC/yield. A non-constant capture is
                 * admitted here and pinned to a small by a loop-entry
                 * SpeculateSmall guard at commit (redispatch otherwise) —
                 * a runtime immediate is GC-invariant in any slot. A
                 * CONSTANT capture cannot be guarded (isel forbids a
                 * constant guard operand), so it must be provably
                 * immediate at compile time; reject the site otherwise. */
                for (uint32_t k = 0; k < mf->live; k++) {
                    const T2Value *fv = resolve_copies(mf->operands[k]);

                    if (is_const_def(fv) && !value_is_immediate(fv)) {
                        p1_reject(call,
                                  "closure has a non-immediate constant "
                                  "capture");
                        return true;
                    }
                }

                uint32_t fun_arity;
                uint32_t impl_arity;
                {
                    const ErtsT2Lambda *lam = &ret->lambdas[mf->index];

                    fun_arity = (uint32_t)(lam->arity - lam->num_free);
                    impl_arity = (uint32_t)lam->arity;
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

                /* Every deopt branches to the site's own T1 CALL PC; a
                 * BODY site also needs the CONT — the CP its resident
                 * re-dispatch call, inner-mode side exits, tombstone
                 * demote and DemoteCallee push (mirror the maps
                 * expander's rule). */
                if (erts_t2_pc_lookup_kind(ret,
                                           fn.fn_index,
                                           call->beam_idx,
                                           ERTS_T2_PC_CALL) == 0) {
                    p1_reject(call, "no CALL pctab entry");
                    return true;
                }
                if (!is_tail && erts_t2_pc_lookup_kind(ret,
                                                       fn.fn_index,
                                                       call->beam_idx,
                                                       ERTS_T2_PC_CONT) == 0) {
                    p1_reject(call, "no CONT pctab entry");
                    return true;
                }

                /* ---- P1b: peel the LOCAL pass-through pre-chain ---- *
                 * Classify the chain of own-module wrappers behind a
                 * local tail site (built from our OWN retained code —
                 * the instance being compiled), compose the argument
                 * bindings, and expose the first cross-module target
                 * as the call to specialize. Classification only: the
                 * site is rewritten at commit time. */
                uint32_t tgt_ar = call->index;
                Eterm tgt_m = call->mfa_m;
                Eterm tgt_f = call->mfa_f;
                uint32_t tgt_fun_pos = (uint32_t)fun_idx;
                uint32_t pre_wrappers = 0;
                P1LocalArg pre[T2_P1_MAX_ARITY];

                for (uint32_t i = 0; i < tgt_ar; i++) {
                    pre[i].param = (int32_t)i;
                }

                if (is_local) {
                    /* uint64_t, not Eterm/UWord, as the template arg:
                     * GCC's -Werror=ignored-attributes rejects an
                     * attributed typedef here (Eterm carries alignment
                     * attributes); the values are only stored + compared
                     * by equality, so the plain integer is equivalent. */
                    std::vector<std::pair<uint64_t, uint32_t>> lvisited{
                            {tgt_f, tgt_ar}};

                    for (;;) {
                        if (pre_wrappers >= T2_P1_MAX_DEPTH) {
                            p1_reject(call,
                                      "local pre-chain exceeds the "
                                      "depth bound");
                            return true;
                        }

                        P1LocalWrap lw;
                        bool classified = false;
                        const char *lwhy = "local wrapper shape not "
                                           "admissible";
                        std::string lerr;
                        T2BuildStatus lbst = t2_build_for_debug(
                                ret,
                                tgt_f,
                                (unsigned)tgt_ar,
                                [&](T2Function &lc) {
                                    classified = p1_classify_local_wrapper(
                                            lc,
                                            tgt_fun_pos,
                                            fun_arity,
                                            &lw,
                                            &lwhy);
                                },
                                &lerr);

                        if (lbst != T2BuildStatus::Ok) {
                            p1_reject(call,
                                      "local wrapper not buildable "
                                      "(eligibility)");
                            return true;
                        }
                        if (!classified) {
                            p1_reject(call, lwhy);
                            return true;
                        }

                        /* Compose the wrapper's bindings over the
                         * site's. */
                        P1LocalArg next[T2_P1_MAX_ARITY];
                        int32_t nfun = -1;

                        for (uint32_t j = 0; j < lw.arity; j++) {
                            if (lw.args[j].param >= 0) {
                                if ((uint32_t)lw.args[j].param == tgt_fun_pos) {
                                    nfun = (int32_t)j;
                                }
                                next[j] = pre[lw.args[j].param];
                            } else {
                                next[j] = lw.args[j];
                            }
                        }
                        ASSERT(nfun >= 0); /* classifier required it */
                        for (uint32_t j = 0; j < lw.arity; j++) {
                            pre[j] = next[j];
                        }
                        tgt_fun_pos = (uint32_t)nfun;
                        tgt_m = lw.m;
                        tgt_f = lw.f;
                        tgt_ar = lw.arity;
                        pre_wrappers++;

                        if (p1_trace()) {
                            erts_fprintf(stderr,
                                         "t2_p1: %T:%T/%u local pre-chain "
                                         "exposes %T:%T/%u (depth %u)\n",
                                         fn.module,
                                         fn.function,
                                         (unsigned)fn.arity,
                                         tgt_m,
                                         tgt_f,
                                         (unsigned)tgt_ar,
                                         (unsigned)pre_wrappers);
                        }

                        if (lw.ext) {
                            break;
                        }

                        bool cycle = false;

                        for (const auto &v : lvisited) {
                            if (v.first == tgt_f && v.second == tgt_ar) {
                                cycle = true;
                                break;
                            }
                        }
                        if (cycle) {
                            p1_reject(call, "local pre-chain cycles");
                            return true;
                        }
                        lvisited.emplace_back(tgt_f, tgt_ar);
                    }

                    if (tgt_m == fn.module) {
                        p1_reject(call,
                                  "own-module callee after the local "
                                  "pre-chain (instance not committed "
                                  "at load time)");
                        return true;
                    }

                    /* The commit-time boundary rewrite copies each
                     * pass-through from its site home x_p to its
                     * exposed home x_j, emitted in DESCENDING j;
                     * p <= j keeps every pending source unclobbered
                     * (constants have no source). */
                    for (uint32_t j = 0; j < tgt_ar; j++) {
                        if (pre[j].param > (int32_t)j) {
                            p1_reject(call,
                                      "local pre-chain argument mapping "
                                      "not monotone");
                            return true;
                        }
                    }
                }

                /* The callee: loaded with retention, T1 entry resolved. */
                Module *cm = erts_get_module(tgt_m, erts_active_code_ix());

                if (cm == nullptr || cm->curr.code_hdr == nullptr ||
                    cm->curr.t2_retained == nullptr) {
                    p1_reject(call, "callee module not loaded/retained");
                    return true;
                }

                const BeamCodeHeader *chdr =
                        (const BeamCodeHeader *)cm->curr.code_hdr;
                const ErtsT2RetainedCode *cret = cm->curr.t2_retained;
                const void *clf = find_lf(chdr, tgt_f, tgt_ar);

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
                                    /* impl arity = decl + num_free: a
                                     * closure's captured env values are
                                     * trailing params of the built impl. */
                                    admitted =
                                            admit_fun(impl, impl_arity, false);
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
                uint32_t ar = tgt_ar;
                Eterm cur_m = tgt_m;
                Eterm cur_f = tgt_f;
                const ErtsT2RetainedCode *cur_ret = cret;
                const BeamCodeHeader *cur_hdr = chdr;
                uint32_t perm[T2_P1_MAX_ARITY];
                uint32_t fun_pos = tgt_fun_pos;
                uint32_t wrappers = 0;
                uint32_t peels = 0;
                P1Pending pend;
                std::vector<const void *> chain_hdrs{(const void *)chdr};
                /* uint64_t, not Eterm: -Werror=ignored-attributes (GCC)
                 * rejects the attributed typedef as a template arg. */
                std::vector<std::pair<uint64_t, uint64_t>> visited{
                        {cur_m, cur_f}};

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

                            if (pre_wrappers > 0) {
                                /* Peeled site: every imm==0
                                 * re-dispatch deopt would branch to
                                 * the T1 call instruction of the
                                 * NARROWER local call — commit only
                                 * in inner mode with a statically
                                 * small entry accumulator (see the
                                 * pre-chain block). */
                                if (inner_lf == nullptr ||
                                    p1_unique_list_param(callee, cli.loops[0]) <
                                            0) {
                                    why = "local pre-chain requires "
                                          "the inner re-dispatch";
                                    return;
                                }
                                if (acc_idx >= 0) {
                                    bool acc_small;

                                    if (pre[acc_idx].param < 0) {
                                        acc_small =
                                                pre[acc_idx].ckind ==
                                                        T2OpKind::ConstInt &&
                                                IS_SSMALL(pre[acc_idx].cint);
                                    } else {
                                        acc_small = const_is_small(resolve_copies(
                                                call->sync->x[pre[acc_idx]
                                                                      .param]));
                                    }
                                    if (!acc_small) {
                                        why = "local pre-chain entry "
                                              "accumulator not "
                                              "statically small";
                                        return;
                                    }
                                }

                                /* ---- the boundary rewrite -------- *
                                 * From here on the site is committed
                                 * to: any later failure is a hard
                                 * error and the whole build falls
                                 * back to T1 (never a rewritten-but-
                                 * generic site). Constants and moved
                                 * params materialize in their exposed
                                 * homes ahead of the (erased) call;
                                 * descending j keeps sources (p <= j)
                                 * unclobbered. */
                                T2BasicBlock *b_site = call->block;
                                T2SyncMap *scmap = call->sync;
                                std::vector<T2Value *> xs(ar, nullptr);

                                for (int32_t j = (int32_t)ar - 1; j >= 0; j--) {
                                    const P1LocalArg &a = pre[j];

                                    if (a.param >= 0) {
                                        T2Value *v = scmap->x[a.param];

                                        if (a.param == j) {
                                            xs[j] = v;
                                            continue;
                                        }

                                        T2Op *cp = fn.new_op(b_site,
                                                             T2OpKind::Copy,
                                                             v->type);

                                        fn.set_operands(cp, {v});
                                        cp->operand_regs =
                                                fn.arena.alloc_array<int32_t>(
                                                        1);
                                        cp->operand_regs[0] =
                                                t2_xreg((uint32_t)a.param);
                                        cp->dst_reg = t2_xreg((uint32_t)j);
                                        cp->flags = T2_OP_INLINED;
                                        cp->beam_idx = call->beam_idx;
                                        xs[j] = cp->result;
                                    } else {
                                        T2Value *cv;

                                        switch (a.ckind) {
                                        case T2OpKind::ConstInt:
                                            cv = fn.emit_const_int(b_site,
                                                                   a.cint);
                                            break;
                                        case T2OpKind::ConstAtom:
                                            cv = fn.emit_const_atom(b_site,
                                                                    a.cterm);
                                            break;
                                        default:
                                            cv = fn.emit_const_nil(b_site);
                                            break;
                                        }
                                        cv->def->dst_reg = t2_xreg((uint32_t)j);
                                        cv->def->flags = T2_OP_INLINED;
                                        cv->def->beam_idx = call->beam_idx;
                                        xs[j] = cv;
                                    }
                                }

                                call->kind = T2OpKind::TailCallExt;
                                call->mfa_m = tgt_m;
                                call->mfa_f = tgt_f;
                                call->index = ar;
                                call->live = ar;
                                fn.set_operands(call, xs);
                                call->operand_regs =
                                        fn.arena.alloc_array<int32_t>(ar);
                                for (uint32_t j = 0; j < ar; j++) {
                                    call->operand_regs[j] = t2_xreg(j);
                                }
                                call->sync = make_map(xs, scmap);
                            }

                            if (!p1_commit(call,
                                           callee,
                                           cli.loops[0],
                                           fun_pos,
                                           acc_idx,
                                           mf,
                                           tgt_m,
                                           tgt_f,
                                           clf,
                                           cur_m,
                                           cur_f,
                                           inner_lf,
                                           chain_hdrs,
                                           perm,
                                           wrappers - peels + pre_wrappers)) {
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

                        bool claimed = false;

                        if (op->mfa_m == am_lists_mod) {
                            for (const IntrinsicKind &k : t2_intrinsic_kinds) {
                                if (op->index == k.call_arity &&
                                    op->mfa_f ==
                                            erts_atom_put(
                                                    (const byte *)k.wrapper,
                                                    sys_strlen(k.wrapper),
                                                    ERTS_ATOM_ENC_LATIN1,
                                                    1)) {
                                    sites.push_back({op, &k});
                                    claimed = true;
                                    break;
                                }
                            }
                        }
                        if (!claimed && !p1_disabled()) {
                            /* P1 general trigger (body sites): any
                             * cross-module non-tail call not claimed by
                             * a hand-coded recognizer; expand_p1_site
                             * does the structural screening. */
                            p1_sites.push_back(op);
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
                               (t->kind == T2OpKind::TailCallExt ||
                                t->kind == T2OpKind::TailCall) &&
                               t->flags == 0 && !p1_disabled()) {
                        /* P1a general trigger (tail sites): any
                         * cross-module tail call not claimed by the
                         * hand-coded recognizers — or, P1b, a LOCAL
                         * (call_only) tail call heading a pass-through
                         * pre-chain; expand_p1_site does the
                         * structural screening. */
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
                    op->deopt_shape != T2DeoptShape::Boundary &&
                    op->deopt_shape != T2DeoptShape::WindowCallee &&
                    op->deopt_shape != T2DeoptShape::Callsite) {
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

    /* ------------------------------------------------------------------ *
     * P-C increment A1: verbatim xN unroll of the skip-count cursor-IV   *
     * scan loop (see t2_intrinsics.hpp; PLAN/T2FULL/14 §4)               *
     * ------------------------------------------------------------------ */

    namespace {

        /* The recognized loop (the P-B output for a `cnt/2`-class
         * skip-count scanner, post loop recovery, pre speculation):
         *
         *   H (header, 2 phis):  ctx_phi @Xc, acc_phi @Xa
         *                        v2 = start_match ctx_phi
         *                        ok = succeeded v2
         *                        branch ok, then: M, else: <err>
         *   M (match):           [pos = bs_get_position v2]   (optional)
         *                        lim = bs_limit v2   !raw
         *                        cur = bs_cursor v2  !raw
         *                        en  = bs_ensure cur, lim     imm = S
         *                        branch en, then: LATCH, else: <backtrack>
         *   LATCH:               cS   = const_int S
         *                        adv  = add_small cur, cS  !raw !no_ovf
         *                        bs_sync v2, adv
         *                        cC   = const_int C
         *                        acc' = add acc_phi, cC    sync={...}
         *                        ctx' = copy v2 @Xc
         *                        reduction_check sync={ctx',acc'}
         *                        jump H
         *
         * (The accumulator is still the GENERIC gc_bif Add at this
         * point in the pipeline — the speculation pass converts it,
         * and every verbatim clone of it, to the boundary-class
         * AddSmall afterwards.)
         *
         * Increment A2 additionally accepts the read-and-sum latch (a
         * `sumt/2`-class byte summer) — an optional read prefix ahead
         * of the advance, with the accumulator consuming the byte
         * instead of a constant:
         *
         *   LATCH:               base = bs_base v2  !raw
         *                        byte = bs_read base, cur   (imm = 8)
         *                        cS   = const_int S
         *                        adv  = add_small cur, cS  !raw !no_ovf
         *                        bs_sync v2, adv
         *                        acc' = add acc_phi, byte  sync={..byte..}
         *                        ...as above...
         *
         * The byte home may alias the (dead-in-latch) ctx phi home —
         * the accumulator's sync map then names the byte there, which
         * both the recognizer's walk and the transform's per-copy walk
         * carry in `content`, so the k-th cloned add's map names
         * byte_k: a deopt there redispatches the beam gc_bif '+' with
         * the very byte being added and .start already advanced past
         * it — deopt-correct exactly like the 1-wide loop.
         *
         * Anything else bails. */
        struct A1Shape {
            T2BasicBlock *h;
            T2BasicBlock *m;
            T2BasicBlock *latch;
            T2Op *ctx_phi;
            T2Op *acc_phi;
            T2Op *sm;      /* start_match           */
            T2Op *get_pos; /* optional; may be null */
            T2Op *limit;
            T2Op *cursor;
            T2Op *ensure;
            T2Op *base; /* A2 read prefix; may be null   */
            T2Op *read; /* A2 read prefix; may be null   */
            /* P-C L2 utf8 shape: the ASCII SpeculateRange guard that
             * sits between the BsRead and the advance (byte < 0x80);
             * null for the plain cnt/sumt single-latch shapes. When
             * set, the latch was SPLIT by one intermediate bs_ensure
             * (a bounds re-check subsumed by FC's N*stride ensure and
             * dropped in the fused FL) — see a1_recognize_utf8. */
            T2Op *spec;
            /* P-C L2: the intermediate (dropped) bs_ensure's mode bits
             * — its unit-8 divisibility check (the `R/binary` tail
             * byte-alignment requirement) is loop-invariant and must be
             * carried into FC's N*stride ensure, else the SWAR FL would
             * process 8 bytes of a bit-unaligned binary that T1 rejects
             * on iteration 1. 0 for the cnt/sumt single-latch shape. */
            Sint64 tail_ensure_index;
            T2Op *adv_const;
            T2Op *adv;
            T2Op *bsync;
            T2Op *acc_const; /* null when acc_is_read    */
            T2Op *acc;
            T2Op *ctx_copy;
            T2Op *rc;
            Sint64 stride;    /* bits */
            bool acc_is_read; /* acc rhs is the read result (A2) */
        };

        bool a1_bail(const T2Function &fn, const char *why) {
            if (getenv("T2_UNROLL_TRACE") != nullptr) {
                erts_fprintf(stderr,
                             "t2_unroll: %T:%T/%u bail: %s\n",
                             fn.module,
                             fn.function,
                             (unsigned)fn.arity,
                             why);
            }
            return false;
        }

        /* ---------------------------------------------------------------- *
         * P-C L2: recognize the utf8 count loop's TWO-block latch (the L1  *
         * ASCII fastpath topology). Fills the SAME A1Shape the single-     *
         * latch recognizer does, plus out->spec (the ASCII SpeculateRange  *
         * guard); the transform's SWAR-fused FL then reuses B1's roll-back *
         * and B2's wide load. Strict shape match; any mismatch bails,      *
         * leaving the loop on L1's 1-wide path (verbatim unroll is NOT a   *
         * fallback here — it would drop the per-byte ASCII guard).         *
         *                                                                  *
         *   H (header, 2 phis): ctx_phi, acc_phi; v=start_match; ok; ...   *
         *   M:  [bs_get_position] cursor=bs_cursor limit=bs_limit          *
         *       bs_ensure(cursor,limit); branch(L-a, backtrack)            *
         *   L-a: base=bs_base; byte=bs_read base,cursor;                   *
         *        speculate_range byte<0x80;   %% the ASCII guard           *
         *        adv=add_small cursor,S !raw; bs_sync ctx,adv;             *
         *        bs_limit; bs_cursor; bs_ensure;  %% INTERMEDIATE (dropped)*
         *        branch(L-b, backtrack)          %% same backtrack as M    *
         *   L-b (=latch): cC=const; acc'=add acc_phi,cC; ctx'=copy v;      *
         *        reduction_check; jump H                                   *
         *                                                                  *
         * The accumulator is a CONST increment only (count); a byte-read   *
         * into acc (sumc) stays on L1's 1-wide path — for utf8 the value   *
         * is the codepoint, which count/validate discard. Anything else    *
         * bails.                                                           *
         * ---------------------------------------------------------------- */
        bool a1_recognize_utf8(T2Function &fn,
                               const T2LoopInfo &li,
                               size_t idx,
                               A1Shape *out) {
            const T2Loop &loop = li.loops[idx];

            std::unordered_set<uint32_t> body(loop.body.begin(),
                                              loop.body.end());
            T2BasicBlock *h = fn.blocks[loop.header];
            T2BasicBlock *latch = fn.blocks[loop.latches[0]];

            /* --- header: {ctx, acc} phis + start_match + succeeded +
             *     branch(M, err) — identical to the single-latch shape. */
            T2Op *p0 = h->phis_head;

            if (p0 == nullptr || p0->next == nullptr ||
                p0->next->next != nullptr) {
                return a1_bail(fn, "utf8: header phi count != 2");
            }

            T2Op *p1 = p0->next;

            for (T2Op *p : {p0, p1}) {
                if (p->num_operands != 2 || p->phi_blocks == nullptr ||
                    p->dst_reg == T2_REG_NONE || !t2_reg_is_x(p->dst_reg) ||
                    (p->flags & T2_OP_RAW_MODE) != 0) {
                    return a1_bail(fn, "utf8: header phi shape");
                }
                bool from_pre = false, from_latch = false;

                for (uint16_t i = 0; i < 2; i++) {
                    from_pre |= p->phi_blocks[i]->id == loop.preheader;
                    from_latch |= p->phi_blocks[i] == latch;
                }
                if (!from_pre || !from_latch) {
                    return a1_bail(fn, "utf8: header phi edges");
                }
            }

            T2Op *sm = h->ops_head;

            if (sm == nullptr || sm->kind != T2OpKind::StartMatch ||
                sm->num_operands != 1 || sm->dst_reg == T2_REG_NONE ||
                !t2_reg_is_x(sm->dst_reg)) {
                return a1_bail(fn, "utf8: header start_match");
            }

            T2Op *ok = sm->next;

            if (ok == nullptr || ok->kind != T2OpKind::Succeeded ||
                ok->num_operands != 1 || ok->operands[0] != sm->result ||
                ok->next != nullptr) {
                return a1_bail(fn, "utf8: header succeeded");
            }

            const T2Op *hterm = h->terminator;

            if (hterm == nullptr || hterm->kind != T2OpKind::Branch ||
                hterm->num_operands != 1 || hterm->operands[0] != ok->result ||
                hterm->succ_then == nullptr || hterm->succ_else == nullptr) {
                return a1_bail(fn, "utf8: header branch");
            }

            T2BasicBlock *m = hterm->succ_then;

            if (body.count(m->id) == 0 || m == h || m == latch ||
                body.count(hterm->succ_else->id) != 0) {
                return a1_bail(fn, "utf8: header edges");
            }

            T2Op *ctx_phi;
            T2Op *acc_phi;

            if (sm->operands[0] == p0->result) {
                ctx_phi = p0;
                acc_phi = p1;
            } else if (sm->operands[0] == p1->result) {
                ctx_phi = p1;
                acc_phi = p0;
            } else {
                return a1_bail(fn,
                               "utf8: start_match operand is not a header phi");
            }

            /* A raw {cursor, limit} projection pair off sm (in EITHER
             * textual order — the utf8 op lowering emits cursor-first,
             * the bs_match lowering limit-first) followed by a BsEnsure
             * of (cursor, limit) with a whole-byte stride. Writes the
             * three ops and advances *after past the ensure. */
            auto match_cl = [&](T2Op *first,
                                bool is_read_bound,
                                T2Op **cursor_out,
                                T2Op **limit_out,
                                T2Op **ensure_out,
                                T2Op **after) -> bool {
                T2Op *a = first;

                if (a == nullptr || (a->kind != T2OpKind::BsLimit &&
                                     a->kind != T2OpKind::BsCursor)) {
                    return false;
                }
                T2Op *b = a->next;

                if (b == nullptr ||
                    (b->kind != T2OpKind::BsLimit &&
                     b->kind != T2OpKind::BsCursor) ||
                    b->kind == a->kind) {
                    return false;
                }
                T2Op *cursor = a->kind == T2OpKind::BsCursor ? a : b;
                T2Op *limit = a->kind == T2OpKind::BsLimit ? a : b;

                for (T2Op *pr : {cursor, limit}) {
                    if (pr->num_operands != 1 ||
                        pr->operands[0] != sm->result ||
                        (pr->flags & T2_OP_RAW_MODE) == 0 ||
                        pr->dst_reg == T2_REG_NONE ||
                        !t2_reg_is_x(pr->dst_reg)) {
                        return false;
                    }
                }
                if (cursor->dst_reg == limit->dst_reg) {
                    return false;
                }

                T2Op *en = b->next;

                if (en == nullptr || en->kind != T2OpKind::BsEnsure ||
                    en->num_operands != 2 ||
                    en->operands[0] != cursor->result ||
                    en->operands[1] != limit->result) {
                    return false;
                }
                /* M's read bound sets the per-byte stride (need == the
                 * read size, a whole byte count). The intermediate
                 * bounds re-check that SPLIT the latch is a vacuous
                 * `ensure at least 0 bits` (the `R/binary` tail match's
                 * always-true check, whose real exit is M's read
                 * bound): its need is dropped in the fused FL — the
                 * FC's N*stride ensure subsumes it — so any non-negative
                 * whole-byte need is fine there. */
                if (is_read_bound) {
                    if (en->imm_int <= 0 || en->imm_int % 8 != 0) {
                        return false;
                    }
                } else {
                    if (en->imm_int < 0 || en->imm_int % 8 != 0) {
                        return false;
                    }
                }
                *cursor_out = cursor;
                *limit_out = limit;
                *ensure_out = en;
                *after = en->next;
                return true;
            };

            /* --- M: [bs_get_position] {cursor,limit} bs_ensure +
             *     branch(L-a, backtrack) ----------------------------- */
            if (m->phis_head != nullptr) {
                return a1_bail(fn, "utf8: match block has phis");
            }

            T2Op *op = m->ops_head;
            T2Op *get_pos = nullptr;

            if (op != nullptr && op->kind == T2OpKind::BsGetPosition) {
                if (op->num_operands != 1 || op->operands[0] != sm->result ||
                    op->dst_reg == T2_REG_NONE || !t2_reg_is_x(op->dst_reg)) {
                    return a1_bail(fn, "utf8: bs_get_position shape");
                }
                get_pos = op;
                op = op->next;
            }

            T2Op *cursor;
            T2Op *limit;
            T2Op *ensure;
            T2Op *after;

            if (!match_cl(op,
                          /*is_read_bound=*/true,
                          &cursor,
                          &limit,
                          &ensure,
                          &after)) {
                return a1_bail(fn, "utf8: match cursor/limit/ensure");
            }
            if (after != nullptr) {
                return a1_bail(fn, "utf8: extra op after match ensure");
            }

            const T2Op *mterm = m->terminator;
            T2BasicBlock *la = mterm != nullptr ? mterm->succ_then : nullptr;
            T2BasicBlock *backtrack =
                    mterm != nullptr ? mterm->succ_else : nullptr;

            if (mterm == nullptr || mterm->kind != T2OpKind::Branch ||
                mterm->num_operands != 1 ||
                mterm->operands[0] != ensure->result || la == nullptr ||
                backtrack == nullptr || body.count(la->id) == 0 || la == h ||
                la == m || la == latch || body.count(backtrack->id) != 0) {
                return a1_bail(fn, "utf8: match branch");
            }

            /* --- L-a: base, read, speculate_range, advance, bs_sync,
             *     intermediate bs_ensure + branch(L-b, backtrack) ---- */
            if (la->phis_head != nullptr) {
                return a1_bail(fn, "utf8: L-a has phis");
            }

            T2Op *base = la->ops_head;

            if (base == nullptr || base->kind != T2OpKind::BsBase ||
                base->num_operands != 1 || base->operands[0] != sm->result ||
                (base->flags & T2_OP_RAW_MODE) == 0 ||
                base->dst_reg == T2_REG_NONE || !t2_reg_is_x(base->dst_reg) ||
                base->sync != nullptr) {
                return a1_bail(fn, "utf8: bs_base shape");
            }

            T2Op *read = base->next;

            if (read == nullptr || read->kind != T2OpKind::BsRead ||
                read->num_operands != 2 || read->operands[0] != base->result ||
                read->operands[1] != cursor->result || read->imm_int != 8 ||
                read->flags != 0 || read->raw_mask != 0 ||
                read->sync != nullptr || read->dst_reg == T2_REG_NONE ||
                !t2_reg_is_x(read->dst_reg)) {
                return a1_bail(fn, "utf8: bs_read shape");
            }

            T2Op *spec = read->next;

            if (spec == nullptr || spec->kind != T2OpKind::SpeculateRange ||
                spec->num_operands != 1 || spec->operands[0] != read->result ||
                spec->deopt_shape != T2DeoptShape::Boundary ||
                spec->sync == nullptr ||
                spec->sync->frame_size != T2_NO_FRAME) {
                return a1_bail(fn, "utf8: ascii speculate_range guard");
            }

            T2Op *c1 = spec->next;

            if (c1 == nullptr || c1->kind != T2OpKind::ConstInt ||
                c1->imm_int != ensure->imm_int) {
                return a1_bail(fn, "utf8: advance constant");
            }

            T2Op *adv = c1->next;

            if (adv == nullptr || adv->kind != T2OpKind::AddSmall ||
                adv->num_operands != 2 || adv->operands[0] != cursor->result ||
                adv->operands[1] != c1->result ||
                (adv->flags & T2_OP_RAW_MODE) == 0 ||
                (adv->flags & T2_OP_NO_OVF) == 0 ||
                adv->dst_reg != cursor->dst_reg || adv->sync != nullptr) {
                return a1_bail(fn, "utf8: cursor advance");
            }

            T2Op *bsync = adv->next;

            if (bsync == nullptr || bsync->kind != T2OpKind::BsSync ||
                bsync->num_operands != 2 || bsync->operands[0] != sm->result ||
                bsync->operands[1] != adv->result) {
                return a1_bail(fn, "utf8: bs_sync");
            }

            /* The intermediate bs_ensure that split the latch: a pure
             * bounds RE-check of the SAME context (cursor/limit
             * projections off sm), subsumed by FC's N*stride ensure and
             * DROPPED in the fused FL. Its then-edge is L-b, its
             * else-edge the SAME backtrack as M's ensure — nothing else
             * (any extra op after it bails). */
            T2Op *cursor2;
            T2Op *limit2;
            T2Op *ensure2;
            T2Op *after2;

            if (!match_cl(bsync->next,
                          /*is_read_bound=*/false,
                          &cursor2,
                          &limit2,
                          &ensure2,
                          &after2)) {
                return a1_bail(fn, "utf8: intermediate cursor/limit/ensure");
            }
            if (after2 != nullptr) {
                return a1_bail(fn, "utf8: extra op after intermediate ensure");
            }
            /* The intermediate ensure must be a plain AT-LEAST bounds
             * check, optionally with the unit-8 divisibility bit (bit 1
             * = the `R/binary` tail byte-alignment requirement). That
             * bit is loop-invariant and MUST be carried into the fused
             * FC (else the SWAR FL would consume 8 bytes of a bit-
             * unaligned binary T1 rejects on iteration 1). An exactly-
             * mode (bit 0) or any other mode has divergent branch
             * semantics we cannot fold — bail. */
            if (ensure2->index != 0 && ensure2->index != 2) {
                return a1_bail(fn,
                               "utf8: intermediate ensure mode "
                               "(not a plain at-least / unit-8 "
                               "bounds re-check)");
            }

            const T2Op *laterm = la->terminator;

            if (laterm == nullptr || laterm->kind != T2OpKind::Branch ||
                laterm->num_operands != 1 ||
                laterm->operands[0] != ensure2->result ||
                laterm->succ_then != latch || laterm->succ_else != backtrack) {
                return a1_bail(fn,
                               "utf8: L-a branch (not L-b / same backtrack)");
            }

            /* --- L-b (the back-edge latch): const, acc add, ctx copy,
             *     reduction_check, jump(H). Const increment only (count)
             *     — a byte-read into acc (sumc) bails to L1's 1-wide. -- */
            if (latch->phis_head != nullptr) {
                return a1_bail(fn, "utf8: latch has phis");
            }

            T2Op *c2 = latch->ops_head;

            if (c2 == nullptr || c2->kind != T2OpKind::ConstInt ||
                !IS_SSMALL(c2->imm_int)) {
                return a1_bail(fn, "utf8: latch accumulator constant");
            }

            T2Op *acc = c2->next;

            if (acc == nullptr || acc->kind != T2OpKind::Add ||
                acc->num_operands != 2 || acc->operands[0] != acc_phi->result ||
                acc->operands[1] != c2->result || acc->sync == nullptr ||
                acc->sync->frame_size != T2_NO_FRAME || acc->raw_mask != 0 ||
                acc->flags != 0 || acc->dst_reg != acc_phi->dst_reg) {
                return a1_bail(fn, "utf8: latch accumulator (const increment)");
            }

            T2Op *cpy = acc->next;

            if (cpy == nullptr || cpy->kind != T2OpKind::Copy ||
                cpy->num_operands != 1 || cpy->operands[0] != sm->result ||
                cpy->flags != 0 || cpy->dst_reg != ctx_phi->dst_reg) {
                return a1_bail(fn, "utf8: latch context copy");
            }

            T2Op *rc = cpy->next;

            if (rc == nullptr || rc->kind != T2OpKind::ReductionCheck ||
                rc->num_operands != 0 || rc->flags != 0 || rc->raw_mask != 0 ||
                rc->sync == nullptr || rc->sync->frame_size != T2_NO_FRAME ||
                rc->next != nullptr) {
                return a1_bail(fn, "utf8: latch reduction_check");
            }

            const T2Op *lterm = latch->terminator;

            if (lterm == nullptr || lterm->kind != T2OpKind::Jump ||
                lterm->succ_then != h) {
                return a1_bail(fn, "utf8: latch back edge");
            }

            /* --- phi threading: the latch edge carries {ctx', acc'} - */
            for (uint16_t i = 0; i < 2; i++) {
                if (ctx_phi->phi_blocks[i] == latch &&
                    ctx_phi->operands[i] != cpy->result) {
                    return a1_bail(fn, "utf8: ctx phi latch input");
                }
                if (acc_phi->phi_blocks[i] == latch &&
                    acc_phi->operands[i] != acc->result) {
                    return a1_bail(fn, "utf8: acc phi latch input");
                }
            }

            /* --- home disjointness (same contract as the single-latch
             *     shape: FC re-materializes the raw cursor/limit/base
             *     temps, so their homes must not alias the three term
             *     homes the clone maps rebuild from). ----------------- */
            const int32_t seed_homes[3] = {ctx_phi->dst_reg,
                                           acc_phi->dst_reg,
                                           sm->dst_reg};

            if (ctx_phi->dst_reg == acc_phi->dst_reg) {
                return a1_bail(fn, "utf8: phi home collision");
            }
            for (int32_t home : seed_homes) {
                if (cursor->dst_reg == home || limit->dst_reg == home ||
                    base->dst_reg == home) {
                    return a1_bail(fn,
                                   "utf8: raw temp home aliases a term home");
                }
            }
            if (cursor->dst_reg == limit->dst_reg ||
                cursor->dst_reg == base->dst_reg ||
                limit->dst_reg == base->dst_reg) {
                return a1_bail(fn, "utf8: cursor/limit/base home collision");
            }
            /* The read (the byte the guard consumes) may write the dead
             * ctx phi home — its value is not carried into the fused FL
             * (which does ONE wide load, no per-byte read) — but never a
             * live term/temp home. */
            if (read->dst_reg == acc_phi->dst_reg ||
                read->dst_reg == sm->dst_reg ||
                read->dst_reg == cursor->dst_reg ||
                read->dst_reg == limit->dst_reg ||
                read->dst_reg == base->dst_reg) {
                return a1_bail(fn, "utf8: bs_read home aliases a live home");
            }

            /* --- sync-map contents (same walk as the single-latch
             *     shape; the intermediate ensure/2nd projections are
             *     dropped, so only the guard's, ACC's and reduction_
             *     check's maps are walked). --------------------------- */
            std::unordered_map<int32_t, const T2Value *> content;

            content[ctx_phi->dst_reg] = ctx_phi->result;
            content[acc_phi->dst_reg] = acc_phi->result;
            content[sm->dst_reg] = sm->result;
            if (get_pos != nullptr) {
                content[get_pos->dst_reg] = get_pos->result;
            }
            /* The read executes (feeding the guard) before the acc add;
             * its home holds the byte when the acc's map is taken. */
            content[read->dst_reg] = read->result;

            auto maps_walked = [&](const T2SyncMap *map) -> bool {
                for (uint32_t i = 0; i < map->x_live; i++) {
                    int32_t r = t2_xreg(i);
                    auto it = content.find(r);

                    if (it == content.end() || it->second != map->x[i]) {
                        return false;
                    }
                    if (r != seed_homes[0] && r != seed_homes[1] &&
                        r != seed_homes[2] && r != read->dst_reg) {
                        return false;
                    }
                }
                return true;
            };

            if (!maps_walked(spec->sync)) {
                return a1_bail(fn, "utf8: speculate_range sync map contents");
            }
            if (!maps_walked(acc->sync)) {
                return a1_bail(fn, "utf8: accumulator sync map contents");
            }
            content[acc->dst_reg] = acc->result;
            content[cpy->dst_reg] = cpy->result;
            if (!maps_walked(rc->sync)) {
                return a1_bail(fn, "utf8: reduction_check sync map contents");
            }

            out->h = h;
            out->m = m;
            out->latch = latch;
            out->ctx_phi = ctx_phi;
            out->acc_phi = acc_phi;
            out->sm = sm;
            out->get_pos = get_pos;
            out->limit = limit;
            out->cursor = cursor;
            out->ensure = ensure;
            out->base = base;
            out->read = read;
            out->spec = spec;
            out->tail_ensure_index = ensure2->index;
            out->adv_const = c1;
            out->adv = adv;
            out->bsync = bsync;
            out->acc_const = c2;
            out->acc = acc;
            out->ctx_copy = cpy;
            out->rc = rc;
            out->stride = ensure->imm_int;
            out->acc_is_read = false;
            return true;
        }

        /* Strict shape match; any mismatch bails (the loop then stays
         * exactly as P-B emitted it). Safety over coverage. */
        bool a1_recognize(T2Function &fn,
                          const T2LoopInfo &li,
                          size_t idx,
                          A1Shape *out) {
            const T2Loop &loop = li.loops[idx];

            if (loop.preheader == T2_NO_LOOP_BLOCK) {
                return a1_bail(fn, "no preheader");
            }
            if (loop.parent != -1) {
                return a1_bail(fn, "nested loop (has a parent)");
            }
            for (size_t i = 0; i < li.loops.size(); i++) {
                if (li.loops[i].parent == (int32_t)idx) {
                    return a1_bail(fn, "nested loop (has a child)");
                }
            }
            if (loop.latches.size() != 1) {
                return a1_bail(fn, "multiple latches");
            }
            /* P-C L2: the utf8 count/validate loop's latch is SPLIT by
             * one intermediate bs_ensure (the L1 ASCII fastpath's
             * two-block latch), so its body is header + match + L-a +
             * L-b = 4 blocks. A dedicated recognizer folds L-a/L-b and
             * records the ASCII SpeculateRange guard; the cnt/sumt
             * single-latch path (body == 3) below is unchanged. */
            if (loop.body.size() == 4) {
                return a1_recognize_utf8(fn, li, idx, out);
            }
            if (loop.body.size() != 3) {
                return a1_bail(fn, "body is not header+match+latch");
            }

            std::unordered_set<uint32_t> body(loop.body.begin(),
                                              loop.body.end());
            T2BasicBlock *h = fn.blocks[loop.header];
            T2BasicBlock *latch = fn.blocks[loop.latches[0]];

            /* --- header: exactly {ctx, acc} phis + start_match +
             *     succeeded + branch(M, err) --------------------------- */
            T2Op *p0 = h->phis_head;

            if (p0 == nullptr || p0->next == nullptr ||
                p0->next->next != nullptr) {
                return a1_bail(fn, "header phi count != 2");
            }

            T2Op *p1 = p0->next;

            for (T2Op *p : {p0, p1}) {
                if (p->num_operands != 2 || p->phi_blocks == nullptr ||
                    p->dst_reg == T2_REG_NONE || !t2_reg_is_x(p->dst_reg) ||
                    (p->flags & T2_OP_RAW_MODE) != 0) {
                    return a1_bail(fn, "header phi shape");
                }
                bool from_pre = false, from_latch = false;

                for (uint16_t i = 0; i < 2; i++) {
                    from_pre |= p->phi_blocks[i]->id == loop.preheader;
                    from_latch |= p->phi_blocks[i] == latch;
                }
                if (!from_pre || !from_latch) {
                    return a1_bail(fn, "header phi edges");
                }
            }

            T2Op *sm = h->ops_head;

            if (sm == nullptr || sm->kind != T2OpKind::StartMatch ||
                sm->num_operands != 1 || sm->dst_reg == T2_REG_NONE ||
                !t2_reg_is_x(sm->dst_reg)) {
                return a1_bail(fn, "header start_match");
            }

            T2Op *ok = sm->next;

            if (ok == nullptr || ok->kind != T2OpKind::Succeeded ||
                ok->num_operands != 1 || ok->operands[0] != sm->result ||
                ok->next != nullptr) {
                return a1_bail(fn, "header succeeded");
            }

            const T2Op *hterm = h->terminator;

            if (hterm == nullptr || hterm->kind != T2OpKind::Branch ||
                hterm->num_operands != 1 || hterm->operands[0] != ok->result ||
                hterm->succ_then == nullptr || hterm->succ_else == nullptr) {
                return a1_bail(fn, "header branch");
            }

            T2BasicBlock *m = hterm->succ_then;

            if (body.count(m->id) == 0 || m == h || m == latch ||
                body.count(hterm->succ_else->id) != 0) {
                return a1_bail(fn, "header edges");
            }

            T2Op *ctx_phi;
            T2Op *acc_phi;

            if (sm->operands[0] == p0->result) {
                ctx_phi = p0;
                acc_phi = p1;
            } else if (sm->operands[0] == p1->result) {
                ctx_phi = p1;
                acc_phi = p0;
            } else {
                return a1_bail(fn, "start_match operand is not a header phi");
            }

            /* --- M: [bs_get_position] bs_limit bs_cursor bs_ensure +
             *     branch(LATCH, backtrack) ----------------------------- */
            if (m->phis_head != nullptr) {
                return a1_bail(fn, "match block has phis");
            }

            T2Op *op = m->ops_head;
            T2Op *get_pos = nullptr;

            if (op != nullptr && op->kind == T2OpKind::BsGetPosition) {
                if (op->num_operands != 1 || op->operands[0] != sm->result ||
                    op->dst_reg == T2_REG_NONE || !t2_reg_is_x(op->dst_reg)) {
                    return a1_bail(fn, "bs_get_position shape");
                }
                get_pos = op;
                op = op->next;
            }

            T2Op *limit = op;

            if (limit == nullptr || limit->kind != T2OpKind::BsLimit ||
                limit->num_operands != 1 || limit->operands[0] != sm->result ||
                (limit->flags & T2_OP_RAW_MODE) == 0 ||
                limit->dst_reg == T2_REG_NONE || !t2_reg_is_x(limit->dst_reg)) {
                return a1_bail(fn, "bs_limit shape");
            }

            T2Op *cursor = limit->next;

            if (cursor == nullptr || cursor->kind != T2OpKind::BsCursor ||
                cursor->num_operands != 1 ||
                cursor->operands[0] != sm->result ||
                (cursor->flags & T2_OP_RAW_MODE) == 0 ||
                cursor->dst_reg == T2_REG_NONE ||
                !t2_reg_is_x(cursor->dst_reg)) {
                return a1_bail(fn, "bs_cursor shape");
            }

            T2Op *ensure = cursor->next;

            if (ensure == nullptr || ensure->kind != T2OpKind::BsEnsure ||
                ensure->num_operands != 2 ||
                ensure->operands[0] != cursor->result ||
                ensure->operands[1] != limit->result ||
                ensure->next != nullptr) {
                return a1_bail(fn, "bs_ensure shape");
            }
            if (ensure->imm_int <= 0 || ensure->imm_int % 8 != 0) {
                return a1_bail(fn, "stride is not a whole byte count");
            }

            const T2Op *mterm = m->terminator;

            if (mterm == nullptr || mterm->kind != T2OpKind::Branch ||
                mterm->num_operands != 1 ||
                mterm->operands[0] != ensure->result ||
                mterm->succ_then != latch || mterm->succ_else == nullptr ||
                body.count(mterm->succ_else->id) != 0) {
                return a1_bail(fn, "match branch");
            }

            /* --- LATCH: exactly the per-byte body ------------------- */
            if (latch->phis_head != nullptr) {
                return a1_bail(fn, "latch has phis");
            }

            /* A2: optional single byte-read prefix (bs_base + bs_read
             * off THIS context and THIS cursor). Strict: exactly one
             * read, byte-sized, feeding the accumulator below; a
             * second read (or any other op) fails the positional walk
             * that follows. */
            T2Op *lop = latch->ops_head;
            T2Op *base = nullptr;
            T2Op *read = nullptr;

            if (lop != nullptr && lop->kind == T2OpKind::BsBase) {
                if (lop->num_operands != 1 || lop->operands[0] != sm->result ||
                    (lop->flags & T2_OP_RAW_MODE) == 0 ||
                    lop->dst_reg == T2_REG_NONE || !t2_reg_is_x(lop->dst_reg) ||
                    lop->sync != nullptr) {
                    return a1_bail(fn, "bs_base shape");
                }
                base = lop;
                lop = lop->next;
            }
            if (lop != nullptr && lop->kind == T2OpKind::BsRead) {
                if (base == nullptr) {
                    return a1_bail(fn, "bs_read without a bs_base");
                }
                if (lop->num_operands != 2 ||
                    lop->operands[0] != base->result ||
                    lop->operands[1] != cursor->result || lop->imm_int != 8 ||
                    lop->flags != 0 || lop->raw_mask != 0 ||
                    lop->sync != nullptr || lop->dst_reg == T2_REG_NONE ||
                    !t2_reg_is_x(lop->dst_reg)) {
                    return a1_bail(fn, "bs_read shape");
                }
                read = lop;
                lop = lop->next;
            }
            if (base != nullptr && read == nullptr) {
                return a1_bail(fn, "bs_base without a bs_read");
            }

            T2Op *c1 = lop;

            if (c1 == nullptr || c1->kind != T2OpKind::ConstInt ||
                c1->imm_int != ensure->imm_int) {
                return a1_bail(fn, "latch advance constant");
            }

            T2Op *adv = c1->next;

            if (adv == nullptr || adv->kind != T2OpKind::AddSmall ||
                adv->num_operands != 2 || adv->operands[0] != cursor->result ||
                adv->operands[1] != c1->result ||
                (adv->flags & T2_OP_RAW_MODE) == 0 ||
                (adv->flags & T2_OP_NO_OVF) == 0 ||
                adv->dst_reg != cursor->dst_reg || adv->sync != nullptr) {
                return a1_bail(fn, "latch cursor advance");
            }

            T2Op *bsync = adv->next;

            if (bsync == nullptr || bsync->kind != T2OpKind::BsSync ||
                bsync->num_operands != 2 || bsync->operands[0] != sm->result ||
                bsync->operands[1] != adv->result) {
                return a1_bail(fn, "latch bs_sync");
            }

            /* The accumulator rhs: the skip-count constant (A1) or the
             * byte read (A2) — nothing else; a read whose result the
             * accumulator does not consume also bails here. */
            T2Op *c2 = nullptr;
            T2Op *acc = bsync->next;

            if (read == nullptr) {
                c2 = acc;
                if (c2 == nullptr || c2->kind != T2OpKind::ConstInt ||
                    !IS_SSMALL(c2->imm_int)) {
                    return a1_bail(fn, "latch accumulator constant");
                }
                acc = c2->next;
            }

            if (acc == nullptr || acc->kind != T2OpKind::Add ||
                acc->num_operands != 2 || acc->operands[0] != acc_phi->result ||
                acc->operands[1] !=
                        (read != nullptr ? read->result : c2->result) ||
                acc->sync == nullptr || acc->sync->frame_size != T2_NO_FRAME ||
                acc->raw_mask != 0 || acc->flags != 0 ||
                acc->dst_reg != acc_phi->dst_reg) {
                return a1_bail(fn,
                               "latch accumulator (rhs must be the "
                               "skip-count constant or the byte read)");
            }

            T2Op *cpy = acc->next;

            if (cpy == nullptr || cpy->kind != T2OpKind::Copy ||
                cpy->num_operands != 1 || cpy->operands[0] != sm->result ||
                cpy->flags != 0 || cpy->dst_reg != ctx_phi->dst_reg) {
                return a1_bail(fn, "latch context copy");
            }

            T2Op *rc = cpy->next;

            if (rc == nullptr || rc->kind != T2OpKind::ReductionCheck ||
                rc->num_operands != 0 || rc->flags != 0 || rc->raw_mask != 0 ||
                rc->sync == nullptr || rc->sync->frame_size != T2_NO_FRAME ||
                rc->next != nullptr) {
                return a1_bail(fn, "latch reduction_check");
            }

            const T2Op *lterm = latch->terminator;

            if (lterm == nullptr || lterm->kind != T2OpKind::Jump ||
                lterm->succ_then != h) {
                return a1_bail(fn, "latch back edge");
            }

            /* --- phi threading: the latch edge carries {ctx', acc'} - */
            for (uint16_t i = 0; i < 2; i++) {
                if (ctx_phi->phi_blocks[i] == latch &&
                    ctx_phi->operands[i] != cpy->result) {
                    return a1_bail(fn, "ctx phi latch input");
                }
                if (acc_phi->phi_blocks[i] == latch &&
                    acc_phi->operands[i] != acc->result) {
                    return a1_bail(fn, "acc phi latch input");
                }
            }

            /* --- home disjointness ----------------------------------- *
             * The transform re-materializes the raw cursor/limit temps
             * on the fast path (FC), so their homes must not alias the
             * three term homes the clone sync maps are rebuilt from —
             * an aliased write would silently invalidate a map. */
            const int32_t seed_homes[3] = {ctx_phi->dst_reg,
                                           acc_phi->dst_reg,
                                           sm->dst_reg};

            if (ctx_phi->dst_reg == acc_phi->dst_reg) {
                return a1_bail(fn, "phi home collision");
            }
            for (int32_t home : seed_homes) {
                if (cursor->dst_reg == home || limit->dst_reg == home) {
                    return a1_bail(fn, "raw temp home aliases a term home");
                }
            }
            if (cursor->dst_reg == limit->dst_reg) {
                return a1_bail(fn, "cursor/limit home collision");
            }
            if (base != nullptr) {
                for (int32_t home : seed_homes) {
                    if (base->dst_reg == home) {
                        return a1_bail(fn, "bs_base home aliases a term home");
                    }
                }
                if (base->dst_reg == cursor->dst_reg ||
                    base->dst_reg == limit->dst_reg) {
                    return a1_bail(fn, "bs_base/raw temp home collision");
                }
                /* The read may write the (dead-in-latch) ctx phi home
                 * — `content` carries the byte there — but never a
                 * home some later op in the copy still reads. */
                if (read->dst_reg == acc_phi->dst_reg ||
                    read->dst_reg == sm->dst_reg ||
                    read->dst_reg == cursor->dst_reg ||
                    read->dst_reg == limit->dst_reg ||
                    read->dst_reg == base->dst_reg) {
                    return a1_bail(fn, "bs_read home aliases a live home");
                }
            }

            /* --- sync maps vs the walked register contents ----------- *
             * Walk the latch's register state (entered via M) and check
             * both maps name exactly the walked contents, and only in
             * homes the fast path re-establishes (the three seed homes;
             * the transform rebuilds the clone maps from the same walk
             * over the FL path, which skips M, so the only divergence
             * is the dead bs_get_position home — it holds the ctx phi's
             * term there, a valid GC term the T1 continuation never
             * reads). */
            std::unordered_map<int32_t, const T2Value *> content;

            content[ctx_phi->dst_reg] = ctx_phi->result;
            content[acc_phi->dst_reg] = acc_phi->result;
            content[sm->dst_reg] = sm->result;
            if (get_pos != nullptr) {
                content[get_pos->dst_reg] = get_pos->result;
            }
            if (read != nullptr) {
                /* The read executes before the accumulator add: its
                 * home holds the byte when the add's map is taken. */
                content[read->dst_reg] = read->result;
            }

            auto maps_walked = [&](const T2SyncMap *map) -> bool {
                for (uint32_t i = 0; i < map->x_live; i++) {
                    int32_t r = t2_xreg(i);
                    auto it = content.find(r);

                    if (it == content.end() || it->second != map->x[i]) {
                        return false;
                    }
                    if (r != seed_homes[0] && r != seed_homes[1] &&
                        r != seed_homes[2] &&
                        (read == nullptr || r != read->dst_reg)) {
                        /* Not FL-coverable (e.g. an out-of-line
                         * bs_get_position home). The read home IS
                         * coverable: every copy re-establishes it. */
                        return false;
                    }
                }
                return true;
            };

            if (!maps_walked(acc->sync)) {
                return a1_bail(fn, "accumulator sync map contents");
            }
            content[acc->dst_reg] = acc->result;
            content[cpy->dst_reg] = cpy->result;
            if (!maps_walked(rc->sync)) {
                return a1_bail(fn, "reduction_check sync map contents");
            }

            out->h = h;
            out->m = m;
            out->latch = latch;
            out->ctx_phi = ctx_phi;
            out->acc_phi = acc_phi;
            out->sm = sm;
            out->get_pos = get_pos;
            out->limit = limit;
            out->cursor = cursor;
            out->ensure = ensure;
            out->base = base;
            out->read = read;
            out->spec = nullptr; /* cnt/sumt single latch (no ASCII guard) */
            out->tail_ensure_index = 0; /* no intermediate ensure */
            out->adv_const = c1;
            out->adv = adv;
            out->bsync = bsync;
            out->acc_const = c2;
            out->acc = acc;
            out->ctx_copy = cpy;
            out->rc = rc;
            out->stride = ensure->imm_int;
            out->acc_is_read = read != nullptr;
            return true;
        }

        /* B1 fused-FL gate: default ON for the skip-count shape;
         * T2_NO_FUSE=1 (or T2_FUSE=0) forces the A1 verbatim FL (the
         * differential-testing lever). */
        bool a1_fuse_enabled() {
            const char *no = getenv("T2_NO_FUSE");
            const char *f = getenv("T2_FUSE");

            if (no != nullptr && no[0] != '\0' && no[0] != '0') {
                return false;
            }
            if (f != nullptr && f[0] == '0') {
                return false;
            }
            return true;
        }

        /* N = word_bits / stride, overridable by T2_UNROLL_N (decimal,
         * clamped to 1..16); N <= 1 disables the transform. */
        int a1_pick_n(Sint64 stride_bits) {
            long n = stride_bits > 0 ? 64 / stride_bits : 1;
            const char *e = getenv("T2_UNROLL_N");

            if (e != nullptr) {
                char *end = nullptr;
                long v = strtol(e, &end, 10);

                if (end != e) {
                    n = v;
                }
            }
            if (n < 1) {
                n = 1;
            }
            if (n > 16) {
                n = 16;
            }
            return (int)n;
        }

    } /* anonymous namespace */

    bool t2_unroll(T2Function &fn,
                   const T2LoopInfo &li,
                   const ErtsT2RetainedCode *ret,
                   bool *changed,
                   unsigned *fused_unrolls,
                   std::string *err) {
        (void)err;
        *changed = false;
        *fused_unrolls = 0;

        if (fn.blocks.empty() || !fn.sync_complete) {
            return true;
        }

        for (size_t idx = 0; idx < li.loops.size(); idx++) {
            A1Shape s;

            if (!a1_recognize(fn, li, idx, &s)) {
                continue;
            }

            /* A READING loop may only unroll when the speculation pass
             * will convert the FL accumulator adds to flag-checked
             * AddSmall (deopt-before-commit, never allocating): the FL
             * hoists ONE raw base projection over all N reads, which
             * is sound precisely because the fused/verbatim lane has
             * no GC point. With T2_NO_SPEC=1 the adds stay generic
             * gc_bif adds that can allocate a bignum and GC mid-lane,
             * leaving the hoisted base (a raw pointer into the moved
             * binary) stale for the remaining reads — wrong bytes, not
             * a crash. Found while validating B2: forced install +
             * T2_NO_SPEC=1 + a small->bignum crossing seed reproduces
             * it on the A2 verbatim FL. The 1-wide loop re-projects
             * the base every iteration, so not unrolling is the sound
             * (and cheap) fallback. The skip-count shape has no reads
             * and its raw temps are bit counts (GC-stable), so it
             * stays unrollable. */
            if (s.acc_is_read && getenv("T2_NO_SPEC") != nullptr) {
                a1_bail(fn,
                        "read-sum unroll needs the speculation pass "
                        "(T2_NO_SPEC): the hoisted base cannot survive "
                        "a generic add's GC");
                continue;
            }

            int n = a1_pick_n(s.stride);

            if (n <= 1) {
                a1_bail(fn, "N <= 1 (no-op)");
                continue;
            }

            /* Register contents along the fast path (header -> FC ->
             * FL), used to build each clone's sync map: identical to
             * the recognizer's latch walk except that M is skipped, so
             * the dead bs_get_position home keeps the ctx phi's term. */
            std::unordered_map<int32_t, T2Value *> content;

            content[s.ctx_phi->dst_reg] = s.ctx_phi->result;
            content[s.acc_phi->dst_reg] = s.acc_phi->result;
            content[s.sm->dst_reg] = s.sm->result;

            /* --- B1 fused-FL admission (skip-count; the read-sum
             * shape has its own B2 SWAR admission below). Any miss
             * falls back to the A1/A2 verbatim FL — never to a
             * compile failure. ---------------------------------------- */
            auto fuse_bail = [&](const char *why) -> bool {
                if (getenv("T2_UNROLL_TRACE") != nullptr) {
                    erts_fprintf(stderr,
                                 "t2_unroll: %T:%T/%u fuse bail (verbatim "
                                 "FL): %s\n",
                                 fn.module,
                                 fn.function,
                                 (unsigned)fn.arity,
                                 why);
                }
                return false;
            };
            auto fuse_ok = [&]() -> bool {
                if (s.acc_is_read || s.acc_const == nullptr) {
                    return false; /* A2 read-sum: B2 territory, no trace */
                }
                if (s.spec != nullptr) {
                    /* P-C L2 utf8 count: it looks like a skip-count but
                     * has a per-byte ASCII guard the plain B1 FL would
                     * DROP — the SWAR ascii_ok path handles it. */
                    return false;
                }
                if (!a1_fuse_enabled()) {
                    return fuse_bail("disabled (T2_NO_FUSE)");
                }
                if (n < 2) {
                    return fuse_bail("N < 2");
                }
                if (!IS_SSMALL(s.acc_const->imm_int * (Sint64)n)) {
                    return fuse_bail("fused increment is not a small");
                }
                if ((Sint64)n * (1 + (Sint64)s.rc->index) >= 4096) {
                    return fuse_bail("fused reduction charge out of range");
                }
                /* The roll-back deopt state: the header start_match's
                 * own sync map IS "T1 about to run the clause". It
                 * must name exactly the FL-top walked contents (the
                 * pre-iteration state physically live at the fused
                 * add) — anything else cannot be pinned there. */
                if (s.sm->sync == nullptr ||
                    s.sm->sync->frame_size != T2_NO_FRAME) {
                    return fuse_bail("header start_match has no frameless "
                                     "sync map");
                }
                for (uint32_t i = 0; i < s.sm->sync->x_live; i++) {
                    auto it = content.find(t2_xreg(i));

                    if (it == content.end() || it->second != s.sm->sync->x[i]) {
                        return fuse_bail("header sync map does not match "
                                         "the FL-top state");
                    }
                }
                /* The roll-back resume PC: the header's clause-entry
                 * EFFECT site (recorded at i_bs_start_match3; P-C B1).
                 * Checked here so a missing entry degrades to the
                 * verbatim FL instead of failing isel. */
                if (s.sm->beam_idx == 0 || ret == nullptr ||
                    erts_t2_pc_lookup_kind(ret,
                                           fn.fn_index,
                                           s.sm->beam_idx,
                                           ERTS_T2_PC_EFFECT) == nullptr) {
                    return fuse_bail("no EFFECT pctab entry for the "
                                     "header start_match");
                }
                return true;
            };

            /* --- B2 SWAR-fused admission (read-sum only). The N
             * byte-reads + N adds collapse to ONE 64-bit wide load +
             * ONE SwarByteSum fold + ONE checked accumulator add with
             * B1's roll-back deopt (identical contract: header
             * beam_idx, header sync map, add placed before the
             * advance). Any miss falls back to the A2 verbatim FL. */
            auto swar_ok = [&]() -> bool {
                if (!s.acc_is_read) {
                    return false; /* skip-count: B1 territory, no trace */
                }
                if (s.spec != nullptr) {
                    return false; /* utf8 shape: ascii_ok, not B2 */
                }
                if (!a1_fuse_enabled()) {
                    return fuse_bail("disabled (T2_NO_FUSE)");
                }
                if (n < 2 || (Sint64)n * s.stride != 64) {
                    /* One 64-bit word exactly — N=16/128-bit is a
                     * follow-up (two ldp halves + two folds). Note the
                     * fused add's raw SWAR addend is only legal on the
                     * flag-checked AddSmall the speculation pass
                     * converts it to; T2_NO_SPEC was already refused
                     * for every reading loop above (the hoisted-base
                     * GC hazard), and isel refuses the generic form as
                     * the backstop. */
                    return fuse_bail("SWAR width is not one 64-bit word");
                }
                if ((Sint64)n * (1 + (Sint64)s.rc->index) >= 4096) {
                    return fuse_bail("fused reduction charge out of range");
                }
                /* Roll-back deopt state: identical to B1 (fuse_ok). */
                if (s.sm->sync == nullptr ||
                    s.sm->sync->frame_size != T2_NO_FRAME) {
                    return fuse_bail("header start_match has no frameless "
                                     "sync map");
                }
                for (uint32_t i = 0; i < s.sm->sync->x_live; i++) {
                    auto it = content.find(t2_xreg(i));

                    if (it == content.end() || it->second != s.sm->sync->x[i]) {
                        return fuse_bail("header sync map does not match "
                                         "the FL-top state");
                    }
                }
                /* The FL-tail reduction_check map is rebuilt from the
                 * walked contents. Unlike the verbatim FL, the SWAR
                 * lane never re-establishes the byte home (there is no
                 * per-byte read), so the map must only name the three
                 * seed homes (ctx re-established by the copy, acc by
                 * the fused add, sm untouched). */
                for (uint32_t i = 0; i < s.rc->sync->x_live; i++) {
                    int32_t r = t2_xreg(i);

                    if (r != s.ctx_phi->dst_reg && r != s.acc_phi->dst_reg &&
                        r != s.sm->dst_reg) {
                        return fuse_bail("reduction_check map names a home "
                                         "the SWAR lane does not "
                                         "re-establish");
                    }
                }
                if (s.sm->beam_idx == 0 || ret == nullptr ||
                    erts_t2_pc_lookup_kind(ret,
                                           fn.fn_index,
                                           s.sm->beam_idx,
                                           ERTS_T2_PC_EFFECT) == nullptr) {
                    return fuse_bail("no EFFECT pctab entry for the "
                                     "header start_match");
                }
                return true;
            };

            /* --- P-C L2 SWAR-fused ASCII admission (utf8 count). The N
             * per-byte reads + N ASCII guards + N const-increments
             * collapse to ONE 64-bit wide load + ONE SwarAsciiTest
             * (all-8-bytes < 0x80, rolling back to the header on any
             * non-ASCII byte) + ONE checked accumulator add (B1's
             * roll-back deopt: overflow also rolls back). Fires ONLY
             * for the recognized utf8 shape (s.spec set); the plain
             * cnt/sumt shapes never reach here. A miss LEAVES THE LOOP
             * ON L1's 1-wide path (the verbatim FL is NOT a fallback —
             * it would drop the ASCII guard). --------------------------- */
            auto ascii_ok = [&]() -> bool {
                if (s.spec == nullptr) {
                    return false; /* not the utf8 shape: B1/B2 territory */
                }
                if (s.acc_is_read) {
                    /* sumc-style byte-into-acc utf8: OUT of L2 scope (the
                     * codepoint, not the byte, is the value) — stays
                     * 1-wide. The recognizer already refuses it, but keep
                     * the invariant explicit. */
                    return fuse_bail("utf8 read-sum accumulator (1-wide)");
                }
                if (!a1_fuse_enabled()) {
                    return fuse_bail("disabled (T2_NO_FUSE)");
                }
                if (n < 2 || (Sint64)n * s.stride != 64) {
                    /* One 64-bit word exactly (8 ASCII bytes): the SWAR
                     * guard's precondition. Else fall back to 1-wide. */
                    return fuse_bail("SWAR width is not one 64-bit word");
                }
                /* count: the fused increment must be a small (validate
                 * has no acc — acc_const null — and is exempt). */
                if (s.acc_const != nullptr &&
                    !IS_SSMALL(s.acc_const->imm_int * (Sint64)n)) {
                    return fuse_bail("fused increment is not a small");
                }
                if ((Sint64)n * (1 + (Sint64)s.rc->index) >= 4096) {
                    return fuse_bail("fused reduction charge out of range");
                }
                /* Roll-back deopt state: identical to B1/B2 — the header
                 * start_match's own sync map IS "T1 about to run the
                 * clause", the roll-back resume state for BOTH the ASCII
                 * guard and the overflow. */
                if (s.sm->sync == nullptr ||
                    s.sm->sync->frame_size != T2_NO_FRAME) {
                    return fuse_bail("header start_match has no frameless "
                                     "sync map");
                }
                for (uint32_t i = 0; i < s.sm->sync->x_live; i++) {
                    auto it = content.find(t2_xreg(i));

                    if (it == content.end() || it->second != s.sm->sync->x[i]) {
                        return fuse_bail("header sync map does not match "
                                         "the FL-top state");
                    }
                }
                /* The FL-tail reduction_check map is rebuilt from the
                 * walked contents; the SWAR lane re-establishes only the
                 * three seed homes (ctx by the copy, acc by the fused
                 * add, sm untouched) — no per-byte read home. */
                for (uint32_t i = 0; i < s.rc->sync->x_live; i++) {
                    int32_t r = t2_xreg(i);

                    if (r != s.ctx_phi->dst_reg && r != s.acc_phi->dst_reg &&
                        r != s.sm->dst_reg) {
                        return fuse_bail("reduction_check map names a home "
                                         "the SWAR lane does not "
                                         "re-establish");
                    }
                }
                if (s.sm->beam_idx == 0 || ret == nullptr ||
                    erts_t2_pc_lookup_kind(ret,
                                           fn.fn_index,
                                           s.sm->beam_idx,
                                           ERTS_T2_PC_EFFECT) == nullptr) {
                    return fuse_bail("no EFFECT pctab entry for the "
                                     "header start_match");
                }
                return true;
            };
            bool fuse = fuse_ok();
            bool swar = swar_ok();
            bool ascii = ascii_ok();

            /* A recognized utf8 shape that could not be SWAR-admitted
             * must NOT be transformed: the verbatim FL clones the read/
             * advance but DROPS the ASCII SpeculateRange guard, which
             * would count/validate non-ASCII bytes as ASCII. Leave it
             * on L1's correct (unoptimized) 1-wide path. */
            if (s.spec != nullptr && !ascii) {
                a1_bail(fn, "utf8 shape not SWAR-admissible (staying 1-wide)");
                continue;
            }

            auto clone_map = [&](const T2SyncMap *map) -> T2SyncMap * {
                T2SyncMap *c = fn.arena.create<T2SyncMap>();

                c->x_live = map->x_live;
                c->x = fn.arena.alloc_array<T2Value *>(map->x_live);
                for (uint32_t i = 0; i < map->x_live; i++) {
                    c->x[i] = content.at(t2_xreg(i));
                }
                c->frame_size = T2_NO_FRAME;
                c->y = nullptr;
                return c;
            };
            auto clone_regs = [&](T2Op *dst, const T2Op *src) {
                if (src->operand_regs == nullptr) {
                    return;
                }
                dst->operand_regs =
                        fn.arena.alloc_array<int32_t>(src->num_operands);
                for (uint16_t i = 0; i < src->num_operands; i++) {
                    dst->operand_regs[i] = src->operand_regs[i];
                }
            };

            /* --- FC: the fast-path bounds check ---------------------- */
            T2BasicBlock *fc = fn.new_block();
            T2BasicBlock *fl = fn.new_block();

            T2Op *lim2 = fn.new_op(fc, T2OpKind::BsLimit, s.limit->type);

            fn.set_operands(lim2, {s.sm->result});
            clone_regs(lim2, s.limit);
            lim2->dst_reg = s.limit->dst_reg;
            lim2->flags = s.limit->flags;
            lim2->beam_idx = s.limit->beam_idx;

            T2Op *cur2 = fn.new_op(fc, T2OpKind::BsCursor, s.cursor->type);

            fn.set_operands(cur2, {s.sm->result});
            clone_regs(cur2, s.cursor);
            cur2->dst_reg = s.cursor->dst_reg;
            cur2->flags = s.cursor->flags;
            cur2->beam_idx = s.cursor->beam_idx;

            T2Op *en2 = fn.new_op(fc, T2OpKind::BsEnsure, s.ensure->type);

            fn.set_operands(en2, {cur2->result, lim2->result});
            clone_regs(en2, s.ensure);
            en2->imm_int = (Sint64)n * s.stride;
            en2->index = s.ensure->index;
            en2->beam_idx = s.ensure->beam_idx;
            if (swar || ascii) {
                /* B2/L2 alignment guard (index bit 2): the wide load's
                 * 64-bit ldr needs a byte-aligned cursor. Alignment is
                 * loop-invariant (+N*8 bits per trip), so an unaligned
                 * scan takes the else edge to the 1-wide M remainder
                 * (emit_read_bits, alignment-agnostic) every
                 * iteration — correct, just not SWAR. */
                en2->index |= 4;
            }
            if (ascii) {
                /* P-C L2: carry the intermediate ensure's unit-8 bit
                 * (the `R/binary` tail byte-alignment check the utf8
                 * latch performs per byte). It is loop-invariant (the
                 * remainder's mod-8 is unchanged by advancing whole
                 * bytes), so checking it once in FC is exact: a bit-
                 * unaligned binary takes the else edge to the 1-wide M
                 * remainder EVERY iteration, where T1 rejects it at the
                 * SAME point (iteration 1, backtracked to the clause
                 * entry) it would without the unroll — byte-identical. */
                en2->index |= (s.tail_ensure_index & 2);
            }

            fn.emit_branch(fc, en2->result, fl, s.m);
            fc->terminator->beam_idx = s.m->terminator->beam_idx;

            /* Re-point the header's success edge at the fast check; M
             * (and everything behind it) becomes the remainder path. */
            s.h->terminator->succ_then = fc;

            /* --- FL: ONE fused block (B1 skip-count / B2 SWAR
             * read-sum) or N verbatim copies (A1/A2) ------------------ */
            T2Value *cur_k = cur2->result;
            T2Value *acc_k = s.acc_phi->result;
            T2Op *base2 = nullptr;

            if (fuse) {
                /* The header-entry snapshot FIRST — content still holds
                 * the pre-iteration FL-top state (verified against the
                 * start_match map above), which the roll-back deopt
                 * hands back to T1. */
                T2SyncMap *hdr_map = clone_map(s.sm->sync);

                /* The fused accumulator: acc' = acc_phi + N*C, CHECKED,
                 * placed BEFORE the advance/sync so .start is still
                 * un-advanced at its deopt. beam_idx = the header's
                 * start_match ordinal: the overflow deopt redispatches
                 * T1 at the clause entry and re-executes all N
                 * iterations from the pre-add accumulator (the
                 * trampoline un-commits the in-place add first). Still
                 * the generic gc_bif Add here; the speculation pass
                 * converts it to boundary-class AddSmall exactly like
                 * the verbatim clones. */
                T2Value *inc =
                        fn.emit_const_int(fl, s.acc_const->imm_int * (Sint64)n);

                inc->def->beam_idx = s.acc_const->beam_idx;

                T2Op *facc = fn.new_op(fl, T2OpKind::Add, s.acc->type);

                fn.set_operands(facc, {acc_k, inc});
                clone_regs(facc, s.acc);
                facc->dst_reg = s.acc->dst_reg;
                facc->live = s.acc->live;
                facc->mfa_m = s.acc->mfa_m;
                facc->mfa_f = s.acc->mfa_f;
                facc->bif_num = s.acc->bif_num;
                facc->beam_idx = s.sm->beam_idx;
                facc->sync = hdr_map;
                facc->flags = T2_OP_ROLLBACK;
                acc_k = facc->result;
                content[s.acc->dst_reg] = acc_k;
            } else if (swar) {
                /* B2 SWAR read-sum: the header-entry snapshot FIRST
                 * (content still holds the pre-iteration FL-top state,
                 * verified against the start_match map in swar_ok). */
                T2SyncMap *hdr_map = clone_map(s.sm->sync);

                /* The hoisted base projection (no GC point in FL, so
                 * the raw pointer cannot go stale within one trip —
                 * same argument as the verbatim A2 hoist below). */
                base2 = fn.new_op(fl, T2OpKind::BsBase, s.base->type);
                fn.set_operands(base2, {s.sm->result});
                clone_regs(base2, s.base);
                base2->dst_reg = s.base->dst_reg;
                base2->flags = s.base->flags;
                base2->beam_idx = s.base->beam_idx;

                /* ONE 64-bit wide load. Its raw temp reuses the limit
                 * home — dead once FC's bs_ensure consumed it, and
                 * disjoint from every seed/cursor/base home by the
                 * recognizer's checks — so no fresh home is needed and
                 * every sync map's live prefix stays untouched. */
                T2Op *word = fn.new_op(fl, T2OpKind::BsLoadWord, T2Type::any());

                fn.set_operands(word, {base2->result, cur_k});
                word->operand_regs = fn.arena.alloc_array<int32_t>(2);
                word->operand_regs[0] = s.base->dst_reg;
                word->operand_regs[1] = s.cursor->dst_reg;
                word->dst_reg = s.limit->dst_reg;
                word->flags = T2_OP_RAW_MODE;
                word->imm_int = (Sint64)n * s.stride; /* 64 */
                word->beam_idx = s.read->beam_idx;

                /* ONE horizontal fold: sum8 = (b0+..+b7) << 4, the raw
                 * <<4 form the checked add consumes directly. Reuses
                 * the base home (dead after the wide load). */
                T2Op *sum8 =
                        fn.new_op(fl, T2OpKind::SwarByteSum, T2Type::any());

                fn.set_operands(sum8, {word->result});
                sum8->operand_regs = fn.arena.alloc_array<int32_t>(1);
                sum8->operand_regs[0] = s.limit->dst_reg;
                sum8->dst_reg = s.base->dst_reg;
                sum8->flags = T2_OP_RAW_MODE;
                sum8->beam_idx = s.read->beam_idx;

                /* The fused accumulator: acc' = acc_phi + sum8,
                 * CHECKED, placed BEFORE the advance/sync so .start is
                 * still un-advanced at its deopt — the B1 roll-back
                 * contract verbatim, the ONLY difference being the
                 * REGISTER addend (raw <<4), which the emitter handles
                 * with the scratch-then-commit form (nothing to
                 * un-commit; the trampoline just branches to the
                 * header PC). beam_idx = the header start_match
                 * ordinal; sync = the header-entry snapshot. Still the
                 * generic gc_bif Add here; the speculation pass
                 * converts it to the boundary-class AddSmall (isel
                 * refuses the unconverted form). */
                T2Op *facc = fn.new_op(fl, T2OpKind::Add, s.acc->type);

                fn.set_operands(facc, {acc_k, sum8->result});
                facc->operand_regs = fn.arena.alloc_array<int32_t>(2);
                facc->operand_regs[0] = s.acc_phi->dst_reg;
                facc->operand_regs[1] = s.base->dst_reg;
                facc->dst_reg = s.acc->dst_reg;
                facc->live = s.acc->live;
                facc->mfa_m = s.acc->mfa_m;
                facc->mfa_f = s.acc->mfa_f;
                facc->bif_num = s.acc->bif_num;
                facc->beam_idx = s.sm->beam_idx;
                facc->sync = hdr_map;
                facc->flags = T2_OP_ROLLBACK;
                acc_k = facc->result;
                content[s.acc->dst_reg] = acc_k;
            } else if (ascii) {
                /* P-C L2 utf8 count/validate: the header-entry snapshot
                 * FIRST (content holds the pre-iteration FL-top state,
                 * verified against the start_match map in ascii_ok) —
                 * the roll-back deopt state for BOTH the ASCII guard and
                 * the count overflow. */
                T2SyncMap *hdr_map = clone_map(s.sm->sync);

                /* The hoisted base projection, consumed by the single
                 * wide load below BEFORE any acc add — no GC point in
                 * FL, so the raw pointer cannot go stale. */
                base2 = fn.new_op(fl, T2OpKind::BsBase, s.base->type);
                fn.set_operands(base2, {s.sm->result});
                clone_regs(base2, s.base);
                base2->dst_reg = s.base->dst_reg;
                base2->flags = s.base->flags;
                base2->beam_idx = s.base->beam_idx;

                /* ONE 64-bit wide load (the 8 ASCII bytes). Its raw temp
                 * reuses the limit home — dead once FC's bs_ensure
                 * consumed it, disjoint from every seed/cursor/base home
                 * by the recognizer's checks. */
                T2Op *word = fn.new_op(fl, T2OpKind::BsLoadWord, T2Type::any());

                fn.set_operands(word, {base2->result, cur_k});
                word->operand_regs = fn.arena.alloc_array<int32_t>(2);
                word->operand_regs[0] = s.base->dst_reg;
                word->operand_regs[1] = s.cursor->dst_reg;
                word->dst_reg = s.limit->dst_reg;
                word->flags = T2_OP_RAW_MODE;
                word->imm_int = (Sint64)n * s.stride; /* 64 */
                word->beam_idx = s.read->beam_idx;

                /* The fused ASCII guard: all 8 bytes < 0x80 (word &
                 * 0x8080808080808080 == 0). On ANY non-ASCII byte it
                 * ROLLS BACK to the header — cursor un-advanced (the
                 * guard precedes the advance/BsSync), acc pre-iteration
                 * (it precedes the acc add), x0/x1 still holding the
                 * pre-iteration ctx/acc — exactly like B1's overflow,
                 * and T1 then re-processes all N bytes one at a time
                 * (byte-identical). It commits nothing, so its roll-back
                 * trampoline has no un-commit. beam_idx = the header
                 * start_match ordinal (its EFFECT PC is the clause
                 * entry); sync = the header snapshot. */
                T2Op *guard =
                        fn.new_op(fl, T2OpKind::SwarAsciiTest, T2Type::none());

                fn.set_operands(guard, {word->result});
                guard->operand_regs = fn.arena.alloc_array<int32_t>(1);
                guard->operand_regs[0] = s.limit->dst_reg;
                guard->beam_idx = s.sm->beam_idx;
                guard->flags = T2_OP_ROLLBACK;
                guard->deopt_shape = T2DeoptShape::Boundary;
                guard->sync = hdr_map;

                /* count: acc' = acc_phi + N*C, CHECKED (overflow also
                 * rolls back to the header, per B1); validate: NO acc op
                 * (acc_const null). Still the generic gc_bif Add here;
                 * the speculation pass converts it to the boundary-class
                 * AddSmall exactly like B1's fused add. */
                if (s.acc_const != nullptr) {
                    T2Value *inc =
                            fn.emit_const_int(fl,
                                              s.acc_const->imm_int * (Sint64)n);

                    inc->def->beam_idx = s.acc_const->beam_idx;

                    T2Op *facc = fn.new_op(fl, T2OpKind::Add, s.acc->type);

                    fn.set_operands(facc, {acc_k, inc});
                    clone_regs(facc, s.acc);
                    facc->dst_reg = s.acc->dst_reg;
                    facc->live = s.acc->live;
                    facc->mfa_m = s.acc->mfa_m;
                    facc->mfa_f = s.acc->mfa_f;
                    facc->bif_num = s.acc->bif_num;
                    facc->beam_idx = s.sm->beam_idx;
                    facc->sync = hdr_map;
                    facc->flags = T2_OP_ROLLBACK;
                    acc_k = facc->result;
                    content[s.acc->dst_reg] = acc_k;
                }
            } else if (s.read != nullptr) {
                /* One hoisted base projection serves every copy: FL
                 * has no GC point before its last read (the cloned
                 * adds deopt on overflow instead of allocating, and
                 * the shared reduction_check runs after all N copies),
                 * so the raw pointer cannot go stale within one trip.
                 * The next trip re-projects it here. */
                base2 = fn.new_op(fl, T2OpKind::BsBase, s.base->type);
                fn.set_operands(base2, {s.sm->result});
                clone_regs(base2, s.base);
                base2->dst_reg = s.base->dst_reg;
                base2->flags = s.base->flags;
                base2->beam_idx = s.base->beam_idx;
            }

            if (fuse || swar || ascii) {
                /* ONE cursor advance + ONE .start commit for the whole
                 * block (the bs_ensure in FC covered N*stride bits). */
                T2Value *cs = fn.emit_const_int(fl, (Sint64)n * s.stride);

                cs->def->beam_idx = s.adv_const->beam_idx;

                T2Op *adv1 = fn.new_op(fl, T2OpKind::AddSmall, s.adv->type);

                fn.set_operands(adv1, {cur_k, cs});
                clone_regs(adv1, s.adv);
                adv1->dst_reg = s.adv->dst_reg;
                adv1->flags = s.adv->flags;
                adv1->beam_idx = s.adv->beam_idx;
                cur_k = adv1->result;

                T2Op *sync1 = fn.new_op(fl, T2OpKind::BsSync, s.bsync->type);

                fn.set_operands(sync1, {s.sm->result, cur_k});
                clone_regs(sync1, s.bsync);
                sync1->beam_idx = s.bsync->beam_idx;
            }

            for (int k = 0; !fuse && !swar && !ascii && k < n; k++) {
                T2Op *read_k = nullptr;

                if (s.read != nullptr) {
                    read_k = fn.new_op(fl, T2OpKind::BsRead, s.read->type);
                    fn.set_operands(read_k, {base2->result, cur_k});
                    clone_regs(read_k, s.read);
                    read_k->dst_reg = s.read->dst_reg;
                    read_k->flags = s.read->flags;
                    read_k->imm_int = s.read->imm_int;
                    read_k->index = s.read->index;
                    read_k->beam_idx = s.read->beam_idx;
                    /* BEFORE this copy's clone_map: the k-th add's map
                     * must name byte_k in the read home (the 1-wide
                     * deopt contract — the redispatched gc_bif '+'
                     * consumes the very byte that was read). */
                    content[s.read->dst_reg] = read_k->result;
                }

                T2Value *cs = fn.emit_const_int(fl, s.stride);

                cs->def->beam_idx = s.adv_const->beam_idx;

                T2Op *adv_k = fn.new_op(fl, T2OpKind::AddSmall, s.adv->type);

                fn.set_operands(adv_k, {cur_k, cs});
                clone_regs(adv_k, s.adv);
                adv_k->dst_reg = s.adv->dst_reg;
                adv_k->flags = s.adv->flags;
                adv_k->beam_idx = s.adv->beam_idx;
                cur_k = adv_k->result;

                T2Op *sync_k = fn.new_op(fl, T2OpKind::BsSync, s.bsync->type);

                fn.set_operands(sync_k, {s.sm->result, cur_k});
                clone_regs(sync_k, s.bsync);
                sync_k->beam_idx = s.bsync->beam_idx;

                T2Value *rhs;

                if (s.acc_is_read) {
                    rhs = read_k->result;
                } else {
                    rhs = fn.emit_const_int(fl, s.acc_const->imm_int);
                    rhs->def->beam_idx = s.acc_const->beam_idx;
                }

                T2Op *acc_next = fn.new_op(fl, T2OpKind::Add, s.acc->type);

                fn.set_operands(acc_next, {acc_k, rhs});
                clone_regs(acc_next, s.acc);
                acc_next->dst_reg = s.acc->dst_reg;
                acc_next->live = s.acc->live;
                acc_next->mfa_m = s.acc->mfa_m;
                acc_next->mfa_f = s.acc->mfa_f;
                acc_next->bif_num = s.acc->bif_num;
                acc_next->beam_idx = s.acc->beam_idx;
                acc_next->sync = clone_map(s.acc->sync);
                acc_k = acc_next->result;
                content[s.acc->dst_reg] = acc_k;
            }

            T2Op *cpy2 = fn.new_op(fl, T2OpKind::Copy, s.ctx_copy->type);

            fn.set_operands(cpy2, {s.sm->result});
            clone_regs(cpy2, s.ctx_copy);
            cpy2->dst_reg = s.ctx_copy->dst_reg;
            cpy2->beam_idx = s.ctx_copy->beam_idx;
            content[s.ctx_copy->dst_reg] = cpy2->result;

            T2Op *rc2 = fn.new_op(fl, T2OpKind::ReductionCheck, s.rc->type);

            fn.set_operands(rc2, {});
            /* isel charges 1 + index at the back edge. The fused FL ran
             * N iterations' work, so it charges N of the 1-wide loop's
             * per-iteration amount (task #46): every iteration is paid
             * exactly once, just before it starts, mirroring T1's
             * i_test_yield-at-entry discipline — reduction counts stay
             * T1-exact. The verbatim FL keeps its historical charge. */
            rc2->index =
                    (fuse || swar || ascii)
                            ? (uint32_t)((Sint64)n * (1 + (Sint64)s.rc->index) -
                                         1)
                            : s.rc->index;
            rc2->beam_idx = s.rc->beam_idx;
            rc2->sync = clone_map(s.rc->sync);

            fn.emit_jump(fl, s.h);
            fl->terminator->beam_idx = s.latch->terminator->beam_idx;

            /* --- third phi inputs: the FL back edge ------------------ */
            for (T2Op *phi : {s.ctx_phi, s.acc_phi}) {
                std::vector<T2Value *> vals;
                std::vector<T2BasicBlock *> preds;

                for (uint16_t i = 0; i < phi->num_operands; i++) {
                    vals.push_back(phi->operands[i]);
                    preds.push_back(phi->phi_blocks[i]);
                }
                vals.push_back(phi == s.ctx_phi ? cpy2->result : acc_k);
                preds.push_back(fl);
                fn.set_phi_inputs(phi, vals, preds);
            }

            fn.finalize();
            *changed = true;
            if (fuse || swar || ascii) {
                /* The install-gate `cursor_unroll` signal (P-C
                 * increment C): ONLY the roll-back-pinned fused FLs
                 * (all admitted with N >= 2 above, incl. the L2 utf8
                 * ASCII FL). The A2 verbatim read-sum FL must NOT be
                 * credited — its generic adds stay allocating when the
                 * profiler withholds acc speculation, and a GC mid-lane
                 * strands the hoisted raw base (see the header comment). */
                (*fused_unrolls)++;
            }

            if (getenv("T2_UNROLL_TRACE") != nullptr) {
                erts_fprintf(
                        stderr,
                        "t2_unroll: %T:%T/%u x%d (%s, %s, stride %ld "
                        "bits, header block%u)\n",
                        fn.module,
                        fn.function,
                        (unsigned)fn.arity,
                        n,
                        ascii ? "utf8-count"
                              : (s.acc_is_read ? "read-sum" : "skip-count"),
                        ascii ? "swar-ascii"
                              : (swar ? "swar-fused"
                                      : (fuse ? "fused" : "verbatim")),
                        (long)s.stride,
                        s.h->id);
            }
        }

        return true;
    }

} /* namespace erts_t2 */
