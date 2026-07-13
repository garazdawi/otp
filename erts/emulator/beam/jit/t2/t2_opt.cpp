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
 * T2-Full tier-2 JIT: the Stage 3 standard optimization suite. See
 * t2_opt.hpp for the design summary and
 * PLAN/T2FULL/census/stage3_opts_design.md for the spec.
 *
 * The deopt/home model constrains every rewrite (design doc §"Hard
 * constraints"); the load-bearing rules, restated:
 *
 *   - The backend reads operand i of an op from operand_regs[i] (a
 *     BEAM register) at runtime; an operand without a register must be
 *     a constant, inlined as an immediate (t2_isel.cpp src_of). So
 *     swapping an operand's *value* while keeping its register is a
 *     runtime no-op, and retargeting a read to another register needs
 *     an availability proof: the new value must still sit in its
 *     canonical home on every path from its def to the use, with no
 *     intervening write to that register and no clobber-class op
 *     (calls trash X registers; GC above the live prefix, frame
 *     motion, match-context ops conservatively clobber).
 *
 *   - Sync maps are uses. A value referenced by any sync map (the
 *     function entry map included) is not dead, and the register a
 *     map claims for it must really hold it — so DCE counts sync
 *     references and the copy that materializes a sync-visible value
 *     stays.
 *
 *   - The validator's register-state walk is exact within a block but
 *     adopts unknown state across blocks, so cross-block soundness is
 *     proven here (the availability walk), never left to it.
 */

#include "t2_opt.hpp"

extern "C"
{
#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "big.h"
#include "erl_fun.h"
#include "erl_map.h"
}

#include <cstdio>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace erts_t2 {

    namespace {

        bool env_flag(const char *name) {
            const char *env = getenv(name);
            return env != NULL && env[0] != '\0' && env[0] != '0';
        }

        bool opt_trace() {
            static int on = -1;

            if (on < 0) {
                on = env_flag("T2_OPT_TRACE") ? 1 : 0;
            }
            return on == 1;
        }

        bool dce_disabled() {
            static int off = -1;

            if (off < 0) {
                off = env_flag("T2_NO_DCE") ? 1 : 0;
            }
            return off == 1;
        }

        bool constfold_disabled() {
            static int off = -1;

            if (off < 0) {
                off = env_flag("T2_NO_CONSTFOLD") ? 1 : 0;
            }
            return off == 1;
        }

        bool copyprop_disabled() {
            static int off = -1;

            if (off < 0) {
                off = env_flag("T2_NO_COPYPROP") ? 1 : 0;
            }
            return off == 1;
        }

        bool cse_disabled() {
            static int off = -1;

            if (off < 0) {
                off = env_flag("T2_NO_CSE") ? 1 : 0;
            }
            return off == 1;
        }

        bool sink_disabled() {
            static int off = -1;

            if (off < 0) {
                off = env_flag("T2_NO_SINK") ? 1 : 0;
            }
            return off == 1;
        }

        /* P3 (redundant-guard elimination + range-based overflow-guard
         * removal, PLAN/T2FULL/census/opt_landscape.md §P3): its own
         * lever on top of T2_NO_OPT so P1/P2 A/B runs stay unaffected;
         * lever-off reproduces the pre-P3 LIR/asm byte-for-byte. */
        bool p3_disabled() {
            static int off = -1;

            if (off < 0) {
                off = env_flag("T2_NO_P3") ? 1 : 0;
            }
            return off == 1;
        }

        bool p3_trace() {
            static int on = -1;

            if (on < 0) {
                on = env_flag("T2_P3_TRACE") ? 1 : 0;
            }
            return on == 1;
        }

        bool is_const_kind(T2OpKind k) {
            switch (k) {
            case T2OpKind::ConstInt:
            case T2OpKind::ConstFloat:
            case T2OpKind::ConstAtom:
            case T2OpKind::ConstNil:
            case T2OpKind::ConstLiteral:
                return true;
            default:
                return false;
            }
        }

        /* A member of a decoded atomic pair (get_list / swap): DCE
         * must not split it and its reads are order-fused. */
        bool is_pair_member(const T2Op *op) {
            if (op->flags & T2_OP_PAIR_HEAD) {
                return true;
            }
            return op->prev != nullptr &&
                   (op->prev->flags & T2_OP_PAIR_HEAD) != 0;
        }

        /* Op kinds whose operand reads may be retargeted (copy
         * propagation / CSE). Deliberately excludes: speculative ops
         * (their operands' small/raw proofs are per-value — a rewrite
         * could sever a SpeculateType from its consumer), sync-tied
         * ops (calls/returns pin x[i] == operands[i]), terminators,
         * phis (an input must sit in the phi's home at the pred exit,
         * which is a different register than the source's), and pair
         * members. */
        bool operand_rewrite_ok(const T2Op *op) {
            if (is_pair_member(op)) {
                return false;
            }
            switch (op->kind) {
            case T2OpKind::Copy:
            case T2OpKind::CmpEqExact:
            case T2OpKind::CmpNeExact:
            case T2OpKind::CmpEq:
            case T2OpKind::CmpNe:
            case T2OpKind::CmpLt:
            case T2OpKind::CmpLe:
            case T2OpKind::CmpGt:
            case T2OpKind::CmpGe:
            case T2OpKind::IsFlatmapBounded:
            case T2OpKind::FlatmapSize:
            case T2OpKind::FlatmapKeyAt:
            case T2OpKind::FlatmapValAt:
            case T2OpKind::GetTupleElement:
                return true;
            default:
                return false;
            }
        }

        /* One recorded use of a value: the using op and the operand
         * slot (phi inputs are operand slots too). */
        struct Use {
            T2Op *op;
            uint16_t idx;
        };

        struct OptPass {
            T2Function &fn;
            bool changed = false;

            /* Per-fixpoint-round statistics (trace only). */
            unsigned n_dce = 0;
            unsigned n_fold = 0;
            unsigned n_copy = 0;
            unsigned n_phi = 0;
            unsigned n_cse = 0;
            unsigned n_sink = 0;
            unsigned n_p3g = 0; /* P3a: redundant guards removed  */
            unsigned n_p3o = 0; /* P3b: overflow checks eliminated */

            /* ---- shared use/def bookkeeping (recomputed per pass) - */

            std::vector<uint32_t> use_count; /* operand + sync uses  */
            std::vector<uint8_t> sync_ref;   /* in some sync map     */
            std::vector<uint8_t> phi_ref;    /* some phi input       */

            explicit OptPass(T2Function &fn_) : fn(fn_) {
            }

            void trace_p3(const char *what, const T2Op *op) {
                if (!p3_trace() && !opt_trace()) {
                    return;
                }
                erts_fprintf(stderr,
                             "t2_opt: %T:%T/%u %s %s%s%u (block %u)\n",
                             fn.module,
                             fn.function,
                             (unsigned)fn.arity,
                             what,
                             t2_op_kind_name(op->kind),
                             op->result != nullptr ? " v" : " #",
                             op->result != nullptr ? op->result->id
                                                   : (uint32_t)0,
                             op->block != nullptr ? op->block->id : 0u);
            }

            void trace(const char *what, const T2Op *op) {
                if (!opt_trace()) {
                    return;
                }
                erts_fprintf(stderr,
                             "t2_opt: %T:%T/%u %s %s%s%u (block %u)\n",
                             fn.module,
                             fn.function,
                             (unsigned)fn.arity,
                             what,
                             t2_op_kind_name(op->kind),
                             op->result != nullptr ? " v" : " #",
                             op->result != nullptr ? op->result->id
                                                   : (uint32_t)0,
                             op->block != nullptr ? op->block->id : 0u);
            }

            void count_sync(const T2SyncMap *m) {
                if (m == nullptr) {
                    return;
                }
                for (uint32_t i = 0; i < m->x_live; i++) {
                    use_count[m->x[i]->id]++;
                    sync_ref[m->x[i]->id] = 1;
                }
                for (int32_t i = 0;
                     m->frame_size != T2_NO_FRAME && i < m->frame_size;
                     i++) {
                    use_count[m->y[i]->id]++;
                    sync_ref[m->y[i]->id] = 1;
                }
            }

            void count_op(const T2Op *op) {
                for (uint16_t i = 0; i < op->num_operands; i++) {
                    use_count[op->operands[i]->id]++;
                    if (op->kind == T2OpKind::Phi) {
                        phi_ref[op->operands[i]->id] = 1;
                    }
                }
                count_sync(op->sync);
            }

            void count_uses() {
                use_count.assign(fn.values.size(), 0);
                sync_ref.assign(fn.values.size(), 0);
                phi_ref.assign(fn.values.size(), 0);

                count_sync(fn.entry_sync);
                for (const T2BasicBlock *b : fn.blocks) {
                    for (const T2Op *phi = b->phis_head; phi != nullptr;
                         phi = phi->next) {
                        count_op(phi);
                    }
                    for (const T2Op *op = b->ops_head; op != nullptr;
                         op = op->next) {
                        count_op(op);
                    }
                    if (b->terminator != nullptr) {
                        count_op(b->terminator);
                    }
                }
            }

            /* All operand-slot uses of `v` (phi inputs included), plus
             * the ops whose sync map references it. */
            void collect_uses(const T2Value *v,
                              std::vector<Use> *ops,
                              std::vector<T2Op *> *sync_users) {
                auto scan = [&](T2Op *op) {
                    for (uint16_t i = 0; i < op->num_operands; i++) {
                        if (op->operands[i] == v) {
                            ops->push_back({op, i});
                        }
                    }
                    const T2SyncMap *m = op->sync;

                    if (m == nullptr || sync_users == nullptr) {
                        return;
                    }
                    bool found = false;

                    for (uint32_t i = 0; i < m->x_live && !found; i++) {
                        found = m->x[i] == v;
                    }
                    for (int32_t i = 0; m->frame_size != T2_NO_FRAME &&
                                        i < m->frame_size && !found;
                         i++) {
                        found = m->y[i] == v;
                    }
                    if (found) {
                        sync_users->push_back(op);
                    }
                };

                for (T2BasicBlock *b : fn.blocks) {
                    for (T2Op *phi = b->phis_head; phi != nullptr;
                         phi = phi->next) {
                        scan(phi);
                    }
                    for (T2Op *op = b->ops_head; op != nullptr; op = op->next) {
                        scan(op);
                    }
                    if (b->terminator != nullptr) {
                        scan(b->terminator);
                    }
                }
            }

            /* Replace every reference to `from` (operands, phi inputs,
             * sync maps) with `to`. Registers are untouched: callers
             * only use this when `to` is materialized in the same
             * locations (the single-input-phi identity). */
            void replace_value(T2Value *from, T2Value *to) {
                auto rewrite = [&](T2Op *op) {
                    for (uint16_t i = 0; i < op->num_operands; i++) {
                        if (op->operands[i] == from) {
                            op->operands[i] = to;
                        }
                    }
                    T2SyncMap *m = op->sync;

                    if (m == nullptr) {
                        return;
                    }
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
                };

                for (T2BasicBlock *b : fn.blocks) {
                    for (T2Op *phi = b->phis_head; phi != nullptr;
                         phi = phi->next) {
                        rewrite(phi);
                    }
                    for (T2Op *op = b->ops_head; op != nullptr; op = op->next) {
                        rewrite(op);
                    }
                    if (b->terminator != nullptr) {
                        rewrite(b->terminator);
                    }
                }
            }

            /* ---- list surgery ------------------------------------- */

            void unlink_body(T2BasicBlock *b, T2Op *op) {
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
                op->block = nullptr;
            }

            void unlink_phi(T2BasicBlock *b, T2Op *phi) {
                if (phi->prev != nullptr) {
                    phi->prev->next = phi->next;
                } else {
                    b->phis_head = phi->next;
                }
                if (phi->next != nullptr) {
                    phi->next->prev = phi->prev;
                } else {
                    b->phis_tail = phi->prev;
                }
                phi->prev = phi->next = nullptr;
                phi->block = nullptr;
            }

            void insert_head(T2BasicBlock *b, T2Op *op) {
                op->block = b;
                op->prev = nullptr;
                op->next = b->ops_head;
                if (b->ops_head != nullptr) {
                    b->ops_head->prev = op;
                } else {
                    b->ops_tail = op;
                }
                b->ops_head = op;
            }

            /* True when `a` executes strictly before `b` in one block
             * (phis precede all body ops). */
            bool op_precedes(const T2Op *a, const T2Op *b) const {
                if (a->kind == T2OpKind::Phi) {
                    return b->kind != T2OpKind::Phi || a != b;
                }
                if (b->kind == T2OpKind::Phi) {
                    return false;
                }
                for (const T2Op *p = a->next; p != nullptr; p = p->next) {
                    if (p == b) {
                        return true;
                    }
                }
                return false;
            }

            /* ---- dominators (validator-style bitset dataflow) ----- */

            std::vector<bool> reach;
            std::vector<std::vector<uint64_t>> dom;
            size_t dom_words = 0;

            bool dominates(uint32_t a, uint32_t b) const {
                return (dom[b][a / 64] >> (a % 64)) & 1;
            }

            template<typename F>
            static void for_each_succ(const T2Op *term, F f) {
                if (term == nullptr) {
                    return;
                }
                switch (term->kind) {
                case T2OpKind::Branch:
                    f(term->succ_then);
                    f(term->succ_else);
                    break;
                case T2OpKind::Jump:
                    f(term->succ_then);
                    break;
                case T2OpKind::Switch:
                    for (uint32_t i = 0; i < term->num_cases; i++) {
                        f(term->cases[i].target);
                    }
                    f(term->default_target);
                    break;
                default:
                    break;
                }
            }

            void compute_dominators() {
                size_t n = fn.blocks.size();

                dom_words = (n + 63) / 64;
                reach.assign(n, false);
                dom.assign(n, std::vector<uint64_t>(dom_words, 0));

                std::vector<const T2BasicBlock *> stack{fn.blocks[0]};

                reach[0] = true;
                while (!stack.empty()) {
                    const T2BasicBlock *b = stack.back();

                    stack.pop_back();
                    for_each_succ(b->terminator, [&](T2BasicBlock *s) {
                        if (s != nullptr && !reach[s->id]) {
                            reach[s->id] = true;
                            stack.push_back(s);
                        }
                    });
                }

                std::vector<uint64_t> all(dom_words, ~uint64_t(0));

                dom[0][0] = 1;
                for (size_t i = 1; i < n; i++) {
                    if (reach[i]) {
                        dom[i] = all;
                    }
                }

                bool ch = true;

                while (ch) {
                    ch = false;
                    for (size_t i = 1; i < n; i++) {
                        if (!reach[i]) {
                            continue;
                        }

                        const T2BasicBlock *b = fn.blocks[i];
                        std::vector<uint64_t> in(dom_words, ~uint64_t(0));
                        bool any_pred = false;

                        for (uint32_t p = 0; p < b->num_preds; p++) {
                            uint32_t pid = b->preds[p]->id;

                            if (!reach[pid]) {
                                continue;
                            }
                            any_pred = true;
                            for (size_t w = 0; w < dom_words; w++) {
                                in[w] &= dom[pid][w];
                            }
                        }
                        if (!any_pred) {
                            in.assign(dom_words, 0);
                        }
                        in[i / 64] |= uint64_t(1) << (i % 64);
                        if (in != dom[i]) {
                            dom[i] = std::move(in);
                            ch = true;
                        }
                    }
                }
            }

            /* `def_op` executes strictly before `user` on every path
             * (same-block order or strict block dominance). */
            bool def_precedes(const T2Op *def_op, const T2Op *user) const {
                if (def_op->block == user->block) {
                    return op_precedes(def_op, user);
                }
                return def_op->block->id != user->block->id &&
                       dominates(def_op->block->id, user->block->id);
            }

            /* ---- availability ------------------------------------- *
             * Is value `w` still in register `r` when `use` executes,
             * given that w's def materialized it there? Conservative:
             * any write to r, and any clobber-class op (calls, gc_bif
             * arithmetic, GC above r's index, frame motion, match ops,
             * yield points) on any def->use path says no. */

            bool clobbers_reg(const T2Op *op, int32_t r) const {
                if (op->result != nullptr && op->dst_reg == r) {
                    return true;
                }
                switch (op->kind) {
                case T2OpKind::Call:
                case T2OpKind::CallExt:
                case T2OpKind::CallFun:
                case T2OpKind::Bif:
                case T2OpKind::GuardBif:
                case T2OpKind::Add:
                case T2OpKind::Sub:
                case T2OpKind::Mul:
                case T2OpKind::IDiv:
                case T2OpKind::Rem:
                case T2OpKind::Band:
                case T2OpKind::Bor:
                case T2OpKind::Bxor:
                case T2OpKind::Bsl:
                case T2OpKind::Bsr:
                case T2OpKind::Bnot:
                case T2OpKind::Neg:
                case T2OpKind::StartMatch:
                case T2OpKind::BsMatch:
                case T2OpKind::BsGetTail:
                case T2OpKind::ReductionCheck:
                case T2OpKind::ScheduleOut:
                case T2OpKind::Allocate:
                case T2OpKind::Deallocate:
                case T2OpKind::Trim:
                    return true;
                case T2OpKind::GcTest:
                    /* GC keeps the value<->slot binding for the live X
                     * prefix (roots are updated in place); everything
                     * above it is garbage afterwards. */
                    return !(t2_reg_is_x(r) && t2_reg_index(r) < op->live);
                default:
                    return false;
                }
            }

            bool phis_clobber(const T2BasicBlock *b,
                              int32_t r,
                              const T2Op *except = nullptr) const {
                for (const T2Op *phi = b->phis_head; phi != nullptr;
                     phi = phi->next) {
                    if (phi != except && phi->dst_reg == r) {
                        return true;
                    }
                }
                return false;
            }

            /* Any clobber of r in [from, to) of a block body; from ==
             * nullptr scans nothing, to == nullptr scans to the end. */
            bool range_clobbers(const T2Op *from,
                                const T2Op *to,
                                int32_t r) const {
                for (const T2Op *p = from; p != nullptr && p != to;
                     p = p->next) {
                    if (clobbers_reg(p, r)) {
                        return true;
                    }
                }
                return false;
            }

            bool block_clobbers(const T2BasicBlock *b, int32_t r) const {
                /* Terminators never write registers (branch/jump/
                 * switch on a path block; return/tail transfers end
                 * paths). */
                return phis_clobber(b, r) ||
                       range_clobbers(b->ops_head, nullptr, r);
            }

            bool value_available_at(const T2Value *w,
                                    int32_t r,
                                    const T2Op *use) const {
                const T2Op *def = w->def;
                const T2BasicBlock *db = def->block;
                const T2BasicBlock *ub = use->block;
                size_t n = fn.blocks.size();

                /* Every op of a block executes on every pass through
                 * it, so a path segment that re-enters the def block
                 * re-executes the def and re-establishes w in r — only
                 * the suffix after the LAST visit of db matters. The
                 * walks below therefore treat db as a barrier: fwd[B]
                 * marks a path db -> B of >= 1 edge not passing
                 * *through* db again, bwd[B] a path B -> ub of >= 1
                 * edge not passing through db. */
                std::vector<uint8_t> fwd(n, 0), bwd(n, 0);
                std::vector<const T2BasicBlock *> work;

                for_each_succ(db->terminator, [&](T2BasicBlock *s) {
                    if (s != nullptr && !fwd[s->id]) {
                        fwd[s->id] = 1;
                        work.push_back(s);
                    }
                });
                while (!work.empty()) {
                    const T2BasicBlock *b = work.back();

                    work.pop_back();
                    if (b == db) {
                        continue; /* barrier: def re-executes */
                    }
                    for_each_succ(b->terminator, [&](T2BasicBlock *s) {
                        if (s != nullptr && !fwd[s->id]) {
                            fwd[s->id] = 1;
                            work.push_back(s);
                        }
                    });
                }
                for (uint32_t p = 0; p < ub->num_preds; p++) {
                    if (!bwd[ub->preds[p]->id]) {
                        bwd[ub->preds[p]->id] = 1;
                        work.push_back(ub->preds[p]);
                    }
                }
                while (!work.empty()) {
                    const T2BasicBlock *b = work.back();

                    work.pop_back();
                    if (b == db) {
                        continue; /* barrier */
                    }
                    for (uint32_t p = 0; p < b->num_preds; p++) {
                        if (!bwd[b->preds[p]->id]) {
                            bwd[b->preds[p]->id] = 1;
                            work.push_back(b->preds[p]);
                        }
                    }
                }

                bool direct = db == ub && op_precedes(def, use);

                if (direct) {
                    /* The straight segment between def and use. */
                    const T2Op *from = def->kind == T2OpKind::Phi ? db->ops_head
                                                                  : def->next;

                    if (def->kind == T2OpKind::Phi &&
                        phis_clobber(db, r, def)) {
                        return false;
                    }
                    if (range_clobbers(from, use, r)) {
                        return false;
                    }
                    if (fwd[db->id]) {
                        /* Cyclic re-entry: the def re-executes, so
                         * only its suffix on the way out matters. */
                        if (range_clobbers(from, nullptr, r)) {
                            return false;
                        }
                    }
                } else {
                    if (!fwd[ub->id]) {
                        /* No path (unreachable use); be conservative. */
                        return false;
                    }
                    /* def-block suffix after the def. */
                    if (def->kind == T2OpKind::Phi) {
                        if (phis_clobber(db, r, def) ||
                            range_clobbers(db->ops_head, nullptr, r)) {
                            return false;
                        }
                    } else if (range_clobbers(def->next, nullptr, r)) {
                        return false;
                    }
                    /* use-block prefix up to the use. */
                    if (use->kind != T2OpKind::Phi) {
                        if (phis_clobber(ub, r) ||
                            range_clobbers(ub->ops_head,
                                           use == ub->terminator ? nullptr
                                                                 : use,
                                           r)) {
                            return false;
                        }
                    }
                }

                /* Every block on an intermediate or cyclic path (fwd
                 * AND bwd, the def-block barrier applied) is traversed
                 * whole — the use block included when a path re-enters
                 * it. */
                for (size_t i = 0; i < n; i++) {
                    if (fwd[i] && bwd[i] && fn.blocks[i] != db &&
                        block_clobbers(fn.blocks[i], r)) {
                        return false;
                    }
                }
                return true;
            }

            /* ================================================================
             * 3a  DCE
             * ================================================================
             */

            bool dce_removable(const T2Op *op) const {
                if (op->result == nullptr || is_pair_member(op)) {
                    return false;
                }
                if (op->kind == T2OpKind::Phi) {
                    return true; /* dead phi */
                }
                if (op->kind == T2OpKind::Param) {
                    /* Alive via the entry sync map by construction;
                     * never removed even if that ever changed. */
                    return false;
                }
                return t2_op_is_pure(op->kind);
            }

            void dce_release_refs(const T2Op *op, std::vector<T2Op *> *work) {
                auto release = [&](T2Value *v) {
                    if (use_count[v->id] > 0 && --use_count[v->id] == 0 &&
                        v->def != nullptr && v->def->block != nullptr &&
                        dce_removable(v->def)) {
                        work->push_back(v->def);
                    }
                };

                for (uint16_t i = 0; i < op->num_operands; i++) {
                    release(op->operands[i]);
                }

                const T2SyncMap *m = op->sync;

                if (m == nullptr) {
                    return;
                }
                for (uint32_t i = 0; i < m->x_live; i++) {
                    release(m->x[i]);
                }
                for (int32_t i = 0;
                     m->frame_size != T2_NO_FRAME && i < m->frame_size;
                     i++) {
                    release(m->y[i]);
                }
            }

            void run_dce() {
                if (dce_disabled()) {
                    return;
                }
                count_uses();

                std::vector<T2Op *> work;

                for (T2BasicBlock *b : fn.blocks) {
                    for (T2Op *phi = b->phis_head; phi != nullptr;
                         phi = phi->next) {
                        if (dce_removable(phi) &&
                            use_count[phi->result->id] == 0) {
                            work.push_back(phi);
                        }
                    }
                    for (T2Op *op = b->ops_head; op != nullptr; op = op->next) {
                        if (dce_removable(op) &&
                            use_count[op->result->id] == 0) {
                            work.push_back(op);
                        }
                    }
                }

                while (!work.empty()) {
                    T2Op *op = work.back();

                    work.pop_back();
                    if (op->block == nullptr ||
                        use_count[op->result->id] != 0) {
                        continue; /* already removed / re-queued stale */
                    }

                    trace("dce", op);

                    T2BasicBlock *b = op->block;

                    dce_release_refs(op, &work);
                    if (op->kind == T2OpKind::Phi) {
                        unlink_phi(b, op);
                    } else {
                        unlink_body(b, op);
                    }
                    n_dce++;
                    changed = true;
                }
            }

            /* ================================================================
             * 3b  constant folding
             * ================================================================
             */

            /* Rewrite `op` in place into a constant of `def`'s kind,
             * keeping its home (the materialization move becomes a
             * move-immediate) and its SSA identity (no use rewriting,
             * so every sync map stays truthful). */
            void fold_to_const(T2Op *op, const T2Op *def) {
                op->kind = def->kind;
                op->imm_int = def->imm_int;
                op->imm_term = def->imm_term;
                op->index = def->index;
                op->type = def->type;
                if (op->result != nullptr && def->result != nullptr) {
                    op->result->type = def->result->type;
                }
                op->num_operands = 0;
                op->operands = nullptr;
                op->operand_regs = nullptr;
                op->sync = nullptr;
                op->flags &= (uint16_t)~(
                        T2_OP_SPEC_BOUNDARY | T2_OP_SPEC_CALLSITE |
                        T2_OP_SPEC_ENTRY | T2_OP_WINDOW_CALLEE | T2_OP_NO_OVF);
            }

            void run_constfold() {
                if (constfold_disabled()) {
                    return;
                }

                for (T2BasicBlock *b : fn.blocks) {
                    for (T2Op *op = b->ops_head; op != nullptr; op = op->next) {
                        if (is_pair_member(op)) {
                            continue;
                        }

                        if (op->kind == T2OpKind::Copy &&
                            op->num_operands == 1) {
                            const T2Op *def = op->operands[0]->def;

                            /* ConstLiteral is excluded: a literal read
                             * from a register may not be expressible
                             * as a bare immediate (dynamic literals),
                             * and losing the register source would
                             * fail isel where the move lowered fine. */
                            if (def != nullptr && is_const_kind(def->kind) &&
                                def->kind != T2OpKind::ConstLiteral &&
                                def->kind != T2OpKind::ConstFloat &&
                                ((op->flags | def->flags) & T2_OP_RAW_MODE) ==
                                        0) {
                                /* Raw-mode consts/copies (P2 loop
                                 * unboxing) carry the tag-cleared
                                 * representation — never foldable to a
                                 * tagged-convention constant. */
                                trace("constfold copy-of-const", op);
                                fold_to_const(op, def);
                                n_fold++;
                                changed = true;
                            }
                            continue;
                        }

                        if (op->kind == T2OpKind::AddSmall ||
                            op->kind == T2OpKind::SubSmall) {
                            if (op->num_operands != 2 ||
                                (op->flags & T2_OP_RAW_MODE) != 0) {
                                /* A raw-mode op's result is the tag-
                                 * cleared representation; folding it to
                                 * a (tagged-convention) ConstInt would
                                 * change what its consumers read. */
                                continue;
                            }

                            const T2Op *a = op->operands[0]->def;
                            const T2Op *c = op->operands[1]->def;

                            if (a == nullptr || c == nullptr ||
                                a->kind != T2OpKind::ConstInt ||
                                c->kind != T2OpKind::ConstInt ||
                                !IS_SSMALL(a->imm_int) ||
                                !IS_SSMALL(c->imm_int)) {
                                continue;
                            }

                            Sint64 r = op->kind == T2OpKind::AddSmall
                                               ? a->imm_int + c->imm_int
                                               : a->imm_int - c->imm_int;

                            if (!IS_SSMALL(r)) {
                                continue; /* the guard would fire */
                            }

                            trace("constfold arith", op);
                            op->kind = T2OpKind::ConstInt;
                            op->imm_int = r;
                            op->imm_term = 0; /* as the builder leaves it */
                            op->type = T2Type::integer(r, r);
                            if (op->result != nullptr) {
                                op->result->type = op->type;
                            }
                            op->num_operands = 0;
                            op->operands = nullptr;
                            op->operand_regs = nullptr;
                            op->sync = nullptr;
                            op->flags &= (uint16_t)~(
                                    T2_OP_SPEC_BOUNDARY | T2_OP_SPEC_CALLSITE |
                                    T2_OP_SPEC_ENTRY | T2_OP_WINDOW_CALLEE |
                                    T2_OP_NO_OVF);
                            n_fold++;
                            changed = true;
                        }
                    }
                }
            }

            /* ================================================================
             * 3b  copy propagation (+ single-input-phi collapse)
             * ================================================================
             */

            void run_copyprop() {
                if (copyprop_disabled()) {
                    return;
                }

                /* Single-input phis are register identities: the one
                 * predecessor materialized the input in the phi's home,
                 * so every reference (operands, sync claims) can name
                 * the input directly — same slots, same terms. */
                for (T2BasicBlock *b : fn.blocks) {
                    if (b->num_preds != 1) {
                        continue;
                    }

                    T2Op *phi = b->phis_head;

                    while (phi != nullptr) {
                        T2Op *next = phi->next;

                        if (phi->num_operands == 1 &&
                            phi->operands[0] != phi->result) {
                            trace("copyprop single-input phi", phi);
                            replace_value(phi->result, phi->operands[0]);
                            unlink_phi(b, phi);
                            n_phi++;
                            changed = true;
                        }
                        phi = next;
                    }
                }

                /* Forward operand reads through register copies where
                 * the source is provably still in its canonical home. */
                for (T2BasicBlock *b : fn.blocks) {
                    for (T2Op *op = b->ops_head; op != nullptr; op = op->next) {
                        if (!operand_rewrite_ok(op)) {
                            continue;
                        }
                        for (uint16_t i = 0; i < op->num_operands; i++) {
                            const T2Op *d = op->operands[i]->def;

                            if (d == nullptr || d->kind != T2OpKind::Copy ||
                                d->num_operands != 1 ||
                                d->dst_reg == T2_REG_NONE) {
                                continue;
                            }
                            if (op->operand_regs == nullptr ||
                                op->operand_regs[i] != d->dst_reg) {
                                continue;
                            }

                            T2Value *w = d->operands[0];
                            int32_t sr = d->operand_regs != nullptr
                                                 ? d->operand_regs[0]
                                                 : T2_REG_NONE;

                            /* Only forward register-homed sources: a
                             * constant source would become an inline
                             * immediate, which several consumers
                             * (guards, flatmap loads) reject in isel;
                             * copy-of-const is the fold's job. */
                            if (sr == T2_REG_NONE || w->def == nullptr ||
                                w->def->dst_reg != sr) {
                                continue;
                            }
                            if (!value_available_at(w, sr, op)) {
                                continue;
                            }

                            trace("copyprop operand", op);
                            op->operands[i] = w;
                            op->operand_regs[i] = sr;
                            n_copy++;
                            changed = true;
                        }
                    }
                }
            }

            /* ================================================================
             * 3c  CSE / GVN-lite
             * ================================================================
             */

            /* Kinds admitted to value numbering. Comparisons are
             * excluded (isel fuses a guard into its block's branch and
             * requires the single-use-in-block shape); Copy is excluded
             * (two moves to different homes are both real). */
            static bool cse_kind(T2OpKind k) {
                switch (k) {
                case T2OpKind::ConstInt:
                case T2OpKind::ConstAtom:
                case T2OpKind::ConstNil:
                case T2OpKind::ConstLiteral:
                case T2OpKind::FlatmapSize:
                case T2OpKind::FlatmapKeyAt:
                case T2OpKind::FlatmapValAt:
                case T2OpKind::GetTupleElement:
                    return true;
                default:
                    return false;
                }
            }

            static std::string cse_key(const T2Op *op) {
                char buf[96];
                std::string key;

                /* P2 loop unboxing: a RAW_MODE op produces/consumes the
                 * tag-cleared representation — never CSE-equivalent to
                 * its tagged twin (e.g. raw ConstInt 0 vs small 0). */
                snprintf(buf,
                         sizeof(buf),
                         "%u:%lld:%lx:%u:%u",
                         (unsigned)op->kind,
                         (long long)op->imm_int,
                         (unsigned long)op->imm_term,
                         op->index,
                         (unsigned)(op->flags & T2_OP_RAW_MODE));
                key = buf;
                for (uint16_t i = 0; i < op->num_operands; i++) {
                    snprintf(buf, sizeof(buf), ":v%u", op->operands[i]->id);
                    key += buf;
                }
                return key;
            }

            bool try_cse_merge(T2Op *sv, T2Op *vc) {
                T2Value *sval = sv->result;
                T2Value *vval = vc->result;

                if (sync_ref[vval->id] || phi_ref[vval->id]) {
                    return false;
                }
                if (!sval->type.equals(vval->type)) {
                    return false;
                }
                if (!def_precedes(sv, vc)) {
                    return false;
                }

                bool inline_const = is_const_kind(sv->kind) &&
                                    sv->dst_reg == T2_REG_NONE &&
                                    vc->dst_reg == T2_REG_NONE;

                std::vector<Use> uses;

                collect_uses(vval, &uses, nullptr);

                /* All-or-nothing: a partial merge keeps the victim
                 * alive and saves nothing. */
                for (const Use &u : uses) {
                    if (u.op->kind == T2OpKind::Phi) {
                        return false;
                    }
                    if (inline_const) {
                        /* The immediate the backend inlines is
                         * identical, so any consumer is fine EXCEPT
                         * ops whose sync map pins x[i] == operands[i]
                         * (calls, returns, demotes): swapping the
                         * value there without rewriting the map breaks
                         * the tie the validator enforces. */
                        switch (u.op->kind) {
                        case T2OpKind::Call:
                        case T2OpKind::CallExt:
                        case T2OpKind::CallFun:
                        case T2OpKind::TailCall:
                        case T2OpKind::TailCallExt:
                        case T2OpKind::TailCallFun:
                        case T2OpKind::Return:
                        case T2OpKind::ReductionCheck:
                        case T2OpKind::DemoteCallee:
                            return false;
                        default:
                            break;
                        }
                        if (u.op->operand_regs != nullptr &&
                            u.op->operand_regs[u.idx] != T2_REG_NONE) {
                            return false;
                        }
                        continue;
                    }
                    if (!operand_rewrite_ok(u.op)) {
                        return false;
                    }
                    if (sv->dst_reg == T2_REG_NONE ||
                        vc->dst_reg == T2_REG_NONE ||
                        u.op->operand_regs == nullptr ||
                        u.op->operand_regs[u.idx] != vc->dst_reg) {
                        return false;
                    }
                    if (!value_available_at(sval, sv->dst_reg, u.op)) {
                        return false;
                    }
                }

                for (const Use &u : uses) {
                    u.op->operands[u.idx] = sval;
                    if (!inline_const) {
                        u.op->operand_regs[u.idx] = sv->dst_reg;
                    }
                }

                trace("cse", vc);
                n_cse++;
                changed = true;
                /* The victim is now unused; DCE reaps it (and its
                 * feeders) on the next fixpoint round. */
                return true;
            }

            void run_cse() {
                if (cse_disabled()) {
                    return;
                }
                count_uses();
                compute_dominators();

                std::unordered_map<std::string, std::vector<T2Op *>> table;

                for (T2BasicBlock *b : fn.blocks) {
                    if (!reach[b->id]) {
                        continue;
                    }
                    for (T2Op *op = b->ops_head; op != nullptr; op = op->next) {
                        if (op->result == nullptr || !cse_kind(op->kind) ||
                            is_pair_member(op)) {
                            continue;
                        }
                        table[cse_key(op)].push_back(op);
                    }
                }

                for (auto &group : table) {
                    std::vector<T2Op *> &ops = group.second;

                    if (ops.size() < 2) {
                        continue;
                    }
                    for (T2Op *vc : ops) {
                        for (T2Op *sv : ops) {
                            if (sv == vc || sv->block == nullptr ||
                                vc->block == nullptr) {
                                continue;
                            }
                            if (try_cse_merge(sv, vc)) {
                                break;
                            }
                        }
                    }
                }
            }

            /* ================================================================
             * P3a  redundant speculative-guard elimination
             * ================================================================
             *
             * A SpeculateType guard's only runtime effect is its
             * conditional deopt; when every operand is already proven
             * SMALL just before it — by a dominating guard on the same
             * SSA value, a small constant, the committed result of a
             * flag-checked op, or the phi AND-rule over all incoming
             * edges — the deopt can never fire and the guard is a pure
             * no-op, so removing it is observationally invisible for
             * every deopt class (window/boundary/callsite/entry/
             * redispatch alike: a deopt that cannot fire has no
             * contract left to honor).
             *
             * The facts engine below is the validator's own
             * speculative-type dataflow (run_speculation_checks,
             * t2_hir.cpp) mirrored BY HAND — keep them in lockstep; if
             * they ever drift, the failure mode is benign in exactly
             * one direction (this pass weaker: missed removals) and
             * loud in the other (a wrong removal fails the post-rewrite
             * t2_validate and the function degrades to T1).
             *
             * Batch removal within one fixpoint is sound: a removable
             * guard's transfer adds nothing (its facts already held
             * before it), so dropping any subset of removable guards
             * leaves the fixpoint solution unchanged; and a fact can
             * never justify itself around a loop, because the entry
             * block seeds no facts and joins intersect — every true
             * fact is anchored on ops present on ALL entering paths. */

            struct P3Facts {
                std::vector<uint64_t> small, raw;
            };

            static void p3_set(std::vector<uint64_t> &s, uint32_t v) {
                s[v / 64] |= uint64_t(1) << (v % 64);
            }
            static bool p3_test(const std::vector<uint64_t> &s, uint32_t v) {
                return (s[v / 64] >> (v % 64)) & 1;
            }

            /* One op's fact effects — the validator's spec_transfer_op,
             * mirrored (see the lockstep note above). */
            static void p3_transfer_op(const T2Op *op, P3Facts &f) {
                switch (op->kind) {
                case T2OpKind::ConstInt:
                    if ((op->flags & T2_OP_RAW_MODE) != 0) {
                        p3_set(f.raw, op->result->id);
                    } else if (IS_SSMALL(op->imm_int)) {
                        p3_set(f.small, op->result->id);
                    }
                    break;
                case T2OpKind::Copy:
                    if (p3_test(f.small, op->operands[0]->id) ||
                        t2_type_proves_small(op->operands[0]->type)) {
                        p3_set(f.small, op->result->id);
                    }
                    if (p3_test(f.raw, op->operands[0]->id)) {
                        p3_set(f.raw, op->result->id);
                    }
                    break;
                case T2OpKind::SpeculateType:
                    for (uint16_t i = 0; i < op->num_operands; i++) {
                        p3_set(f.small, op->operands[i]->id);
                    }
                    break;
                case T2OpKind::AddSmall:
                case T2OpKind::SubSmall:
                    if ((op->flags & T2_OP_RAW_MODE) != 0) {
                        p3_set(f.raw, op->result->id);
                    } else {
                        p3_set(f.small, op->result->id);
                    }
                    break;
                case T2OpKind::TagInt:
                    p3_set(f.small, op->result->id);
                    break;
                case T2OpKind::FlatmapSize:
                    if ((op->flags & T2_OP_RAW_MODE) != 0) {
                        p3_set(f.raw, op->result->id);
                    }
                    break;
                case T2OpKind::UntagInt:
                case T2OpKind::MulRaw:
                    p3_set(f.raw, op->result->id);
                    break;
                default:
                    break;
                }
            }

            void run_p3_guard_elim() {
                if (p3_disabled()) {
                    return;
                }

                bool any = false;

                for (const T2BasicBlock *b : fn.blocks) {
                    for (const T2Op *op = b->ops_head; op != nullptr && !any;
                         op = op->next) {
                        any = op->kind == T2OpKind::SpeculateType;
                    }
                    if (any) {
                        break;
                    }
                }
                if (!any) {
                    return;
                }

                compute_dominators(); /* for `reach` */

                size_t n = fn.blocks.size();
                size_t vwords = (fn.values.size() + 63) / 64;
                std::vector<P3Facts> out(n);

                /* Optimistic init (everything proven except at the
                 * entry), then intersect down to the fixpoint — the
                 * validator's exact scheme. */
                for (size_t i = 0; i < n; i++) {
                    bool top = reach[i] && i != 0;

                    out[i].small.assign(vwords, top ? ~uint64_t(0) : 0);
                    out[i].raw.assign(vwords, top ? ~uint64_t(0) : 0);
                }

                auto block_in = [&](const T2BasicBlock *b) {
                    P3Facts in;

                    in.small.assign(vwords, b->id == 0 ? 0 : ~uint64_t(0));
                    in.raw.assign(vwords, b->id == 0 ? 0 : ~uint64_t(0));
                    if (b->id == 0) {
                        return in;
                    }

                    bool any_pred = false;

                    for (uint32_t p = 0; p < b->num_preds; p++) {
                        const T2BasicBlock *pred = b->preds[p];

                        if (!reach[pred->id]) {
                            continue;
                        }
                        any_pred = true;
                        for (size_t w = 0; w < vwords; w++) {
                            in.small[w] &= out[pred->id].small[w];
                            in.raw[w] &= out[pred->id].raw[w];
                        }
                    }
                    if (!any_pred) {
                        in.small.assign(vwords, 0);
                        in.raw.assign(vwords, 0);
                    }
                    return in;
                };

                auto apply_phis = [&](const T2BasicBlock *b, P3Facts &f) {
                    for (const T2Op *phi = b->phis_head; phi != nullptr;
                         phi = phi->next) {
                        bool all_small = phi->num_operands > 0;
                        bool all_raw = phi->num_operands > 0;

                        for (uint16_t i = 0; i < phi->num_operands; i++) {
                            const T2BasicBlock *pred = phi->phi_blocks[i];

                            if (!reach[pred->id]) {
                                continue;
                            }

                            uint32_t vid = phi->operands[i]->id;

                            all_small = all_small &&
                                        (p3_test(out[pred->id].small, vid) ||
                                         t2_type_proves_small(
                                                 phi->operands[i]->type));
                            all_raw =
                                    all_raw && p3_test(out[pred->id].raw, vid);
                        }
                        if (all_small) {
                            p3_set(f.small, phi->result->id);
                        }
                        if (all_raw) {
                            p3_set(f.raw, phi->result->id);
                        }
                    }
                };

                bool ch = true;

                while (ch) {
                    ch = false;
                    for (const T2BasicBlock *b : fn.blocks) {
                        if (!reach[b->id]) {
                            continue;
                        }

                        P3Facts f = block_in(b);

                        apply_phis(b, f);
                        for (const T2Op *op = b->ops_head; op != nullptr;
                             op = op->next) {
                            p3_transfer_op(op, f);
                        }
                        if (f.small != out[b->id].small ||
                            f.raw != out[b->id].raw) {
                            out[b->id] = std::move(f);
                            ch = true;
                        }
                    }
                }

                /* Exact pass at the fixpoint: drop every guard whose
                 * operands are all proven just before it. A removed
                 * guard's transfer is skipped, which is equivalent —
                 * its facts were already in `f`. */
                for (T2BasicBlock *b : fn.blocks) {
                    if (!reach[b->id]) {
                        continue;
                    }

                    P3Facts f = block_in(b);

                    apply_phis(b, f);

                    T2Op *op = b->ops_head;

                    while (op != nullptr) {
                        T2Op *next = op->next;

                        if (op->kind == T2OpKind::SpeculateType &&
                            !is_pair_member(op)) {
                            bool proven = true;

                            for (uint16_t i = 0; proven && i < op->num_operands;
                                 i++) {
                                proven =
                                        p3_test(f.small, op->operands[i]->id) ||
                                        t2_type_proves_small(
                                                op->operands[i]->type);
                            }
                            if (proven) {
                                trace_p3("p3 guard-elim", op);
                                unlink_body(b, op);
                                n_p3g++;
                                erts_t2_opt_stats.p3_guards_removed++;
                                changed = true;
                                op = next;
                                continue;
                            }
                        }
                        p3_transfer_op(op, f);
                        op = next;
                    }
                }
            }

            /* ================================================================
             * P3b  range-based overflow-guard elimination
             * ================================================================
             *
             * Mark every AddSmall whose overflow deopt provably cannot
             * fire (t2_addsub_no_ovf_provable: the bounded-IV shape of
             * the maps:fold flatmap loop — increment by a small
             * positive constant under a dominating i < FlatmapSize
             * bound with the map IsFlatmapBounded-guarded, so the
             * result stays <= MAP_SMALL_MAP_LIMIT + c, far inside the
             * small range). The emitter then omits the b.vs and its
             * trampoline. The ACCUMULATOR's overflow guard has no such
             * bound (element magnitudes are unbounded) and is never
             * touched: the prover requires the constant-increment +
             * loop-bound shape, which a data-dependent sum can never
             * satisfy. The validator re-proves every claimed flag. */
            void run_p3_no_ovf() {
                if (p3_disabled()) {
                    return;
                }

                for (T2BasicBlock *b : fn.blocks) {
                    for (T2Op *op = b->ops_head; op != nullptr; op = op->next) {
                        if (op->kind != T2OpKind::AddSmall ||
                            (op->flags & T2_OP_NO_OVF) != 0) {
                            continue;
                        }
                        if (!t2_addsub_no_ovf_provable(fn, op)) {
                            continue;
                        }
                        trace_p3("p3 no-ovf", op);
                        op->flags |= T2_OP_NO_OVF;
                        n_p3o++;
                        erts_t2_opt_stats.p3_iv_ovf_removed++;
                        changed = true;
                    }
                }
            }

            /* ================================================================
             * 3d  make_fun sinking
             * ================================================================
             */

            /* The post-transform entry-recall dirt predicate, mirrored
             * by the validator's rule (t2_validate_windows): re-running
             * the whole invocation from the T1 entry body is legal only
             * while nothing effectful/frame-moving/charging ran and the
             * argument vector X0..arity-1 is untouched. `converts` are
             * the ops about to join the entry class (their own commits
             * are exempt, as is the accepted FoldBudget batch-charge
             * deviation); `moved` are the ops leaving the fast path. */
            bool sink_path_dirt(const T2Op *op,
                                const std::vector<T2Op *> &converts,
                                const T2Op *mf,
                                const T2Op *cp,
                                const T2Op *g,
                                uint32_t fun_words) const {
                if (op == mf || op == cp) {
                    return false;
                }
                if (op == g) {
                    return op->index != fun_words && op->live < fn.arity;
                }
                if (op->kind == T2OpKind::Param) {
                    return false;
                }
                for (const T2Op *c : converts) {
                    if (op == c) {
                        return false;
                    }
                }
                if (op->kind == T2OpKind::ReductionCheck) {
                    /* A back-edge charge: entry re-execution would pay
                     * it again. */
                    return true;
                }
                return t2_op_dirties_window(op, fn.arity);
            }

            bool sink_one(T2Op *mf) {
                T2BasicBlock *b0 = fn.blocks[0];

                if (mf->block != b0 || mf->dst_reg == T2_REG_NONE ||
                    mf->sync != nullptr || is_pair_member(mf) ||
                    !t2_reg_is_x(mf->dst_reg)) {
                    return false;
                }

                uint32_t fun_words = (uint32_t)ERL_FUN_SIZE + mf->num_operands;

                /* --- the fun's single consumer: one Copy in b0 ------ */
                std::vector<Use> mf_uses;
                std::vector<T2Op *> mf_sync;

                collect_uses(mf->result, &mf_uses, &mf_sync);
                if (mf_uses.size() != 1 || !mf_sync.empty()) {
                    return false;
                }

                T2Op *cp = mf_uses[0].op;

                if (cp->kind != T2OpKind::Copy || cp->block != b0 ||
                    cp->num_operands != 1 || cp->sync != nullptr ||
                    is_pair_member(cp) || cp->operand_regs == nullptr ||
                    cp->operand_regs[0] != mf->dst_reg ||
                    !op_precedes(mf, cp)) {
                    return false;
                }

                /* --- the copy's consumers: one slow block + the
                 *     callsite-class deopts to convert -------------- */
                std::vector<Use> cp_uses;
                std::vector<T2Op *> cp_sync;

                collect_uses(cp->result, &cp_uses, &cp_sync);
                if (cp_uses.empty()) {
                    return false;
                }

                T2BasicBlock *S = cp_uses[0].op->block;

                if (S == nullptr || S == b0 || S->num_preds != 1 ||
                    S->preds[0] != b0 || S->phis_head != nullptr) {
                    return false;
                }
                for (const Use &u : cp_uses) {
                    if (u.op->block != S || u.op->kind == T2OpKind::Phi) {
                        return false;
                    }
                }

                std::vector<T2Op *> converts;

                for (T2Op *su : cp_sync) {
                    if (su->block == S) {
                        continue;
                    }
                    if ((su->flags & T2_OP_SPEC_CALLSITE) == 0) {
                        return false;
                    }
                    switch (su->kind) {
                    case T2OpKind::FoldBudget:
                    case T2OpKind::SpeculateType:
                    case T2OpKind::AddSmall:
                    case T2OpKind::SubSmall:
                        converts.push_back(su);
                        break;
                    default:
                        return false;
                    }
                }

                /* --- the erased call in S (its sync names the
                 *     boundary's live X prefix) --------------------- */
                const T2Op *call = nullptr;

                for (const Use &u : cp_uses) {
                    switch (u.op->kind) {
                    case T2OpKind::Call:
                    case T2OpKind::CallExt:
                    case T2OpKind::CallFun:
                    case T2OpKind::TailCall:
                    case T2OpKind::TailCallExt:
                    case T2OpKind::TailCallFun:
                        if (u.op->sync != nullptr) {
                            call = u.op;
                        }
                        break;
                    default:
                        break;
                    }
                }
                if (call == nullptr || call->sync->frame_size != T2_NO_FRAME) {
                    return false;
                }

                /* --- no frame motion and no conditional-write op in
                 *     the entry block (a gc_bif whose result commits
                 *     only on the Succeeded edge would falsify the
                 *     plain register walk below) ------------------- */
                for (const T2Op *p = b0->ops_head; p != nullptr; p = p->next) {
                    switch (p->kind) {
                    case T2OpKind::Allocate:
                    case T2OpKind::Deallocate:
                    case T2OpKind::Trim:
                    case T2OpKind::Succeeded:
                        return false;
                    default:
                        break;
                    }
                }

                /* --- the entry GcTest covering the fun words -------- */
                T2Op *g = nullptr;

                for (T2Op *p = b0->ops_head; p != nullptr && p != mf;
                     p = p->next) {
                    if (p->kind == T2OpKind::GcTest && p->index >= fun_words) {
                        g = p;
                    }
                }
                if (g == nullptr) {
                    return false;
                }

                /* --- free variables: X-homed, canonical ------------- */
                for (uint16_t j = 0; j < mf->num_operands; j++) {
                    int32_t rj = mf->operand_regs != nullptr
                                         ? mf->operand_regs[j]
                                         : T2_REG_NONE;

                    if (rj == T2_REG_NONE || !t2_reg_is_x(rj)) {
                        return false;
                    }
                }

                /* --- entry-block exit register state (mf/cp gone) --- */
                std::unordered_map<int32_t, T2Value *> state;

                for (T2Op *p = b0->ops_head; p != nullptr; p = p->next) {
                    if (p == mf || p == cp) {
                        continue;
                    }
                    if (p->result != nullptr && p->dst_reg != T2_REG_NONE) {
                        state[p->dst_reg] = p->result;
                    }
                }

                for (uint16_t j = 0; j < mf->num_operands; j++) {
                    auto it = state.find(mf->operand_regs[j]);

                    if (it == state.end() || it->second != mf->operands[j]) {
                        return false;
                    }
                }

                uint32_t live = call->sync->x_live;

                for (uint16_t j = 0; j < mf->num_operands; j++) {
                    uint32_t idx = t2_reg_index(mf->operand_regs[j]);

                    if (idx + 1 > live) {
                        live = idx + 1;
                    }
                }

                std::vector<T2Value *> gc_claims(live, nullptr);

                for (uint32_t i = 0; i < live; i++) {
                    auto it = state.find(t2_xreg(i));

                    if (it == state.end()) {
                        return false;
                    }
                    const T2Op *d = it->second->def;

                    if (d != nullptr && (d->kind == T2OpKind::UntagInt ||
                                         d->kind == T2OpKind::MulRaw ||
                                         (d->flags & T2_OP_RAW_MODE) != 0)) {
                        return false; /* never a GC-visible slot */
                    }
                    gc_claims[i] = it->second;
                }

                /* --- a fresh X home for the sunk make_fun ----------- */
                uint32_t max_xlive = live;

                for (const T2Op *p = S->ops_head; p != nullptr; p = p->next) {
                    if (p->sync != nullptr && p->sync->x_live > max_xlive) {
                        max_xlive = p->sync->x_live;
                    }
                }
                if (S->terminator != nullptr &&
                    S->terminator->sync != nullptr &&
                    S->terminator->sync->x_live > max_xlive) {
                    max_xlive = S->terminator->sync->x_live;
                }

                auto reg_read_in_S = [&](int32_t r) {
                    auto reads = [&](const T2Op *p) {
                        if (p->operand_regs == nullptr) {
                            return false;
                        }
                        for (uint16_t i = 0; i < p->num_operands; i++) {
                            if (p->operand_regs[i] == r) {
                                return true;
                            }
                        }
                        return false;
                    };

                    for (const T2Op *p = S->ops_head; p != nullptr;
                         p = p->next) {
                        if (reads(p)) {
                            return true;
                        }
                    }
                    return S->terminator != nullptr && reads(S->terminator);
                };

                int32_t rx = T2_REG_NONE;

                for (uint32_t cand = max_xlive; cand < max_xlive + 16; cand++) {
                    if (!reg_read_in_S(t2_xreg(cand))) {
                        rx = t2_xreg(cand);
                        break;
                    }
                }
                if (rx == T2_REG_NONE) {
                    return false;
                }

                /* No existing op in S may read the copy's destination
                 * expecting an older value (the sunk copy will write it
                 * first). */
                {
                    auto reads_other = [&](const T2Op *p) {
                        if (p->operand_regs == nullptr) {
                            return false;
                        }
                        for (uint16_t i = 0; i < p->num_operands; i++) {
                            if (p->operand_regs[i] == cp->dst_reg &&
                                p->operands[i] != cp->result) {
                                return true;
                            }
                        }
                        return false;
                    };

                    for (const T2Op *p = S->ops_head; p != nullptr;
                         p = p->next) {
                        if (reads_other(p)) {
                            return false;
                        }
                    }
                    if (S->terminator != nullptr &&
                        reads_other(S->terminator)) {
                        return false;
                    }
                }

                /* --- the params (the entry-window deopt vector) ----- */
                std::vector<T2Value *> params(fn.arity, nullptr);

                for (const T2Op *p = b0->ops_head; p != nullptr; p = p->next) {
                    if (p->kind == T2OpKind::Param && p->index < fn.arity) {
                        params[p->index] = p->result;
                    }
                }
                for (uint32_t i = 0; i < fn.arity; i++) {
                    if (params[i] == nullptr) {
                        return false;
                    }
                }

                /* --- entry-recall legality for the converts:
                 *     every path from entry to each convert is clean
                 *     (fixpoint over the whole CFG — loop back edges
                 *     propagate, so prior iterations count too) ------ */
                {
                    size_t n = fn.blocks.size();
                    std::vector<uint8_t> dirty_in(n, 0);
                    auto block_out = [&](const T2BasicBlock *b, uint8_t flag) {
                        for (const T2Op *phi = b->phis_head; phi != nullptr;
                             phi = phi->next) {
                            if (sink_path_dirt(phi,
                                               converts,
                                               mf,
                                               cp,
                                               g,
                                               fun_words)) {
                                flag = 1;
                            }
                        }
                        for (const T2Op *p = b->ops_head; p != nullptr;
                             p = p->next) {
                            if (sink_path_dirt(p,
                                               converts,
                                               mf,
                                               cp,
                                               g,
                                               fun_words)) {
                                flag = 1;
                            }
                        }
                        return flag;
                    };
                    bool ch = true;

                    while (ch) {
                        ch = false;
                        for (const T2BasicBlock *b : fn.blocks) {
                            uint8_t out = block_out(b, dirty_in[b->id]);

                            for_each_succ(b->terminator, [&](T2BasicBlock *s) {
                                if (s != nullptr && out && !dirty_in[s->id]) {
                                    dirty_in[s->id] = 1;
                                    ch = true;
                                }
                            });
                        }
                    }

                    for (const T2Op *c : converts) {
                        uint8_t flag = dirty_in[c->block->id];

                        for (const T2Op *phi = c->block->phis_head;
                             phi != nullptr && !flag;
                             phi = phi->next) {
                            if (sink_path_dirt(phi,
                                               converts,
                                               mf,
                                               cp,
                                               g,
                                               fun_words)) {
                                flag = 1;
                            }
                        }
                        for (const T2Op *p = c->block->ops_head;
                             p != nullptr && p != c && !flag;
                             p = p->next) {
                            if (sink_path_dirt(p,
                                               converts,
                                               mf,
                                               cp,
                                               g,
                                               fun_words)) {
                                flag = 1;
                            }
                        }
                        if (flag) {
                            return false;
                        }
                    }
                }

                /* ---------------- transform ------------------------- */

                /* 1. The slow block's own heap reservation, first. */
                T2SyncMap *gm = fn.arena.create<T2SyncMap>();

                gm->x_live = live;
                gm->x = fn.arena.alloc_array<T2Value *>(live);
                for (uint32_t i = 0; i < live; i++) {
                    gm->x[i] = gc_claims[i];
                }
                gm->frame_size = T2_NO_FRAME;
                gm->y = nullptr;

                T2Op *g2;

                if (g->index == fun_words) {
                    /* The entry test covered only the fun: move it. */
                    unlink_body(b0, g);
                    g2 = g;
                } else {
                    g->index -= fun_words;
                    g2 = fn.new_op(S, T2OpKind::GcTest, T2Type::none());
                    fn.set_operands(g2, {});
                    unlink_body(S, g2); /* re-inserted at the head below */
                    g2->index = fun_words;
                    g2->beam_idx = g->beam_idx;
                }
                g2->live = live;
                g2->sync = gm;

                /* 2. Sink the fun construction, re-homed clear of the
                 *    slow block's staged arguments. */
                unlink_body(b0, mf);
                unlink_body(b0, cp);
                mf->dst_reg = rx;
                cp->operand_regs[0] = rx;

                insert_head(S, cp);
                insert_head(S, mf);
                insert_head(S, g2);

                /* 3. Re-class the fast path's deopts: the call boundary
                 *    is gone; they re-execute the whole invocation from
                 *    the T1 entry body instead (fresh-call vector =
                 *    X0..arity-1, still physically intact — proven
                 *    above and re-proven by the validator's
                 *    entry-recall rule). */
                T2SyncMap *em = fn.arena.create<T2SyncMap>();

                em->x_live = fn.arity;
                em->x = fn.arena.alloc_array<T2Value *>(fn.arity);
                for (uint32_t i = 0; i < fn.arity; i++) {
                    em->x[i] = params[i];
                }
                em->frame_size = T2_NO_FRAME;
                em->y = nullptr;

                for (T2Op *c : converts) {
                    c->flags &= (uint16_t)~T2_OP_SPEC_CALLSITE;
                    c->flags |= T2_OP_SPEC_ENTRY;
                    c->sync = c->kind == T2OpKind::FoldBudget ? em : nullptr;
                }

                trace("sink make_fun -> slow block", mf);
                n_sink++;
                changed = true;
                return true;
            }

            void run_sink() {
                if (sink_disabled() || fn.blocks.empty() || !fn.sync_complete ||
                    fn.entry_sync == nullptr) {
                    return;
                }

                std::vector<T2Op *> mfs;

                for (T2Op *op = fn.blocks[0]->ops_head; op != nullptr;
                     op = op->next) {
                    if (op->kind == T2OpKind::MakeFun) {
                        mfs.push_back(op);
                    }
                }
                for (T2Op *mf : mfs) {
                    (void)sink_one(mf);
                }
            }

            /* ================================================================
             * driver
             * ================================================================
             */

            bool run() {
                if (fn.blocks.empty()) {
                    return true;
                }

                run_sink();

                /* Fixpoint of the cleanup passes; each round only
                 * removes or simplifies, so this terminates — the cap
                 * is a safety net, not a budget. */
                for (int round = 0; round < 16; round++) {
                    bool before = changed;

                    changed = false;
                    run_dce();
                    run_constfold();
                    run_copyprop();
                    run_cse();
                    run_p3_guard_elim();
                    run_p3_no_ovf();

                    bool this_round = changed;

                    changed = before || changed;
                    if (!this_round) {
                        break;
                    }
                }

                if (opt_trace() && changed) {
                    erts_fprintf(stderr,
                                 "t2_opt: %T:%T/%u summary: dce=%u fold=%u "
                                 "copy=%u phi=%u cse=%u sink=%u p3g=%u "
                                 "p3o=%u\n",
                                 fn.module,
                                 fn.function,
                                 (unsigned)fn.arity,
                                 n_dce,
                                 n_fold,
                                 n_copy,
                                 n_phi,
                                 n_cse,
                                 n_sink,
                                 n_p3g,
                                 n_p3o);
                }
                return true;
            }
        };

    } /* anonymous namespace */

    bool t2_op_is_pure(T2OpKind kind) {
        switch (kind) {
        case T2OpKind::ConstInt:
        case T2OpKind::ConstFloat:
        case T2OpKind::ConstAtom:
        case T2OpKind::ConstNil:
        case T2OpKind::ConstLiteral:
        case T2OpKind::Copy:
        case T2OpKind::CmpEqExact:
        case T2OpKind::CmpNeExact:
        case T2OpKind::CmpEq:
        case T2OpKind::CmpNe:
        case T2OpKind::CmpLt:
        case T2OpKind::CmpLe:
        case T2OpKind::CmpGt:
        case T2OpKind::CmpGe:
        case T2OpKind::IsFlatmapBounded:
        case T2OpKind::FlatmapSize:
        case T2OpKind::FlatmapKeyAt:
        case T2OpKind::FlatmapValAt:
        /* Dead speculated arithmetic: the overflow guard is dead with
         * the result (see t2_opt.hpp). */
        case T2OpKind::AddSmall:
        case T2OpKind::SubSmall:
            return true;
        default:
            return false;
        }
    }

    /* ------------------------------------------------------------------ *
     * P3b shared prover (also the validator's re-proof; t2_hir.hpp)      *
     * ------------------------------------------------------------------ */

    /* Edge-sensitive single-bit "must hold" dataflow. The fact is
     * established on the TRUE edge out of `est` (whose terminator is a
     * two-way Branch with distinct successors, testing a condition
     * evaluated in `est` itself, so it holds for the CURRENT values on
     * exit), killed by any block in `kill` (a block whose execution
     * re-evaluates a value the fact talks about) and holds at a block
     * only when every reachable incoming edge carries it; the entry
     * block seeds nothing, so a cycle cannot justify itself — every
     * true fact is anchored on establishing edges crossed on ALL
     * entering paths since the last kill. Conservative in every
     * unknown: an unreachable/killed/est `at` is a NO. */
    static bool t2_edge_fact_holds_at(const T2Function &fn,
                                      const T2BasicBlock *est,
                                      const T2BasicBlock *est_true,
                                      const std::vector<uint8_t> &kill,
                                      const T2BasicBlock *at) {
        size_t n = fn.blocks.size();

        /* Reachability from the entry block. */
        std::vector<uint8_t> reach(n, 0);
        std::vector<const T2BasicBlock *> work{fn.blocks[0]};

        reach[0] = 1;
        while (!work.empty()) {
            const T2BasicBlock *b = work.back();
            const T2Op *t = b->terminator;

            work.pop_back();
            if (t == nullptr) {
                continue;
            }

            auto push = [&](const T2BasicBlock *s) {
                if (s != nullptr && !reach[s->id]) {
                    reach[s->id] = 1;
                    work.push_back(s);
                }
            };

            switch (t->kind) {
            case T2OpKind::Branch:
                push(t->succ_then);
                push(t->succ_else);
                break;
            case T2OpKind::Jump:
                push(t->succ_then);
                break;
            case T2OpKind::Switch:
                for (uint32_t i = 0; i < t->num_cases; i++) {
                    push(t->cases[i].target);
                }
                push(t->default_target);
                break;
            default:
                break;
            }
        }

        if (!reach[at->id] || at == est || kill[at->id]) {
            return false;
        }

        /* in[b]: the fact holds on b's entry on EVERY path. Optimistic
         * init, monotone intersection to the fixpoint. `est`'s outgoing
         * facts do not depend on in[est] (the branch re-establishes the
         * fact at exit regardless of entry state — kill-then-retest is
         * exactly the loop header's behavior). */
        std::vector<uint8_t> in(n, 1);

        in[0] = 0;

        auto edge_out = [&](const T2BasicBlock *p,
                            const T2BasicBlock *b) -> uint8_t {
            if (p == est) {
                return b == est_true ? 1 : 0;
            }
            if (kill[p->id]) {
                return 0;
            }
            return in[p->id];
        };

        bool ch = true;

        while (ch) {
            ch = false;
            for (const T2BasicBlock *b : fn.blocks) {
                if (!reach[b->id] || b->id == 0) {
                    continue;
                }

                uint8_t nv = 1;
                bool any_pred = false;

                for (uint32_t i = 0; i < b->num_preds; i++) {
                    const T2BasicBlock *p = b->preds[i];

                    if (!reach[p->id]) {
                        continue;
                    }
                    any_pred = true;
                    nv = (uint8_t)(nv & edge_out(p, b));
                }
                if (!any_pred) {
                    nv = 0;
                }
                if (nv != in[b->id]) {
                    in[b->id] = nv;
                    ch = true;
                }
            }
        }
        return in[at->id] != 0;
    }

    /* True when `op` (an AddSmall) provably cannot overflow the small
     * range, so its b.vs deopt may be omitted (T2_OP_NO_OVF).
     *
     * The proof — the bounded-IV shape of the maps:fold flatmap loop:
     *
     *   1. op = AddSmall(v, ConstInt c) with 0 < c and the static
     *      headroom MAP_SMALL_MAP_LIMIT + c a small. Adding a positive
     *      constant can only overflow UPWARD (a small plus a positive
     *      constant moving toward zero cannot leave the small range),
     *      so an upper bound on v alone suffices.
     *   2. Some block H ends in Branch(CmpLt(v, n)) — same-block cmp,
     *      distinct successors — with n a FlatmapSize result: on the
     *      true edge, value(v) < value(n) for the CURRENT evaluations.
     *      Numeric semantics hold on every execution that reaches op:
     *      the post-rewrite validator independently proves v SMALL/RAW
     *      at op on every path (run_speculation_checks; the same term
     *      was compared), n is a small by FlatmapSize's construction,
     *      and both the raw compare (value<<4, order-preserving) and
     *      the generic CmpLt order numbers numerically.
     *   3. n's map operand passed IsFlatmapBounded on an edge that
     *      must-reach n's load (edge dataflow; kill = the map value's
     *      def block), so value(n) <= MAP_SMALL_MAP_LIMIT — flatmap
     *      headers are immutable, GC moves but never resizes.
     *   4. The H-true fact must-holds at op (edge dataflow; kill = v's
     *      def block — for the loop IV that is the header itself, so
     *      each new evaluation of v is re-tested before op — and,
     *      conservatively, n's def block).
     *
     *   Therefore at op: value(v) < value(n) <= MAP_SMALL_MAP_LIMIT,
     *   and value(v) + c <= MAP_SMALL_MAP_LIMIT + c is a small — the
     *   flag-setting add's V bit cannot be set. The accumulator of a
     *   fold can never satisfy this shape (its increment is data-
     *   dependent, not a positive constant), so its overflow deopt is
     *   structurally out of reach of this prover — by design. */
    bool t2_addsub_no_ovf_provable(const T2Function &fn, const T2Op *op) {
        if (op == nullptr || op->block == nullptr ||
            op->kind != T2OpKind::AddSmall || op->num_operands != 2 ||
            fn.blocks.empty()) {
            return false;
        }

        const T2Value *lhs = op->operands[0];
        const T2Op *cdef = op->operands[1]->def;

        if (cdef == nullptr || cdef->kind != T2OpKind::ConstInt) {
            return false;
        }

        Sint64 c = cdef->imm_int;

        if (c <= 0 || c > 4096 || !IS_SSMALL((Sint64)MAP_SMALL_MAP_LIMIT + c)) {
            return false;
        }

        size_t n = fn.blocks.size();

        for (const T2BasicBlock *H : fn.blocks) {
            const T2Op *term = H->terminator;

            if (term == nullptr || term->kind != T2OpKind::Branch ||
                term->num_operands != 1 || term->succ_then == term->succ_else) {
                continue;
            }

            const T2Op *cmp = term->operands[0]->def;

            if (cmp == nullptr || cmp->kind != T2OpKind::CmpLt ||
                cmp->block != H || cmp->num_operands != 2 ||
                cmp->operands[0] != lhs) {
                continue;
            }

            const T2Op *ndef = cmp->operands[1]->def;

            if (ndef == nullptr || ndef->kind != T2OpKind::FlatmapSize ||
                ndef->block == nullptr || ndef->num_operands != 1) {
                continue;
            }

            /* 3. the bound: IsFlatmapBounded must-reaches n's load. */
            const T2Value *mapv = ndef->operands[0];
            std::vector<uint8_t> kill2(n, 0);

            if (mapv->def != nullptr && mapv->def->block != nullptr) {
                kill2[mapv->def->block->id] = 1;
            }

            bool bounded = false;

            for (const T2BasicBlock *Bg : fn.blocks) {
                const T2Op *bt = Bg->terminator;

                if (bt == nullptr || bt->kind != T2OpKind::Branch ||
                    bt->num_operands != 1 || bt->succ_then == bt->succ_else) {
                    continue;
                }

                const T2Op *g = bt->operands[0]->def;

                if (g == nullptr || g->kind != T2OpKind::IsFlatmapBounded ||
                    g->block != Bg || g->num_operands != 1 ||
                    g->operands[0] != mapv) {
                    continue;
                }
                if (t2_edge_fact_holds_at(fn,
                                          Bg,
                                          bt->succ_then,
                                          kill2,
                                          ndef->block)) {
                    bounded = true;
                    break;
                }
            }
            if (!bounded) {
                continue;
            }

            /* 4. the compare: H-true must-reaches op. */
            std::vector<uint8_t> kill1(n, 0);

            if (lhs->def != nullptr && lhs->def->block != nullptr) {
                kill1[lhs->def->block->id] = 1;
            }
            kill1[ndef->block->id] = 1;

            if (t2_edge_fact_holds_at(fn,
                                      H,
                                      term->succ_then,
                                      kill1,
                                      op->block)) {
                return true;
            }
        }
        return false;
    }

    bool t2_opt(T2Function &fn,
                const T2LoopInfo &li,
                bool *changed,
                std::string *err) {
        /* The CFG is never edited, so the caller's `li` stays valid;
         * the entry-recall dirt analysis is CFG-global and needs no
         * loop side data. No internal failure mode yet. */
        (void)li;
        (void)err;

        *changed = false;

        OptPass pass(fn);

        if (!pass.run()) {
            return false;
        }
        *changed = pass.changed;
        return true;
    }

} /* namespace erts_t2 */
