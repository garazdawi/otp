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
 * T2-Full tier-2 JIT: local leaf inlining + loop shape-up. See
 * t2_inline.hpp.
 */

#include "t2_inline.hpp"

extern "C"
{
#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#include "sys.h"
#include "global.h"
}

#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace erts_t2 {

    namespace {

        constexpr uint32_t T2_INLINE_MAX_CALLEE_OPS = 24;

        bool op_is_call_class(const T2Op *op) {
            switch (op->kind) {
            case T2OpKind::Call:
            case T2OpKind::CallExt:
            case T2OpKind::CallFun:
            case T2OpKind::Bif:
            case T2OpKind::GuardBif:
                return true;
            default:
                return false;
            }
        }

        /* Ops a callee may consist of (besides the leading Params).
         * Add/Sub are admissible because the speculation pass can
         * always window-convert them (their operands are guardable
         * register values by construction). */
        bool callee_op_ok(const T2Op *op) {
            switch (op->kind) {
            case T2OpKind::ConstInt:
            case T2OpKind::ConstAtom:
            case T2OpKind::ConstNil:
            case T2OpKind::ConstLiteral:
            case T2OpKind::Copy:
            case T2OpKind::GetTupleElement:
            case T2OpKind::GetHd:
            case T2OpKind::GetTl:
                return true;
            case T2OpKind::Add:
            case T2OpKind::Sub:
                return op->num_operands == 2 && op->result != nullptr &&
                       op->dst_reg != T2_REG_NONE && op->flags == 0;
            default:
                return false;
            }
        }

        /* --- intrusive-list helpers ---------------------------------- */

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

        /* Move `op` (freshly appended at b's tail by new_op) to sit
         * directly before `before` in the same block. */
        void move_before(T2BasicBlock *b, T2Op *before, T2Op *op) {
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

        const T2Value *resolve_copy_defs(const T2Value *v) {
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

        struct Inliner {
            T2Function &fn;
            const T2LoopInfo &li;
            const ErtsT2RetainedCode *ret;
            std::string *err;
            bool changed = false;

            bool fail(const char *what) {
                if (err != nullptr) {
                    *err = std::string("inliner: ") + what;
                }
                return false;
            }

            /* --- caller-side admission -------------------------------- */

            struct Site {
                const T2Loop *loop = nullptr;
                T2Op *call = nullptr;
                T2Op *alloc = nullptr; /* may be null (frame-free)     */
                T2Op *dealloc = nullptr;
            };

            /* Is `op` an Add/Sub the speculation pass can convert? */
            static bool convertible_arith(const T2Op *op) {
                if (op->kind != T2OpKind::Add && op->kind != T2OpKind::Sub) {
                    return false;
                }
                if (op->num_operands != 2 || op->result == nullptr ||
                    op->dst_reg == T2_REG_NONE) {
                    return false;
                }
                if (op->next != nullptr &&
                    op->next->kind == T2OpKind::Succeeded) {
                    return false;
                }
                return true;
            }

            bool admit_loop(const T2Loop &loop, Site *site) {
                std::vector<T2Op *> calls, allocs, deallocs;

                site->loop = &loop;

                for (uint32_t bid : loop.body) {
                    T2BasicBlock *b = fn.blocks[bid];

                    for (T2Op *op = b->ops_head; op != nullptr; op = op->next) {
                        if (op_is_call_class(op)) {
                            calls.push_back(op);
                        }
                        switch (op->kind) {
                        case T2OpKind::Allocate:
                            allocs.push_back(op);
                            break;
                        case T2OpKind::Deallocate:
                            deallocs.push_back(op);
                            break;
                        case T2OpKind::Trim:
                        case T2OpKind::GcTest:
                            /* Trim renumbers slots; a GcTest's live
                             * count would not cover retargeted
                             * scratch. Both: leave the loop alone. */
                            return false;
                        default:
                            break;
                        }

                        /* Y-slot writes must be fill Copies; Y-slot
                         * reads must root at an X-homed/const value
                         * through Copies (the copy-prop precondition). */
                        if (op->dst_reg != T2_REG_NONE &&
                            t2_reg_is_y(op->dst_reg) &&
                            op->kind != T2OpKind::Copy) {
                            return false;
                        }
                        if (op->operand_regs != nullptr) {
                            for (uint16_t i = 0; i < op->num_operands; i++) {
                                if (op->operand_regs[i] == T2_REG_NONE ||
                                    !t2_reg_is_y(op->operand_regs[i])) {
                                    continue;
                                }
                                const T2Value *root =
                                        resolve_copy_defs(op->operands[i]);

                                if (is_const_def(root)) {
                                    continue;
                                }
                                if (root->def == nullptr ||
                                    root->def->dst_reg == T2_REG_NONE ||
                                    !t2_reg_is_x(root->def->dst_reg)) {
                                    return false;
                                }
                            }
                        }

                        /* Sync-carrying ops must be of the kinds whose
                         * maps survive or provably die (converted
                         * arith, the erased call, the elided
                         * Allocate, the back edge). */
                        if (op->sync != nullptr) {
                            switch (op->kind) {
                            case T2OpKind::Call:
                            case T2OpKind::Allocate:
                            case T2OpKind::ReductionCheck:
                                break;
                            case T2OpKind::Add:
                            case T2OpKind::Sub:
                                if (!convertible_arith(op)) {
                                    return false;
                                }
                                break;
                            default:
                                return false;
                            }
                        }
                    }

                    /* Terminators never read Y under the identity
                     * decode shapes we admit; keep it that way. */
                    const T2Op *t = b->terminator;
                    if (t != nullptr && t->operand_regs != nullptr) {
                        for (uint16_t i = 0; i < t->num_operands; i++) {
                            if (t->operand_regs[i] != T2_REG_NONE &&
                                t2_reg_is_y(t->operand_regs[i])) {
                                return false;
                            }
                        }
                    }
                }

                if (calls.size() != 1 || calls[0]->kind != T2OpKind::Call ||
                    calls[0]->flags != 0 || calls[0]->mfa_m != fn.module ||
                    calls[0]->sync == nullptr) {
                    return false;
                }
                /* The call must sit in the (single) latch block so the
                 * back edge can absorb the erased call's reduction
                 * charges — T1 charges 1 at the callee's i_test_yield
                 * and 1 at its return dispatch, per iteration. */
                if (loop.latches.size() != 1 ||
                    calls[0]->block->id != loop.latches[0]) {
                    return false;
                }
                site->call = calls[0];

                if (allocs.empty() && deallocs.empty()) {
                    return true;
                }
                if (allocs.size() != 1 || deallocs.size() != 1) {
                    return false;
                }
                site->alloc = allocs[0];
                site->dealloc = deallocs[0];

                /* One balanced, heap-free frame pair around the call,
                 * all in the call's block. */
                if (site->alloc->block != site->call->block ||
                    site->dealloc->block != site->call->block ||
                    site->alloc->index != site->dealloc->index ||
                    site->alloc->imm_int != 0) {
                    return false;
                }
                {
                    /* Order within the block: alloc < call < dealloc. */
                    int state = 0;

                    for (const T2Op *op = site->call->block->ops_head;
                         op != nullptr;
                         op = op->next) {
                        if (op == site->alloc && state == 0) {
                            state = 1;
                        } else if (op == site->call && state == 1) {
                            state = 2;
                        } else if (op == site->dealloc && state == 2) {
                            state = 3;
                        } else if (op == site->alloc || op == site->call ||
                                   op == site->dealloc) {
                            return false;
                        }
                    }
                    if (state != 3) {
                        return false;
                    }
                }
                return true;
            }

            /* --- callee-side admission + splice ------------------------ */

            bool splice_callee(Site &site, const T2Function &callee) {
                T2Op *call = site.call;
                T2BasicBlock *cb = callee.blocks[0];

                /* Map callee values to caller values; remember which
                 * are parameter passthroughs (their caller home at the
                 * splice point is the call's argument register). */
                std::unordered_map<const T2Value *, T2Value *> vmap;
                std::unordered_map<const T2Value *, int32_t> vhome;

                const T2Op *op = cb->ops_head;
                uint32_t nparams = 0;

                for (; op != nullptr && op->kind == T2OpKind::Param;
                     op = op->next) {
                    if (op->index >= call->num_operands) {
                        return fail("callee param out of range");
                    }
                    vmap[op->result] = call->operands[op->index];
                    vhome[call->operands[op->index]] =
                            call->operand_regs != nullptr
                                    ? call->operand_regs[op->index]
                                    : T2_REG_NONE;
                    nparams++;
                }
                if (nparams != callee.arity) {
                    return fail("callee param run mismatch");
                }

                for (; op != nullptr; op = op->next) {
                    T2Op *cl = fn.new_op(call->block, op->kind, op->type);

                    move_before(call->block, call, cl);

                    std::vector<T2Value *> ins;
                    for (uint16_t i = 0; i < op->num_operands; i++) {
                        auto it = vmap.find(op->operands[i]);

                        if (it == vmap.end()) {
                            return fail("callee operand escapes the "
                                        "value map");
                        }
                        ins.push_back(it->second);
                    }
                    fn.set_operands(cl, ins);

                    if (op->operand_regs != nullptr) {
                        cl->operand_regs =
                                fn.arena.alloc_array<int32_t>(op->num_operands);
                        for (uint16_t i = 0; i < op->num_operands; i++) {
                            cl->operand_regs[i] = op->operand_regs[i];
                        }
                    }

                    cl->imm_int = op->imm_int;
                    cl->imm_term = op->imm_term;
                    cl->mfa_m = op->mfa_m;
                    cl->mfa_f = op->mfa_f;
                    cl->index = op->index;
                    cl->bif_num = op->bif_num;
                    cl->live = op->live;
                    cl->dst_reg = op->dst_reg;
                    cl->flags = T2_OP_INLINED;
                    cl->sync = nullptr;
                    cl->beam_idx = call->beam_idx;

                    if (op->result != nullptr) {
                        vmap[op->result] = cl->result;
                    }
                }

                /* The returned value, materialized in the call's
                 * result home so every consumer (and the merge/sync
                 * contract) finds it where the call left it. */
                const T2Op *rt = cb->terminator;

                if (rt == nullptr || rt->kind != T2OpKind::Return ||
                    rt->num_operands != 1) {
                    return fail("callee terminator is not a return");
                }
                auto rit = vmap.find(rt->operands[0]);
                if (rit == vmap.end()) {
                    return fail("callee return value escapes the map");
                }
                T2Value *retval = rit->second;

                T2Op *cp = fn.new_op(call->block, T2OpKind::Copy, retval->type);

                move_before(call->block, call, cp);
                fn.set_operands(cp, {retval});
                cp->dst_reg = call->dst_reg != T2_REG_NONE ? call->dst_reg
                                                           : t2_xreg(0);
                cp->operand_regs = fn.arena.alloc_array<int32_t>(1);
                {
                    int32_t home = T2_REG_NONE;

                    if (retval->def != nullptr &&
                        retval->def->dst_reg != T2_REG_NONE) {
                        home = retval->def->dst_reg;
                    }
                    auto hit = vhome.find(retval);
                    if (hit != vhome.end()) {
                        home = hit->second;
                    }
                    if (is_const_def(retval)) {
                        home = T2_REG_NONE;
                    }
                    cp->operand_regs[0] = home;
                }
                cp->flags = T2_OP_INLINED;
                cp->beam_idx = call->beam_idx;

                replace_value(fn, call->result, cp->result);
                unlink_op(call->block, call);

                /* Reduction parity (PLAN/T2/08 §5.4): the erased call
                 * charged 2 reductions per iteration in T1 (callee
                 * entry i_test_yield + callee return dispatch). Fold
                 * them into the back edge so process_info(reductions)
                 * and the yield schedule stay T1-identical; the
                 * ReductionCheck's `index` carries the extra charge. */
                {
                    T2Op *rc = nullptr;

                    for (T2Op *op = call->block->ops_head; op != nullptr;
                         op = op->next) {
                        if (op->kind == T2OpKind::ReductionCheck) {
                            rc = op;
                        }
                    }
                    if (rc == nullptr) {
                        return fail("no back edge to absorb the call's "
                                    "reduction charges");
                    }
                    rc->index += 2;
                }

                changed = true;
                return true;
            }

            /* --- shape-up ---------------------------------------------- */

            uint32_t count_uses(const T2Value *v) {
                uint32_t n = 0;

                for_each_op(fn, [&](T2Op *op) {
                    for (uint16_t i = 0; i < op->num_operands; i++) {
                        if (op->operands[i] == v) {
                            n++;
                        }
                    }
                    if (op->sync != nullptr) {
                        const T2SyncMap *m = op->sync;

                        for (uint32_t i = 0; i < m->x_live; i++) {
                            if (m->x[i] == v) {
                                n++;
                            }
                        }
                        for (int32_t i = 0;
                             m->frame_size != T2_NO_FRAME && i < m->frame_size;
                             i++) {
                            if (m->y[i] == v) {
                                n++;
                            }
                        }
                    }
                });
                return n;
            }

            bool shape_up(const Site &site) {
                const T2Loop &loop = *site.loop;

                /* 1. Copy-propagate every Y-slot read to its root
                 *    (pre-verified by admit_loop to resolve). */
                for (uint32_t bid : loop.body) {
                    T2BasicBlock *b = fn.blocks[bid];

                    for (T2Op *op = b->ops_head; op != nullptr; op = op->next) {
                        if (op->operand_regs == nullptr) {
                            continue;
                        }
                        for (uint16_t i = 0; i < op->num_operands; i++) {
                            if (op->operand_regs[i] == T2_REG_NONE ||
                                !t2_reg_is_y(op->operand_regs[i])) {
                                continue;
                            }
                            const T2Value *root =
                                    resolve_copy_defs(op->operands[i]);

                            op->operands[i] = const_cast<T2Value *>(root);
                            if (is_const_def(root)) {
                                op->operand_regs[i] = T2_REG_NONE;
                            } else {
                                ASSERT(root->def != nullptr &&
                                       t2_reg_is_x(root->def->dst_reg));
                                op->operand_regs[i] = root->def->dst_reg;
                            }
                        }
                    }
                }

                /* 2. Sync maps of the loop's generic arithmetic can no
                 *    longer describe a T1 boundary (the frame and the
                 *    call are gone): force the window shape. The
                 *    speculation pass must convert these or validation
                 *    fails the function to T1. Dropped before the fill
                 *    Copies are reaped — the maps are their last
                 *    remaining readers. */
                for (uint32_t bid : loop.body) {
                    T2BasicBlock *b = fn.blocks[bid];

                    for (T2Op *op = b->ops_head; op != nullptr; op = op->next) {
                        if ((op->kind == T2OpKind::Add ||
                             op->kind == T2OpKind::Sub) &&
                            op->sync != nullptr) {
                            op->sync = nullptr;
                            op->flags |= T2_OP_INLINED;
                        }
                    }
                }

                /* 3. Drop the frame: the fill Copies must now be dead. */
                if (site.alloc != nullptr) {
                    for (uint32_t bid : loop.body) {
                        T2BasicBlock *b = fn.blocks[bid];
                        T2Op *op = b->ops_head;

                        while (op != nullptr) {
                            T2Op *next = op->next;

                            if (op->kind == T2OpKind::Copy &&
                                op->dst_reg != T2_REG_NONE &&
                                t2_reg_is_y(op->dst_reg)) {
                                if (count_uses(op->result) != 0) {
                                    return fail("live frame fill after "
                                                "copy propagation");
                                }
                                unlink_op(b, op);
                            }
                            op = next;
                        }
                    }
                    unlink_op(site.alloc->block, site.alloc);
                    unlink_op(site.dealloc->block, site.dealloc);
                }

                /* 4. Forward loop-local copies (the inliner's
                 *    return-materialization Copy and any decoded move
                 *    whose home no longer matters): a Copy whose result
                 *    is not a phi input, appears in no sync map, and is
                 *    only read by loop-body operands is replaced by its
                 *    operand and removed. */
                {
                    std::unordered_set<const T2Value *> pinned;

                    for_each_op(fn, [&](T2Op *op) {
                        if (op->kind == T2OpKind::Phi) {
                            for (uint16_t i = 0; i < op->num_operands; i++) {
                                pinned.insert(op->operands[i]);
                            }
                        }
                        if (op->sync != nullptr) {
                            const T2SyncMap *m = op->sync;

                            for (uint32_t i = 0; i < m->x_live; i++) {
                                pinned.insert(m->x[i]);
                            }
                            for (int32_t i = 0; m->frame_size != T2_NO_FRAME &&
                                                i < m->frame_size;
                                 i++) {
                                pinned.insert(m->y[i]);
                            }
                        }
                    });

                    std::unordered_set<uint32_t> in_loop(loop.body.begin(),
                                                         loop.body.end());

                    for (uint32_t bid : loop.body) {
                        T2BasicBlock *b = fn.blocks[bid];
                        T2Op *op = b->ops_head;

                        while (op != nullptr) {
                            T2Op *next = op->next;

                            if (op->kind != T2OpKind::Copy ||
                                op->dst_reg == T2_REG_NONE ||
                                !t2_reg_is_x(op->dst_reg) ||
                                pinned.count(op->result) != 0) {
                                op = next;
                                continue;
                            }

                            /* Every use must be a loop-body operand. */
                            bool ok = true;
                            std::vector<std::pair<T2Op *, uint16_t>> uses;

                            for_each_op(fn, [&](T2Op *use) {
                                for (uint16_t i = 0; i < use->num_operands;
                                     i++) {
                                    if (use->operands[i] != op->result) {
                                        continue;
                                    }
                                    if (use->block == nullptr ||
                                        in_loop.count(use->block->id) == 0 ||
                                        t2_op_is_terminator(use->kind)) {
                                        ok = false;
                                    } else {
                                        uses.emplace_back(use, i);
                                    }
                                }
                            });
                            if (ok) {
                                int32_t src_reg = op->operand_regs != nullptr
                                                          ? op->operand_regs[0]
                                                          : T2_REG_NONE;

                                for (auto &u : uses) {
                                    u.first->operands[u.second] =
                                            op->operands[0];
                                    if (u.first->operand_regs != nullptr) {
                                        u.first->operand_regs[u.second] =
                                                src_reg;
                                    }
                                }
                                unlink_op(b, op);
                            }
                            op = next;
                        }
                    }
                }

                /* 5. Preserve the re-call vector: iteration-local
                 *    values decoded into X0..arity-1 move out of the
                 *    prefix; only the latch materializations (the back
                 *    edge's phi inputs) keep the phi homes. Reuse a
                 *    dead register-backed slot (X0..X5 are machine
                 *    registers on aarch64) when one is free at the def,
                 *    else take a fresh slot. */
                std::unordered_set<uint32_t> used_x;
                std::unordered_set<const T2Value *> latch_mat;

                for (uint32_t i = 0; i < fn.arity; i++) {
                    used_x.insert(i);
                }
                for (uint32_t bid : loop.body) {
                    T2BasicBlock *b = fn.blocks[bid];

                    for (T2Op *op = b->ops_head; op != nullptr; op = op->next) {
                        if (op->dst_reg != T2_REG_NONE &&
                            t2_reg_is_x(op->dst_reg)) {
                            used_x.insert(t2_reg_index(op->dst_reg));
                        }
                        if (op->operand_regs != nullptr) {
                            for (uint16_t i = 0; i < op->num_operands; i++) {
                                if (op->operand_regs[i] != T2_REG_NONE &&
                                    t2_reg_is_x(op->operand_regs[i])) {
                                    used_x.insert(
                                            t2_reg_index(op->operand_regs[i]));
                                }
                            }
                        }
                        if (op->sync != nullptr) {
                            for (uint32_t i = 0; i < op->sync->x_live; i++) {
                                used_x.insert(i);
                            }
                        }
                    }
                }

                {
                    const T2BasicBlock *h = fn.blocks[loop.header];
                    std::unordered_set<uint32_t> latches(loop.latches.begin(),
                                                         loop.latches.end());

                    for (const T2Op *phi = h->phis_head; phi != nullptr;
                         phi = phi->next) {
                        for (uint16_t i = 0; i < phi->num_operands; i++) {
                            if (latches.count(phi->phi_blocks[i]->id) != 0) {
                                latch_mat.insert(phi->operands[i]);
                            }
                        }
                        if (phi->dst_reg != T2_REG_NONE &&
                            t2_reg_is_x(phi->dst_reg)) {
                            used_x.insert(t2_reg_index(phi->dst_reg));
                        }
                    }
                }

                /* Slots that must never be repurposed: the phi homes
                 * (== the arity prefix for a recovered loop) and every
                 * slot a surviving sync map's X prefix names. */
                uint32_t protected_x = fn.arity;

                for (uint32_t bid : loop.body) {
                    const T2BasicBlock *b = fn.blocks[bid];

                    for (const T2Op *op = b->ops_head; op != nullptr;
                         op = op->next) {
                        if (op->sync != nullptr &&
                            op->sync->x_live > protected_x) {
                            protected_x = op->sync->x_live;
                        }
                    }
                }

                uint32_t next_free = fn.arity;

                auto fresh_x = [&]() {
                    while (used_x.count(next_free) != 0) {
                        next_free++;
                    }
                    used_x.insert(next_free);
                    return next_free;
                };

                /* X0..X5 are register-backed on aarch64; reusing a slot
                 * that is untouched after the def keeps the value in a
                 * machine register instead of the in-memory X array. */
                constexpr uint32_t REG_BACKED_XREGS = 6;

                for (uint32_t bid : loop.body) {
                    T2BasicBlock *b = fn.blocks[bid];

                    /* Positions within the block, for the reuse scan. */
                    std::unordered_map<const T2Op *, uint32_t> pos;
                    {
                        uint32_t idx = 0;

                        for (const T2Op *op = b->ops_head; op != nullptr;
                             op = op->next) {
                            pos[op] = idx++;
                        }
                    }

                    for (T2Op *op = b->ops_head; op != nullptr; op = op->next) {
                        if (op->dst_reg == T2_REG_NONE ||
                            !t2_reg_is_x(op->dst_reg) ||
                            t2_reg_index(op->dst_reg) >= fn.arity) {
                            continue;
                        }
                        if (op->result != nullptr &&
                            latch_mat.count(op->result) != 0) {
                            continue; /* the back edge needs this home */
                        }

                        /* Uses all in this block? (Loop-local values
                         * always are; anything else takes the fresh
                         * path below with a conservative home.) */
                        bool local = true;

                        if (op->result != nullptr) {
                            for_each_op(fn, [&](T2Op *use) {
                                for (uint16_t i = 0; i < use->num_operands;
                                     i++) {
                                    if (use->operands[i] == op->result &&
                                        use->block != b) {
                                        local = false;
                                    }
                                }
                            });
                        }

                        int32_t nr = -1;

                        if (local) {
                            /* A register-backed slot untouched after
                             * this def (reads at the def itself are
                             * fine: the op reads before it writes). */
                            uint32_t p = pos[op];

                            for (uint32_t k = protected_x; k < REG_BACKED_XREGS;
                                 k++) {
                                bool touched = false;

                                for (const T2Op *o = b->ops_head;
                                     o != nullptr && !touched;
                                     o = o->next) {
                                    if (pos[o] <= p) {
                                        continue;
                                    }
                                    if (o->dst_reg == t2_xreg(k)) {
                                        touched = true;
                                    }
                                    if (o->operand_regs != nullptr) {
                                        for (uint16_t i = 0;
                                             i < o->num_operands;
                                             i++) {
                                            if (o->operand_regs[i] ==
                                                t2_xreg(k)) {
                                                touched = true;
                                            }
                                        }
                                    }
                                }
                                {
                                    const T2Op *t = b->terminator;

                                    if (t != nullptr &&
                                        t->operand_regs != nullptr) {
                                        for (uint16_t i = 0;
                                             i < t->num_operands;
                                             i++) {
                                            if (t->operand_regs[i] ==
                                                t2_xreg(k)) {
                                                touched = true;
                                            }
                                        }
                                    }
                                }
                                if (!touched) {
                                    nr = t2_xreg(k);
                                    break;
                                }
                            }
                        }
                        if (nr < 0) {
                            nr = t2_xreg(fresh_x());
                        }

                        op->dst_reg = nr;
                        if (op->result == nullptr) {
                            continue;
                        }
                        for (uint32_t bid2 : loop.body) {
                            T2BasicBlock *b2 = fn.blocks[bid2];

                            for (T2Op *use = b2->ops_head; use != nullptr;
                                 use = use->next) {
                                if (use->operand_regs == nullptr) {
                                    continue;
                                }
                                for (uint16_t i = 0; i < use->num_operands;
                                     i++) {
                                    if (use->operands[i] == op->result) {
                                        use->operand_regs[i] = nr;
                                    }
                                }
                            }
                        }
                    }
                }

                /* 6. Fuse is_tuple + test_arity chains that share one
                 *    fail edge into T1's own i_is_tuple_of_arity shape
                 *    (the loader transform T1 applies to the same
                 *    decoded sequence — instruction-count parity, not
                 *    an optimization beyond T1). */
                fuse_tuple_arity(loop);

                fn.finalize();

                changed = true;
                return true;
            }

            void fuse_tuple_arity(const T2Loop &loop) {
                for (uint32_t bid : loop.body) {
                    T2BasicBlock *b = fn.blocks[bid];
                    T2Op *test = b->ops_tail;
                    T2Op *br = b->terminator;

                    /* Shape: B = [... IsTuple v] Branch(C, E);
                     *        C = [TestArity v N] Branch(D, E), preds{B}. */
                    if (test == nullptr || test->kind != T2OpKind::IsTuple ||
                        br == nullptr || br->kind != T2OpKind::Branch ||
                        br->num_operands != 1 ||
                        br->operands[0] != test->result) {
                        continue;
                    }

                    T2BasicBlock *c = br->succ_then;
                    T2Op *ar = c->ops_head;
                    T2Op *cbr = c->terminator;

                    if (c->num_preds != 1 || c->phis_head != nullptr ||
                        ar == nullptr || ar->next != nullptr ||
                        ar->kind != T2OpKind::TestArity ||
                        ar->operands[0] != test->operands[0] ||
                        cbr == nullptr || cbr->kind != T2OpKind::Branch ||
                        cbr->num_operands != 1 ||
                        cbr->operands[0] != ar->result ||
                        cbr->succ_else != br->succ_else) {
                        continue;
                    }

                    /* Rewrite B's test to the fused shape and skip C. */
                    test->kind = T2OpKind::TestArity;
                    test->index = ar->index;
                    test->flags |= T2_OP_TUPLE_ARITY_FUSED;
                    br->succ_then = cbr->succ_then;

                    /* Orphan C: keep it structurally valid (a stub
                     * jump; unreachable after finalize). */
                    unlink_op(c, ar);
                    c->terminator = nullptr;
                    fn.emit_jump(c, cbr->succ_then);

                    changed = true;
                }
            }

            bool run() {
                if (fn.blocks.empty() || !fn.sync_complete ||
                    li.loops.empty() || ret == nullptr) {
                    return true;
                }

                for (const T2Loop &loop : li.loops) {
                    Site site;

                    if (loop.preheader != 0 || !admit_loop(loop, &site)) {
                        continue;
                    }

                    /* Build the callee's SSA and splice it if it is an
                     * admissible leaf. A callee that fails admission
                     * (or fails to build) leaves the site untouched. */
                    Eterm cf = site.call->mfa_f;
                    uint32_t car = site.call->index;
                    bool spliced = false;
                    bool splice_err = false;
                    std::string berr;

                    if (site.call->num_operands != car) {
                        continue;
                    }

                    T2BuildStatus st = t2_build_for_debug(
                            ret,
                            cf,
                            car,
                            [&](T2Function &callee) {
                                if (callee.blocks.size() != 1 ||
                                    callee.blocks[0]->phis_head != nullptr ||
                                    callee.blocks[0]->terminator == nullptr ||
                                    callee.blocks[0]->terminator->kind !=
                                            T2OpKind::Return) {
                                    return;
                                }

                                uint32_t nops = 0;
                                bool params_done = false;

                                for (const T2Op *op =
                                             callee.blocks[0]->ops_head;
                                     op != nullptr;
                                     op = op->next) {
                                    if (op->kind == T2OpKind::Param) {
                                        if (params_done) {
                                            return;
                                        }
                                        continue;
                                    }
                                    params_done = true;
                                    if (!callee_op_ok(op)) {
                                        return;
                                    }
                                    nops++;
                                }
                                if (nops > T2_INLINE_MAX_CALLEE_OPS) {
                                    return;
                                }

                                if (!splice_callee(site, callee)) {
                                    splice_err = true;
                                    return;
                                }
                                spliced = true;
                            },
                            &berr);

                    if (splice_err) {
                        return false; /* *err set by splice_callee */
                    }
                    if (st != T2BuildStatus::Ok || !spliced) {
                        continue;
                    }

                    if (!shape_up(site)) {
                        return false;
                    }
                }

                return true;
            }
        };

    } /* anonymous namespace */

    bool t2_inline_leaf(T2Function &fn,
                        const T2LoopInfo &li,
                        const ErtsT2RetainedCode *ret,
                        bool *changed,
                        std::string *err) {
        Inliner inl{fn, li, ret, err};

        if (!inl.run()) {
            return false;
        }
        *changed = inl.changed;
        return true;
    }

} /* namespace erts_t2 */
