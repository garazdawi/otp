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
 * T2-Full tier-2 JIT: the speculation-insertion pass. See t2_spec.hpp.
 */

#include "t2_spec.hpp"

extern "C"
{
#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "big.h"

#include "t2_pctab.h"
}

#include "t2_lir.hpp"

#include <algorithm>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace erts_t2 {

    /* ------------------------------------------------------------------ *
     * The profile-less default fact source                               *
     * ------------------------------------------------------------------ */

    T2DefaultFactSource::T2DefaultFactSource(const T2Function &fn) {
        allow.assign(fn.arity, 1);

        if (fn.blocks.empty()) {
            return;
        }
        for (const T2Op *op = fn.blocks[0]->ops_head; op != nullptr;
             op = op->next) {
            if (op->kind == T2OpKind::Param && op->index < fn.arity &&
                op->result != nullptr &&
                !op->result->type.has(BEAM_TYPE_INTEGER)) {
                /* The seeded entry type excludes integers entirely:
                 * a small-int speculation could never hold. */
                allow[op->index] = 0;
            }
        }
    }

    bool T2DefaultFactSource::speculate_param_small(uint32_t idx) const {
        return idx < allow.size() && allow[idx] != 0;
    }

    /* ------------------------------------------------------------------ *
     * The pass                                                           *
     * ------------------------------------------------------------------ */

    namespace {

        bool op_is_effect_boundary(const T2Op *op) {
            switch (op->kind) {
            case T2OpKind::Call:
            case T2OpKind::CallExt:
            case T2OpKind::Bif:
                return true;
            default:
                return false;
            }
        }

        bool op_is_frame_op(const T2Op *op) {
            switch (op->kind) {
            case T2OpKind::Allocate:
            case T2OpKind::Deallocate:
            case T2OpKind::Trim:
                return true;
            default:
                return false;
            }
        }

        /* Does executing `op` end the clean prefix of an iteration —
         * i.e. make a window-shaped deopt AFTER it illegal? The shared
         * classifier (t2_loop.cpp) is the single source of truth so
         * this pass and the window validator can never drift. */
        bool op_dirties_window(const T2Op *op, uint32_t arity) {
            return t2_op_dirties_window(op, arity);
        }

        /* Resolve an SSA value through identity chains: Copy ops and
         * single-input (pass-through) phis. Both are SSA identities of
         * their operand, and the validator's fact walk is transparent
         * to both (Copy by its transfer rule, a one-input phi by the
         * AND-over-edges rule degenerating to its single edge). The
         * bound is paranoia against malformed cycles. */
        const T2Value *resolve_copies(const T2Value *v) {
            for (int depth = 0; depth < 64; depth++) {
                const T2Op *d = v->def;

                if (d == nullptr) {
                    break;
                }
                if (d->kind == T2OpKind::Copy ||
                    (d->kind == T2OpKind::Phi && d->num_operands == 1)) {
                    v = d->operands[0];
                    continue;
                }
                break;
            }
            return v;
        }

        struct Spec {
            T2Function &fn;
            const T2LoopInfo &li;
            const ErtsT2RetainedCode *ret;
            const T2FactSource &facts;
            std::string *err;

            bool changed = false;

            /* Values proven small without any guard. */
            std::vector<uint8_t> proven;

            /* Candidate conversions, discovered per loop. */
            struct Cand {
                T2Op *op;
                bool window; /* else boundary */
            };
            std::vector<Cand> cands;
            std::unordered_map<const T2Op *, size_t> cand_of;

            /* Loop-header phis assumed small (entry guard + latch
             * proofs), and whether the assumption was consumed. */
            struct PhiInfo {
                T2Op *phi;
                T2Value *entry_val;     /* input on the preheader edge */
                bool entry_needs_guard; /* guard vs already proven     */
                bool assumed = false;
                bool used = false;
            };
            std::vector<PhiInfo> phis;
            std::unordered_map<const T2Value *, size_t> phi_of;

            bool fail(const char *what) {
                if (err != nullptr) {
                    *err = std::string("speculation pass: ") + what;
                }
                return false;
            }

            /* ---- base facts ---------------------------------------- */

            void compute_proven() {
                proven.assign(fn.values.size(), 0);
                for (const T2Value *v : fn.values) {
                    if (v->def != nullptr &&
                        v->def->kind == T2OpKind::ConstInt &&
                        IS_SSMALL(v->def->imm_int)) {
                        proven[v->id] = 1;
                    } else if (t2_type_proves_small(v->type)) {
                        proven[v->id] = 1;
                    }
                }
            }

            bool value_small_now(const T2Value *v) {
                v = resolve_copies(v);
                if (proven[v->id]) {
                    return true;
                }
                auto pit = phi_of.find(v);
                if (pit != phi_of.end() && phis[pit->second].assumed) {
                    return true;
                }
                auto cit = cand_of.find(v->def);
                if (cit != cand_of.end() && v->def->result == v) {
                    /* The committed result of a flag-checked op is a
                     * proven small on the fall-through path. */
                    return true;
                }
                return false;
            }

            /* ---- candidate discovery -------------------------------- */

            bool operand_guardable(const T2Op *op, uint16_t i) {
                /* An at-op guard reads the operand from its decoded
                 * home; a constant that is not a small can never pass
                 * the tag test, so such an op stays generic. */
                return op->operand_regs != nullptr &&
                       op->operand_regs[i] != T2_REG_NONE;
            }

            bool boundary_available(const T2Op *op) {
                if (op->sync == nullptr || (op->flags & T2_OP_INLINED) != 0 ||
                    op->beam_idx == 0 || ret == nullptr) {
                    return false;
                }
                return erts_t2_pc_lookup_kind(ret,
                                              fn.fn_index,
                                              op->beam_idx,
                                              ERTS_T2_PC_EFFECT) != nullptr;
            }

            bool op_is_candidate_kind(const T2Op *op) {
                if (op->kind != T2OpKind::Add && op->kind != T2OpKind::Sub) {
                    return false;
                }
                if (op->num_operands != 2 || op->result == nullptr ||
                    op->dst_reg == T2_REG_NONE) {
                    return false;
                }
                if ((op->flags & (T2_OP_ERR_EXIT_OP | T2_OP_ERR_EXIT_SHARED |
                                  T2_OP_GARBAGE_DEALLOC | T2_OP_PAIR_HEAD)) !=
                    0) {
                    return false;
                }
                /* A decoded fail edge (Succeeded consumer) means the op
                 * is a guard-context test; converting it would change
                 * which edge overflow takes. Keep it generic. */
                if (op->next != nullptr &&
                    op->next->kind == T2OpKind::Succeeded &&
                    op->next->num_operands == 1 &&
                    op->next->operands[0] == op->result) {
                    return false;
                }
                return true;
            }

            void collect_candidates() {
                for (const T2Loop &loop : li.loops) {
                    if (loop.preheader != 0) {
                        /* Post-recovery loops have the function entry
                         * block as their preheader; anything else is
                         * outside this pass's scope. */
                        continue;
                    }

                    std::vector<bool> in_loop(fn.blocks.size(), false);
                    for (uint32_t b : loop.body) {
                        in_loop[b] = true;
                    }

                    /* clean_in[b]: no window-dirtying op on any path
                     * from the header entry to b's entry (edges into
                     * the header start a fresh iteration). */
                    std::vector<uint8_t> dirty_in(fn.blocks.size(), 0);
                    bool ch = true;
                    while (ch) {
                        ch = false;
                        for (uint32_t bid : loop.body) {
                            const T2BasicBlock *b = fn.blocks[bid];
                            uint8_t flag = dirty_in[bid];

                            for (const T2Op *op = b->ops_head; op != nullptr;
                                 op = op->next) {
                                if (op_dirties_window(op, fn.arity)) {
                                    flag = 1;
                                }
                            }

                            const T2Op *t = b->terminator;
                            auto push = [&](T2BasicBlock *succ) {
                                if (succ == nullptr || !in_loop[succ->id] ||
                                    succ->id == loop.header) {
                                    return;
                                }
                                if (flag && !dirty_in[succ->id]) {
                                    dirty_in[succ->id] = 1;
                                    ch = true;
                                }
                            };
                            if (t != nullptr) {
                                switch (t->kind) {
                                case T2OpKind::Branch:
                                    push(t->succ_then);
                                    push(t->succ_else);
                                    break;
                                case T2OpKind::Jump:
                                    push(t->succ_then);
                                    break;
                                case T2OpKind::Switch:
                                    for (uint32_t c = 0; c < t->num_cases;
                                         c++) {
                                        push(t->cases[c].target);
                                    }
                                    push(t->default_target);
                                    break;
                                default:
                                    break;
                                }
                            }
                        }
                    }

                    for (uint32_t bid : loop.body) {
                        T2BasicBlock *b = fn.blocks[bid];
                        uint8_t dirty = dirty_in[bid];

                        for (T2Op *op = b->ops_head; op != nullptr;
                             op = op->next) {
                            if (op_is_candidate_kind(op) &&
                                cand_of.find(op) == cand_of.end()) {
                                bool window = !dirty;

                                if (window || boundary_available(op)) {
                                    cand_of.emplace(op, cands.size());
                                    cands.push_back(Cand{op, window});
                                }
                            }
                            if (op_dirties_window(op, fn.arity)) {
                                dirty = 1;
                            }
                        }
                    }

                    /* Header phis: entry input from the preheader edge. */
                    const T2BasicBlock *h = fn.blocks[loop.header];
                    for (T2Op *phi = h->phis_head; phi != nullptr;
                         phi = phi->next) {
                        T2Value *entry_val = nullptr;
                        bool multiple = false;

                        for (uint16_t i = 0; i < phi->num_operands; i++) {
                            if (phi->phi_blocks[i]->id == loop.preheader) {
                                if (entry_val != nullptr) {
                                    multiple = true;
                                }
                                entry_val = phi->operands[i];
                            }
                        }
                        if (entry_val == nullptr || multiple ||
                            phi->result == nullptr) {
                            continue;
                        }
                        if (phi_of.find(phi->result) != phi_of.end()) {
                            continue;
                        }
                        phi_of.emplace(phi->result, phis.size());
                        phis.push_back(PhiInfo{phi, entry_val, false});
                    }
                }
            }

            /* ---- phi assumption fixpoint ----------------------------- */

            bool entry_speculable(const T2Value *v) {
                v = resolve_copies(v);
                if (proven[v->id]) {
                    return true;
                }
                if (v->def != nullptr && v->def->kind == T2OpKind::Param &&
                    v->def->result != nullptr &&
                    v->def->result->type.has(BEAM_TYPE_INTEGER)) {
                    return facts.speculate_param_small(v->def->index);
                }
                return false;
            }

            void assume_phis() {
                /* Optimistic init: every collected phi assumed, then
                 * strike the ones whose edges cannot be established. */
                for (PhiInfo &pi : phis) {
                    pi.assumed = true;
                }

                bool ch = true;
                while (ch) {
                    ch = false;
                    for (PhiInfo &pi : phis) {
                        if (!pi.assumed) {
                            continue;
                        }

                        bool ok = entry_speculable(pi.entry_val);

                        /* Every non-entry (latch) input must be small
                         * by proof, by another assumed phi, or as the
                         * committed result of a converting candidate. */
                        for (uint16_t i = 0; ok && i < pi.phi->num_operands;
                             i++) {
                            const T2Value *v = pi.phi->operands[i];

                            if (v == pi.entry_val) {
                                continue;
                            }
                            if (!value_small_now(v)) {
                                ok = false;
                            }
                        }

                        if (!ok) {
                            pi.assumed = false;
                            ch = true;
                        }
                    }
                }

                for (PhiInfo &pi : phis) {
                    if (pi.assumed) {
                        const T2Value *root = resolve_copies(pi.entry_val);
                        pi.entry_needs_guard = !proven[root->id];
                    }
                }

                if (getenv("T2_SPEC_TRACE") != nullptr) {
                    for (const PhiInfo &pi : phis) {
                        erts_fprintf(stderr,
                                     "t2_spec: %T:%T/%u phi v%u entry v%u: "
                                     "assumed=%d entry_spec=%d\n",
                                     fn.module,
                                     fn.function,
                                     fn.arity,
                                     pi.phi->result->id,
                                     pi.entry_val->id,
                                     (int)pi.assumed,
                                     (int)entry_speculable(pi.entry_val));
                    }
                }
            }

            /* ---- guard insertion + conversion ------------------------ */

            T2Op *insert_before(T2BasicBlock *b, T2Op *before, T2Op *op) {
                /* new_op appended `op` at b's tail; unlink it... */
                ASSERT(b->ops_tail == op && op->next == nullptr);
                if (op->prev != nullptr) {
                    op->prev->next = nullptr;
                } else {
                    b->ops_head = nullptr;
                }
                b->ops_tail = op->prev;
                op->prev = nullptr;

                /* ...and splice it in before `before`. */
                op->prev = before->prev;
                op->next = before;
                if (before->prev != nullptr) {
                    before->prev->next = op;
                } else {
                    b->ops_head = op;
                }
                before->prev = op;
                return op;
            }

            void set_guard_operands(T2Op *g,
                                    const std::vector<T2Value *> &vals,
                                    const std::vector<int32_t> &regs) {
                fn.set_operands(g, vals);
                g->operand_regs = fn.arena.alloc_array<int32_t>(vals.size());
                for (size_t i = 0; i < regs.size(); i++) {
                    g->operand_regs[i] = regs[i];
                }
            }

            /* Mark an assumed phi (and, transitively, assumed phis its
             * latch inputs pass through) as consumed. */
            void mark_phi_used(const T2Value *v) {
                v = resolve_copies(v);
                auto it = phi_of.find(v);

                if (it == phi_of.end() || !phis[it->second].assumed ||
                    phis[it->second].used) {
                    return;
                }
                PhiInfo &pi = phis[it->second];

                pi.used = true;
                for (uint16_t i = 0; i < pi.phi->num_operands; i++) {
                    if (pi.phi->operands[i] != pi.entry_val) {
                        mark_phi_used(pi.phi->operands[i]);
                    }
                }
            }

            bool commit() {
                /* Operands whose smallness leaned on another candidate's
                 * committed result; re-checked after any strike. */
                std::vector<const T2Value *> cand_deps;

                /* Convert candidates, inserting fused at-op guards for
                 * operands that are neither proven nor phi-assumed. */
                for (Cand &c : cands) {
                    T2Op *op = c.op;
                    std::vector<T2Value *> guard_vals;
                    std::vector<int32_t> guard_regs;
                    bool viable = true;

                    for (uint16_t i = 0; i < op->num_operands; i++) {
                        T2Value *v = op->operands[i];

                        if (getenv("T2_SPEC_TRACE") != nullptr) {
                            erts_fprintf(stderr,
                                         "t2_spec: cand %s operand %u = "
                                         "v%u small_now=%d\n",
                                         t2_op_kind_name(op->kind),
                                         (unsigned)i,
                                         v->id,
                                         (int)value_small_now(v));
                        }

                        if (value_small_now(v)) {
                            mark_phi_used(v);
                            {
                                const T2Value *r = resolve_copies(v);

                                if (r->def != nullptr &&
                                    cand_of.find(r->def) != cand_of.end()) {
                                    cand_deps.push_back(r);
                                }
                            }
                            continue;
                        }
                        if (!operand_guardable(op, i)) {
                            viable = false;
                            break;
                        }
                        if (std::find(guard_vals.begin(),
                                      guard_vals.end(),
                                      v) == guard_vals.end()) {
                            guard_vals.push_back(v);
                            guard_regs.push_back(op->operand_regs[i]);
                        }
                    }
                    if (!viable) {
                        /* Strike the candidate; anything that assumed
                         * on its result re-checks below. */
                        cand_of.erase(op);
                        c.op = nullptr;
                        continue;
                    }

                    if (!guard_vals.empty()) {
                        T2Op *g = fn.new_op(op->block,
                                            T2OpKind::SpeculateType,
                                            T2Type::none());

                        insert_before(op->block, op, g);
                        set_guard_operands(g, guard_vals, guard_regs);
                        g->beam_idx = op->beam_idx;
                        if (!c.window) {
                            g->flags |= T2_OP_SPEC_BOUNDARY;
                            g->sync = op->sync;
                        }
                    }

                    op->kind = op->kind == T2OpKind::Add ? T2OpKind::AddSmall
                                                         : T2OpKind::SubSmall;
                    if (c.window) {
                        /* Window ops deopt to the iteration re-call
                         * boundary; the boundary map is not part of
                         * their contract (and keeping it would pin the
                         * op as an allocator barrier for no reason). */
                        op->sync = nullptr;
                    } else {
                        op->flags |= T2_OP_SPEC_BOUNDARY;
                    }
                    changed = true;
                }

                /* A struck candidate invalidates assumptions that leaned
                 * on its result — a phi whose latch input it was, or a
                 * converted op that consumed it directly. Re-check; on
                 * any breakage bail to T1 loudly rather than emit an
                 * unproven consumer (the validator would reject the
                 * function anyway, this just names the cause). */
                {
                    bool any_struck = false;

                    for (const Cand &c : cands) {
                        if (c.op == nullptr) {
                            any_struck = true;
                        }
                    }
                    if (any_struck) {
                        std::vector<uint8_t> was(phis.size());

                        for (size_t i = 0; i < phis.size(); i++) {
                            was[i] = phis[i].assumed && phis[i].used;
                        }
                        assume_phis();
                        for (size_t i = 0; i < phis.size(); i++) {
                            if (was[i] && !phis[i].assumed) {
                                return fail("phi assumption broke after "
                                            "a candidate was struck");
                            }
                        }
                        for (const T2Value *v : cand_deps) {
                            if (cand_of.find(v->def) == cand_of.end()) {
                                return fail("converted op consumed a "
                                            "struck candidate's result");
                            }
                        }
                    }
                }

                /* Entry guards for the consumed assumptions, fused into
                 * multi-operand SpeculateType ops appended to the entry
                 * block (before its terminator — after the params). */
                {
                    std::vector<T2Value *> vals;
                    std::vector<int32_t> regs;
                    std::unordered_set<const T2Value *> seen;

                    for (PhiInfo &pi : phis) {
                        if (!pi.assumed || !pi.used || !pi.entry_needs_guard) {
                            continue;
                        }
                        const T2Value *root = resolve_copies(pi.entry_val);

                        if (root->def == nullptr ||
                            root->def->kind != T2OpKind::Param) {
                            return fail("guarded entry value is not a "
                                        "parameter");
                        }
                        if (!seen.insert(root).second) {
                            continue;
                        }
                        vals.push_back(const_cast<T2Value *>(root));
                        regs.push_back(t2_xreg(root->def->index));
                    }

                    for (size_t i = 0; i < vals.size(); i += T2_LIR_MAX_SRCS) {
                        size_t n = std::min(vals.size() - i,
                                            (size_t)T2_LIR_MAX_SRCS);
                        std::vector<T2Value *> gv(vals.begin() + i,
                                                  vals.begin() + i + n);
                        std::vector<int32_t> gr(regs.begin() + i,
                                                regs.begin() + i + n);

                        T2Op *g = fn.new_op(fn.blocks[0],
                                            T2OpKind::SpeculateType,
                                            T2Type::none());
                        /* new_op appends to the ops list; the entry
                         * block's terminator is stored separately, so
                         * this lands after the params, before the
                         * preheader jump. */
                        set_guard_operands(g, gv, gr);
                        g->beam_idx = 0;
                        changed = true;
                    }
                }

                return true;
            }

            bool run() {
                if (fn.blocks.empty() || !fn.sync_complete ||
                    li.loops.empty()) {
                    return true;
                }

                compute_proven();
                collect_candidates();
                if (cands.empty()) {
                    return true;
                }
                assume_phis();
                return commit();
            }
        };

    } /* anonymous namespace */

    bool t2_speculate(T2Function &fn,
                      const T2LoopInfo &li,
                      const ErtsT2RetainedCode *ret,
                      const T2FactSource &facts,
                      bool *changed,
                      std::string *err) {
        Spec s{fn, li, ret, facts, err};

        if (!s.run()) {
            return false;
        }
        *changed = s.changed;
        return true;
    }

} /* namespace erts_t2 */
