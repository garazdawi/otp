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
 * T2-Full tier-2 JIT: loop analysis + self-tail-recursion loop
 * recovery. See t2_loop.hpp.
 */

#include "t2_loop.hpp"

extern "C"
{
#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#include "sys.h"
#include "global.h"
}

#include <algorithm>
#include <unordered_map>
#include <vector>

namespace erts_t2 {

    namespace {

        template<typename F>
        void for_each_succ(const T2Op *term, F fn) {
            if (term == nullptr) {
                return;
            }
            switch (term->kind) {
            case T2OpKind::Branch:
                fn(term->succ_then);
                fn(term->succ_else);
                break;
            case T2OpKind::Jump:
                fn(term->succ_then);
                break;
            case T2OpKind::Switch:
                for (uint32_t i = 0; i < term->num_cases; i++) {
                    fn(term->cases[i].target);
                }
                fn(term->default_target);
                break;
            default:
                break;
            }
        }

        struct Dominators {
            std::vector<bool> reachable;
            std::vector<std::vector<uint64_t>> dom;
            size_t words = 0;

            bool dominates(uint32_t a, uint32_t b) const {
                return (dom[b][a / 64] >> (a % 64)) & 1;
            }

            void compute(const T2Function &fn) {
                size_t n = fn.blocks.size();

                words = (n + 63) / 64;
                reachable.assign(n, false);
                dom.assign(n, std::vector<uint64_t>(words, 0));

                std::vector<const T2BasicBlock *> stack{fn.blocks[0]};
                reachable[0] = true;
                while (!stack.empty()) {
                    const T2BasicBlock *b = stack.back();
                    stack.pop_back();
                    for_each_succ(b->terminator, [&](T2BasicBlock *succ) {
                        if (succ != nullptr && !reachable[succ->id]) {
                            reachable[succ->id] = true;
                            stack.push_back(succ);
                        }
                    });
                }

                std::vector<uint64_t> all(words, ~uint64_t(0));

                dom[0][0] = 1;
                for (size_t i = 1; i < n; i++) {
                    if (reachable[i]) {
                        dom[i] = all;
                    }
                }

                bool changed = true;
                while (changed) {
                    changed = false;
                    for (size_t i = 1; i < n; i++) {
                        if (!reachable[i]) {
                            continue;
                        }

                        const T2BasicBlock *b = fn.blocks[i];
                        std::vector<uint64_t> in(words, ~uint64_t(0));
                        bool any_pred = false;

                        for (uint32_t p = 0; p < b->num_preds; p++) {
                            uint32_t pid = b->preds[p]->id;

                            if (!reachable[pid]) {
                                continue;
                            }
                            any_pred = true;
                            for (size_t w = 0; w < words; w++) {
                                in[w] &= dom[pid][w];
                            }
                        }

                        if (!any_pred) {
                            in.assign(words, 0);
                        }

                        in[i / 64] |= uint64_t(1) << (i % 64);

                        if (in != dom[i]) {
                            dom[i] = std::move(in);
                            changed = true;
                        }
                    }
                }
            }
        };

    } /* anonymous namespace */

    /* ------------------------------------------------------------------ *
     * LoopInfo (PLAN/T2/04 §10.5): dominators -> back-edges -> groups     *
     * ------------------------------------------------------------------ */

    void t2_loop_info(const T2Function &fn, T2LoopInfo *out) {
        out->loops.clear();
        if (fn.blocks.empty()) {
            return;
        }

        Dominators d;
        d.compute(fn);

        /* Back edges a->h where h dominates a; group latches per header. */
        std::unordered_map<uint32_t, size_t> header_loop;

        for (const T2BasicBlock *b : fn.blocks) {
            if (!d.reachable[b->id]) {
                continue;
            }
            for_each_succ(b->terminator, [&](T2BasicBlock *succ) {
                if (succ == nullptr || !d.dominates(succ->id, b->id)) {
                    return;
                }
                auto it = header_loop.find(succ->id);
                size_t li;

                if (it == header_loop.end()) {
                    li = out->loops.size();
                    out->loops.emplace_back();
                    out->loops[li].header = succ->id;
                    header_loop.emplace(succ->id, li);
                } else {
                    li = it->second;
                }
                auto &latches = out->loops[li].latches;
                if (std::find(latches.begin(), latches.end(), b->id) ==
                    latches.end()) {
                    latches.push_back(b->id);
                }
            });
        }

        /* Natural-loop bodies: backward flood from each latch to the
         * header, then exits and the preheader. */
        for (T2Loop &loop : out->loops) {
            std::vector<bool> in_body(fn.blocks.size(), false);
            std::vector<uint32_t> work = loop.latches;

            in_body[loop.header] = true;
            loop.body.push_back(loop.header);

            while (!work.empty()) {
                uint32_t b = work.back();
                work.pop_back();

                if (in_body[b]) {
                    continue;
                }
                in_body[b] = true;
                loop.body.push_back(b);

                const T2BasicBlock *blk = fn.blocks[b];
                for (uint32_t p = 0; p < blk->num_preds; p++) {
                    uint32_t pid = blk->preds[p]->id;

                    if (d.reachable[pid] && !in_body[pid]) {
                        work.push_back(pid);
                    }
                }
            }
            std::sort(loop.body.begin(), loop.body.end());

            for (uint32_t b : loop.body) {
                bool is_exit = false;

                for_each_succ(fn.blocks[b]->terminator,
                              [&](T2BasicBlock *succ) {
                                  if (succ != nullptr && !in_body[succ->id]) {
                                      is_exit = true;
                                  }
                              });
                if (is_exit) {
                    loop.exits.push_back(b);
                }
            }

            /* Preheader: the unique out-of-loop predecessor of the
             * header whose only successor is the header. */
            const T2BasicBlock *hdr = fn.blocks[loop.header];
            uint32_t cand = T2_NO_LOOP_BLOCK;
            uint32_t out_preds = 0;

            for (uint32_t p = 0; p < hdr->num_preds; p++) {
                uint32_t pid = hdr->preds[p]->id;

                if (!in_body[pid]) {
                    out_preds++;
                    cand = pid;
                }
            }
            if (out_preds == 1) {
                uint32_t nsucc = 0;
                bool only_header = true;

                for_each_succ(fn.blocks[cand]->terminator,
                              [&](T2BasicBlock *succ) {
                                  if (succ != nullptr) {
                                      nsucc++;
                                      if (succ->id != loop.header) {
                                          only_header = false;
                                      }
                                  }
                              });
                if (nsucc >= 1 && only_header) {
                    loop.preheader = cand;
                }
            }
        }

        /* Nesting: parent = the smallest other loop whose body contains
         * this loop's header. */
        for (size_t i = 0; i < out->loops.size(); i++) {
            size_t best = SIZE_MAX;

            for (size_t j = 0; j < out->loops.size(); j++) {
                if (i == j) {
                    continue;
                }
                const auto &body = out->loops[j].body;

                if (std::binary_search(body.begin(),
                                       body.end(),
                                       out->loops[i].header) &&
                    (best == SIZE_MAX ||
                     out->loops[j].body.size() <
                             out->loops[best].body.size())) {
                    best = j;
                }
            }
            out->loops[i].parent = best == SIZE_MAX ? -1 : (int32_t)best;
        }
    }

    /* ------------------------------------------------------------------ *
     * Self-tail-recursion loop recovery                                   *
     * ------------------------------------------------------------------ */

    namespace {

        /* Effect classification for the window rule: conservative —
         * every call-class op is treated as an effect boundary (a
         * shared effect table refines this with the speculation
         * phase). Allocation is not an effect (PLAN/T2/08 §3). */
        bool op_is_effect(const T2Op *op) {
            switch (op->kind) {
            case T2OpKind::Call:
            case T2OpKind::CallExt:
            case T2OpKind::Bif:
            /* BsSync writes the raw cursor back to ErlSubBits.start —
             * an observable heap mutation (PLAN/T2FULL/14 §2) — and
             * BsSetPosition (P-B) is the same store off the tagged
             * small. */
            case T2OpKind::BsSync:
            case T2OpKind::BsSetPosition:
                return true;
            default:
                return false;
            }
        }

        bool op_is_speculative_kind(const T2Op *op) {
            switch (op->kind) {
            case T2OpKind::SpeculateType:
            case T2OpKind::SpeculateRange:
            /* P-C L2 fused ASCII guard: a boundary-class deopt (rolls
             * back to the header), hence exempt from the window rule
             * below just like SpeculateRange. */
            case T2OpKind::SwarAsciiTest:
            case T2OpKind::UntagInt:
            case T2OpKind::AddSmall:
            case T2OpKind::SubSmall:
            case T2OpKind::MulSmall:
            case T2OpKind::MulRaw:
                return true;
            default:
                return false;
            }
        }

        /* Window-shaped deopt guards: speculative ops whose side exit
         * re-executes the iteration from the window's re-call boundary.
         * Boundary-shaped ops (Boundary-shape) deopt to their own
         * T1 EFFECT PC — they re-execute nothing before themselves and
         * are exempt from the window rule. Callsite-shaped ops
         * (Callsite-shape) deopt by re-executing the erased call
         * from the call-boundary sync map they carry; entry-shaped ops
         * (Entry-shape) re-execute the whole invocation from the
         * T1 entry body; each has its own rule (the callsite and
         * entry-recall passes in t2_validate_windows). */
        bool op_is_window_guard(const T2Op *op) {
            /* Re-dispatch-class ops (Redispatch-shape, P1a) are
             * deliberately NOT exempted: their deopt re-invokes the
             * generic callee with the loop-carried vector, so the
             * per-iteration clean-prefix rule applies to them exactly
             * as to plain window guards (T1 re-executes the current
             * iteration inside the real callee); their extra
             * obligations (sync map presence, out-of-loop prefix) are
             * checked by the redispatch rule in t2_validate_windows.
             *
             * A T2_OP_NO_OVF op is exempt: its claim is re-proven by
             * the validator (run_no_ovf_checks, strictly before this
             * pass runs) and the emitter then omits the deopt
             * entirely — an op with no deopt is not a deopt guard.
             * The bs cursor advance (PLAN/T2FULL/14 P-A) rides this:
             * it sits after StartMatch/BsRead dirt in every bs_match
             * decode. */
            return op_is_speculative_kind(op) &&
                   op->deopt_shape != T2DeoptShape::Boundary &&
                   op->deopt_shape != T2DeoptShape::Callsite &&
                   op->deopt_shape != T2DeoptShape::Entry &&
                   (op->flags & T2_OP_NO_OVF) == 0;
        }

        bool is_self_tail_call(const T2Function &fn, const T2Op *term) {
            return term != nullptr && term->kind == T2OpKind::TailCall &&
                   term->mfa_m == fn.module && term->mfa_f == fn.function &&
                   term->index == fn.arity &&
                   (term->flags & (T2_OP_ERR_EXIT_OP | T2_OP_ERR_EXIT_SHARED |
                                   T2_OP_GARBAGE_DEALLOC)) == 0;
        }

    } /* anonymous namespace */

    bool t2_loop_recover(T2Function &fn, bool *recovered, std::string *err) {
        auto fail = [&](const char *what) {
            if (err != nullptr) {
                *err = std::string("loop recovery: ") + what;
            }
            return false;
        };

        *recovered = false;

        if (fn.blocks.empty() || !fn.sync_complete) {
            return true;
        }

        /* Any local recursive tail call to self? (The latch *blocks* are
         * re-scanned after the entry-body move below: when the entry
         * block itself ends in the self tail call, that terminator
         * moves into the header.) */
        {
            bool any = false;

            for (T2BasicBlock *b : fn.blocks) {
                if (is_self_tail_call(fn, b->terminator)) {
                    if (b->terminator->sync == nullptr) {
                        return fail("self tail call without a sync map");
                    }
                    any = true;
                }
            }
            if (!any) {
                return true;
            }
        }

        T2BasicBlock *b0 = fn.blocks[0];

        if (b0->num_preds != 0 || b0->phis_head != nullptr) {
            /* The entry block is a branch target (never produced by the
             * loader) — do not restructure. */
            return true;
        }

        /* Collect the Param values (the leading run of the entry block's
         * body; the builder binds all params before any other op). */
        std::vector<T2Value *> params(fn.arity, nullptr);
        T2Op *last_param = nullptr;
        T2Op *first_moved = b0->ops_head;

        while (first_moved != nullptr && first_moved->kind == T2OpKind::Param) {
            if (first_moved->index >= fn.arity ||
                params[first_moved->index] != nullptr) {
                return fail("malformed parameter run");
            }
            params[first_moved->index] = first_moved->result;
            last_param = first_moved;
            first_moved = first_moved->next;
        }
        for (uint32_t i = 0; i < fn.arity; i++) {
            if (params[i] == nullptr) {
                return fail("missing parameter definition");
            }
        }
        for (const T2Op *q = first_moved; q != nullptr; q = q->next) {
            if (q->kind == T2OpKind::Param) {
                return fail("parameter defined after entry body ops");
            }
        }

        /* ---- 1. synthesize the header; move the entry body there ---- */

        T2BasicBlock *h = fn.new_block();

        if (first_moved != nullptr) {
            T2Op *move_tail = b0->ops_tail;

            if (last_param != nullptr) {
                last_param->next = nullptr;
            } else {
                b0->ops_head = nullptr;
            }
            b0->ops_tail = last_param;

            first_moved->prev = nullptr;
            h->ops_head = first_moved;
            h->ops_tail = move_tail;
            for (T2Op *q = first_moved; q != nullptr; q = q->next) {
                q->block = h;
            }
        }

        if (b0->terminator == nullptr) {
            return fail("entry block without terminator");
        }
        h->terminator = b0->terminator;
        h->terminator->block = h;
        b0->terminator = nullptr;
        fn.emit_jump(b0, h);

        /* The entry edge moved: phis in former successors of the entry
         * block now receive their input from the header. */
        for (T2BasicBlock *b : fn.blocks) {
            for (T2Op *phi = b->phis_head; phi != nullptr; phi = phi->next) {
                if (phi->phi_blocks == nullptr) {
                    continue;
                }
                for (uint16_t i = 0; i < phi->num_operands; i++) {
                    if (phi->phi_blocks[i] == b0) {
                        phi->phi_blocks[i] = h;
                    }
                }
            }
        }

        /* ---- 2. header phis: one per parameter, home X0..arity-1 ---- */

        std::vector<T2Op *> phis(fn.arity, nullptr);

        for (uint32_t i = 0; i < fn.arity; i++) {
            T2Op *phi = fn.new_phi(h, T2Type::any());

            phi->dst_reg = t2_xreg(i);
            phi->beam_idx = 0;
            phi->deopt_beam_idx = 0;
            phis[i] = phi;
        }

        /* ---- 3. replace every use of a param with its phi ------------ *
         * Everywhere except the function-entry sync map (the preheader
         * boundary, where the params themselves are the state) — the
         * phi inputs for the entry edge are set explicitly below. */

        std::unordered_map<const T2Value *, T2Value *> repl;

        for (uint32_t i = 0; i < fn.arity; i++) {
            repl.emplace(params[i], phis[i]->result);
        }

        auto replace_in_op = [&](T2Op *op) {
            for (uint16_t i = 0; i < op->num_operands; i++) {
                auto it = repl.find(op->operands[i]);

                if (it != repl.end()) {
                    op->operands[i] = it->second;
                }
            }
            if (op->sync != nullptr) {
                T2SyncMap *m = op->sync;

                for (uint32_t i = 0; i < m->x_live; i++) {
                    auto it = repl.find(m->x[i]);

                    if (it != repl.end()) {
                        m->x[i] = it->second;
                    }
                }
                for (int32_t i = 0;
                     m->frame_size != T2_NO_FRAME && i < m->frame_size;
                     i++) {
                    auto it = repl.find(m->y[i]);

                    if (it != repl.end()) {
                        m->y[i] = it->second;
                    }
                }
            }
        };

        for (T2BasicBlock *b : fn.blocks) {
            for (T2Op *phi = b->phis_head; phi != nullptr; phi = phi->next) {
                replace_in_op(phi);
            }
            for (T2Op *op = b->ops_head; op != nullptr; op = op->next) {
                replace_in_op(op);
            }
            if (b->terminator != nullptr) {
                replace_in_op(b->terminator);
            }
        }

        /* ---- 4. rewrite each latch: back-edge charge + back-jump ---- */

        std::vector<T2BasicBlock *> latches;

        for (T2BasicBlock *b : fn.blocks) {
            if (is_self_tail_call(fn, b->terminator)) {
                latches.push_back(b);
            }
        }
        if (latches.empty()) {
            return fail("self tail calls vanished during recovery");
        }

        std::vector<std::vector<T2Value *>> latch_args;

        for (T2BasicBlock *l : latches) {
            T2Op *t = l->terminator;

            /* The tail transfer becomes: ReductionCheck carrying the
             * transfer's sync map (args in X0..arity-1, no frame — the
             * fresh-call vector, and exactly T1's per-iteration charge
             * point: T1 charges one reduction at the callee's entry
             * i_test_yield, the recovered loop charges one at the
             * back edge), then an unconditional back-jump. */
            std::vector<T2Value *> args;

            if (t->num_operands != fn.arity) {
                return fail("self tail call arity mismatch");
            }
            for (uint16_t i = 0; i < t->num_operands; i++) {
                args.push_back(t->operands[i]);
            }
            latch_args.push_back(std::move(args));

            T2Op *rc = fn.new_op(l, T2OpKind::ReductionCheck, T2Type::none());

            fn.set_operands(rc, {});
            rc->beam_idx = t->beam_idx;
            rc->deopt_beam_idx = t->deopt_beam_idx;
            rc->sync = t->sync;

            l->terminator = nullptr;

            T2Op *j = fn.new_op(l, T2OpKind::Jump, T2Type::none());

            fn.set_operands(j, {});
            j->succ_then = h;
            j->beam_idx = t->beam_idx;
            j->deopt_beam_idx = t->deopt_beam_idx;
        }

        /* ---- 5. phi inputs: entry values + per-latch update values -- */

        for (uint32_t i = 0; i < fn.arity; i++) {
            std::vector<T2Value *> vals{params[i]};
            std::vector<T2BasicBlock *> preds{b0};

            for (size_t l = 0; l < latches.size(); l++) {
                vals.push_back(latch_args[l][i]);
                preds.push_back(latches[l]);
            }
            fn.set_phi_inputs(phis[i], vals, preds);
        }

        fn.finalize();

        erts_t2_opt_stats.p1_loops_recovered++;
        *recovered = true;
        return true;
    }

    /* ------------------------------------------------------------------ *
     * Re-execution-window validator (see t2_loop.hpp)                     *
     * ------------------------------------------------------------------ */

    bool t2_op_dirties_window(const T2Op *op, uint32_t arity) {
        if (op_is_effect(op)) {
            return true;
        }
        switch (op->kind) {
        case T2OpKind::Allocate:
        case T2OpKind::Deallocate:
        case T2OpKind::Trim:
            return true;
        case T2OpKind::GcTest:
            /* A GC that does not treat the whole fresh-call vector as
             * live would garbage X_live..arity-1. */
            return op->live < arity;
        case T2OpKind::StartMatch:
        case T2OpKind::BsMatch:
        case T2OpKind::BsGetTail:
            /* StartMatch may replace a binary with a fresh context;
             * BsMatch advances the context's position (a heap-object
             * mutation a window re-execution would repeat, reading
             * past the already-consumed bytes); BsGetTail GCs with
             * the decoded live count. All conservatively end the
             * clean prefix — speculation after them takes the
             * boundary shape. */
            return true;
        case T2OpKind::ChargeReds:
            /* A re-executed iteration must not charge twice. */
            return true;
        case T2OpKind::FoldBudget:
            /* Batch reduction charge: same double-charge concern. */
            return true;
        default:
            break;
        }
        return op->dst_reg != T2_REG_NONE && t2_reg_is_x(op->dst_reg) &&
               t2_reg_index(op->dst_reg) < arity;
    }

    bool t2_validate_windows(const T2Function &fn,
                             const T2LoopInfo &li,
                             std::string *err) {
        auto fail = [&](uint32_t block, const T2Op *op, const char *what) {
            if (err != nullptr) {
                *err = "window validator: block " + std::to_string(block) +
                       ": window-shaped deopt guard (" +
                       std::string(t2_op_kind_name(op->kind)) + ") " + what;
            }
            return false;
        };

        /* Entry-block guards (the loop-preheader entry speculation):
         * everything before them must be a Param or another guard, so
         * the deopt state — the argument vector in X0..arity-1, no
         * frame — is exactly the untouched entry state. */
        if (!fn.blocks.empty()) {
            const T2BasicBlock *b0 = fn.blocks[0];
            bool dirty = false;

            for (const T2Op *op = b0->ops_head; op != nullptr; op = op->next) {
                if (op_is_window_guard(op)) {
                    if (dirty) {
                        return fail(0,
                                    op,
                                    "preceded by a non-parameter op in "
                                    "the entry block (the entry deopt "
                                    "state must be untouched)");
                    }
                    continue;
                }
                if (op->kind != T2OpKind::Param &&
                    !op_is_speculative_kind(op)) {
                    dirty = true;
                }
            }
        }

        for (const T2Loop &loop : li.loops) {
            std::vector<bool> in_loop(fn.blocks.size(), false);

            for (uint32_t b : loop.body) {
                in_loop[b] = true;
            }

            /* Effective window arity (P2 commit 8): an intrinsic loop's
             * re-call vector is the CALLEE's (its latch ReductionCheck
             * carries T2_OP_RC_CALLEE with the callee arity in `live`),
             * so the clean-prefix write rule protects X0..callee_ar-1
             * rather than the enclosing function's arity prefix. */
            uint32_t arity_eff = fn.arity;

            for (uint32_t bid : loop.body) {
                for (const T2Op *op = fn.blocks[bid]->ops_head; op != nullptr;
                     op = op->next) {
                    if (op->kind == T2OpKind::ReductionCheck &&
                        (op->flags & T2_OP_RC_CALLEE) != 0) {
                        arity_eff = op->live;
                    }
                }
            }

            /* dirty_in[b]: the clean prefix may have ended on some path
             * from the window start (the iteration's header entry) to
             * b's entry. Monotone OR to a fixpoint; edges into the
             * header start a fresh iteration and do not propagate. */
            std::vector<uint8_t> dirty_in(fn.blocks.size(), 0);
            bool changed = true;

            while (changed) {
                changed = false;
                for (uint32_t bid : loop.body) {
                    const T2BasicBlock *b = fn.blocks[bid];
                    uint8_t flag = dirty_in[bid];

                    for (const T2Op *op = b->ops_head; op != nullptr;
                         op = op->next) {
                        if (t2_op_dirties_window(op, arity_eff)) {
                            flag = 1;
                        }
                    }

                    for_each_succ(b->terminator, [&](T2BasicBlock *succ) {
                        if (succ == nullptr || !in_loop[succ->id] ||
                            succ->id == loop.header) {
                            return;
                        }
                        if (flag && !dirty_in[succ->id]) {
                            dirty_in[succ->id] = 1;
                            changed = true;
                        }
                    });
                }
            }

            /* Check pass with the stable flags. The op's own write —
             * an AddSmall committing to X0..arity-1 — is fine: the
             * flag-check fires before the commit; only *preceding*
             * dirt is illegal. */
            for (uint32_t bid : loop.body) {
                const T2BasicBlock *b = fn.blocks[bid];
                uint8_t flag = dirty_in[bid];

                for (const T2Op *op = b->ops_head; op != nullptr;
                     op = op->next) {
                    if (flag && op_is_window_guard(op)) {
                        return fail(bid,
                                    op,
                                    "after an effect / frame op / "
                                    "X0..arity-1 write in its "
                                    "re-execution window "
                                    "(clean-prefix rule, PLAN/T2/08 "
                                    "§4.2)");
                    }
                    if (t2_op_dirties_window(op, arity_eff)) {
                        flag = 1;
                    }
                }
            }
        }

        /* ---- Callsite-class rule (maps:fold Stage 1) ------------------ *
         * An op whose deopt RE-EXECUTES THE ERASED CALL
         * (Callsite-shape) must carry the call-boundary sync map,
         * and the specialized region must have left that boundary
         * intact when the op fires: no effect, frame op, reduction
         * charge, GC/heap op, or write below the boundary's live X
         * prefix (or to any Y slot) may precede it — checked over the
         * op's own block prefix and over the innermost loop containing
         * it (every iteration re-runs the loop body before the op can
         * fire again). Same-class ops are exempt: the FoldBudget's
         * batch charge preceding a later deopt is the accepted,
         * documented deviation (T1 re-charges what the budget already
         * paid — reduction accounting only, never term results). */
        {
            auto is_callsite_op = [](const T2Op *op) {
                return op->deopt_shape == T2DeoptShape::Callsite &&
                       (op_is_speculative_kind(op) ||
                        op->kind == T2OpKind::FoldBudget);
            };
            auto callsite_dirt = [](const T2Op *op, uint32_t x_live) {
                if (op->deopt_shape == T2DeoptShape::Callsite) {
                    return false; /* same class */
                }
                if (op_is_effect(op)) {
                    return true;
                }
                switch (op->kind) {
                case T2OpKind::Allocate:
                case T2OpKind::Deallocate:
                case T2OpKind::Trim:
                case T2OpKind::ChargeReds:
                case T2OpKind::GcTest:
                case T2OpKind::StartMatch:
                case T2OpKind::BsMatch:
                case T2OpKind::BsGetTail:
                    return true;
                default:
                    break;
                }
                if (op->dst_reg != T2_REG_NONE) {
                    if (t2_reg_is_y(op->dst_reg)) {
                        return true;
                    }
                    if (t2_reg_is_x(op->dst_reg) &&
                        t2_reg_index(op->dst_reg) < x_live) {
                        return true;
                    }
                }
                return false;
            };

            for (const T2BasicBlock *b : fn.blocks) {
                for (const T2Op *op = b->ops_head; op != nullptr;
                     op = op->next) {
                    if (!is_callsite_op(op)) {
                        continue;
                    }
                    if (op->sync == nullptr) {
                        return fail(b->id,
                                    op,
                                    "callsite-class op without the "
                                    "call-boundary sync map");
                    }

                    uint32_t x_live = op->sync->x_live;

                    /* Own-block prefix. */
                    for (const T2Op *p = b->ops_head; p != op; p = p->next) {
                        if (callsite_dirt(p, x_live)) {
                            return fail(b->id,
                                        op,
                                        "callsite-class op after an "
                                        "effect/frame/charge/low-X "
                                        "write in its own block (the "
                                        "call-boundary state must be "
                                        "intact)");
                        }
                    }

                    /* Innermost containing loop: the whole body must
                     * keep the boundary intact. */
                    const T2Loop *inner = nullptr;

                    for (const T2Loop &loop : li.loops) {
                        bool contains = false;

                        for (uint32_t bid : loop.body) {
                            if (bid == b->id) {
                                contains = true;
                                break;
                            }
                        }
                        if (contains &&
                            (inner == nullptr ||
                             loop.body.size() < inner->body.size())) {
                            inner = &loop;
                        }
                    }
                    if (inner != nullptr) {
                        for (uint32_t bid : inner->body) {
                            for (const T2Op *p = fn.blocks[bid]->ops_head;
                                 p != nullptr;
                                 p = p->next) {
                                if (callsite_dirt(p, x_live)) {
                                    return fail(bid,
                                                op,
                                                "callsite-class op in a "
                                                "loop whose body dirties "
                                                "the call-boundary state");
                                }
                            }
                        }
                    }
                }
            }
        }

        /* ---- Re-dispatch-class rule (Redispatch-shape; P1a) ------ *
         * An op whose deopt RE-INVOKES THE GENERIC CALLEE WITH THE
         * LOOP-CARRIED STATE (a side exit to the erased call's own T1
         * PC over the current X vector) must carry the loop-carried
         * sync map — the register-state walk (t2_hir.cpp) proves the
         * vector is physically in X0..x_live-1 at the op — and its
         * own-block prefix must not have dirtied the vector or charged
         * reductions (the in-loop paths are covered by the
         * per-iteration clean-prefix walk above, since the class is
         * not exempted from op_is_window_guard; this prefix check is
         * what covers an out-of-loop placement such as the preheader
         * entry guard). */
        for (const T2BasicBlock *b : fn.blocks) {
            for (const T2Op *op = b->ops_head; op != nullptr; op = op->next) {
                if (op->deopt_shape != T2DeoptShape::Redispatch ||
                    !op_is_speculative_kind(op)) {
                    continue;
                }
                if (op->sync == nullptr) {
                    return fail(b->id,
                                op,
                                "redispatch-class op without the "
                                "loop-carried sync map");
                }
                for (const T2Op *p = b->ops_head; p != op; p = p->next) {
                    if (t2_op_dirties_window(p, op->sync->x_live)) {
                        return fail(b->id,
                                    op,
                                    "redispatch-class op after an effect/"
                                    "frame/charge/vector write in its own "
                                    "block (the re-dispatch vector must be "
                                    "intact)");
                    }
                }
            }
        }

        /* ---- Entry-recall rule (Entry-shape; Stage 3 sink) ------- *
         * An op whose deopt RE-EXECUTES THE WHOLE INVOCATION from the
         * function's T1 entry body needs every path from function entry
         * to it clean: no effect, frame op, reduction charge, GC below
         * the arity prefix, or write to X0..arity-1 — otherwise the
         * re-run repeats an effect or reads a corrupted argument
         * vector. Unlike the per-iteration loop rule above, dirt
         * propagates through loop back edges (prior iterations are
         * part of the path), so the fixpoint runs over the whole CFG
         * with no header exemption. Params are the vector itself, not
         * writes to it; same-class ops are exempt (the entry-class
         * FoldBudget's batch charge preceding a later deopt is the same
         * documented deviation the callsite rule accepts); a back-edge
         * ReductionCheck's charge would be paid twice, so it is dirt
         * even though the per-iteration model treats it as a boundary. */
        {
            bool any_entry = false;

            for (const T2BasicBlock *b : fn.blocks) {
                for (const T2Op *op = b->ops_head; op != nullptr && !any_entry;
                     op = op->next) {
                    any_entry = op->deopt_shape == T2DeoptShape::Entry;
                }
                if (any_entry) {
                    break;
                }
            }

            if (any_entry) {
                auto entry_dirt = [&](const T2Op *op) {
                    if (op->deopt_shape == T2DeoptShape::Entry) {
                        return false;
                    }
                    if (op->kind == T2OpKind::Param) {
                        return false;
                    }
                    if (op->kind == T2OpKind::ReductionCheck) {
                        return true;
                    }
                    return t2_op_dirties_window(op, fn.arity);
                };

                std::vector<uint8_t> dirty_in(fn.blocks.size(), 0);
                bool changed = true;

                while (changed) {
                    changed = false;
                    for (const T2BasicBlock *b : fn.blocks) {
                        uint8_t flag = dirty_in[b->id];

                        for (const T2Op *phi = b->phis_head; phi != nullptr;
                             phi = phi->next) {
                            if (entry_dirt(phi)) {
                                flag = 1;
                            }
                        }
                        for (const T2Op *op = b->ops_head; op != nullptr;
                             op = op->next) {
                            if (entry_dirt(op)) {
                                flag = 1;
                            }
                        }

                        for_each_succ(b->terminator, [&](T2BasicBlock *s) {
                            if (s != nullptr && flag && !dirty_in[s->id]) {
                                dirty_in[s->id] = 1;
                                changed = true;
                            }
                        });
                    }
                }

                for (const T2BasicBlock *b : fn.blocks) {
                    uint8_t flag = dirty_in[b->id];

                    for (const T2Op *phi = b->phis_head; phi != nullptr;
                         phi = phi->next) {
                        if (entry_dirt(phi)) {
                            flag = 1;
                        }
                    }
                    for (const T2Op *op = b->ops_head; op != nullptr;
                         op = op->next) {
                        if (op->deopt_shape == T2DeoptShape::Entry) {
                            if (op->kind != T2OpKind::FoldBudget &&
                                !op_is_speculative_kind(op)) {
                                return fail(b->id,
                                            op,
                                            "entry-class flag on a "
                                            "non-deopt op kind");
                            }
                            if (op->kind == T2OpKind::FoldBudget &&
                                op->sync == nullptr) {
                                return fail(b->id,
                                            op,
                                            "entry-class fold budget "
                                            "without the entry sync map");
                            }
                            if (flag) {
                                return fail(b->id,
                                            op,
                                            "entry-class op on a dirty "
                                            "path from the function "
                                            "entry (entry-recall rule)");
                            }
                        }
                        if (entry_dirt(op)) {
                            flag = 1;
                        }
                    }
                }
            }
        }

        /* ---- Frame-restart rule (FrameRestart shape; body recursion,
         * task #88). An op whose deopt DEALLOCATES THE SYNTHESIZED
         * FRAME and re-executes the whole invocation from the T1 entry
         * body (the frame_restart_tramps family) needs, like the
         * entry-recall rule, every path from function entry to it free
         * of effects and of writes to X0..arity-1 — the recall vector
         * is the ORIGINAL arguments, physically untouched in
         * X0..arity-1. Deviations the trampoline/contract absorbs are
         * exempt: frame ops (the trampoline pops op->live slots — which
         * must equal the walked frame, checked via the sync map) and
         * the back-edge ReductionCheck charge (a recalled invocation
         * re-charges; the accepted reduction-accounting deviation of
         * the deopt paths, term results never affected). The op must
         * carry the framed sync map whose x prefix names the Param
         * values (the recall vector) — the register-state walk then
         * proves the vector is physically in X0..arity-1 and the loop
         * state in its Y homes at the op. */
        {
            bool any_fr = false;

            for (const T2BasicBlock *b : fn.blocks) {
                for (const T2Op *op = b->ops_head; op != nullptr && !any_fr;
                     op = op->next) {
                    any_fr = op->deopt_shape == T2DeoptShape::FrameRestart;
                }
                if (any_fr) {
                    break;
                }
            }

            if (any_fr) {
                auto fr_dirt = [&](const T2Op *op) {
                    if (op->deopt_shape == T2DeoptShape::FrameRestart) {
                        return false; /* same class */
                    }
                    switch (op->kind) {
                    case T2OpKind::Param:
                        return false; /* the vector itself */
                    case T2OpKind::Allocate:
                    case T2OpKind::Deallocate:
                    case T2OpKind::Trim:
                        return false; /* the trampoline pops the frame */
                    case T2OpKind::ReductionCheck:
                        return false; /* accepted re-charge deviation */
                    case T2OpKind::GcTest:
                        return op->live < fn.arity;
                    default:
                        break;
                    }
                    return t2_op_dirties_window(op, fn.arity);
                };

                std::vector<uint8_t> dirty_in(fn.blocks.size(), 0);
                bool changed = true;

                while (changed) {
                    changed = false;
                    for (const T2BasicBlock *b : fn.blocks) {
                        uint8_t flag = dirty_in[b->id];

                        for (const T2Op *phi = b->phis_head; phi != nullptr;
                             phi = phi->next) {
                            if (fr_dirt(phi)) {
                                flag = 1;
                            }
                        }
                        for (const T2Op *op = b->ops_head; op != nullptr;
                             op = op->next) {
                            if (fr_dirt(op)) {
                                flag = 1;
                            }
                        }

                        for_each_succ(b->terminator, [&](T2BasicBlock *s) {
                            if (s != nullptr && flag && !dirty_in[s->id]) {
                                dirty_in[s->id] = 1;
                                changed = true;
                            }
                        });
                    }
                }

                for (const T2BasicBlock *b : fn.blocks) {
                    uint8_t flag = dirty_in[b->id];

                    for (const T2Op *op = b->ops_head; op != nullptr;
                         op = op->next) {
                        if (op->deopt_shape == T2DeoptShape::FrameRestart) {
                            if (!op_is_speculative_kind(op)) {
                                return fail(b->id,
                                            op,
                                            "frame-restart shape on a "
                                            "non-deopt op kind");
                            }
                            if (op->sync == nullptr) {
                                return fail(b->id,
                                            op,
                                            "frame-restart op without the "
                                            "framed sync map");
                            }
                            if (op->sync->frame_size == T2_NO_FRAME ||
                                (int32_t)op->live != op->sync->frame_size) {
                                return fail(b->id,
                                            op,
                                            "frame-restart op whose live "
                                            "slot count does not match "
                                            "its sync map's frame (the "
                                            "trampoline would mis-pop)");
                            }
                            if (op->sync->x_live != fn.arity) {
                                return fail(b->id,
                                            op,
                                            "frame-restart sync map does "
                                            "not cover exactly the "
                                            "argument vector");
                            }
                            for (uint32_t i = 0; i < op->sync->x_live; i++) {
                                const T2Op *d = op->sync->x[i]->def;

                                if (d == nullptr ||
                                    d->kind != T2OpKind::Param ||
                                    d->index != i) {
                                    return fail(b->id,
                                                op,
                                                "frame-restart sync map "
                                                "x entry is not the "
                                                "original parameter (the "
                                                "recall-from-top vector)");
                                }
                            }
                            if (flag) {
                                return fail(b->id,
                                            op,
                                            "frame-restart op on a dirty "
                                            "path from the function entry "
                                            "(recall-from-top rule)");
                            }
                        }
                        if (fr_dirt(op)) {
                            flag = 1;
                        }
                    }
                }
            }
        }

        return true;
    }

} /* namespace erts_t2 */

/* ------------------------------------------------------------------ *
 * Self-test (T2_SELFTEST)                                            *
 * ------------------------------------------------------------------ */

using namespace erts_t2;

#define T2_LOOP_CHECK(Cond)                                                    \
    do {                                                                       \
        if (!(Cond)) {                                                         \
            erts_fprintf(stderr,                                               \
                         "t2 loop selftest failure at %s:%d: %s\n",            \
                         __FILE__,                                             \
                         __LINE__,                                             \
                         #Cond);                                               \
            return __LINE__;                                                   \
        }                                                                      \
    } while (0)

/* Hand-build a natural loop:
 *   b0: p = param0; jump b1
 *   b1: v = phi(p from b0, r from b2); c = is_integer v; branch b2, b3
 *   b2: [guard?] r0 = call m:f(v); [guard?]; jump b1     (the latch)
 *   b3: return v
 * with a SpeculateType guard placed either before (legal) or after
 * (illegal) the call, exercising t2_loop_info + the window rule. */
static int t2_loop_selftest_window(bool guard_after) {
    T2Function fn;
    std::string err;

    fn.module = am_erlang;
    fn.function = am_ok;
    fn.arity = 1;

    T2BasicBlock *b0 = fn.new_block();
    T2BasicBlock *b1 = fn.new_block();
    T2BasicBlock *b2 = fn.new_block();
    T2BasicBlock *b3 = fn.new_block();

    T2Value *p0 = fn.emit_param(b0, 0, T2Type::any());
    fn.emit_jump(b0, b1);

    T2Op *phi = fn.new_phi(b1, T2Type::any());
    T2Value *cond = fn.emit_unary(b1,
                                  T2OpKind::IsInteger,
                                  phi->result,
                                  T2Type::of(BEAM_TYPE_ATOM));
    fn.emit_branch(b1, cond, b2, b3);

    if (!guard_after) {
        T2Op *spec = fn.new_op(b2, T2OpKind::SpeculateType, T2Type::none());
        fn.set_operands(spec, {phi->result});
    }
    T2Value *r = fn.emit_call(b2,
                              T2OpKind::Call,
                              am_erlang,
                              am_ok,
                              1,
                              {phi->result},
                              T2Type::any());
    if (guard_after) {
        T2Op *spec = fn.new_op(b2, T2OpKind::SpeculateType, T2Type::none());
        fn.set_operands(spec, {phi->result});
    }
    fn.emit_jump(b2, b1);

    fn.emit_return(b3, phi->result);

    fn.set_phi_inputs(phi, {p0, r}, {b0, b2});
    fn.finalize();

    if (!t2_validate(fn, &err)) {
        erts_fprintf(stderr, "t2 loop selftest: validate: %s\n", err.c_str());
        return __LINE__;
    }

    T2LoopInfo li;

    t2_loop_info(fn, &li);
    T2_LOOP_CHECK(li.loops.size() == 1);
    T2_LOOP_CHECK(li.loops[0].header == 1);
    T2_LOOP_CHECK(li.loops[0].preheader == 0);
    T2_LOOP_CHECK(li.loops[0].latches.size() == 1 &&
                  li.loops[0].latches[0] == 2);
    T2_LOOP_CHECK(li.loops[0].body.size() == 2);
    T2_LOOP_CHECK(li.loops[0].exits.size() == 1 && li.loops[0].exits[0] == 1);

    bool ok = t2_validate_windows(fn, li, &err);

    if (guard_after) {
        T2_LOOP_CHECK(!ok);
    } else if (!ok) {
        erts_fprintf(stderr, "t2 loop selftest: windows: %s\n", err.c_str());
        return __LINE__;
    }

    return 0;
}

extern "C" int erts_t2_loop_selftest(void) {
    int res;

    if ((res = t2_loop_selftest_window(false)) != 0) {
        return res;
    }
    if ((res = t2_loop_selftest_window(true)) != 0) {
        return res;
    }
    return 0;
}
