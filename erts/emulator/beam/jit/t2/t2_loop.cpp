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
                                  if (succ != nullptr &&
                                      !in_body[succ->id]) {
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
                return true;
            default:
                return false;
            }
        }

        /* Deopt-able guards: every speculative-class op side-exits
         * (tag-bit test or overflow flags) and re-executes from the
         * window's re-call boundary. */
        bool op_is_deopt_guard(const T2Op *op) {
            switch (op->kind) {
            case T2OpKind::SpeculateType:
            case T2OpKind::SpeculateRange:
            case T2OpKind::UntagInt:
            case T2OpKind::AddSmall:
            case T2OpKind::SubSmall:
            case T2OpKind::MulRaw:
                return true;
            default:
                return false;
            }
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

        while (first_moved != nullptr &&
               first_moved->kind == T2OpKind::Param) {
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
            rc->sync = t->sync;

            l->terminator = nullptr;

            T2Op *j = fn.new_op(l, T2OpKind::Jump, T2Type::none());

            fn.set_operands(j, {});
            j->succ_then = h;
            j->beam_idx = t->beam_idx;
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

        *recovered = true;
        return true;
    }

    /* ------------------------------------------------------------------ *
     * Re-execution-window validator (see t2_loop.hpp)                     *
     * ------------------------------------------------------------------ */

    bool t2_validate_windows(const T2Function &fn,
                             const T2LoopInfo &li,
                             std::string *err) {
        auto fail = [&](uint32_t block, const T2Op *op) {
            if (err != nullptr) {
                *err = "window validator: block " + std::to_string(block) +
                       ": deopt-able guard (" +
                       std::string(t2_op_kind_name(op->kind)) +
                       ") after an effect in its re-execution window "
                       "(guards-before-effects, PLAN/T2/08 §4.2)";
            }
            return false;
        };

        for (const T2Loop &loop : li.loops) {
            std::vector<bool> in_loop(fn.blocks.size(), false);

            for (uint32_t b : loop.body) {
                in_loop[b] = true;
            }

            /* effect_in[b]: an effect may have executed on some path
             * from the window start (the iteration's header entry) to
             * b's entry. Monotone OR to a fixpoint; edges into the
             * header start a fresh iteration and do not propagate. */
            std::vector<uint8_t> effect_in(fn.blocks.size(), 0);
            bool changed = true;

            while (changed) {
                changed = false;
                for (uint32_t bid : loop.body) {
                    const T2BasicBlock *b = fn.blocks[bid];
                    uint8_t flag = effect_in[bid];

                    for (const T2Op *op = b->ops_head; op != nullptr;
                         op = op->next) {
                        if (op_is_effect(op)) {
                            flag = 1;
                        }
                    }

                    for_each_succ(b->terminator, [&](T2BasicBlock *succ) {
                        if (succ == nullptr || !in_loop[succ->id] ||
                            succ->id == loop.header) {
                            return;
                        }
                        if (flag && !effect_in[succ->id]) {
                            effect_in[succ->id] = 1;
                            changed = true;
                        }
                    });
                }
            }

            /* Check pass with the stable flags. */
            for (uint32_t bid : loop.body) {
                const T2BasicBlock *b = fn.blocks[bid];
                uint8_t flag = effect_in[bid];

                for (const T2Op *op = b->ops_head; op != nullptr;
                     op = op->next) {
                    if (flag && op_is_deopt_guard(op)) {
                        return fail(bid, op);
                    }
                    if (op_is_effect(op)) {
                        flag = 1;
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
                         "t2 loop selftest failure at %s:%d: %s\n",           \
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
    T2_LOOP_CHECK(li.loops[0].exits.size() == 1 &&
                  li.loops[0].exits[0] == 1);

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
