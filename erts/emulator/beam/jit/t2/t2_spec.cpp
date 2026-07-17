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
#include "t2_retain.h"
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

        [[maybe_unused]] bool op_is_effect_boundary(const T2Op *op) {
            switch (op->kind) {
            case T2OpKind::Call:
            case T2OpKind::CallExt:
            case T2OpKind::Bif:
                return true;
            default:
                return false;
            }
        }

        [[maybe_unused]] bool op_is_frame_op(const T2Op *op) {
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

        /* The unique Param that `v` resolves to through Copy nodes and
         * loop Phis, or nullptr if it is not a single loop-invariant
         * parameter. A loop-carried value that is threaded UNCHANGED (e.g.
         * a struct read every iteration, r16(K,M,A)->...M... ) is a
         * multi-input header Phi whose every operand chains back to the
         * one entry Param; resolve_copies stops at that Phi (it only
         * folds single-input phis), so the map-shape spec needs this
         * stronger walk. DFS over Copy/Phi, breaking Phi cycles; every
         * non-Copy/Phi leaf must be the SAME Param op. This is exact:
         * if the entry Param is the only source, the value equals it on
         * every path, so its entry-sampled shape holds every iteration. */
        const T2Op *resolve_invariant_param(const T2Value *v) {
            std::vector<const T2Value *> work;
            std::unordered_set<const T2Op *> seen_phi;
            const T2Op *param = nullptr;

            work.push_back(v);
            while (!work.empty()) {
                const T2Value *cur = work.back();
                work.pop_back();
                const T2Op *d = cur->def;
                if (d == nullptr) {
                    return nullptr;
                }
                if (d->kind == T2OpKind::Copy) {
                    work.push_back(d->operands[0]);
                } else if (d->kind == T2OpKind::Phi) {
                    if (seen_phi.insert(d).second) {
                        for (uint16_t i = 0; i < d->num_operands; i++) {
                            work.push_back(d->operands[i]);
                        }
                    }
                } else if (d->kind == T2OpKind::Param) {
                    if (param == nullptr) {
                        param = d;
                    } else if (param != d) {
                        return nullptr;
                    }
                } else {
                    return nullptr;
                }
            }
            return param;
        }

        /* Census helper: is `k` a type-test op that the entry-type class
         * `seen` (a single ERTS_T2_TY_* bit) proves always-true? Those are
         * the guards an is_C elimination consumer could drop. */
        [[maybe_unused]] bool entry_type_test_matches(T2OpKind k,
                                                      uint16_t seen) {
            switch (seen) {
            case ERTS_T2_TY_TUPLE:
                return k == T2OpKind::IsTuple || k == T2OpKind::IsTaggedTuple;
            case ERTS_T2_TY_CONS:
                return k == T2OpKind::IsNonemptyList;
            case ERTS_T2_TY_NIL:
                return k == T2OpKind::IsNil;
            case ERTS_T2_TY_ATOM:
                return k == T2OpKind::IsAtom || k == T2OpKind::IsBoolean;
            case ERTS_T2_TY_FLOAT:
                return k == T2OpKind::IsFloat;
            case ERTS_T2_TY_BINARY:
                return k == T2OpKind::IsBinary || k == T2OpKind::IsBitstring;
            case ERTS_T2_TY_MAP_FLAT:
            case ERTS_T2_TY_MAP_HASH:
                return k == T2OpKind::IsMap;
            default:
                return false;
            }
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
                    op->deopt_beam_idx == 0 || ret == nullptr) {
                    return false;
                }
                return erts_t2_pc_lookup_kind(ret,
                                              fn.fn_index,
                                              op->deopt_beam_idx,
                                              ERTS_T2_PC_EFFECT) != nullptr;
            }

            /* ---- map-shape specialization (S1b.3c) ------------------ */

            /* Like boundary_available but WITHOUT the sync requirement: a
             * shape-guarded GetMapElement deopts like a read-only GuardBif
             * (X-homed map, the guard fires before any write), so it needs
             * only an ERTS_T2_PC_EFFECT resume PC, no sync map. */
            bool map_effect_available(const T2Op *op) {
                if (op->deopt_beam_idx == 0 || ret == nullptr) {
                    return false;
                }
                return erts_t2_pc_lookup_kind(ret,
                                              fn.fn_index,
                                              op->deopt_beam_idx,
                                              ERTS_T2_PC_EFFECT) != nullptr;
            }

            /* Attach a profiled monomorphic flatmap shape to every
             * GetMapElement whose map operand is a function parameter with
             * a mono shape fact. isel validates the shape is a safe
             * compiled-module literal, derives the key's fixed flatmap
             * index, and lowers a shape guard + O(1) load; it drops back to
             * the key scan if any of that fails, so this is a hint only
             * (map_monomorphic_design.md S1b.3c). */
            void specialize_map_shapes() {
                for (T2BasicBlock *b : fn.blocks) {
                    for (T2Op *op = b->ops_head; op != nullptr; op = op->next) {
                        if (op->kind != T2OpKind::GetMapElement ||
                            op->num_operands != 2 || op->result == nullptr) {
                            continue;
                        }
                        const T2Op *pdef =
                                resolve_invariant_param(op->operands[0]);
                        if (pdef == nullptr) {
                            continue;
                        }
                        Eterm shape = facts.map_shape_for_param(pdef->index);
                        if (shape == THE_NON_VALUE ||
                            !map_effect_available(op)) {
                            continue;
                        }
                        op->imm_term = shape;
                        op->flags |= T2_OP_MAP_SHAPE_SPEC;
                        changed = true;
                    }
                }
            }

            /* ---- entry type-class speculation (#1c) ---------------- *
             *
             * When the tier-up profile observed a function-entry argument
             * MONOMORPHICALLY as a single cheap-tag non-small class
             * (tuple/cons/nil/atom/float/binary/map), plant ONE entry
             * SpeculateType(class) guard on that Param and narrow its SSA
             * type to the class. The guard is a runtime tag test that
             * deopts to T1 on any mismatch (Entry deopt shape: T1
             * re-executes the whole invocation from the fresh-call vector
             * in X0..arity-1), so the monomorphic profile hint is a
             * speculation, never trusted blind: a wrong guess merely
             * deopts, never a wrong result. The narrowing feeds a future
             * downstream is_C guard-elim; it is sound because the guard
             * dominates every use (planted at the entry block, before its
             * terminator), and inert-and-safe today (the only backend
             * type-lattice reader is t2_type_proves_small, which a
             * non-small class only ever turns OFF).
             * ------------------------------------------------------- */

            /* The BEAM_TYPE_* union bit a monomorphic ERTS_T2_TY_* class
             * narrows to (and the guard proves at runtime), or 0 for a
             * class with no cheap single tag test (small/big/fun/other —
             * out of scope; small has its own pass). */
            static uint16_t entry_class_beam_bits(uint16_t ty_class) {
                switch (ty_class) {
                case ERTS_T2_TY_TUPLE:
                    return BEAM_TYPE_TUPLE;
                case ERTS_T2_TY_CONS:
                    return BEAM_TYPE_CONS;
                case ERTS_T2_TY_NIL:
                    return BEAM_TYPE_NIL;
                case ERTS_T2_TY_ATOM:
                    return BEAM_TYPE_ATOM;
                case ERTS_T2_TY_FLOAT:
                    return BEAM_TYPE_FLOAT;
                case ERTS_T2_TY_BINARY:
                    return BEAM_TYPE_BITSTRING;
                case ERTS_T2_TY_MAP_FLAT:
                case ERTS_T2_TY_MAP_HASH:
                    return BEAM_TYPE_MAP;
                default:
                    return 0;
                }
            }

            void specialize_entry_types() {
                /* OFF by default (opt-in via T2_ENTRY_TYPE). The emit path is
                 * correct and crash-free (each class' tag test is lowered
                 * INLINE at disp1MB — no dispUnknown veneer, so the former
                 * "could not resolve all labels" abort cannot recur), but the
                 * speculation is currently a NET COST: the T2_ENTRY_TYPE_CENSUS
                 * measured 827 guards planted over a dialyzer tier-up of stdlib
                 * with ZERO eliminable downstream type-tests, and no other pass
                 * consumes a non-small type narrowing, so the planted guard buys
                 * nothing today. Kept behind the opt-in (and exercised by the
                 * t2-correctness natural-tier-up leg with T2_ENTRY_TYPE=1) until
                 * a beneficial consumer of the narrowing exists. */
                if (getenv("T2_ENTRY_TYPE") == nullptr ||
                    fn.blocks.empty()) {
                    return;
                }

                T2BasicBlock *b0 = fn.blocks[0];

                /* The entry deopt state (X0..arity-1, no frame) must be the
                 * untouched fresh-call vector. If any real op already
                 * dirties the entry block's prefix, degrade to a NO-OP
                 * (leave the generic path) rather than plant a guard the
                 * entry-recall validator would correctly reject — a bail is
                 * always safe, a bad guard is not. Params and other
                 * speculation guards are the only clean-prefix ops. */
                for (const T2Op *op = b0->ops_head; op != nullptr;
                     op = op->next) {
                    if (op->kind != T2OpKind::Param &&
                        op->kind != T2OpKind::SpeculateType) {
                        return;
                    }
                }

                /* Snapshot the entry Params before planting (new_op appends
                 * to the same body list we would otherwise be iterating). */
                std::vector<T2Op *> params;
                for (T2Op *op = b0->ops_head; op != nullptr; op = op->next) {
                    if (op->kind == T2OpKind::Param && op->result != nullptr &&
                        op->index < fn.arity) {
                        params.push_back(op);
                    }
                }

                for (T2Op *param : params) {
                    uint32_t idx = param->index;
                    uint16_t seen = facts.param_seen_types(idx);

                    /* Require a real profile record AND monomorphism:
                     * exactly one observed class bit. 0 == no evidence
                     * (forced compile-at-load, an unsampled arg, or the
                     * profile-less default) -> no-op, so the +JT2enable
                     * differential stays byte-identical. Two or more bits
                     * == polymorphic -> no-op. */
                    if (seen == 0 || (seen & (uint16_t)(seen - 1)) != 0) {
                        continue;
                    }

                    uint16_t beam_bits = entry_class_beam_bits(seen);
                    if (beam_bits == 0) {
                        continue;
                    }

                    /* Profile narrows, never contradicts (02 §1): only
                     * speculate a class the seeded AOT type still admits.
                     * If the type chunk already excludes it the observation
                     * is inconsistent -> leave it generic. */
                    if (!param->result->type.has(beam_bits)) {
                        continue;
                    }

                    /* Plant one entry SpeculateType(class) guard on the
                     * Param (Entry deopt shape). new_op appends to b0's
                     * body list — after the params, before the (separately
                     * held) terminator — exactly where the small entry
                     * guards land. */
                    T2Op *g = fn.new_op(b0,
                                        T2OpKind::SpeculateType,
                                        T2Type::none());
                    std::vector<T2Value *> gv{param->result};
                    std::vector<int32_t> gr{t2_xreg(idx)};

                    set_guard_operands(g, gv, gr);
                    g->spec_type_class = seen;
                    g->deopt_shape = T2DeoptShape::Entry;
                    g->beam_idx = 0;
                    g->deopt_beam_idx = 0;

                    /* Narrow the Param's SSA type to the proven class (meet
                     * with the class bit; keep any range/unit refinement,
                     * which the tag test does not disturb). op->type mirrors
                     * result->type by the builder's invariant. */
                    T2Type nt = param->result->type;
                    nt.type_union = (uint16_t)(nt.type_union & beam_bits);
                    param->result->type = nt;
                    param->type = nt;

                    changed = true;

                    /* Census (T2_ENTRY_TYPE_CENSUS, read-only): how many
                     * downstream type-test ops on this now-proven-class param
                     * would an is_C guard-elimination consumer actually be able
                     * to drop? Measures the win BEFORE building the (CFG-
                     * surgery) pass — the narrowing is otherwise inert. */
                    if (getenv("T2_ENTRY_TYPE_CENSUS") != nullptr) {
                        unsigned elim = 0;
                        for (T2BasicBlock *cb : fn.blocks) {
                            for (T2Op *o = cb->ops_head; o != nullptr;
                                 o = o->next) {
                                if (o->num_operands == 1 &&
                                    resolve_copies(o->operands[0]) ==
                                            param->result &&
                                    entry_type_test_matches(o->kind, seen)) {
                                    elim++;
                                }
                            }
                        }
                        erts_fprintf(stderr,
                                     "T2_ENTRY_CENSUS %T:%T/%u p%u class=0x%x "
                                     "elim=%u\n",
                                     fn.module,
                                     fn.function,
                                     fn.arity,
                                     idx,
                                     (unsigned)seen,
                                     elim);
                    }
                }
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
                                /* A roll-back op (P-C B1) is boundary
                                 * by contract: its attached header map
                                 * + header beam_idx ARE the deopt state
                                 * (window class would null the map and
                                 * re-call the iteration instead). Its
                                 * prefix is dirty anyway (the header's
                                 * start_match); this keeps the contract
                                 * explicit. */
                                bool window = !dirty &&
                                              (op->flags & T2_OP_ROLLBACK) == 0;

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
                        if (t2_value_is_raw_home(v)) {
                            /* P-C B2: the fused read-sum accumulator's
                             * SWAR addend is a RAW-IN-HOME word (the
                             * <<4-form byte sum) — admissible to the
                             * flag-checked add by construction
                             * (spec_check_op's raw_ok; the validator's
                             * raw rules pin it to operand 1 of the
                             * ROLLBACK class), and never guarded: a
                             * tag test on a tag-cleared word always
                             * fails. */
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
                        /* The type guard shares the speculative op's own
                         * source position, and deopts wherever the op deopts
                         * (== its deopt ordinal, which for a split ROLLBACK /
                         * callsite op is NOT its beam_idx). */
                        g->beam_idx = op->beam_idx;
                        g->deopt_beam_idx = op->deopt_beam_idx;
                        if (!c.window) {
                            g->deopt_shape = T2DeoptShape::Boundary;
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
                        op->deopt_shape = T2DeoptShape::Boundary;
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
                        g->deopt_beam_idx = 0;
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

                /* Map-shape specialization is independent of the arithmetic
                 * candidate machinery, so run it before the cands.empty()
                 * early-out (a struct-reading loop has no Add/Sub cands). */
                specialize_map_shapes();

                /* Entry type-class speculation (#1c) is likewise independent
                 * of the arithmetic candidates and must run before them: it
                 * may narrow a boxed param's type, which correctly withholds
                 * the doomed small speculation on that same argument. */
                specialize_entry_types();

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
