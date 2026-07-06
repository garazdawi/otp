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
 * T2-Full tier-2 JIT: register allocation (PLAN/T2/04 §11.2,
 * PLAN/T2FULL/09 §4).
 *
 * A Wimmer-style linear scan on SSA over the LIR. The pipeline stage is
 * real from P2 commit 1 on: SSA liveness is computed over the
 * value-annotated LIR (including the phi defs/edge-inputs isel carries
 * onto each block), live intervals are built with per-use position +
 * location records, and every op that carries a sync map contributes
 * *fixed-slot* constraints — the T2SyncMap the builder attached IS the
 * allocator's pin-constraint set (PLAN/T2FULL/09 §4: no new metadata).
 *
 * Placement policy by commit:
 *
 *   - P2 commit 1 (this code): the policy reproduces the P1 identity
 *     placement exactly — every interval lives in its canonical BEAM
 *     slot at every use (the decoded homes isel already wrote into the
 *     LIR), and no interval is assigned a PhysLoc::Phys. The machinery
 *     (liveness, intervals, pins) runs for real and *verifies* that the
 *     identity placement is a consistent allocation; the emitted LIR is
 *     unchanged, so the identity gate stays byte-identical.
 *
 *   - P2 commit 3 relaxes non-sync-point boundaries: an interval that
 *     crosses no sync point may be assigned a Phys register and stay in
 *     a CPU register across ops, with resolution moves at the
 *     boundaries that pin it.
 *
 * What is verified here (allocator-grade, strictly stronger than the P1
 * structural verifier, which is retained below):
 *
 *   1. SSA liveness closes: no value is live into the function entry
 *      other than the parameters (a use without a reaching def is a
 *      hard error, never silent).
 *   2. Placement soundness, walked forward in slot space per block: a
 *      slot read (op operand, sync-map entry, phi merge at block entry)
 *      must find the value the annotation names in that slot; Trim
 *      renumbers Y slots, Allocate/Deallocate invalidate the frame,
 *      calls clobber the X file. Adopt-on-first-mention at block entry
 *      (exact within a block, partial across blocks — phi homes seed
 *      the known part), mirroring the HIR validator's walk one stage
 *      further down the pipeline.
 *   3. Clobber liveness: a value whose interval is live across a
 *      call-class op must hold a frame (Y) home at that boundary —
 *      an X-only placement across a clobber is a hard error where the
 *      walk knows the placement.
 *   4. Untagged discipline (PLAN/T2/04 §11.2, armed for the speculation
 *      phase): an untagged interval must never have an X/Y-slot use and
 *      must never be named by a sync map.
 */

#include "t2_isel.hpp"

extern "C"
{
#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#include "sys.h"
#include "global.h"
}

#include <algorithm>
#include <cstdlib>
#include <string>
#include <unordered_map>
#include <vector>

namespace erts_t2 {

    namespace {

        /* ------------------------------------------------------------------ *
         * Structural verification (the P1 checks, retained verbatim)          *
         * ------------------------------------------------------------------ */

        struct VerifyStructure {
            T2LirFunction &lir;
            std::string *err;

            bool fail(const std::string &msg) {
                if (err) {
                    *err = msg;
                }
                return false;
            }

            bool check_src(const T2LirSrc &s, const char *what) {
                if (s.is_const) {
                    return true;
                }
                if (!s.loc.is_slot()) {
                    return fail(std::string(what) +
                                ": operand is not a canonical slot or "
                                "immediate");
                }
                return true;
            }

            bool check_block_ref(uint32_t id, const char *what) {
                if (id == T2_LIR_NO_BLOCK || id >= lir.blocks.size()) {
                    return fail(std::string(what) +
                                ": successor block out of range");
                }
                return true;
            }

            bool run() {
                for (const T2LirBlock &b : lir.blocks) {
                    for (size_t i = 0; i < b.ops.size(); i++) {
                        const T2LirOp &op = b.ops[i];
                        const char *name = t2_lir_kind_name(op.kind);
                        bool is_term = t2_lir_kind_is_terminator(op.kind) ||
                                       op.kind == T2LirKind::Call ||
                                       op.kind == T2LirKind::CallExt ||
                                       /* folded guards absorb the branch */
                                       op.succ_else != T2_LIR_NO_BLOCK;

                        if (op.kind == T2LirKind::Invalid) {
                            return fail("invalid LIR op");
                        }

                        for (uint8_t s = 0; s < op.num_srcs; s++) {
                            if (!check_src(op.srcs[s], name)) {
                                return false;
                            }
                        }
                        for (uint32_t s = 0; s < op.num_srcs_ext; s++) {
                            if (op.pool_first + s >= lir.src_pool.size()) {
                                return fail("operand pool out of range");
                            }
                            if (!check_src(lir.src_pool[op.pool_first + s],
                                           name)) {
                                return false;
                            }
                        }
                        if (!op.dst.is_none() && !op.dst.is_slot()) {
                            return fail(std::string(name) +
                                        ": destination is not a canonical "
                                        "slot");
                        }
                        if (!op.dst2.is_none() && !op.dst2.is_slot()) {
                            return fail(std::string(name) +
                                        ": second destination is not a "
                                        "canonical slot");
                        }

                        /* Edges. */
                        if (op.succ_then != T2_LIR_NO_BLOCK &&
                            !check_block_ref(op.succ_then, name)) {
                            return false;
                        }
                        if (op.succ_else != T2_LIR_NO_BLOCK &&
                            !check_block_ref(op.succ_else, name)) {
                            return false;
                        }
                        if (op.kind == T2LirKind::Switch) {
                            if (op.first_case + op.num_cases >
                                lir.switch_cases.size()) {
                                return fail("switch case pool out of range");
                            }
                            for (uint32_t c = 0; c < op.num_cases; c++) {
                                if (!check_block_ref(
                                            lir.switch_cases[op.first_case + c]
                                                    .target,
                                            name)) {
                                    return false;
                                }
                            }
                            if (!check_block_ref(op.default_target, name)) {
                                return false;
                            }
                        }

                        /* Cross-tier addresses. */
                        switch (op.kind) {
                        case T2LirKind::Call:
                        case T2LirKind::CallExt:
                            if (op.t1_pc_cont == nullptr) {
                                return fail("call without a T1 continuation");
                            }
                            /* fall through */
                        case T2LirKind::TailCall:
                        case T2LirKind::TailCallExt:
                            if (op.kind == T2LirKind::Call ||
                                op.kind == T2LirKind::TailCall) {
                                if (op.target == nullptr) {
                                    return fail("local call without a "
                                                "resolved target");
                                }
                            } else if (op.exp == nullptr) {
                                return fail("remote call without an export");
                            }
                            break;
                        case T2LirKind::SideExit:
                            if (op.t1_pc_fail == nullptr) {
                                return fail("side exit without a T1 PC");
                            }
                            break;
                        case T2LirKind::ReductionCheck:
                            /* Back-edge: the demote target (own L_f)
                             * and the fresh-call sync map must both
                             * be present. */
                            if (op.target == nullptr) {
                                return fail("back-edge without a demote "
                                            "target");
                            }
                            if (op.sync == nullptr) {
                                return fail("back-edge without a sync "
                                            "map");
                            }
                            break;
                        case T2LirKind::CallBif:
                            /* Both T1 addresses and both callee handles
                             * must be resolved: the site (yield/error),
                             * the continuation (trap/trace CP), the
                             * export and the BIF C function. */
                            if (op.t1_pc_fail == nullptr) {
                                return fail("bif call without a T1 site");
                            }
                            if (op.t1_pc_cont == nullptr) {
                                return fail("bif call without a T1 "
                                            "continuation");
                            }
                            if (op.exp == nullptr || op.target == nullptr) {
                                return fail("bif call without a resolved "
                                            "export/function");
                            }
                            break;
                        default:
                            break;
                        }

                        /* Terminators (and the ops that absorb them, or end
                         * the T2 region: non-tail calls demote-on-return)
                         * must be last in the block. */
                        if (is_term && i + 1 != b.ops.size() &&
                            op.kind != T2LirKind::Call &&
                            op.kind != T2LirKind::CallExt) {
                            return fail(std::string(name) +
                                        ": terminator not last in block");
                        }
                    }
                }

                return true;
            }
        };

        /* ------------------------------------------------------------------ *
         * The allocator                                                       *
         * ------------------------------------------------------------------ */

        /* A use position: where a value is consumed, and the location the
         * consumer requires. Under the P1-identity policy every use
         * requires the canonical slot isel recorded; sync-map mentions are
         * the pin constraints (LSRA fixed uses). */
        struct T2Use {
            uint32_t pos;
            PhysLoc loc;
            bool from_sync;
        };

        struct T2Range {
            uint32_t from; /* inclusive  */
            uint32_t to;   /* inclusive  */
        };

        /* A live interval per SSA value (PLAN/T2FULL/09 §4's
         * T2Interval): multi-range (correct across disjoint CFG paths),
         * with the canonical fixed slots carried per use rather than as
         * one fixed_slot — Trim renumbers Y homes mid-interval, so the
         * canonical home is positional, not a property of the def. */
        struct T2Interval {
            uint32_t value_id = T2_NO_VALUE;
            std::vector<T2Range> ranges; /* normalized: sorted, disjoint */
            std::vector<T2Use> uses;     /* sorted by pos               */
            uint32_t def_pos = UINT32_MAX;
            PhysLoc def_loc = PhysLoc::none();
            bool untagged = false;
            /* Relaxation output (P2 commit 3+): None = the interval
             * lives in its canonical slots (identity placement). */
            PhysLoc assigned_phys = PhysLoc::none();

            bool empty() const {
                return ranges.empty() && uses.empty() &&
                       def_pos == UINT32_MAX;
            }

            bool covers(uint32_t pos) const {
                for (const T2Range &r : ranges) {
                    if (r.from <= pos && pos <= r.to) {
                        return true;
                    }
                }
                return false;
            }
        };

        constexpr uint32_t loc_key(PhysLoc l) {
            return ((uint32_t)l.kind << 16) | l.num;
        }

        struct RegAlloc {
            T2LirFunction &lir;
            std::string *err;

            uint32_t nvals = 0;
            size_t words = 0;

            /* Linearized positions. */
            std::vector<uint32_t> entry_pos, exit_pos;   /* per block   */
            std::vector<std::vector<uint32_t>> op_pos;   /* [block][op] */

            /* CFG successors per block. */
            std::vector<std::vector<uint32_t>> succs;

            /* Liveness bitsets per block. */
            std::vector<std::vector<uint64_t>> live_in, live_out;
            std::vector<std::vector<uint64_t>> use_set, def_set;

            std::vector<T2Interval> intervals; /* indexed by value id */

            bool fail(const std::string &msg) {
                if (err) {
                    *err = "regalloc: " + msg;
                }
                return false;
            }

            /* ---- bitset helpers ---------------------------------------- */

            static void bs_set(std::vector<uint64_t> &s, uint32_t v) {
                s[v / 64] |= uint64_t(1) << (v % 64);
            }
            static void bs_clear(std::vector<uint64_t> &s, uint32_t v) {
                s[v / 64] &= ~(uint64_t(1) << (v % 64));
            }
            static bool bs_test(const std::vector<uint64_t> &s, uint32_t v) {
                return (s[v / 64] >> (v % 64)) & 1;
            }

            /* ---- shared per-op operand iteration ----------------------- */

            template<typename F>
            void for_each_src(const T2LirOp &op, F fn) const {
                for (uint8_t i = 0; i < op.num_srcs; i++) {
                    fn(op.srcs[i]);
                }
                for (uint32_t i = 0; i < op.num_srcs_ext; i++) {
                    fn(lir.src_pool[op.pool_first + i]);
                }
            }

            bool op_is_call_clobber(const T2LirOp &op) const {
                switch (op.kind) {
                case T2LirKind::Call:
                case T2LirKind::CallExt:
                case T2LirKind::CallBif:
                    return true;
                default:
                    return false;
                }
            }

            /* ---- 1. positions + successors ------------------------------ */

            void number_positions() {
                uint32_t next = 2;

                entry_pos.resize(lir.blocks.size());
                exit_pos.resize(lir.blocks.size());
                op_pos.resize(lir.blocks.size());
                succs.assign(lir.blocks.size(), {});

                for (const T2LirBlock &b : lir.blocks) {
                    entry_pos[b.id] = next;
                    next += 2;
                    op_pos[b.id].reserve(b.ops.size());
                    for (const T2LirOp &op : b.ops) {
                        op_pos[b.id].push_back(next);
                        next += 2;

                        auto add_succ = [&](uint32_t s) {
                            if (s == T2_LIR_NO_BLOCK) {
                                return;
                            }
                            auto &v = succs[b.id];
                            if (std::find(v.begin(), v.end(), s) == v.end()) {
                                v.push_back(s);
                            }
                        };
                        add_succ(op.succ_then);
                        add_succ(op.succ_else);
                        if (op.kind == T2LirKind::Switch) {
                            for (uint32_t c = 0; c < op.num_cases; c++) {
                                add_succ(lir.switch_cases[op.first_case + c]
                                                 .target);
                            }
                            add_succ(op.default_target);
                        }
                    }
                    exit_pos[b.id] = next;
                    next += 2;
                }
            }

            /* ---- 2. per-block use/def + dataflow ------------------------ */

            /* Values a sync map names, with their pinned slots. */
            template<typename F>
            static void for_each_sync_pin(const T2SyncMap *m, F fn) {
                if (m == nullptr) {
                    return;
                }
                for (uint32_t i = 0; i < m->x_live; i++) {
                    fn(m->x[i]->id, PhysLoc::xreg((uint16_t)i));
                }
                if (m->frame_size != T2_NO_FRAME) {
                    for (int32_t i = 0; i < m->frame_size; i++) {
                        fn(m->y[i]->id, PhysLoc::yreg((uint16_t)i));
                    }
                }
            }

            bool compute_liveness() {
                size_t n = lir.blocks.size();

                live_in.assign(n, std::vector<uint64_t>(words, 0));
                live_out.assign(n, std::vector<uint64_t>(words, 0));
                use_set.assign(n, std::vector<uint64_t>(words, 0));
                def_set.assign(n, std::vector<uint64_t>(words, 0));

                /* Per-block upward-exposed uses + defs. Phi defs count at
                 * block entry (before every use in the block); phi
                 * *inputs* are uses on the predecessor edge and appear in
                 * the pred's live_out (added in the dataflow below). */
                for (const T2LirBlock &b : lir.blocks) {
                    auto &use = use_set[b.id];
                    auto &def = def_set[b.id];

                    for (const T2LirPhi &phi : b.phis) {
                        bs_set(def, phi.value);
                    }
                    if (b.id == 0) {
                        for (uint32_t v = 0; v < nvals; v++) {
                            if (lir.param_x[v] >= 0) {
                                bs_set(def, v);
                            }
                        }
                    }

                    for (const T2LirOp &op : b.ops) {
                        auto mention = [&](uint32_t v) {
                            if (v != T2_NO_VALUE && !bs_test(def, v)) {
                                bs_set(use, v);
                            }
                        };
                        for_each_src(op, [&](const T2LirSrc &s) {
                            if (!s.is_const) {
                                mention(s.value);
                            }
                        });
                        for_each_sync_pin(op.sync,
                                          [&](uint32_t v, PhysLoc) {
                                              mention(v);
                                          });
                        if (op.dst_value != T2_NO_VALUE) {
                            bs_set(def, op.dst_value);
                        }
                        if (op.dst2_value != T2_NO_VALUE) {
                            bs_set(def, op.dst2_value);
                        }
                    }
                }

                /* Backward dataflow to fixpoint. */
                bool changed = true;
                while (changed) {
                    changed = false;
                    for (size_t bi = n; bi-- > 0;) {
                        const T2LirBlock &b = lir.blocks[bi];
                        std::vector<uint64_t> out(words, 0);

                        for (uint32_t s : succs[b.id]) {
                            const T2LirBlock &sb = lir.blocks[s];
                            std::vector<uint64_t> in = live_in[s];

                            for (const T2LirPhi &phi : sb.phis) {
                                bs_clear(in, phi.value);
                            }
                            for (size_t w = 0; w < words; w++) {
                                out[w] |= in[w];
                            }
                            for (const T2LirPhi &phi : sb.phis) {
                                for (const T2LirPhi::In &pin : phi.ins) {
                                    if (pin.pred_block == b.id &&
                                        pin.value != T2_NO_VALUE) {
                                        bs_set(out, pin.value);
                                    }
                                }
                            }
                        }

                        std::vector<uint64_t> in(words);
                        for (size_t w = 0; w < words; w++) {
                            in[w] = use_set[b.id][w] |
                                    (out[w] & ~def_set[b.id][w]);
                        }

                        if (out != live_out[b.id] || in != live_in[b.id]) {
                            live_out[b.id] = std::move(out);
                            live_in[b.id] = std::move(in);
                            changed = true;
                        }
                    }
                }

                /* SSA closure: nothing but the parameters may be live
                 * into the entry block. */
                for (uint32_t v = 0; v < nvals; v++) {
                    if (bs_test(live_in[0], v) && lir.param_x[v] < 0) {
                        return fail("value v" + std::to_string(v) +
                                    " is live at function entry without a "
                                    "definition");
                    }
                }
                return true;
            }

            /* ---- 3. interval construction ------------------------------- */

            void add_range(uint32_t v, uint32_t from, uint32_t to) {
                intervals[v].value_id = v;
                intervals[v].ranges.push_back(T2Range{from, to});
            }

            void add_use(uint32_t v, uint32_t pos, PhysLoc loc, bool sync) {
                intervals[v].value_id = v;
                intervals[v].uses.push_back(T2Use{pos, loc, sync});
            }

            bool build_intervals() {
                intervals.assign(nvals, T2Interval());

                for (uint32_t v = 0; v < nvals; v++) {
                    intervals[v].untagged =
                            v < lir.value_flags.size() &&
                            (lir.value_flags[v] & T2_LIR_VF_UNTAGGED) != 0;
                }

                for (size_t bi = lir.blocks.size(); bi-- > 0;) {
                    const T2LirBlock &b = lir.blocks[bi];
                    std::vector<uint64_t> live = live_out[b.id];

                    /* Phi inputs on each outgoing edge are uses at this
                     * block's exit, in the phi's home slot (the identity
                     * merge contract; already in live_out). */
                    for (uint32_t s : succs[b.id]) {
                        for (const T2LirPhi &phi : lir.blocks[s].phis) {
                            for (const T2LirPhi::In &pin : phi.ins) {
                                if (pin.pred_block == b.id &&
                                    pin.value != T2_NO_VALUE) {
                                    add_use(pin.value,
                                            exit_pos[b.id],
                                            phi.home,
                                            false);
                                }
                            }
                        }
                    }

                    /* Everything in live_out spans the whole block until
                     * a def trims it. */
                    for (uint32_t v = 0; v < nvals; v++) {
                        if (bs_test(live, v)) {
                            add_range(v, entry_pos[b.id], exit_pos[b.id]);
                        }
                    }

                    for (size_t oi = b.ops.size(); oi-- > 0;) {
                        const T2LirOp &op = b.ops[oi];
                        uint32_t pos = op_pos[b.id][oi];

                        auto def = [&](uint32_t v, PhysLoc loc) {
                            if (v == T2_NO_VALUE) {
                                return;
                            }
                            T2Interval &iv = intervals[v];

                            iv.value_id = v;
                            iv.def_pos = pos;
                            iv.def_loc = loc;
                            if (bs_test(live, v)) {
                                /* Trim the block-spanning range to start
                                 * at the def. */
                                iv.ranges.back().from = pos;
                                bs_clear(live, v);
                            } else {
                                /* Dead def: occupies its slot at the op. */
                                add_range(v, pos, pos + 1);
                            }
                        };
                        auto use = [&](uint32_t v, PhysLoc loc, bool sync) {
                            if (v == T2_NO_VALUE) {
                                return;
                            }
                            add_use(v, pos, loc, sync);
                            if (!bs_test(live, v)) {
                                add_range(v, entry_pos[b.id], pos + 1);
                                bs_set(live, v);
                            }
                        };

                        def(op.dst_value, op.dst);
                        def(op.dst2_value, op.dst2);
                        for_each_src(op, [&](const T2LirSrc &s) {
                            if (!s.is_const) {
                                use(s.value, s.loc, false);
                            }
                        });
                        for_each_sync_pin(op.sync,
                                          [&](uint32_t v, PhysLoc loc) {
                                              use(v, loc, true);
                                          });
                    }

                    /* Phi defs live from block entry. */
                    for (const T2LirPhi &phi : b.phis) {
                        T2Interval &iv = intervals[phi.value];

                        iv.value_id = phi.value;
                        iv.def_pos = entry_pos[b.id];
                        iv.def_loc = phi.home;
                        if (bs_test(live, phi.value)) {
                            iv.ranges.back().from = entry_pos[b.id];
                            bs_clear(live, phi.value);
                        }
                    }

                    /* Parameters + the entry sync map (block 0 only). */
                    if (b.id == 0) {
                        for (uint32_t v = 0; v < nvals; v++) {
                            if (lir.param_x[v] < 0) {
                                continue;
                            }
                            T2Interval &iv = intervals[v];

                            iv.value_id = v;
                            iv.def_pos = entry_pos[0];
                            iv.def_loc = PhysLoc::xreg(
                                    (uint16_t)lir.param_x[v]);
                            if (bs_test(live, v)) {
                                iv.ranges.back().from = entry_pos[0];
                                bs_clear(live, v);
                            }
                        }
                        for_each_sync_pin(lir.entry_sync,
                                          [&](uint32_t v, PhysLoc loc) {
                                              add_use(v,
                                                      entry_pos[0],
                                                      loc,
                                                      true);
                                          });
                    }
                }

                /* Normalize: sort + merge ranges, sort uses. */
                for (T2Interval &iv : intervals) {
                    if (iv.empty()) {
                        continue;
                    }
                    std::sort(iv.ranges.begin(),
                              iv.ranges.end(),
                              [](const T2Range &a, const T2Range &b) {
                                  return a.from < b.from;
                              });
                    std::vector<T2Range> merged;
                    for (const T2Range &r : iv.ranges) {
                        if (!merged.empty() &&
                            r.from <= merged.back().to + 1) {
                            merged.back().to =
                                    std::max(merged.back().to, r.to);
                        } else {
                            merged.push_back(r);
                        }
                    }
                    iv.ranges = std::move(merged);
                    std::sort(iv.uses.begin(),
                              iv.uses.end(),
                              [](const T2Use &a, const T2Use &b) {
                                  return a.pos < b.pos;
                              });
                }

                return true;
            }

            /* ---- 4. placement walk (slot space) -------------------------- */

            bool placement_walk() {
                for (const T2LirBlock &b : lir.blocks) {
                    /* slot -> value, adopt-on-first-mention. */
                    std::unordered_map<uint32_t, uint32_t> state;

                    for (const T2LirPhi &phi : b.phis) {
                        state[loc_key(phi.home)] = phi.value;
                    }
                    if (b.id == 0) {
                        for (uint32_t v = 0; v < nvals; v++) {
                            if (lir.param_x[v] >= 0) {
                                state[loc_key(PhysLoc::xreg(
                                        (uint16_t)lir.param_x[v]))] = v;
                            }
                        }
                    }

                    auto read = [&](PhysLoc loc,
                                    uint32_t v,
                                    const char *what) -> bool {
                        if (v == T2_NO_VALUE || !loc.is_slot()) {
                            return true;
                        }
                        auto it = state.find(loc_key(loc));

                        if (it == state.end()) {
                            state[loc_key(loc)] = v;
                            return true;
                        }
                        if (it->second != v) {
                            return fail(std::string(what) + " in block " +
                                        std::to_string(b.id) +
                                        " reads a slot as v" +
                                        std::to_string(v) +
                                        " but placement says v" +
                                        std::to_string(it->second));
                        }
                        return true;
                    };

                    auto clobber_x = [&]() {
                        for (auto it = state.begin(); it != state.end();) {
                            if ((it->first >> 16) ==
                                (uint32_t)PhysLoc::Kind::XReg) {
                                it = state.erase(it);
                            } else {
                                ++it;
                            }
                        }
                    };
                    auto clobber_y = [&]() {
                        for (auto it = state.begin(); it != state.end();) {
                            if ((it->first >> 16) ==
                                (uint32_t)PhysLoc::Kind::YReg) {
                                it = state.erase(it);
                            } else {
                                ++it;
                            }
                        }
                    };

                    for (size_t oi = 0; oi < b.ops.size(); oi++) {
                        const T2LirOp &op = b.ops[oi];
                        const char *name = t2_lir_kind_name(op.kind);
                        uint32_t pos = op_pos[b.id][oi];
                        bool ok = true;

                        /* Reads first (op operands + sync pins). */
                        for_each_src(op, [&](const T2LirSrc &s) {
                            if (!s.is_const && ok) {
                                ok = read(s.loc, s.value, name);
                            }
                        });
                        if (!ok) {
                            return false;
                        }
                        {
                            bool pin_ok = true;
                            for_each_sync_pin(
                                    op.sync,
                                    [&](uint32_t v, PhysLoc loc) {
                                        if (pin_ok) {
                                            pin_ok = read(loc, v, name);
                                        }
                                    });
                            if (!pin_ok) {
                                return false;
                            }
                        }

                        /* Frame / clobber effects. */
                        switch (op.kind) {
                        case T2LirKind::Trim: {
                            /* Post-trim Y_i = pre-trim Y_{i+drop}. */
                            uint32_t drop = (uint32_t)op.imm;
                            std::unordered_map<uint32_t, uint32_t> ns;

                            for (const auto &e : state) {
                                if ((e.first >> 16) !=
                                    (uint32_t)PhysLoc::Kind::YReg) {
                                    ns.insert(e);
                                    continue;
                                }
                                uint32_t idx = e.first & 0xFFFF;

                                if (idx >= drop) {
                                    ns[loc_key(PhysLoc::yreg(
                                            (uint16_t)(idx - drop)))] =
                                            e.second;
                                }
                            }
                            state = std::move(ns);
                            break;
                        }
                        case T2LirKind::Allocate:
                        case T2LirKind::Deallocate:
                            /* A fresh frame's slots are garbage; a popped
                             * frame's are gone. */
                            clobber_y();
                            break;
                        case T2LirKind::Call:
                        case T2LirKind::CallExt:
                        case T2LirKind::CallBif: {
                            /* The X file is clobbered across the call
                             * (only the result lands in X0, written
                             * below). Check clobber liveness first: a
                             * value live past the call whose only known
                             * placement is an X slot has no home on the
                             * other side. */
                            for (const auto &e : state) {
                                uint32_t v = e.second;

                                if ((e.first >> 16) !=
                                            (uint32_t)PhysLoc::Kind::XReg ||
                                    v == T2_NO_VALUE ||
                                    !intervals[v].covers(pos + 2)) {
                                    continue;
                                }
                                /* Also placed in a Y slot? */
                                bool has_y = false;
                                for (const auto &e2 : state) {
                                    if ((e2.first >> 16) ==
                                                (uint32_t)PhysLoc::Kind::
                                                        YReg &&
                                        e2.second == v) {
                                        has_y = true;
                                        break;
                                    }
                                }
                                if (!has_y) {
                                    return fail(
                                            "v" + std::to_string(v) +
                                            " is live across a call in "
                                            "block " +
                                            std::to_string(b.id) +
                                            " with no frame home");
                                }
                            }
                            clobber_x();
                            break;
                        }
                        default:
                            break;
                        }

                        /* Writes. */
                        auto write = [&](PhysLoc loc, uint32_t v) {
                            if (!loc.is_slot()) {
                                return;
                            }
                            if (v == T2_NO_VALUE) {
                                /* Runtime-defined content (e.g. a BIF
                                 * result consumed by a synthesized
                                 * epilogue): unknown. */
                                state.erase(loc_key(loc));
                            } else {
                                state[loc_key(loc)] = v;
                            }
                        };
                        if (!op.dst.is_none()) {
                            write(op.dst, op.dst_value);
                        }
                        if (!op.dst2.is_none()) {
                            write(op.dst2, op.dst2_value);
                        }
                    }
                }

                return true;
            }

            /* ---- 5. untagged discipline --------------------------------- */

            bool check_untagged() {
                for (const T2Interval &iv : intervals) {
                    if (iv.empty() || !iv.untagged) {
                        continue;
                    }
                    if (iv.def_loc.is_slot()) {
                        return fail("untagged v" +
                                    std::to_string(iv.value_id) +
                                    " defined into an X/Y slot");
                    }
                    for (const T2Use &u : iv.uses) {
                        if (u.from_sync) {
                            return fail("untagged v" +
                                        std::to_string(iv.value_id) +
                                        " named by a sync map");
                        }
                        if (u.loc.is_slot()) {
                            return fail("untagged v" +
                                        std::to_string(iv.value_id) +
                                        " used from an X/Y slot");
                        }
                    }
                }
                return true;
            }

            /* ---- 6. assignment (the policy) ------------------------------ */

            /* P2 commit-1 policy: identity — every interval keeps its
             * canonical per-use placement; no PhysLoc::Phys is assigned,
             * so the LIR is emitted exactly as isel produced it and the
             * blob is byte-identical to P1. The relaxation (commit 3)
             * assigns Phys to intervals that cross no sync point. */
            void assign() {
                for (T2Interval &iv : intervals) {
                    iv.assigned_phys = PhysLoc::none();
                }
            }

            /* ---- debug dump ---------------------------------------------- */

            void dump() const {
                if (getenv("T2_RA_DUMP") == nullptr) {
                    return;
                }
                erts_fprintf(stderr,
                             "t2_regalloc: %T:%T/%u  %u values\n",
                             lir.module,
                             lir.function,
                             lir.arity,
                             (unsigned)nvals);
                for (const T2Interval &iv : intervals) {
                    if (iv.empty()) {
                        continue;
                    }
                    std::string line = "  v" + std::to_string(iv.value_id);

                    line += iv.untagged ? " raw " : " ";
                    for (const T2Range &r : iv.ranges) {
                        line += "[" + std::to_string(r.from) + "," +
                                std::to_string(r.to) + "]";
                    }
                    line += " uses:";
                    for (const T2Use &u : iv.uses) {
                        line += " " + std::to_string(u.pos);
                        if (u.from_sync) {
                            line += "*";
                        }
                    }
                    erts_fprintf(stderr, "%s\n", line.c_str());
                }
            }

            bool run() {
                nvals = lir.num_values;
                words = (nvals + 63) / 64;

                if (lir.param_x.size() < nvals) {
                    return fail("value table inconsistent");
                }
                if (nvals == 0) {
                    /* Hand-built LIR (selftest): nothing to allocate;
                     * the structural verification already ran. */
                    return true;
                }

                number_positions();
                if (!compute_liveness()) {
                    return false;
                }
                if (!build_intervals()) {
                    return false;
                }
                if (!placement_walk()) {
                    return false;
                }
                if (!check_untagged()) {
                    return false;
                }
                assign();
                dump();
                return true;
            }
        };

    } /* anonymous namespace */

    bool t2_regalloc(T2LirFunction &lir, std::string *err) {
        VerifyStructure vs{lir, err};

        if (!vs.run()) {
            return false;
        }

        RegAlloc ra{lir, err};
        return ra.run();
    }

} /* namespace erts_t2 */
