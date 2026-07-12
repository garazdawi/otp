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
 * T2-Full tier-2 JIT: BEAM generic-op -> SSA builder (PLAN/T2FULL/07 §2).
 *
 * Re-decodes the retained code chunk with beamfile_get_code against a
 * transient BeamFile view over the retained tables. This yields
 * generic, PRE-transform ops -- the loader's transform engine
 * (erts_transform_engine) is applied only to the loader's own stream,
 * never to the chunk, and the builder must not invoke it: generic ops
 * are post-beam_ssa_codegen BEAM assembly, exactly the level the SSA
 * reconstruction wants.
 *
 * SSA construction is the Braun et al. on-the-fly algorithm: local
 * value numbering per block via per-block definition maps, operandless
 * phis recorded on unsealed blocks, and a seal pass once the full CFG
 * is known (translation is a single forward pass over the ops, so
 * every block is sealed at the end when all predecessor edges exist).
 *
 * The op subset handled here is exactly the supported set of
 * t2_eligible.c (single source of truth); hitting an unsupported op in
 * an eligible function is reported as builder/table drift.
 *
 * P0 exception edges: guard failures to a real label become branch
 * edges; failure targets of {f,0} (TAG_p) raise and get no CFG edge;
 * jumps to the function's func_info label and the badmatch/if_end/
 * case_end ops become synthesized error-exit blocks terminated by a
 * tail call to erlang:error. No exception CFG is modelled.
 *
 * P1: the builder additionally snapshots its own Braun reg->value
 * bookkeeping at every sync-point op into T2SyncMap metadata, makes
 * allocate/deallocate/trim first-class ops, and records the decoded
 * canonical home of every value (see t2_hir.hpp's P1 header comment
 * for the conventions and their rationale). The stack-frame size at
 * every point comes from a small fixpoint pre-pass over the decoded
 * ops (frame size is a property of program points, not of SSA values,
 * and a label may be reached only by edges that appear later in the
 * stream).
 */

#include "t2_hir.hpp"

extern "C"
{
#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "erl_alloc.h"
#include "erl_vm.h"
#include "erl_message.h"
#include "erl_bits.h"
#include "beam_file.h"
#include "beam_opcodes.h"

#include "t2_retain.h"
}

#include <cstring>
#include <map>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "t2_isel.hpp"

namespace erts_t2 {

    namespace {

        /* ------------------------------------------------------------------ *
         * Module decode                                                      *
         * ------------------------------------------------------------------ */

        struct DecodedArg {
            UWord type;
            UWord val;
        };

        struct DecodedOp {
            int op;
            uint32_t beam_idx; /* ordinal within the function */
            std::vector<DecodedArg> args;
        };

        struct FunctionCode {
            Eterm module;
            Eterm function;
            uint32_t arity;
            UWord func_label; /* the func_info label (raises function_clause) */
            UWord entry_label;          /* where callers land */
            std::vector<DecodedOp> ops; /* entry label onwards */
        };

        struct ModuleDecode {
            BeamFile view;
            const ErtsT2RetainedCode *ret = nullptr;
            std::vector<FunctionCode> functions;

            /* label -> function index, for both entry and func_info labels.
             * Key is a plain uint64_t, not UWord: UWord carries an alignment
             * attribute (sys.h) that GCC rejects as a template argument
             * (-Werror=ignored-attributes). */
            std::unordered_map<uint64_t, size_t> label_fn;

            void init(const ErtsT2RetainedCode *ret_) {
                ret = ret_;
                sys_memset(&view, 0, sizeof(view));

                view.atoms.count = ret->atom_count;
                view.atoms.entries = ret->atoms;
                view.module = ret->atom_count > 1 ? ret->atoms[1] : NIL;

                view.types.count = ret->type_count;
                view.types.fallback = ret->types_fallback;
                view.types.entries = ret->types;

                /* Only the count is consulted during decode (TAG_q validation);
                 * values are resolved through ret->literal_map instead. */
                view.static_literals.count = ret->literal_count;

                view.code.data = ret->code;
                view.code.size = (Sint32)ret->code_size;
                view.code.function_count = ret->function_count;
                view.code.label_count = ret->label_count;
                view.code.max_opcode = ret->max_opcode;
            }

            /* Decoding may create dynamic literals (bignum immediates in the
             * code are marshalled into literal-table fragments); they belong to
             * this view and must be freed with it. Mirrors the dynamic-literal
             * half of beamfile_free. */
            void cleanup() {
                BeamFile_LiteralTable *literals = &view.dynamic_literals;

                if (literals->entries != nullptr) {
                    for (Sint32 i = 0; i < literals->count; i++) {
                        ErlHeapFragment *fragment =
                                literals->entries[i].heap_fragments;

                        while (fragment != nullptr) {
                            ErlHeapFragment *next = fragment->next;

                            erts_cleanup_offheap(&fragment->off_heap);
                            ERTS_HEAP_FREE(ERTS_ALC_T_PREPARED_CODE,
                                           (void *)fragment,
                                           ERTS_HEAP_FRAG_SIZE(fragment->size));

                            fragment = next;
                        }
                    }

                    erts_free(ERTS_ALC_T_PREPARED_CODE, literals->entries);
                    literals->entries = nullptr;
                }
            }
        };

        /* Literal resolver for erts_t2_bs_match_check at build time:
         * static literals resolve through the retained literal_map
         * (alive as long as the module); dynamic literals cannot be a
         * flags list, so a negative index rejects. */
        Eterm builder_bs_lit(void *env, SWord idx) {
            const ErtsT2RetainedCode *ret = (const ErtsT2RetainedCode *)env;

            if (idx >= 0 && idx < ret->literal_count) {
                return ret->literal_map[idx];
            }
            return THE_NON_VALUE;
        }

        bool decode_module(const ErtsT2RetainedCode *ret,
                           ModuleDecode &md,
                           std::string *err) {
            BeamOpAllocator op_alloc;
            BeamCodeReader *reader;
            BeamOp *op;

            FunctionCode *cur = nullptr;
            bool expect_entry_label = false;
            bool done = false;
            uint32_t idx = 0;

            md.init(ret);

            beamopallocator_init(&op_alloc);
            reader = beamfile_get_code(&md.view, &op_alloc);

            while (!done && beamcodereader_next(reader, &op)) {
                switch (op->op) {
                case genop_int_func_start_5:
                    md.functions.emplace_back();
                    cur = &md.functions.back();
                    cur->func_label = op->a[0].val;
                    cur->module = op->a[2].val;
                    cur->function = op->a[3].val;
                    cur->arity = (uint32_t)op->a[4].val;
                    cur->entry_label = 0;
                    expect_entry_label = true;
                    idx = 0;
                    break;
                case genop_int_func_end_2:
                    cur = nullptr;
                    break;
                case genop_int_code_end_0:
                    done = true;
                    break;
                default:
                    if (cur != nullptr) {
                        if (expect_entry_label) {
                            if (op->op != genop_label_1) {
                                *err = "no entry label after int_func_start";
                                beamopallocator_free_op(&op_alloc, op);
                                goto decode_error;
                            }
                            cur->entry_label = op->a[0].val;
                            expect_entry_label = false;
                        }

                        DecodedOp dop;
                        dop.op = op->op;
                        dop.beam_idx = idx++;
                        dop.args.reserve(op->arity);
                        for (int i = 0; i < op->arity; i++) {
                            dop.args.push_back(
                                    DecodedArg{op->a[i].type, op->a[i].val});
                        }
                        cur->ops.push_back(std::move(dop));
                    }
                    break;
                }

                beamopallocator_free_op(&op_alloc, op);
            }

            beamcodereader_close(reader);
            beamopallocator_dtor(&op_alloc);

            if (!done) {
                *err = "unexpected end of code chunk";
                return false;
            }

            for (size_t i = 0; i < md.functions.size(); i++) {
                md.label_fn[md.functions[i].entry_label] = i;
                md.label_fn[md.functions[i].func_label] = i;
            }

            return true;

        decode_error:
            beamcodereader_close(reader);
            beamopallocator_dtor(&op_alloc);
            return false;
        }

        /* ------------------------------------------------------------------ *
         * Function builder                                                   *
         * ------------------------------------------------------------------ */

        /* Braun-variable keys: X registers as-is, Y registers offset. The
         * key doubles as the canonical-home register encoding recorded on
         * ops and sync maps (t2_hir.hpp). */
        constexpr uint32_t Y_VAR_BASE = (uint32_t)T2_YREG_BASE;

        /* Frame size at a point: T2_NO_FRAME, a slot count, unknown
         * (label never reached by the fixpoint — unreachable code), or
         * conflicting (an error-exit label reached from guard fails at
         * differing frame depths — legal BEAM; such blocks only raise,
         * so nothing frame-dependent may appear in them). */
        constexpr int32_t FRAME_UNKNOWN = -2;
        constexpr int32_t FRAME_CONFLICT = -3;

        /* An operand read: the SSA value plus the BEAM register it was
         * read from (T2_REG_NONE for constants). */
        struct SrcVal {
            T2Value *v;
            int32_t reg;
        };

        class FunctionBuilder {
        public:
            FunctionBuilder(ModuleDecode &md_,
                            const FunctionCode &fc_,
                            uint32_t fn_index_,
                            bool tolerant_ = false)
                    : md(md_), ret(md_.ret), fc(fc_), fn_index(fn_index_),
                      tolerant(tolerant_) {
            }

            std::unique_ptr<T2Function> build(std::string *err);

        private:
            ModuleDecode &md;
            const ErtsT2RetainedCode *ret;
            const FunctionCode &fc;
            uint32_t fn_index;

            /* Tolerant mode (P1c-2, t2_build_for_p1): a translation
             * failure degrades the current block into an Opaque leaf
             * instead of failing the whole build, so a partially
             * buildable multi-clause function (e.g. an is_list-guarded
             * fold clause next to map-matching clauses) still yields
             * classifiable HIR. Classification-only: the result is
             * never lowered. */
            bool tolerant;
            bool skipping = false;

            std::unique_ptr<T2Function> fn;

            std::unordered_map<uint64_t, T2BasicBlock *> label_block;
            T2BasicBlock *error_block = nullptr;
            T2BasicBlock *cur = nullptr;
            const DecodedOp *cur_op = nullptr;

            /* Stack-frame size at the current translation point, and the
             * per-label frame-in map from the pre-pass. */
            int32_t cur_frame = T2_NO_FRAME;
            std::unordered_map<uint64_t, int32_t> label_frame;

            /* Braun state, indexed by block id. */
            std::vector<std::unordered_map<uint32_t, T2Value *>> defs;
            std::vector<std::vector<std::pair<uint32_t, T2Op *>>> incomplete;
            std::vector<std::vector<T2BasicBlock *>> preds;

            bool failed = false;
            std::string error;

            void fail(const std::string &what) {
                if (!failed) {
                    failed = true;
                    error = what;
                }
            }

            void grow_to(uint32_t block_id) {
                size_t need = block_id + 1;
                if (defs.size() < need) {
                    defs.resize(need);
                    incomplete.resize(need);
                    preds.resize(need);
                }
            }

            T2BasicBlock *new_block() {
                T2BasicBlock *b = fn->new_block();
                grow_to(b->id);
                return b;
            }

            void add_edge(T2BasicBlock *from, T2BasicBlock *to) {
                auto &v = preds[to->id];
                for (T2BasicBlock *p : v) {
                    if (p == from) {
                        return; /* deduplicated, matching T2Function::finalize
                                 */
                    }
                }
                v.push_back(from);
            }

            /* ---- Braun on-the-fly SSA ------------------------------------ */

            void write_var(T2BasicBlock *b, uint32_t var, T2Value *v) {
                defs[b->id][var] = v;
            }

            T2Value *read_var(T2BasicBlock *b, uint32_t var) {
                auto &map = defs[b->id];
                auto it = map.find(var);

                if (it != map.end()) {
                    return it->second;
                }
                return read_var_recursive(b, var);
            }

            T2Value *read_var_recursive(T2BasicBlock *b, uint32_t var) {
                T2Value *val;

                if (!b->sealed) {
                    T2Op *phi = fn->new_phi(b, T2Type::any());
                    phi->beam_idx = cur_op != nullptr ? cur_op->beam_idx : 0;
                    /* The phi's canonical home is the register it merges. */
                    phi->dst_reg = (int32_t)var;
                    incomplete[b->id].push_back({var, phi});
                    val = phi->result;
                } else if (preds[b->id].size() == 1) {
                    val = read_var(preds[b->id][0], var);
                } else if (preds[b->id].empty()) {
                    fail("read of undefined register");
                    return nullptr;
                } else {
                    T2Op *phi = fn->new_phi(b, T2Type::any());
                    phi->beam_idx = cur_op != nullptr ? cur_op->beam_idx : 0;
                    phi->dst_reg = (int32_t)var;
                    /* Break lookup cycles through loops before filling. */
                    write_var(b, var, phi->result);
                    val = add_phi_operands(b, var, phi);
                }

                if (val != nullptr) {
                    write_var(b, var, val);
                }
                return val;
            }

            T2Value *add_phi_operands(T2BasicBlock *b,
                                      uint32_t var,
                                      T2Op *phi) {
                std::vector<T2Value *> vals;
                std::vector<T2BasicBlock *> inputs;

                for (T2BasicBlock *pred : preds[b->id]) {
                    T2Value *v = read_var(pred, var);

                    if (v == nullptr) {
                        return nullptr;
                    }
                    vals.push_back(v);
                    inputs.push_back(pred);
                }

                fn->set_phi_inputs(phi, vals, inputs);
                return phi->result;
            }

            void seal_block(T2BasicBlock *b) {
                for (auto &entry : incomplete[b->id]) {
                    add_phi_operands(b, entry.first, entry.second);
                }
                incomplete[b->id].clear();
                b->sealed = true;
            }

            /* ---- operand handling -----------------------------------------
             */

            /* AOT type-chunk seeding: register operands carry the type-table
             * slot packed above the register bits. */
            void seed_type(T2Value *v, UWord raw_reg_val) {
                UWord tidx = raw_reg_val >> 10;

                if (v == nullptr || tidx == 0 || ret->types_fallback ||
                    tidx >= (UWord)ret->type_count) {
                    return;
                }

                if (v->type.is_any()) {
                    v->type = T2Type::from_beam(ret->types[tidx]);
                } else {
                    v->type = v->type.meet(T2Type::from_beam(ret->types[tidx]));
                }
            }

            static uint32_t reg_var(const DecodedArg &a) {
                uint32_t reg = (uint32_t)(a.val & REG_MASK);
                return a.type == TAG_y ? Y_VAR_BASE + reg : reg;
            }

            T2Value *read_arg(const DecodedArg &a) {
                switch (a.type) {
                case TAG_x:
                case TAG_y: {
                    T2Value *v = read_var(cur, reg_var(a));
                    seed_type(v, a.val);
                    return v;
                }
                case TAG_i:
                    return fn->emit_const_int(cur, (Sint64)(SWord)a.val);
                case TAG_a:
                    return fn->emit_const_atom(cur, (Eterm)a.val);
                case TAG_n:
                    return fn->emit_const_nil(cur);
                case TAG_q: {
                    Sint index = (Sint)(SWord)a.val;
                    Eterm term;

                    if (index >= 0) {
                        if (index >= ret->literal_count) {
                            fail("static literal index out of range");
                            return nullptr;
                        }
                        term = ret->literal_map[index];
                    } else {
                        /* Dynamic literal created during this decode; the term
                         * lives in the view until ModuleDecode::cleanup. */
                        term = beamfile_get_literal(&md.view, index);
                    }
                    return fn->emit_const_literal(cur,
                                                  index >= 0 ? (uint32_t)index
                                                             : 0,
                                                  term,
                                                  T2Type::any());
                }
                default:
                    fail("unexpected source operand tag");
                    return nullptr;
                }
            }

            /* read_arg plus the canonical home the value was read from
             * (T2_REG_NONE for constants). */
            SrcVal read_arg_r(const DecodedArg &a) {
                int32_t reg = (a.type == TAG_x || a.type == TAG_y)
                                      ? (int32_t)reg_var(a)
                                      : T2_REG_NONE;
                return SrcVal{read_arg(a), reg};
            }

            /* Attach the operand-register array when any operand came
             * from a BEAM register. */
            void set_operand_regs(T2Op *op, const std::vector<SrcVal> &srcs) {
                bool any = false;

                for (const SrcVal &s : srcs) {
                    if (s.reg != T2_REG_NONE) {
                        any = true;
                        break;
                    }
                }
                if (!any) {
                    return;
                }

                op->operand_regs = fn->arena.alloc_array<int32_t>(srcs.size());
                for (size_t i = 0; i < srcs.size(); i++) {
                    op->operand_regs[i] = srcs[i].reg;
                }
            }

            void write_dst(const DecodedArg &a, T2Value *v) {
                if (a.type != TAG_x && a.type != TAG_y) {
                    fail("unexpected destination operand tag");
                    return;
                }
                if (v == nullptr) {
                    return;
                }
                seed_type(v, a.val);
                write_var(cur, reg_var(a), v);
            }

            /* write_dst for a freshly created op: additionally records the
             * decoded destination register as the value's canonical home.
             * Every op result that lands in a BEAM register must go
             * through here (identity-placement discipline). */
            void write_dst_new(const DecodedArg &a, T2Value *v) {
                if (v != nullptr && a.type != TAG_x && a.type != TAG_y) {
                    fail("unexpected destination operand tag");
                    return;
                }
                if (v != nullptr) {
                    if (v->def->dst_reg != T2_REG_NONE) {
                        fail("value rebound to a second home (builder drift)");
                        return;
                    }
                    v->def->dst_reg = (int32_t)reg_var(a);
                }
                write_dst(a, v);
            }

            /* ---- sync-point snapshots -------------------------------- */

            /* Snapshot the exact BEAM register state at the current
             * boundary from the Braun maps: X0..x_live-1 plus the whole
             * frame. See T2SyncMap in t2_hir.hpp for the conventions. */
            T2SyncMap *snapshot_sync(uint32_t x_live) {
                if (failed) {
                    return nullptr;
                }
                if (cur_frame == FRAME_UNKNOWN || cur_frame == FRAME_CONFLICT) {
                    fail(cur_frame == FRAME_UNKNOWN
                                 ? "sync point with unknown frame size "
                                   "(unreachable label?)"
                                 : "sync point in a frame-polymorphic error "
                                   "block");
                    return nullptr;
                }

                T2SyncMap *m = fn->arena.create<T2SyncMap>();

                m->x_live = x_live;
                m->x = fn->arena.alloc_array<T2Value *>(x_live);
                for (uint32_t i = 0; i < x_live; i++) {
                    T2Value *v = read_var(cur, i);

                    if (v == nullptr) {
                        fail("sync map: live X register undefined");
                        return nullptr;
                    }
                    m->x[i] = v;
                }

                m->frame_size = cur_frame;
                m->y = nullptr;
                if (cur_frame > 0) {
                    m->y = fn->arena.alloc_array<T2Value *>((size_t)cur_frame);
                    for (int32_t i = 0; i < cur_frame; i++) {
                        T2Value *v = read_var(cur, Y_VAR_BASE + (uint32_t)i);

                        if (v == nullptr) {
                            fail("sync map: frame Y slot undefined (compiler "
                                 "init contract violated?)");
                            return nullptr;
                        }
                        m->y[i] = v;
                    }
                }

                return m;
            }

            /* ---- control-flow helpers --------------------------------------
             */

            T2BasicBlock *get_error_block() {
                if (error_block == nullptr) {
                    T2BasicBlock *b = new_block();
                    T2Op *op = fn->new_op(b,
                                          T2OpKind::TailCallExt,
                                          T2Type::none());

                    fn->set_operands(op, {});
                    op->mfa_m = am_erlang;
                    op->mfa_f = am_error;
                    op->index = 0;

                    /* The shared function_clause exit: P1 lowers it to a
                     * side exit to the function's func_info. No sync map
                     * by design — predecessors reach it with differing
                     * frame states, exactly as T1 guard sites jump to one
                     * func_info label (see T2_OP_ERR_EXIT_SHARED). */
                    op->flags |= T2_OP_ERR_EXIT_SHARED;

                    error_block = b;
                }
                return error_block;
            }

            T2BasicBlock *block_for_label(UWord label) {
                auto it = label_block.find(label);

                if (it != label_block.end()) {
                    return it->second;
                }
                /* Not a label in this function's body: a jump to the function's
                 * own func_info label (function_clause). */
                return get_error_block();
            }

            /* Emits `cond ? fall through : fail`. A fail operand of TAG_p
             * ({f,0}) raises instead of branching; no edge is modelled. */
            void guard_branch(T2Value *cond, const DecodedArg &fail_arg) {
                if (cond == nullptr || failed) {
                    return;
                }

                if (fail_arg.type == TAG_p) {
                    return;
                }
                if (fail_arg.type != TAG_f) {
                    fail("unexpected fail-label tag");
                    return;
                }

                T2BasicBlock *fail_block = block_for_label(fail_arg.val);
                T2BasicBlock *cont = new_block();

                fn->emit_branch(cur, cond, cont, fail_block);
                add_edge(cur, cont);
                add_edge(cur, fail_block);

                /* Anonymous continuation: its single predecessor is known and
                 * nothing else can target it, so it is born sealed. */
                cur = cont;
                cur->sealed = true;
            }

            void end_block() {
                cur = nullptr;
            }

            /* Tolerant-mode degrade: seal the current block with an
             * Opaque terminator and skip to the next reachable label.
             * Admissible only while the block has no terminator yet —
             * an op that failed after attaching one (e.g. a select
             * rejected mid-case-list, with edges already added) stays
             * a hard error, as does a failure between blocks. Ops
             * translated before the failing one remain in the block;
             * that is sound because an Opaque block is only ever
             * consumed as an as-a-whole "unknown behavior" leaf. */
            bool opaque_cut() {
                if (cur == nullptr || cur->terminator != nullptr) {
                    return false;
                }

                T2Op *op = fn->new_op(cur, T2OpKind::Opaque, T2Type::none());

                fn->set_operands(op, {});
                op->beam_idx = cur_op != nullptr ? cur_op->beam_idx : 0;

                failed = false;
                error.clear();
                skipping = true;
                cur = nullptr;
                return true;
            }

            /* ---- op translation --------------------------------------------
             */

            T2Value *emit_result_op(T2OpKind kind,
                                    const std::vector<SrcVal> &srcs,
                                    T2Type ty) {
                std::vector<T2Value *> vals;

                vals.reserve(srcs.size());
                for (const SrcVal &s : srcs) {
                    if (s.v == nullptr) {
                        return nullptr;
                    }
                    vals.push_back(s.v);
                }

                T2Op *op = fn->new_op(cur, kind, ty);
                op->beam_idx = cur_op->beam_idx;
                fn->set_operands(op, vals);
                set_operand_regs(op, srcs);
                return op->result;
            }

            static T2Type bool_type() {
                return T2Type::of(BEAM_TYPE_ATOM);
            }

            void translate_type_test(T2OpKind kind, const DecodedOp &dop) {
                T2Value *v = emit_result_op(kind,
                                            {read_arg_r(dop.args[1])},
                                            bool_type());
                guard_branch(v, dop.args[0]);
            }

            void translate_compare(T2OpKind kind, const DecodedOp &dop) {
                T2Value *v = emit_result_op(
                        kind,
                        {read_arg_r(dop.args[1]), read_arg_r(dop.args[2])},
                        bool_type());
                guard_branch(v, dop.args[0]);
            }

            /* Maps an erlang BIF MFA to the corresponding arithmetic op kind,
             * or Bif when there is no dedicated kind. */
            T2OpKind bif_kind(Eterm m, Eterm f, uint32_t arity) {
                if (m != am_erlang) {
                    return T2OpKind::Bif;
                }

                if (arity == 2) {
                    if (f == am_Plus) {
                        return T2OpKind::Add;
                    } else if (f == am_Minus) {
                        return T2OpKind::Sub;
                    } else if (f == am_Times) {
                        return T2OpKind::Mul;
                    } else if (f == am_div) {
                        return T2OpKind::IDiv;
                    } else if (f == am_rem) {
                        return T2OpKind::Rem;
                    } else if (f == am_band) {
                        return T2OpKind::Band;
                    } else if (f == am_bor) {
                        return T2OpKind::Bor;
                    } else if (f == am_bxor) {
                        return T2OpKind::Bxor;
                    } else if (f == am_bsl) {
                        return T2OpKind::Bsl;
                    } else if (f == am_bsr) {
                        return T2OpKind::Bsr;
                    }
                } else if (arity == 1) {
                    if (f == am_Minus) {
                        return T2OpKind::Neg;
                    } else if (f == am_bnot) {
                        return T2OpKind::Bnot;
                    }
                }

                return T2OpKind::Bif;
            }

            void translate_gc_bif(const DecodedOp &dop, int num_sources) {
                /* (Fail, Live, BifImportIndex, Src..., Dst) */
                const DecodedArg &fail = dop.args[0];
                UWord live = dop.args[1].val;
                UWord import_index = dop.args[2].val;
                const DecodedArg &dst = dop.args[3 + num_sources];

                if (import_index >= (UWord)ret->import_count) {
                    fail_op(dop, "gc_bif import index out of range");
                    return;
                }

                const BeamFile_ImportEntry &mfa = ret->imports[import_index];
                T2OpKind kind = bif_kind(mfa.module, mfa.function, mfa.arity);

                std::vector<SrcVal> srcs;
                for (int i = 0; i < num_sources; i++) {
                    srcs.push_back(read_arg_r(dop.args[3 + i]));
                }

                T2Value *v = emit_result_op(kind, srcs, T2Type::any());
                if (v == nullptr) {
                    return;
                }

                T2Op *op = v->def;
                op->mfa_m = mfa.module;
                op->mfa_f = mfa.function;
                op->index = (uint32_t)num_sources;
                op->live = (uint32_t)live;

                /* gc_bifs may GC (and, with a {f,0} fail, trap): a sync
                 * point. The map is the boundary before the op; T1's own
                 * Live count bounds the live X prefix. */
                op->sync = snapshot_sync((uint32_t)live);

                if (fail.type == TAG_f) {
                    T2Value *ok = emit_result_op(T2OpKind::Succeeded,
                                                 {SrcVal{v, T2_REG_NONE}},
                                                 bool_type());
                    guard_branch(ok, fail);
                }

                write_dst_new(dst, v);
            }

            void fail_op(const DecodedOp &dop, const char *what) {
                fail(std::string(what) + " (genop " + std::to_string(dop.op) +
                     ")");
            }

            /* Resolves a local call target label to its MFA. */
            bool local_mfa(UWord label, Eterm *m, Eterm *f, uint32_t *a) {
                auto it = md.label_fn.find(label);

                if (it == md.label_fn.end()) {
                    return false;
                }

                const FunctionCode &target = md.functions[it->second];
                *m = target.module;
                *f = target.function;
                *a = target.arity;
                return true;
            }

            void translate_call(const DecodedOp &dop,
                                bool is_tail,
                                bool is_ext,
                                bool has_dealloc) {
                uint32_t arity = (uint32_t)dop.args[0].val;
                Eterm m, f;
                uint32_t target_arity;

                if (is_ext) {
                    UWord import_index = dop.args[1].val;

                    if (import_index >= (UWord)ret->import_count) {
                        fail_op(dop, "call import index out of range");
                        return;
                    }
                    m = ret->imports[import_index].module;
                    f = ret->imports[import_index].function;
                    target_arity = ret->imports[import_index].arity;
                } else {
                    if (dop.args[1].type != TAG_f ||
                        !local_mfa(dop.args[1].val, &m, &f, &target_arity)) {
                        fail("unresolvable local call target label " +
                             std::to_string(dop.args[1].val) + " (genop " +
                             std::to_string(dop.op) + ")");
                        return;
                    }
                }

                std::vector<T2Value *> args;
                std::vector<int32_t> arg_regs;
                for (uint32_t i = 0; i < arity; i++) {
                    T2Value *v = read_var(cur, i);

                    if (v == nullptr) {
                        fail_op(dop, "call argument register undefined");
                        return;
                    }
                    args.push_back(v);
                    arg_regs.push_back(t2_xreg(i));
                }

                /* A fused call_last deallocates before transferring; split
                 * that into an explicit Deallocate op so the tail-call sync
                 * map records the transfer state (no frame) and the frame
                 * layout stays derivable op by op.
                 *
                 * When the Deallocate operand does not match the tracked
                 * frame (or a call_only carries a live frame), this is the
                 * compiler's garbage-Deallocate shape for calls it proved
                 * cannot succeed (see T2_OP_GARBAGE_DEALLOC); translate
                 * without a frame op or sync map. */
                bool garbage_dealloc = false;

                if (has_dealloc) {
                    UWord dealloc = dop.args[2].val;

                    if (cur_frame != (int32_t)dealloc) {
                        garbage_dealloc = true;
                    } else {
                        T2Op *dop_op = fn->new_op(cur,
                                                  T2OpKind::Deallocate,
                                                  T2Type::none());
                        dop_op->beam_idx = dop.beam_idx;
                        fn->set_operands(dop_op, {});
                        dop_op->index = (uint32_t)dealloc;
                        cur_frame = T2_NO_FRAME;
                    }
                } else if (is_tail && cur_frame != T2_NO_FRAME) {
                    garbage_dealloc = true;
                }

                T2OpKind kind;
                if (is_tail) {
                    kind = is_ext ? T2OpKind::TailCallExt : T2OpKind::TailCall;
                } else {
                    kind = is_ext ? T2OpKind::CallExt : T2OpKind::Call;
                }

                T2Op *op = fn->new_op(cur, kind, T2Type::any());
                op->beam_idx = dop.beam_idx;
                fn->set_operands(op, args);
                op->mfa_m = m;
                op->mfa_f = f;
                op->index = arity;
                op->live = arity;
                if (!args.empty()) {
                    op->operand_regs =
                            fn->arena.alloc_array<int32_t>(args.size());
                    for (size_t i = 0; i < args.size(); i++) {
                        op->operand_regs[i] = arg_regs[i];
                    }
                }

                /* Call boundary sync map: args in X0..arity-1 (by
                 * construction identical to the operands — the validator
                 * asserts it), plus the live frame for a non-tail call /
                 * no frame for a tail transfer. Garbage-dealloc transfers
                 * carry no map (nothing may consume their frame state). */
                if (garbage_dealloc) {
                    op->flags |= T2_OP_GARBAGE_DEALLOC;
                } else {
                    op->sync = snapshot_sync(arity);
                }

                if (is_tail) {
                    end_block();
                } else {
                    /* The result lands in x0; other X registers die. */
                    op->dst_reg = t2_xreg(0);
                    write_var(cur, 0, op->result);
                }
            }

            /* call_fun Arity (fun implicitly in x[Arity]) and
             * call_fun2 Tag Arity Func (explicit fun operand): a fun
             * application. Always a body call at the generic-op level
             * (a tail fun call decodes as call_fun + deallocate +
             * return). Operands are the args in X0..Arity-1 plus the
             * fun as the LAST operand; index = the call arity; the
             * sync map's X prefix additionally covers the fun's X home
             * (T1 re-executing the call reads it from there). There is
             * no isel lowering — a CallFun must be erased (the P1
             * devirtualizer) or the function stays T1. */
            void translate_call_fun(const DecodedOp &dop) {
                bool is_cf2 = dop.op == genop_call_fun2_3;
                uint32_t arity = (uint32_t)dop.args[is_cf2 ? 1 : 0].val;
                std::vector<T2Value *> args;
                std::vector<int32_t> arg_regs;
                uint32_t x_live = arity;
                T2Value *funv;
                int32_t fun_reg;

                for (uint32_t i = 0; i < arity; i++) {
                    T2Value *v = read_var(cur, i);

                    if (v == nullptr) {
                        fail_op(dop, "call_fun argument register undefined");
                        return;
                    }
                    args.push_back(v);
                    arg_regs.push_back(t2_xreg(i));
                }

                if (is_cf2) {
                    const DecodedArg &fa = dop.args[2];

                    if (fa.type != TAG_x && fa.type != TAG_y) {
                        fail_op(dop, "call_fun2 fun operand is not a register");
                        return;
                    }
                    fun_reg = (int32_t)reg_var(fa);
                    funv = read_var(cur, reg_var(fa));
                    if (t2_reg_is_x(fun_reg) &&
                        t2_reg_index(fun_reg) >= x_live) {
                        x_live = t2_reg_index(fun_reg) + 1;
                    }
                } else {
                    fun_reg = t2_xreg(arity);
                    funv = read_var(cur, arity);
                    x_live = arity + 1;
                }
                if (funv == nullptr) {
                    fail_op(dop, "call_fun fun register undefined");
                    return;
                }
                args.push_back(funv);
                arg_regs.push_back(fun_reg);

                T2Op *op = fn->new_op(cur, T2OpKind::CallFun, T2Type::any());

                op->beam_idx = dop.beam_idx;
                fn->set_operands(op, args);
                op->index = arity;
                op->live = x_live;
                op->operand_regs = fn->arena.alloc_array<int32_t>(args.size());
                for (size_t i = 0; i < args.size(); i++) {
                    op->operand_regs[i] = arg_regs[i];
                }
                op->sync = snapshot_sync(x_live);

                /* The result lands in x0; other X registers die. */
                op->dst_reg = t2_xreg(0);
                write_var(cur, 0, op->result);
            }

            void translate_error_exit(const DecodedOp &dop, bool has_value) {
                std::vector<SrcVal> args;

                if (has_value) {
                    SrcVal s = read_arg_r(dop.args[0]);

                    if (s.v == nullptr) {
                        return;
                    }
                    args.push_back(s);
                }

                T2Op *op =
                        fn->new_op(cur, T2OpKind::TailCallExt, T2Type::none());
                op->beam_idx = dop.beam_idx;
                {
                    std::vector<T2Value *> vals;

                    for (const SrcVal &s : args) {
                        vals.push_back(s.v);
                    }
                    fn->set_operands(op, vals);
                }
                set_operand_regs(op, args);
                op->mfa_m = am_erlang;
                op->mfa_f = am_error;
                op->index = (uint32_t)args.size();

                /* A decoded error op (badmatch/case_end/if_end): P1 lowers
                 * it to a side exit to the op's own T1 PC, which re-reads
                 * the source operand from its decoded register and raises.
                 * No sync map: error labels are frame-polymorphic (one
                 * raise label is reached from guard fails at differing
                 * frame depths — see FRAME_CONFLICT), and the raise
                 * consumes no mapped state; the boundary state is
                 * established by the predecessors under full sync, exactly
                 * as in T1. */
                op->flags |= T2_OP_ERR_EXIT_OP;

                end_block();
            }

            void translate_select(const DecodedOp &dop, bool on_arity) {
                SrcVal src = read_arg_r(dop.args[0]);
                const DecodedArg &fail = dop.args[1];
                UWord count = dop.args[2].val;

                if (src.v == nullptr) {
                    return;
                }
                if (fail.type != TAG_f || (count % 2) != 0 ||
                    dop.args.size() != 3 + count) {
                    fail_op(dop, "malformed select");
                    return;
                }

                T2Op *sw = fn->new_op(cur, T2OpKind::Switch, T2Type::none());
                sw->beam_idx = dop.beam_idx;
                fn->set_operands(sw, {src.v});
                set_operand_regs(sw, {src});

                if (on_arity) {
                    /* select_tuple_arity: the cases carry make_small(N)
                     * arities, but dispatch compares the tuple *header*
                     * against make_arityval(N) (see T2_OP_SWITCH_ARITY). */
                    sw->flags |= T2_OP_SWITCH_ARITY;
                }

                uint32_t num_cases = (uint32_t)(count / 2);
                sw->num_cases = num_cases;
                sw->cases = fn->arena.alloc_array<T2SwitchCase>(num_cases);

                for (uint32_t i = 0; i < num_cases; i++) {
                    const DecodedArg &val = dop.args[3 + i * 2];
                    const DecodedArg &target = dop.args[3 + i * 2 + 1];
                    Eterm match;

                    if (on_arity || val.type == TAG_u) {
                        match = make_small((Uint)val.val);
                    } else if (val.type == TAG_i) {
                        match = make_small((Sint)(SWord)val.val);
                    } else if (val.type == TAG_a) {
                        match = (Eterm)val.val;
                    } else if (val.type == TAG_n) {
                        match = NIL;
                    } else if (val.type == TAG_q) {
                        Sint index = (Sint)(SWord)val.val;

                        if (index >= 0 && index < ret->literal_count) {
                            match = ret->literal_map[index];
                        } else if (index < 0) {
                            match = beamfile_get_literal(&md.view, index);
                        } else {
                            fail_op(dop, "select literal out of range");
                            return;
                        }
                    } else {
                        fail_op(dop, "unexpected select value tag");
                        return;
                    }

                    if (target.type != TAG_f) {
                        fail_op(dop, "unexpected select target tag");
                        return;
                    }

                    sw->cases[i].value = match;
                    sw->cases[i].target = block_for_label(target.val);
                    add_edge(cur, sw->cases[i].target);
                }

                sw->default_target = block_for_label(fail.val);
                add_edge(cur, sw->default_target);

                end_block();
            }

            void translate_op(const DecodedOp &dop);

            /* ---- frame-size pre-pass ---------------------------------- *
             * Computes the stack-frame size at every body label by a
             * fixpoint simulation of the decoded op stream. Needed
             * up front because a label may be reached only by edges that
             * appear later in the stream (e.g. a loop header entered via
             * a forward jump further down). Frame sizes only move
             * unknown -> known; disagreeing predecessors are a hard
             * error (the compiler never produces them). */
            bool is_stream_terminator(int op) {
                switch (op) {
                case genop_return_0:
                case genop_jump_1:
                case genop_select_val_3:
                case genop_select_tuple_arity_3:
                case genop_call_last_3:
                case genop_call_only_2:
                case genop_call_ext_last_3:
                case genop_call_ext_only_2:
                case genop_badmatch_1:
                case genop_case_end_1:
                case genop_if_end_0:
                case genop_badrecord_1:
                    return true;
                default:
                    return false;
                }
            }

            void merge_label_frame(UWord label, int32_t f, bool *changed) {
                if (f == FRAME_UNKNOWN) {
                    return;
                }
                if (label_block.find(label) == label_block.end()) {
                    /* Not a body label of this function (a call target or
                     * the func_info label): no frame to track. */
                    return;
                }

                auto it = label_frame.find(label);
                if (it == label_frame.end() || it->second == FRAME_UNKNOWN) {
                    label_frame[label] = f;
                    *changed = true;
                } else if (it->second != f && it->second != FRAME_CONFLICT) {
                    /* Frame-polymorphic label (shared raise block reached
                     * from differing frame depths). Legal; nothing
                     * frame-dependent may appear under it. */
                    label_frame[label] = FRAME_CONFLICT;
                    *changed = true;
                }
            }

            bool compute_label_frames() {
                size_t rounds = fc.ops.size() + 2;
                bool changed = true;

                while (changed && rounds-- > 0 && !failed) {
                    int32_t frame = T2_NO_FRAME;
                    bool live = true;

                    changed = false;

                    for (const DecodedOp &dop : fc.ops) {
                        if (dop.op == genop_label_1) {
                            UWord label = dop.args[0].val;

                            if (live) {
                                /* Fall-through edge into the label. */
                                merge_label_frame(label, frame, &changed);
                            }
                            {
                                auto it = label_frame.find(label);
                                frame = it != label_frame.end() ? it->second
                                                                : FRAME_UNKNOWN;
                            }
                            live = true;
                            continue;
                        }
                        if (!live) {
                            continue;
                        }

                        /* Branch edges: any local-label argument is a
                         * potential target (guard fails, jump/select
                         * targets) — except call targets, which are entry
                         * labels, not branch edges (a self-recursive call
                         * names this function's own entry label). Foreign
                         * labels are filtered by merge_label_frame. */
                        switch (dop.op) {
                        case genop_call_2:
                        case genop_call_only_2:
                        case genop_call_last_3:
                        case genop_call_ext_2:
                        case genop_call_ext_only_2:
                        case genop_call_ext_last_3:
                            break;
                        default:
                            for (const DecodedArg &a : dop.args) {
                                if (a.type == TAG_f) {
                                    merge_label_frame(a.val, frame, &changed);
                                }
                            }
                            break;
                        }

                        switch (dop.op) {
                        case genop_allocate_2:
                        case genop_allocate_heap_3:
                            frame = (int32_t)dop.args[0].val;
                            break;
                        case genop_deallocate_1:
                            frame = T2_NO_FRAME;
                            break;
                        case genop_trim_2:
                            if (frame != FRAME_UNKNOWN) {
                                frame -= (int32_t)dop.args[0].val;
                            }
                            break;
                        default:
                            break;
                        }

                        if (is_stream_terminator(dop.op)) {
                            live = false;
                        }
                    }

                    if (failed) {
                        return false;
                    }
                }

                return !failed;
            }
        };

        void FunctionBuilder::translate_op(const DecodedOp &dop) {
            switch (dop.op) {
            case genop_label_1: {
                T2BasicBlock *b = label_block.at(dop.args[0].val);

                if (cur != nullptr) {
                    /* Fall through into the labelled block. */
                    T2Op *op = fn->new_op(cur, T2OpKind::Jump, T2Type::none());
                    op->beam_idx = dop.beam_idx;
                    op->succ_then = b;
                    add_edge(cur, b);
                }
                cur = b;

                /* Frame size at block entry, from the pre-pass. */
                {
                    auto it = label_frame.find(dop.args[0].val);
                    cur_frame = it != label_frame.end() ? it->second
                                                        : FRAME_UNKNOWN;
                }
                break;
            }

            case genop_line_1:
            case genop_executable_line_2:
                break;

            case genop_move_2:
                if (dop.args[0].type == TAG_x || dop.args[0].type == TAG_y) {
                    /* Register-to-register move: a Copy op, so the value
                     * is materialized in its new canonical home (mirrors
                     * beam_ssa_pre_codegen's `copy`). */
                    SrcVal s = read_arg_r(dop.args[0]);

                    if (s.v == nullptr) {
                        return;
                    }
                    write_dst_new(
                            dop.args[1],
                            emit_result_op(T2OpKind::Copy, {s}, s.v->type));
                } else {
                    /* Constant move: the materialization op itself is the
                     * write; its home is the destination register. */
                    write_dst_new(dop.args[1], read_arg(dop.args[0]));
                }
                break;

            case genop_swap_2: {
                SrcVal a = read_arg_r(dop.args[0]);
                SrcVal b = read_arg_r(dop.args[1]);

                if (a.v == nullptr || b.v == nullptr) {
                    return;
                }

                /* Two Copies whose reads both precede the writes; the
                 * PAIR_HEAD flag makes the backend emit them fused
                 * (emit_swap) and the validator walk them atomically. */
                T2Value *cb = emit_result_op(T2OpKind::Copy, {b}, b.v->type);
                if (cb != nullptr) {
                    cb->def->flags |= T2_OP_PAIR_HEAD;
                }
                T2Value *ca = emit_result_op(T2OpKind::Copy, {a}, a.v->type);

                write_dst_new(dop.args[0], cb);
                write_dst_new(dop.args[1], ca);
                break;
            }

            case genop_init_yregs_1: {
                UWord count = dop.args[0].val;

                for (UWord i = 0; i < count; i++) {
                    /* A killed Y slot is written as const_nil — T1's own
                     * kill representation (init_yregs stores NIL), so a
                     * later sync map reads the kill, never a stale value. */
                    T2Value *v = fn->emit_const_nil(cur);

                    v->def->beam_idx = dop.beam_idx;
                    write_dst_new(dop.args[1 + i], v);
                }
                break;
            }

            case genop_allocate_2:
            case genop_allocate_heap_3: {
                /* First-class frame op. allocate may GC (stack need) with
                 * Live X registers, *before* the frame is pushed, so the
                 * sync boundary is the pre-frame state. allocate_heap
                 * additionally reserves heap words in the same test
                 * (fused, as T1's emit_allocate_heap does). */
                bool has_heap = dop.op == genop_allocate_heap_3;
                UWord slots = dop.args[0].val;
                UWord heap = has_heap ? dop.args[1].val : 0;
                UWord live = dop.args[has_heap ? 2 : 1].val;

                if (cur_frame != T2_NO_FRAME) {
                    fail_op(dop, "allocate with a live frame");
                    return;
                }

                T2Op *op = fn->new_op(cur, T2OpKind::Allocate, T2Type::none());
                op->beam_idx = dop.beam_idx;
                fn->set_operands(op, {});
                op->index = (uint32_t)slots;
                op->imm_int = (Sint64)heap;
                op->live = (uint32_t)live;
                op->sync = snapshot_sync((uint32_t)live);

                cur_frame = (int32_t)slots;
                break;
            }

            case genop_deallocate_1: {
                UWord slots = dop.args[0].val;

                if (cur_frame != (int32_t)slots) {
                    fail_op(dop, "deallocate != frame size");
                    return;
                }

                T2Op *op =
                        fn->new_op(cur, T2OpKind::Deallocate, T2Type::none());
                op->beam_idx = dop.beam_idx;
                fn->set_operands(op, {});
                op->index = (uint32_t)slots;

                cur_frame = T2_NO_FRAME;
                break;
            }

            case genop_test_heap_2: {
                T2Op *op = fn->new_op(cur, T2OpKind::GcTest, T2Type::none());

                op->beam_idx = dop.beam_idx;
                fn->set_operands(op, {});
                op->index = (uint32_t)dop.args[0].val;
                op->live = (uint32_t)dop.args[1].val;
                op->sync = snapshot_sync(op->live);
                break;
            }

            case genop_trim_2: {
                UWord n = dop.args[0].val;
                UWord remaining = dop.args[1].val;
                std::vector<T2Value *> vals;

                if (cur_frame != (int32_t)(n + remaining)) {
                    fail_op(dop, "trim != frame size");
                    return;
                }

                /* First-class frame op: T1 moves E, renumbering the
                 * surviving slots without moving data. */
                T2Op *op = fn->new_op(cur, T2OpKind::Trim, T2Type::none());
                op->beam_idx = dop.beam_idx;
                fn->set_operands(op, {});
                op->index = (uint32_t)n;
                op->imm_int = (Sint64)remaining;

                /* Renumber the Braun Y variables to post-trim numbering;
                 * every later sync map is therefore post-trim, matching
                 * what the loaded code's continuation expects. */
                for (UWord i = 0; i < remaining; i++) {
                    T2Value *v = read_var(cur, Y_VAR_BASE + (uint32_t)(i + n));

                    if (v == nullptr) {
                        fail_op(dop, "trim of undefined Y register");
                        return;
                    }
                    vals.push_back(v);
                }
                for (UWord i = 0; i < remaining; i++) {
                    write_var(cur, Y_VAR_BASE + (uint32_t)i, vals[i]);
                }

                cur_frame = (int32_t)remaining;
                break;
            }

            case genop_call_2:
                translate_call(dop, false, false, false);
                break;
            case genop_call_only_2:
                translate_call(dop, true, false, false);
                break;
            case genop_call_last_3:
                translate_call(dop, true, false, true);
                break;
            case genop_call_ext_2:
                translate_call(dop, false, true, false);
                break;
            case genop_call_ext_only_2:
                translate_call(dop, true, true, false);
                break;
            case genop_call_ext_last_3:
                translate_call(dop, true, true, true);
                break;

            case genop_call_fun_1:
            case genop_call_fun2_3:
                translate_call_fun(dop);
                break;

            case genop_return_0: {
                T2Value *v = read_var(cur, 0);

                if (v == nullptr) {
                    fail_op(dop, "return with undefined x0");
                    return;
                }

                T2Op *op = fn->new_op(cur, T2OpKind::Return, T2Type::none());
                op->beam_idx = dop.beam_idx;
                fn->set_operands(op, {v});
                op->operand_regs = fn->arena.alloc_array<int32_t>(1);
                op->operand_regs[0] = t2_xreg(0);

                /* Return boundary: X0 = the return value, frame already
                 * deallocated (the validator asserts both). */
                op->sync = snapshot_sync(1);

                end_block();
                break;
            }

            case genop_jump_1: {
                if (dop.args[0].type != TAG_f) {
                    fail_op(dop, "unexpected jump target tag");
                    return;
                }

                T2BasicBlock *b = block_for_label(dop.args[0].val);
                T2Op *op = fn->new_op(cur, T2OpKind::Jump, T2Type::none());

                op->beam_idx = dop.beam_idx;
                op->succ_then = b;
                add_edge(cur, b);
                end_block();
                break;
            }

            case genop_select_val_3:
                translate_select(dop, false);
                break;
            case genop_select_tuple_arity_3:
                translate_select(dop, true);
                break;

            case genop_is_integer_2:
                translate_type_test(T2OpKind::IsInteger, dop);
                break;
            case genop_is_atom_2:
                translate_type_test(T2OpKind::IsAtom, dop);
                break;
            case genop_is_nil_2:
                translate_type_test(T2OpKind::IsNil, dop);
                break;
            case genop_is_list_2:
                translate_type_test(T2OpKind::IsList, dop);
                break;
            case genop_is_nonempty_list_2:
                translate_type_test(T2OpKind::IsNonemptyList, dop);
                break;
            case genop_is_tuple_2:
                translate_type_test(T2OpKind::IsTuple, dop);
                break;
            case genop_is_map_2:
                translate_type_test(T2OpKind::IsMap, dop);
                break;

            case genop_test_arity_3: {
                T2Value *v = emit_result_op(T2OpKind::TestArity,
                                            {read_arg_r(dop.args[1])},
                                            bool_type());

                if (v != nullptr) {
                    v->def->index = (uint32_t)dop.args[2].val;
                }
                guard_branch(v, dop.args[0]);
                break;
            }

            case genop_is_tagged_tuple_4: {
                T2Value *v = emit_result_op(T2OpKind::IsTaggedTuple,
                                            {read_arg_r(dop.args[1])},
                                            bool_type());

                if (v != nullptr) {
                    v->def->index = (uint32_t)dop.args[2].val;
                    v->def->imm_term = (Eterm)dop.args[3].val;
                }
                guard_branch(v, dop.args[0]);
                break;
            }

            case genop_is_function2_3: {
                /* is_function2 Fail Src Arity — the eligibility scan
                 * admitted only the register-source, immediate-arity
                 * shape. index = the tested arity. Decoded so the P1c
                 * wrapper classifier sees guard-carrying fold wrappers
                 * (lists:foldl/3); like CallFun there is deliberately
                 * NO isel lowering — a function whose blob would still
                 * contain an IsFunction degrades to T1 at isel. */
                T2Value *v = emit_result_op(T2OpKind::IsFunction,
                                            {read_arg_r(dop.args[1])},
                                            bool_type());

                if (v != nullptr) {
                    v->def->index = (uint32_t)dop.args[2].val;
                }
                guard_branch(v, dop.args[0]);
                break;
            }

            case genop_is_lt_3:
                translate_compare(T2OpKind::CmpLt, dop);
                break;
            case genop_is_ge_3:
                translate_compare(T2OpKind::CmpGe, dop);
                break;
            case genop_is_eq_3:
                translate_compare(T2OpKind::CmpEq, dop);
                break;
            case genop_is_ne_3:
                translate_compare(T2OpKind::CmpNe, dop);
                break;
            case genop_is_eq_exact_3:
                translate_compare(T2OpKind::CmpEqExact, dop);
                break;
            case genop_is_ne_exact_3:
                translate_compare(T2OpKind::CmpNeExact, dop);
                break;

            case genop_get_list_3: {
                SrcVal src = read_arg_r(dop.args[0]);

                if (src.v == nullptr) {
                    return;
                }

                /* A decoded pair: both reads precede both writes (a
                 * destination may alias the source), so the backend must
                 * emit it fused (emit_get_list, as T1 does). */
                T2Value *hd =
                        emit_result_op(T2OpKind::GetHd, {src}, T2Type::any());
                if (hd != nullptr) {
                    hd->def->flags |= T2_OP_PAIR_HEAD;
                }
                T2Value *tl =
                        emit_result_op(T2OpKind::GetTl, {src}, T2Type::any());

                write_dst_new(dop.args[1], hd);
                write_dst_new(dop.args[2], tl);
                break;
            }
            case genop_get_hd_2:
                write_dst_new(dop.args[1],
                              emit_result_op(T2OpKind::GetHd,
                                             {read_arg_r(dop.args[0])},
                                             T2Type::any()));
                break;
            case genop_get_tl_2:
                write_dst_new(dop.args[1],
                              emit_result_op(T2OpKind::GetTl,
                                             {read_arg_r(dop.args[0])},
                                             T2Type::any()));
                break;

            case genop_get_tuple_element_3: {
                T2Value *v = emit_result_op(T2OpKind::GetTupleElement,
                                            {read_arg_r(dop.args[0])},
                                            T2Type::any());

                if (v != nullptr) {
                    v->def->index = (uint32_t)dop.args[1].val;
                }
                write_dst_new(dop.args[2], v);
                break;
            }

            case genop_put_list_3:
                write_dst_new(dop.args[2],
                              emit_result_op(T2OpKind::MakeList,
                                             {read_arg_r(dop.args[0]),
                                              read_arg_r(dop.args[1])},
                                             T2Type::of(BEAM_TYPE_CONS)));
                break;

            case genop_make_fun3_3: {
                /* (LambdaIndex, Dst, NumFree, Env...) after list
                 * expansion. Heap need is covered by the preceding
                 * test_heap (the compiler's alloc list includes fun
                 * words) — not a sync point. index = the lambda
                 * ordinal (isel resolves the ErlFunEntry through the
                 * retained table), live = num_free, imm_int = the
                 * implementation function's index (the intrinsics'
                 * fun-body resolution; -1 when the label is not a
                 * function entry). */
                UWord lambda_idx = dop.args[0].val;
                UWord num_free = dop.args[2].val;

                if (ret->lambdas == NULL ||
                    lambda_idx >= (UWord)ret->lambda_count) {
                    fail_op(dop, "make_fun3 lambda index out of range");
                    return;
                }

                const ErtsT2Lambda *lam = &ret->lambdas[lambda_idx];

                if ((UWord)lam->num_free != num_free ||
                    dop.args.size() != 3 + num_free) {
                    fail_op(dop, "make_fun3 env/lambda mismatch");
                    return;
                }

                std::vector<SrcVal> env;
                for (UWord i = 0; i < num_free; i++) {
                    env.push_back(read_arg_r(dop.args[3 + i]));
                }

                T2Value *v = emit_result_op(T2OpKind::MakeFun,
                                            env,
                                            T2Type::of(BEAM_TYPE_FUN));
                if (v == nullptr) {
                    return;
                }
                v->def->index = (uint32_t)lambda_idx;
                v->def->live = (uint32_t)num_free;
                {
                    auto it = md.label_fn.find((UWord)lam->label);

                    v->def->imm_int =
                            it != md.label_fn.end() &&
                                            md.functions[it->second]
                                                            .entry_label ==
                                                    (UWord)lam->label
                                    ? (Sint64)it->second
                                    : (Sint64)-1;
                }
                write_dst_new(dop.args[1], v);
                break;
            }

            case genop_put_tuple2_2: {
                UWord count = dop.args[1].val;
                std::vector<SrcVal> elems;

                for (UWord i = 0; i < count; i++) {
                    elems.push_back(read_arg_r(dop.args[2 + i]));
                }
                write_dst_new(dop.args[0],
                              emit_result_op(T2OpKind::MakeTuple,
                                             elems,
                                             T2Type::of(BEAM_TYPE_TUPLE)));
                break;
            }

            case genop_bif2_5: {
                /* Value-producing total comparison (P2 commit 8):
                 * `bif2 {f,0} erlang:CMP/2 A B D`. The eligibility
                 * scan admitted only the never-failing comparison
                 * subset; anything else is a decode error. Pure, no
                 * sync point (T1's bif_is_* lowerings are fragment
                 * compares, no GC, no trap). */
                UWord import_index = dop.args[1].val;

                /* A zero fail label decodes as TAG_p. */
                if (dop.args[0].type != TAG_p ||
                    import_index >= (UWord)ret->import_count) {
                    fail_op(dop, "unsupported bif2 shape");
                    return;
                }

                const BeamFile_ImportEntry &mfa = ret->imports[import_index];
                T2OpKind kind;

                if (mfa.module != am_erlang || mfa.arity != 2) {
                    fail_op(dop, "unsupported bif2 target");
                    return;
                }
                if (mfa.function == am_Ge) {
                    kind = T2OpKind::CmpGe;
                } else if (mfa.function == am_Lt) {
                    kind = T2OpKind::CmpLt;
                } else if (mfa.function == am_Le) {
                    kind = T2OpKind::CmpLe;
                } else if (mfa.function == am_Gt) {
                    kind = T2OpKind::CmpGt;
                } else if (mfa.function == am_Eq) {
                    kind = T2OpKind::CmpEqExact;
                } else if (mfa.function == am_Neq) {
                    kind = T2OpKind::CmpNeExact;
                } else {
                    fail_op(dop, "unsupported bif2 comparison");
                    return;
                }

                write_dst_new(dop.args[4],
                              emit_result_op(kind,
                                             {read_arg_r(dop.args[2]),
                                              read_arg_r(dop.args[3])},
                                             bool_type()));
                break;
            }

            case genop_gc_bif1_5:
                translate_gc_bif(dop, 1);
                break;
            case genop_gc_bif2_6:
                translate_gc_bif(dop, 2);
                break;
            case genop_gc_bif3_7:
                translate_gc_bif(dop, 3);
                break;

            case genop_badmatch_1:
            case genop_case_end_1:
            case genop_badrecord_1:
                translate_error_exit(dop, true);
                break;
            case genop_if_end_0:
                translate_error_exit(dop, false);
                break;

            case genop_get_map_elements_3: {
                /* Fail Src N K1 D1 ... (the {list,...} expanded) — the
                 * eligibility scan admitted only the register-source,
                 * real-fail-label shape (get_map_elements_op_supported);
                 * a mismatch here is scan/builder drift. Decomposed into
                 * one read-only GetMapElement per key, each with its own
                 * Succeeded/Branch fail edge to the decoded label —
                 * semantically T1's own progressive-write behavior (on
                 * the fail path earlier destinations may already be
                 * written; the compiler treats them as dead there).
                 *
                 * Pair order: a destination aliasing the map's home
                 * register is looked up LAST, so every earlier lookup
                 * still reads the map from its canonical home (multi-
                 * pair keys are constants by the loader's single-
                 * variable-key rule, so only the source can be
                 * clobbered). Within one op reads precede the write. */
                const DecodedArg &fail = dop.args[0];
                UWord count = dop.args[2].val;
                SrcVal src = read_arg_r(dop.args[1]);

                if (src.v == nullptr) {
                    return;
                }
                if (fail.type != TAG_f || count < 2 || (count % 2) != 0 ||
                    dop.args.size() != 3 + count ||
                    (dop.args[1].type != TAG_x && dop.args[1].type != TAG_y)) {
                    fail_op(dop,
                            "get_map_elements outside the decoded shape "
                            "(eligibility/builder drift)");
                    return;
                }

                UWord npairs = count / 2;
                UWord aliased = npairs; /* sentinel: no aliasing pair */

                for (UWord i = 0; i < npairs; i++) {
                    const DecodedArg &d = dop.args[3 + 2 * i + 1];

                    if ((d.type == TAG_x || d.type == TAG_y) &&
                        (int32_t)reg_var(d) == src.reg) {
                        aliased = i;
                    }
                }

                for (UWord n = 0; n < npairs; n++) {
                    /* All non-aliasing pairs in stream order, then the
                     * aliasing one. */
                    UWord i = n;

                    if (aliased < npairs) {
                        if (n >= aliased) {
                            i = n + 1;
                        }
                        if (n == npairs - 1) {
                            i = aliased;
                        }
                    }

                    SrcVal key = read_arg_r(dop.args[3 + 2 * i]);
                    T2Value *v = emit_result_op(T2OpKind::GetMapElement,
                                                {src, key},
                                                T2Type::any());

                    if (v == nullptr) {
                        return;
                    }

                    T2Value *ok = emit_result_op(T2OpKind::Succeeded,
                                                 {SrcVal{v, T2_REG_NONE}},
                                                 bool_type());
                    guard_branch(ok, fail);
                    write_dst_new(dop.args[3 + 2 * i + 1], v);
                    if (failed) {
                        return;
                    }
                }
                break;
            }

                /* --- the byte-aligned binary scan subset (P2 commit 7) --- */

            case genop_bs_start_match3_4: {
                /* Fail Bin Live Dst. May GC (fresh context): sync
                 * point with the decoded Live. The destination is
                 * written on the success edge only (T1 branches to
                 * Fail before the write), which the Succeeded/Branch
                 * shape below models exactly like a gc_bif. */
                const DecodedArg &fail = dop.args[0];
                SrcVal src = read_arg_r(dop.args[1]);
                UWord live = dop.args[2].val;

                if (src.v == nullptr) {
                    return;
                }
                if (fail.type != TAG_f && fail.type != TAG_p) {
                    fail_op(dop, "unexpected bs_start_match3 fail tag");
                    return;
                }

                T2Value *v = emit_result_op(T2OpKind::StartMatch,
                                            {src},
                                            T2Type::of(BEAM_TYPE_BITSTRING));
                if (v == nullptr) {
                    return;
                }
                v->def->live = (uint32_t)live;
                v->def->sync = snapshot_sync((uint32_t)live);

                if (fail.type == TAG_f) {
                    T2Value *ok = emit_result_op(T2OpKind::Succeeded,
                                                 {SrcVal{v, T2_REG_NONE}},
                                                 bool_type());
                    guard_branch(ok, fail);
                }
                write_dst_new(dop.args[3], v);
                break;
            }

            case genop_bs_match_3: {
                /* Fail Ctx N Cmds... — the eligibility scan admitted
                 * only the byte-aligned subset; re-parse through the
                 * same shared checker (single source of truth), so a
                 * mismatch here is scan/builder drift. */
                const DecodedArg &fail = dop.args[0];
                UWord types[64], vals[64];
                ErtsT2BsCmd cmds[ERTS_T2_BS_MAX_CMDS];
                int dsts = 0;

                if (dop.args.size() > 64) {
                    fail_op(dop, "oversized bs_match");
                    return;
                }
                for (size_t i = 0; i < dop.args.size(); i++) {
                    types[i] = dop.args[i].type;
                    vals[i] = dop.args[i].val;
                }

                int ncmds = erts_t2_bs_match_check(types,
                                                   vals,
                                                   3,
                                                   (int)dop.args.size(),
                                                   builder_bs_lit,
                                                   (void *)ret,
                                                   cmds,
                                                   &dsts);
                if (ncmds < 0) {
                    fail_op(dop,
                            "bs_match outside the byte-aligned subset "
                            "(eligibility/builder drift)");
                    return;
                }
                if (fail.type != TAG_f) {
                    fail_op(dop, "unexpected bs_match fail tag");
                    return;
                }

                SrcVal ctx = read_arg_r(dop.args[1]);

                if (ctx.v == nullptr) {
                    return;
                }

                Sint64 heap = 0;
                uint32_t live = 0;
                int dst_arg = -1;
                T2Type ty = T2Type::any();

                for (int i = 0; i < ncmds; i++) {
                    switch (cmds[i].kind) {
                    case ERTS_T2_BS_READ_INT8:
                        live = std::max(live, (uint32_t)cmds[i].live);
                        dst_arg = cmds[i].dst_arg;
                        ty = T2Type::integer(0, 255);
                        break;
                    case ERTS_T2_BS_GET_TAIL:
                        heap += BUILD_SUB_BITSTRING_HEAP_NEED;
                        live = std::max(live, (uint32_t)cmds[i].live);
                        dst_arg = cmds[i].dst_arg;
                        ty = T2Type::of(BEAM_TYPE_BITSTRING);
                        break;
                    default:
                        break;
                    }
                }

                T2Value *v = emit_result_op(T2OpKind::BsMatch, {ctx}, ty);

                if (v == nullptr) {
                    return;
                }

                T2Op *op = v->def;
                ErtsT2BsCmd *ac =
                        fn->arena.alloc_array<ErtsT2BsCmd>((size_t)ncmds);

                sys_memcpy(ac, cmds, sizeof(ErtsT2BsCmd) * (size_t)ncmds);
                op->bs_cmds = ac;
                op->num_bs_cmds = (uint16_t)ncmds;
                op->live = live;
                op->imm_int = heap;
                if (heap != 0) {
                    /* The reused T1 emitter inserts an internal
                     * TEST_HEAP with this live count; X/Y state at
                     * that point equals the op boundary (nothing is
                     * written before it). */
                    op->sync = snapshot_sync(live);
                }

                T2Value *ok = emit_result_op(T2OpKind::Succeeded,
                                             {SrcVal{v, T2_REG_NONE}},
                                             bool_type());
                guard_branch(ok, fail);

                if (dst_arg >= 0) {
                    write_dst_new(dop.args[dst_arg], v);
                }
                break;
            }

            case genop_bs_get_tail_3: {
                /* Ctx Dst Live. Allocates the tail sub-bitstring:
                 * GC with the decoded Live; no fail edge. */
                SrcVal ctx = read_arg_r(dop.args[0]);
                UWord live = dop.args[2].val;

                if (ctx.v == nullptr) {
                    return;
                }

                T2Value *v = emit_result_op(T2OpKind::BsGetTail,
                                            {ctx},
                                            T2Type::of(BEAM_TYPE_BITSTRING));
                if (v == nullptr) {
                    return;
                }
                v->def->live = (uint32_t)live;
                v->def->sync = snapshot_sync((uint32_t)live);
                write_dst_new(dop.args[1], v);
                break;
            }

            case genop_bs_test_tail2_3: {
                /* Fail Ctx Bits — a pure size guard. */
                T2Value *v = emit_result_op(T2OpKind::BsTestTail,
                                            {read_arg_r(dop.args[1])},
                                            bool_type());

                if (v != nullptr) {
                    v->def->index = (uint32_t)dop.args[2].val;
                }
                guard_branch(v, dop.args[0]);
                break;
            }

            default:
                /* Drift between the eligibility table and this builder. */
                fail_op(dop, "unsupported generic op in eligible function");
                break;
            }
        }

        std::unique_ptr<T2Function> FunctionBuilder::build(std::string *err) {
            fn = std::make_unique<T2Function>();
            fn->module = fc.module;
            fn->function = fc.function;
            fn->arity = fc.arity;
            fn->fn_index = fn_index;

            /* Pass 1: a block per label, in stream order; the entry label is
             * the first label and becomes blocks[0]. */
            for (const DecodedOp &dop : fc.ops) {
                if (dop.op == genop_label_1) {
                    T2BasicBlock *b = new_block();
                    label_block[dop.args[0].val] = b;
                }
            }

            if (fn->blocks.empty()) {
                *err = "function has no labels";
                return nullptr;
            }

            /* Frame-size pre-pass (needs the body-label set from pass 1). */
            if (!compute_label_frames()) {
                *err = error;
                return nullptr;
            }

            /* Pass 2: translate. */
            for (const DecodedOp &dop : fc.ops) {
                cur_op = &dop;

                if (skipping) {
                    /* Tolerant mode, after an opaque cut: skip the
                     * unbuildable region. Translation resumes at the
                     * next label an already-translated edge targets;
                     * a label only the skipped region references
                     * stays an inert island (post-pass below). */
                    if (dop.op != genop_label_1 ||
                        preds[label_block.at(dop.args[0].val)->id].empty()) {
                        continue;
                    }
                    skipping = false;
                }

                if (cur == nullptr && dop.op != genop_label_1) {
                    /* Ignorable ops may trail a terminator without a label. */
                    if (dop.op == genop_line_1 ||
                        dop.op == genop_executable_line_2) {
                        continue;
                    }
                    fail_op(dop, "op after terminator without label");
                    break;
                }

                translate_op(dop);

                if (dop.op == genop_label_1 && cur == fn->blocks[0] &&
                    defs[0].find(0) == defs[0].end()) {
                    /* First entry into the entry block: bind parameters
                     * (canonical homes X0..arity-1) and take the entry
                     * sync map (no frame). */
                    for (uint32_t i = 0; i < fc.arity; i++) {
                        T2Value *p = fn->emit_param(cur, i, T2Type::any());
                        p->def->dst_reg = t2_xreg(i);
                        write_var(cur, i, p);
                    }
                    fn->entry_sync = snapshot_sync(fc.arity);
                }

                if (failed && tolerant && opaque_cut()) {
                    continue;
                }
                if (failed) {
                    break;
                }
            }

            if (!failed && cur != nullptr) {
                fail("function ended without a terminator");
            }

            if (!failed) {
                /* All edges exist now; seal every block, completing the
                 * incomplete phis. */
                for (T2BasicBlock *b : fn->blocks) {
                    if (!b->sealed) {
                        seal_block(b);
                    }
                }
            }

            if (!failed && tolerant) {
                /* Labels only skipped regions referenced were never
                 * translated: seal them as inert Opaque islands (no
                 * ops, no phis, no predecessors — nothing consults
                 * them). A terminator-less block that acquired
                 * predecessors would mean a translated edge targets a
                 * skipped label (a backward reference into the cut
                 * region); reject rather than fabricate its body. */
                for (T2BasicBlock *b : fn->blocks) {
                    if (b->terminator != nullptr) {
                        continue;
                    }
                    if (!preds[b->id].empty() || b->ops_head != nullptr ||
                        b->phis_head != nullptr || b == fn->blocks[0]) {
                        fail("tolerant build: translated edge into a "
                             "skipped label");
                        break;
                    }

                    T2Op *op = fn->new_op(b, T2OpKind::Opaque, T2Type::none());

                    fn->set_operands(op, {});
                }
            }

            if (failed) {
                *err = error;
                return nullptr;
            }

            /* All P1 metadata (sync maps, frame ops, canonical homes) is
             * attached; arm the validator's coherence checks. */
            fn->sync_complete = true;

            fn->finalize();
            return std::move(fn);
        }

    } /* anonymous namespace */

    /* ------------------------------------------------------------------ *
     * Single-function builder entry (t2_build_ssa debug BIF; see         *
     * t2_hir.hpp). Defined here — not in the anonymous namespace — so the *
     * TU-local ModuleDecode / FunctionBuilder above remain reachable.     *
     * ------------------------------------------------------------------ */

    static T2BuildStatus build_named(
            const ErtsT2RetainedCode *ret,
            Eterm function,
            unsigned arity,
            bool tolerant,
            const std::function<void(T2Function &)> &emit,
            std::string *err) {
        ModuleDecode md;
        std::string local_err;

        if (!decode_module(ret, md, &local_err)) {
            if (err != nullptr) {
                *err = "decode: " + local_err;
            }
            md.cleanup();
            return T2BuildStatus::Failed;
        }

        T2BuildStatus status = T2BuildStatus::NotFound;

        for (size_t i = 0; i < md.functions.size(); i++) {
            const FunctionCode &fc = md.functions[i];

            if (fc.function != function || fc.arity != arity) {
                continue;
            }

            /* Found by name/arity; the standard SSA build only handles
             * functions the eligibility scan accepted, while a tolerant
             * build degrades the unsupported regions to Opaque leaves
             * instead. */
            if (!tolerant &&
                (i >= (size_t)ret->function_count ||
                 !(ret->eligible_bitmap[i / 32] & (((Uint32)1) << (i % 32))))) {
                status = T2BuildStatus::NotEligible;
                break;
            }

            FunctionBuilder builder(md, fc, (uint32_t)i, tolerant);
            std::unique_ptr<T2Function> fn = builder.build(&local_err);

            if (fn == nullptr) {
                if (err != nullptr) {
                    *err = "build: " + local_err;
                }
                status = T2BuildStatus::Failed;
                break;
            }

            if (!t2_validate(*fn, &local_err)) {
                if (err != nullptr) {
                    *err = "validate: " + local_err;
                }
                status = T2BuildStatus::Failed;
                break;
            }

            /* md (and thus any dynamic-literal terms the IR points at) is
             * still alive; serialization must happen inside `emit`. */
            emit(*fn);
            status = T2BuildStatus::Ok;
            break;
        }

        md.cleanup();
        return status;
    }

    T2BuildStatus t2_build_for_debug(
            const ErtsT2RetainedCode *ret,
            Eterm function,
            unsigned arity,
            const std::function<void(T2Function &)> &emit,
            std::string *err) {
        return build_named(ret, function, arity, false, emit, err);
    }

    T2BuildStatus t2_build_for_p1(const ErtsT2RetainedCode *ret,
                                  Eterm function,
                                  unsigned arity,
                                  const std::function<void(T2Function &)> &emit,
                                  std::string *err) {
        return build_named(ret, function, arity, true, emit, err);
    }

    bool t2_build_selected(const ErtsT2RetainedCode *ret,
                           const uint32_t *fn_indices,
                           size_t n,
                           const std::function<void(T2Function &)> &emit,
                           std::string *err) {
        ModuleDecode md;
        std::string local_err;

        if (!decode_module(ret, md, &local_err)) {
            if (err != nullptr) {
                *err = "decode: " + local_err;
            }
            md.cleanup();
            return false;
        }

        for (size_t j = 0; j < n; j++) {
            size_t i = fn_indices[j];

            if (i >= md.functions.size() || i >= (size_t)ret->function_count ||
                !(ret->eligible_bitmap[i / 32] & (((Uint32)1) << (i % 32)))) {
                continue;
            }

            FunctionBuilder builder(md, md.functions[i], (uint32_t)i);
            std::unique_ptr<T2Function> fn = builder.build(&local_err);

            if (fn == nullptr || !t2_validate(*fn, &local_err)) {
                continue; /* degrade to T1 for this function */
            }

            emit(*fn);
        }

        md.cleanup();
        return true;
    }

    bool t2_build_each(const ErtsT2RetainedCode *ret,
                       const std::function<void(T2Function &)> &emit,
                       int *failures,
                       std::string *err) {
        ModuleDecode md;
        std::string local_err;
        int failed = 0;

        if (!decode_module(ret, md, &local_err)) {
            if (err != nullptr) {
                *err = "decode: " + local_err;
            }
            md.cleanup();
            return false;
        }

        for (size_t i = 0; i < md.functions.size(); i++) {
            const FunctionCode &fc = md.functions[i];

            /* The strict install bitmap: a buildable-only function
             * (call_fun / is_function2) is reachable as a P1 chain
             * callee but must not burn a standalone compile attempt
             * it is guaranteed to lose at isel. */
            if (i >= (size_t)ret->function_count ||
                !(ret->install_bitmap[i / 32] & (((Uint32)1) << (i % 32)))) {
                continue;
            }

            FunctionBuilder builder(md, fc, (uint32_t)i);
            std::unique_ptr<T2Function> fn = builder.build(&local_err);

            if (fn == nullptr || !t2_validate(*fn, &local_err)) {
                /* Degrade to T1 for this function; never abort the
                 * load (map §5). */
                failed++;
                continue;
            }

            emit(*fn);
        }

        if (failures != nullptr) {
            *failures = failed;
        }

        md.cleanup();
        return true;
    }

} /* namespace erts_t2 */

/* ------------------------------------------------------------------ *
 * C entry points (test hook; see t2_retain.h)                        *
 * ------------------------------------------------------------------ */

using namespace erts_t2;

extern "C" int erts_t2_build_enabled(void) {
    static const int enabled = []() {
        const char *env = getenv("T2_BUILD");
        return (env != nullptr && env[0] == '1') ? 1 : 0;
    }();
    return enabled;
}

static int t2_dump_enabled(void) {
    static const int enabled = []() {
        const char *env = getenv("T2_DUMP");
        return (env != nullptr && env[0] == '1') ? 1 : 0;
    }();
    return enabled;
}

static int t2_debug_output_enabled(void) {
    static const int enabled = []() {
        const char *env = getenv("T2_DEBUG");
        return (env != nullptr && env[0] == '1') ? 1 : 0;
    }();
    return enabled;
}

static int t2_isel_sweep_enabled(void) {
    static const int enabled = []() {
        const char *env = getenv("T2_ISEL");
        return (env != nullptr && env[0] == '1') ? 1 : 0;
    }();
    return enabled;
}

extern "C" int erts_t2_build_all(const ErtsT2RetainedCode *ret,
                                 const void *code_hdr) {
    ModuleDecode md;
    std::string err;
    int failures = 0;
    int built = 0;
    int isel_ok = 0, isel_no = 0;
    std::map<std::string, int> isel_reasons;
    bool sweep = t2_isel_sweep_enabled() && code_hdr != nullptr;

    if (!decode_module(ret, md, &err)) {
        erts_fprintf(stderr, "t2_build: decode failed: %s\n", err.c_str());
        md.cleanup();
        return 1;
    }

    for (size_t i = 0; i < md.functions.size(); i++) {
        const FunctionCode &fc = md.functions[i];

        if (i >= (size_t)ret->function_count ||
            !(ret->eligible_bitmap[i / 32] & (((Uint32)1) << (i % 32)))) {
            continue;
        }

        FunctionBuilder builder(md, fc, (uint32_t)i);
        std::unique_ptr<T2Function> fn = builder.build(&err);

        if (fn == nullptr) {
            erts_fprintf(stderr,
                         "t2_build: %T:%T/%u: build failed: %s\n",
                         fc.module,
                         fc.function,
                         fc.arity,
                         err.c_str());
            failures++;
            continue;
        }

        if (!t2_validate(*fn, &err)) {
            erts_fprintf(stderr,
                         "t2_build: %T:%T/%u: validation failed: %s\n",
                         fc.module,
                         fc.function,
                         fc.arity,
                         err.c_str());
            failures++;
            continue;
        }

        built++;

        if (t2_dump_enabled()) {
            erts_fprintf(stderr, "%s\n", t2_dump(*fn).c_str());
        }

        /* T2_ISEL=1: identity-backend coverage. An isel rejection is a
         * scope report, not a failure — the P1 lowering table is
         * partial by design. */
        if (sweep) {
            T2IselContext ctx;
            T2LirFunction lir;
            std::string ierr;

            ctx.ret = ret;
            ctx.code_hdr = code_hdr;

            if (t2_isel(*fn, ctx, lir, &ierr) && t2_regalloc(lir, &ierr)) {
                isel_ok++;
            } else {
                isel_no++;
                isel_reasons[ierr]++;
                if (t2_debug_output_enabled()) {
                    erts_fprintf(stderr,
                                 "t2_isel: %T:%T/%u: %s\n",
                                 fc.module,
                                 fc.function,
                                 fc.arity,
                                 ierr.c_str());
                }
            }
        }
    }

    if (t2_debug_output_enabled() && !md.functions.empty()) {
        erts_fprintf(stderr,
                     "t2_build: module=%T built=%d failed=%d\n",
                     md.functions[0].module,
                     built,
                     failures);
    }

    if (sweep && !md.functions.empty()) {
        erts_fprintf(stderr,
                     "t2_isel: module=%T lowered=%d unsupported=%d\n",
                     md.functions[0].module,
                     isel_ok,
                     isel_no);
        if (t2_debug_output_enabled()) {
            for (const auto &r : isel_reasons) {
                erts_fprintf(stderr,
                             "t2_isel:   %d x %s\n",
                             r.second,
                             r.first.c_str());
            }
        }
    }

    md.cleanup();
    return failures;
}
