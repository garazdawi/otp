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
 * T2-Full tier-2 JIT: HIR core implementation. See t2_hir.hpp.
 */

#include "t2_hir.hpp"

extern "C"
{
#include "sys.h"
#include "global.h"
#include "erl_alloc.h"
#include "big.h" /* IS_SSMALL */
}

#include <algorithm>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <unordered_map>

namespace erts_t2 {

    /* ------------------------------------------------------------------ *
     * Op kind properties                                                 *
     * ------------------------------------------------------------------ */

    const char *t2_op_kind_name(T2OpKind kind) {
        switch (kind) {
        case T2OpKind::ConstInt:
            return "const_int";
        case T2OpKind::ConstFloat:
            return "const_float";
        case T2OpKind::ConstAtom:
            return "const_atom";
        case T2OpKind::ConstNil:
            return "const_nil";
        case T2OpKind::ConstLiteral:
            return "const_literal";
        case T2OpKind::Param:
            return "param";
        case T2OpKind::Phi:
            return "phi";
        case T2OpKind::Copy:
            return "copy";
        case T2OpKind::IsInteger:
            return "is_integer";
        case T2OpKind::IsFloat:
            return "is_float";
        case T2OpKind::IsNumber:
            return "is_number";
        case T2OpKind::IsAtom:
            return "is_atom";
        case T2OpKind::IsBoolean:
            return "is_boolean";
        case T2OpKind::IsTuple:
            return "is_tuple";
        case T2OpKind::IsList:
            return "is_list";
        case T2OpKind::IsNonemptyList:
            return "is_nonempty_list";
        case T2OpKind::IsNil:
            return "is_nil";
        case T2OpKind::IsBinary:
            return "is_binary";
        case T2OpKind::IsBitstring:
            return "is_bitstring";
        case T2OpKind::IsMap:
            return "is_map";
        case T2OpKind::IsPid:
            return "is_pid";
        case T2OpKind::IsPort:
            return "is_port";
        case T2OpKind::IsReference:
            return "is_reference";
        case T2OpKind::IsFunction:
            return "is_function";
        case T2OpKind::IsTaggedTuple:
            return "is_tagged_tuple";
        case T2OpKind::TestArity:
            return "test_arity";
        case T2OpKind::Succeeded:
            return "succeeded";
        case T2OpKind::CmpEqExact:
            return "cmp_eq_exact";
        case T2OpKind::CmpNeExact:
            return "cmp_ne_exact";
        case T2OpKind::CmpEq:
            return "cmp_eq";
        case T2OpKind::CmpNe:
            return "cmp_ne";
        case T2OpKind::CmpLt:
            return "cmp_lt";
        case T2OpKind::CmpLe:
            return "cmp_le";
        case T2OpKind::CmpGt:
            return "cmp_gt";
        case T2OpKind::CmpGe:
            return "cmp_ge";
        case T2OpKind::Add:
            return "add";
        case T2OpKind::Sub:
            return "sub";
        case T2OpKind::Mul:
            return "mul";
        case T2OpKind::IDiv:
            return "idiv";
        case T2OpKind::Rem:
            return "rem";
        case T2OpKind::Band:
            return "band";
        case T2OpKind::Bor:
            return "bor";
        case T2OpKind::Bxor:
            return "bxor";
        case T2OpKind::Bsl:
            return "bsl";
        case T2OpKind::Bsr:
            return "bsr";
        case T2OpKind::Bnot:
            return "bnot";
        case T2OpKind::Neg:
            return "neg";
        case T2OpKind::UntagInt:
            return "untag_int";
        case T2OpKind::TagInt:
            return "tag_int";
        case T2OpKind::AddSmall:
            return "add_small";
        case T2OpKind::SubSmall:
            return "sub_small";
        case T2OpKind::MulRaw:
            return "mul_raw";
        case T2OpKind::SpeculateType:
            return "speculate_type";
        case T2OpKind::SpeculateRange:
            return "speculate_range";
        case T2OpKind::MakeTuple:
            return "make_tuple";
        case T2OpKind::GetTupleElement:
            return "get_tuple_element";
        case T2OpKind::MakeList:
            return "make_list";
        case T2OpKind::GetHd:
            return "get_hd";
        case T2OpKind::GetTl:
            return "get_tl";
        case T2OpKind::GetMapElement:
            return "get_map_element";
        case T2OpKind::IsFlatmapBounded:
            return "is_flatmap_bounded";
        case T2OpKind::FlatmapSize:
            return "flatmap_size";
        case T2OpKind::FlatmapKeyAt:
            return "flatmap_key_at";
        case T2OpKind::FlatmapValAt:
            return "flatmap_val_at";
        case T2OpKind::FoldBudget:
            return "fold_budget";
        case T2OpKind::StartMatch:
            return "start_match";
        case T2OpKind::BsMatch:
            return "bs_match";
        case T2OpKind::BsGetTail:
            return "bs_get_tail";
        case T2OpKind::BsTestTail:
            return "bs_test_tail";
        case T2OpKind::BsBase:
            return "bs_base";
        case T2OpKind::BsLimit:
            return "bs_limit";
        case T2OpKind::BsCursor:
            return "bs_cursor";
        case T2OpKind::BsEnsure:
            return "bs_ensure";
        case T2OpKind::BsRead:
            return "bs_read";
        case T2OpKind::BsSync:
            return "bs_sync";
        case T2OpKind::Call:
            return "call";
        case T2OpKind::CallExt:
            return "call_ext";
        case T2OpKind::CallFun:
            return "call_fun";
        case T2OpKind::TailCall:
            return "tail_call";
        case T2OpKind::TailCallExt:
            return "tail_call_ext";
        case T2OpKind::TailCallFun:
            return "tail_call_fun";
        case T2OpKind::Bif:
            return "bif";
        case T2OpKind::GuardBif:
            return "guard_bif";
        case T2OpKind::MakeFun:
            return "make_fun";
        case T2OpKind::Branch:
            return "branch";
        case T2OpKind::Jump:
            return "jump";
        case T2OpKind::Switch:
            return "switch";
        case T2OpKind::Return:
            return "return";
        case T2OpKind::GcTest:
            return "gc_test";
        case T2OpKind::ReductionCheck:
            return "reduction_check";
        case T2OpKind::DemoteCallee:
            return "demote_callee";
        case T2OpKind::ChargeReds:
            return "charge_reds";
        case T2OpKind::ScheduleOut:
            return "schedule_out";
        case T2OpKind::FrameState:
            return "framestate";
        case T2OpKind::Allocate:
            return "allocate";
        case T2OpKind::Deallocate:
            return "deallocate";
        case T2OpKind::Trim:
            return "trim";
        case T2OpKind::Opaque:
            return "opaque";
        case T2OpKind::Invalid:
            break;
        }
        return "invalid";
    }

    bool t2_op_is_terminator(T2OpKind kind) {
        switch (kind) {
        case T2OpKind::Branch:
        case T2OpKind::Jump:
        case T2OpKind::Switch:
        case T2OpKind::Return:
        case T2OpKind::TailCall:
        case T2OpKind::TailCallExt:
        case T2OpKind::TailCallFun:
        case T2OpKind::DemoteCallee:
        case T2OpKind::Opaque:
            return true;
        default:
            return false;
        }
    }

    bool t2_op_produces_value(T2OpKind kind) {
        switch (kind) {
        case T2OpKind::ConstInt:
        case T2OpKind::ConstFloat:
        case T2OpKind::ConstAtom:
        case T2OpKind::ConstNil:
        case T2OpKind::ConstLiteral:
        case T2OpKind::Param:
        case T2OpKind::Phi:
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
        case T2OpKind::IsBinary:
        case T2OpKind::IsBitstring:
        case T2OpKind::IsMap:
        case T2OpKind::IsPid:
        case T2OpKind::IsPort:
        case T2OpKind::IsReference:
        case T2OpKind::IsFunction:
        case T2OpKind::IsTaggedTuple:
        case T2OpKind::TestArity:
        case T2OpKind::Succeeded:
        case T2OpKind::CmpEqExact:
        case T2OpKind::CmpNeExact:
        case T2OpKind::CmpEq:
        case T2OpKind::CmpNe:
        case T2OpKind::CmpLt:
        case T2OpKind::CmpLe:
        case T2OpKind::CmpGt:
        case T2OpKind::CmpGe:
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
        case T2OpKind::UntagInt:
        case T2OpKind::TagInt:
        case T2OpKind::AddSmall:
        case T2OpKind::SubSmall:
        case T2OpKind::MulRaw:
        case T2OpKind::MakeTuple:
        case T2OpKind::GetTupleElement:
        case T2OpKind::MakeList:
        case T2OpKind::GetHd:
        case T2OpKind::GetTl:
        case T2OpKind::GetMapElement:
        case T2OpKind::IsFlatmapBounded:
        case T2OpKind::FlatmapSize:
        case T2OpKind::FlatmapKeyAt:
        case T2OpKind::FlatmapValAt:
        case T2OpKind::StartMatch:
        case T2OpKind::BsMatch:
        case T2OpKind::BsGetTail:
        case T2OpKind::BsTestTail:
        case T2OpKind::BsBase:
        case T2OpKind::BsLimit:
        case T2OpKind::BsCursor:
        case T2OpKind::BsEnsure:
        case T2OpKind::BsRead:
        case T2OpKind::Call:
        case T2OpKind::CallExt:
        case T2OpKind::CallFun:
        case T2OpKind::Bif:
        case T2OpKind::GuardBif:
        case T2OpKind::MakeFun:
            return true;
        default:
            return false;
        }
    }

    /* ------------------------------------------------------------------ *
     * Arena                                                              *
     * ------------------------------------------------------------------ */

    T2Arena::T2Arena() : current(nullptr), used_bytes(0) {
    }

    T2Arena::~T2Arena() {
        Chunk *c = current;
        while (c != nullptr) {
            Chunk *next = c->next;
            erts_free(ERTS_ALC_T_T2_CODE, c);
            c = next;
        }
    }

    void T2Arena::new_chunk(size_t min_size) {
        constexpr size_t DEFAULT_PAYLOAD = 16 * 1024 - sizeof(Chunk);
        size_t capacity =
                DEFAULT_PAYLOAD < min_size ? min_size : DEFAULT_PAYLOAD;
        Chunk *c = static_cast<Chunk *>(
                erts_alloc(ERTS_ALC_T_T2_CODE, sizeof(Chunk) + capacity));

        c->next = current;
        c->capacity = capacity;
        c->used = 0;
        current = c;
    }

    void *T2Arena::alloc(size_t size, size_t align) {
        ASSERT(align != 0 && (align & (align - 1)) == 0);

        if (size == 0) {
            size = 1;
        }

        for (;;) {
            if (current != nullptr) {
                uintptr_t base = reinterpret_cast<uintptr_t>(current + 1);
                uintptr_t ptr = (base + current->used + (align - 1)) &
                                ~static_cast<uintptr_t>(align - 1);
                size_t new_used = (ptr + size) - base;

                if (new_used <= current->capacity) {
                    current->used = new_used;
                    used_bytes += size;
                    return reinterpret_cast<void *>(ptr);
                }
            }

            new_chunk(size + align);
        }
    }

    /* ------------------------------------------------------------------ *
     * Function construction                                              *
     * ------------------------------------------------------------------ */

    T2BasicBlock *T2Function::new_block() {
        T2BasicBlock *b = arena.create<T2BasicBlock>();

        b->id = static_cast<uint32_t>(blocks.size());
        blocks.push_back(b);

        return b;
    }

    T2Op *T2Function::link_op(T2BasicBlock *b, T2Op *op) {
        op->block = b;

        if (op->kind == T2OpKind::Phi) {
            op->prev = b->phis_tail;
            if (b->phis_tail != nullptr) {
                b->phis_tail->next = op;
            } else {
                b->phis_head = op;
            }
            b->phis_tail = op;
        } else if (t2_op_is_terminator(op->kind)) {
            ASSERT(b->terminator == nullptr);
            b->terminator = op;
        } else {
            op->prev = b->ops_tail;
            if (b->ops_tail != nullptr) {
                b->ops_tail->next = op;
            } else {
                b->ops_head = op;
            }
            b->ops_tail = op;
        }

        return op;
    }

    T2Op *T2Function::new_op(T2BasicBlock *b, T2OpKind kind, T2Type ty) {
        T2Op *op = arena.create<T2Op>();

        op->kind = kind;
        op->type = ty;

        if (t2_op_produces_value(kind)) {
            T2Value *v = arena.create<T2Value>();

            v->id = static_cast<uint32_t>(values.size());
            v->type = ty;
            v->def = op;
            values.push_back(v);

            op->result = v;
        }

        return link_op(b, op);
    }

    void T2Function::set_operands(T2Op *op,
                                  std::initializer_list<T2Value *> ins) {
        op->num_operands = static_cast<uint16_t>(ins.size());
        op->operands = arena.alloc_array<T2Value *>(ins.size());

        size_t i = 0;
        for (T2Value *v : ins) {
            op->operands[i++] = v;
        }
    }

    void T2Function::set_operands(T2Op *op, const std::vector<T2Value *> &ins) {
        op->num_operands = static_cast<uint16_t>(ins.size());
        op->operands = arena.alloc_array<T2Value *>(ins.size());

        for (size_t i = 0; i < ins.size(); i++) {
            op->operands[i] = ins[i];
        }
    }

    T2Op *T2Function::new_phi(T2BasicBlock *b, T2Type ty) {
        return new_op(b, T2OpKind::Phi, ty);
    }

    void T2Function::set_phi_inputs(T2Op *phi,
                                    const std::vector<T2Value *> &vals,
                                    const std::vector<T2BasicBlock *> &preds) {
        ASSERT(phi->kind == T2OpKind::Phi);
        ASSERT(vals.size() == preds.size());

        set_operands(phi, vals);

        phi->phi_blocks = arena.alloc_array<T2BasicBlock *>(preds.size());
        for (size_t i = 0; i < preds.size(); i++) {
            phi->phi_blocks[i] = preds[i];
        }
    }

    T2Value *T2Function::emit_param(T2BasicBlock *b, uint32_t idx, T2Type ty) {
        T2Op *op = new_op(b, T2OpKind::Param, ty);
        op->index = idx;
        return op->result;
    }

    T2Value *T2Function::emit_const_int(T2BasicBlock *b, Sint64 v) {
        T2Op *op = new_op(b, T2OpKind::ConstInt, T2Type::integer(v, v));
        op->imm_int = v;
        return op->result;
    }

    T2Value *T2Function::emit_const_atom(T2BasicBlock *b, Eterm atom) {
        T2Op *op = new_op(b, T2OpKind::ConstAtom, T2Type::of(BEAM_TYPE_ATOM));
        op->imm_term = atom;
        return op->result;
    }

    T2Value *T2Function::emit_const_nil(T2BasicBlock *b) {
        T2Op *op = new_op(b, T2OpKind::ConstNil, T2Type::of(BEAM_TYPE_NIL));
        op->imm_term = NIL;
        return op->result;
    }

    T2Value *T2Function::emit_const_literal(T2BasicBlock *b,
                                            uint32_t idx,
                                            Eterm term,
                                            T2Type ty) {
        T2Op *op = new_op(b, T2OpKind::ConstLiteral, ty);
        op->index = idx;
        op->imm_term = term;
        return op->result;
    }

    T2Value *T2Function::emit_unary(T2BasicBlock *b,
                                    T2OpKind kind,
                                    T2Value *a,
                                    T2Type ty) {
        T2Op *op = new_op(b, kind, ty);
        set_operands(op, {a});
        return op->result;
    }

    T2Value *T2Function::emit_binary(T2BasicBlock *b,
                                     T2OpKind kind,
                                     T2Value *a,
                                     T2Value *c,
                                     T2Type ty) {
        T2Op *op = new_op(b, kind, ty);
        set_operands(op, {a, c});
        return op->result;
    }

    T2Value *T2Function::emit_get_tuple_element(T2BasicBlock *b,
                                                T2Value *tuple,
                                                uint32_t idx,
                                                T2Type ty) {
        T2Op *op = new_op(b, T2OpKind::GetTupleElement, ty);
        set_operands(op, {tuple});
        op->index = idx;
        return op->result;
    }

    T2Value *T2Function::emit_call(T2BasicBlock *b,
                                   T2OpKind kind,
                                   Eterm m,
                                   Eterm f,
                                   uint32_t ar,
                                   const std::vector<T2Value *> &args,
                                   T2Type ty) {
        T2Op *op = new_op(b, kind, ty);
        set_operands(op, args);
        op->mfa_m = m;
        op->mfa_f = f;
        op->index = ar;
        return op->result; /* null for tail calls */
    }

    void T2Function::emit_branch(T2BasicBlock *b,
                                 T2Value *cond,
                                 T2BasicBlock *t,
                                 T2BasicBlock *e) {
        T2Op *op = new_op(b, T2OpKind::Branch, T2Type::none());
        set_operands(op, {cond});
        op->succ_then = t;
        op->succ_else = e;
    }

    void T2Function::emit_jump(T2BasicBlock *b, T2BasicBlock *target) {
        T2Op *op = new_op(b, T2OpKind::Jump, T2Type::none());
        op->succ_then = target;
    }

    void T2Function::emit_return(T2BasicBlock *b, T2Value *v) {
        T2Op *op = new_op(b, T2OpKind::Return, T2Type::none());
        set_operands(op, {v});
    }

    /* Enumerate the successors of a terminator. */
    template<typename F>
    static void for_each_succ(const T2Op *term, F fn) {
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
            /* Return and tail calls have no successors. */
            break;
        }
    }

    void T2Function::finalize() {
        /* Compute the predecessor arrays. A terminator can name the same
         * successor more than once (e.g. a switch with several values
         * mapping to one target); CFG edges are deduplicated so a
         * predecessor appears once and phis carry one input per edge. */
        std::vector<std::vector<T2BasicBlock *>> preds(blocks.size());

        for (T2BasicBlock *b : blocks) {
            for_each_succ(b->terminator, [&](T2BasicBlock *succ) {
                if (succ != nullptr) {
                    auto &v = preds[succ->id];

                    if (std::find(v.begin(), v.end(), b) == v.end()) {
                        v.push_back(b);
                    }
                }
            });
        }

        for (T2BasicBlock *b : blocks) {
            const auto &v = preds[b->id];

            b->num_preds = static_cast<uint32_t>(v.size());
            b->preds = arena.alloc_array<T2BasicBlock *>(v.size());
            for (size_t i = 0; i < v.size(); i++) {
                b->preds[i] = v[i];
            }
        }
    }

    /* ------------------------------------------------------------------ *
     * Validator                                                          *
     * ------------------------------------------------------------------ */

    namespace {

        struct Validator {
            const T2Function &fn;
            std::string *err;

            /* Reachability and dominator sets over block ids (bitsets). */
            std::vector<bool> reachable;
            std::vector<std::vector<uint64_t>> dom;
            size_t words = 0;

            /* Position of each op within its block: phis are 0, body ops are
             * 1..N, the terminator is UINT32_MAX. */
            std::unordered_map<const T2Op *, uint32_t> pos;

            Validator(const T2Function &fn_, std::string *err_)
                    : fn(fn_), err(err_) {
            }

            bool fail(const char *fmt, ...) {
                if (err != nullptr) {
                    char buf[256];
                    va_list args;

                    va_start(args, fmt);
                    vsnprintf(buf, sizeof(buf), fmt, args);
                    va_end(args);

                    *err = buf;
                }
                return false;
            }

            bool dominates(uint32_t a, uint32_t b) const {
                return (dom[b][a / 64] >> (a % 64)) & 1;
            }

            void compute_dominators() {
                size_t n = fn.blocks.size();

                words = (n + 63) / 64;
                reachable.assign(n, false);
                dom.assign(n, std::vector<uint64_t>(words, 0));

                /* DFS reachability from the entry block. */
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

                /* Iterative dataflow: dom(entry) = {entry};
                 * dom(b) = {b} | intersect(dom(p) for reachable preds p). */
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

            bool check_op_common(const T2Op *op, const T2BasicBlock *b) {
                if (op->kind == T2OpKind::Invalid ||
                    op->kind == T2OpKind::FrameState) {
                    return fail("block %u: op kind %s must not appear in P0 IR",
                                b->id,
                                t2_op_kind_name(op->kind));
                }

                if (op->fs != nullptr) {
                    return fail(
                            "block %u: op %s has a framestate ref; framestates "
                            "are reserved-unused in P0",
                            b->id,
                            t2_op_kind_name(op->kind));
                }

                if (op->block != b) {
                    return fail("block %u: op %s has wrong owner block",
                                b->id,
                                t2_op_kind_name(op->kind));
                }

                {
                    bool wants_result = t2_op_produces_value(op->kind);

                    if (wants_result != (op->result != nullptr)) {
                        return fail("block %u: op %s result presence mismatch",
                                    b->id,
                                    t2_op_kind_name(op->kind));
                    }
                }

                if (op->result != nullptr) {
                    if (op->result->id >= fn.values.size() ||
                        fn.values[op->result->id] != op->result) {
                        return fail("block %u: op %s result not registered",
                                    b->id,
                                    t2_op_kind_name(op->kind));
                    }
                    if (op->result->def != op) {
                        return fail("block %u: op %s result->def mismatch",
                                    b->id,
                                    t2_op_kind_name(op->kind));
                    }
                }

                for (uint16_t i = 0; i < op->num_operands; i++) {
                    const T2Value *v = op->operands[i];

                    if (v == nullptr) {
                        return fail("block %u: op %s operand %u is null",
                                    b->id,
                                    t2_op_kind_name(op->kind),
                                    i);
                    }
                    if (v->id >= fn.values.size() || fn.values[v->id] != v) {
                        return fail("block %u: op %s operand %u not registered",
                                    b->id,
                                    t2_op_kind_name(op->kind),
                                    i);
                    }
                    if (v->def == nullptr) {
                        return fail("block %u: op %s operand %u has no def",
                                    b->id,
                                    t2_op_kind_name(op->kind),
                                    i);
                    }
                }

                return true;
            }

            bool check_terminator(const T2BasicBlock *b) {
                const T2Op *term = b->terminator;

                if (term == nullptr) {
                    return fail("block %u: missing terminator", b->id);
                }
                if (!t2_op_is_terminator(term->kind)) {
                    return fail(
                            "block %u: terminator kind %s is not a terminator",
                            b->id,
                            t2_op_kind_name(term->kind));
                }
                if (!check_op_common(term, b)) {
                    return false;
                }

                switch (term->kind) {
                case T2OpKind::Branch:
                    if (term->num_operands != 1) {
                        return fail("block %u: branch expects 1 operand",
                                    b->id);
                    }
                    if (term->succ_then == nullptr ||
                        term->succ_else == nullptr) {
                        return fail("block %u: branch with null successor",
                                    b->id);
                    }
                    break;
                case T2OpKind::Jump:
                    if (term->succ_then == nullptr) {
                        return fail("block %u: jump with null target", b->id);
                    }
                    break;
                case T2OpKind::Switch:
                    if (term->num_operands != 1) {
                        return fail("block %u: switch expects 1 operand",
                                    b->id);
                    }
                    if (term->default_target == nullptr) {
                        return fail("block %u: switch with null default",
                                    b->id);
                    }
                    for (uint32_t i = 0; i < term->num_cases; i++) {
                        if (term->cases[i].target == nullptr) {
                            return fail(
                                    "block %u: switch case %u has null target",
                                    b->id,
                                    i);
                        }
                    }
                    break;
                case T2OpKind::Return:
                    if (term->num_operands != 1) {
                        return fail("block %u: return expects 1 operand",
                                    b->id);
                    }
                    break;
                default:
                    /* Tail calls: any number of arguments. */
                    break;
                }

                /* All successors must be registered blocks. */
                {
                    bool ok = true;

                    for_each_succ(term, [&](T2BasicBlock *succ) {
                        if (succ != nullptr && (succ->id >= fn.blocks.size() ||
                                                fn.blocks[succ->id] != succ)) {
                            ok = false;
                        }
                    });
                    if (!ok) {
                        return fail("block %u: terminator targets unregistered "
                                    "block",
                                    b->id);
                    }
                }

                return true;
            }

            /* SSA property: defs dominate uses. */
            bool check_use(const T2Op *user, uint16_t operand_idx) {
                const T2Value *v = user->operands[operand_idx];
                const T2Op *def = v->def;
                const T2BasicBlock *ub = user->block;
                const T2BasicBlock *db = def->block;

                if (!reachable[ub->id]) {
                    /* Unreachable code is structurally checked only. */
                    return true;
                }

                if (user->kind == T2OpKind::Phi) {
                    /* The def must dominate the incoming edge's predecessor. */
                    const T2BasicBlock *pred = user->phi_blocks[operand_idx];

                    if (!reachable[pred->id]) {
                        return true;
                    }
                    if (!dominates(db->id, pred->id)) {
                        return fail("block %u: phi input %u (v%u) does not "
                                    "dominate "
                                    "incoming edge from block %u",
                                    ub->id,
                                    operand_idx,
                                    v->id,
                                    pred->id);
                    }
                    return true;
                }

                if (db == ub) {
                    if (pos.at(def) >= pos.at(user)) {
                        return fail("block %u: v%u used before its definition",
                                    ub->id,
                                    v->id);
                    }
                    return true;
                }

                if (!dominates(db->id, ub->id)) {
                    return fail("block %u: use of v%u whose def (block %u) "
                                "does not "
                                "dominate it",
                                ub->id,
                                v->id,
                                db->id);
                }

                return true;
            }

            bool check_phi(const T2Op *phi, const T2BasicBlock *b) {
                if (phi->kind != T2OpKind::Phi) {
                    return fail("block %u: non-phi op %s on the phi list",
                                b->id,
                                t2_op_kind_name(phi->kind));
                }

                if (phi->num_operands != b->num_preds) {
                    return fail("block %u: phi has %u inputs but block has %u "
                                "preds",
                                b->id,
                                phi->num_operands,
                                b->num_preds);
                }

                if (phi->num_operands == 0 && reachable[b->id]) {
                    /* An empty phi in reachable code is an undefined
                     * read that leaked through construction. */
                    return fail("block %u: empty phi in reachable block",
                                b->id);
                }

                if (phi->num_operands > 0 && phi->phi_blocks == nullptr) {
                    return fail(
                            "block %u: phi with inputs but no incoming blocks",
                            b->id);
                }

                /* Each incoming block must be a distinct predecessor. */
                for (uint16_t i = 0; i < phi->num_operands; i++) {
                    const T2BasicBlock *in = phi->phi_blocks[i];
                    bool found = false;

                    if (in == nullptr) {
                        return fail("block %u: phi input %u has null block",
                                    b->id,
                                    i);
                    }
                    for (uint32_t p = 0; p < b->num_preds; p++) {
                        if (b->preds[p] == in) {
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        return fail(
                                "block %u: phi input %u comes from non-pred "
                                "block %u",
                                b->id,
                                i,
                                in->id);
                    }
                    for (uint16_t j = 0; j < i; j++) {
                        if (phi->phi_blocks[j] == in) {
                            return fail("block %u: phi has duplicate input for "
                                        "block %u",
                                        b->id,
                                        in->id);
                        }
                    }
                }

                return true;
            }

            /* ---------------------------------------------------------- *
             * P1 sync-map / frame / canonical-home checks                *
             * (run only when fn.sync_complete; see t2_hir.hpp)           *
             * ---------------------------------------------------------- */

            /* Op kinds whose boundary is a rung-1 sync point and must
             * carry a map (PLAN/T2/01 §6.1's exit/trap/GC set restricted
             * to what the eligible op set can produce). */
            static bool sync_required(const T2Op *op) {
                switch (op->kind) {
                case T2OpKind::Call:
                case T2OpKind::CallExt:
                /* A fun application (P1a): a call boundary like Call —
                 * T1 re-executing the site reads the args AND the fun
                 * from their decoded homes. */
                case T2OpKind::CallFun:
                case T2OpKind::Return:
                case T2OpKind::GcTest:
                case T2OpKind::Allocate:
                /* gc_bif-lowered arithmetic + generic BIFs may GC/trap. */
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
                case T2OpKind::Bif:
                /* GuardBif is deliberately NOT here: the WIN 3 subset
                 * is read-only, no alloc, no trap. A real fail label
                 * branches in-blob; a {f,0} fail side-exits to the
                 * op's own T1 EFFECT site, which re-reads the sources
                 * from their canonical slots and raises — the same
                 * boundary-state-by-predecessors contract as the
                 * error-exit TailCallExt shapes below
                 * (translate_error_exit). */
                /* A recovered loop's back-edge yield check
                 * (t2_loop.cpp): the map is the fresh-call argument
                 * vector the demote path re-enters T1 with. */
                case T2OpKind::ReductionCheck:
                /* An intrinsic loop's demote-to-callee transfer: the
                 * map is the callee's fresh-call vector over the
                 * caller's live frame. */
                case T2OpKind::DemoteCallee:
                /* bs_start_match3 may allocate a fresh context (GC);
                 * bs_get_tail allocates the tail sub-bitstring. */
                case T2OpKind::StartMatch:
                case T2OpKind::BsGetTail:
                    return true;
                /* The maps:fold budget check side-exits (uncharged) to
                 * the erased call's T1 PC; the map IS the deopt
                 * contract (the call-boundary state). */
                case T2OpKind::FoldBudget:
                    return true;
                case T2OpKind::BsMatch:
                    /* A sync point only when its command list needs
                     * heap (imm_int = heap words; the internal
                     * TEST_HEAP the reused T1 emitter inserts). */
                    return op->imm_int != 0;
                case T2OpKind::TailCall:
                case T2OpKind::TailCallExt:
                    /* Error exits (frame-polymorphic raise blocks) and
                     * garbage-dealloc transfers carry no map by design;
                     * see t2_hir.hpp. */
                    return (op->flags &
                            (T2_OP_ERR_EXIT_SHARED | T2_OP_ERR_EXIT_OP |
                             T2_OP_GARBAGE_DEALLOC)) == 0;
                default:
                    return false;
                }
            }

            /* A sync-map value must be defined strictly before the op the
             * map is attached to (the map describes the boundary *before*
             * the op). */
            bool check_sync_value_def(const T2Op *op,
                                      const T2Value *v,
                                      const char *what,
                                      uint32_t slot) {
                if (v == nullptr || v->id >= fn.values.size() ||
                    fn.values[v->id] != v) {
                    return fail("block %u: sync map %s%u on %s holds an "
                                "unregistered value",
                                op->block->id,
                                what,
                                slot,
                                t2_op_kind_name(op->kind));
                }

                const T2BasicBlock *db = v->def->block;
                const T2BasicBlock *ub = op->block;

                if (!reachable[ub->id]) {
                    return true;
                }
                if (db == ub) {
                    if (pos.at(v->def) >= pos.at(op)) {
                        return fail("block %u: sync map %s%u on %s uses v%u "
                                    "before its definition",
                                    ub->id,
                                    what,
                                    slot,
                                    t2_op_kind_name(op->kind),
                                    v->id);
                    }
                } else if (!dominates(db->id, ub->id)) {
                    return fail("block %u: sync map %s%u on %s uses v%u whose "
                                "def does not dominate it",
                                ub->id,
                                what,
                                slot,
                                t2_op_kind_name(op->kind),
                                v->id);
                }
                return true;
            }

            /* An untagged machine word (PLAN/T2/04 §11.2): never a
             * valid term, so it must never occupy an X/Y slot at a sync
             * boundary — GC would misread it. Corruption-class
             * invariant (i) of PLAN/T2FULL/09 §4. A T2_OP_RAW_MODE
             * UntagInt is exempt: it is the P2 loop-unboxing form,
             * whose result IS slot-homed and whose sync-map mentions
             * are governed by the raw_mask rule below instead. */
            static bool value_is_untagged(const T2Value *v) {
                return v->def != nullptr &&
                       (v->def->kind == T2OpKind::UntagInt ||
                        v->def->kind == T2OpKind::MulRaw) &&
                       (v->def->flags & T2_OP_RAW_MODE) == 0;
            }

            bool check_sync_map(const T2Op *op) {
                const T2SyncMap *m = op->sync;

                if (m == nullptr) {
                    if (sync_required(op)) {
                        return fail("block %u: sync-point op %s has no sync "
                                    "map",
                                    op->block->id,
                                    t2_op_kind_name(op->kind));
                    }
                    if (op->raw_mask != 0) {
                        /* A re-tag mask is only meaningful against a sync
                         * map (the emitter re-tags exactly the masked
                         * map homes); a mask without a map is a transform
                         * bug, never silent. */
                        return fail("block %u: %s carries a raw re-tag mask "
                                    "but no sync map",
                                    op->block->id,
                                    t2_op_kind_name(op->kind));
                    }
                    return true;
                }

                for (uint32_t i = 0; i < m->x_live; i++) {
                    if (value_is_untagged(m->x[i])) {
                        return fail("block %u: untagged v%u in the %s sync "
                                    "map (x%u)",
                                    op->block->id,
                                    m->x[i]->id,
                                    t2_op_kind_name(op->kind),
                                    i);
                    }
                    /* P2 loop unboxing: a RAW-IN-HOME value may be named
                     * by a sync map ONLY when the op's raw_mask declares
                     * its X home, so the emitter re-tags it in the cold
                     * path before T1 observes the register file. Both
                     * directions are corruption-class: a raw home the
                     * mask misses leaks a raw word to T1 as a term; a
                     * masked tagged home would be double-tagged. */
                    {
                        bool raw = t2_value_is_raw_home(m->x[i]);
                        bool masked = i < 32 &&
                                      (op->raw_mask & ((uint32_t)1 << i)) != 0;

                        if (raw != masked) {
                            return fail("block %u: %s sync map x%u (v%u) is "
                                        "%s but the raw re-tag mask says %s",
                                        op->block->id,
                                        t2_op_kind_name(op->kind),
                                        i,
                                        m->x[i]->id,
                                        raw ? "raw-in-home" : "tagged",
                                        masked ? "raw" : "tagged");
                        }
                    }
                }
                if (op->raw_mask != 0 && m->x_live < 32 &&
                    (op->raw_mask >> m->x_live) != 0) {
                    return fail("block %u: %s raw re-tag mask names an X "
                                "home at/above x_live %u",
                                op->block->id,
                                t2_op_kind_name(op->kind),
                                m->x_live);
                }
                for (int32_t i = 0;
                     m->frame_size != T2_NO_FRAME && i < m->frame_size;
                     i++) {
                    if (value_is_untagged(m->y[i]) ||
                        t2_value_is_raw_home(m->y[i])) {
                        return fail("block %u: untagged/raw v%u in the %s "
                                    "sync map (y%d)",
                                    op->block->id,
                                    m->y[i]->id,
                                    t2_op_kind_name(op->kind),
                                    i);
                    }
                }

                for (uint32_t i = 0; i < m->x_live; i++) {
                    if (!check_sync_value_def(op, m->x[i], "x", i)) {
                        return false;
                    }
                }
                if (m->frame_size != T2_NO_FRAME) {
                    for (int32_t i = 0; i < m->frame_size; i++) {
                        if (!check_sync_value_def(op,
                                                  m->y[i],
                                                  "y",
                                                  (uint32_t)i)) {
                            return false;
                        }
                    }
                }

                /* Boundary convention for calls: X0..arity-1 hold exactly
                 * the call's operands (state after argument moves, before
                 * the transfer). */
                switch (op->kind) {
                case T2OpKind::Call:
                case T2OpKind::CallExt:
                case T2OpKind::TailCall:
                case T2OpKind::TailCallExt:
                    if ((op->flags &
                         (T2_OP_ERR_EXIT_OP | T2_OP_ERR_EXIT_SHARED)) != 0) {
                        break; /* error exits: args are not a live X prefix */
                    }
                    if (m->x_live != op->index ||
                        op->num_operands != op->index) {
                        return fail("block %u: call sync map x_live %u != "
                                    "arity %u",
                                    op->block->id,
                                    m->x_live,
                                    op->index);
                    }
                    for (uint32_t i = 0; i < m->x_live; i++) {
                        if (m->x[i] != op->operands[i]) {
                            return fail("block %u: call sync map x%u does not "
                                        "hold argument %u",
                                        op->block->id,
                                        i,
                                        i);
                        }
                    }
                    /* Tail transfers leave no frame behind. */
                    if ((op->kind == T2OpKind::TailCall ||
                         op->kind == T2OpKind::TailCallExt) &&
                        m->frame_size != T2_NO_FRAME) {
                        return fail("block %u: tail-call sync map still has a "
                                    "frame",
                                    op->block->id);
                    }
                    break;
                case T2OpKind::ReductionCheck:
                    if (op->flags & T2_OP_RC_CALLEE) {
                        /* Intrinsic back edge (P2 commit 8): the map
                         * is the CALLEE's fresh-call vector — x_live
                         * must equal the callee arity recorded on the
                         * op — over the caller's still-live frame
                         * (walk-checked below); the callee MFA and
                         * its T1 entry must be resolved. */
                        if (op->live == 0 || m->x_live != op->live) {
                            return fail("block %u: callee back-edge sync "
                                        "map x_live %u != callee arity %u",
                                        op->block->id,
                                        m->x_live,
                                        op->live);
                        }
                        if (op->mfa_m == 0 || op->mfa_f == 0 ||
                            op->imm_int == 0) {
                            return fail("block %u: callee back-edge "
                                        "without a resolved callee",
                                        op->block->id);
                        }
                        break;
                    }
                    /* Back-edge boundary: exactly a fresh-call
                     * argument vector (PLAN/T2/08 §4.5) — the loop
                     * state in X0..arity-1, no frame. */
                    if (m->x_live != fn.arity) {
                        return fail("block %u: back-edge sync map "
                                    "x_live %u != arity %u",
                                    op->block->id,
                                    m->x_live,
                                    fn.arity);
                    }
                    if (m->frame_size != T2_NO_FRAME) {
                        return fail("block %u: back-edge sync map "
                                    "still has a frame",
                                    op->block->id);
                    }
                    break;
                case T2OpKind::DemoteCallee:
                    /* The callee's fresh-call vector, pinned at the
                     * transfer (P2 commit 8). */
                    if (op->live == 0 || m->x_live != op->live) {
                        return fail("block %u: demote-callee sync map "
                                    "x_live %u != callee arity %u",
                                    op->block->id,
                                    m->x_live,
                                    op->live);
                    }
                    if (op->mfa_m == 0 || op->mfa_f == 0 || op->imm_int == 0) {
                        return fail("block %u: demote-callee without a "
                                    "resolved callee",
                                    op->block->id);
                    }
                    break;
                case T2OpKind::Return:
                    if (m->x_live != 1 || op->num_operands != 1 ||
                        m->x[0] != op->operands[0]) {
                        return fail("block %u: return sync map must be "
                                    "exactly {x0 -> return value}",
                                    op->block->id);
                    }
                    if (m->frame_size != T2_NO_FRAME) {
                        return fail("block %u: return sync map still has a "
                                    "frame (missing deallocate)",
                                    op->block->id);
                    }
                    break;
                default:
                    break;
                }

                return true;
            }

            /* Forward dataflow of the frame size over the CFG. Preds that
             * disagree on a block's incoming frame mark it
             * frame-polymorphic (FRAME_CONFLICT) — legal for raise-only
             * blocks; any frame-dependent op under a conflict is an
             * error. */
            static constexpr int32_t FRAME_UNKNOWN = -2;
            static constexpr int32_t FRAME_CONFLICT = -3;

            bool frame_transfer(const T2BasicBlock *b,
                                int32_t in,
                                int32_t *out) {
                int32_t f = in;

                for (const T2Op *op = b->ops_head; op != nullptr;
                     op = op->next) {
                    if (f == FRAME_CONFLICT) {
                        switch (op->kind) {
                        case T2OpKind::Allocate:
                        case T2OpKind::Deallocate:
                        case T2OpKind::Trim:
                            return fail("block %u: frame op in a "
                                        "frame-polymorphic block",
                                        b->id);
                        default:
                            continue;
                        }
                    }
                    switch (op->kind) {
                    case T2OpKind::Allocate:
                        if (f != T2_NO_FRAME) {
                            return fail("block %u: allocate with a live frame",
                                        b->id);
                        }
                        f = (int32_t)op->index;
                        break;
                    case T2OpKind::Deallocate:
                        if (f != (int32_t)op->index) {
                            return fail("block %u: deallocate %u but frame "
                                        "size is %d",
                                        b->id,
                                        op->index,
                                        f);
                        }
                        f = T2_NO_FRAME;
                        break;
                    case T2OpKind::Trim:
                        if (f != (int32_t)(op->index + op->imm_int)) {
                            return fail("block %u: trim %u/%lld but frame "
                                        "size is %d",
                                        b->id,
                                        op->index,
                                        (long long)op->imm_int,
                                        f);
                        }
                        f = (int32_t)op->imm_int;
                        break;
                    default:
                        break;
                    }
                }

                *out = f;
                return true;
            }

            bool run_sync_checks() {
                size_t n = fn.blocks.size();

                /* --- frame dataflow ------------------------------------ */
                std::vector<int32_t> frame_in(n, FRAME_UNKNOWN);
                std::vector<const T2BasicBlock *> work;

                frame_in[0] = T2_NO_FRAME;
                work.push_back(fn.blocks[0]);

                while (!work.empty()) {
                    const T2BasicBlock *b = work.back();
                    /* Always set by frame_transfer() before use below; the
                     * sentinel init only silences -Werror=maybe-uninitialized
                     * on GCC builds that can't see through the callee. */
                    int32_t out = FRAME_UNKNOWN;

                    work.pop_back();
                    if (!frame_transfer(b, frame_in[b->id], &out)) {
                        return false;
                    }

                    for_each_succ(b->terminator, [&](T2BasicBlock *succ) {
                        if (succ == nullptr) {
                            return;
                        }
                        if (frame_in[succ->id] == FRAME_UNKNOWN) {
                            frame_in[succ->id] = out;
                            work.push_back(succ);
                        } else if (frame_in[succ->id] != out &&
                                   frame_in[succ->id] != FRAME_CONFLICT) {
                            /* Frame-polymorphic (raise-only) block. */
                            frame_in[succ->id] = FRAME_CONFLICT;
                            work.push_back(succ);
                        }
                    });
                }

                /* --- per-block register-state walk --------------------- *
                 * Verifies that every sync-map entry names a value that is
                 * actually materialized in that BEAM register at that
                 * boundary (canonical-home discipline; hard error). State
                 * unknown at block entry is adopted on first mention, so
                 * the check is exact within a block and partial across
                 * blocks (phi homes seed the known part). */
                std::vector<std::unordered_map<int32_t, const T2Value *>>
                        exit_state(n);
                /* A result whose write is conditional on the block's
                 * Succeeded/Branch (gc_bif with a fail edge): T1 writes
                 * the destination only on the success path, so the write
                 * belongs to the then-edge, not the block exit. */
                struct PendingWrite {
                    int32_t reg = T2_REG_NONE;
                    const T2Value *val = nullptr;
                };
                std::vector<PendingWrite> then_write(n);

                for (const T2BasicBlock *b : fn.blocks) {
                    if (!reachable[b->id]) {
                        continue;
                    }

                    std::unordered_map<int32_t, const T2Value *> state;
                    int32_t frame = frame_in[b->id];

                    for (const T2Op *phi = b->phis_head; phi != nullptr;
                         phi = phi->next) {
                        if (phi->dst_reg == T2_REG_NONE) {
                            return fail("block %u: phi without a canonical "
                                        "home",
                                        b->id);
                        }
                        state[phi->dst_reg] = phi->result;
                    }

                    /* Reads-then-writes over one op (or an atomic pair). */
                    auto read_checks = [&](const T2Op *op) -> bool {
                        if (op->operand_regs == nullptr) {
                            return true;
                        }
                        for (uint16_t i = 0; i < op->num_operands; i++) {
                            int32_t r = op->operand_regs[i];

                            if (r == T2_REG_NONE) {
                                continue;
                            }
                            auto it = state.find(r);
                            if (it == state.end()) {
                                state[r] = op->operands[i];
                            } else if (it->second != op->operands[i]) {
                                return fail("block %u: op %s reads %c%u as v%u "
                                            "but the register holds v%u",
                                            b->id,
                                            t2_op_kind_name(op->kind),
                                            t2_reg_is_y(r) ? 'y' : 'x',
                                            t2_reg_index(r),
                                            op->operands[i]->id,
                                            it->second->id);
                            }
                        }
                        return true;
                    };

                    auto apply_frame_and_write = [&](const T2Op *op) {
                        switch (op->kind) {
                        case T2OpKind::Allocate:
                            frame = (int32_t)op->index;
                            /* Fresh frame: all Y slots are garbage. */
                            for (auto it = state.begin(); it != state.end();) {
                                if (t2_reg_is_y(it->first)) {
                                    it = state.erase(it);
                                } else {
                                    ++it;
                                }
                            }
                            break;
                        case T2OpKind::Deallocate:
                            frame = T2_NO_FRAME;
                            for (auto it = state.begin(); it != state.end();) {
                                if (t2_reg_is_y(it->first)) {
                                    it = state.erase(it);
                                } else {
                                    ++it;
                                }
                            }
                            break;
                        case T2OpKind::Trim: {
                            /* Post-trim Y_i = pre-trim Y_{i+N}. */
                            uint32_t drop = op->index;
                            std::unordered_map<int32_t, const T2Value *> ns;

                            frame = (int32_t)op->imm_int;

                            for (const auto &e : state) {
                                if (!t2_reg_is_y(e.first)) {
                                    ns.emplace(e);
                                } else {
                                    uint32_t idx = t2_reg_index(e.first);

                                    if (idx >= drop) {
                                        ns[t2_yreg(idx - drop)] = e.second;
                                    }
                                }
                            }
                            state = std::move(ns);
                            break;
                        }
                        default:
                            break;
                        }
                        if (op->dst_reg != T2_REG_NONE &&
                            op->result != nullptr) {
                            state[op->dst_reg] = op->result;
                        }
                    };

                    auto sync_checks = [&](const T2Op *op) -> bool {
                        const T2SyncMap *m = op->sync;

                        if (m == nullptr) {
                            return true;
                        }
                        if (m->frame_size != frame) {
                            return fail("block %u: %s sync map frame %d but "
                                        "walked frame is %d",
                                        b->id,
                                        t2_op_kind_name(op->kind),
                                        m->frame_size,
                                        frame);
                        }
                        for (uint32_t i = 0; i < m->x_live; i++) {
                            auto it = state.find(t2_xreg(i));

                            if (it == state.end()) {
                                state[t2_xreg(i)] = m->x[i];
                            } else if (it->second != m->x[i]) {
                                return fail("block %u: %s sync map says x%u "
                                            "= v%u but the register holds "
                                            "v%u (no canonical home)",
                                            b->id,
                                            t2_op_kind_name(op->kind),
                                            i,
                                            m->x[i]->id,
                                            it->second->id);
                            }
                        }
                        for (int32_t i = 0;
                             m->frame_size != T2_NO_FRAME && i < m->frame_size;
                             i++) {
                            auto it = state.find(t2_yreg((uint32_t)i));

                            if (it == state.end()) {
                                state[t2_yreg((uint32_t)i)] = m->y[i];
                            } else if (it->second != m->y[i]) {
                                return fail("block %u: %s sync map says y%d "
                                            "= v%u but the slot holds v%u "
                                            "(no canonical home)",
                                            b->id,
                                            t2_op_kind_name(op->kind),
                                            i,
                                            m->y[i]->id,
                                            it->second->id);
                            }
                        }
                        return true;
                    };

                    const T2Op *op = b->ops_head;
                    while (op != nullptr) {
                        const T2Op *pair_tail = nullptr;

                        if (op->flags & T2_OP_PAIR_HEAD) {
                            pair_tail = op->next;
                            if (pair_tail == nullptr ||
                                pair_tail->beam_idx != op->beam_idx) {
                                return fail("block %u: dangling pair-head op "
                                            "%s",
                                            b->id,
                                            t2_op_kind_name(op->kind));
                            }
                        }

                        if (!sync_checks(op) || !read_checks(op)) {
                            return false;
                        }
                        if (pair_tail != nullptr && !read_checks(pair_tail)) {
                            return false;
                        }

                        /* Conditional destination write: op feeds a
                         * Succeeded that the block's Branch consumes; the
                         * write only happens on the success edge. */
                        if (op->dst_reg != T2_REG_NONE &&
                            op->result != nullptr && op->next != nullptr &&
                            op->next->kind == T2OpKind::Succeeded &&
                            op->next->num_operands == 1 &&
                            op->next->operands[0] == op->result &&
                            b->terminator != nullptr &&
                            b->terminator->kind == T2OpKind::Branch &&
                            b->terminator->num_operands == 1 &&
                            b->terminator->operands[0] == op->next->result) {
                            then_write[b->id].reg = op->dst_reg;
                            then_write[b->id].val = op->result;
                            /* Frame effects (none for gc_bifs) would still
                             * apply; skip only the register write. */
                        } else {
                            apply_frame_and_write(op);
                        }

                        if (pair_tail != nullptr) {
                            apply_frame_and_write(pair_tail);
                            op = pair_tail->next;
                        } else {
                            op = op->next;
                        }
                    }

                    if (b->terminator != nullptr) {
                        if (!sync_checks(b->terminator) ||
                            !read_checks(b->terminator)) {
                            return false;
                        }
                    }

                    exit_state[b->id] = std::move(state);
                }

                /* --- phi merge-slot verification ------------------------ *
                 * Each phi input must sit in the phi's home register at
                 * the corresponding predecessor's exit (where known). */
                for (const T2BasicBlock *b : fn.blocks) {
                    if (!reachable[b->id]) {
                        continue;
                    }
                    for (const T2Op *phi = b->phis_head; phi != nullptr;
                         phi = phi->next) {
                        for (uint16_t i = 0; i < phi->num_operands; i++) {
                            const T2BasicBlock *pred = phi->phi_blocks[i];

                            if (!reachable[pred->id]) {
                                continue;
                            }

                            /* A conditional write on the pred applies only
                             * along its then-edge. */
                            const PendingWrite &pw = then_write[pred->id];
                            if (pw.reg == phi->dst_reg) {
                                const T2Op *t = pred->terminator;
                                bool is_then = t->succ_then == b;
                                bool is_else = t->succ_else == b;

                                if (is_then && !is_else) {
                                    if (pw.val != phi->operands[i]) {
                                        return fail("block %u: phi home holds "
                                                    "the conditional write v%u "
                                                    "on the then-edge from "
                                                    "block %u, expected v%u",
                                                    b->id,
                                                    pw.val->id,
                                                    pred->id,
                                                    phi->operands[i]->id);
                                    }
                                    continue;
                                }
                                if (is_then && is_else) {
                                    continue; /* ambiguous edge; skip */
                                }
                                /* else-edge: fall through to the base
                                 * (pre-write) state below. */
                            }

                            const auto &es = exit_state[pred->id];
                            auto it = es.find(phi->dst_reg);

                            if (it != es.end() &&
                                it->second != phi->operands[i]) {
                                return fail(
                                        "block %u: phi home %c%u holds v%u "
                                        "at exit of pred block %u, expected "
                                        "v%u",
                                        b->id,
                                        t2_reg_is_y(phi->dst_reg) ? 'y' : 'x',
                                        t2_reg_index(phi->dst_reg),
                                        it->second->id,
                                        pred->id,
                                        phi->operands[i]->id);
                            }
                        }
                    }
                }

                /* --- entry map ------------------------------------------ */
                if (fn.entry_sync == nullptr) {
                    return fail("sync_complete function without an entry "
                                "sync map");
                }
                if (fn.entry_sync->x_live != fn.arity ||
                    fn.entry_sync->frame_size != T2_NO_FRAME) {
                    return fail("entry sync map must be {x0..x%u, no frame}",
                                fn.arity);
                }

                /* Per-op structural map checks (presence + contents). */
                for (const T2BasicBlock *b : fn.blocks) {
                    for (const T2Op *op = b->ops_head; op != nullptr;
                         op = op->next) {
                        if (!check_sync_map(op)) {
                            return false;
                        }
                    }
                    if (b->terminator != nullptr &&
                        !check_sync_map(b->terminator)) {
                        return false;
                    }
                }

                return true;
            }

            /* -------------------------------------------------------- *
             * P2 commit 3: speculative-type discipline (corruption-     *
             * class invariant (ii), PLAN/T2FULL/09 §4; PLAN/T2/08 §4.4) *
             *                                                          *
             * Every value consumed by a speculative-class op must have  *
             * its type established by a dominating guard or proof on    *
             * EVERY path, including all phi edges (the AND, never OR,   *
             * fusion rule). The lattice has two facts:                  *
             *                                                          *
             *   SMALL — proven small integer: established by            *
             *     SpeculateType (the tag-bit deopt guard), by a small   *
             *     ConstInt, and by the committed result of a            *
             *     flag-checked AddSmall/SubSmall/TagInt (deopt fires    *
             *     before the commit); a phi is SMALL iff every input    *
             *     is SMALL at its edge — loop-header phis are exactly   *
             *     where the latch value's re-proof (the flag-checked    *
             *     op that produced it) is required.                     *
             *   RAW — an untagged machine word (UntagInt/MulRaw).       *
             *                                                          *
             * Consumers: UntagInt needs SMALL; TagInt needs RAW;        *
             * MulRaw needs RAW operands; AddSmall/SubSmall need each    *
             * operand SMALL or RAW (the one-untag mix). Facts flow      *
             * forward to a fixpoint (optimistic init, monotone          *
             * intersection at joins), then one exact checking pass.     *
             * Functions without speculative ops skip the dataflow.      *
             * -------------------------------------------------------- */

            static bool op_is_speculative(T2OpKind k) {
                switch (k) {
                case T2OpKind::UntagInt:
                case T2OpKind::TagInt:
                case T2OpKind::AddSmall:
                case T2OpKind::SubSmall:
                case T2OpKind::MulRaw:
                case T2OpKind::SpeculateType:
                case T2OpKind::SpeculateRange:
                    return true;
                default:
                    return false;
                }
            }

            struct SpecFacts {
                std::vector<uint64_t> small, raw;
            };

            static void sf_set(std::vector<uint64_t> &s, uint32_t v) {
                s[v / 64] |= uint64_t(1) << (v % 64);
            }
            static bool sf_test(const std::vector<uint64_t> &s, uint32_t v) {
                return (s[v / 64] >> (v % 64)) & 1;
            }

            /* Apply one op's fact effects to `f`. */
            static void spec_transfer_op(const T2Op *op, SpecFacts &f) {
                switch (op->kind) {
                case T2OpKind::ConstInt:
                    if ((op->flags & T2_OP_RAW_MODE) != 0) {
                        /* P2 loop unboxing: the constant materializes in
                         * the tag-cleared representation. */
                        sf_set(f.raw, op->result->id);
                    } else if (IS_SSMALL(op->imm_int)) {
                        sf_set(f.small, op->result->id);
                    }
                    break;
                case T2OpKind::Copy:
                    /* SSA-identity of its operand: facts pass through
                     * (the speculation pass sees through the decoded
                     * frame copies the same way). A RAW_MODE Copy is a
                     * raw move (P2 loop unboxing: the P1 latch
                     * materialization of a raw accumulator). */
                    if (sf_test(f.small, op->operands[0]->id) ||
                        t2_type_proves_small(op->operands[0]->type)) {
                        sf_set(f.small, op->result->id);
                    }
                    if (sf_test(f.raw, op->operands[0]->id)) {
                        sf_set(f.raw, op->result->id);
                    }
                    break;
                case T2OpKind::SpeculateType:
                    /* The tag-bit test: after it, every operand is a
                     * proven small (side exit otherwise). A fused guard
                     * (P2 commit 4, the AND-combining rule) carries
                     * several operands; the combined mask requires every
                     * input to satisfy every tag bit, so all of them are
                     * proven on fall-through. */
                    for (uint16_t i = 0; i < op->num_operands; i++) {
                        sf_set(f.small, op->operands[i]->id);
                    }
                    break;
                case T2OpKind::AddSmall:
                case T2OpKind::SubSmall:
                    /* Flag-checked commit: the result is small on the
                     * fall-through path (deopt before the commit) — or
                     * the tag-cleared RAW word in raw mode (the deopt
                     * condition is bit-identical; only the low tag
                     * nibble differs). */
                    if ((op->flags & T2_OP_RAW_MODE) != 0) {
                        sf_set(f.raw, op->result->id);
                    } else {
                        sf_set(f.small, op->result->id);
                    }
                    break;
                case T2OpKind::TagInt:
                    sf_set(f.small, op->result->id);
                    break;
                case T2OpKind::FlatmapSize:
                    if ((op->flags & T2_OP_RAW_MODE) != 0) {
                        sf_set(f.raw, op->result->id);
                    }
                    break;
                case T2OpKind::UntagInt:
                case T2OpKind::MulRaw:
                    sf_set(f.raw, op->result->id);
                    break;
                default:
                    break;
                }
            }

            bool spec_check_op(const T2Op *op, const SpecFacts &f) {
                auto need = [&](uint16_t i,
                                bool small_ok,
                                bool raw_ok,
                                const char *what) -> bool {
                    const T2Value *v = op->operands[i];
                    bool is_small = sf_test(f.small, v->id) ||
                                    t2_type_proves_small(v->type);
                    bool is_raw = sf_test(f.raw, v->id);

                    if ((small_ok && is_small) || (raw_ok && is_raw)) {
                        return true;
                    }
                    return fail("block %u: %s operand %u (v%u) is not %s "
                                "on every path (a dominating guard/proof "
                                "must hold on all edges, phi inputs "
                                "included — the AND-fusion rule)",
                                op->block->id,
                                t2_op_kind_name(op->kind),
                                i,
                                v->id,
                                what);
                };

                switch (op->kind) {
                case T2OpKind::UntagInt:
                    return need(0, true, false, "a proven small");
                case T2OpKind::TagInt:
                    return need(0, false, true, "an untagged word");
                case T2OpKind::MulRaw:
                    return need(0, false, true, "an untagged word") &&
                           need(1, false, true, "an untagged word");
                case T2OpKind::AddSmall:
                case T2OpKind::SubSmall:
                    return need(0, true, true, "a proven small/raw") &&
                           need(1, true, true, "a proven small/raw");
                default:
                    return true;
                }
            }

            /* -------------------------------------------------------- *
             * P2 loop unboxing: structural RAW-IN-HOME discipline.      *
             *                                                          *
             * Rawness is a property of a value's def (one               *
             * representation per SSA value), so the whole discipline    *
             * is checkable structurally, without dataflow:              *
             *                                                          *
             *   - T2_OP_RAW_MODE may only mark the known producer /    *
             *     raw-consumer kinds; raw producers must be X-homed    *
             *     (a raw word in a Y slot would be walked as a term    *
             *     by the stack scanner).                               *
             *   - Every consumer of a raw value must be raw-aware:     *
             *     a raw word reaching a generic op (which treats its   *
             *     operand as a term) is corruption, never silent.      *
             *   - Sync-map mentions are covered by the raw_mask rule   *
             *     in check_sync_map.                                   *
             * -------------------------------------------------------- */

            bool raw_check_op(const T2Op *op) {
                bool raw_mode = (op->flags & T2_OP_RAW_MODE) != 0;
                auto is_raw = [](const T2Value *v) {
                    return t2_value_is_raw_home(v);
                };
                auto no_raw_operands = [&](const char *why) -> bool {
                    for (uint16_t i = 0; i < op->num_operands; i++) {
                        if (is_raw(op->operands[i])) {
                            return fail("block %u: %s operand %u (v%u) is a "
                                        "raw-in-home word (%s)",
                                        op->block->id,
                                        t2_op_kind_name(op->kind),
                                        i,
                                        op->operands[i]->id,
                                        why);
                        }
                    }
                    return true;
                };

                if (raw_mode) {
                    switch (op->kind) {
                    case T2OpKind::Phi:
                    case T2OpKind::Copy: /* raw move */
                    case T2OpKind::AddSmall:
                    case T2OpKind::SubSmall:
                    case T2OpKind::ConstInt:
                    case T2OpKind::FlatmapSize:
                    case T2OpKind::UntagInt:
                    case T2OpKind::CmpLt:
                    case T2OpKind::FlatmapKeyAt:
                    case T2OpKind::FlatmapValAt:
                        break;
                    default:
                        return fail("block %u: T2_OP_RAW_MODE on %s (not a "
                                    "raw-aware kind)",
                                    op->block->id,
                                    t2_op_kind_name(op->kind));
                    }
                    if (op->result != nullptr && op->kind != T2OpKind::CmpLt &&
                        op->kind != T2OpKind::FlatmapKeyAt &&
                        op->kind != T2OpKind::FlatmapValAt &&
                        (op->dst_reg == T2_REG_NONE ||
                         !t2_reg_is_x(op->dst_reg))) {
                        return fail("block %u: raw producer %s is not "
                                    "X-homed",
                                    op->block->id,
                                    t2_op_kind_name(op->kind));
                    }
                    if (op->kind == T2OpKind::ConstInt &&
                        !IS_SSMALL(op->imm_int)) {
                        return fail("block %u: raw ConstInt is not a small",
                                    op->block->id);
                    }
                }

                switch (op->kind) {
                case T2OpKind::Phi:
                    for (uint16_t i = 0; i < op->num_operands; i++) {
                        if (is_raw(op->operands[i]) != raw_mode) {
                            return fail("block %u: phi input %u (v%u) is %s "
                                        "but the phi is %s",
                                        op->block->id,
                                        i,
                                        op->operands[i]->id,
                                        is_raw(op->operands[i]) ? "raw"
                                                                : "tagged",
                                        raw_mode ? "raw" : "tagged");
                        }
                    }
                    return true;
                case T2OpKind::AddSmall:
                case T2OpKind::SubSmall:
                    /* Raw mode clears every tagged operand and commits
                     * raw; tagged mode relies on the tag riding the
                     * add, so no raw operand may reach it. */
                    if (!raw_mode) {
                        return no_raw_operands("tagged flag-checked "
                                               "arithmetic");
                    }
                    return true;
                case T2OpKind::TagInt:
                    if (op->num_operands != 1 || !is_raw(op->operands[0])) {
                        return fail("block %u: tag_int operand is not a "
                                    "raw-in-home word",
                                    op->block->id);
                    }
                    return true;
                case T2OpKind::UntagInt:
                    return no_raw_operands("untag of a raw word");
                case T2OpKind::Copy:
                    /* A raw move passes the raw word through (the LIR
                     * Move is representation-agnostic); a plain Copy
                     * must never consume one. */
                    if (raw_mode) {
                        if (op->num_operands != 1 || !is_raw(op->operands[0])) {
                            return fail("block %u: raw copy of a non-raw "
                                        "value",
                                        op->block->id);
                        }
                        return true;
                    }
                    return no_raw_operands("plain register copy");
                case T2OpKind::CmpLt:
                    if (raw_mode) {
                        if (op->num_operands != 2 || !is_raw(op->operands[0]) ||
                            !is_raw(op->operands[1])) {
                            return fail("block %u: raw cmp_lt needs two "
                                        "raw operands",
                                        op->block->id);
                        }
                        return true;
                    }
                    return no_raw_operands("generic comparison");
                case T2OpKind::FlatmapKeyAt:
                case T2OpKind::FlatmapValAt:
                    if (is_raw(op->operands[0])) {
                        return fail("block %u: %s map operand is raw",
                                    op->block->id,
                                    t2_op_kind_name(op->kind));
                    }
                    if (is_raw(op->operands[1]) != raw_mode) {
                        return fail("block %u: %s index rawness does not "
                                    "match the op's raw mode",
                                    op->block->id,
                                    t2_op_kind_name(op->kind));
                    }
                    return true;
                case T2OpKind::FoldBudget:
                    /* The batch charge untags its operand with a plain
                     * shift, which reads the same value from the tagged
                     * and the tag-cleared forms — both admissible. */
                    return true;
                case T2OpKind::Call:
                case T2OpKind::CallExt:
                case T2OpKind::TailCall:
                case T2OpKind::TailCallExt:
                    /* A re-dispatch over the loop-carried vector (P1
                     * fallback mode) consumes raw homes as real call
                     * arguments; the emitter re-tags them in place
                     * before the transfer, driven by raw_mask. Require
                     * the declaration for every raw argument. */
                    for (uint16_t i = 0; i < op->num_operands; i++) {
                        if (!is_raw(op->operands[i])) {
                            continue;
                        }
                        if (op->operand_regs == nullptr ||
                            !t2_reg_is_x(op->operand_regs[i]) ||
                            t2_reg_index(op->operand_regs[i]) >= 32 ||
                            (op->raw_mask &
                             ((uint32_t)1
                              << t2_reg_index(op->operand_regs[i]))) == 0) {
                            return fail("block %u: %s argument %u (v%u) is "
                                        "raw but not declared in the re-tag "
                                        "mask",
                                        op->block->id,
                                        t2_op_kind_name(op->kind),
                                        i,
                                        op->operands[i]->id);
                        }
                    }
                    return true;
                default:
                    return no_raw_operands("not a raw-aware consumer");
                }
            }

            bool run_raw_checks() {
                bool any = false;

                for (const T2BasicBlock *b : fn.blocks) {
                    for (const T2Op *op = b->ops_head; op != nullptr && !any;
                         op = op->next) {
                        any = (op->flags & T2_OP_RAW_MODE) != 0;
                    }
                    for (const T2Op *phi = b->phis_head; phi != nullptr && !any;
                         phi = phi->next) {
                        any = (phi->flags & T2_OP_RAW_MODE) != 0;
                    }
                    if (any) {
                        break;
                    }
                }
                if (!any) {
                    return true; /* the common case costs one scan */
                }

                for (const T2BasicBlock *b : fn.blocks) {
                    for (const T2Op *phi = b->phis_head; phi != nullptr;
                         phi = phi->next) {
                        if (!raw_check_op(phi)) {
                            return false;
                        }
                    }
                    for (const T2Op *op = b->ops_head; op != nullptr;
                         op = op->next) {
                        if (!raw_check_op(op)) {
                            return false;
                        }
                    }
                    if (b->terminator != nullptr &&
                        !raw_check_op(b->terminator)) {
                        return false;
                    }
                }
                return true;
            }

            /* -------------------------------------------------------- *
             * P3: every claimed T2_OP_NO_OVF (the emitter omits the    *
             * overflow deopt) is RE-PROVEN by the shared range prover  *
             * (t2_addsub_no_ovf_provable, t2_opt.cpp) — a flag the     *
             * prover cannot re-establish is a hard error, so a         *
             * silently missing overflow guard cannot install.          *
             * -------------------------------------------------------- */
            bool run_no_ovf_checks() {
                for (const T2BasicBlock *b : fn.blocks) {
                    for (const T2Op *op = b->ops_head; op != nullptr;
                         op = op->next) {
                        if ((op->flags & T2_OP_NO_OVF) == 0) {
                            continue;
                        }
                        if (op->kind != T2OpKind::AddSmall) {
                            return fail("block %u: T2_OP_NO_OVF on %s "
                                        "(only AddSmall is provable)",
                                        b->id,
                                        t2_op_kind_name(op->kind));
                        }
                        if (!t2_addsub_no_ovf_provable(fn, op)) {
                            return fail("block %u: T2_OP_NO_OVF claim on "
                                        "add_small (v%u) is not provable",
                                        b->id,
                                        op->result != nullptr ? op->result->id
                                                              : 0);
                        }
                    }
                }
                return true;
            }

            bool run_speculation_checks() {
                bool any = false;

                for (const T2BasicBlock *b : fn.blocks) {
                    for (const T2Op *op = b->ops_head; op != nullptr && !any;
                         op = op->next) {
                        any = op_is_speculative(op->kind);
                    }
                    if (any) {
                        break;
                    }
                }
                if (!any) {
                    return true; /* the common case costs one scan */
                }

                size_t n = fn.blocks.size();
                size_t vwords = (fn.values.size() + 63) / 64;
                std::vector<SpecFacts> out(n);

                /* Optimistic init: everything proven, then intersect
                 * down to the fixpoint (entry gets no incoming facts). */
                for (size_t i = 0; i < n; i++) {
                    bool top = reachable[i] && i != 0;

                    out[i].small.assign(vwords, top ? ~uint64_t(0) : 0);
                    out[i].raw.assign(vwords, top ? ~uint64_t(0) : 0);
                }

                auto block_in = [&](const T2BasicBlock *b) {
                    SpecFacts in;

                    in.small.assign(vwords, b->id == 0 ? 0 : ~uint64_t(0));
                    in.raw.assign(vwords, b->id == 0 ? 0 : ~uint64_t(0));
                    if (b->id == 0) {
                        return in;
                    }

                    bool any_pred = false;
                    for (uint32_t p = 0; p < b->num_preds; p++) {
                        const T2BasicBlock *pred = b->preds[p];

                        if (!reachable[pred->id]) {
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

                auto transfer = [&](const T2BasicBlock *b, SpecFacts f) {
                    /* Phi facts: AND over the incoming edges. */
                    for (const T2Op *phi = b->phis_head; phi != nullptr;
                         phi = phi->next) {
                        bool all_small = phi->num_operands > 0;
                        bool all_raw = phi->num_operands > 0;

                        for (uint16_t i = 0; i < phi->num_operands; i++) {
                            const T2BasicBlock *pred = phi->phi_blocks[i];

                            if (!reachable[pred->id]) {
                                continue;
                            }
                            uint32_t vid = phi->operands[i]->id;

                            all_small &= sf_test(out[pred->id].small, vid) ||
                                         t2_type_proves_small(
                                                 phi->operands[i]->type);
                            all_raw &= sf_test(out[pred->id].raw, vid);
                        }
                        if (all_small) {
                            sf_set(f.small, phi->result->id);
                        }
                        if (all_raw) {
                            sf_set(f.raw, phi->result->id);
                        }
                    }
                    for (const T2Op *op = b->ops_head; op != nullptr;
                         op = op->next) {
                        spec_transfer_op(op, f);
                    }
                    return f;
                };

                bool changed = true;
                while (changed) {
                    changed = false;
                    for (const T2BasicBlock *b : fn.blocks) {
                        if (!reachable[b->id]) {
                            continue;
                        }
                        SpecFacts nf = transfer(b, block_in(b));

                        if (nf.small != out[b->id].small ||
                            nf.raw != out[b->id].raw) {
                            out[b->id] = std::move(nf);
                            changed = true;
                        }
                    }
                }

                /* Exact checking pass at the fixpoint. */
                for (const T2BasicBlock *b : fn.blocks) {
                    if (!reachable[b->id]) {
                        continue;
                    }
                    SpecFacts f = block_in(b);

                    for (const T2Op *phi = b->phis_head; phi != nullptr;
                         phi = phi->next) {
                        bool all_small = phi->num_operands > 0;
                        bool all_raw = phi->num_operands > 0;

                        for (uint16_t i = 0; i < phi->num_operands; i++) {
                            const T2BasicBlock *pred = phi->phi_blocks[i];

                            if (!reachable[pred->id]) {
                                continue;
                            }
                            uint32_t vid = phi->operands[i]->id;

                            all_small &= sf_test(out[pred->id].small, vid) ||
                                         t2_type_proves_small(
                                                 phi->operands[i]->type);
                            all_raw &= sf_test(out[pred->id].raw, vid);
                        }
                        if (all_small) {
                            sf_set(f.small, phi->result->id);
                        }
                        if (all_raw) {
                            sf_set(f.raw, phi->result->id);
                        }
                    }
                    for (const T2Op *op = b->ops_head; op != nullptr;
                         op = op->next) {
                        if (!spec_check_op(op, f)) {
                            return false;
                        }
                        spec_transfer_op(op, f);
                    }
                }

                return true;
            }

            bool run() {
                if (fn.blocks.empty()) {
                    return fail("function has no blocks");
                }

                for (size_t i = 0; i < fn.blocks.size(); i++) {
                    if (fn.blocks[i] == nullptr || fn.blocks[i]->id != i) {
                        return fail("block table corrupt at index %u",
                                    (unsigned)i);
                    }
                }

                for (size_t i = 0; i < fn.values.size(); i++) {
                    if (fn.values[i] == nullptr || fn.values[i]->id != i) {
                        return fail("value table corrupt at index %u",
                                    (unsigned)i);
                    }
                }

                compute_dominators();

                /* Number ops within blocks and run the structural checks. */
                for (const T2BasicBlock *b : fn.blocks) {
                    for (const T2Op *phi = b->phis_head; phi != nullptr;
                         phi = phi->next) {
                        pos[phi] = 0;
                        if (!check_phi(phi, b) || !check_op_common(phi, b)) {
                            return false;
                        }
                    }

                    uint32_t idx = 1;
                    for (const T2Op *op = b->ops_head; op != nullptr;
                         op = op->next) {
                        pos[op] = idx++;
                        if (t2_op_is_terminator(op->kind) ||
                            op->kind == T2OpKind::Phi) {
                            return fail("block %u: %s in op body list",
                                        b->id,
                                        t2_op_kind_name(op->kind));
                        }
                        if (!check_op_common(op, b)) {
                            return false;
                        }
                    }

                    if (!check_terminator(b)) {
                        return false;
                    }
                    pos[b->terminator] = UINT32_MAX;
                }

                /* SSA dominance checks. */
                for (const T2BasicBlock *b : fn.blocks) {
                    for (const T2Op *phi = b->phis_head; phi != nullptr;
                         phi = phi->next) {
                        for (uint16_t i = 0; i < phi->num_operands; i++) {
                            if (!check_use(phi, i)) {
                                return false;
                            }
                        }
                    }
                    for (const T2Op *op = b->ops_head; op != nullptr;
                         op = op->next) {
                        for (uint16_t i = 0; i < op->num_operands; i++) {
                            if (!check_use(op, i)) {
                                return false;
                            }
                        }
                    }
                    for (uint16_t i = 0; i < b->terminator->num_operands; i++) {
                        if (!check_use(b->terminator, i)) {
                            return false;
                        }
                    }
                }

                /* P1: sync-map / frame / canonical-home coherence. */
                if (fn.sync_complete && !run_sync_checks()) {
                    return false;
                }

                /* P2: speculative-type discipline (invariant ii). Runs
                 * for hand-built functions too — the speculation pass
                 * must be provable before any relaxed blob installs. */
                if (!run_speculation_checks()) {
                    return false;
                }

                /* P2 loop unboxing: RAW-IN-HOME discipline (structural;
                 * sync-map mentions are covered by the raw_mask rule
                 * inside run_sync_checks). */
                if (!run_raw_checks()) {
                    return false;
                }

                /* P3: re-prove every claimed elided overflow guard. */
                if (!run_no_ovf_checks()) {
                    return false;
                }

                return true;
            }
        };

    } /* anonymous namespace */

    bool t2_validate(const T2Function &fn, std::string *err) {
        Validator v(fn, err);
        return v.run();
    }

    bool t2_type_proves_small(const T2Type &t) {
        return t.integer_only() && t.has_min && t.has_max && IS_SSMALL(t.min) &&
               IS_SSMALL(t.max);
    }

    bool t2_value_is_raw_home(const T2Value *v) {
        if (v->def == nullptr || (v->def->flags & T2_OP_RAW_MODE) == 0 ||
            v->def->result != v) {
            return false;
        }
        switch (v->def->kind) {
        case T2OpKind::CmpLt:        /* boolean result           */
        case T2OpKind::FlatmapKeyAt: /* tagged term result — the */
        case T2OpKind::FlatmapValAt: /* flag marks the raw index */
            return false;
        default:
            return true;
        }
    }

    /* ------------------------------------------------------------------ *
     * Dump                                                               *
     * ------------------------------------------------------------------ */

    static void dump_reg(std::string &out, int32_t reg) {
        char buf[32];

        if (reg == T2_REG_NONE) {
            return;
        }
        snprintf(buf,
                 sizeof(buf),
                 "%c%u",
                 t2_reg_is_y(reg) ? 'y' : 'x',
                 t2_reg_index(reg));
        out += buf;
    }

    static void dump_sync_map(std::string &out, const T2SyncMap *m) {
        char buf[64];

        out += " sync={x:[";
        for (uint32_t i = 0; i < m->x_live; i++) {
            snprintf(buf, sizeof(buf), "%sv%u", i == 0 ? "" : ",", m->x[i]->id);
            out += buf;
        }
        out += "]";
        if (m->frame_size != T2_NO_FRAME) {
            out += " y:[";
            for (int32_t i = 0; i < m->frame_size; i++) {
                snprintf(buf,
                         sizeof(buf),
                         "%sv%u",
                         i == 0 ? "" : ",",
                         m->y[i]->id);
                out += buf;
            }
            snprintf(buf, sizeof(buf), "] frame:%d", m->frame_size);
            out += buf;
        } else {
            out += " frame:none";
        }
        out += "}";
    }

    static void dump_op(std::string &out, const T2Op *op) {
        char buf[192];

        out += "    ";
        if (op->result != nullptr) {
            snprintf(buf, sizeof(buf), "v%u", op->result->id);
            out += buf;
            if (op->dst_reg != T2_REG_NONE) {
                out += "@";
                dump_reg(out, op->dst_reg);
            }
            out += " = ";
        }
        out += t2_op_kind_name(op->kind);

        switch (op->kind) {
        case T2OpKind::ConstInt:
            snprintf(buf, sizeof(buf), " %lld", (long long)op->imm_int);
            out += buf;
            break;
        case T2OpKind::Allocate:
            snprintf(buf,
                     sizeof(buf),
                     " slots=%u heap=%lld live=%u",
                     op->index,
                     (long long)op->imm_int,
                     op->live);
            out += buf;
            break;
        case T2OpKind::Deallocate:
            snprintf(buf, sizeof(buf), " slots=%u", op->index);
            out += buf;
            break;
        case T2OpKind::Trim:
            snprintf(buf,
                     sizeof(buf),
                     " drop=%u remaining=%lld",
                     op->index,
                     (long long)op->imm_int);
            out += buf;
            break;
        case T2OpKind::GcTest:
            snprintf(buf,
                     sizeof(buf),
                     " words=%u live=%u",
                     op->index,
                     op->live);
            out += buf;
            break;
        case T2OpKind::ConstAtom:
            erts_snprintf(buf, sizeof(buf), " %T", op->imm_term);
            out += buf;
            break;
        case T2OpKind::ConstLiteral:
        case T2OpKind::Param:
        case T2OpKind::GetTupleElement:
        case T2OpKind::IsFunction:
        case T2OpKind::IsTaggedTuple:
        case T2OpKind::TestArity:
        case T2OpKind::MakeFun:
            snprintf(buf, sizeof(buf), " #%u", op->index);
            out += buf;
            break;
        case T2OpKind::Call:
        case T2OpKind::CallExt:
        case T2OpKind::TailCall:
        case T2OpKind::TailCallExt:
            erts_snprintf(buf,
                          sizeof(buf),
                          " %T:%T/%u",
                          op->mfa_m,
                          op->mfa_f,
                          op->index);
            out += buf;
            break;
        default:
            break;
        }

        for (uint16_t i = 0; i < op->num_operands; i++) {
            snprintf(buf,
                     sizeof(buf),
                     "%s v%u",
                     i == 0 ? "" : ",",
                     op->operands[i]->id);
            out += buf;
            if (op->operand_regs != nullptr &&
                op->operand_regs[i] != T2_REG_NONE) {
                out += "@";
                dump_reg(out, op->operand_regs[i]);
            }
            if (op->kind == T2OpKind::Phi && op->phi_blocks != nullptr) {
                snprintf(buf, sizeof(buf), ":block%u", op->phi_blocks[i]->id);
                out += buf;
            }
        }

        switch (op->kind) {
        case T2OpKind::Branch:
            snprintf(buf,
                     sizeof(buf),
                     ", then: block%u, else: block%u",
                     op->succ_then->id,
                     op->succ_else->id);
            out += buf;
            break;
        case T2OpKind::Jump:
            snprintf(buf, sizeof(buf), " block%u", op->succ_then->id);
            out += buf;
            break;
        case T2OpKind::Switch:
            for (uint32_t i = 0; i < op->num_cases; i++) {
                erts_snprintf(buf,
                              sizeof(buf),
                              ", %T: block%u",
                              op->cases[i].value,
                              op->cases[i].target->id);
                out += buf;
            }
            snprintf(buf,
                     sizeof(buf),
                     ", default: block%u",
                     op->default_target->id);
            out += buf;
            break;
        default:
            break;
        }

        if (op->sync != nullptr) {
            dump_sync_map(out, op->sync);
        }
        if (op->flags & T2_OP_ERR_EXIT_OP) {
            out += " !err_exit";
        }
        if (op->flags & T2_OP_ERR_EXIT_SHARED) {
            out += " !err_exit_shared";
        }
        if (op->flags & T2_OP_RAW_MODE) {
            out += " !raw";
        }
        if (op->raw_mask != 0) {
            snprintf(buf, sizeof(buf), " !retag=0x%x", op->raw_mask);
            out += buf;
        }

        out += "\n";
    }

    std::string t2_dump(const T2Function &fn) {
        std::string out;
        char buf[192];

        erts_snprintf(buf,
                      sizeof(buf),
                      "function %T:%T/%u  blocks=%u values=%u\n",
                      fn.module,
                      fn.function,
                      fn.arity,
                      (unsigned)fn.blocks.size(),
                      (unsigned)fn.values.size());
        out += buf;

        if (fn.entry_sync != nullptr) {
            out += "  entry";
            dump_sync_map(out, fn.entry_sync);
            out += "\n";
        }

        for (const T2BasicBlock *b : fn.blocks) {
            snprintf(buf, sizeof(buf), "  block%u:", b->id);
            out += buf;
            if (b->num_preds > 0) {
                out += " preds=[";
                for (uint32_t i = 0; i < b->num_preds; i++) {
                    snprintf(buf,
                             sizeof(buf),
                             "%sblock%u",
                             i == 0 ? "" : ",",
                             b->preds[i]->id);
                    out += buf;
                }
                out += "]";
            }
            out += "\n";

            for (const T2Op *phi = b->phis_head; phi != nullptr;
                 phi = phi->next) {
                dump_op(out, phi);
            }
            for (const T2Op *op = b->ops_head; op != nullptr; op = op->next) {
                dump_op(out, op);
            }
            if (b->terminator != nullptr) {
                dump_op(out, b->terminator);
            }
        }

        return out;
    }

    /* ------------------------------------------------------------------ *
     * Self-test                                                          *
     * ------------------------------------------------------------------ */

#define T2_CHECK(Cond)                                                         \
    do {                                                                       \
        if (!(Cond)) {                                                         \
            erts_fprintf(stderr,                                               \
                         "t2 selftest failure at %s:%d: %s\n",                 \
                         __FILE__,                                             \
                         __LINE__,                                             \
                         #Cond);                                               \
            return __LINE__;                                                   \
        }                                                                      \
    } while (0)

    static int selftest_lattice(void) {
        T2Type any = T2Type::any();
        T2Type none = T2Type::none();
        T2Type i_1_10 = T2Type::integer(1, 10);
        T2Type i_5_20 = T2Type::integer(5, 20);
        T2Type atom = T2Type::of(BEAM_TYPE_ATOM);

        /* meet/join against top and bottom */
        T2_CHECK(any.meet(i_1_10).equals(i_1_10));
        T2_CHECK(none.meet(i_1_10).is_none());
        T2_CHECK(none.join(i_1_10).equals(i_1_10));
        T2_CHECK(any.join(i_1_10).type_union == BEAM_TYPE_ANY);

        /* range intersection / union */
        {
            T2Type m = i_1_10.meet(i_5_20);
            T2Type j = i_1_10.join(i_5_20);

            T2_CHECK(m.integer_only() && m.has_min && m.min == 5 && m.has_max &&
                     m.max == 10);
            T2_CHECK(j.integer_only() && j.has_min && j.min == 1 && j.has_max &&
                     j.max == 20);
        }

        /* disjoint types meet to none; empty ranges collapse */
        T2_CHECK(atom.meet(i_1_10).is_none());
        T2_CHECK(T2Type::integer(11, 12).meet(i_1_10).is_none());

        /* joining discards one-sided bounds */
        {
            T2Type ji = i_1_10.join(T2Type::of(BEAM_TYPE_INTEGER));

            T2_CHECK(ji.integer_only() && !ji.has_min && !ji.has_max);
        }

        /* seeding from a decoded type-chunk entry */
        {
            BeamType bt;

            bt.type_union = BEAM_TYPE_INTEGER;
            bt.metadata_flags =
                    BEAM_TYPE_HAS_LOWER_BOUND | BEAM_TYPE_HAS_UPPER_BOUND;
            bt.min = -3;
            bt.max = 17;
            bt.size_unit = 0;

            T2Type t = T2Type::from_beam(bt);
            T2_CHECK(t.integer_only() && t.min == -3 && t.max == 17);
        }

        return 0;
    }

    static int selftest_straight_line(void) {
        T2Function fn;
        std::string err;

        fn.module = am_erlang;
        fn.function = am_ok;
        fn.arity = 2;

        T2BasicBlock *b0 = fn.new_block();
        T2Value *p0 = fn.emit_param(b0, 0, T2Type::any());
        T2Value *p1 = fn.emit_param(b0, 1, T2Type::any());
        T2Value *sum =
                fn.emit_binary(b0,
                               T2OpKind::Add,
                               p0,
                               p1,
                               T2Type::of(BEAM_TYPE_INTEGER | BEAM_TYPE_FLOAT));
        fn.emit_return(b0, sum);

        fn.finalize();
        if (!t2_validate(fn, &err)) {
            erts_fprintf(stderr,
                         "t2 selftest: straight-line: %s\n",
                         err.c_str());
            return __LINE__;
        }

        T2_CHECK(fn.blocks.size() == 1);
        T2_CHECK(fn.values.size() == 3);
        T2_CHECK(!t2_dump(fn).empty());

        return 0;
    }

    static int selftest_diamond_phi(void) {
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
        T2Value *cond = fn.emit_unary(b0,
                                      T2OpKind::IsInteger,
                                      p0,
                                      T2Type::of(BEAM_TYPE_ATOM));
        fn.emit_branch(b0, cond, b1, b2);

        T2Value *v1 = fn.emit_const_int(b1, 1);
        fn.emit_jump(b1, b3);

        T2Value *v2 = fn.emit_const_int(b2, 2);
        fn.emit_jump(b2, b3);

        T2Op *phi = fn.new_phi(b3, T2Type::integer(1, 2));
        fn.set_phi_inputs(phi, {v1, v2}, {b1, b2});
        fn.emit_return(b3, phi->result);

        fn.finalize();
        if (!t2_validate(fn, &err)) {
            erts_fprintf(stderr, "t2 selftest: diamond: %s\n", err.c_str());
            return __LINE__;
        }

        {
            std::string text = t2_dump(fn);

            T2_CHECK(text.find("phi") != std::string::npos);
            T2_CHECK(text.find("branch") != std::string::npos);
        }

        return 0;
    }

    /* P2 commit 3: the speculative-type discipline (validator
     * invariant ii). */
    static int selftest_speculation(void) {
        std::string err;

        /* Guarded straight line: SpeculateType proves the param small,
         * so untag/retag validate. */
        {
            T2Function fn;

            fn.module = am_erlang;
            fn.function = am_ok;
            fn.arity = 1;

            T2BasicBlock *b0 = fn.new_block();
            T2Value *p0 = fn.emit_param(b0, 0, T2Type::any());
            T2Op *spec = fn.new_op(b0, T2OpKind::SpeculateType, T2Type::none());

            fn.set_operands(spec, {p0});

            T2Value *u =
                    fn.emit_unary(b0, T2OpKind::UntagInt, p0, T2Type::any());
            T2Value *t = fn.emit_unary(b0, T2OpKind::TagInt, u, T2Type::any());

            fn.emit_return(b0, t);
            fn.finalize();
            if (!t2_validate(fn, &err)) {
                erts_fprintf(stderr,
                             "t2 selftest: speculation guarded: %s\n",
                             err.c_str());
                return __LINE__;
            }
        }

        /* The same without the guard must be rejected. */
        {
            T2Function fn;

            fn.module = am_erlang;
            fn.function = am_ok;
            fn.arity = 1;

            T2BasicBlock *b0 = fn.new_block();
            T2Value *p0 = fn.emit_param(b0, 0, T2Type::any());
            T2Value *u =
                    fn.emit_unary(b0, T2OpKind::UntagInt, p0, T2Type::any());
            T2Value *t = fn.emit_unary(b0, T2OpKind::TagInt, u, T2Type::any());

            fn.emit_return(b0, t);
            fn.finalize();
            T2_CHECK(!t2_validate(fn, &err));
        }

        /* The phi AND rule: a guard on one incoming edge only must be
         * rejected; guards on both edges pass. */
        for (int guarded_both = 0; guarded_both <= 1; guarded_both++) {
            T2Function fn;

            fn.module = am_erlang;
            fn.function = am_ok;
            fn.arity = 1;

            T2BasicBlock *b0 = fn.new_block();
            T2BasicBlock *b1 = fn.new_block();
            T2BasicBlock *b2 = fn.new_block();
            T2BasicBlock *b3 = fn.new_block();

            T2Value *p0 = fn.emit_param(b0, 0, T2Type::any());
            T2Value *cond = fn.emit_unary(b0,
                                          T2OpKind::IsInteger,
                                          p0,
                                          T2Type::of(BEAM_TYPE_ATOM));
            fn.emit_branch(b0, cond, b1, b2);

            {
                T2Op *spec =
                        fn.new_op(b1, T2OpKind::SpeculateType, T2Type::none());
                fn.set_operands(spec, {p0});
            }
            fn.emit_jump(b1, b3);

            if (guarded_both) {
                T2Op *spec =
                        fn.new_op(b2, T2OpKind::SpeculateType, T2Type::none());
                fn.set_operands(spec, {p0});
            }
            fn.emit_jump(b2, b3);

            /* Both phi inputs are p0; what differs per edge is whether
             * the fact was established on that path. */
            T2Op *phi = fn.new_phi(b3, T2Type::any());
            fn.set_phi_inputs(phi, {p0, p0}, {b1, b2});

            T2Value *u = fn.emit_unary(b3,
                                       T2OpKind::UntagInt,
                                       phi->result,
                                       T2Type::any());
            T2Value *t = fn.emit_unary(b3, T2OpKind::TagInt, u, T2Type::any());
            fn.emit_return(b3, t);
            fn.finalize();

            if (guarded_both) {
                if (!t2_validate(fn, &err)) {
                    erts_fprintf(stderr,
                                 "t2 selftest: speculation both-edges: "
                                 "%s\n",
                                 err.c_str());
                    return __LINE__;
                }
            } else {
                T2_CHECK(!t2_validate(fn, &err));
            }
        }

        return 0;
    }

    static int selftest_negative(void) {
        std::string err;

        /* A missing terminator must be rejected. */
        {
            T2Function fn;

            fn.module = am_erlang;
            fn.function = am_ok;
            fn.arity = 0;

            T2BasicBlock *b0 = fn.new_block();
            fn.emit_const_int(b0, 42);

            fn.finalize();
            T2_CHECK(!t2_validate(fn, &err));
        }

        /* A use whose def does not dominate it must be rejected. */
        {
            T2Function fn;

            fn.module = am_erlang;
            fn.function = am_ok;
            fn.arity = 1;

            T2BasicBlock *b0 = fn.new_block();
            T2BasicBlock *b1 = fn.new_block();
            T2BasicBlock *b2 = fn.new_block();
            T2BasicBlock *b3 = fn.new_block();

            T2Value *p0 = fn.emit_param(b0, 0, T2Type::any());
            fn.emit_branch(b0, p0, b1, b2);

            T2Value *x = fn.emit_const_int(b1, 1);
            fn.emit_jump(b1, b3);

            /* Uses x, but b1 does not dominate b2. */
            fn.emit_binary(b2, T2OpKind::Add, x, x, T2Type::any());
            fn.emit_jump(b2, b3);

            fn.emit_return(b3, p0);

            fn.finalize();
            T2_CHECK(!t2_validate(fn, &err));
        }

        return 0;
    }

} /* namespace erts_t2 */

using namespace erts_t2;

extern "C" int erts_t2_selftest_enabled(void) {
    static const int enabled = []() {
        const char *env = getenv("T2_SELFTEST");
        return (env != nullptr && env[0] == '1') ? 1 : 0;
    }();
    return enabled;
}

extern "C" int erts_t2_hir_selftest(void) {
    int res;

    if ((res = selftest_lattice()) != 0) {
        return res;
    }
    if ((res = selftest_straight_line()) != 0) {
        return res;
    }
    if ((res = selftest_diamond_phi()) != 0) {
        return res;
    }
    if ((res = selftest_negative()) != 0) {
        return res;
    }
    if ((res = selftest_speculation()) != 0) {
        return res;
    }

    return 0;
}
