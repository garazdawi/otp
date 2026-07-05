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
}

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
        case T2OpKind::ScheduleOut:
            return "schedule_out";
        case T2OpKind::FrameState:
            return "framestate";
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
        /* Reset and count predecessors. */
        for (T2BasicBlock *b : blocks) {
            b->num_preds = 0;
            b->preds = nullptr;
        }

        for (T2BasicBlock *b : blocks) {
            for_each_succ(b->terminator, [&](T2BasicBlock *succ) {
                if (succ != nullptr) {
                    succ->num_preds++;
                }
            });
        }

        for (T2BasicBlock *b : blocks) {
            b->preds = arena.alloc_array<T2BasicBlock *>(b->num_preds);
            b->num_preds = 0; /* reused as a fill cursor below */
        }

        for (T2BasicBlock *b : blocks) {
            for_each_succ(b->terminator, [&](T2BasicBlock *succ) {
                if (succ != nullptr) {
                    succ->preds[succ->num_preds++] = b;
                }
            });
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

                return true;
            }
        };

    } /* anonymous namespace */

    bool t2_validate(const T2Function &fn, std::string *err) {
        Validator v(fn, err);
        return v.run();
    }

    /* ------------------------------------------------------------------ *
     * Dump                                                               *
     * ------------------------------------------------------------------ */

    static void dump_op(std::string &out, const T2Op *op) {
        char buf[192];

        out += "    ";
        if (op->result != nullptr) {
            snprintf(buf, sizeof(buf), "v%u = ", op->result->id);
            out += buf;
        }
        out += t2_op_kind_name(op->kind);

        switch (op->kind) {
        case T2OpKind::ConstInt:
            snprintf(buf, sizeof(buf), " %lld", (long long)op->imm_int);
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

    return 0;
}
