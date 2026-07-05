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
 * T2-Full tier-2 JIT: high-level IR (HIR) core.
 *
 * A CFG-based SSA IR (PLAN/T2/01 §5) that maps 1:1 onto BEAM SSA ops in
 * the Phase-A subset, with the additions needed later for speculation,
 * unboxing, and deopt. This file defines the data structures, an arena,
 * a handful of construction helpers, and a validator.
 *
 * P0 scope: the IR is block-structured SSA with explicit phi nodes,
 * arena-allocated. The `framestate` op and the T2FrameState reference on
 * each op are reserved but never populated — P0 deopt is re-call only
 * (PLAN/T2FULL/01 §2, rung 1), so framestates arrive with general
 * inlining (P3). Nothing here is backend-specific; per
 * PLAN/T2FULL/04_backend.md the HIR must not depend on asmjit types.
 */

#ifndef _JIT_T2_HIR_HPP
#define _JIT_T2_HIR_HPP

#include <cstddef>
#include <cstdint>
#include <functional>
#include <new>
#include <string>
#include <utility>
#include <vector>

#include "t2_types.hpp"

/* The C retention struct (jit/t2/t2_retain.h); only used by pointer here. */
struct ErtsT2RetainedCode;

namespace erts_t2 {

    /* ------------------------------------------------------------------ *
     * Op kinds                                                           *
     * ------------------------------------------------------------------ */

    /*
     * The Phase-A op set (PLAN/T2/01 §5.2). Ops are grouped by category;
     * the ordering within a category is not significant. Speculative and
     * framestate ops are declared but not emitted by the P0 builder.
     */
    enum class T2OpKind : uint16_t {
        /* Constants and parameters */
        ConstInt,
        ConstFloat,
        ConstAtom,
        ConstNil,
        ConstLiteral,
        Param,
        Phi,

        /* Type tests (produce a boolean) */
        IsInteger,
        IsFloat,
        IsNumber,
        IsAtom,
        IsBoolean,
        IsTuple,
        IsList,
        IsNonemptyList,
        IsNil,
        IsBinary,
        IsBitstring,
        IsMap,
        IsPid,
        IsPort,
        IsReference,
        IsFunction,
        IsTaggedTuple,
        TestArity, /* tuple arity check (BEAM test_arity) */
        Succeeded, /* did the preceding op succeed (beam_ssa 'succeeded') */

        /* Comparisons */
        CmpEqExact,
        CmpNeExact,
        CmpEq,
        CmpNe,
        CmpLt,
        CmpLe,
        CmpGt,
        CmpGe,

        /* Generic arithmetic / bitwise (may allocate bignum, may raise) */
        Add,
        Sub,
        Mul,
        IDiv,
        Rem,
        Band,
        Bor,
        Bxor,
        Bsl,
        Bsr,
        Bnot,
        Neg,

        /* Speculative arithmetic (reserved; emitted only after speculation) */
        UntagInt,
        TagInt,
        AddSmall,
        SubSmall,
        MulRaw,

        /* Speculation guards (reserved) */
        SpeculateType,
        SpeculateRange,

        /* Tuples and lists */
        MakeTuple,
        GetTupleElement,
        MakeList,
        GetHd,
        GetTl,

        /* Maps (reserved for Phase B) */
        GetMapElement,

        /* Funs and calls */
        Call,
        CallExt,
        CallFun,
        TailCall,
        TailCallExt,
        TailCallFun,
        Bif,
        GuardBif,
        MakeFun,

        /* Control flow (terminators) */
        Branch,
        Jump,
        Switch,
        Return,

        /* Process / runtime */
        GcTest,
        ReductionCheck,
        ScheduleOut,
        FrameState, /* reserved marker, never generated in P0 */

        /* Sentinel */
        Invalid
    };

    const char *t2_op_kind_name(T2OpKind kind);
    bool t2_op_is_terminator(T2OpKind kind);
    bool t2_op_produces_value(T2OpKind kind);

    /* ------------------------------------------------------------------ *
     * Arena                                                              *
     * ------------------------------------------------------------------ */

    /*
     * A bump allocator that owns a chain of chunks and releases them all at
     * once when the function is destroyed. All IR nodes are trivially
     * destructible, so the arena never runs destructors.
     */
    class T2Arena {
    public:
        T2Arena();
        ~T2Arena();

        T2Arena(const T2Arena &) = delete;
        T2Arena &operator=(const T2Arena &) = delete;

        void *alloc(size_t size, size_t align);

        template<typename T>
        T *create() {
            void *p = alloc(sizeof(T), alignof(T));
            return new (p) T();
        }

        template<typename T>
        T *alloc_array(size_t n) {
            if (n == 0) {
                return nullptr;
            }
            return static_cast<T *>(alloc(sizeof(T) * n, alignof(T)));
        }

        size_t bytes_used() const {
            return used_bytes;
        }

    private:
        struct Chunk {
            Chunk *next;
            size_t capacity;
            size_t used;
            /* payload follows */
        };

        Chunk *current;
        size_t used_bytes;

        void new_chunk(size_t min_size);
    };

    /* ------------------------------------------------------------------ *
     * IR nodes                                                           *
     * ------------------------------------------------------------------ */

    struct T2Op;
    struct T2BasicBlock;
    struct T2FrameState; /* reserved; see file header */

    /* An SSA value: the result of exactly one op. */
    struct T2Value {
        uint32_t id; /* dense index into T2Function::values */
        T2Type type; /* lattice element */
        T2Op *def;   /* defining op */
    };

    /* One arm of a `switch` terminator. */
    struct T2SwitchCase {
        Eterm value; /* the matched term (atom / small / literal) */
        T2BasicBlock *target;
    };

    struct T2Op {
        T2OpKind kind;
        T2Type type; /* result type (== result->type when present) */

        T2Value *result; /* null for effect-only ops */

        uint16_t num_operands;
        T2Value **operands; /* arena array of length num_operands */

        T2BasicBlock *block; /* owning block */

        /* Intrusive doubly-linked list within the owning block's phi or body
         * list. Terminators are held directly by the block. */
        T2Op *next;
        T2Op *prev;

        /* Op-specific attributes. Only the field relevant to `kind` is
         * meaningful. */
        Sint64 imm_int;   /* ConstInt */
        Eterm imm_term;   /* ConstAtom, ConstNil, ConstLiteral term */
        Eterm mfa_m;      /* Call / CallExt / TailCall* target module */
        Eterm mfa_f;      /* ... function */
        uint32_t index;   /* param index, tuple-element index, call arity,
                           * type-test arity, literal index */
        uint32_t bif_num; /* Bif/GuardBif number (reserved) */

        /* Terminator successors. */
        T2BasicBlock *succ_then;      /* Branch-true / Jump target */
        T2BasicBlock *succ_else;      /* Branch-false */
        uint32_t num_cases;           /* Switch */
        T2SwitchCase *cases;          /* arena array */
        T2BasicBlock *default_target; /* Switch default */

        /* Phi incoming edges, parallel to `operands` (Phi only). */
        T2BasicBlock **phi_blocks;

        /* Reserved for rung-2 deopt (P3). Always null in P0. */
        T2FrameState *fs;

        /* Ordinal of the source BEAM op within the function, for the T1 PC
         * side table (populated by the builder; 0 for synthesized ops). */
        uint32_t beam_idx;
    };

    struct T2BasicBlock {
        uint32_t id;

        /* Explicit phi nodes at block entry (PLAN/T2/01 §5.1). */
        T2Op *phis_head;
        T2Op *phis_tail;

        /* Body ops in program order (excludes phis and the terminator). */
        T2Op *ops_head;
        T2Op *ops_tail;

        T2Op *terminator; /* branch / jump / switch / return; null until set */

        /* Predecessors, computed by T2Function::finalize() from successors. */
        uint32_t num_preds;
        T2BasicBlock **preds;

        /* Braun on-the-fly SSA bookkeeping (used by the builder). */
        bool sealed;
    };

    /* Reserved deopt metadata (PLAN/T2/01 §5.1, §6.5). Empty in P0. */
    struct T2FrameState {
        uint32_t beam_pc;
        T2FrameState *parent_fs;
    };

    /* ------------------------------------------------------------------ *
     * Function                                                           *
     * ------------------------------------------------------------------ */

    struct T2Function {
        T2Arena arena;

        Eterm module;
        Eterm function;
        uint32_t arity;

        /* Dense node lists. These live in the function object (not in the
         * arena) so they release their own storage on destruction; the arena
         * frees the node objects they point at. */
        std::vector<T2BasicBlock *> blocks;
        std::vector<T2Value *> values;

        /* blocks[0] is the entry block once new_block() has been called. */

        /* --- construction ------------------------------------------------- */

        T2BasicBlock *new_block();

        /* Create an op in `b`. `produces_value` controls whether a fresh SSA
         * value is attached as the result. Body ops are appended in order;
         * phis go on the phi list; terminators are stored on the block. */
        T2Op *new_op(T2BasicBlock *b, T2OpKind kind, T2Type ty);

        void set_operands(T2Op *op, std::initializer_list<T2Value *> ins);
        void set_operands(T2Op *op, const std::vector<T2Value *> &ins);

        /* Phi helpers: create an (initially empty) phi, then fill its incoming
         * edges once predecessors are known. */
        T2Op *new_phi(T2BasicBlock *b, T2Type ty);
        void set_phi_inputs(T2Op *phi,
                            const std::vector<T2Value *> &vals,
                            const std::vector<T2BasicBlock *> &preds);

        /* Convenience emitters used by the builder and self-test. */
        T2Value *emit_param(T2BasicBlock *b, uint32_t idx, T2Type ty);
        T2Value *emit_const_int(T2BasicBlock *b, Sint64 v);
        T2Value *emit_const_atom(T2BasicBlock *b, Eterm atom);
        T2Value *emit_const_nil(T2BasicBlock *b);
        T2Value *emit_const_literal(T2BasicBlock *b,
                                    uint32_t idx,
                                    Eterm term,
                                    T2Type ty);
        T2Value *emit_unary(T2BasicBlock *b,
                            T2OpKind kind,
                            T2Value *a,
                            T2Type ty);
        T2Value *emit_binary(T2BasicBlock *b,
                             T2OpKind kind,
                             T2Value *a,
                             T2Value *c,
                             T2Type ty);
        T2Value *emit_get_tuple_element(T2BasicBlock *b,
                                        T2Value *tuple,
                                        uint32_t idx,
                                        T2Type ty);
        T2Value *emit_call(T2BasicBlock *b,
                           T2OpKind kind,
                           Eterm m,
                           Eterm f,
                           uint32_t ar,
                           const std::vector<T2Value *> &args,
                           T2Type ty);

        void emit_branch(T2BasicBlock *b,
                         T2Value *cond,
                         T2BasicBlock *t,
                         T2BasicBlock *e);
        void emit_jump(T2BasicBlock *b, T2BasicBlock *target);
        void emit_return(T2BasicBlock *b, T2Value *v);

        /* Compute predecessor arrays from the terminator successors. Must be
         * called once the CFG is complete, before validation. */
        void finalize();

    private:
        T2Op *link_op(T2BasicBlock *b, T2Op *op);
    };

    /* ------------------------------------------------------------------ *
     * Validation and debug                                               *
     * ------------------------------------------------------------------ */

    /* Structural well-formedness check. Returns true on success; on failure
     * returns false and, if `err` is non-null, fills it with a description of
     * the first problem found. Assumes finalize() has run. */
    bool t2_validate(const T2Function &fn, std::string *err);

    /* A compact human-readable dump of the CFG and ops. */
    std::string t2_dump(const T2Function &fn);

    /* ------------------------------------------------------------------ *
     * Single-function builder entry (used by the t2_build_ssa debug BIF) *
     * ------------------------------------------------------------------ */

    enum class T2BuildStatus {
        Ok,          /* built + validated; `emit` was invoked */
        NotFound,    /* no function with that name/arity in the module */
        NotEligible, /* found, but not in the eligibility bitmap        */
        Failed       /* decode/build/validate failed; *err filled       */
    };

    /* Decode the retained module (§2), locate the eligible function whose
     * name/arity match, build + validate its SSA, and invoke `emit` with
     * it while every term the IR references is still alive — the module
     * decode (including any dynamic literals synthesized from the code
     * chunk) is released only after `emit` returns. Implemented in
     * t2_hir_builder.cpp. */
    T2BuildStatus t2_build_for_debug(
            const ErtsT2RetainedCode *ret,
            Eterm function,
            unsigned arity,
            const std::function<void(const T2Function &)> &emit,
            std::string *err);

} /* namespace erts_t2 */

/* ------------------------------------------------------------------ *
 * Self-test (P0 commit 2 test hook)                                  *
 * ------------------------------------------------------------------ */

/* Builds a couple of functions by hand and round-trips them through the
 * validator. Returns 0 on success, non-zero on failure. Wired into
 * beamasm_init() behind the T2_SELFTEST environment variable. */
extern "C" int erts_t2_hir_selftest(void);

/* True iff T2_SELFTEST is set (read once). */
extern "C" int erts_t2_selftest_enabled(void);

#endif /* _JIT_T2_HIR_HPP */
