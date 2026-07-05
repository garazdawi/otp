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
 *
 * P1 additions (PLAN/T2/01 §6.4's "stackmap-style metadata"):
 *
 *   - Sync maps (T2SyncMap). Every op at which T2 can exit, trap or GC
 *     (function entry, calls, returns, GC tests, frame allocation,
 *     gc_bif arithmetic, decoded error exits) carries a snapshot of the
 *     exact BEAM register state at that instruction boundary as decoded:
 *     {X0..X(x_live-1) -> value, Y0..Y(frame_size-1) -> value} plus the
 *     current stack-frame size in slots. The boundary convention is
 *     pinned as: the state T1 would observe when *about to execute* the
 *     op — i.e. after all preceding ops (including argument moves) have
 *     taken effect, before the op itself transfers control/traps. For a
 *     call this is exactly the state the T1-continuation contract needs
 *     (Y slots live across the call + args in X0..; X0 holds the result
 *     on return). For a fused call_last, the deallocation is split into
 *     an explicit preceding Deallocate op so the tail-call map records
 *     the post-dealloc (transfer) state.
 *
 *   - First-class frame ops (Allocate/Deallocate/Trim), so the frame
 *     layout is derivable at any program point and the identity backend
 *     emits the same frame motion T1 does.
 *
 *   - Canonical homes. Each op records the BEAM register its result was
 *     decoded into (dst_reg) and the register each operand was read
 *     from (operand_regs); phis record the Braun variable they merge as
 *     their home. Thus every live value at a rung-1 sync point has a
 *     canonical home *by construction*, and the validator cross-checks
 *     every sync map against a per-block register-state walk (a value
 *     appearing in a map without being materialized in that register is
 *     a hard error, never silent).
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

        /* Register copy (BEAM `move`/`swap`; mirrors beam_ssa_pre_codegen's
         * `copy`). SSA-wise an identity of its operand; its purpose is
         * placement: dst_reg names the BEAM register the copy fills so
         * later sync points find the value in its canonical home. */
        Copy,

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

        /* Stack-frame ops (P1; first-class so frame layout is derivable
         * at any point — PLAN/T2FULL/08 §"sync maps"). Allocate carries
         * the slot count (index), a fused heap need (imm_int; nonzero
         * for allocate_heap) and the GC live count (live). Deallocate
         * carries the popped slot count (index). Trim carries the
         * number of dropped slots (index) and the remaining count
         * (imm_int); it moves no data — E moves, renumbering the
         * surviving slots, and the builder renumbers its Braun Y
         * variables to match, so all later sync maps are in post-trim
         * numbering (what the loaded code's continuation expects). */
        Allocate,
        Deallocate,
        Trim,

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

    /* ------------------------------------------------------------------ *
     * BEAM register encoding + sync maps (P1)                            *
     * ------------------------------------------------------------------ */

    /* A BEAM register is encoded in one int32: X regs as their index,
     * Y regs offset by T2_YREG_BASE, T2_REG_NONE for "no register"
     * (consts, boolean guard results that are never materialized). The
     * same encoding is the Braun SSA variable key in the builder. */
    constexpr int32_t T2_REG_NONE = -1;
    constexpr int32_t T2_YREG_BASE = 0x10000;

    constexpr bool t2_reg_is_x(int32_t r) {
        return r >= 0 && r < T2_YREG_BASE;
    }
    constexpr bool t2_reg_is_y(int32_t r) {
        return r >= T2_YREG_BASE;
    }
    constexpr int32_t t2_xreg(uint32_t n) {
        return (int32_t)n;
    }
    constexpr int32_t t2_yreg(uint32_t n) {
        return T2_YREG_BASE + (int32_t)n;
    }
    constexpr uint32_t t2_reg_index(int32_t r) {
        return (uint32_t)(t2_reg_is_y(r) ? r - T2_YREG_BASE : r);
    }

    /* No stack frame (as opposed to a zero-slot frame, which does not
     * exist in BEAM: `allocate 0` still pushes the CP slot — frame_size
     * counts the Y slots, so it is 0 then). */
    constexpr int32_t T2_NO_FRAME = -1;

    /*
     * The sync-point register map (PLAN/T2/01 §6.4): the exact BEAM
     * register state at an instruction boundary, as decoded.
     *
     * Boundary convention (design decision, load-bearing): the state T1
     * would observe when *about to execute* the op the map is attached
     * to — after every preceding op (including argument moves) has taken
     * effect, before the op itself runs/transfers/traps. This is the
     * state the deopt/continuation contract consumes: a side exit
     * branches to the op's own T1 PC and T1 re-executes the op from this
     * state; a non-tail call transfers with args in X0.. and the frame
     * live, and the T1 continuation resumes from the Y slots + X0.
     *
     * X liveness is a prefix (BEAM's Live discipline): X0..x_live-1 are
     * live and every entry holds a value; X regs at or above x_live are
     * dead at this boundary (T1's GC would not scan them, so T2 may
     * leave anything there). There are no "dead holes" below x_live.
     *
     * Y slots cover the whole frame (frame_size slots). T1's stack
     * walker scans exactly these slots, so every entry must hold a value
     * that is a valid term at runtime. A slot killed at this boundary
     * (init_yregs) reads as the builder's const_nil value — precisely
     * T1's own representation of a killed slot (init_yregs stores NIL);
     * it can never read a stale pre-kill value because the kill was
     * written through the same Braun variable map this snapshot reads.
     */
    struct T2SyncMap {
        uint32_t x_live;    /* X0..x_live-1 are live                     */
        T2Value **x;        /* arena array [x_live]; no null entries     */
        int32_t frame_size; /* Y slot count, or T2_NO_FRAME              */
        T2Value **y;        /* arena array [frame_size]; no null entries */
    };

    /* T2Op::flags bits. */
    enum : uint8_t {
        /* A decoded error-exit op (badmatch/case_end/if_end): lowers to
         * a side exit to the op's own T1 PC. Carries a sync map. */
        T2_OP_ERR_EXIT_OP = 1 << 0,
        /* The synthesized, shared function_clause exit block: lowers to
         * a side exit to the function's func_info (ErtsCodeInfo). No
         * sync map — the state contract is each predecessor guard's
         * boundary, which full sync establishes; this mirrors T1, where
         * many guard sites jump to one func_info with differing frame
         * states. */
        T2_OP_ERR_EXIT_SHARED = 1 << 1,
        /* First op of a two-op decoded pair whose reads must precede
         * both writes (get_list's GetHd/GetTl, swap's Copy/Copy). The
         * pair shares a beam_idx; the backend must emit it fused (T1
         * does), and the validator's register-state walk treats it
         * atomically. */
        T2_OP_PAIR_HEAD = 1 << 2,
        /* A tail call whose decoded Deallocate operand does not match
         * the tracked frame (or a call_only with a live frame). The
         * compiler emits garbage Deallocate values when it has proven
         * the call cannot succeed (beam_validator's will_call_succeed
         * `no` arm: "the compiler is allowed to emit garbage values"),
         * e.g. `call_ext_last erlang:throw/1` after a trim. No frame op
         * is synthesized and no sync map attached; the P1 backend must
         * not lower such an op (these targets are raising BIFs / known
         * no-return calls, outside the identity tier's call lowering). */
        T2_OP_GARBAGE_DEALLOC = 1 << 3,
        /* Switch translated from select_tuple_arity: the case values are
         * small-tagged *arities* (make_small(N)), and the dispatch
         * semantics compare the operand's tuple header word against
         * make_arityval(N) — never the operand value itself. The backend
         * must emit the header-load shape (T1's
         * emit_i_select_tuple_arity); a value-compare lowering silently
         * sends every input to the default edge. */
        T2_OP_SWITCH_ARITY = 1 << 4
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
        Sint64 imm_int;   /* ConstInt; Allocate heap words; Trim remaining */
        Eterm imm_term;   /* ConstAtom, ConstNil, ConstLiteral term */
        Eterm mfa_m;      /* Call / CallExt / TailCall* target module */
        Eterm mfa_f;      /* ... function */
        uint32_t index;   /* param index, tuple-element index, call arity,
                           * type-test arity, literal index, GcTest heap
                           * words, Allocate/Deallocate/Trim slot count */
        uint32_t bif_num; /* Bif/GuardBif number (reserved) */
        uint32_t live;    /* decoded GC live count (GcTest / Allocate /
                           * gc_bif arithmetic)                          */

        /* Canonical homes (P1; see the file header). dst_reg is the BEAM
         * register the op's result was decoded into (T2_REG_NONE when the
         * result never lands in a register — guard booleans, operand
         * materializations). operand_regs[i] is the register operand i
         * was read from (T2_REG_NONE for constants); the array is null
         * when no operand came from a register. */
        int32_t dst_reg = T2_REG_NONE; /* NSDMI: arena zero-init would be x0 */
        int32_t *operand_regs; /* arena array [num_operands] or null */

        uint8_t flags; /* T2_OP_* bits */

        /* Sync-point register map (P1); null on non-sync ops. */
        T2SyncMap *sync;

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

        /* Index of this function within the module (== the retained
         * code's function ordinal; keys the pctab + code-header lookups
         * downstream). Set by the builder; 0 for hand-built functions. */
        uint32_t fn_index = 0;

        /* Sync map at function entry: X0..arity-1 = the Param values, no
         * frame. Null for hand-built functions. */
        T2SyncMap *entry_sync = nullptr;

        /* True when the builder attached sync maps + canonical homes;
         * gates the validator's sync/frame/home checks so hand-built
         * (selftest) functions remain valid without them. The P1
         * backend refuses functions without it. */
        bool sync_complete = false;

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
