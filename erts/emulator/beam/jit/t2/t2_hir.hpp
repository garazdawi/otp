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
/* The decoded bs_match command struct (jit/t2/t2_retain.h). */
struct ErtsT2BsCmd;

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

        /* maps:fold flatmap specialization (Stage 1;
         * PLAN/T2FULL/census/mapsfold_design.md). All five are
         * synthesized only by the maps:fold expander
         * (t2_intrinsics.cpp) — never decoded from BEAM.
         *
         * IsFlatmapBounded: boxed ∧ header subtag == flatmap ∧
         * size <= MAP_SMALL_MAP_LIMIT; a boolean consumed by the
         * block's Branch (both edges stay in the blob — non-flatmap
         * is the general case, not an error).
         *
         * FlatmapSize: load the flatmap's raw size word and tag it as
         * a small. Loop-invariant, never fails (dominated by the
         * shape guard).
         *
         * FlatmapKeyAt/FlatmapValAt: (map, index) -> term; the index
         * operand is a TAGGED small, provably < size by the loop
         * bound, so neither op can fail.
         *
         * FoldBudget: the whole-fold reduction batch. Charges
         * imm_int + index * untag(n) reductions (n = operand 0, the
         * tagged size; index = per-element charge, imm_int = the
         * constant charge) against FCALLS; when the budget is not
         * available it side-exits UNCHARGED to the erased call's own
         * T1 PC (ERTS_T2_PC_CALL) with the call-boundary sync map, so
         * T1 re-executes the fold and does its own charging/yielding.
         * Effect-only, sync required (T2_OP_SPEC_CALLSITE class). */
        IsFlatmapBounded,
        FlatmapSize,
        FlatmapKeyAt,
        FlatmapValAt,
        FoldBudget,

        /* The byte-aligned binary scan subset (P2 commit 7;
         * PLAN/T2FULL/09 §7). StartMatch creates/validates a match
         * context (bs_start_match3: may GC — sync point; result
         * written on the success edge only). BsMatch carries the
         * decoded byte-aligned command subset (bs_cmds/num_bs_cmds;
         * see ErtsT2BsCmd in t2_retain.h) over a context operand; at
         * most one command produces a value (the op result). It
         * advances the context's position — a heap-object mutation,
         * so it dirties re-execution windows — and is a sync point
         * only when its commands need heap (imm_int = heap words;
         * get_tail). BsGetTail builds the tail sub-bitstring (GC —
         * sync point). BsTestTail is a pure size guard. The match
         * context itself stays whole in its canonical slot at every
         * sync point (GC-visible); only the fused scan-loop emitter
         * (t2_emit.cpp) registerizes position/end between sync
         * points, restoring the context's start field on every path
         * out of the fused region. */
        StartMatch,
        BsMatch,
        BsGetTail,
        BsTestTail,

        /* Cursor-IV binary matching (PLAN/T2FULL/14). Decomposes a match
         * context into an explicit raw bit-cursor induction variable so
         * byte-scan loops can later unroll + SWAR. BsBase/BsLimit/BsCursor
         * project the boxed ErlSubBits: base pointer (GC-clobbered,
         * rematerializable), end bit-count (loop-invariant), start
         * bit-offset (the initial cursor). BsEnsure is a separable bounds
         * guard (imm_int = need bits; index = mode, 0 at_least / 1
         * exactly) folded into guard_branch like BsTestTail. BsRead is a
         * PURE non-allocating extraction at base+cursor (imm_int = size
         * bits; index = read kind). BsSync writes the raw cursor back to
         * ErlSubBits.start at every sync/exit/deopt. The cursor advances
         * via a raw AddSmall (T2_OP_RAW_MODE); bs_get/set_position
         * tag/untag it as pure SSA. See §2 of the design doc. */
        BsBase,
        BsLimit,
        BsCursor,
        BsEnsure,
        BsRead,
        BsSync,

        /* Funs and calls */
        Call,
        CallExt,
        CallFun,
        TailCall,
        TailCallExt,
        TailCallFun,
        Bif,
        /* The read-only guard-BIF subset (eligibility_wins.md WIN 3):
         * hd/tl ({f,0} shapes), node/1, element/2, map_get/2,
         * is_map_key/2 (bif1/bif2) and map_size/byte_size/bit_size
         * (gc_bif1). mfa_m/mfa_f carry the target, index its arity.
         * Read-only, no alloc, no trap — NOT a sync point: a real
         * fail label is a Succeeded/Branch in-blob edge, a {f,0}
         * fail side-exits to the op's own T1 EFFECT site (T1
         * re-executes and raises). */
        GuardBif,
        MakeFun,

        /* Control flow (terminators) */
        Branch,
        Jump,
        Switch,
        Return,

        /* Opaque: a zero-operand, zero-successor terminator sealing a
         * block whose real code could not be built — the TOLERANT
         * build mode's degrade marker (t2_build_for_p1). Such partial
         * HIR exists only for read-only classification (the P1
         * call-site specializer's wrapper walk, t2_intrinsics.cpp);
         * it is never lowered, and the standard build paths never
         * emit it. */
        Opaque,

        /* Process / runtime */
        GcTest,
        ReductionCheck,
        ScheduleOut,
        FrameState, /* reserved marker, never generated in P0 */

        /* Lists-intrinsic support (P2 commit 8; PLAN/T2FULL/09 §8).
         *
         * DemoteCallee: a terminator that transfers the invocation to
         * a T1 function *other than* the enclosing one — the lists
         * helper the intrinsic loop stands in for — entering its body
         * past the entry reduction check with the fresh-call vector
         * pinned in X0..arity-1 by the attached sync map, after
         * pushing the intrinsic call site's T1 continuation as the CP
         * the skipped prologue would have pushed. T1 then re-executes
         * the iteration (raising the byte-identical error on the
         * error edges) and returns to the caller's own T1
         * continuation. imm_int = the callee's T1 entry L_f; live =
         * the callee arity; mfa_m/mfa_f name it; beam_idx = the
         * intrinsic call site (CONT lookup). At a TAIL site
         * (T2_OP_TAIL_SITE; the P1 inner re-dispatch) there is no T1
         * continuation and no CP is pushed — the blob's own return
         * address already points at the caller's caller.
         *
         * ChargeReds: `FCALLS -= imm_int`, no check, no sync — the
         * reduction charges T1 pays on a path where the intrinsic
         * loop cannot yield (an early-exit edge charging the erased
         * fun call). Dirties re-execution windows (a re-executed
         * iteration must not double-charge). */
        DemoteCallee,
        ChargeReds,

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
    enum : uint16_t {
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
        T2_OP_SWITCH_ARITY = 1 << 4,
        /* Speculative op (SpeculateType / AddSmall / SubSmall) whose
         * deopt is *boundary*-shaped (P2 commit 4): the side exit
         * branches to the op's own T1 EFFECT PC and T1 re-executes just
         * that op from its sync-map state — nothing before it is
         * re-executed, so it is legal after effects. The op must keep
         * its sync map (the boundary contract). Without this flag a
         * speculative op is *window*-shaped: the side exit branches to
         * the function's T1 entry body (L_f + TEST_YIELD_RETURN_OFFSET)
         * and T1 re-executes the whole iteration/invocation from the
         * fresh-call vector in X0..arity-1 — legal only on a clean
         * prefix (no effect, no frame op, no write to X0..arity-1
         * before it; enforced by t2_validate_windows). */
        T2_OP_SPEC_BOUNDARY = 1 << 5,
        /* Op spliced from an inlined callee (P2 commit 6). Such an op
         * has no T1 PC of its own in the caller, so a fallible inlined
         * op must be converted to a window-shaped speculative op — the
         * enclosing iteration's re-execution covers the inlined body
         * (PLAN/T2/08 §4.2) — or inlining is abandoned. */
        T2_OP_INLINED = 1 << 6,
        /* A TestArity that also performs the boxed/tuple check (the
         * loop shape-up fused an is_tuple + test_arity chain with one
         * shared fail edge, exactly like T1's own
         * is_tuple+test_arity => i_is_tuple_of_arity loader transform;
         * lowers to emit_i_is_tuple_of_arity). */
        T2_OP_TUPLE_ARITY_FUSED = 1 << 7,
        /* A ReductionCheck whose demote target is a CALLEE — the lists
         * helper an intrinsic loop (P2 commit 8) stands in for — not
         * the enclosing function. Its sync map is the callee's
         * fresh-call vector (x_live = the callee arity, in `live`;
         * mfa_m/mfa_f name the callee; imm_int = the callee's T1
         * entry) over the caller's still-live frame. The yield stub
         * embeds the callee MFA, so a suspended process introspects
         * exactly as if it had yielded inside the T1 helper. */
        T2_OP_RC_CALLEE = 1 << 8,
        /* A window-shaped speculative op inside an intrinsic loop: its
         * deopt trampoline pushes the intrinsic call site's T1
         * continuation as CP and enters the CALLEE body (imm_int =
         * callee L_f) — T1 re-executes the iteration as a fresh helper
         * call from X0..callee_arity-1. */
        T2_OP_WINDOW_CALLEE = 1 << 9,
        /* The third deopt class (maps:fold Stage 1): a speculative op
         * (SpeculateType/AddSmall/SubSmall) or FoldBudget whose side
         * exit RE-EXECUTES THE ERASED CALL — it branches to the call
         * site's own T1 PC (ERTS_T2_PC_CALL), no CP push, and T1
         * re-executes the whole call_ext from the call-boundary state.
         * The op must carry the call-boundary sync map (cmap); sound
         * because the specialized fast path is effect-free/alloc-free
         * and writes nothing below cmap->x_live nor any Y slot before
         * the exit, so the boundary state is physically intact.
         * Validated by the callsite rule in t2_validate_windows. */
        T2_OP_SPEC_CALLSITE = 1 << 10,
        /* The fourth deopt class (Stage 3 make_fun sinking,
         * PLAN/T2FULL/census/stage3_opts_design.md §3d): a former
         * callsite-class op whose erased-call boundary was dissolved
         * by the sink — the fun that boundary needs in X0 is no longer
         * materialized on the fast path. Its side exit RE-EXECUTES THE
         * WHOLE INVOCATION instead: it branches to the function's own
         * T1 entry body (L_f + TEST_YIELD_RETURN_OFFSET, the plain
         * window target), no CP push, and T1 re-runs everything —
         * including the sunk make_fun and the erased call — from the
         * fresh-call vector in X0..arity-1. Sound only when every path
         * from function entry to the op is clean: no effect, frame op,
         * reduction charge (FoldBudget's own batch charge is the same
         * accepted deviation the callsite class documents), GC below
         * the arity prefix, or write to X0..arity-1 — enforced by the
         * entry-recall rule in t2_validate_windows. FoldBudget keeps a
         * sync map (the entry vector; it is a sync-required op);
         * converted SpeculateType/AddSmall/SubSmall carry none, like
         * any window-shaped guard. The deopt monitoring counter still
         * counts these exits (t2_isel keeps spec_callsite set). */
        T2_OP_SPEC_ENTRY = 1 << 11,
        /* The fifth deopt class (P1a general call-site specialization,
         * PLAN/T2FULL/census/p1_design.md): a speculative op inside an
         * inlined UNBOUNDED callee loop whose side exit RE-DISPATCHES
         * THE GENERIC CALLEE WITH THE LOOP-CARRIED STATE. Inner mode
         * (imm_int = the TERMINAL inlined loop function's T1 entry
         * L_f; identity permutation only): the side exit enters that
         * function's body past its entry check — the back edge
         * pre-charged this iteration's entry — so T1 re-executes the
         * iteration inside the real loop function with T1-exact
         * reductions and error frames (at a BODY site — no
         * T2_OP_TAIL_SITE — the deopt trampoline first pushes the
         * call site's T1 CONT as the CP the skipped callee prologue
         * would have pushed; t2_isel's fill_spec_cont). Otherwise
         * (imm_int = 0) it branches to the erased call site's own T1
         * PC (ERTS_T2_PC_CALL, no CP push; the T1 call instruction
         * itself re-establishes the CP for a body site and
         * tail-transfers for a tail site), and T1 re-executes the
         * call over the CURRENT X0..x_live-1, which by the op's sync
         * map holds exactly {loop-carried vector} — so T1 continues
         * the fold from element k with no redo (contrast
         * T2_OP_SPEC_CALLSITE, whose contract is the *original*
         * boundary being intact: a restart, only legal for bounded
         * loops). The op must carry the loop-carried sync map; the
         * register-state walk proves the vector is physically in
         * X0..x_live-1, and the window validator applies the
         * per-iteration clean-prefix rule (the flag is deliberately
         * NOT exempted in op_is_window_guard). */
        T2_OP_SPEC_REDISPATCH = 1 << 12,
        /* The specialized site was a TAIL call (call_ext_only): there
         * is no T1 continuation (no CONT pctab entry, nothing to push
         * as CP). On a ReductionCheck with T2_OP_RC_CALLEE this makes
         * the tombstone demote enter the callee body WITHOUT the CP
         * push (the blob's own return address already points at the
         * caller's caller, exactly as after the erased tail call);
         * same on a DemoteCallee and on an inner-mode re-dispatch
         * spec op (whose BODY-site form pushes the site's CONT
         * instead). P1 sets it on every site-shape-sensitive op it
         * plants; its absence means a BODY site throughout. */
        T2_OP_TAIL_SITE = 1 << 13,
        /* P2 loop unboxing (tag elimination): the op's result is a
         * RAW-IN-HOME word — the tagged small with its low
         * _TAG_IMMED1_SIZE tag bits CLEARED (value << 4) — living in
         * its canonical X home across the loop. The representation is
         * chosen so the flag-setting add/sub overflow check (b.vs) is
         * bit-identical to the tagged one, re-tagging is one ORR with
         * _TAG_IMMED1_SMALL and un-tagging one AND. Legal only on
         * Phi / AddSmall / SubSmall / ConstInt / FlatmapSize
         * (producers) and CmpLt / FlatmapKeyAt / FlatmapValAt
         * (raw-consuming ops); the producers must be X-homed, and
         * every consumer relationship is enforced by run_raw_checks
         * (t2_hir.cpp). At any op whose deopt/yield path hands the
         * register file to T1, the raw homes named by its sync map
         * must be declared in T2Op::raw_mask so the emitter re-tags
         * them in the cold path — a raw word reaching T1 as a term is
         * memory corruption, so the validator makes a missed mask a
         * hard error. */
        T2_OP_RAW_MODE = 1 << 14,
        /* P3 (range-based overflow-guard elimination): this AddSmall
         * provably cannot overflow the small range, so the emitter
         * omits the flag-check deopt (plain `add`, no b.vs, no
         * trampoline). The proof is the bounded-IV shape of the
         * maps:fold flatmap template: the op adds a small positive
         * constant to a value that a loop-bound compare (CmpLt against
         * a FlatmapSize result whose map passed IsFlatmapBounded, so
         * bound <= MAP_SMALL_MAP_LIMIT) proves < the bound on every
         * path since its last redefinition — see
         * t2_addsub_no_ovf_provable. Everything else about the op
         * (deopt-class flags, sync map, t1_pc_fail plumbing) is kept,
         * so the validators' class rules apply unchanged; the flag is
         * set only by the P3 pass (t2_opt.cpp) and RE-PROVEN by the
         * validator — an unprovable claim is a hard validate error,
         * never a silently missing guard. */
        T2_OP_NO_OVF = 1 << 15
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
        int32_t *operand_regs;         /* arena array [num_operands] or null */

        uint16_t flags; /* T2_OP_* bits */

        /* P2 loop unboxing: bit i set = the value this op's sync map
         * names at X i is RAW-IN-HOME (see T2_OP_RAW_MODE) and the
         * op's deopt/yield emission must re-tag X i in the cold path
         * before T1 observes the register file (and un-tag it again
         * on a resume path back into the blob). Zero everywhere
         * outside an unboxed loop. Only X0..X31 are maskable; the
         * validator rejects raw homes above that. */
        uint32_t raw_mask = 0;

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

        /* Decoded bs_match command subset (BsMatch only; arena array).
         * The struct is the C parser's output (ErtsT2BsCmd,
         * t2_retain.h) with dst_arg resolved away — the single
         * destination command's home is the op's dst_reg. */
        const struct ::ErtsT2BsCmd *bs_cmds;
        uint16_t num_bs_cmds;

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

        /* Cross-module dependencies (P2 commit 8): the BeamCodeHeader of
         * every OTHER module instance whose T1 addresses are baked into
         * this function's blob (the lists helper an intrinsic demotes
         * to). The installer records them so the blob is jettisoned
         * when a dependency is deleted, traced or NIF-patched. */
        std::vector<const void *> dep_hdrs;

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

    /* True when a value's lattice element *proves* it is a small integer
     * (integer-only union with a range inside the small range). Shared
     * fact between the speculation pass (t2_spec.cpp) and the validator's
     * speculative-type walk: a proof never needs a guard. */
    bool t2_type_proves_small(const T2Type &t);

    /* True when the value is a RAW-IN-HOME word (P2 loop unboxing): its
     * def carries T2_OP_RAW_MODE and produces a value. Structural — a
     * value has one representation everywhere (SSA), so rawness is a
     * property of the def. Shared between the validator (t2_hir.cpp)
     * and isel (t2_isel.cpp). */
    bool t2_value_is_raw_home(const T2Value *v);

    /* P3: true when `op` (an AddSmall) provably cannot overflow the
     * small range, so its b.vs deopt guard may be omitted
     * (T2_OP_NO_OVF). The proof is range-based and edge-sensitive; see
     * the implementation (t2_opt.cpp). Shared fact between the P3 pass
     * (which sets the flag only when this holds) and the validator
     * (which re-proves every claimed flag — a hard error otherwise). */
    bool t2_addsub_no_ovf_provable(const T2Function &fn, const T2Op *op);

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
            const std::function<void(T2Function &)> &emit,
            std::string *err);

    /* t2_build_for_debug's TOLERANT sibling (P1c-2): builds the named
     * function even when it is NOT in the eligibility bitmap, degrading
     * every unbuildable op into an Opaque leaf terminator sealing its
     * block (the rest of the unsupported region is skipped up to the
     * next reachable label). The result validates as ordinary HIR but
     * exists only for read-only classification — the P1 wrapper walk
     * treats Opaque blocks as not-the-fast-path leaves — and must never
     * be lowered or committed as a cloned loop. Same emit/lifetime
     * contract as t2_build_for_debug. */
    T2BuildStatus t2_build_for_p1(const ErtsT2RetainedCode *ret,
                                  Eterm function,
                                  unsigned arity,
                                  const std::function<void(T2Function &)> &emit,
                                  std::string *err);

    /* Decode the retained module once and build + validate every
     * standalone-installable function (the install bitmap — NOT the
     * wider eligible bitmap: a buildable-only function would just
     * degrade at isel), invoking `emit` for each success while the
     * module decode is alive (same contract as t2_build_for_debug).
     * Build/validate failures skip the function (it stays on T1).
     * Returns false with *err set only when the module decode itself
     * fails. `failures`, when non-null, receives the number of
     * functions that failed to build/validate. Drives the +JT2enable
     * compile-at-load path (t2_compile.cpp). */
    bool t2_build_each(const ErtsT2RetainedCode *ret,
                       const std::function<void(T2Function &)> &emit,
                       int *failures,
                       std::string *err);

    /* Decode the retained module ONCE and build + validate exactly the
     * eligible functions whose indices are listed (others are skipped;
     * a listed ineligible/out-of-range index is skipped too). The
     * tier-up worker's per-module batch path — the ModuleDecode is
     * shared across all listed functions (PLAN/T2FULL/09 §1's queue
     * decode caching). Same emit contract as t2_build_each. */
    bool t2_build_selected(const ErtsT2RetainedCode *ret,
                           const uint32_t *fn_indices,
                           size_t n,
                           const std::function<void(T2Function &)> &emit,
                           std::string *err);

} /* namespace erts_t2 */

/* ------------------------------------------------------------------ *
 * Optimization fire-counters (erts_debug:get_internal_state(         *
 * t2_opt_stats))                                                     *
 * ------------------------------------------------------------------ */

/* Cumulative-since-boot counts of committed P1/P2/P3 transforms, one
 * bump per commit point (the same points the T2_P1_TRACE /
 * T2_INTRIN_TRACE / T2_P3_TRACE lines report). Racy monitoring
 * counters in the t2_yield_stats tradition: plain non-atomic words,
 * bumped only downstream of the pass levers (T2_NO_P2 / T2_NO_P3), so
 * they never influence codegen or control flow — reads are advisory.
 * Defined in t2_compile.cpp next to t2_stats; the debug hook is
 * erts_t2_debug_opt_stats (t2_install.h). */
struct ErtsT2OptStats {
    uint64_t p1_sites_inlined;   /* P1: fold call sites inlined
                                  * (tail/body/wrapper commits)     */
    uint64_t p1_loops_recovered; /* t2_loop_recover successes       */
    uint64_t p2_acc_unboxed;     /* P2: loop accumulators unboxed   */
    uint64_t p2_iv_unboxed;      /* P2: induction-var/bound pairs
                                  * made raw (maps:fold flatmap)    */
    uint64_t p3_guards_removed;  /* P3a: redundant guards removed   */
    uint64_t p3_iv_ovf_removed;  /* P3b: IV overflow guards removed */
};

extern ErtsT2OptStats erts_t2_opt_stats;

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
