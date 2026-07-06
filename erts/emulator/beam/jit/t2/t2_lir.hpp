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
 * T2-Full tier-2 JIT: low-level IR (LIR) core (PLAN/T2FULL/08 §1).
 *
 * LIR is the backend representation between HIR (backend-neutral SSA,
 * t2_hir.hpp) and asmjit emission. Its whole contract is
 * *isel + regalloc + encode + framestate metadata* (04_backend.md §4).
 * The HIR must not depend on asmjit types; the LIR is where machine
 * concerns first appear -- but even the LIR stays asmjit-free (it names
 * canonical BEAM slots and abstract physical registers), so that the
 * emitter (t2_emit.cpp) is the only place that touches asmjit / the
 * reused T1 emitters.
 *
 * The P1 identity transform lowers each HIR op 1:1 to a LIR op
 * (t2_isel.cpp). Register allocation (t2_regalloc.cpp) runs the
 * sync-everything policy: every op boundary is a sync point that pins
 * each live X/Y value to its canonical BEAM slot, so cross-op live
 * ranges never span physical registers and every operand of every op
 * is a canonical slot (Xn/Yn). Physical-register PhysLocs are reserved
 * for the per-op TMP scratch a real (non-sync-everything) P2 allocator
 * will start using once it demotes non-sync-point boundaries.
 *
 * The structure the map prescribes: a flat vector of
 *   T2LirOp { kind; PhysLoc dst; PhysLoc srcs[]; imm; mfa; t1_pc_fail; beam_idx }
 * grouped into blocks so terminators keep their CFG edges.
 */

#ifndef _JIT_T2_LIR_HPP
#define _JIT_T2_LIR_HPP

#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

#include "t2_types.hpp"

namespace erts_t2 {

    /* Sync-point register map (t2_hir.hpp); the LIR references the HIR's
     * maps by pointer (the HIR function outlives the LIR in every
     * pipeline — emission happens inside the builder's emit callback). */
    struct T2SyncMap;

    /* "No SSA value" marker for the allocator annotations below. */
    constexpr uint32_t T2_NO_VALUE = 0xFFFFFFFFu;

    /* ------------------------------------------------------------------ *
     * LIR op kinds                                                       *
     * ------------------------------------------------------------------ */

    /*
     * The identity lowering table (PLAN/T2FULL/08 §2) maps HIR op kinds
     * onto these LIR kinds, each of which reuses one T1 arm emitter. The
     * mapping is intentionally close to 1:1 with the HIR op set; the LIR
     * kind names the *emitter* to invoke rather than the abstract SSA op.
     */
    enum class T2LirKind : uint16_t {
        /* Data movement + constant materialization (emit_i_move / mov_arg). */
        Move,
        /* Register exchange (emit_swap; a decoded pair whose reads must
         * precede both writes). dst/dst2 name the two registers. */
        Swap,

        /* Type tests (emit_is_X / emit_i_is_tuple / emit_i_test_arity).
         * Fail redirects to the op's T1 PC. */
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
        TestArity,

        /* Comparisons (emit_is_eq_exact ...). Fail -> T1 PC. */
        CmpEqExact,
        CmpNeExact,
        CmpEq,
        CmpNe,
        CmpLt,
        CmpLe,
        CmpGt,
        CmpGe,

        /* Generic arithmetic / bitwise (emit_i_plus/minus/... , generic
         * emit_i_bifN). Fail -> T1 PC; these *side-exit*, never raise. */
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

        /* Guard BIF (CP-less leaf). Fail -> T1 PC. */
        GuardBif,

        /* Tuples and lists (heap covered by a preceding GcTest). */
        GetTupleElement,
        GetHd,
        GetTl,
        GetList, /* fused hd+tl (emit_get_list; dst=hd, dst2=tl — the
                  * destinations may alias the source)                 */
        MakeList,
        MakeTuple,

        /* Heap reservation (emit_gc_test / emit_test_heap). */
        GcTest,

        /* Stack-frame ops (emit_allocate_heap / emit_deallocate /
         * emit_trim), first-class as in the HIR. */
        Allocate,   /* imm = slots, imm2 = fused heap words, live      */
        Deallocate, /* imm = slots                                     */
        Trim,       /* imm = dropped slots, imm2 = remaining           */

        /* Terminators. */
        Jump,   /* unconditional branch to succ_then                     */
        Branch, /* the *last* type-test/compare already branched to Fail;
                 * an explicit Branch is used when isel keeps the boolean
                 * (succ_then on true, succ_else on false)               */
        Switch, /* emit_i_select_val_*                                   */
        Return, /* emit_return                                           */

        /* Calls. Non-tail calls hand-emit the T1-continuation CP (surprise
         * #4); tail calls re-enter through the callee's patched prologue. */
        Call,
        CallExt,
        TailCall,
        TailCallExt,

        /* A light-BIF call (T1's call_light_bif), via the T2 variant of
         * the shared fragment (t2_call_light_bif_shared). NOT a
         * terminator: the success path returns into the blob and
         * execution continues; yield/trap/trace demote to T1 through
         * the two T1 addresses this op carries (t1_pc_fail = the site,
         * t1_pc_cont = the continuation). `exp` is the Export*,
         * `target` the BIF's C function from bif_table. Tail-shaped
         * sites (call_ext_last/only) lower to CallBif + Deallocate +
         * Return, mirroring T1's own transform, which calls the BIF
         * with the frame intact and only then deallocates. */
        CallBif,

        /* An unconditional side-exit: branch to t1_pc_fail. Used to lower
         * error-exit blocks (badmatch/if_end/case_end) -- T2 never raises
         * (surprise #7). */
        SideExit,

        /* Speculative ops (P2 commit 4; PLAN/T2/08 §4.4). All three
         * carry t1_pc_fail = the deopt PC (boundary shape: the op's own
         * T1 EFFECT site; window shape: the function's T1 entry body,
         * re-executing the iteration from the fresh-call vector).
         *
         * SpeculateSmall is the fused tag-bit guard (the MVP's AND
         * rule): AND all 1..4 source values, require every small-tag
         * bit, b.ne -> deopt. AddSmall/SubSmall are the flag-checked
         * one-untag arithmetic: clear one operand's tag bits, compute
         * with a flag-setting add/sub into scratch, b.vs -> deopt,
         * commit the (still-tagged) result to dst afterwards — deopt
         * fires strictly before the commit. */
        SpeculateSmall,
        AddSmall,
        SubSmall,

        /* A recovered loop's back-edge reduction charge + yield check
         * (t2_loop.cpp; PLAN/T2/08 §5.4): `subs FCALLS, #1` — exactly
         * T1's one-per-self-tail-call charge — and on exhaustion a
         * demote to T1 through i_test_yield_shared with ARG3 =
         * `target` (the function's own T1 entry L_f, resolved like a
         * local tail call), so the resume PC is the T1 body and the
         * saved state is the back-edge sync map's fresh-call vector.
         * Not a terminator: falls through to the back-jump. Only
         * emittable in install mode (the L_f contract). */
        ReductionCheck,

        Invalid
    };

    const char *t2_lir_kind_name(T2LirKind kind);
    bool t2_lir_kind_is_terminator(T2LirKind kind);

    /* ------------------------------------------------------------------ *
     * PhysLoc: a canonical BEAM slot or a pinned physical register       *
     * ------------------------------------------------------------------ */

    /*
     * Either a canonical BEAM slot (Xn/Yn) -- the only kind the P1
     * sync-everything allocator ever produces for a cross-op value -- or
     * a pinned physical register (reserved for P2's per-op scratch and
     * range-carrying values). `None` marks an absent operand/result.
     */
    struct PhysLoc {
        enum class Kind : uint8_t { None, XReg, YReg, Phys };

        Kind kind;
        uint16_t num; /* X/Y index, or abstract physical-register id */

        constexpr PhysLoc() : kind(Kind::None), num(0) {}
        constexpr PhysLoc(Kind k, uint16_t n) : kind(k), num(n) {}

        static constexpr PhysLoc none() {
            return PhysLoc(Kind::None, 0);
        }
        /* Named xreg/yreg (not x/y) to dodge the `x(N)`/`y(N)` BEAM
         * register-access macros from beam_common.h. */
        static constexpr PhysLoc xreg(uint16_t n) {
            return PhysLoc(Kind::XReg, n);
        }
        static constexpr PhysLoc yreg(uint16_t n) {
            return PhysLoc(Kind::YReg, n);
        }
        static constexpr PhysLoc phys(uint16_t id) {
            return PhysLoc(Kind::Phys, id);
        }

        constexpr bool is_none() const {
            return kind == Kind::None;
        }
        constexpr bool is_xreg() const {
            return kind == Kind::XReg;
        }
        constexpr bool is_yreg() const {
            return kind == Kind::YReg;
        }
        constexpr bool is_slot() const {
            return kind == Kind::XReg || kind == Kind::YReg;
        }
        constexpr bool is_phys() const {
            return kind == Kind::Phys;
        }

        constexpr bool operator==(const PhysLoc &o) const {
            return kind == o.kind && num == o.num;
        }
        constexpr bool operator!=(const PhysLoc &o) const {
            return !(*this == o);
        }
    };

    /* ------------------------------------------------------------------ *
     * LIR ops                                                            *
     * ------------------------------------------------------------------ */

    static constexpr int T2_LIR_MAX_SRCS = 4;

    /*
     * A source operand: either a slot/physical location (the common case
     * under sync-everything) or an inline constant term. Constants appear
     * inline so that isel can keep a `Move Xn <- const` explicit while
     * still letting arithmetic/test ops name a constant directly where the
     * T1 emitter accepts one.
     */
    struct T2LirSrc {
        bool is_const;
        PhysLoc loc;   /* when !is_const */
        Eterm term;    /* when is_const: a tagged immediate (small/atom/nil) */

        /* The SSA value read (P2 allocator annotation), or T2_NO_VALUE for
         * constants and runtime-defined slots (e.g. a BIF result in X0
         * consumed by a synthesized epilogue op — no SSA value exists). */
        uint32_t value;

        constexpr T2LirSrc()
                : is_const(false), loc(), term(0), value(T2_NO_VALUE) {}

        static T2LirSrc slot(PhysLoc l, uint32_t v = T2_NO_VALUE) {
            T2LirSrc s;
            s.is_const = false;
            s.loc = l;
            s.value = v;
            return s;
        }
        static T2LirSrc immediate(Eterm t) {
            T2LirSrc s;
            s.is_const = true;
            s.term = t;
            return s;
        }
    };

    /* Sentinel for "no successor edge" (block 0 is a real block). */
    static constexpr uint32_t T2_LIR_NO_BLOCK = 0xFFFFFFFFu;

    struct T2LirOp {
        T2LirKind kind;

        PhysLoc dst;                 /* result slot, or None            */
        PhysLoc dst2;                /* second result (GetList tl, Swap) */
        T2LirSrc srcs[T2_LIR_MAX_SRCS];
        uint8_t num_srcs;

        /* Op-specific immediates. */
        Sint64 imm;    /* raw immediate (tuple/element index, arity,
                        * frame slots, ...)                               */
        Sint64 imm2;   /* second immediate (Allocate heap words, Trim
                        * remaining)                                      */
        Eterm imm_term;/* tagged immediate (Move of const / switch key)    */

        /* Call target (Call/TailCall/CallExt/TailCallExt and arith BIF
         * provenance). */
        Eterm mfa_m;
        Eterm mfa_f;
        uint32_t arity;      /* call arity                                  */
        uint32_t live;       /* decoded GC live count (arith/GcTest/Alloc)  */
        const void *exp;     /* resolved Export* for a remote call          */
        const void *target;  /* resolved ErtsCodePtr for a local call/tail  */

        /* The T1 PC this op side-exits to on failure (arith with a {f,0}
         * fail — the op's own EFFECT site) or unconditionally (SideExit:
         * an ERROR site or the function's func_info). NULL when the op
         * has no side exit. */
        const void *t1_pc_fail;

        /* The T1 post-call continuation a non-tail Call/CallExt pushes
         * as its CP (PLAN/T2/08 §4.3: no return addresses into blobs). */
        const void *t1_pc_cont;

        /* Shared decode ordinal with the HIR/pctab (PLAN/T2FULL/07 §4). */
        uint32_t beam_idx;

        /* CFG edges (block ids into T2LirFunction::blocks, or
         * T2_LIR_NO_BLOCK). Tests/compares/arith-with-fail-edge use
         * succ_else as the in-blob fail target and succ_then as the
         * continuation. */
        uint32_t succ_then;
        uint32_t succ_else;

        /* Switch cases: [first_case, first_case+num_cases) index the
         * function-level `switch_cases` pool. default_target is a block. */
        uint32_t first_case;
        uint32_t num_cases;
        uint32_t default_target;

        /* MakeTuple with more than T2_LIR_MAX_SRCS elements: operands
         * live in T2LirFunction::src_pool[pool_first..pool_first+
         * num_srcs_ext) and num_srcs is 0. */
        uint32_t pool_first;
        uint32_t num_srcs_ext;

        /* --- P2 allocator annotations (t2_regalloc.cpp) ---------------
         * The SSA values this op defines (dst/dst2), or T2_NO_VALUE, and
         * the sync-point register map attached to the originating HIR op
         * (null on non-sync ops). The sync map is the allocator's
         * pin-constraint set: at this op's boundary every value it names
         * must occupy its canonical slot. For the reordered tail-BIF
         * shape the attached map is the *transfer* map (X part exact,
         * frame part positionally early — see emit_tail_bif). */
        uint32_t dst_value;
        uint32_t dst2_value;
        const T2SyncMap *sync;

        T2LirOp()
                : kind(T2LirKind::Invalid), dst(), dst2(), num_srcs(0),
                  imm(0), imm2(0), imm_term(0), mfa_m(0), mfa_f(0), arity(0),
                  live(0), exp(nullptr), target(nullptr), t1_pc_fail(nullptr),
                  t1_pc_cont(nullptr), beam_idx(0),
                  succ_then(T2_LIR_NO_BLOCK), succ_else(T2_LIR_NO_BLOCK),
                  first_case(0), num_cases(0),
                  default_target(T2_LIR_NO_BLOCK), pool_first(0),
                  num_srcs_ext(0), dst_value(T2_NO_VALUE),
                  dst2_value(T2_NO_VALUE), sync(nullptr) {}
    };

    /* One arm of a Switch terminator. */
    struct T2LirSwitchCase {
        Eterm value;         /* matched term (small / atom / literal)   */
        uint32_t target;     /* block id                                */
    };

    /* An SSA phi carried onto the LIR for the allocator. Under identity
     * placement a phi emits no code (every predecessor materialized the
     * merged value in the phi's home slot), but the allocator needs the
     * def (block entry) and the per-edge inputs (uses at each pred's
     * exit) to build correct liveness across merges and loop back-edges.
     * An input value of T2_NO_VALUE marks an input the allocator does
     * not track (never occurs today; defensive). */
    struct T2LirPhi {
        uint32_t value;   /* the phi's SSA value                        */
        PhysLoc home;     /* canonical home slot (Xn/Yn)                */
        struct In {
            uint32_t pred_block; /* LIR block id of the incoming edge   */
            uint32_t value;      /* SSA value on that edge              */
        };
        std::vector<In> ins;
    };

    struct T2LirBlock {
        uint32_t id;
        std::vector<T2LirOp> ops; /* flat vector, terminator last */
        std::vector<T2LirPhi> phis; /* allocator-only; emit no code */
    };

    struct T2LirFunction {
        Eterm module;
        Eterm function;
        uint32_t arity;

        /* Public T1 entry of this function (the fallback re-entry point,
         * and the default fail target for leaf single-op ops). */
        const void *t1_entry;

        std::vector<T2LirBlock> blocks;
        std::vector<T2LirSwitchCase> switch_cases;

        /* Overflow operand pool (MakeTuple with many elements). */
        std::vector<T2LirSrc> src_pool;

        /* P1 identity isel emits concrete canonical slots directly (the
         * HIR carries decoded homes); these fields remain from the
         * placeholder scheme for the P2 allocator's benefit. */
        uint32_t num_values;
        std::vector<int32_t> param_x;

        /* P2 allocator inputs. entry_sync is the HIR function-entry map
         * (params in X0..arity-1, no frame); value_flags[i] carries the
         * per-value facts the allocator needs without reaching back into
         * the HIR (T2_LIR_VF_* bits below). */
        const T2SyncMap *entry_sync;
        std::vector<uint8_t> value_flags;

        T2LirFunction()
                : module(0), function(0), arity(0), t1_entry(nullptr),
                  num_values(0), entry_sync(nullptr) {}

        T2LirBlock &new_block() {
            T2LirBlock b;
            b.id = (uint32_t)blocks.size();
            blocks.push_back(std::move(b));
            return blocks.back();
        }
    };

    /* T2LirFunction::value_flags bits. */
    enum : uint8_t {
        /* The value's HIR def is a constant materialization; it is not
         * tracked by liveness (its uses are inline immediates or the
         * materializing Move's own def). */
        T2_LIR_VF_CONST = 1 << 0,
        /* The value is an untagged machine word (UntagInt/MulRaw class).
         * It must never be homed in an X/Y slot nor appear in any sync
         * map (PLAN/T2/04 §11.2); the allocator and validators enforce
         * both. Never set before the speculation phase emits the ops. */
        T2_LIR_VF_UNTAGGED = 1 << 1
    };

    /* A compact human-readable dump, mirroring t2_dump for the HIR. */
    std::string t2_lir_dump(const T2LirFunction &fn);

} /* namespace erts_t2 */

#endif /* _JIT_T2_LIR_HPP */
