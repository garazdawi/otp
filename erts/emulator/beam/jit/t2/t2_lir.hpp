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
        MakeList,
        MakeTuple,

        /* Heap reservation (emit_gc_test / emit_test_heap). */
        GcTest,

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

        /* An unconditional side-exit: branch to t1_pc_fail. Used to lower
         * error-exit blocks (badmatch/if_end/case_end) -- T2 never raises
         * (surprise #7). */
        SideExit,

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

        constexpr T2LirSrc() : is_const(false), loc(), term(0) {}

        static T2LirSrc slot(PhysLoc l) {
            T2LirSrc s;
            s.is_const = false;
            s.loc = l;
            return s;
        }
        static T2LirSrc immediate(Eterm t) {
            T2LirSrc s;
            s.is_const = true;
            s.term = t;
            return s;
        }
    };

    struct T2LirOp {
        T2LirKind kind;

        PhysLoc dst;                 /* result slot, or None            */
        T2LirSrc srcs[T2_LIR_MAX_SRCS];
        uint8_t num_srcs;

        /* Op-specific immediates. */
        Sint64 imm;    /* raw immediate (tuple/element index, arity, ...) */
        Eterm imm_term;/* tagged immediate (Move of const / switch key)    */

        /* Call target (Call/TailCall/CallExt/TailCallExt and arith BIF
         * provenance). */
        Eterm mfa_m;
        Eterm mfa_f;
        uint32_t arity;      /* call arity / heap need / gc live count etc. */
        const void *exp;     /* resolved Export* for a remote call          */
        const void *target;  /* resolved ErtsCodePtr for a local call/tail  */

        /* The T1 PC this op side-exits to on failure (arith/guard/type
         * test) or unconditionally (SideExit / error-block lowering). NULL
         * when the op has no fail edge. */
        const void *t1_pc_fail;

        /* Shared decode ordinal with the HIR/pctab (PLAN/T2FULL/07 §4). */
        uint32_t beam_idx;

        /* Terminator CFG edges (block ids into T2LirFunction::blocks). */
        uint32_t succ_then;
        uint32_t succ_else;

        /* Switch cases: [first_case, first_case+num_cases) index the
         * function-level `switch_cases` pool. default_target is a block. */
        uint32_t first_case;
        uint32_t num_cases;
        uint32_t default_target;

        T2LirOp()
                : kind(T2LirKind::Invalid), dst(), num_srcs(0), imm(0),
                  imm_term(0), mfa_m(0), mfa_f(0), arity(0), exp(nullptr),
                  target(nullptr), t1_pc_fail(nullptr), beam_idx(0),
                  succ_then(0), succ_else(0), first_case(0), num_cases(0),
                  default_target(0) {}
    };

    /* One arm of a Switch terminator. */
    struct T2LirSwitchCase {
        Eterm value;         /* matched term (small / atom / literal)   */
        uint32_t target;     /* block id                                */
    };

    struct T2LirBlock {
        uint32_t id;
        std::vector<T2LirOp> ops; /* flat vector, terminator last */
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

        /* Isel emits LIR operands as PhysLoc::phys(value_id) placeholders
         * that regalloc lowers to canonical X/Y slots. num_values sizes the
         * value->slot maps; param_x[value_id] is the fixed X slot of a
         * parameter value (== its parameter index), or -1 for non-params. */
        uint32_t num_values;
        std::vector<int32_t> param_x;

        T2LirFunction()
                : module(0), function(0), arity(0), t1_entry(nullptr),
                  num_values(0) {}

        T2LirBlock &new_block() {
            T2LirBlock b;
            b.id = (uint32_t)blocks.size();
            blocks.push_back(std::move(b));
            return blocks.back();
        }
    };

    /* A compact human-readable dump, mirroring t2_dump for the HIR. */
    std::string t2_lir_dump(const T2LirFunction &fn);

} /* namespace erts_t2 */

#endif /* _JIT_T2_LIR_HPP */
