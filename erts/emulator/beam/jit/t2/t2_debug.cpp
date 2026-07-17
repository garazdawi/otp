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
 * T2-Full tier-2 JIT: SSA-reconstruction debug serializer + BIF backing
 * (PLAN/T2FULL/07 §6). Reachable from Erlang through
 *
 *     erts_debug:get_internal_state({t2_build_ssa, M, F, A})
 *
 * (a new `case 4:` in erl_bif_info.c's get_internal_state tuple switch).
 * It looks up module M's active instance, runs the BEAM->SSA builder
 * (t2_hir_builder.cpp) on the retained code chunk for F/A, and turns the
 * resulting T2Function into a structured Erlang term meant for machine
 * comparison (the G1 fidelity gate), not for pretty-printing.
 *
 * ==================================================================
 * Term format (all tuples / lists / atoms / integers; no maps, no
 * boxed literals so nothing dangles once the transient module decode
 * is released):
 *
 *   Result :=
 *       undefined                       % module not loaded / not retained
 *     | {error, not_found}              % no such F/A in the module
 *     | {error, not_eligible}           % F/A found but not tier-2 eligible
 *     | {error, build_failed, RsnBin}   % decode/build/validate failed
 *     | {ok, Fn}
 *
 *   Fn      := {Mfa, Counts, Blocks}
 *   Mfa     := {M, F, A}                          % M,F atoms; A int
 *   Counts  := {NBlocks, NValues, NPhis, NBody, NTerm}
 *                % NPhis/NBody/NTerm are totals over all blocks; NTerm is
 *                % the number of blocks that carry a terminator. The
 *                % +JT2dump HIR text prints, per block, phis then body then
 *                % the terminator, so its op-line count == NPhis+NBody+NTerm.
 *   Blocks  := [ Block ]                          % in block-id order 0..N-1
 *
 *   Block   := {Id, Preds, Succs, Phis, Body, Term}
 *   Preds   := [ BlockId ]                        % ascending (finalize order)
 *   Succs   := [ BlockId ]                        % terminator edge order:
 *                % branch -> [Then,Else]; jump -> [Target];
 *                % switch -> Case targets ++ [Default]; return/tailcall -> []
 *   Phis    := [ Op ]
 *   Body    := [ Op ]                             % body ops, program order
 *   Term    := Op | none
 *
 *   Op      := {Kind, Result, Type, Operands, Attrs}
 *   Kind    := atom                               % t2_op_kind_name()
 *   Result  := ValueId(int) | none                % SSA value id
 *   Type    := TypeTerm
 *   Operands:= [ OperandRef ]
 *   OperandRef :=
 *         ValueId(int)                            % ordinary op
 *       | {ValueId(int), PredBlockId(int)}        % phi input (value, edge)
 *   Attrs   := [ {Key, Val} ]                     % proplist; keys per kind:
 *                % index => int      (param/get_tuple_element/test_arity/
 *                %                     is_tagged_tuple/const_literal idx)
 *                % value => int|atom (const_int/const_atom/const_nil)
 *                % tag   => atom     (is_tagged_tuple record tag)
 *                % mfa   => {M,F,A}  (call / call_ext / tail_call / arith bif)
 *                % cases => [ {V,TargetBlockId} ]  (switch; V=literal for
 *                %                                  boxed match values)
 *                % -- P1 metadata (additive; the G1 comparator ignores
 *                %    unknown attrs and folds the frame/copy bookkeeping
 *                %    ops to `ignore`) --
 *                % frame => int      (allocate/deallocate slot count)
 *                % heap  => int      (allocate fused heap words; gc_test
 *                %                    heap words ride `index`)
 *                % live  => int      (allocate/gc_test/arith GC live count)
 *                % trim/remaining => int (trim drop / survivor count)
 *                % dst   => {x,N} | {y,N}   (canonical home of the result)
 *                % sync  => {XLive, [XValueId], Frame | none, [YValueId]}
 *                %          (sync-point register map; see t2_hir.hpp)
 *                % err_exit => op | shared  (error-exit lowering class)
 *                % (the function-entry sync map is not serialized; the
 *                %  +JT2dump HIR text carries it)
 *
 *   TypeTerm :=
 *         any
 *       | none
 *       | {Union(int), Min, Max, Unit(int)}       % Union = BEAM_TYPE_* mask
 *                % (see beam_types.h; e.g. BEAM_TYPE_INTEGER = 1<<3).
 *                % Min/Max := int | '*'  ('*' == unbounded)
 * ==================================================================
 *
 * Everything above is built with the two-pass erts_bld_* idiom (size
 * with hpp=NULL, then HAlloc, then build), mirroring
 * erts_recv_stats_term (erl_process.c). Serialization runs inside the
 * builder's `emit` callback, while the module decode -- and any dynamic
 * literals it synthesized from the code chunk -- are still alive.
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
#include "erl_bits.h"
#include "beam_types.h"
#include "code_ix.h"
#include "module.h"

#include "t2_retain.h"
}

namespace erts_t2 {

    namespace {

        /* Atoms interned once per serialization. Everything here is an
         * immediate, so it is safe to reference from the process-heap
         * result term regardless of module lifetime. */
        struct Atoms {
            Eterm none;
            Eterm any;
            Eterm star;
            Eterm value;
            Eterm index;
            Eterm tag;
            Eterm mfa;
            Eterm cases;
            Eterm literal;
            Eterm frame;
            Eterm heap;
            Eterm live;
            Eterm trim;
            Eterm remaining;
            Eterm dst;
            Eterm sync;
            Eterm err_exit;
            Eterm op;
            Eterm shared;
            Eterm x;
            Eterm y;

            static Eterm intern(const char *s) {
                return erts_atom_put((const byte *)s,
                                     (int)sys_strlen(s),
                                     ERTS_ATOM_ENC_LATIN1,
                                     1);
            }

            void init() {
                none = intern("none");
                any = intern("any");
                star = intern("*");
                value = intern("value");
                index = intern("index");
                tag = intern("tag");
                mfa = intern("mfa");
                cases = intern("cases");
                literal = intern("literal");
                frame = intern("frame");
                heap = intern("heap");
                live = intern("live");
                trim = intern("trim");
                remaining = intern("remaining");
                dst = intern("dst");
                sync = intern("sync");
                err_exit = intern("err_exit");
                op = intern("op");
                shared = intern("shared");
                x = intern("x");
                y = intern("y");
            }
        };

        /* Serializes a validated T2Function with the two-pass idiom. One
         * instance is walked twice: once with hpp == nullptr to size, once
         * with a real heap pointer to build. */
        struct Serializer {
            Atoms atoms;

            Eterm *hp; /* build pass */
            Uint *szp; /* size pass; nullptr during build */

            void init_atoms() {
                atoms.init();
            }

            Eterm kind_atom(T2OpKind kind) {
                return Atoms::intern(t2_op_kind_name(kind));
            }

            Eterm uint_term(Uint u) {
                return erts_bld_uint(szp ? nullptr : &hp, szp, u);
            }

            Eterm sint_term(Sint64 s) {
                return erts_bld_sint64(szp ? nullptr : &hp, szp, s);
            }

            Eterm cons(Eterm car, Eterm cdr) {
                return erts_bld_cons(szp ? nullptr : &hp, szp, car, cdr);
            }

            Eterm tuple2(Eterm a, Eterm b) {
                return erts_bld_tuple(szp ? nullptr : &hp, szp, 2, a, b);
            }

            Eterm tuple3(Eterm a, Eterm b, Eterm c) {
                return erts_bld_tuple(szp ? nullptr : &hp, szp, 3, a, b, c);
            }

            /* Build a proplist entry {Key, Val} consed onto `tail`. */
            Eterm prop(Eterm key, Eterm val, Eterm tail) {
                return cons(tuple2(key, val), tail);
            }

            Eterm build_type(const T2Type &t) {
                if (t.is_none()) {
                    return atoms.none;
                }
                if (t.is_any()) {
                    return atoms.any;
                }

                Eterm mn = t.has_min ? sint_term(t.min) : atoms.star;
                Eterm mx = t.has_max ? sint_term(t.max) : atoms.star;

                return erts_bld_tuple(szp ? nullptr : &hp,
                                      szp,
                                      4,
                                      make_small(t.type_union),
                                      mn,
                                      mx,
                                      make_small(t.unit));
            }

            /* Immediate values are safe to embed directly; boxed match
             * values (module or dynamic literals) are replaced by the
             * `literal` placeholder so nothing dangles after the decode is
             * released. */
            Eterm safe_value(Eterm v) {
                return is_immed(v) ? v : atoms.literal;
            }

            Eterm build_mfa(Eterm m, Eterm f, Uint a) {
                return tuple3(m, f, uint_term(a));
            }

            Eterm build_reg(int32_t reg) {
                return tuple2(t2_reg_is_y(reg) ? atoms.y : atoms.x,
                              uint_term(t2_reg_index(reg)));
            }

            Eterm build_sync(const T2SyncMap *m) {
                Eterm xs = NIL;
                Eterm ys = NIL;

                for (uint32_t i = m->x_live; i > 0; i--) {
                    xs = cons(make_small(m->x[i - 1]->id), xs);
                }
                if (m->frame_size != T2_NO_FRAME) {
                    for (int32_t i = m->frame_size; i > 0; i--) {
                        ys = cons(make_small(m->y[i - 1]->id), ys);
                    }
                }

                Eterm frame = m->frame_size == T2_NO_FRAME
                                      ? atoms.none
                                      : uint_term((Uint)m->frame_size);

                return erts_bld_tuple(szp ? nullptr : &hp,
                                      szp,
                                      4,
                                      uint_term(m->x_live),
                                      xs,
                                      frame,
                                      ys);
            }

            Eterm build_attrs(const T2Op *op) {
                Eterm tail = NIL;

                /* P1 metadata common to every op kind. */
                if (op->sync != nullptr) {
                    tail = prop(atoms.sync, build_sync(op->sync), tail);
                }
                if (op->dst_reg != T2_REG_NONE) {
                    tail = prop(atoms.dst, build_reg(op->dst_reg), tail);
                }
                if (op->flags & T2_OP_ERR_EXIT_OP) {
                    tail = prop(atoms.err_exit, atoms.op, tail);
                }
                if (op->flags & T2_OP_ERR_EXIT_SHARED) {
                    tail = prop(atoms.err_exit, atoms.shared, tail);
                }

                switch (op->kind) {
                case T2OpKind::Allocate:
                    tail = prop(atoms.frame, uint_term(op->index), tail);
                    tail = prop(atoms.heap, uint_term((Uint)op->imm_int), tail);
                    tail = prop(atoms.live, uint_term(op->live), tail);
                    break;
                case T2OpKind::Deallocate:
                    tail = prop(atoms.frame, uint_term(op->index), tail);
                    break;
                case T2OpKind::Trim:
                    tail = prop(atoms.trim, uint_term(op->index), tail);
                    tail = prop(atoms.remaining,
                                uint_term((Uint)op->imm_int),
                                tail);
                    break;
                case T2OpKind::GcTest:
                    tail = prop(atoms.index, uint_term(op->index), tail);
                    tail = prop(atoms.live, uint_term(op->live), tail);
                    break;
                case T2OpKind::ConstInt:
                    tail = prop(atoms.value, sint_term(op->imm_int), tail);
                    break;
                case T2OpKind::ConstAtom:
                case T2OpKind::ConstNil:
                    /* imm_term is an atom or NIL: an immediate. */
                    tail = prop(atoms.value, safe_value(op->imm_term), tail);
                    break;
                case T2OpKind::ConstLiteral:
                    tail = prop(atoms.index, uint_term(op->index), tail);
                    break;
                case T2OpKind::Param:
                case T2OpKind::GetTupleElement:
                case T2OpKind::TestArity:
                    tail = prop(atoms.index, uint_term(op->index), tail);
                    break;
                case T2OpKind::IsTaggedTuple:
                    tail = prop(atoms.tag, safe_value(op->imm_term), tail);
                    tail = prop(atoms.index, uint_term(op->index), tail);
                    break;
                case T2OpKind::Call:
                case T2OpKind::CallExt:
                case T2OpKind::TailCall:
                case T2OpKind::TailCallExt:
                    tail = prop(atoms.mfa,
                                build_mfa(op->mfa_m, op->mfa_f, op->index),
                                tail);
                    break;
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
                case T2OpKind::GuardBif:
                    /* Arithmetic/BIF ops carry their originating MFA
                     * (unset ops leave mfa_m zero-initialized by the arena). */
                    if (is_atom(op->mfa_m)) {
                        tail = prop(atoms.mfa,
                                    build_mfa(op->mfa_m,
                                              op->mfa_f,
                                              op->num_operands),
                                    tail);
                    }
                    tail = prop(atoms.live, uint_term(op->live), tail);
                    break;
                case T2OpKind::Switch: {
                    Eterm clist = NIL;

                    for (uint32_t i = op->num_cases; i > 0; i--) {
                        const T2SwitchCase &c = op->cases[i - 1];
                        Eterm entry = tuple2(safe_value(c.value),
                                             uint_term(c.target->id));
                        clist = cons(entry, clist);
                    }
                    tail = prop(atoms.cases, clist, tail);
                    break;
                }
                default:
                    break;
                }

                return tail;
            }

            Eterm build_operands(const T2Op *op) {
                Eterm list = NIL;

                for (uint16_t i = op->num_operands; i > 0; i--) {
                    const T2Value *v = op->operands[i - 1];
                    Eterm ref;

                    if (op->kind == T2OpKind::Phi && op->phi_blocks != NULL) {
                        ref = tuple2(make_small(v->id),
                                     make_small(op->phi_blocks[i - 1]->id));
                    } else {
                        ref = make_small(v->id);
                    }
                    list = cons(ref, list);
                }

                return list;
            }

            Eterm build_op(const T2Op *op) {
                Eterm kind = kind_atom(op->kind);
                Eterm result = op->result != NULL ? make_small(op->result->id)
                                                  : atoms.none;
                Eterm type = build_type(op->type);
                Eterm operands = build_operands(op);
                Eterm attrs = build_attrs(op);

                return erts_bld_tuple(szp ? nullptr : &hp,
                                      szp,
                                      5,
                                      kind,
                                      result,
                                      type,
                                      operands,
                                      attrs);
            }

            /* Terminator successors, in edge order (matches for_each_succ
             * in t2_hir.cpp). */
            Eterm build_succs(const T2Op *term) {
                if (term == NULL) {
                    return NIL;
                }

                switch (term->kind) {
                case T2OpKind::Branch:
                    return cons(make_small(term->succ_then->id),
                                cons(make_small(term->succ_else->id), NIL));
                case T2OpKind::Jump:
                    return cons(make_small(term->succ_then->id), NIL);
                case T2OpKind::Switch: {
                    Eterm list =
                            cons(make_small(term->default_target->id), NIL);

                    for (uint32_t i = term->num_cases; i > 0; i--) {
                        list = cons(make_small(term->cases[i - 1].target->id),
                                    list);
                    }
                    return list;
                }
                default:
                    /* Return, tail calls: no successors. */
                    return NIL;
                }
            }

            Eterm build_op_list(const T2Op *head) {
                /* Reverse-walk to cons in program order. Collect then emit. */
                std::vector<const T2Op *> ops;

                for (const T2Op *op = head; op != NULL; op = op->next) {
                    ops.push_back(op);
                }

                Eterm list = NIL;
                for (size_t i = ops.size(); i > 0; i--) {
                    list = cons(build_op(ops[i - 1]), list);
                }
                return list;
            }

            Eterm build_preds(const T2BasicBlock *b) {
                Eterm list = NIL;

                for (uint32_t i = b->num_preds; i > 0; i--) {
                    list = cons(make_small(b->preds[i - 1]->id), list);
                }
                return list;
            }

            Eterm build_block(const T2BasicBlock *b) {
                Eterm id = make_small(b->id);
                Eterm preds = build_preds(b);
                Eterm succs = build_succs(b->terminator);
                Eterm phis = build_op_list(b->phis_head);
                Eterm body = build_op_list(b->ops_head);
                Eterm term = b->terminator != NULL ? build_op(b->terminator)
                                                   : atoms.none;

                return erts_bld_tuple(szp ? nullptr : &hp,
                                      szp,
                                      6,
                                      id,
                                      preds,
                                      succs,
                                      phis,
                                      body,
                                      term);
            }

            Eterm build_function(const T2Function &fn) {
                Uint nphis = 0, nbody = 0, nterm = 0;

                for (const T2BasicBlock *b : fn.blocks) {
                    for (const T2Op *p = b->phis_head; p != NULL; p = p->next) {
                        nphis++;
                    }
                    for (const T2Op *o = b->ops_head; o != NULL; o = o->next) {
                        nbody++;
                    }
                    if (b->terminator != NULL) {
                        nterm++;
                    }
                }

                Eterm mfa = build_mfa(fn.module, fn.function, fn.arity);
                Eterm counts =
                        erts_bld_tuple(szp ? nullptr : &hp,
                                       szp,
                                       5,
                                       make_small((Uint)fn.blocks.size()),
                                       make_small((Uint)fn.values.size()),
                                       uint_term(nphis),
                                       uint_term(nbody),
                                       uint_term(nterm));

                Eterm blocks = NIL;
                for (size_t i = fn.blocks.size(); i > 0; i--) {
                    blocks = cons(build_block(fn.blocks[i - 1]), blocks);
                }

                return tuple3(mfa, counts, blocks);
            }
        };

        /* Two-pass serialization onto p's heap, returning {ok, Fn}. */
        Eterm serialize(Process *p, const T2Function &fn) {
            Serializer s;
            Uint sz = 0;

            s.init_atoms();

            /* Size pass. */
            s.hp = NULL;
            s.szp = &sz;
            (void)s.build_function(fn);
            sz += 3; /* {ok, Fn} tuple */

            /* Build pass. (HAlloc compares the word count against the
             * signed HeapWordsLeft; cast so GCC's -Wsign-compare is happy.) */
            Eterm *hp = HAlloc(p, (Sint)sz);
            s.hp = hp;
            s.szp = NULL;
            Eterm fn_term = s.build_function(fn);
            Eterm result = TUPLE2(s.hp, am_ok, fn_term);
            s.hp += 3;

            ASSERT(s.hp == hp + sz);
            return result;
        }

    } /* anonymous namespace */

} /* namespace erts_t2 */

using namespace erts_t2;

extern "C" Eterm erts_t2_debug_build_ssa(Process *p,
                                         Eterm mod,
                                         Eterm func,
                                         Eterm arity) {
    Module *modp;
    const ErtsT2RetainedCode *ret;
    Uint ar;

    if (!is_atom(mod) || !is_atom(func) || !is_small(arity)) {
        return am_undefined;
    }
    if (signed_val(arity) < 0 || signed_val(arity) > MAX_ARG) {
        return am_undefined;
    }
    ar = unsigned_val(arity);

    /* The active instance is stable for the duration of this BIF: a
     * concurrent load stages into a new code_ix and only the old
     * (previous) instance is purged, never the active one out from under
     * a running process. */
    modp = erts_get_module(mod, erts_active_code_ix());
    if (modp == NULL || modp->curr.t2_retained == NULL) {
        return am_undefined;
    }
    ret = modp->curr.t2_retained;

    Eterm result = am_undefined;
    std::string err;

    T2BuildStatus status = t2_build_for_debug(
            ret,
            func,
            (unsigned)ar,
            [&](T2Function &fn) {
                result = serialize(p, fn);
            },
            &err);

    switch (status) {
    case T2BuildStatus::Ok:
        return result;
    case T2BuildStatus::NotFound: {
        Eterm *hp = HAlloc(p, 3);
        return TUPLE2(hp, am_error, Atoms::intern("not_found"));
    }
    case T2BuildStatus::NotEligible: {
        Eterm *hp = HAlloc(p, 3);
        return TUPLE2(hp, am_error, Atoms::intern("not_eligible"));
    }
    case T2BuildStatus::Failed:
    default: {
        Eterm reason;
        Eterm *hp;
        Uint sz = 0;
        Eterm rbin;

        (void)erts_bld_string_n(NULL, &sz, err.c_str(), (Sint)err.size());
        sz += 4; /* {error, build_failed, Reason} */
        hp = HAlloc(p, (Sint)sz);
        rbin = erts_bld_string_n(&hp, NULL, err.c_str(), (Sint)err.size());
        reason = TUPLE3(hp, am_error, Atoms::intern("build_failed"), rbin);
        return reason;
    }
    }
}

/* ==================================================================== *
 * The entry-type profile dump (PLAN/T2FULL/02 §7.2). Reached through    *
 *                                                                       *
 *     erts_debug:get_internal_state({t2_profile, Module})               *
 *                                                                       *
 * Walks module M's active-instance profile block and returns one tuple  *
 * per armed (loop-shaped, installable) function -- the set the tier-up  *
 * profiler actually samples:                                            *
 *                                                                       *
 *   [ {FnIndex, Arity, Count, Nonsmall, [T0, T1, T2, T3]} ]             *
 *                                                                       *
 * Tn is the ERTS_T2_TY_* union bitmask observed for argument n (0 for   *
 * unsampled args). Everything is an immediate or a fresh cons/tuple on  *
 * the caller's heap. am_undefined when M is unloaded/unretained, [] if  *
 * it has no profile block, am_badarg for a non-atom module.             *
 * ==================================================================== */
extern "C" Eterm erts_t2_debug_profile(Process *p, Eterm mod) {
    Module *modp;
    const ErtsT2RetainedCode *ret;
    Uint sz;
    Uint *szp;
    Eterm *hp;
    Eterm result;

    if (!is_atom(mod)) {
        return am_badarg;
    }
    modp = erts_get_module(mod, erts_active_code_ix());
    if (modp == NULL || modp->curr.t2_retained == NULL) {
        return am_undefined;
    }
    ret = modp->curr.t2_retained;
    if (ret->profiles == NULL) {
        return NIL;
    }

    /* Two-pass build: size (szp set, hpp NULL), HAlloc, then materialize
     * (szp NULL, hpp set), the same erts_bld_* idiom the census uses. */
    sz = 0;
    szp = &sz;
    hp = NULL;
    result = NIL;
    for (;;) {
        Eterm **hpp = szp ? NULL : &hp;
        Eterm list = NIL;
        int i;

        for (i = ret->function_count - 1; i >= 0; i--) {
            const ErtsT2Profile *rec =
                    (const ErtsT2Profile *)((const byte *)ret->profiles +
                                            (size_t)i * ERTS_T2_PROFILE_STRIDE);
            Eterm types, fnix, art, cnt, ns, tup;
            int a;

            /* Only functions the profiler arms (loop-shaped installable);
             * threshold != 0 stays true through the PENDING sentinel. */
            if (rec->threshold == 0) {
                continue;
            }

            Eterm fun_info;

            types = NIL;
            for (a = ERTS_T2_PROFILE_ARGS - 1; a >= 0; a--) {
                Eterm tb = erts_bld_uint(hpp, szp, rec->seen_types[a]);
                types = erts_bld_cons(hpp, szp, tb, types);
            }
            /* Monomorphic-target status: none | poly | {mono, Arg, Flags}
             * (Flags bit0 env-free, bit1 closure -- see ERTS_T2_FUNF_*). */
            if (rec->seen_fun == NULL) {
                fun_info = Atoms::intern("none");
            } else if (rec->seen_fun == ERTS_T2_FUN_POLY) {
                fun_info = Atoms::intern("poly");
            } else {
                Eterm fa = erts_bld_uint(hpp, szp, rec->fun_arg);
                Eterm ff = erts_bld_uint(hpp, szp, rec->fun_flags);
                fun_info = erts_bld_tuple(hpp,
                                          szp,
                                          3,
                                          Atoms::intern("mono"),
                                          fa,
                                          ff);
            }
            /* S1b.3b map-shape sampling: none | poly | {mono, Arg, Ptr},
             * where Ptr is the tagged keys-tuple pointer as an integer
             * (comparable to {map_keys_ptr, Map}). */
            Eterm mshape;
            if (rec->map_shape == (Eterm)0) {
                mshape = Atoms::intern("none");
            } else if (rec->map_shape == ERTS_T2_MAP_SHAPE_POLY) {
                mshape = Atoms::intern("poly");
            } else {
                Eterm sa = erts_bld_uint(hpp, szp, rec->map_shape_arg);
                Eterm sp = erts_bld_uword(hpp, szp, (UWord)rec->map_shape);
                mshape = erts_bld_tuple(hpp,
                                        szp,
                                        3,
                                        Atoms::intern("mono"),
                                        sa,
                                        sp);
            }
            fnix = erts_bld_uint(hpp, szp, rec->fn_index);
            art = erts_bld_uint(hpp, szp, rec->arity);
            cnt = erts_bld_uint(hpp, szp, rec->count);
            ns = erts_bld_uint(hpp, szp, rec->nonsmall);
            tup = erts_bld_tuple(hpp,
                                 szp,
                                 7,
                                 fnix,
                                 art,
                                 cnt,
                                 ns,
                                 types,
                                 fun_info,
                                 mshape);
            list = erts_bld_cons(hpp, szp, tup, list);
        }

        if (szp) {
            hp = HAlloc(p, (Sint)sz);
            szp = NULL;
        } else {
            result = list;
            break;
        }
    }

    return result;
}

/* ==================================================================== *
 * The addressable-share census term serializer (PLAN/T2FULL/17 §3 +     *
 * 19 §2 S0). Reached through                                            *
 *                                                                       *
 *     erts_debug:get_internal_state({t2_census, BeamBinary})            *
 *                                                                       *
 * BeamBinary is a raw .beam (e.g. from code:get_object_code/1). A         *
 * throwaway BeamFile is parsed, the census scan is run over it, and one  *
 * tuple per function is returned:                                        *
 *                                                                        *
 *   [ {Name, Arity, Size, Eligible, LoopShaped, Classes} ]              *
 *   Classes = [ {ClassAtom, Total, InLoop, OutLoop} ]  (Total>0 only)   *
 *                                                                        *
 * ClassAtom in call_fun | maps | bs_construction | bs_position |        *
 *   exceptions | recv | float_reg | general_bif | other. Everything is  *
 * an immediate or a fresh cons/tuple on the caller's heap, so nothing   *
 * dangles once the transient BeamFile decode is freed. Returns badarg   *
 * for a non-binary and {error, bad_beam} for an unparseable one.        *
 * ==================================================================== */
extern "C" Eterm erts_t2_debug_census(Process *p, Eterm bin) {
    const byte *temp_alloc = NULL;
    const byte *code;
    Uint size;
    BeamFile beam;
    enum beamfile_read_result rr;
    ErtsT2CensusFn *fns = NULL;
    int nfns = 0;
    int i, c;
    Eterm class_atoms[ERTS_T2_BLK__COUNT];
    static const char *const class_names[ERTS_T2_BLK__COUNT] = {
            "call_fun",
            "maps",
            "bs_construction",
            "bs_position",
            "exceptions",
            "recv",
            "float_reg",
            "general_bif",
            "other"};
    Uint sz;
    Uint *szp;
    Eterm *hp;
    Eterm result;

    code = erts_get_aligned_binary_bytes(bin, &size, &temp_alloc);
    if (code == NULL) {
        return am_badarg;
    }

    rr = beamfile_read(code, size, &beam);
    if (rr != BEAMFILE_READ_SUCCESS) {
        Eterm *ehp = HAlloc(p, 3);
        erts_free_aligned_binary_bytes(temp_alloc);
        return TUPLE2(ehp, am_error, Atoms::intern("bad_beam"));
    }

    (void)erts_t2_census_scan(&beam, &fns, &nfns);

    for (c = 0; c < ERTS_T2_BLK__COUNT; c++) {
        class_atoms[c] = Atoms::intern(class_names[c]);
    }

    /* Two-pass build: size (szp set, hpp NULL), HAlloc, then materialize
     * (szp NULL, hpp set), exactly the erts_bld_* idiom used above. */
    sz = 0;
    szp = &sz;
    hp = NULL;
    result = NIL;
    for (;;) {
        Eterm **hpp = szp ? NULL : &hp;
        Eterm list = NIL;

        for (i = nfns - 1; i >= 0; i--) {
            ErtsT2CensusFn *f = &fns[i];
            Eterm classes = NIL;
            Eterm ar, szt, fnt;

            for (c = ERTS_T2_BLK__COUNT - 1; c >= 0; c--) {
                Eterm t, il, ol, tup;
                if (f->total[c] == 0) {
                    continue;
                }
                t = erts_bld_uint(hpp, szp, f->total[c]);
                il = erts_bld_uint(hpp, szp, f->in_loop[c]);
                ol = erts_bld_uint(hpp, szp, f->out_loop[c]);
                tup = erts_bld_tuple(hpp, szp, 4, class_atoms[c], t, il, ol);
                classes = erts_bld_cons(hpp, szp, tup, classes);
            }

            ar = erts_bld_uint(hpp, szp, f->arity);
            szt = erts_bld_uint(hpp, szp, f->size);
            fnt = erts_bld_tuple(hpp,
                                 szp,
                                 6,
                                 f->name,
                                 ar,
                                 szt,
                                 f->eligible ? am_true : am_false,
                                 f->loop_shaped ? am_true : am_false,
                                 classes);
            list = erts_bld_cons(hpp, szp, fnt, list);
        }

        if (szp) {
            /* HAlloc compares the word count against the signed
             * HeapWordsLeft; cast so GCC's -Wsign-compare is happy
             * (same as erts_t2_debug_build_ssa above). */
            hp = HAlloc(p, (Sint)sz);
            szp = NULL;
        } else {
            result = list;
            break;
        }
    }

    if (fns != NULL) {
        erts_free(ERTS_ALC_T_T2_CODE, fns);
    }
    beamfile_free(&beam);
    erts_free_aligned_binary_bytes(temp_alloc);
    return result;
}
