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
 * T2-Full tier-2 JIT: LIR -> asmjit emission (PLAN/T2FULL/08 §1, §2).
 *
 * BeamT2ModuleAssembler derives from the aarch64 BeamModuleAssembler so
 * it inherits the register defs, mov_arg, emit_enter/leave_runtime,
 * emit_gc_test, the veneer machinery, and the `protected` per-op T1
 * emitters. For an identity op it synthesizes the loader ArgVal family
 * from the LIR slot and calls the reused T1 emitter, redirecting the
 * emitter's `Fail` label to a small in-blob trampoline that branches to
 * the op's T1 PC (Fail->T1-PC, §2). A fresh assembler is created per
 * blob so no loader per-module state leaks in.
 */

#include "beam_asm.hpp"

#include "asmjit/core/logger.h"

extern "C"
{
#include "beam_common.h"
#include "global.h"
#include "erl_vm.h"
#include "code_ix.h"
#include "export.h"
#include "module.h"

#include "t2_retain.h"
#include "t2_pctab.h"
#include "t2_install.h"
}

#include "t2_lir.hpp"
#include "t2_emit.hpp"
#include "t2_hir.hpp"
#include "t2_isel.hpp"

#include <cstdio>
#include <functional>
#include <set>
#include <unordered_map>
#include <vector>

using namespace asmjit;

/* Defined in beam_jit_main.cpp, where the process-wide global assembler
 * and JIT allocator are file-static. */
BeamGlobalAssembler *erts_t2_global_assembler(void);
JitAllocator *erts_t2_jit_allocator(void);

namespace erts_t2 {

    namespace {

        /* A synthetic, read-only BeamFile giving getTypeEntry() a valid
         * "any" type table for typeIndex 0. The identity transform pins
         * every synthesized register operand's type slot to 0 (any), so
         * the reused emitters always take their conservative,
         * fully-checked path -- exactly what "no optimizations" wants.
         * lambdas.count is 0 so the constructor emits no fun trampolines. */
        const BeamFile *t2_synth_beamfile() {
            static BeamType any_type;
            static BeamFile bf;
            static bool inited = false;

            if (!inited) {
                any_type.type_union = BEAM_TYPE_ANY;
                any_type.metadata_flags = 0;
                any_type.min = 0;
                any_type.max = 0;
                any_type.size_unit = 0;

                bf.types.count = 1;
                bf.types.fallback = 1;
                bf.types.entries = &any_type;

                inited = true;
            }

            return &bf;
        }

    } /* anonymous namespace */

    /* ------------------------------------------------------------------ *
     * The per-blob assembler                                             *
     * ------------------------------------------------------------------ */

    class BeamT2ModuleAssembler : public BeamModuleAssembler {
        const T2LirFunction &fn;

        /* Entry label bound at the very first byte of the blob. */
        Label entry_label;

        /* Distinct T1 fail PCs -> the BEAM label number of the in-blob
         * trampoline that branches to them. */
        std::unordered_map<const void *, unsigned> fail_labels;
        unsigned next_label;

        /* When non-null, emit_all lays down the install entry stub
         * (PLAN/T2/06 §2.3) instead of the exec-harness frame prologue;
         * the value is the function's public T1 entry L_f. */
        const void *install_entry = nullptr;

        std::string emit_error;

    public:
        /* Emit-time facts, recorded for the structural asserts of
         * T2_EMIT_SELFTEST (repeatable verification that CPs, transfer
         * targets and side exits carry the exact cross-tier addresses
         * the pctab/loaded module prescribe). */
        struct EmitFact {
            enum Kind {
                CpCont,     /* value = the T1 continuation moved into LR  */
                CallTarget, /* value = local callee T1 entry              */
                CallExport, /* value = Export* of a remote callee         */
                TailTarget, /* value = local tail-callee T1 entry         */
                TailExport, /* value = Export* of a remote tail-callee    */
                SideExitPc, /* value = unconditional side-exit T1 PC      */
                FailExitPc, /* value = arith side-exit trampoline T1 PC   */
                BifExport,  /* value = Export* of a light-BIF callee      */
                BifSitePc,  /* value = the BIF site's own T1 PC (ARG3:
                             * yield resume + raise address)              */
                BifContPc   /* value = the BIF site's T1 continuation
                             * (ARG5: trap/trace CP)                      */
            } kind;
            uint32_t beam_idx;
            uint64_t value;
        };
        std::vector<EmitFact> facts;

    private:
        void fact(EmitFact::Kind kind, uint32_t beam_idx, const void *value) {
            facts.push_back(EmitFact{kind, beam_idx, (uint64_t)value});
        }

    public:
        /* NB: the base constructor eagerly creates rawLabels
         * 1..num_labels-1, and the DEBUG codegen verifier requires every
         * created label to be bound — so num_labels must be exactly
         * 1 + block count (block b binds label 1+b; fail trampolines
         * allocate + bind their own labels from next_label up). */
        BeamT2ModuleAssembler(BeamGlobalAssembler *ga,
                              Eterm mod,
                              int num_labels,
                              const T2LirFunction &fn_)
                : BeamModuleAssembler(ga, mod, num_labels, t2_synth_beamfile()),
                  fn(fn_),
                  next_label((unsigned)fn_.blocks.size() + 1) {
            entry_label = a.new_label();
        }

        void set_string_logger(StringLogger *slog) {
            code.set_logger(slog);
        }

        void set_install_entry(const void *lf) {
            install_entry = lf;
        }

        /* Span of the finalized blob (valid after finalize_to_allocator),
         * for t2_ranges registration and eventual release. */
        const void *blob_base = nullptr;
        size_t blob_size = 0;

        bool failed() const {
            return !emit_error.empty();
        }
        const std::string &error() const {
            return emit_error;
        }

        /* Emit the whole blob: entry, enter-frame prologue, the body, then
         * the Fail->T1-PC trampolines, then the standard finalize (dispatch
         * table + veneer/constant flush). */
        void emit_all() {
            a.bind(entry_label);

            if (install_entry != nullptr) {
                /* Installed blob: the patched `b` at L_f+4 lands here
                 * *after* L_f+0's enter_erlang_frame, so the frame is
                 * already pushed and must not be pushed again; T1's
                 * i_test_yield at L_f+12 is bypassed, so the stub does
                 * its own reduction bookkeeping (PLAN/T2/06 §2.3). */
                emit_t2_entry_stub();
            } else {
                /* Mirror a normal callee: push the return address onto
                 * the Erlang stack. The standalone test blob supplies
                 * its own prologue so the exec-harness trampoline can
                 * enter it directly as if freshly called. */
                emit_enter_erlang_frame();
            }

            for (const T2LirBlock &b : fn.blocks) {
                /* Mirror emit_label: a block entry invalidates the
                 * register cache the reused T1 emitters maintain, and
                 * long bodies must give pending veneers a chance to
                 * flush. */
                reg_cache.invalidate();
                check_pending_stubs();

                bind_veneer_target(block_label(b.id));
                for (const T2LirOp &op : b.ops) {
                    if (failed()) {
                        return;
                    }
                    emit_op(op);
                }
            }

            emit_fail_trampolines();

            /* Reuse the loader's finalize: binds code_end, flushes the
             * dispatch table (for resolve_fragment thunks such as the
             * dispatch-return used by emit_return) and all pending
             * veneers/constants. */
            emit_int_code_end();
        }

        /* Flatten + relocate into the global JIT allocator; return the
         * blob's entry code pointer (or nullptr on failure). */
        const void *finalize_to_allocator() {
            if (failed()) {
                return nullptr;
            }

            const void *exec = nullptr;
            void *rw = nullptr;

            codegen(erts_t2_jit_allocator(), &exec, &rw);

            /* codegen leaves the calling thread in JIT-write mode
             * (protect_jit_memory(kReadWrite)); executing any JIT page in
             * that state faults on Apple Silicon outside a debugger.
             * Mirror erts_seal_module: flush the icache for the fresh
             * blob, then flip the thread back to execute mode. */
            beamasm_flush_icache(exec, code.code_size());
            beamasm_seal_module(exec, rw, code.code_size());

            blob_base = exec;
            blob_size = code.code_size();

            return (const void *)getCode(entry_label);
        }

    private:
        /* The install entry stub (PLAN/T2/06 §2.3, map §3): a 1:1 mirror
         * of emit_i_test_yield (arm/instr_common.cpp) minus the frame
         * push -- L_f+0 already ran. ARG3 = L_f keeps the MFA-derivation
         * contract of i_test_yield_shared (`sub ARG2, ARG3,
         * sizeof(ErtsCodeMFA)`), and its resume PC computation
         * (ARG3 + TEST_YIELD_RETURN_OFFSET = L_f + 24 = the T1 body)
         * transparently demotes an entry-yielded invocation to T1 --
         * hence no c_p->i ever points into a P1 blob (map §4). */
        void emit_t2_entry_stub() {
            mov_imm(ARG3, (Uint64)install_entry);

            if (erts_alcu_enable_code_atags) {
                /* Keep the point-of-origin allocation tags exactly as
                 * accurate as T1's i_test_yield does. */
                a.str(ARG3, a64::Mem(c_p, offsetof(Process, i)));
            }

            a.subs(FCALLS, FCALLS, imm(1));
            a.b_le(resolve_fragment(ga->get_i_test_yield_shared(), disp1MB));
            /* Fall through into the body (blocks[0]). */
        }

        const Label &block_label(uint32_t id) {
            /* Block b uses BEAM label number 1 + b (reserved in the ctor's
             * num_labels). */
            return rawLabels.at(1 + id);
        }

        /* Get (or create) the ArgLabel number of the trampoline for a T1
         * fail PC. rawLabels must map it so resolve_beam_label() resolves. */
        unsigned fail_label_num(const void *t1_pc) {
            auto it = fail_labels.find(t1_pc);
            if (it != fail_labels.end()) {
                return it->second;
            }
            unsigned n = next_label++;
            rawLabels.emplace(n, a.new_label());
            fail_labels.emplace(t1_pc, n);
            return n;
        }

        void emit_fail_trampolines() {
            for (const auto &pair : fail_labels) {
                reg_cache.invalidate();
                bind_veneer_target(rawLabels.at(pair.second));
                /* Absolute branch to the T1 PC. mov_imm materializes the
                 * 64-bit address; br transfers control. T1 re-executes and
                 * (for error paths) raises a byte-identical exception. */
                mov_imm(TMP1, (Uint64)pair.first);
                a.br(TMP1);
                mark_unreachable();
            }
        }

        /* ---- ArgVal synthesis ---------------------------------------- */

        ArgVal loc_argval(const PhysLoc &l) {
            switch (l.kind) {
            case PhysLoc::Kind::XReg:
                return ArgVal(ArgVal::Type::XReg, l.num);
            case PhysLoc::Kind::YReg:
                return ArgVal(ArgVal::Type::YReg, l.num);
            default:
                fail("unsupported PhysLoc (phys/none) in identity emit");
                return ArgVal(ArgVal::Type::XReg, 0);
            }
        }

        ArgVal src_argval(const T2LirSrc &s) {
            if (s.is_const) {
                return ArgVal(ArgVal::Type::Immediate, s.term);
            }
            return loc_argval(s.loc);
        }

        void fail(const char *msg) {
            if (emit_error.empty()) {
                emit_error = msg;
            }
        }

        /* ---- op dispatch --------------------------------------------- */

        void emit_op(const T2LirOp &op) {
            switch (op.kind) {
            case T2LirKind::Move:
                emit_lir_move(op);
                break;
            case T2LirKind::Swap:
                emit_swap(ArgRegister(loc_argval(op.dst)),
                          ArgRegister(loc_argval(op.dst2)));
                break;
            case T2LirKind::Add:
            case T2LirKind::Sub:
            case T2LirKind::Mul:
            case T2LirKind::IDiv:
            case T2LirKind::Rem:
            case T2LirKind::Band:
            case T2LirKind::Bor:
            case T2LirKind::Bxor:
            case T2LirKind::Bsl:
            case T2LirKind::Bsr:
            case T2LirKind::Bnot:
            case T2LirKind::Neg:
                emit_lir_arith(op);
                break;
            case T2LirKind::IsInteger:
            case T2LirKind::IsAtom:
            case T2LirKind::IsNil:
            case T2LirKind::IsList:
            case T2LirKind::IsNonemptyList:
            case T2LirKind::IsTuple:
            case T2LirKind::TestArity:
            case T2LirKind::IsTaggedTuple:
            case T2LirKind::CmpEqExact:
            case T2LirKind::CmpNeExact:
            case T2LirKind::CmpEq:
            case T2LirKind::CmpNe:
            case T2LirKind::CmpLt:
            case T2LirKind::CmpGe:
                emit_lir_guard(op);
                break;
            case T2LirKind::GetHd:
                emit_get_hd(ArgRegister(src_argval(op.srcs[0])),
                            ArgRegister(loc_argval(op.dst)));
                break;
            case T2LirKind::GetTl:
                emit_get_tl(ArgRegister(src_argval(op.srcs[0])),
                            ArgRegister(loc_argval(op.dst)));
                break;
            case T2LirKind::GetList:
                emit_get_list(ArgRegister(src_argval(op.srcs[0])),
                              ArgRegister(loc_argval(op.dst)),
                              ArgRegister(loc_argval(op.dst2)));
                break;
            case T2LirKind::GetTupleElement:
                /* T1's transform pairs a load_tuple_ptr with each
                 * (non-fused) element fetch; the element operand is the
                 * byte offset past the arity header. */
                emit_load_tuple_ptr(ArgSource(src_argval(op.srcs[0])));
                emit_i_get_tuple_element(
                        ArgSource(src_argval(op.srcs[0])),
                        ArgWord(((UWord)op.imm + 1) * sizeof(Eterm)),
                        ArgRegister(loc_argval(op.dst)));
                break;
            case T2LirKind::MakeList:
                emit_put_list(ArgSource(src_argval(op.srcs[0])),
                              ArgSource(src_argval(op.srcs[1])),
                              ArgRegister(loc_argval(op.dst)));
                break;
            case T2LirKind::MakeTuple:
                emit_lir_make_tuple(op);
                break;
            case T2LirKind::GcTest:
                emit_test_heap(ArgWord((UWord)op.imm), ArgWord(op.live));
                break;
            case T2LirKind::Allocate:
                emit_allocate_heap(ArgWord((UWord)op.imm),
                                   ArgWord((UWord)op.imm2),
                                   ArgWord(op.live));
                break;
            case T2LirKind::Deallocate:
                emit_deallocate(ArgWord((UWord)op.imm));
                break;
            case T2LirKind::Trim:
                emit_trim(ArgWord((UWord)op.imm), ArgWord((UWord)op.imm2));
                break;
            case T2LirKind::Jump:
                a.b(block_label(op.succ_then));
                mark_unreachable();
                break;
            case T2LirKind::Switch:
                emit_lir_switch(op);
                break;
            case T2LirKind::Return:
                emit_return();
                break;
            case T2LirKind::Call:
            case T2LirKind::CallExt:
                emit_lir_call(op, /*is_tail=*/false);
                break;
            case T2LirKind::TailCall:
            case T2LirKind::TailCallExt:
                emit_lir_call(op, /*is_tail=*/true);
                break;
            case T2LirKind::CallBif:
                emit_lir_call_bif(op);
                break;
            case T2LirKind::SideExit:
                /* Unconditional transfer to a T1 PC: an error-exit op's
                 * own site or the function's func_info. T1 re-executes /
                 * raises; T2 never raises. */
                fact(EmitFact::SideExitPc, op.beam_idx, op.t1_pc_fail);
                mov_imm(TMP1, (Uint64)op.t1_pc_fail);
                a.br(TMP1);
                mark_unreachable();
                break;
            default:
                fail("unsupported LIR op kind in P1 identity emit");
                break;
            }
        }

        void emit_lir_move(const T2LirOp &op) {
            if (op.num_srcs != 1 || op.dst.is_none()) {
                fail("malformed move");
                return;
            }
            emit_i_move(ArgSource(src_argval(op.srcs[0])),
                        ArgRegister(loc_argval(op.dst)));
        }

        /* The Fail label of an arith op with a {f,0} decoded fail: a
         * trampoline branching to the op's own T1 EFFECT site (side-exit;
         * T2 never raises). Ops with a real decoded fail label branch
         * in-blob instead (succ_else). */
        ArgVal arith_fail_argval(const T2LirOp &op) {
            if (op.succ_else != T2_LIR_NO_BLOCK) {
                return ArgVal(ArgVal::Type::Label, 1 + op.succ_else);
            }
            if (op.t1_pc_fail == nullptr) {
                fail("arith op without a fail edge or T1 side exit");
                return ArgVal(ArgVal::Type::Label, 1);
            }
            fact(EmitFact::FailExitPc, op.beam_idx, op.t1_pc_fail);
            return ArgVal(ArgVal::Type::Label, fail_label_num(op.t1_pc_fail));
        }

        void emit_lir_arith(const T2LirOp &op) {
            ArgLabel failL(arith_fail_argval(op));
            if (failed()) {
                return;
            }
            ArgWord live(op.live);
            ArgRegister dst(loc_argval(op.dst));

            if (op.num_srcs == 2) {
                ArgSource lhs(src_argval(op.srcs[0]));
                ArgSource rhs(src_argval(op.srcs[1]));

                switch (op.kind) {
                case T2LirKind::Add:
                    emit_i_plus(failL, live, lhs, rhs, dst);
                    break;
                case T2LirKind::Sub:
                    emit_i_minus(failL, live, lhs, rhs, dst);
                    break;
                case T2LirKind::Mul:
                    /* T1's transform for a plain '*' is
                     * `i_mul_add Fail S1 S2 Dst i Dst` — the destination
                     * as the (ignored) addend source and a zero small as
                     * the increment. Mirror it exactly. */
                    emit_i_mul_add(failL,
                                   lhs,
                                   rhs,
                                   ArgSource(loc_argval(op.dst)),
                                   ArgImmed(make_small(0)),
                                   dst);
                    break;
                case T2LirKind::IDiv:
                    emit_i_int_div(failL, live, lhs, rhs, dst);
                    break;
                case T2LirKind::Rem:
                    emit_i_rem(failL, live, lhs, rhs, dst);
                    break;
                case T2LirKind::Band:
                    emit_i_band(failL, live, lhs, rhs, dst);
                    break;
                case T2LirKind::Bor:
                    emit_i_bor(failL, live, lhs, rhs, dst);
                    break;
                case T2LirKind::Bxor:
                    emit_i_bxor(failL, live, lhs, rhs, dst);
                    break;
                case T2LirKind::Bsl:
                    emit_i_bsl(failL, live, lhs, rhs, dst);
                    break;
                case T2LirKind::Bsr:
                    emit_i_bsr(failL, live, lhs, rhs, dst);
                    break;
                default:
                    fail("unhandled binary arithmetic op");
                    break;
                }
            } else if (op.num_srcs == 1) {
                ArgSource src(src_argval(op.srcs[0]));

                switch (op.kind) {
                case T2LirKind::Neg:
                    emit_i_unary_minus(failL, live, src, dst);
                    break;
                case T2LirKind::Bnot:
                    emit_i_bnot(failL, live, src, dst);
                    break;
                default:
                    fail("unhandled unary arithmetic op");
                    break;
                }
            } else {
                fail("malformed arithmetic op");
                return;
            }

            /* A folded fail edge means the op absorbed the block branch:
             * fall through was rewritten to an explicit jump. */
            if (op.succ_then != T2_LIR_NO_BLOCK && !failed()) {
                a.b(block_label(op.succ_then));
                mark_unreachable();
            }
        }

        void emit_lir_guard(const T2LirOp &op) {
            if (op.succ_then == T2_LIR_NO_BLOCK ||
                op.succ_else == T2_LIR_NO_BLOCK) {
                fail("guard without both branch edges");
                return;
            }

            ArgLabel failL(ArgVal(ArgVal::Type::Label, 1 + op.succ_else));

            switch (op.kind) {
            case T2LirKind::IsInteger:
                emit_is_integer(failL, ArgSource(src_argval(op.srcs[0])));
                break;
            case T2LirKind::IsAtom:
                emit_is_atom(failL, ArgSource(src_argval(op.srcs[0])));
                break;
            case T2LirKind::IsNil:
                emit_is_nil(failL, ArgRegister(src_argval(op.srcs[0])));
                break;
            case T2LirKind::IsList:
                emit_is_list(failL, ArgSource(src_argval(op.srcs[0])));
                break;
            case T2LirKind::IsNonemptyList:
                emit_is_nonempty_list(failL,
                                      ArgRegister(src_argval(op.srcs[0])));
                break;
            case T2LirKind::IsTuple:
                emit_i_is_tuple(failL, ArgSource(src_argval(op.srcs[0])));
                break;
            case T2LirKind::TestArity:
                emit_i_test_arity(failL,
                                  ArgSource(src_argval(op.srcs[0])),
                                  ArgWord(make_arityval((UWord)op.imm)));
                break;
            case T2LirKind::IsTaggedTuple:
                emit_i_is_tagged_tuple(failL,
                                       ArgSource(src_argval(op.srcs[0])),
                                       ArgWord(make_arityval((UWord)op.imm)),
                                       ArgAtom(ArgVal(ArgVal::Type::Immediate,
                                                      op.imm_term)));
                break;
            case T2LirKind::CmpEqExact:
                emit_is_eq_exact(failL,
                                 ArgSource(src_argval(op.srcs[0])),
                                 ArgSource(src_argval(op.srcs[1])));
                break;
            case T2LirKind::CmpNeExact:
                emit_is_ne_exact(failL,
                                 ArgSource(src_argval(op.srcs[0])),
                                 ArgSource(src_argval(op.srcs[1])));
                break;
            case T2LirKind::CmpEq:
                emit_is_eq(failL,
                           ArgSource(src_argval(op.srcs[0])),
                           ArgSource(src_argval(op.srcs[1])));
                break;
            case T2LirKind::CmpNe:
                emit_is_ne(failL,
                           ArgSource(src_argval(op.srcs[0])),
                           ArgSource(src_argval(op.srcs[1])));
                break;
            case T2LirKind::CmpLt:
                emit_is_lt(failL,
                           ArgSource(src_argval(op.srcs[0])),
                           ArgSource(src_argval(op.srcs[1])));
                break;
            case T2LirKind::CmpGe:
                emit_is_ge(failL,
                           ArgSource(src_argval(op.srcs[0])),
                           ArgSource(src_argval(op.srcs[1])));
                break;
            default:
                fail("unhandled guard kind");
                return;
            }

            if (!failed()) {
                a.b(block_label(op.succ_then));
                mark_unreachable();
            }
        }

        void emit_lir_make_tuple(const T2LirOp &op) {
            std::vector<ArgVal> elems;
            size_t count = op.num_srcs_ext > 0 ? op.num_srcs_ext
                                               : op.num_srcs;

            elems.reserve(count);
            for (size_t i = 0; i < count; i++) {
                const T2LirSrc &s = op.num_srcs_ext > 0
                                            ? fn.src_pool[op.pool_first + i]
                                            : op.srcs[i];
                elems.push_back(src_argval(s));
            }

            emit_put_tuple2(ArgRegister(loc_argval(op.dst)),
                            ArgWord(make_arityval(count)),
                            Span<const ArgVal>(elems.data(), elems.size()));
        }

        void emit_lir_switch(const T2LirOp &op) {
            mov_arg(TMP1, ArgSource(src_argval(op.srcs[0])));

            if (op.imm != 0) {
                /* select_tuple_arity (isel imm=1): mirror T1's
                 * emit_i_select_tuple_arity — boxed check, load the
                 * header word, require the arityval header tag, then
                 * compare against make_arityval(N). The LIR case values
                 * carry the arity as make_small(N). */
                const Label &dflt = rawLabels.at(1 + op.default_target);

                emit_is_boxed(resolve_label(dflt, disp1MB), TMP1);

                a64::Gp boxed_ptr = emit_ptr_val(TMP2, TMP1);
                a.ldur(TMP1, emit_boxed_val(boxed_ptr, 0));

                ERTS_CT_ASSERT(_TAG_HEADER_ARITYVAL == 0);
                a.tst(TMP1, imm(_TAG_HEADER_MASK));
                a.b_ne(resolve_label(dflt, disp1MB));

                for (uint32_t i = 0; i < op.num_cases; i++) {
                    const T2LirSwitchCase &c =
                            fn.switch_cases[op.first_case + i];

                    if (!is_small(c.value)) {
                        fail("arity switch case is not a small");
                        return;
                    }
                    cmp_arg(TMP1,
                            ArgWord(make_arityval(unsigned_val(c.value))));
                    a.b_eq(resolve_label(rawLabels.at(1 + c.target),
                                         disp1MB));
                }

                a.b(block_label(op.default_target));
                mark_unreachable();
                return;
            }

            /* A linear compare chain, semantically identical to T1's
             * i_select_val_lins on immediates (exact-equal compare per
             * case, then the default). */
            for (uint32_t i = 0; i < op.num_cases; i++) {
                const T2LirSwitchCase &c = fn.switch_cases[op.first_case + i];

                cmp_arg(TMP1, ArgVal(ArgVal::Type::Immediate, c.value));
                a.b_eq(resolve_label(rawLabels.at(1 + c.target), disp1MB));
            }

            a.b(block_label(op.default_target));
            mark_unreachable();
        }

        /* Non-tail calls: establish nothing (the canonical state already
         * holds by identity placement), materialize the T1 continuation
         * as the CP and branch — never `bl` — to the callee's T1 entry
         * (PLAN/T2/08 §4.3: no return addresses into T2 blobs; two
         * instructions instead of bl). The callee's prologue pushes our
         * LR as its frame CP; on return execution continues in T1
         * (demote-on-return). Tail calls leave the frame and branch; the
         * callee returns directly to our caller. Reductions are charged
         * at the callee's entry, exactly as T1. */
        void emit_lir_call(const T2LirOp &op, bool is_tail) {
            bool is_ext = op.kind == T2LirKind::CallExt ||
                          op.kind == T2LirKind::TailCallExt;

            if (is_ext) {
                /* Export dispatch through the active code index, exactly
                 * like emit_i_call_ext / emit_i_call_ext_only (including
                 * the save_calls fragment reached via a special index). */
                mov_imm(ARG1, (Uint64)op.exp);
                a64::Mem target = emit_setup_dispatchable_call(ARG1);

                if (is_tail) {
                    fact(EmitFact::TailExport, op.beam_idx, op.exp);
                    emit_leave_erlang_frame();
                } else {
                    fact(EmitFact::CallExport, op.beam_idx, op.exp);
                    fact(EmitFact::CpCont, op.beam_idx, op.t1_pc_cont);
                    mov_imm(a64::x30, (Uint64)op.t1_pc_cont);
                }
                branch(target);
            } else {
                if (is_tail) {
                    fact(EmitFact::TailTarget, op.beam_idx, op.target);
                    emit_leave_erlang_frame();
                } else {
                    fact(EmitFact::CallTarget, op.beam_idx, op.target);
                    fact(EmitFact::CpCont, op.beam_idx, op.t1_pc_cont);
                    mov_imm(a64::x30, (Uint64)op.t1_pc_cont);
                }
                mov_imm(SUPER_TMP, (Uint64)op.target);
                a.br(SUPER_TMP);
            }
            mark_unreachable();
        }

        /* A light-BIF call: T1's call_light_bif with the two T2
         * redirections, via the t2_call_light_bif_shared fragment
         * (instr_bif.cpp). Mirrors emit_call_light_bif exactly except
         * that the entry (ARG3) is the site's *T1* PC rather than the
         * emission's own address, and the T1 continuation rides in
         * ARG5 for the trap/trace CP. The success path returns here
         * (plain `bl` return) and execution continues in the blob —
         * the only outcome that stays in T2; yield/trap/trace demote
         * the invocation to T1 with only T1 addresses in c_p->i / on
         * the stack. Reductions: the fragment's own `subs FCALLS`
         * charge, identical to T1. */
        void emit_lir_call_bif(const T2LirOp &op) {
            fact(EmitFact::BifExport, op.beam_idx, op.exp);
            fact(EmitFact::BifSitePc, op.beam_idx, op.t1_pc_fail);
            fact(EmitFact::BifContPc, op.beam_idx, op.t1_pc_cont);

            if (logger.file()) {
                comment("T2 BIF: %T:%T/%u",
                        op.mfa_m,
                        op.mfa_f,
                        (unsigned)op.arity);
            }

            mov_imm(ARG8, (Uint64)op.target); /* BIF C function      */
            mov_imm(ARG4, (Uint64)op.exp);    /* Export*             */
            mov_imm(ARG3, (Uint64)op.t1_pc_fail); /* T1 site         */
            mov_imm(ARG5, (Uint64)op.t1_pc_cont); /* T1 continuation */

            fragment_call(ga->get_t2_call_light_bif_shared());

            /* The fragment clobbers everything caller-saved; the
             * offset-keyed register cache self-invalidates, but be
             * explicit for the reused emitters that follow. */
            reg_cache.invalidate();
        }

    public:
        /* Emit the standalone C-callable re-entry trampoline (see
         * t2_emit.hpp). Reuses emit_leave_runtime / emit_enter_runtime to
         * mirror process_main's schedule-in / schedule-out exactly. */
        const void *emit_reentry_trampoline_blob() {
            Label tr = a.new_label();
            a.bind(tr);

            /* Preserve the C caller's callee-saved GPRs (x19..x28 hold the
             * scheduler machine registers while the blob runs). */
            a.stp(a64::x19, a64::x20, a64::Mem(a64::sp, -16).pre());
            a.stp(a64::x21, a64::x22, a64::Mem(a64::sp, -16).pre());
            a.stp(a64::x23, a64::x24, a64::Mem(a64::sp, -16).pre());
            a.stp(a64::x25, a64::x26, a64::Mem(a64::sp, -16).pre());
            a.stp(a64::x27, a64::x28, a64::Mem(a64::sp, -16).pre());
            a.stp(a64::x29, a64::x30, a64::Mem(a64::sp, -16).pre());

            /* ARG1=c_p, ARG2=entry, ARG3=ErtsSchedulerRegisters*. */
            a.mov(scheduler_registers, ARG3);
            a.mov(c_p, ARG1);
            a.mov(TMP1, ARG2); /* entry, saved before ARGs are clobbered */

            /* Load E/HTOP and the register-backed X regs from the process /
             * X array (args were written into the array by the C caller). */
            emit_leave_runtime<Update::eStack | Update::eHeap |
                               Update::eXRegs>();

            /* Keep reductions well above zero so emit_return's dispatch does
             * not context-switch out of the harness. */
            mov_imm(FCALLS, CONTEXT_REDS);

            mov_imm(SUPER_TMP, (Uint64)(UWord)&the_active_code_index);
            a.ldr(active_code_ix.w(), a64::Mem(SUPER_TMP));

            /* Return address of the blob = the sync-back label below. */
            Label after = a.new_label();
            a.adr(a64::x30, after);
            a.br(TMP1);

            a.bind(after);
            /* Blob returned; result is in XREG0. Sync E/HTOP/X regs back to
             * the process the way a schedule-out does. */
            emit_enter_runtime<Update::eStack | Update::eHeap |
                               Update::eXRegs>();

            a.ldr(ARG1, getXRef(0)); /* result -> return value */

            a.ldp(a64::x29, a64::x30, a64::Mem(a64::sp).post(16));
            a.ldp(a64::x27, a64::x28, a64::Mem(a64::sp).post(16));
            a.ldp(a64::x25, a64::x26, a64::Mem(a64::sp).post(16));
            a.ldp(a64::x23, a64::x24, a64::Mem(a64::sp).post(16));
            a.ldp(a64::x21, a64::x22, a64::Mem(a64::sp).post(16));
            a.ldp(a64::x19, a64::x20, a64::Mem(a64::sp).post(16));
            a.ret(a64::x30);

            emit_int_code_end();

            const void *exec = nullptr;
            void *rw = nullptr;
            codegen(erts_t2_jit_allocator(), &exec, &rw);

            /* See finalize_to_allocator: flush + reseal after codegen. */
            beamasm_flush_icache(exec, code.code_size());
            beamasm_seal_module(exec, rw, code.code_size());

            return getCode(tr);
        }
    };

    /* ------------------------------------------------------------------ *
     * Public emit entry                                                  *
     * ------------------------------------------------------------------ */

    const void *t2_emit_blob(const T2LirFunction &fn,
                             std::string *err,
                             std::string *disasm,
                             T2EmitResult *out) {
        BeamGlobalAssembler *ga = erts_t2_global_assembler();
        if (ga == nullptr || erts_t2_jit_allocator() == nullptr) {
            if (err) {
                *err = "T2 JIT not initialized";
            }
            return nullptr;
        }

        int num_labels = (int)fn.blocks.size() + 1;

        BeamT2ModuleAssembler ma(ga, fn.module, num_labels, fn);

        StringLogger slog;
        if (disasm) {
            ma.set_string_logger(&slog);
        }

        ma.emit_all();

        if (ma.failed()) {
            if (err) {
                *err = ma.error();
            }
            if (disasm) {
                *disasm = std::string(slog.data(), slog.data_size());
            }
            return nullptr;
        }

        const void *entry = ma.finalize_to_allocator();

        if (disasm) {
            *disasm = std::string(slog.data(), slog.data_size());
        }
        if (entry == nullptr && err && err->empty()) {
            *err = "codegen produced no entry";
        }
        if (entry != nullptr && out != nullptr) {
            out->entry = entry;
            out->base = ma.blob_base;
            out->size = ma.blob_size;
        }
        return entry;
    }

    bool t2_emit_blob_install(const T2LirFunction &fn,
                              const void *install_entry,
                              T2EmitResult *out,
                              std::string *err,
                              std::string *disasm) {
        BeamGlobalAssembler *ga = erts_t2_global_assembler();

        if (ga == nullptr || erts_t2_jit_allocator() == nullptr) {
            if (err) {
                *err = "T2 JIT not initialized";
            }
            return false;
        }
        if (install_entry == nullptr) {
            if (err) {
                *err = "no install entry (L_f)";
            }
            return false;
        }

        int num_labels = (int)fn.blocks.size() + 1;

        BeamT2ModuleAssembler ma(ga, fn.module, num_labels, fn);

        StringLogger slog;
        if (disasm) {
            ma.set_string_logger(&slog);
        }

        ma.set_install_entry(install_entry);
        ma.emit_all();

        if (ma.failed()) {
            if (err) {
                *err = ma.error();
            }
            if (disasm) {
                *disasm = std::string(slog.data(), slog.data_size());
            }
            return false;
        }

        const void *entry = ma.finalize_to_allocator();

        if (disasm) {
            *disasm = std::string(slog.data(), slog.data_size());
        }
        if (entry == nullptr) {
            if (err && err->empty()) {
                *err = "codegen produced no entry";
            }
            return false;
        }

        out->entry = entry;
        out->base = ma.blob_base;
        out->size = ma.blob_size;
        return true;
    }

    ErtsT2ReentryFn t2_get_reentry_trampoline(void) {
        static ErtsT2ReentryFn cached = nullptr;
        if (cached != nullptr) {
            return cached;
        }

        BeamGlobalAssembler *ga = erts_t2_global_assembler();
        if (ga == nullptr || erts_t2_jit_allocator() == nullptr) {
            return nullptr;
        }

        T2LirFunction empty;
        empty.module = am_undefined;

        BeamT2ModuleAssembler ma(ga, empty.module, 1, empty);
        cached = (ErtsT2ReentryFn)ma.emit_reentry_trampoline_blob();
        return cached;
    }

} /* namespace erts_t2 */

/* ------------------------------------------------------------------ *
 * Exec harness debug BIF (erts_debug:get_internal_state(             *
 *   {t2_exec, M, F, A, Args}))                                        *
 * ------------------------------------------------------------------ */

using namespace erts_t2;

namespace {
    Eterm t2_exec_error(Process *p, const char *s) {
        Uint sz = 0;
        Eterm *hp;
        (void)erts_bld_string_n(NULL, &sz, s, (Sint)sys_strlen(s));
        sz += 3;
        hp = HAlloc(p, sz);
        Eterm str = erts_bld_string_n(&hp, NULL, s, (Sint)sys_strlen(s));
        return TUPLE2(hp, am_error, str);
    }
}

extern "C" Eterm erts_t2_debug_exec(Process *p,
                                    Eterm mod,
                                    Eterm func,
                                    Eterm arity,
                                    Eterm args) {
    if (!is_atom(mod) || !is_atom(func) || !is_small(arity)) {
        return am_undefined;
    }
    if (signed_val(arity) < 0 || signed_val(arity) > MAX_ARG) {
        return am_undefined;
    }
    Uint ar = unsigned_val(arity);

    Module *modp = erts_get_module(mod, erts_active_code_ix());
    if (modp == NULL || modp->curr.t2_retained == NULL ||
        modp->curr.code_hdr == NULL) {
        return am_undefined;
    }
    const ErtsT2RetainedCode *ret = modp->curr.t2_retained;

    /* T1 entry of the function (kept on the LIR for reference/debug). */
    const Export *e =
            erts_find_function(mod, func, (unsigned)ar, erts_active_code_ix());
    const void *t1_entry =
            e ? (const void *)e->dispatch.addresses[erts_active_code_ix()]
              : NULL;

    T2IselContext ctx;
    ctx.ret = ret;
    ctx.code_hdr = (const void *)modp->curr.code_hdr;
    /* Standalone exec: no loader permission, must not create export stubs. */
    ctx.allow_stub = false;

    /* Build HIR -> isel -> verify -> emit, all while the module decode
     * (and any literals the HIR references) is still alive. */
    const void *blob = NULL;
    T2EmitResult blob_span;
    std::string build_err;
    std::string pipe_err;

    T2BuildStatus status = t2_build_for_debug(
            ret,
            func,
            (unsigned)ar,
            [&](const T2Function &hir) {
                T2LirFunction lir;
                if (!t2_isel(hir, ctx, lir, &pipe_err)) {
                    return;
                }
                lir.t1_entry = t1_entry;
                if (!t2_regalloc(lir, &pipe_err)) {
                    return;
                }
                blob = t2_emit_blob(lir, &pipe_err, nullptr, &blob_span);
            },
            &build_err);

    switch (status) {
    case T2BuildStatus::Ok:
        break;
    case T2BuildStatus::NotFound:
        return t2_exec_error(p, "not_found");
    case T2BuildStatus::NotEligible:
        return t2_exec_error(p, "not_eligible");
    case T2BuildStatus::Failed:
    default:
        return t2_exec_error(p, build_err.empty() ? "build_failed"
                                                   : build_err.c_str());
    }

    if (blob == NULL) {
        return t2_exec_error(
                p, pipe_err.empty() ? "emit_failed" : pipe_err.c_str());
    }

    ErtsT2ReentryFn tramp = t2_get_reentry_trampoline();
    if (tramp == NULL) {
        return am_undefined;
    }

    /* The JIT spilled this BIF caller's live X registers into the X array
     * on the way in; the blob (and the trampoline's schedule-in/out) reads
     * and clobbers the low slots. Save every slot we may touch and restore
     * it afterwards so returning to the caller preserves its X state. The
     * 6 register-backed X registers are always synced by
     * emit_leave/enter_runtime; args past those live in the array. */
    ErtsSchedulerRegisters *regs = erts_proc_sched_data(p)->registers;
    Eterm *xreg = regs->x_reg_array.d;

    Uint nsave = ar > 6 ? ar : 6;
    Eterm saved[MAX_ARG + 8];
    for (Uint i = 0; i < nsave; i++) {
        saved[i] = xreg[i];
    }

    Eterm lst = args;
    for (Uint i = 0; i < ar; i++) {
        if (is_not_list(lst)) {
            for (Uint j = 0; j < nsave; j++) {
                xreg[j] = saved[j];
            }
            return t2_exec_error(p, "argument_list_too_short");
        }
        Eterm *cons = list_val(lst);
        xreg[i] = CAR(cons);
        lst = CDR(cons);
    }
    if (is_not_nil(lst)) {
        for (Uint j = 0; j < nsave; j++) {
            xreg[j] = saved[j];
        }
        return t2_exec_error(p, "argument_list_too_long");
    }

    /* Enter the blob and capture its result. NB: valid for non-raising
     * inputs only; a raising input side-exits into T1's raise path, which
     * unwinds via the scheduler (to an enclosing Erlang catch) rather than
     * back through this trampoline -- so the restore below is skipped on
     * that path, but the unwind trims the caller off the stack anyway. */
    Eterm result = tramp((void *)p, blob, (void *)regs);

    for (Uint i = 0; i < nsave; i++) {
        xreg[i] = saved[i];
    }

    /* The throwaway blob is dead; release its span once every scheduler
     * has passed a code barrier. (A raising input skips this like it
     * skips the restore above -- the P1 install wave's tombstone-free
     * covers the paths that matter; the raise path of this pre-install
     * debug harness keeps its historical one-blob leak.) */
    erts_t2_free_spans_after_barrier((void *)blob_span.base,
                                     blob_span.size,
                                     NULL,
                                     0);

    return result;
}

/* ------------------------------------------------------------------ *
 * Selftest (T2_EMIT_SELFTEST)                                        *
 * ------------------------------------------------------------------ */

extern "C" int erts_t2_emit_selftest_enabled(void) {
    static int enabled = -1;
    if (enabled < 0) {
        const char *env = getenv("T2_EMIT_SELFTEST");
        enabled = (env != NULL && env[0] != '\0' && env[0] != '0') ? 1 : 0;
    }
    return enabled;
}

extern "C" int erts_t2_emit_selftest(void) {
    /* Hand-build a 1-op LIR: move a small integer into X0, then return it.
     * Emit it through a fresh BeamT2ModuleAssembler and disassemble. */
    T2LirFunction fn;
    ERTS_DECL_AM(t2_selftest);
    fn.module = AM_t2_selftest;
    fn.function = AM_t2_selftest;
    fn.arity = 0;
    fn.t1_entry = nullptr;

    T2LirBlock &b = fn.new_block();

    T2LirOp mv;
    mv.kind = T2LirKind::Move;
    mv.dst = PhysLoc::xreg(0);
    mv.num_srcs = 1;
    mv.srcs[0] = T2LirSrc::immediate(make_small(42));
    b.ops.push_back(mv);

    T2LirOp ret;
    ret.kind = T2LirKind::Return;
    ret.num_srcs = 1;
    ret.srcs[0] = T2LirSrc::slot(PhysLoc::xreg(0));
    b.ops.push_back(ret);

    erts_fprintf(stderr, "T2 emit self-test: LIR:\n%s", t2_lir_dump(fn).c_str());

    std::string err;
    std::string disasm;
    const void *code = t2_emit_blob(fn, &err, &disasm);

    erts_fprintf(stderr, "T2 emit self-test: disassembly:\n%s", disasm.c_str());

    if (code == nullptr) {
        erts_fprintf(stderr,
                     "T2 emit self-test FAILED: %s\n",
                     err.empty() ? "(no error)" : err.c_str());
        return 1;
    }

    erts_fprintf(stderr,
                 "T2 emit self-test: emitted 1-op blob at %p\n",
                 code);
    return 0;
}

/* ------------------------------------------------------------------ *
 * Module structural selftest (T2_EMIT_SELFTEST; P1 commit 3)         *
 *                                                                    *
 * Runs at retain-commit (after the pctab is built) when the loaded   *
 * module is t2_mvp: pushes total/2 through build -> isel -> verify ->*
 * emit and asserts the emitted blob's cross-tier structure against   *
 * the pctab and the loaded code:                                     *
 *                                                                    *
 *   1. the CP materialized at the diff/2 call site is exactly the    *
 *      pctab's post-call continuation for that op (and not the       *
 *      function entry or the call site itself);                      *
 *   2. the Y-slot stores preceding the call cover exactly the slots  *
 *      the call's sync map declares live;                            *
 *   3. the '+' side exit targets the op's own T1 EFFECT site (never  *
 *      the entry fallback);                                          *
 *   4. the shared function_clause side exit targets the function's   *
 *      func_info (its ErtsCodeInfo);                                 *
 *   5. the self-recursive tail call transfers to total/2's own T1    *
 *      entry (identity; no back-edge optimization).                  *
 * ------------------------------------------------------------------ */

namespace {

    int t2_selftest_total2(const ErtsT2RetainedCode *ret,
                           const BeamCodeHeader *hdr) {
        ERTS_DECL_AM(total);
        ERTS_DECL_AM(diff);
        std::string build_err;
        int failures = 0;
        bool ran = false;

        T2IselContext ctx;
        ctx.ret = ret;
        ctx.code_hdr = (const void *)hdr;

#define T2_STRUCT_CHECK(Cond, What)                                           \
    do {                                                                      \
        if (!(Cond)) {                                                        \
            erts_fprintf(stderr,                                              \
                         "T2 emit module self-test FAILED: %s\n",             \
                         (What));                                             \
            failures++;                                                       \
        }                                                                     \
    } while (0)

        T2BuildStatus status = t2_build_for_debug(
                ret,
                AM_total,
                2,
                [&](const T2Function &hir) {
                    T2LirFunction lir;
                    std::string err;

                    ran = true;

                    if (!t2_isel(hir, ctx, lir, &err) ||
                        !t2_regalloc(lir, &err)) {
                        erts_fprintf(stderr,
                                     "T2 emit module self-test FAILED: "
                                     "pipeline: %s\n",
                                     err.c_str());
                        failures++;
                        return;
                    }

                    /* Reference addresses from the pctab / loaded code. */
                    const ErtsCodeInfo *ci = hdr->functions[hir.fn_index];
                    const void *own_entry =
                            (const void *)erts_codeinfo_to_code(ci);
                    const void *entry_pc =
                            (const void *)erts_t2_pc_lookup_kind(
                                    ret,
                                    hir.fn_index,
                                    0,
                                    ERTS_T2_PC_ENTRY);

                    /* Locate the HIR call op (diff/2) + its sync map. */
                    const T2Op *hir_call = nullptr;
                    for (const T2BasicBlock *b : hir.blocks) {
                        for (const T2Op *op = b->ops_head; op != nullptr;
                             op = op->next) {
                            if (op->kind == T2OpKind::Call &&
                                op->mfa_f == AM_diff) {
                                hir_call = op;
                            }
                        }
                    }
                    T2_STRUCT_CHECK(hir_call != nullptr &&
                                            hir_call->sync != nullptr,
                                    "no diff/2 call with a sync map in the "
                                    "HIR");
                    if (hir_call == nullptr || hir_call->sync == nullptr) {
                        return;
                    }

                    const void *cont = (const void *)erts_t2_pc_lookup_kind(
                            ret,
                            hir.fn_index,
                            hir_call->beam_idx,
                            ERTS_T2_PC_CONT);
                    const void *call_site =
                            (const void *)erts_t2_pc_lookup_kind(
                                    ret,
                                    hir.fn_index,
                                    hir_call->beam_idx,
                                    ERTS_T2_PC_CALL);

                    T2_STRUCT_CHECK(cont != nullptr,
                                    "no CONT pctab entry for diff/2");
                    T2_STRUCT_CHECK(cont != entry_pc && cont != call_site,
                                    "CONT must be the post-call PC, not the "
                                    "entry/call site");

                    /* LIR-level: find the Call and check its block. */
                    const T2LirOp *lir_call = nullptr;
                    const T2LirBlock *call_block = nullptr;
                    std::set<uint16_t> y_stores;

                    for (const T2LirBlock &b : lir.blocks) {
                        for (const T2LirOp &op : b.ops) {
                            if (op.kind == T2LirKind::Call &&
                                op.mfa_f == AM_diff) {
                                lir_call = &op;
                                call_block = &b;
                                break;
                            }
                        }
                    }
                    T2_STRUCT_CHECK(lir_call != nullptr,
                                    "no diff/2 Call op in the LIR");
                    if (lir_call == nullptr) {
                        return;
                    }

                    T2_STRUCT_CHECK(lir_call->t1_pc_cont == cont,
                                    "LIR call CP != pctab CONT");
                    T2_STRUCT_CHECK(lir_call->target == own_entry ||
                                            lir_call->target != nullptr,
                                    "LIR call target unresolved");

                    /* 2: Y stores before the call == the sync map's live
                     * frame slots. */
                    for (const T2LirOp &op : call_block->ops) {
                        if (&op == lir_call) {
                            break;
                        }
                        if (op.kind == T2LirKind::Move && op.dst.is_yreg()) {
                            y_stores.insert(op.dst.num);
                        }
                    }
                    {
                        const T2SyncMap *m = hir_call->sync;
                        bool y_ok = m->frame_size >= 0 &&
                                    y_stores.size() ==
                                            (size_t)m->frame_size;

                        for (int32_t i = 0; y_ok && i < m->frame_size; i++) {
                            y_ok = y_stores.count((uint16_t)i) != 0;
                        }
                        T2_STRUCT_CHECK(y_ok,
                                        "Y stores before the call do not "
                                        "match the sync map's frame");
                    }

                    /* 3: the '+' side exit is the op-specific EFFECT PC. */
                    const T2LirOp *lir_add = nullptr;
                    for (const T2LirBlock &b : lir.blocks) {
                        for (const T2LirOp &op : b.ops) {
                            if (op.kind == T2LirKind::Add) {
                                lir_add = &op;
                            }
                        }
                    }
                    T2_STRUCT_CHECK(lir_add != nullptr, "no Add op in LIR");
                    if (lir_add != nullptr) {
                        const void *eff = (const void *)
                                erts_t2_pc_lookup_kind(ret,
                                                       hir.fn_index,
                                                       lir_add->beam_idx,
                                                       ERTS_T2_PC_EFFECT);

                        T2_STRUCT_CHECK(lir_add->t1_pc_fail != nullptr &&
                                                lir_add->t1_pc_fail == eff,
                                        "arith side exit != its own EFFECT "
                                        "site");
                        T2_STRUCT_CHECK(lir_add->t1_pc_fail != entry_pc,
                                        "arith side exit fell back to the "
                                        "entry");
                    }

                    /* 4 + 5 at LIR level. */
                    const T2LirOp *side_exit = nullptr;
                    const T2LirOp *tail_call = nullptr;
                    for (const T2LirBlock &b : lir.blocks) {
                        for (const T2LirOp &op : b.ops) {
                            if (op.kind == T2LirKind::SideExit) {
                                side_exit = &op;
                            }
                            if (op.kind == T2LirKind::TailCall) {
                                tail_call = &op;
                            }
                        }
                    }
                    T2_STRUCT_CHECK(side_exit != nullptr &&
                                            side_exit->t1_pc_fail ==
                                                    (const void *)ci,
                                    "shared error exit != func_info "
                                    "ErtsCodeInfo");
                    T2_STRUCT_CHECK(tail_call != nullptr &&
                                            tail_call->target == own_entry,
                                    "self tail call != own T1 entry");

                    /* Emit through a fresh assembler and check the same
                     * facts at the emitted-code level. */
                    BeamGlobalAssembler *ga = erts_t2_global_assembler();

                    if (ga == nullptr || erts_t2_jit_allocator() == nullptr) {
                        erts_fprintf(stderr,
                                     "T2 emit module self-test FAILED: JIT "
                                     "not initialized\n");
                        failures++;
                        return;
                    }

                    int num_labels = (int)lir.blocks.size() + 1;
                    BeamT2ModuleAssembler ma(ga,
                                             lir.module,
                                             num_labels,
                                             lir);
                    StringLogger slog;

                    ma.set_string_logger(&slog);
                    ma.emit_all();

                    if (ma.failed()) {
                        erts_fprintf(stderr,
                                     "T2 emit module self-test FAILED: "
                                     "emit: %s\n",
                                     ma.error().c_str());
                        failures++;
                        return;
                    }

                    const void *blob = ma.finalize_to_allocator();

                    T2_STRUCT_CHECK(blob != nullptr, "no blob produced");

                    bool saw_cp = false, saw_side = false, saw_tail = false,
                         saw_fail = false;
                    for (const auto &f : ma.facts) {
                        switch (f.kind) {
                        case BeamT2ModuleAssembler::EmitFact::CpCont:
                            saw_cp = true;
                            T2_STRUCT_CHECK(f.value == (uint64_t)cont,
                                            "emitted CP != pctab CONT");
                            break;
                        case BeamT2ModuleAssembler::EmitFact::SideExitPc:
                            saw_side = true;
                            T2_STRUCT_CHECK(f.value == (uint64_t)ci,
                                            "emitted side exit != func_info");
                            break;
                        case BeamT2ModuleAssembler::EmitFact::TailTarget:
                            saw_tail = true;
                            T2_STRUCT_CHECK(f.value == (uint64_t)own_entry,
                                            "emitted tail target != own T1 "
                                            "entry");
                            break;
                        case BeamT2ModuleAssembler::EmitFact::FailExitPc:
                            saw_fail = true;
                            T2_STRUCT_CHECK(f.value != (uint64_t)entry_pc,
                                            "emitted fail exit fell back to "
                                            "the entry");
                            break;
                        default:
                            break;
                        }
                    }
                    T2_STRUCT_CHECK(saw_cp, "no CP fact emitted");
                    T2_STRUCT_CHECK(saw_side, "no side-exit fact emitted");
                    T2_STRUCT_CHECK(saw_tail, "no tail-target fact emitted");
                    T2_STRUCT_CHECK(saw_fail, "no arith-fail fact emitted");

                    if (failures == 0) {
                        erts_fprintf(stderr,
                                     "T2 emit module self-test: total/2 blob "
                                     "at %p, %lu facts verified\n",
                                     blob,
                                     (unsigned long)ma.facts.size());
                        if (getenv("T2_DUMP") != NULL) {
                            erts_fprintf(stderr,
                                         "%s\nT2 emit module self-test "
                                         "disassembly:\n%.*s\n",
                                         t2_lir_dump(lir).c_str(),
                                         (int)slog.data_size(),
                                         slog.data());
                        }
                    }
                },
                &build_err);

#undef T2_STRUCT_CHECK

        if (status != T2BuildStatus::Ok || !ran) {
            erts_fprintf(stderr,
                         "T2 emit module self-test FAILED: build: %s\n",
                         build_err.empty() ? "not run" : build_err.c_str());
            return 1;
        }

        return failures;
    }

} /* anonymous namespace */

extern "C" void erts_t2_emit_selftest_module(
        const struct ErtsT2RetainedCode *ret,
        const void *code_hdr) {
    ERTS_DECL_AM(t2_mvp);

    if (!erts_t2_emit_selftest_enabled() || ret == NULL ||
        code_hdr == NULL) {
        return;
    }

    const BeamCodeHeader *hdr = (const BeamCodeHeader *)code_hdr;

    /* Only the t2_mvp corpus module is checked. */
    if (ret->atom_count < 2 || ret->atoms[1] != AM_t2_mvp) {
        return;
    }

    int failures = t2_selftest_total2(ret, hdr);

    if (failures != 0) {
        erts_exit(ERTS_ABORT_EXIT,
                  "T2 emit module self-test failed (%d)\n",
                  failures);
    }

    erts_fprintf(stderr, "T2 emit module self-test passed\n");
}
