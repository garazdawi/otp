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
#include "erl_binary.h"
#include "erl_bits.h"
#include "erl_map.h"

#include "t2_retain.h"
#include "t2_pctab.h"
#include "t2_install.h"
}

#include "t2_lir.hpp"
#include "t2_emit.hpp"
#include "t2_hir.hpp"
#include "t2_isel.hpp"
#include "t2_loop.hpp"

#include <algorithm>
#include <cstdio>
#include <functional>
#include <map>
#include <set>
#include <unordered_map>
#include <vector>

using namespace asmjit;

/* Defined in beam_jit_main.cpp, where the process-wide global assembler
 * and JIT allocator are file-static. */
BeamGlobalAssembler *erts_t2_global_assembler(void);
JitAllocator *erts_t2_jit_allocator(void);

/* The T2 emitter is aarch64 codegen: it subclasses the arm
 * BeamModuleAssembler and emits a64:: asmjit. The T2 mid-end, by contrast,
 * is architecture-independent and is built for every JIT arch (see
 * erts/emulator/Makefile.in). So on non-aarch64 targets the whole emitter
 * below is compiled out and replaced (at the #else near EOF) with linkable
 * stubs for every symbol the mid-end and common code reference — none of
 * which run there, since erts_t2_enabled() drives no real T2 install off
 * aarch64. */
#if defined(__aarch64__)

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

        /* Distinct T1 fail PCs (keyed with the CP a callee-window
         * trampoline pushes — null for the plain shape — and the
         * callsite-deopt marker, which makes the trampoline bump the
         * erts_t2_callsite_deopts monitoring counter) -> the BEAM
         * label number of the in-blob trampoline that branches to
         * them. */
        std::map<std::pair<std::pair<const void *, const void *>, bool>,
                 unsigned>
                fail_labels;
        unsigned next_label;

        /* When non-null, emit_all lays down the install entry stub
         * (PLAN/T2/06 §2.3) instead of the exec-harness frame prologue;
         * the value is the function's public T1 entry L_f. */
        const void *install_entry = nullptr;

        /* Per recovered-loop back-edge resume stubs (P2 commit 5;
         * PLAN/T2/08 §4.5): the body's b.le targets `yield`, the cold
         * yield setup, which hands ARG3 = `anchor` to
         * i_test_yield_shared so the stored resume PC becomes
         * anchor + TEST_YIELD_RETURN_OFFSET = `resume`, back INTO the
         * blob. The gap between anchor and resume hosts the blob's
         * per-stub tombstone flag word (jettison sets it; a
         * post-jettison resume then demotes itself to T1). */
        struct ResumeStub {
            Label yield;
            Label anchor;
            Label resume;
            Label header;       /* the loop header's block label     */
            const void *demote; /* tombstone demote: the T1 body the
                                 * stub branches to (own L_f + yield
                                 * offset, or the callee body for an
                                 * intrinsic back edge)              */
            uint32_t beam_idx;
            /* Fused-scan region index, or -1: the yield setup first
             * writes the deferred position back into the context
             * (emit_scan_writeback), completing the fresh-call
             * vector the stub saves. */
            int scan = -1;
            /* Intrinsic (callee-demote) back edges (P2 commit 8): the
             * MFA embedded before the anchor (introspection = the T1
             * helper's), the CP the tombstone demote pushes (the
             * skipped callee prologue's push), and the per-entry
             * c_p->i translation target (the call site's T1 PC).
             * Self-recursion stubs keep mfa_m == NIL (use the
             * function's own MFA), cont == NULL (no push) and
             * translate == demote. */
            Eterm mfa_m = NIL;
            Eterm mfa_f = NIL;
            uint32_t mfa_ar = 0;
            const void *cont = nullptr;
            const void *translate = nullptr;
        };
        std::vector<ResumeStub> resume_stubs;

        /* Lookahead context for emit_lir_reduction_check (the back-jump
         * that follows it names the loop header). */
        const T2LirBlock *cur_block = nullptr;
        size_t cur_op_idx = 0;

        /* ---- fused byte-scan loops (P2 commit 7; the G-bin shape) ----
         *
         * A recovered loop whose body is exactly the byte-scan subset
         * — StartMatch fast-check, one BsMatch [ensure, read-int8 |
         * skip], byte guards, flag-checked accumulator arithmetic,
         * back edge — is emitted as one fused loop: the context is
         * validated and its position/end/base hoisted into scratch
         * registers once per entry (and per resume), and the hot
         * back edge jumps past that reload, so the per-iteration body
         * touches the context object not at all. The context term
         * stays whole in its canonical X slot throughout (GC-visible
         * at the sync points, which are all outside the hot path);
         * only its *position* is deferred: every path that leaves the
         * fused region — loop exits, deopt trampolines, the yield
         * stub — first writes the T1-true bit position back into
         * ErlSubBits.start (advanced or not, per where the edge
         * leaves), so outside the region the whole-context-in-slot
         * contract holds exactly (PLAN/T2FULL/09 §7 surprise 4).
         *
         * Register discipline inside a region: SCAN_PTR/SCAN_LIM/
         * SCAN_BASE (TMP4/TMP5/TMP6) carry the byte pointer, byte
         * limit and byte base across the whole loop, including across
         * the back edge; SCAN_BYTE (ARG2) carries the read byte from
         * the BsMatch to its guards. Admission therefore only allows
         * ops whose emission provably avoids those registers: the
         * scan ops themselves, Move, flag-checked AddSmall/SubSmall
         * (TMP1..TMP3/ARG1 only), byte guards/switch (emitted here),
         * ReductionCheck and the back-jump. Anything else rejects the
         * region and the loop is emitted 1:1 (still correct, just not
         * fused). T2_NO_SCAN=1 disables fusion (the A/B lever). */
        struct ScanExitKey {
            uint32_t target;
            bool advanced;
            bool operator==(const ScanExitKey &o) const {
                return target == o.target && advanced == o.advanced;
            }
        };
        struct ScanLoop {
            uint32_t header = T2_LIR_NO_BLOCK;
            uint32_t latch = T2_LIR_NO_BLOCK;
            std::vector<uint32_t> blocks; /* chain order, starts at header */
            std::vector<bool> in_region;  /* indexed by block id           */
            std::vector<bool> adv_in;     /* block-entry "read happened"   */
            uint32_t bs_block = T2_LIR_NO_BLOCK;
            PhysLoc ctx_slot;          /* the match context's X slot    */
            PhysLoc byte_slot;         /* READ_INT8 dst, or None        */
            uint32_t adv_bytes = 0;    /* bytes consumed per iteration  */
            uint32_t ensure_bytes = 0; /* the ensure_at_least check     */
            bool read_byte = false;    /* READ_INT8 present             */
            /* The byte's canonical slot is dead outside the region's
             * own guards (no reads anywhere else, no sync-map pin, no
             * phi home): skip the tagged materialization. */
            bool byte_dead = false;
            /* Emission labels. */
            Label reload;      /* ctx -> ptr/lim/base           */
            Label scan_resume; /* hot back-jump target          */
            Label scan_body;   /* post-ensure body (rotation)   */
            Label unaligned;   /* bit-offset context: demote    */
            /* Rotated loop: nothing executes between the header and the
             * ensure check, so the back edge carries its own bounds
             * check and branches straight to the post-ensure body —
             * one conditional taken branch per iteration, no trailing
             * unconditional jump. */
            bool rotated = false;
            /* Cold continuations, emitted after the body. */
            std::vector<std::pair<ScanExitKey, Label>> exit_thunks;
            struct WbTramp {
                const void *t1_pc;
                bool advanced;
                Label label;
                /* Direct-committed flag-checked arithmetic (adds/subs
                 * straight into the register-backed destination — the
                 * loop-carried dependency is then one cycle, not the
                 * scratch-then-commit two): on overflow the register
                 * holds the wrapped sum, and two's-complement makes
                 * the un-commit exact — the trampoline restores the
                 * original before T1 re-executes. none = no uncommit. */
                PhysLoc uncommit_slot = PhysLoc::none();
                uint64_t uncommit_imm = 0;
                bool uncommit_add = false; /* the op was an add */
            };
            std::vector<WbTramp> wb_tramps;
            bool emitted = false; /* region laid down already   */
            /* The StartMatch op (generic slow path in a cold stub). */
            const T2LirOp *start_match = nullptr;
            Label cold_start;
        };
        std::vector<ScanLoop> scan_loops;
        /* Region containing block b, or -1. */
        std::vector<int> scan_of_block;
        int cur_scan = -1;
        bool cur_scan_adv = false;

        /* The block emitted right after the current one (layout order),
         * for fall-through elision of trailing unconditional branches. */
        uint32_t next_block_id = T2_LIR_NO_BLOCK;

        std::string emit_error;

    public:
        /* Emit-time facts, recorded for the structural asserts of
         * T2_EMIT_SELFTEST (repeatable verification that CPs, transfer
         * targets and side exits carry the exact cross-tier addresses
         * the pctab/loaded module prescribe). */
        struct EmitFact {
            enum Kind {
                CpCont,         /* value = the T1 continuation moved into LR  */
                CallTarget,     /* value = local callee T1 entry              */
                CallExport,     /* value = Export* of a remote callee         */
                TailTarget,     /* value = local tail-callee T1 entry         */
                TailExport,     /* value = Export* of a remote tail-callee    */
                SideExitPc,     /* value = unconditional side-exit T1 PC      */
                FailExitPc,     /* value = arith side-exit trampoline T1 PC   */
                BifExport,      /* value = Export* of a light-BIF callee      */
                BifSitePc,      /* value = the BIF site's own T1 PC (ARG3:
                                 * yield resume + raise address)              */
                BifContPc,      /* value = the BIF site's T1 continuation
                                 * (ARG5: trap/trace CP)                      */
                BackEdgeDemote, /* value = the back-edge demote target
                                 * (the function's own T1 entry L_f)      */
                SpecDeoptPc     /* value = a speculative op's deopt PC (its
                                 * own EFFECT site for the boundary shape,
                                 * the T1 entry body for the window shape)   */
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
                  fn(fn_), next_label((unsigned)fn_.blocks.size() + 1) {
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
        void *blob_rw = nullptr;

        /* Blob-relative back-edge resume PCs (valid after
         * finalize_to_allocator; P2 commit 5). */
        std::vector<T2EmitResult::ResumePoint> resume_points;

        bool failed() const {
            return !emit_error.empty();
        }
        const std::string &error() const {
            return emit_error;
        }

        /* Count of fused byte scan-run regions admit_scan_loop accepted
         * and emit_scan_region laid down (valid after emit_all; the
         * install-quality gate's "bs work eliminated" signal). A
         * multi-clause classifier whose recovered loop is not
         * single-path is rejected by admission and emitted 1:1, so it
         * contributes 0 here even though its LIR still carries bs ops. */
        unsigned emitted_scan_runs() const {
            unsigned n = 0;
            for (const ScanLoop &sl : scan_loops) {
                if (sl.emitted) {
                    n++;
                }
            }
            return n;
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

            detect_scan_loops();

            for (const T2LirBlock &b : fn.blocks) {
                /* Mirror emit_label: a block entry invalidates the
                 * register cache the reused T1 emitters maintain, and
                 * long bodies must give pending veneers a chance to
                 * flush. */
                reg_cache.invalidate();
                check_pending_stubs();

                bind_veneer_target(block_label(b.id));
                cur_block = &b;
                next_block_id = b.id + 1 < fn.blocks.size() ? b.id + 1
                                                            : T2_LIR_NO_BLOCK;

                cur_scan =
                        b.id < scan_of_block.size() ? scan_of_block[b.id] : -1;
                if (cur_scan >= 0) {
                    /* Emit the whole region contiguously in chain
                     * order at the first region block encountered, so
                     * the hot loop is one straight fall-through run
                     * with a single taken back-branch per iteration;
                     * later layout positions of region blocks emit
                     * nothing (their labels are bound in the chain). */
                    if (!scan_loops[cur_scan].emitted) {
                        emit_scan_region(scan_loops[cur_scan]);
                    }
                    continue;
                }

                for (size_t oi = 0; oi < b.ops.size(); oi++) {
                    if (failed()) {
                        return;
                    }
                    cur_op_idx = oi;
                    emit_op(b.ops[oi]);
                }
            }
            cur_scan = -1;

            emit_scan_cold();
            emit_resume_stubs();
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
            blob_rw = rw;

            for (const ResumeStub &st : resume_stubs) {
                T2EmitResult::ResumePoint pt;

                pt.offset = (uint32_t)((const char *)getCode(st.resume) -
                                       (const char *)exec);
                pt.t1_demote =
                        st.translate != nullptr ? st.translate : st.demote;
                resume_points.push_back(pt);
            }
            std::sort(resume_points.begin(),
                      resume_points.end(),
                      [](const T2EmitResult::ResumePoint &a,
                         const T2EmitResult::ResumePoint &b) {
                          return a.offset < b.offset;
                      });

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
         * fail PC. rawLabels must map it so resolve_beam_label() resolves.
         * `callsite` marks a callsite-class deopt (maps:fold Stage 1):
         * its trampoline additionally bumps the monitoring counter. */
        unsigned fail_label_num(const void *t1_pc,
                                const void *cont = nullptr,
                                bool callsite = false) {
            auto key = std::make_pair(std::make_pair(t1_pc, cont), callsite);
            auto it = fail_labels.find(key);
            if (it != fail_labels.end()) {
                return it->second;
            }
            unsigned n = next_label++;
            rawLabels.emplace(n, a.new_label());
            fail_labels.emplace(key, n);
            return n;
        }

        void emit_fail_trampolines() {
            for (const auto &pair : fail_labels) {
                const void *t1_pc = pair.first.first.first;
                const void *cont = pair.first.first.second;
                bool callsite = pair.first.second;

                reg_cache.invalidate();
                bind_veneer_target(rawLabels.at(pair.second));
                /* Absolute branch to the T1 PC. mov_imm materializes the
                 * 64-bit address; br transfers control. T1 re-executes and
                 * (for error paths) raises a byte-identical exception.
                 * A callee-window trampoline (P2 commit 8) first pushes
                 * the intrinsic call site's continuation — the CP the
                 * skipped callee prologue would have pushed — so the T1
                 * helper returns to the caller's own T1 continuation.
                 * A callsite-class trampoline (maps:fold Stage 1) bumps
                 * the deopt counter (racy, monitoring only) so deopt
                 * storms are visible. */
                if (callsite) {
                    mov_imm(TMP2, (Uint64)&erts_t2_callsite_deopts);
                    a.ldr(TMP3, a64::Mem(TMP2));
                    a.add(TMP3, TMP3, imm(1));
                    a.str(TMP3, a64::Mem(TMP2));
                }
                if (cont != nullptr) {
                    mov_imm(TMP1, (Uint64)cont);
                    a.str(TMP1, a64::Mem(E, -8).pre());
                }
                mov_imm(TMP1, (Uint64)t1_pc);
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

        /* Unconditional transfer to a block, eliding the branch when
         * the target is the next block in layout order (T1's emitters
         * fall through between ops the same way; check_pending_stubs
         * jumps around any veneer pool it flushes, so linear flow is
         * preserved). */
        void emit_goto(uint32_t succ) {
            if (succ != next_block_id) {
                a.b(block_label(succ));
                mark_unreachable();
            }
        }

        /* ---- op dispatch --------------------------------------------- */

        void emit_op(const T2LirOp &op) {
            /* Mirror T1's emit(): every op gives pending veneers and
             * constants a chance to flush, so no more than one op's
             * worth of code can pass between checks (a long straight-
             * line block would otherwise let pending stubs drift out
             * of their displacement range). */
            check_pending_stubs();

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
            case T2LirKind::IsTupleOfArity:
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
            case T2LirKind::MakeFun:
                emit_lir_make_fun(op);
                break;
            case T2LirKind::CmpBool:
                emit_lir_cmp_bool(op);
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
                emit_goto(op.succ_then);
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
            case T2LirKind::ReductionCheck:
            case T2LirKind::ReductionCheckCallee:
                emit_lir_reduction_check(op);
                break;
            case T2LirKind::ChargeReds:
                emit_lir_charge_reds(op);
                break;
            case T2LirKind::DemoteCallee:
                emit_lir_demote_callee(op);
                break;
            case T2LirKind::SpeculateSmall:
                emit_lir_speculate_small(op);
                break;
            case T2LirKind::AddSmall:
            case T2LirKind::SubSmall:
                emit_lir_addsub_small(op);
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
            case T2LirKind::IsFlatmapBounded:
                emit_lir_is_flatmap_bounded(op);
                break;
            case T2LirKind::FlatmapSize:
                emit_lir_flatmap_size(op);
                break;
            case T2LirKind::FlatmapKeyAt:
            case T2LirKind::FlatmapValAt:
                emit_lir_flatmap_at(op);
                break;
            case T2LirKind::FoldBudget:
                emit_lir_fold_budget(op);
                break;
            case T2LirKind::StartMatch:
                emit_lir_start_match(op);
                break;
            case T2LirKind::BsMatch:
                emit_lir_bs_match(op);
                break;
            case T2LirKind::BsGetTail:
                emit_bs_get_tail(ArgRegister(src_argval(op.srcs[0])),
                                 ArgRegister(loc_argval(op.dst)),
                                 ArgWord(op.live));
                break;
            case T2LirKind::BsTestTail:
                if (op.succ_then == T2_LIR_NO_BLOCK ||
                    op.succ_else == T2_LIR_NO_BLOCK) {
                    fail("bs_test_tail without both branch edges");
                    break;
                }
                emit_bs_test_tail2(
                        ArgLabel(ArgVal(ArgVal::Type::Label, 1 + op.succ_else)),
                        ArgRegister(src_argval(op.srcs[0])),
                        ArgWord((UWord)op.imm));
                if (!failed()) {
                    emit_goto(op.succ_then);
                }
                break;
            default:
                fail("unsupported LIR op kind in P1 identity emit");
                break;
            }
        }

        /* Phys id -> machine register: Phys k is the callee-save
         * register backing BEAM Xk (x25..x27; the regalloc pool stops
         * before XREG3, whose backing is caller-save in DEBUG builds,
         * T2_PHYS_POOL in t2_regalloc.cpp). */
        a64::Gp phys_gp(const PhysLoc &l) {
            ASSERT(l.is_phys() && l.num < 3);
            return register_backed_xregs[l.num];
        }

        void emit_lir_move(const T2LirOp &op) {
            if (op.num_srcs != 1 || op.dst.is_none()) {
                fail("malformed move");
                return;
            }

            /* Relaxed placements: Move is the one Phys-capable op; the
             * mov_arg Gp overloads are the documented register seam and
             * keep the T1 register cache coherent. */
            bool src_phys = !op.srcs[0].is_const && op.srcs[0].loc.is_phys();
            bool dst_phys = op.dst.is_phys();

            if (src_phys && dst_phys) {
                a.mov(phys_gp(op.dst), phys_gp(op.srcs[0].loc));
                return;
            }
            if (dst_phys) {
                mov_arg(phys_gp(op.dst), src_argval(op.srcs[0]));
                return;
            }
            if (src_phys) {
                mov_arg(loc_argval(op.dst), phys_gp(op.srcs[0].loc));
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
                emit_goto(op.succ_then);
            }
        }

        /* The fused tag-bit speculation guard (P2 commit 4; the MVP's
         * AND rule): AND the 1..4 values together, require every
         * small-tag bit, branch to the deopt trampoline otherwise. The
         * combined predicate holds only when *every* input has all
         * _TAG_IMMED1_SMALL bits set (AND, never OR — MVP Finding 4). */
        void emit_lir_speculate_small(const T2LirOp &op) {
            if (op.num_srcs < 1 || op.num_srcs > T2_LIR_MAX_SRCS ||
                op.t1_pc_fail == nullptr) {
                fail("malformed speculation guard");
                return;
            }

            const Label &fl = rawLabels.at(fail_label_num(op.t1_pc_fail,
                                                          op.t1_pc_cont,
                                                          op.spec_callsite));
            fact(EmitFact::SpecDeoptPc, op.beam_idx, op.t1_pc_fail);

            a64::Gp vals[T2_LIR_MAX_SRCS];
            const a64::Gp hints[T2_LIR_MAX_SRCS] = {TMP2, TMP3, TMP4, TMP5};

            for (uint8_t i = 0; i < op.num_srcs; i++) {
                if (op.srcs[i].is_const) {
                    fail("constant operand in a speculation guard");
                    return;
                }
                if (op.srcs[i].loc.is_phys()) {
                    vals[i] = phys_gp(op.srcs[i].loc);
                } else {
                    vals[i] = load_source(src_argval(op.srcs[i]), hints[i]).reg;
                }
            }

            comment("T2 speculate small (fused %u)", (unsigned)op.num_srcs);
            ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
            if (op.num_srcs == 1) {
                a.and_(TMP1, vals[0], imm(_TAG_IMMED1_MASK));
            } else {
                a.and_(TMP1, vals[0], vals[1]);
                for (uint8_t i = 2; i < op.num_srcs; i++) {
                    a.and_(TMP1, TMP1, vals[i]);
                }
                a.and_(TMP1, TMP1, imm(_TAG_IMMED1_MASK));
            }
            a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
            a.b_ne(resolve_label(fl, disp1MB));
        }

        /* Flag-checked one-untag add/sub (P2 commit 4; PLAN/T2/03 §9.4,
         * PLAN/T2/08 §4.4): clear one operand's tag bits, compute with a
         * flag-setting add/sub into scratch — tagged + cleared preserves
         * the tag — branch on overflow to the deopt trampoline, and only
         * then commit the result. Mirrors T1's small-int fast path
         * (arm/instr_arith.cpp emit_i_plus/minus) minus the per-op type
         * checks, which the dominating guards/proofs hoisted away. */
        void emit_lir_addsub_small(const T2LirOp &op,
                                   const Label *fail_override = nullptr) {
            bool is_add = op.kind == T2LirKind::AddSmall;

            if (op.num_srcs != 2 || op.dst.is_none() ||
                op.t1_pc_fail == nullptr) {
                fail("malformed flag-checked arithmetic op");
                return;
            }

            const Label &fl =
                    fail_override != nullptr
                            ? *fail_override
                            : rawLabels.at(fail_label_num(op.t1_pc_fail,
                                                          op.t1_pc_cont,
                                                          op.spec_callsite));
            if (fail_override == nullptr) {
                fact(EmitFact::SpecDeoptPc, op.beam_idx, op.t1_pc_fail);
            }

            comment("T2 %s small (flag-checked)", is_add ? "add" : "sub");

            a64::Gp lhs;
            if (op.srcs[0].is_const) {
                /* A proven-small constant operand (the pass never
                 * guards constants). */
                mov_imm(TMP2, op.srcs[0].term);
                lhs = TMP2;
            } else if (op.srcs[0].loc.is_phys()) {
                lhs = phys_gp(op.srcs[0].loc);
            } else {
                lhs = load_source(src_argval(op.srcs[0]), TMP2).reg;
            }

            /* Uniform scratch-then-commit ("deopt before the *commit*",
             * PLAN/T2/08 §4.4). A direct-to-destination variant for
             * window-shaped ops with non-prefix register-backed
             * destinations is legal (the deopt reads only
             * X0..arity-1) but measured ~5% slower on Apple Silicon —
             * likely loop-alignment/port effects — so it is not used. */
            a64::Gp out = ARG1;

            if (op.srcs[1].is_const) {
                Uint64 cleared =
                        (Uint64)op.srcs[1].term & ~(Uint64)_TAG_IMMED1_MASK;

                if (cleared <= 0xFFF) {
                    if (is_add) {
                        a.adds(out, lhs, imm(cleared));
                    } else {
                        a.subs(out, lhs, imm(cleared));
                    }
                } else {
                    mov_imm(TMP1, cleared);
                    if (is_add) {
                        a.adds(out, lhs, TMP1);
                    } else {
                        a.subs(out, lhs, TMP1);
                    }
                }
            } else {
                a64::Gp rhs;

                if (op.srcs[1].loc.is_phys()) {
                    rhs = phys_gp(op.srcs[1].loc);
                } else {
                    rhs = load_source(src_argval(op.srcs[1]), TMP3).reg;
                }
                a.and_(TMP1, rhs, imm(~_TAG_IMMED1_MASK));
                if (is_add) {
                    a.adds(out, lhs, TMP1);
                } else {
                    a.subs(out, lhs, TMP1);
                }
            }

            /* Deopt before the *commit* (PLAN/T2/08 §4.4). */
            a.b_vs(resolve_label(fl, disp1MB));

            if (op.dst.is_phys()) {
                a.mov(phys_gp(op.dst), ARG1);
            } else {
                mov_arg(loc_argval(op.dst), ARG1);
            }
        }

        /* Identity bs_start_match3 (P2 commit 7): the reused T1 emitter
         * with the fail edge redirected in-blob (or absent for a
         * decoded {f,0}). The destination write is conditional on the
         * success edge, exactly as in T1. */
        void emit_lir_start_match(const T2LirOp &op) {
            unsigned fail_num =
                    op.succ_else != T2_LIR_NO_BLOCK ? 1 + op.succ_else : 0;

            emit_i_bs_start_match3(
                    ArgRegister(src_argval(op.srcs[0])),
                    ArgWord(op.live),
                    ArgLabel(ArgVal(ArgVal::Type::Label, fail_num)),
                    ArgRegister(loc_argval(op.dst)));
            if (op.succ_then != T2_LIR_NO_BLOCK && !failed()) {
                emit_goto(op.succ_then);
            }
        }

        /* Identity bs_match (byte-aligned subset): synthesize the
         * ArgVal command list the unified T1 emitter walks
         * (beam_jit_bsm_init) and reuse it 1:1. Flags ride as a Word
         * (the parser's bs_get_integer2 path), and the heap need for a
         * get_tail is recomputed internally by opt_bsm_segments —
         * Need/Live stay 0 exactly as for a non-fused i_bs_match. */
        void emit_lir_bs_match(const T2LirOp &op) {
            if (op.succ_else == T2_LIR_NO_BLOCK ||
                op.succ_then == T2_LIR_NO_BLOCK) {
                fail("bs_match without both branch edges");
                return;
            }

            std::vector<ArgVal> list;

            for (uint32_t i = 0; i < op.num_bs_cmds; i++) {
                const T2LirBsCmd &c = fn.bs_cmds[op.first_bs_cmd + i];

                switch (c.kind) {
                case ERTS_T2_BS_ENSURE:
                    list.push_back(ArgImmed(am_ensure_at_least));
                    list.push_back(ArgWord(c.size));
                    list.push_back(ArgWord(c.unit));
                    break;
                case ERTS_T2_BS_READ_INT8:
                    list.push_back(ArgImmed(am_integer));
                    list.push_back(ArgWord(c.live));
                    list.push_back(ArgWord(0)); /* flags: unsigned big */
                    list.push_back(ArgWord(8));
                    list.push_back(ArgWord(1));
                    list.push_back(loc_argval(op.dst));
                    break;
                case ERTS_T2_BS_SKIP:
                    list.push_back(ArgImmed(am_skip));
                    list.push_back(ArgWord(c.size));
                    break;
                case ERTS_T2_BS_GET_TAIL:
                    list.push_back(ArgImmed(am_get_tail));
                    list.push_back(ArgWord(c.live));
                    list.push_back(ArgWord(1));
                    list.push_back(loc_argval(op.dst));
                    break;
                default:
                    fail("unknown bs_match command kind");
                    return;
                }
            }

            emit_i_bs_match_test_heap(
                    ArgLabel(ArgVal(ArgVal::Type::Label, 1 + op.succ_else)),
                    ArgRegister(src_argval(op.srcs[0])),
                    ArgWord(0),
                    ArgWord(0),
                    Span<const ArgVal>(list.data(), list.size()));

            if (!failed()) {
                emit_goto(op.succ_then);
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
                /* Matching the empty tuple {} is legal, so the arity can
                 * be 0 here (make_arityval() asserts > 0). */
                emit_i_test_arity(
                        failL,
                        ArgSource(src_argval(op.srcs[0])),
                        ArgWord(make_arityval_unchecked((UWord)op.imm)));
                break;
            case T2LirKind::IsTupleOfArity:
                emit_i_is_tuple_of_arity(
                        failL,
                        ArgSource(src_argval(op.srcs[0])),
                        ArgWord(make_arityval_unchecked((UWord)op.imm)));
                break;
            case T2LirKind::IsTaggedTuple:
                emit_i_is_tagged_tuple(
                        failL,
                        ArgSource(src_argval(op.srcs[0])),
                        ArgWord(make_arityval((UWord)op.imm)),
                        ArgAtom(ArgVal(ArgVal::Type::Immediate, op.imm_term)));
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
                emit_goto(op.succ_then);
            }
        }

        void emit_lir_make_tuple(const T2LirOp &op) {
            std::vector<ArgVal> elems;
            size_t count = op.num_srcs_ext > 0 ? op.num_srcs_ext : op.num_srcs;

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

        /* Value-producing total comparison (P2 commit 8): reuse T1's
         * boolean comparison emitters. =</> lower via the argument
         * swap T1's own generator applies. */
        void emit_lir_cmp_bool(const T2LirOp &op) {
            ArgSource a1 = ArgSource(src_argval(op.srcs[0]));
            ArgSource a2 = ArgSource(src_argval(op.srcs[1]));
            ArgRegister dst = ArgRegister(loc_argval(op.dst));

            switch ((T2OpKind)op.imm) {
            case T2OpKind::CmpGe:
                emit_bif_is_ge(a1, a2, dst);
                break;
            case T2OpKind::CmpLt:
                emit_bif_is_lt(a1, a2, dst);
                break;
            case T2OpKind::CmpLe:
                emit_bif_is_ge(a2, a1, dst);
                break;
            case T2OpKind::CmpGt:
                emit_bif_is_lt(a2, a1, dst);
                break;
            case T2OpKind::CmpEqExact:
                emit_bif_is_eq_exact(ArgRegister(src_argval(op.srcs[0])),
                                     a2,
                                     dst);
                break;
            case T2OpKind::CmpNeExact:
                emit_bif_is_ne_exact(ArgRegister(src_argval(op.srcs[0])),
                                     a2,
                                     dst);
                break;
            default:
                fail("unsupported cmp_bool kind");
                break;
            }
        }

        /* make_fun3 (P2 commit 8): T1's emit_i_make_fun3 with the
         * ErlFunEntry materialized as an immediate (op.target — the
         * retained lambda table's entry, resolved at isel; T1 gets the
         * same pointer through its load-time lambda patch). Heap need
         * was reserved by the preceding GcTest, exactly as in T1.
         *
         * T1's loader turns a num_free==0 make_fun3 into a load-time
         * literal instead; T2 creates the fun thing for real. The terms
         * are =:=-indistinguishable (same entry, no environment) — the
         * only delta is ERL_FUN_SIZE heap words per creation, noted in
         * the commit message. */
        void emit_lir_make_fun(const T2LirOp &op) {
            size_t num_free =
                    op.num_srcs_ext > 0 ? op.num_srcs_ext : op.num_srcs;
            Uint arity = (Uint)op.imm;

            ASSERT((Sint64)num_free == op.imm2);

            comment("T2 make_fun3 (arity %u, %u free)",
                    (unsigned)arity,
                    (unsigned)num_free);

            mov_imm(TMP2, (Uint64)op.target);
            mov_imm(TMP1, MAKE_FUN_HEADER(arity, num_free, 0));
            ERTS_CT_ASSERT_FIELD_PAIR(ErlFunThing, thing_word, entry.fun);
            a.stp(TMP1,
                  TMP2,
                  a64::Mem(HTOP, offsetof(ErlFunThing, thing_word)));

            for (size_t i = 0; i < num_free; i++) {
                const T2LirSrc &s = op.num_srcs_ext > 0
                                            ? fn.src_pool[op.pool_first + i]
                                            : op.srcs[i];
                int offset = offsetof(ErlFunThing, env) + i * sizeof(Eterm);

                mov_arg(a64::Mem(HTOP, offset), ArgSource(src_argval(s)));
            }

            {
                ArgVal dst = loc_argval(op.dst);
                auto d = init_destination(ArgRegister(dst), TMP1);

                a.orr(d.reg, HTOP, imm(TAG_PRIMARY_BOXED));
                add(HTOP, HTOP, (ERL_FUN_SIZE + num_free) * sizeof(Eterm));
                flush_var(d);
            }
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

                    /* Unbounded straight-line emission: give pending
                     * stubs a chance to flush, as T1's long emitters
                     * do (the pool is branched around, so the chain's
                     * fall-through is preserved). */
                    if (i != 0 && (i % 128) == 0) {
                        check_pending_stubs();
                    }

                    if (!is_small(c.value)) {
                        fail("arity switch case is not a small");
                        return;
                    }
                    cmp_arg(TMP1,
                            ArgWord(make_arityval(unsigned_val(c.value))));
                    a.b_eq(resolve_label(rawLabels.at(1 + c.target), disp1MB));
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

                /* Huge selects (e.g. unicode_util's generated tables)
                 * emit more than the stub displacement window in this
                 * one op: flush pending stubs periodically, as T1's
                 * long emitters do. */
                if (i != 0 && (i % 128) == 0) {
                    check_pending_stubs();
                }

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

            mov_imm(ARG8, (Uint64)op.target);     /* BIF C function      */
            mov_imm(ARG4, (Uint64)op.exp);        /* Export*             */
            mov_imm(ARG3, (Uint64)op.t1_pc_fail); /* T1 site         */
            mov_imm(ARG5, (Uint64)op.t1_pc_cont); /* T1 continuation */

            fragment_call(ga->get_t2_call_light_bif_shared());

            /* The fragment clobbers everything caller-saved; the
             * offset-keyed register cache self-invalidates, but be
             * explicit for the reused emitters that follow. */
            reg_cache.invalidate();
        }

        /* The recovered loop's back-edge reduction charge + yield
         * check (PLAN/T2/08 §5.4): `subs FCALLS, #1` is exactly T1's
         * per-iteration charge — T1 charges one reduction at the
         * callee entry's i_test_yield on every self tail call, the
         * recovered loop charges one at the back edge — so
         * process_info(_, reductions) is byte-identical.
         *
         * P2 commit 5: the yield path resumes INTO T2 (PLAN/T2/08
         * §4.5). The b.le targets a per-loop cold stub that routes
         * through i_test_yield_shared with ARG3 = an in-blob anchor
         * preceded by the function's real ErtsCodeMFA, so the shared
         * fragment's contract holds unchanged: c_p->current = our
         * MFA, X0..arity-1 saved (the back-edge sync map's fresh-call
         * vector), and c_p->i = anchor + TEST_YIELD_RETURN_OFFSET —
         * the blob's resume code, which re-enters the loop header.
         * This is the first (and only) T2 address class the runtime
         * ever holds; the resume_tab + jettison translation own its
         * lifetime. */
        void emit_lir_reduction_check(const T2LirOp &op) {
            bool callee = op.kind == T2LirKind::ReductionCheckCallee;

            if (install_entry == nullptr) {
                fail("recovered back-edge outside install-mode "
                     "emission");
                return;
            }
            if (!callee && op.target != install_entry) {
                fail("back-edge demote target is not the install "
                     "entry");
                return;
            }
            if (callee && (op.target == nullptr || op.t1_pc_fail == nullptr ||
                           op.t1_pc_cont == nullptr || op.arity == 0)) {
                fail("callee back-edge with unresolved addresses");
                return;
            }

            /* The back-jump that follows names the loop header. */
            if (cur_block == nullptr ||
                cur_op_idx + 1 >= cur_block->ops.size() ||
                cur_block->ops[cur_op_idx + 1].kind != T2LirKind::Jump) {
                fail("back-edge reduction check without a trailing "
                     "back-jump");
                return;
            }
            uint32_t header = cur_block->ops[cur_op_idx + 1].succ_then;

            fact(EmitFact::BackEdgeDemote, op.beam_idx, op.target);

            /* Cached slot values must not survive the boundary (the
             * yield path re-enters with only the canonical state). */
            reg_cache.invalidate();

            if (erts_alcu_enable_code_atags) {
                /* Mirror emit_i_test_yield's point-of-origin tag
                 * store (a T1 address; running processes' c_p->i is
                 * advisory). */
                mov_imm(ARG3, (Uint64)op.target);
                a.str(ARG3, a64::Mem(c_p, offsetof(Process, i)));
            }

            ResumeStub st;

            st.yield = a.new_label();
            st.anchor = a.new_label();
            st.resume = a.new_label();
            st.header = block_label(header);
            st.beam_idx = op.beam_idx;
            if (callee) {
                /* Intrinsic back edge (P2 commit 8): the stub embeds
                 * the CALLEE MFA (a suspended process introspects as
                 * yielded inside the T1 helper, saving callee-arity X
                 * registers — the fresh-call vector); the tombstone
                 * demote pushes the call site's CONT as the CP the
                 * skipped callee prologue would have pushed and enters
                 * the callee body (the back-edge subs already paid its
                 * entry charge); a parked c_p->i translates to the
                 * call site's own T1 PC, which re-executes call_ext
                 * over the saved vector. */
                st.mfa_m = op.mfa_m;
                st.mfa_f = op.mfa_f;
                st.mfa_ar = op.arity;
                st.cont = op.t1_pc_cont;
                st.demote = (const void *)((const char *)op.target +
                                           erts_t2_test_yield_return_offset());
                st.translate = op.t1_pc_fail;
            } else {
                st.demote = (const void *)((const char *)op.target +
                                           erts_t2_test_yield_return_offset());
            }
            resume_stubs.push_back(st);

            ASSERT(op.imm >= 1 && op.imm < 4096);
            a.subs(FCALLS, FCALLS, imm(op.imm));
            a.b_le(resume_stubs.back().yield);
            /* Fall through to the back-jump. */
        }

        /* `FCALLS -= imm`, no check (P2 commit 8): the reduction charge
         * T1 pays on an intrinsic loop's early-exit edge. The next
         * check (the caller's own back edge / call / entry sequence)
         * observes the debt, exactly as T1's lazy yield discipline
         * does. */
        void emit_lir_charge_reds(const T2LirOp &op) {
            ASSERT(op.imm >= 1 && op.imm < 4096);
            a.sub(FCALLS, FCALLS, imm(op.imm));
        }

        /* ---- maps:fold flatmap specialization (Stage 1) --------------- */

        /* The fused flatmap shape guard (design §"shape guard"; T1
         * precedent: instr_map.cpp's simplified multi-element lookup):
         * boxed ∧ header subtag == flatmap ∧ size <=
         * MAP_SMALL_MAP_LIMIT. Both edges stay in the blob — the else
         * edge is the general case (hashmap/iterator/non-map), which
         * runs the original call. */
        void emit_lir_is_flatmap_bounded(const T2LirOp &op) {
            if (op.num_srcs != 1 || op.srcs[0].is_const ||
                op.succ_then == T2_LIR_NO_BLOCK ||
                op.succ_else == T2_LIR_NO_BLOCK) {
                fail("malformed flatmap guard");
                return;
            }

            const Label &else_l = block_label(op.succ_else);
            a64::Gp map = load_source(src_argval(op.srcs[0]), TMP1).reg;

            comment("T2 is_flatmap_bounded");
            emit_is_boxed(resolve_label(else_l, disp1MB), map);
            emit_untag_ptr(TMP1, map);
            ERTS_CT_ASSERT_FIELD_PAIR(flatmap_t, thing_word, size);
            a.ldp(TMP2, TMP3, a64::Mem(TMP1, offsetof(flatmap_t, thing_word)));
            a.and_(TMP2, TMP2, imm(_HEADER_MAP_SUBTAG_MASK));
            a.cmp(TMP2, imm(HAMT_SUBTAG_HEAD_FLATMAP));
            a.b_ne(resolve_label(else_l, disp1MB));
            a.cmp(TMP3, imm(MAP_SMALL_MAP_LIMIT));
            a.b_hi(resolve_label(else_l, disp1MB));
            emit_goto(op.succ_then);
        }

        /* The flatmap's raw size word, tagged as a small. Never fails
         * (dominated by the shape guard). */
        void emit_lir_flatmap_size(const T2LirOp &op) {
            if (op.num_srcs != 1 || op.srcs[0].is_const || op.dst.is_none()) {
                fail("malformed flatmap_size");
                return;
            }

            a64::Gp map = load_source(src_argval(op.srcs[0]), TMP1).reg;

            comment("T2 flatmap_size");
            /* emit_untag_ptr, not emit_boxed_val's folded -2: a literal
             * map term carries the literal tag bit as well. */
            emit_untag_ptr(TMP1, map);
            a.ldr(TMP2, a64::Mem(TMP1, offsetof(flatmap_t, size)));
            a.lsl(TMP2, TMP2, imm(_TAG_IMMED1_SIZE));
            a.orr(TMP2, TMP2, imm(_TAG_IMMED1_SMALL));
            mov_arg(loc_argval(op.dst), TMP2);
        }

        /* K_i / V_i of a flatmap, indexed by a TAGGED small (provably
         * < size by the loop bound, so no checks). Keys live in the
         * separate keys tuple (skip its arity word); values are inline
         * after the flatmap header. */
        void emit_lir_flatmap_at(const T2LirOp &op) {
            bool is_key = op.kind == T2LirKind::FlatmapKeyAt;

            if (op.num_srcs != 2 || op.srcs[0].is_const ||
                op.srcs[1].is_const || op.dst.is_none()) {
                fail("malformed flatmap access");
                return;
            }

            a64::Gp map = load_source(src_argval(op.srcs[0]), TMP1).reg;
            a64::Gp idx = load_source(src_argval(op.srcs[1]), TMP2).reg;

            comment(is_key ? "T2 flatmap_key_at" : "T2 flatmap_val_at");
            a.lsr(TMP3, idx, imm(_TAG_IMMED1_SIZE));
            /* emit_untag_ptr throughout (a literal map / literal keys
             * tuple carries the literal tag bit too). */
            emit_untag_ptr(TMP1, map);
            if (is_key) {
                /* keys tuple term -> untagged ptr -> element 1 + i. */
                a.ldr(TMP1, a64::Mem(TMP1, offsetof(flatmap_t, keys)));
                emit_untag_ptr(TMP1, TMP1);
                a.add(TMP1, TMP1, imm(sizeof(Eterm)));
            } else {
                /* Values are inline right after the flatmap_t header. */
                a.add(TMP1, TMP1, imm(sizeof(flatmap_t)));
            }
            mov_arg(loc_argval(op.dst), a64::Mem(TMP1, TMP3, a64::lsl(3)));
        }

        /* The whole-fold reduction batch: charge = imm2 + imm *
         * untag(n). When FCALLS does not cover it, side-exit UNCHARGED
         * to the erased call's own T1 PC (the callsite-class
         * trampoline, which also bumps the deopt counter) — T1 then
         * re-executes the fold, charging and yielding exactly as it
         * always does. Otherwise pay the batch and run the fast loop,
         * which cannot yield (<= MAP_SMALL_MAP_LIMIT iterations). */
        void emit_lir_fold_budget(const T2LirOp &op) {
            if (op.num_srcs != 1 || op.srcs[0].is_const ||
                op.t1_pc_fail == nullptr || op.imm <= 0 || op.imm2 <= 0 ||
                op.imm2 >= 4096) {
                fail("malformed fold budget");
                return;
            }

            const Label &fl =
                    rawLabels.at(fail_label_num(op.t1_pc_fail, nullptr, true));

            fact(EmitFact::SpecDeoptPc, op.beam_idx, op.t1_pc_fail);

            a64::Gp n = load_source(src_argval(op.srcs[0]), TMP1).reg;

            comment("T2 fold budget (%ld + %ld*n)",
                    (long)op.imm2,
                    (long)op.imm);
            a.lsr(TMP2, n, imm(_TAG_IMMED1_SIZE));
            mov_imm(TMP3, (Uint64)op.imm);
            a.mul(TMP2, TMP2, TMP3);
            a.add(TMP2, TMP2, imm(op.imm2));
            /* FCALLS is the 32-bit w22 (beam_asm.hpp); the operand
             * widths must match or asmjit rejects the encoding. The
             * charge fits: n <= MAP_SMALL_MAP_LIMIT by the dominating
             * shape guard, so imm2 + imm*n is at most a few hundred. */
            a.cmp(FCALLS, TMP2.w());
            a.b_le(resolve_label(fl, disp1MB));
            a.sub(FCALLS, FCALLS, TMP2.w());
        }

        /* Terminator: demote the invocation to a T1 CALLEE body (P2
         * commit 8). The sync map pinned the callee's fresh-call
         * vector in X0..arity-1; push the intrinsic call site's T1
         * continuation as the CP the skipped callee prologue would
         * have pushed, then enter the callee body past its entry
         * check (the loop's charges already paid it). T1 re-executes
         * the iteration — raising the byte-identical error on the
         * error edges — and returns to the caller's own T1
         * continuation. */
        void emit_lir_demote_callee(const T2LirOp &op) {
            if (op.target == nullptr || op.t1_pc_cont == nullptr) {
                fail("demote-callee with unresolved addresses");
                return;
            }

            fact(EmitFact::SideExitPc, op.beam_idx, op.target);

            reg_cache.invalidate();
            mov_imm(TMP1, (Uint64)op.t1_pc_cont);
            a.str(TMP1, a64::Mem(E, -8).pre());
            mov_imm(TMP1, (Uint64)op.target);
            a.br(TMP1);
            mark_unreachable();
        }

        /* Emit the cold per-loop yield-setup + resume stubs collected
         * above. Layout per stub:
         *
         *   yield:  bump erts_t2_backedge_yields (racy, stats only)
         *           adr ARG3, anchor
         *           b i_test_yield_shared
         *           .align 8
         *           .quad module, function, arity   ; ErtsCodeMFA
         *   anchor: .quad 0                          ; tombstone flag
         *           .zero TEST_YIELD_RETURN_OFFSET-8 ; pad to contract
         *   resume: bump erts_t2_backedge_resumes
         *           adr TMP1, anchor; ldr; cbnz -> demote
         *           b header
         *   demote: mov TMP1, L_f+TEST_YIELD_RETURN_OFFSET; br TMP1
         *
         * The MFA must directly precede the anchor
         * (i_test_yield_shared reads [ARG3 - sizeof(ErtsCodeMFA)]);
         * the anchor..resume gap is exactly TEST_YIELD_RETURN_OFFSET
         * (the shared fragment computes the resume PC as ARG3 + that
         * offset), and its first word is the tombstone flag: jettison
         * sets it before the translation pass, so any process that
         * resumes in the window between prologue revert and c_p->i
         * translation demotes itself to the T1 body — the same
         * fresh-call semantics, no re-entry into a dying blob. */
        void emit_resume_stubs() {
            for (const ResumeStub &st : resume_stubs) {
                Label demote = a.new_label();
                Uint yield_off = erts_t2_test_yield_return_offset();
                static const byte zeros[64] = {0};
                Uint64 word;

                reg_cache.invalidate();

                a.bind(st.yield);
                if (st.scan >= 0) {
                    /* Fused scan loop: restore ErlSubBits.start (the
                     * pointer was advanced at the back edge, so the
                     * raw position is the next iteration's start —
                     * exactly the fresh-call contract the saved X
                     * vector needs). */
                    emit_scan_writeback(scan_loops[st.scan], false);
                }
                mov_imm(TMP1, (Uint64)&erts_t2_backedge_yields);
                a.ldr(TMP2, a64::Mem(TMP1));
                a.add(TMP2, TMP2, imm(1));
                a.str(TMP2, a64::Mem(TMP1));
                a.adr(ARG3, st.anchor);
                a.b(resolve_fragment(ga->get_i_test_yield_shared(), disp1MB));

                /* ErtsCodeMFA + anchor gap (data). An intrinsic stub
                 * embeds the CALLEE MFA (P2 commit 8) — introspection
                 * and the saved X count then match a T1 yield inside
                 * the helper exactly. */
                a.align(AlignMode::kData, 8);
                ERTS_CT_ASSERT(sizeof(ErtsCodeMFA) == 3 * sizeof(Uint64));
                word = (Uint64)(st.mfa_m != NIL ? st.mfa_m : fn.module);
                a.embed(&word, sizeof(word));
                word = (Uint64)(st.mfa_m != NIL ? st.mfa_f : fn.function);
                a.embed(&word, sizeof(word));
                word = (Uint64)(st.mfa_m != NIL ? (Uint64)st.mfa_ar
                                                : (Uint64)fn.arity);
                a.embed(&word, sizeof(word));

                a.bind(st.anchor);
                word = 0; /* the tombstone flag */
                a.embed(&word, sizeof(word));
                ASSERT(yield_off > sizeof(Uint64) &&
                       yield_off - sizeof(Uint64) <= sizeof(zeros));
                a.embed(zeros, yield_off - sizeof(Uint64));

                reg_cache.invalidate();
                a.bind(st.resume);
                mov_imm(TMP1, (Uint64)&erts_t2_backedge_resumes);
                a.ldr(TMP2, a64::Mem(TMP1));
                a.add(TMP2, TMP2, imm(1));
                a.str(TMP2, a64::Mem(TMP1));

                a.adr(TMP1, st.anchor);
                a.ldr(TMP1, a64::Mem(TMP1));
                a.cbnz(TMP1, demote);
                a.b(st.header);

                a.bind(demote);
                if (st.cont != nullptr) {
                    /* The CP push the skipped callee prologue would
                     * have done (P2 commit 8). */
                    mov_imm(TMP1, (Uint64)st.cont);
                    a.str(TMP1, a64::Mem(E, -8).pre());
                }
                mov_imm(TMP1, (Uint64)st.demote);
                a.br(TMP1);
                mark_unreachable();
            }
        }

        /* ---- fused byte-scan loops (see the struct comment) ---------- */

        /* The reserved region registers. TMP4..TMP6 are safe because
         * every op admitted into a region is emitted either right here
         * or by an emitter audited to touch only TMP1..TMP3/ARG1 and
         * the register-backed X file (emit_lir_move's mov_arg path,
         * emit_lir_addsub_small); no fragment call, runtime call or
         * unaudited T1 emitter is admitted. */
        const a64::Gp SCAN_PTR = TMP4;
        const a64::Gp SCAN_LIM = TMP5;
        const a64::Gp SCAN_BASE = TMP6;
        const a64::Gp SCAN_BYTE = ARG2;

        static bool scan_disabled() {
            static const bool off = getenv("T2_NO_SCAN") != nullptr;
            return off;
        }

        /* A small-integer constant in [0,255], untagged into *out. */
        static bool byte_const(const T2LirSrc &s, uint32_t *out) {
            if (!s.is_const || !is_small(s.term)) {
                return false;
            }
            Sint v = signed_val(s.term);
            if (v < 0 || v > 255) {
                return false;
            }
            *out = (uint32_t)v;
            return true;
        }

        void detect_scan_loops() {
            scan_of_block.assign(fn.blocks.size(), -1);
            if (install_entry == nullptr || scan_disabled()) {
                return;
            }

            /* Successor / predecessor sets over the LIR CFG. */
            size_t n = fn.blocks.size();
            std::vector<std::vector<uint32_t>> succs(n), preds(n);
            auto add_edge = [&](uint32_t from, uint32_t to) {
                if (to == T2_LIR_NO_BLOCK) {
                    return;
                }
                auto &v = succs[from];
                if (std::find(v.begin(), v.end(), to) == v.end()) {
                    v.push_back(to);
                    preds[to].push_back(from);
                }
            };
            for (const T2LirBlock &b : fn.blocks) {
                for (const T2LirOp &op : b.ops) {
                    add_edge(b.id, op.succ_then);
                    add_edge(b.id, op.succ_else);
                    if (op.kind == T2LirKind::Switch) {
                        for (uint32_t c = 0; c < op.num_cases; c++) {
                            add_edge(b.id,
                                     fn.switch_cases[op.first_case + c].target);
                        }
                        add_edge(b.id, op.default_target);
                    }
                }
            }

            for (const T2LirBlock &lb : fn.blocks) {
                if (lb.ops.size() < 2) {
                    continue;
                }
                const T2LirOp &rc = lb.ops[lb.ops.size() - 2];
                const T2LirOp &bj = lb.ops[lb.ops.size() - 1];

                if (rc.kind != T2LirKind::ReductionCheck ||
                    bj.kind != T2LirKind::Jump) {
                    continue;
                }
                uint32_t h = bj.succ_then;

                ScanLoop sl;
                sl.header = h;
                sl.latch = lb.id;
                sl.in_region.assign(n, false);

                /* Natural-loop body: backward flood from the latch,
                 * stopping at the header. */
                {
                    std::vector<uint32_t> work{lb.id};
                    sl.in_region[h] = true;
                    while (!work.empty()) {
                        uint32_t b = work.back();
                        work.pop_back();
                        if (sl.in_region[b]) {
                            continue;
                        }
                        sl.in_region[b] = true;
                        for (uint32_t p : preds[b]) {
                            if (!sl.in_region[p]) {
                                work.push_back(p);
                            }
                        }
                    }
                }

                if (!admit_scan_loop(sl, succs, preds)) {
                    continue;
                }

                /* Accepted: create the labels + claim the blocks. */
                sl.reload = a.new_label();
                sl.scan_resume = a.new_label();
                sl.scan_body = a.new_label();
                sl.unaligned = a.new_label();
                sl.cold_start = a.new_label();
                /* Rotation legality: the ensure check must be the very
                 * first thing an iteration executes. */
                sl.rotated = fn.blocks[sl.header].ops.size() == 1 &&
                             sl.blocks.size() >= 2 &&
                             sl.blocks[1] == sl.bs_block &&
                             fn.blocks[sl.bs_block].ops.size() >= 1 &&
                             fn.blocks[sl.bs_block].ops[0].kind ==
                                     T2LirKind::BsMatch;
                int idx = (int)scan_loops.size();
                for (uint32_t b : sl.blocks) {
                    scan_of_block[b] = idx;
                }
                scan_loops.push_back(std::move(sl));
            }
        }

        /* Region admission (must reject anything the fused emitters
         * cannot preserve exactly; a rejected loop is emitted 1:1). */
        bool admit_scan_loop(ScanLoop &sl,
                             const std::vector<std::vector<uint32_t>> &succs,
                             const std::vector<std::vector<uint32_t>> &preds) {
            size_t n = fn.blocks.size();

            /* Region blocks may only be entered through the header. */
            for (uint32_t b = 0; b < n; b++) {
                if (!sl.in_region[b] || b == sl.header) {
                    continue;
                }
                if (b < scan_of_block.size() && scan_of_block[b] >= 0) {
                    return false; /* already claimed */
                }
                for (uint32_t p : preds[b]) {
                    if (!sl.in_region[p]) {
                        return false;
                    }
                }
            }
            if (scan_of_block[sl.header] >= 0) {
                return false;
            }

            /* Single-path chain from the header back to itself. */
            {
                uint32_t cur = sl.header;
                size_t region_size = 0;
                for (uint32_t b = 0; b < n; b++) {
                    region_size += sl.in_region[b] ? 1 : 0;
                }
                std::vector<bool> seen(n, false);
                for (;;) {
                    if (seen[cur]) {
                        return false;
                    }
                    seen[cur] = true;
                    sl.blocks.push_back(cur);

                    uint32_t next = T2_LIR_NO_BLOCK;
                    for (uint32_t s : succs[cur]) {
                        if (!sl.in_region[s]) {
                            continue;
                        }
                        if (next != T2_LIR_NO_BLOCK && next != s) {
                            return false; /* two in-region successors */
                        }
                        next = s;
                    }
                    if (next == T2_LIR_NO_BLOCK) {
                        return false;
                    }
                    if (next == sl.header) {
                        break; /* the back edge closes the chain */
                    }
                    cur = next;
                }
                if (sl.blocks.size() != region_size) {
                    return false;
                }
                if (sl.blocks.back() != sl.latch) {
                    return false;
                }
            }

            /* Per-op admission, walking the chain. */
            bool adv = false;
            bool have_bs = false;
            sl.adv_in.assign(n, false);

            for (size_t ci = 0; ci < sl.blocks.size(); ci++) {
                uint32_t bid = sl.blocks[ci];
                const T2LirBlock &b = fn.blocks[bid];

                sl.adv_in[bid] = adv;

                for (size_t oi = 0; oi < b.ops.size(); oi++) {
                    const T2LirOp &op = b.ops[oi];

                    switch (op.kind) {
                    case T2LirKind::StartMatch: {
                        /* Only as the header's first op, src == dst
                         * (the context-reuse shape), an X slot. */
                        if (bid != sl.header || oi != 0 ||
                            sl.start_match != nullptr) {
                            return false;
                        }
                        if (op.srcs[0].is_const || !op.srcs[0].loc.is_xreg() ||
                            op.dst != op.srcs[0].loc) {
                            return false;
                        }
                        sl.start_match = &op;
                        sl.ctx_slot = op.dst;
                        break;
                    }
                    case T2LirKind::BsMatch: {
                        if (have_bs || sl.start_match == nullptr ||
                            op.imm != 0 /* heap need (get_tail) */) {
                            return false;
                        }
                        if (op.srcs[0].is_const ||
                            op.srcs[0].loc != sl.ctx_slot) {
                            return false;
                        }
                        if (op.succ_else == T2_LIR_NO_BLOCK ||
                            sl.in_region[op.succ_else]) {
                            return false; /* fail edge must exit */
                        }
                        /* Command shape: ensure_at_least first, then
                         * consuming commands with at most one
                         * READ_INT8, which must be the first
                         * consumer (the byte is read at the current
                         * position). */
                        if (op.num_bs_cmds < 2 ||
                            fn.bs_cmds[op.first_bs_cmd].kind !=
                                    ERTS_T2_BS_ENSURE ||
                            fn.bs_cmds[op.first_bs_cmd].unit != 1) {
                            return false;
                        }
                        uint32_t consumed = 0;
                        for (uint32_t i = 1; i < op.num_bs_cmds; i++) {
                            const T2LirBsCmd &c =
                                    fn.bs_cmds[op.first_bs_cmd + i];

                            if (c.kind == ERTS_T2_BS_READ_INT8) {
                                if (consumed != 0 || sl.read_byte) {
                                    return false;
                                }
                                sl.read_byte = true;
                                if (op.dst.is_none() || op.dst == sl.ctx_slot ||
                                    op.dst.is_phys()) {
                                    return false;
                                }
                                sl.byte_slot = op.dst;
                                consumed += 8;
                            } else if (c.kind == ERTS_T2_BS_SKIP) {
                                consumed += c.size;
                            } else {
                                return false;
                            }
                        }
                        uint32_t ensure = fn.bs_cmds[op.first_bs_cmd].size;
                        if (consumed == 0 || (consumed % 8) != 0 ||
                            ensure < consumed || (ensure % 8) != 0 ||
                            ensure / 8 > 4095) {
                            return false;
                        }
                        if (!sl.read_byte && !op.dst.is_none()) {
                            return false; /* dst without a read?? */
                        }
                        sl.adv_bytes = consumed / 8;
                        sl.ensure_bytes = ensure / 8;
                        if (sl.adv_bytes > 255) {
                            return false;
                        }
                        sl.bs_block = bid;
                        have_bs = true;
                        break;
                    }
                    case T2LirKind::CmpLt:
                    case T2LirKind::CmpGe:
                    case T2LirKind::CmpEqExact:
                    case T2LirKind::CmpNeExact:
                    case T2LirKind::CmpEq:
                    case T2LirKind::CmpNe: {
                        /* Byte guards only: one side the byte's slot,
                         * the other a constant in [0,255]; both edges
                         * present; the byte must have been read. */
                        uint32_t k;
                        if (!sl.read_byte || op.num_srcs != 2 ||
                            op.succ_then == T2_LIR_NO_BLOCK ||
                            op.succ_else == T2_LIR_NO_BLOCK) {
                            return false;
                        }
                        bool lhs_byte = !op.srcs[0].is_const &&
                                        op.srcs[0].loc == sl.byte_slot &&
                                        byte_const(op.srcs[1], &k);
                        bool rhs_byte = !op.srcs[1].is_const &&
                                        op.srcs[1].loc == sl.byte_slot &&
                                        byte_const(op.srcs[0], &k);
                        if (!lhs_byte && !rhs_byte) {
                            return false;
                        }
                        if (!adv) {
                            return false; /* guard before the read */
                        }
                        break;
                    }
                    case T2LirKind::Switch: {
                        if (!sl.read_byte || op.imm != 0 ||
                            op.srcs[0].is_const ||
                            op.srcs[0].loc != sl.byte_slot || !adv) {
                            return false;
                        }
                        for (uint32_t c = 0; c < op.num_cases; c++) {
                            const T2LirSwitchCase &sc =
                                    fn.switch_cases[op.first_case + c];
                            if (!is_small(sc.value) ||
                                signed_val(sc.value) < 0 ||
                                signed_val(sc.value) > 255) {
                                return false;
                            }
                        }
                        break;
                    }
                    case T2LirKind::AddSmall:
                    case T2LirKind::SubSmall: {
                        /* The audited flag-checked emitter. Must not
                         * write the context or byte slots. */
                        if (op.dst == sl.ctx_slot ||
                            (sl.read_byte && op.dst == sl.byte_slot) ||
                            op.dst.is_phys() || op.t1_pc_fail == nullptr) {
                            return false;
                        }
                        for (uint8_t s = 0; s < op.num_srcs; s++) {
                            if (!op.srcs[s].is_const &&
                                op.srcs[s].loc.is_phys()) {
                                return false;
                            }
                        }
                        break;
                    }
                    case T2LirKind::Move:
                        if (op.dst == sl.ctx_slot ||
                            (sl.read_byte && op.dst == sl.byte_slot) ||
                            op.dst.is_phys() ||
                            (!op.srcs[0].is_const &&
                             op.srcs[0].loc.is_phys())) {
                            return false;
                        }
                        break;
                    case T2LirKind::SpeculateSmall:
                        /* load_source hints reach TMP4/TMP5: not
                         * audited for the region registers. */
                        return false;
                    case T2LirKind::ReductionCheck:
                        if (bid != sl.latch || oi + 2 != b.ops.size() ||
                            op.sync == nullptr) {
                            return false;
                        }
                        break;
                    case T2LirKind::Jump:
                        if (op.succ_then != T2_LIR_NO_BLOCK &&
                            !sl.in_region[op.succ_then] &&
                            op.succ_then != sl.header) {
                            return false;
                        }
                        break;
                    default:
                        return false;
                    }

                    if (op.kind == T2LirKind::BsMatch) {
                        adv = true;
                    }
                }
            }

            if (!have_bs || sl.start_match == nullptr) {
                return false;
            }

            /* Is the byte's canonical slot dead outside the region's
             * byte guards? Then the tagged materialization can be
             * skipped entirely. Conservative: any read of the slot by
             * a non-guard op, any sync-map pin covering it, and any
             * phi home on it keep it alive. */
            if (sl.read_byte) {
                bool dead = true;
                auto sync_covers = [&](const T2SyncMap *m) {
                    if (m == nullptr) {
                        return false;
                    }
                    if (sl.byte_slot.is_xreg()) {
                        return (uint32_t)sl.byte_slot.num < m->x_live;
                    }
                    return m->frame_size != T2_NO_FRAME &&
                           (int32_t)sl.byte_slot.num < m->frame_size;
                };

                if (sync_covers(fn.entry_sync)) {
                    dead = false;
                }
                for (const T2LirBlock &b : fn.blocks) {
                    for (const T2LirPhi &phi : b.phis) {
                        if (phi.home == sl.byte_slot) {
                            dead = false;
                        }
                    }
                    for (const T2LirOp &op : b.ops) {
                        if (sync_covers(op.sync)) {
                            dead = false;
                        }
                        bool is_region_guard =
                                sl.in_region[b.id] &&
                                (op.kind == T2LirKind::CmpLt ||
                                 op.kind == T2LirKind::CmpGe ||
                                 op.kind == T2LirKind::CmpEqExact ||
                                 op.kind == T2LirKind::CmpNeExact ||
                                 op.kind == T2LirKind::CmpEq ||
                                 op.kind == T2LirKind::CmpNe ||
                                 op.kind == T2LirKind::Switch);
                        if (is_region_guard) {
                            continue; /* reads via the byte register */
                        }
                        for (uint8_t s = 0; s < op.num_srcs; s++) {
                            if (!op.srcs[s].is_const &&
                                op.srcs[s].loc == sl.byte_slot) {
                                dead = false;
                            }
                        }
                        for (uint32_t s = 0; s < op.num_srcs_ext; s++) {
                            const T2LirSrc &ps = fn.src_pool[op.pool_first + s];
                            if (!ps.is_const && ps.loc == sl.byte_slot) {
                                dead = false;
                            }
                        }
                    }
                }
                sl.byte_dead = dead;
            }

            return true;
        }

        /* The false-edge branch for a byte guard: cmp SCAN_BYTE, #k has
         * run; branch to `to` when the guard is false. */
        void emit_scan_guard_false(T2LirKind kind,
                                   bool byte_is_lhs,
                                   const Label &to) {
            switch (kind) {
            case T2LirKind::CmpLt:
                if (byte_is_lhs) {
                    a.b_hs(to); /* !(B < k)  <=> B >= k */
                } else {
                    a.b_ls(to); /* !(k < B)  <=> B <= k */
                }
                break;
            case T2LirKind::CmpGe:
                if (byte_is_lhs) {
                    a.b_lo(to); /* !(B >= k) <=> B < k  */
                } else {
                    a.b_hi(to); /* !(k >= B) <=> B > k  */
                }
                break;
            case T2LirKind::CmpEqExact:
            case T2LirKind::CmpEq:
                a.b_ne(to);
                break;
            case T2LirKind::CmpNeExact:
            case T2LirKind::CmpNe:
                a.b_eq(to);
                break;
            default:
                fail("unexpected byte-guard kind");
                break;
            }
        }

        /* Exit thunk: restore ErlSubBits.start (the deferred position)
         * and continue at the out-of-region block. */
        Label scan_exit_label(ScanLoop &sl, uint32_t target, bool advanced) {
            for (auto &e : sl.exit_thunks) {
                if (e.first == ScanExitKey{target, advanced}) {
                    return e.second;
                }
            }
            sl.exit_thunks.push_back(
                    {ScanExitKey{target, advanced}, a.new_label()});
            return sl.exit_thunks.back().second;
        }

        /* Write-back deopt trampoline (in-region speculative ops).
         * Only un-commit-free trampolines are shared. */
        Label scan_wb_label(ScanLoop &sl, const void *t1_pc, bool advanced) {
            for (auto &t : sl.wb_tramps) {
                if (t.t1_pc == t1_pc && t.advanced == advanced &&
                    t.uncommit_slot.is_none()) {
                    return t.label;
                }
            }
            sl.wb_tramps.push_back(
                    ScanLoop::WbTramp{t1_pc, advanced, a.new_label()});
            return sl.wb_tramps.back().label;
        }

        /* ErlSubBits.start := ((SCAN_PTR - SCAN_BASE) + adv?) << 3.
         * Uses TMP1..TMP3; the context term is read from its slot. */
        void emit_scan_writeback(const ScanLoop &sl, bool advanced) {
            a.sub(TMP1, SCAN_PTR, SCAN_BASE);
            if (advanced) {
                a.add(TMP1, TMP1, imm(sl.adv_bytes));
            }
            a.lsl(TMP1, TMP1, imm(3));
            mov_arg(TMP2, ArgVal(ArgVal::Type::XReg, sl.ctx_slot.num));
            emit_untag_ptr(TMP3, TMP2);
            a.str(TMP1, a64::Mem(TMP3, offsetof(ErlSubBits, start)));
        }

        /* Single-op byte-guard block: the op, or null. */
        const T2LirOp *scan_lone_guard(uint32_t bid) {
            const T2LirBlock &b = fn.blocks[bid];

            if (b.ops.size() != 1) {
                return nullptr;
            }
            switch (b.ops[0].kind) {
            case T2LirKind::CmpLt:
            case T2LirKind::CmpGe:
            case T2LirKind::CmpEqExact:
            case T2LirKind::CmpNeExact:
            case T2LirKind::CmpEq:
            case T2LirKind::CmpNe:
                return &b.ops[0];
            default:
                return nullptr;
            }
        }

        void emit_scan_region(ScanLoop &sl) {
            sl.emitted = true;

            for (size_t ci = 0; ci < sl.blocks.size(); ci++) {
                const T2LirBlock &b = fn.blocks[sl.blocks[ci]];

                reg_cache.invalidate();
                check_pending_stubs();
                bind_veneer_target(block_label(b.id));
                cur_block = &b;
                /* Fall-through elision follows the *chain*, not the
                 * layout: the next emitted block is the next chain
                 * block (the back edge to the header always branches). */
                next_block_id = ci + 1 < sl.blocks.size() ? sl.blocks[ci + 1]
                                                          : T2_LIR_NO_BLOCK;
                cur_scan_adv = sl.adv_in[b.id];

                /* Range fusion: two adjacent lone byte guards forming
                 * `lo =< B` then `B =< hi` with one shared exit fold
                 * into `(B - lo) <=u (hi - lo)` — one branch. */
                if (ci + 1 < sl.blocks.size()) {
                    const T2LirOp *g1 = scan_lone_guard(sl.blocks[ci]);
                    const T2LirOp *g2 = scan_lone_guard(sl.blocks[ci + 1]);
                    uint32_t lo = 0, hi = 0;

                    if (g1 != nullptr && g2 != nullptr &&
                        g1->kind == T2LirKind::CmpGe &&
                        g2->kind == T2LirKind::CmpGe && !g1->srcs[0].is_const &&
                        g1->srcs[0].loc == sl.byte_slot &&
                        byte_const(g1->srcs[1], &lo) && !g2->srcs[1].is_const &&
                        g2->srcs[1].loc == sl.byte_slot &&
                        byte_const(g2->srcs[0], &hi) && lo <= hi &&
                        g1->succ_else == g2->succ_else &&
                        !sl.in_region[g1->succ_else] &&
                        g1->succ_then == sl.blocks[ci + 1]) {
                        comment("T2 fused scan: range guard %u..%u", lo, hi);
                        bind_veneer_target(block_label(sl.blocks[ci + 1]));
                        if (lo != 0) {
                            a.sub(TMP1, SCAN_BYTE, imm(lo));
                            a.cmp(TMP1, imm(hi - lo));
                        } else {
                            a.cmp(SCAN_BYTE, imm(hi));
                        }
                        a.b_hi(scan_exit_label(sl,
                                               g1->succ_else,
                                               cur_scan_adv));
                        ci++; /* consumed both blocks */
                        next_block_id = ci + 1 < sl.blocks.size()
                                                ? sl.blocks[ci + 1]
                                                : T2_LIR_NO_BLOCK;
                        emit_goto(g2->succ_then);
                        continue;
                    }
                }

                for (size_t oi = 0; oi < b.ops.size(); oi++) {
                    if (failed()) {
                        return;
                    }
                    cur_op_idx = oi;
                    emit_scan_op(sl, b, b.ops[oi]);
                }
            }
        }

        void emit_scan_op(ScanLoop &sl,
                          const T2LirBlock &b,
                          const T2LirOp &op) {
            switch (op.kind) {
            case T2LirKind::StartMatch: {
                /* Hot path: the fast is-a-match-context check only;
                 * src == dst, so nothing moves. The slow path (fresh
                 * binary: allocate a context, may GC) runs the full
                 * reused T1 emitter in a cold stub and rejoins at the
                 * reload. Entered only through the block label —
                 * entry and yield-resume — where the context's start
                 * field is coherent by the write-back contract. */
                comment("T2 fused scan: start_match fast check");
                mov_arg(TMP1, ArgVal(ArgVal::Type::XReg, sl.ctx_slot.num));
                /* Non-boxed terms must not be dereferenced: the cold
                 * slow path re-runs the full T1 emitter, whose own
                 * boxed test routes them to the decoded fail edge. */
                emit_is_boxed(sl.cold_start, TMP1);
                emit_untag_ptr(TMP1, TMP1);
                ERTS_CT_ASSERT_FIELD_PAIR(ErlSubBits, thing_word, base_flags);
                a.ldp(TMP2, TMP3, a64::Mem(TMP1));
                ERTS_CT_ASSERT((HEADER_SUB_BITS & _TAG_PRIMARY_MASK) == 0 &&
                               (ERL_SUB_BITS_FLAG_MASK == _TAG_PRIMARY_MASK));
                a.bfi(TMP2, TMP3, imm(0), imm(2));
                a.cmp(TMP2,
                      imm(HEADER_SUB_BITS | ERL_SUB_BITS_FLAGS_MATCH_CONTEXT));
                a.b_ne(sl.cold_start);

                /* Reload: hoist position/end/base as byte pointers.
                 * A bit-offset (unaligned) context demotes the whole
                 * invocation to the T1 body — the fresh-call window
                 * contract at the header. */
                a.bind(sl.reload);
                comment("T2 fused scan: reload ptr/lim/base");
                reg_cache.invalidate();
                mov_arg(TMP1, ArgVal(ArgVal::Type::XReg, sl.ctx_slot.num));
                emit_untag_ptr(TMP1, TMP1);
                ERTS_CT_ASSERT_FIELD_PAIR(ErlSubBits, start, end);
                a.ldp(TMP2, TMP3, a64::Mem(TMP1, offsetof(ErlSubBits, start)));
                a.tst(TMP2, imm(7));
                a.b_ne(sl.unaligned);
                a.ldr(SCAN_BASE,
                      a64::Mem(TMP1, offsetof(ErlSubBits, base_flags)));
                a.and_(SCAN_BASE,
                       SCAN_BASE,
                       imm(~(Uint64)ERL_SUB_BITS_FLAG_MASK));
                a.add(SCAN_PTR, SCAN_BASE, TMP2, a64::lsr(3));
                a.add(SCAN_LIM, SCAN_BASE, TMP3, a64::lsr(3));

                a.bind(sl.scan_resume);
                if (op.succ_then != T2_LIR_NO_BLOCK) {
                    /* Folded StartMatch: continue on the success edge
                     * (the fail edge is fully covered by the cold slow
                     * path's own emitter). */
                    emit_goto(op.succ_then);
                }
                break;
            }

            case T2LirKind::BsMatch: {
                comment("T2 fused scan: ensure %u + read", sl.ensure_bytes);
                Label fail_thunk = scan_exit_label(sl, op.succ_else, false);

                if (sl.ensure_bytes == 1) {
                    a.cmp(SCAN_PTR, SCAN_LIM);
                    a.b_hs(fail_thunk);
                } else {
                    a.add(TMP1, SCAN_PTR, imm(sl.ensure_bytes));
                    a.cmp(TMP1, SCAN_LIM);
                    a.b_hi(fail_thunk);
                }
                if (sl.rotated) {
                    /* The rotated back edge re-checks the bound itself
                     * and lands here, past the check above. */
                    a.bind(sl.scan_body);
                }
                if (sl.read_byte) {
                    a.ldrb(SCAN_BYTE.w(), a64::Mem(SCAN_PTR));
                    if (!sl.byte_dead) {
                        /* Materialize the tagged byte in its canonical
                         * slot: everything outside the fused emitters
                         * (sync maps, exit blocks, arithmetic) reads
                         * the slot, exactly as in the 1:1 emission.
                         * Skipped when the slot is provably dead
                         * outside the region's own byte guards. */
                        ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
                        a.lsl(TMP1, SCAN_BYTE, imm(_TAG_IMMED1_SIZE));
                        a.orr(TMP1, TMP1, imm(_TAG_IMMED1_SMALL));
                        mov_arg(loc_argval(op.dst), TMP1);
                    }
                }
                cur_scan_adv = true;
                if (op.succ_then != T2_LIR_NO_BLOCK) {
                    emit_goto(op.succ_then);
                }
                break;
            }

            case T2LirKind::CmpLt:
            case T2LirKind::CmpGe:
            case T2LirKind::CmpEqExact:
            case T2LirKind::CmpNeExact:
            case T2LirKind::CmpEq:
            case T2LirKind::CmpNe: {
                bool byte_lhs =
                        !op.srcs[0].is_const && op.srcs[0].loc == sl.byte_slot;
                uint32_t k = 0;

                byte_const(op.srcs[byte_lhs ? 1 : 0], &k);
                a.cmp(SCAN_BYTE, imm(k));

                Label to = sl.in_region[op.succ_else]
                                   ? block_label(op.succ_else)
                                   : scan_exit_label(sl,
                                                     op.succ_else,
                                                     cur_scan_adv);
                emit_scan_guard_false(op.kind, byte_lhs, to);
                if (failed()) {
                    return;
                }
                if (sl.in_region[op.succ_then]) {
                    emit_goto(op.succ_then);
                } else {
                    a.b(scan_exit_label(sl, op.succ_then, cur_scan_adv));
                    mark_unreachable();
                }
                break;
            }

            case T2LirKind::Switch: {
                if (op.num_cases == 2 &&
                    fn.switch_cases[op.first_case].target ==
                            fn.switch_cases[op.first_case + 1].target &&
                    !sl.in_region[fn.switch_cases[op.first_case].target]) {
                    /* Two not-equal cases to one exit: cmp + ccmp +
                     * one branch. When the first compare misses, ccmp
                     * compares against the second value; when it
                     * hits, ccmp forces Z so the b.eq is taken. */
                    const T2LirSwitchCase &c0 = fn.switch_cases[op.first_case];
                    const T2LirSwitchCase &c1 =
                            fn.switch_cases[op.first_case + 1];

                    uint32_t k0 = (uint32_t)signed_val(c0.value);
                    uint32_t k1 = (uint32_t)signed_val(c1.value);

                    a.cmp(SCAN_BYTE, imm(k0));
                    if (k1 <= 31) {
                        /* ccmp's immediate form is 5 bits only. */
                        a.ccmp(SCAN_BYTE,
                               imm(k1),
                               imm(NZCV::kEqual),
                               imm(arm::CondCode::kNE));
                    } else {
                        mov_imm(TMP1, k1);
                        a.ccmp(SCAN_BYTE,
                               TMP1,
                               imm(NZCV::kEqual),
                               imm(arm::CondCode::kNE));
                    }
                    a.b_eq(scan_exit_label(sl, c0.target, cur_scan_adv));
                    if (sl.in_region[op.default_target]) {
                        emit_goto(op.default_target);
                    } else {
                        a.b(scan_exit_label(sl,
                                            op.default_target,
                                            cur_scan_adv));
                        mark_unreachable();
                    }
                    break;
                }
                for (uint32_t c = 0; c < op.num_cases; c++) {
                    const T2LirSwitchCase &sc =
                            fn.switch_cases[op.first_case + c];

                    a.cmp(SCAN_BYTE, imm((uint32_t)signed_val(sc.value)));
                    if (sl.in_region[sc.target]) {
                        a.b_eq(block_label(sc.target));
                    } else {
                        a.b_eq(scan_exit_label(sl, sc.target, cur_scan_adv));
                    }
                }
                if (sl.in_region[op.default_target]) {
                    emit_goto(op.default_target);
                } else {
                    a.b(scan_exit_label(sl, op.default_target, cur_scan_adv));
                    mark_unreachable();
                }
                break;
            }

            case T2LirKind::AddSmall:
            case T2LirKind::SubSmall: {
                /* Window shape (no sync): re-executes the iteration —
                 * the position write-back must be the iteration-start
                 * value. Boundary shape (sync map kept): re-executes
                 * just this op — the T1-true position at this point. */
                bool is_add = op.kind == T2LirKind::AddSmall;
                bool adv = op.sync != nullptr && cur_scan_adv;

                /* Direct-commit form for the loop accumulator: dst ==
                 * lhs slot, small-constant rhs, register-backed X —
                 * `adds xN, xN, #k` makes the loop-carried dependency
                 * one cycle instead of scratch-then-commit's two. On
                 * overflow the register holds the wrapped sum; the
                 * trampoline un-commits it exactly (two's complement)
                 * before the write-back + T1 re-execution. */
                if (!op.srcs[0].is_const && op.srcs[0].loc == op.dst &&
                    op.srcs[1].is_const && op.dst.is_xreg() &&
                    op.dst.num < num_register_backed_xregs) {
                    Uint64 cleared =
                            (Uint64)op.srcs[1].term & ~(Uint64)_TAG_IMMED1_MASK;

                    if (cleared <= 0xFFF) {
                        a64::Gp dstreg = register_backed_xregs[op.dst.num];
                        ScanLoop::WbTramp t;

                        t.t1_pc = op.t1_pc_fail;
                        t.advanced = adv;
                        t.label = a.new_label();
                        t.uncommit_slot = op.dst;
                        t.uncommit_imm = cleared;
                        t.uncommit_add = is_add;
                        sl.wb_tramps.push_back(t);

                        comment("T2 fused scan: direct %s small",
                                is_add ? "add" : "sub");
                        if (is_add) {
                            a.adds(dstreg, dstreg, imm(cleared));
                        } else {
                            a.subs(dstreg, dstreg, imm(cleared));
                        }
                        a.b_vs(sl.wb_tramps.back().label);
                        break;
                    }
                }

                Label deopt = scan_wb_label(sl, op.t1_pc_fail, adv);

                emit_lir_addsub_small(op, &deopt);
                break;
            }

            case T2LirKind::Move:
                emit_lir_move(op);
                break;

            case T2LirKind::ReductionCheck:
                emit_scan_reduction_check(sl, op);
                break;

            case T2LirKind::Jump:
                if (op.succ_then == sl.header) {
                    if (!sl.rotated) {
                        /* The hot back edge: skip the per-entry check
                         * + reload. */
                        a.b(sl.scan_resume);
                    }
                    /* Rotated loops branched from the reduction check
                     * already. */
                    mark_unreachable();
                } else {
                    emit_goto(op.succ_then);
                }
                break;

            default:
                fail("unadmitted op reached the fused scan emitter");
                break;
            }
        }

        /* The fused loop's back edge: advance the byte pointer, charge
         * reductions, yield through a write-back resume stub. */
        void emit_scan_reduction_check(ScanLoop &sl, const T2LirOp &op) {
            if (install_entry == nullptr || op.target != install_entry) {
                fail("fused scan back-edge outside install mode");
                return;
            }

            fact(EmitFact::BackEdgeDemote, op.beam_idx, op.target);
            reg_cache.invalidate();

            if (erts_alcu_enable_code_atags) {
                mov_imm(ARG3, (Uint64)op.target);
                a.str(ARG3, a64::Mem(c_p, offsetof(Process, i)));
            }

            a.add(SCAN_PTR, SCAN_PTR, imm(sl.adv_bytes));

            ResumeStub st;

            st.yield = a.new_label();
            st.anchor = a.new_label();
            st.resume = a.new_label();
            st.header = block_label(sl.header);
            st.demote = (const void *)((const char *)op.target +
                                       erts_t2_test_yield_return_offset());
            st.beam_idx = op.beam_idx;
            st.scan = cur_scan;
            resume_stubs.push_back(st);

            ASSERT(op.imm >= 1 && op.imm < 4096);
            a.subs(FCALLS, FCALLS, imm(op.imm));
            a.b_le(resume_stubs.back().yield);

            if (sl.rotated) {
                /* Rotated back edge: re-check the bound here and jump
                 * straight to the post-ensure body — one conditional
                 * taken branch per iteration. The fall-through is the
                 * end-of-input exit (position raw = the next
                 * iteration's start, i.e. the unadvanced thunk). */
                const T2LirOp &bs = fn.blocks[sl.bs_block].ops[0];
                Label fail_thunk = scan_exit_label(sl, bs.succ_else, false);

                if (sl.ensure_bytes == 1) {
                    a.cmp(SCAN_PTR, SCAN_LIM);
                    a.b_lo(sl.scan_body);
                } else {
                    a.add(TMP1, SCAN_PTR, imm(sl.ensure_bytes));
                    a.cmp(TMP1, SCAN_LIM);
                    a.b_ls(sl.scan_body);
                }
                a.b(fail_thunk);
                mark_unreachable();
            }
            /* Otherwise fall through to the back-jump (b scan_resume). */
        }

        /* Cold region continuations: exit thunks, write-back deopt
         * trampolines, the unaligned demote and the StartMatch slow
         * path. */
        void emit_scan_cold() {
            for (ScanLoop &sl : scan_loops) {
                if (failed()) {
                    return;
                }

                for (auto &e : sl.exit_thunks) {
                    reg_cache.invalidate();
                    a.bind(e.second);
                    comment("T2 fused scan: exit thunk (adv=%d)",
                            (int)e.first.advanced);
                    emit_scan_writeback(sl, e.first.advanced);
                    a.b(block_label(e.first.target));
                    mark_unreachable();
                }

                for (auto &t : sl.wb_tramps) {
                    reg_cache.invalidate();
                    a.bind(t.label);
                    comment("T2 fused scan: write-back deopt (adv=%d)",
                            (int)t.advanced);
                    if (!t.uncommit_slot.is_none()) {
                        /* Un-commit the direct flag-checked write:
                         * the wrapped sum minus/plus the addend is
                         * the exact original (two's complement). */
                        a64::Gp r = register_backed_xregs[t.uncommit_slot.num];

                        if (t.uncommit_add) {
                            a.sub(r, r, imm(t.uncommit_imm));
                        } else {
                            a.add(r, r, imm(t.uncommit_imm));
                        }
                    }
                    emit_scan_writeback(sl, t.advanced);
                    fact(EmitFact::SpecDeoptPc, 0, t.t1_pc);
                    mov_imm(TMP1, (Uint64)t.t1_pc);
                    a.br(TMP1);
                    mark_unreachable();
                }

                {
                    /* Unaligned (bit-offset) context: demote the whole
                     * invocation to the T1 body — the header state is
                     * exactly the fresh-call vector, and the position
                     * field is coherent on every header entry path. */
                    reg_cache.invalidate();
                    a.bind(sl.unaligned);
                    comment("T2 fused scan: unaligned context demote");
                    mov_imm(TMP1,
                            (Uint64)install_entry +
                                    erts_t2_test_yield_return_offset());
                    a.br(TMP1);
                    mark_unreachable();
                }

                {
                    /* StartMatch slow path: the full reused T1
                     * emitter (allocates the context; may GC — the
                     * op's sync contract, canonical state holds
                     * here), then rejoin at the reload. */
                    const T2LirOp &sm = *sl.start_match;

                    reg_cache.invalidate();
                    a.bind(sl.cold_start);
                    comment("T2 fused scan: start_match slow path");

                    unsigned fail_num = sm.succ_else != T2_LIR_NO_BLOCK
                                                ? 1 + sm.succ_else
                                                : 0;
                    emit_i_bs_start_match3(
                            ArgRegister(src_argval(sm.srcs[0])),
                            ArgWord(sm.live),
                            ArgLabel(ArgVal(ArgVal::Type::Label, fail_num)),
                            ArgRegister(loc_argval(sm.dst)));
                    a.b(sl.reload);
                    mark_unreachable();
                }
            }
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
        out->rw_base = ma.blob_rw;
        out->resume_points = ma.resume_points;
        out->scan_runs = ma.emitted_scan_runs();
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
        hp = HAlloc(p, Sint(sz));
        Eterm str = erts_bld_string_n(&hp, NULL, s, (Sint)sys_strlen(s));
        return TUPLE2(hp, am_error, str);
    }
} // namespace

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
            [&](T2Function &hir) {
                /* No loop recovery here: a standalone exec blob has
                 * no patched prologue, so a back-edge demote's
                 * ARG3 = L_f contract cannot hold. The identity
                 * shape (self tail call through the T1 entry) is
                 * kept instead. */
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
        return t2_exec_error(p,
                             build_err.empty() ? "build_failed"
                                               : build_err.c_str());
    }

    if (blob == NULL) {
        return t2_exec_error(p,
                             pipe_err.empty() ? "emit_failed"
                                              : pipe_err.c_str());
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

    erts_fprintf(stderr,
                 "T2 emit self-test: LIR:\n%s",
                 t2_lir_dump(fn).c_str());

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

    erts_fprintf(stderr, "T2 emit self-test: emitted 1-op blob at %p\n", code);
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
 *   5. (P2) the self-recursive tail call is loop-recovered: the      *
 *      entry block is the synthesized preheader, the header holds   *
 *      one phi per parameter, and the latch carries a back-edge     *
 *      ReductionCheck whose demote target is total/2's own T1       *
 *      entry — no self TailCall survives.                           *
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

#    define T2_STRUCT_CHECK(Cond, What)                                        \
        do {                                                                   \
            if (!(Cond)) {                                                     \
                erts_fprintf(stderr,                                           \
                             "T2 emit module self-test FAILED: %s\n",          \
                             (What));                                          \
                failures++;                                                    \
            }                                                                  \
        } while (0)

        T2BuildStatus status = t2_build_for_debug(
                ret,
                AM_total,
                2,
                [&](T2Function &hir) {
                    T2LirFunction lir;
                    std::string err;

                    ran = true;

                    /* P2: recover the self-recursion loop exactly as
                     * the install pipeline does, re-validate, and
                     * assert the recovered structure. */
                    {
                        bool recovered = false;

                        if (!t2_loop_recover(hir, &recovered, &err) ||
                            !recovered || !t2_validate(hir, &err)) {
                            erts_fprintf(stderr,
                                         "T2 emit module self-test "
                                         "FAILED: recovery: %s\n",
                                         err.c_str());
                            failures++;
                            return;
                        }
                    }
                    {
                        T2LoopInfo li;

                        t2_loop_info(hir, &li);
                        T2_STRUCT_CHECK(li.loops.size() == 1,
                                        "recovered total/2 must have "
                                        "exactly one loop");
                        if (li.loops.size() == 1) {
                            const T2Loop &loop = li.loops[0];

                            T2_STRUCT_CHECK(loop.preheader == 0,
                                            "the entry block must be "
                                            "the synthesized "
                                            "preheader");
                            T2_STRUCT_CHECK(
                                    loop.header ==
                                            (uint32_t)hir.blocks.size() - 1,
                                    "the synthesized header is the "
                                    "last block");
                            T2_STRUCT_CHECK(loop.latches.size() == 1,
                                            "total/2 has one latch");
                            T2_STRUCT_CHECK(!loop.exits.empty(),
                                            "the loop has exits");
                        }
                    }

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
                    const void *entry_pc = (const void *)erts_t2_pc_lookup_kind(
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
                                    y_stores.size() == (size_t)m->frame_size;

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
                        const void *eff = (const void *)erts_t2_pc_lookup_kind(
                                ret,
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
                    const T2LirOp *back_edge = nullptr;
                    bool any_tail_call = false;
                    for (const T2LirBlock &b : lir.blocks) {
                        for (const T2LirOp &op : b.ops) {
                            if (op.kind == T2LirKind::SideExit) {
                                side_exit = &op;
                            }
                            if (op.kind == T2LirKind::ReductionCheck) {
                                back_edge = &op;
                            }
                            if (op.kind == T2LirKind::TailCall) {
                                any_tail_call = true;
                            }
                        }
                    }
                    T2_STRUCT_CHECK(side_exit != nullptr &&
                                            side_exit->t1_pc_fail ==
                                                    (const void *)ci,
                                    "shared error exit != func_info "
                                    "ErtsCodeInfo");
                    T2_STRUCT_CHECK(back_edge != nullptr &&
                                            back_edge->target == own_entry &&
                                            back_edge->sync != nullptr,
                                    "recovered back edge missing or "
                                    "mis-targeted");
                    T2_STRUCT_CHECK(!any_tail_call,
                                    "a self tail call survived "
                                    "recovery");
                    T2_STRUCT_CHECK(!lir.blocks.empty() &&
                                            lir.blocks.back().phis.size() ==
                                                    2 &&
                                            lir.blocks.back().phis[0].home ==
                                                    PhysLoc::xreg(0) &&
                                            lir.blocks.back().phis[1].home ==
                                                    PhysLoc::xreg(1),
                                    "header phis must be homed X0/X1");

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
                    BeamT2ModuleAssembler ma(ga, lir.module, num_labels, lir);
                    StringLogger slog;

                    ma.set_string_logger(&slog);
                    /* A recovered blob is install-shaped: the
                     * back-edge demote requires the L_f contract. */
                    ma.set_install_entry(own_entry);
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

                    bool saw_cp = false, saw_side = false, saw_backedge = false,
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
                            T2_STRUCT_CHECK(false,
                                            "tail-target fact in a "
                                            "recovered function");
                            break;
                        case BeamT2ModuleAssembler::EmitFact::BackEdgeDemote:
                            saw_backedge = true;
                            T2_STRUCT_CHECK(f.value == (uint64_t)own_entry,
                                            "back-edge demote != own T1 "
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
                    T2_STRUCT_CHECK(saw_backedge,
                                    "no back-edge demote fact emitted");
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

#    undef T2_STRUCT_CHECK

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

    if (!erts_t2_emit_selftest_enabled() || ret == NULL || code_hdr == NULL) {
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

#else /* !defined(__aarch64__) */

/* ------------------------------------------------------------------ *
 * Non-aarch64 stubs. The T2 tier never emits or installs off aarch64  *
 * (see the note at the #if above); these exist only so the arch-      *
 * independent mid-end and the common JIT/BIF code link. One of them    *
 * (test_yield_return_offset) is defined by the arm per-arch source     *
 * (arm/instr_common.cpp) on aarch64 — here it gets its non-aarch64     *
 * definition. (profile_throwaway_addr is NOT stubbed here: t2_tier.c   *
 * defines it unconditionally on every arch.)                           *
 * ------------------------------------------------------------------ */

namespace erts_t2 {

    const void *t2_emit_blob(const T2LirFunction &fn,
                             std::string *err,
                             std::string *disasm,
                             T2EmitResult *out) {
        (void)fn;
        (void)disasm;
        (void)out;
        if (err != nullptr) {
            *err = "T2 emit is aarch64-only";
        }
        return nullptr;
    }

    bool t2_emit_blob_install(const T2LirFunction &fn,
                              const void *install_entry,
                              T2EmitResult *out,
                              std::string *err,
                              std::string *disasm) {
        (void)fn;
        (void)install_entry;
        (void)out;
        (void)disasm;
        if (err != nullptr) {
            *err = "T2 emit is aarch64-only";
        }
        return false;
    }

    ErtsT2ReentryFn t2_get_reentry_trampoline(void) {
        return nullptr;
    }

} /* namespace erts_t2 */

extern "C"
{
    Eterm erts_t2_debug_exec(Process *p,
                             Eterm mod,
                             Eterm func,
                             Eterm arity,
                             Eterm args) {
        (void)p;
        (void)mod;
        (void)func;
        (void)arity;
        (void)args;
        return am_undefined;
    }

    int erts_t2_emit_selftest_enabled(void) {
        return 0;
    }

    int erts_t2_emit_selftest(void) {
        return 0;
    }

    void erts_t2_emit_selftest_module(const struct ErtsT2RetainedCode *ret,
                                      const void *code_hdr) {
        (void)ret;
        (void)code_hdr;
    }

    Uint erts_t2_test_yield_return_offset(void) {
        return 0;
    }

} /* extern "C" */

#endif /* defined(__aarch64__) */
