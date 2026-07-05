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
}

#include "t2_lir.hpp"
#include "t2_emit.hpp"
#include "t2_hir.hpp"
#include "t2_isel.hpp"

#include <cstdio>
#include <functional>
#include <unordered_map>

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

        std::string emit_error;

    public:
        BeamT2ModuleAssembler(BeamGlobalAssembler *ga,
                              Eterm mod,
                              int num_labels,
                              const T2LirFunction &fn_)
                : BeamModuleAssembler(ga, mod, num_labels, t2_synth_beamfile()),
                  fn(fn_),
                  next_label((unsigned)fn_.blocks.size() + 2) {
            entry_label = a.new_label();
        }

        void set_string_logger(StringLogger *slog) {
            code.set_logger(slog);
        }

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

            /* Mirror a normal callee: push the return address onto the
             * Erlang stack. The standalone test blob supplies its own
             * prologue (the installed blob uses the entry stub instead --
             * P1 install wave). */
            emit_enter_erlang_frame();

            for (const T2LirBlock &b : fn.blocks) {
                a.bind(block_label(b.id));
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

            return (const void *)getCode(entry_label);
        }

    private:
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
                a.bind(rawLabels.at(pair.second));
                /* Absolute branch to the T1 PC. mov_imm materializes the
                 * 64-bit address; br transfers control. T1 re-executes and
                 * (for error paths) raises a byte-identical exception. */
                mov_imm(TMP1, (Uint64)pair.first);
                a.br(TMP1);
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
            case T2LirKind::Add:
            case T2LirKind::Sub:
                emit_lir_arith2(op);
                break;
            case T2LirKind::Return:
                emit_return();
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

        /* The Fail label of an arith op resolves to a trampoline branching
         * to the op's T1 PC (side-exit; T2 never raises). Falls back to the
         * function's T1 entry when the op carries no specific PC. */
        ArgVal arith_fail_argval(const T2LirOp &op) {
            const void *pc = op.t1_pc_fail ? op.t1_pc_fail : fn.t1_entry;
            if (pc == nullptr) {
                fail("arith op without a T1 fail target");
                return ArgVal(ArgVal::Type::Label, 1);
            }
            return ArgVal(ArgVal::Type::Label, fail_label_num(pc));
        }

        void emit_lir_arith2(const T2LirOp &op) {
            if (op.num_srcs != 2 || op.dst.is_none()) {
                fail("malformed binary arithmetic op");
                return;
            }
            ArgLabel failL(arith_fail_argval(op));
            if (failed()) {
                return;
            }
            ArgWord live((UWord)op.arity);
            ArgSource lhs(src_argval(op.srcs[0]));
            ArgSource rhs(src_argval(op.srcs[1]));
            ArgRegister dst(loc_argval(op.dst));

            switch (op.kind) {
            case T2LirKind::Add:
                emit_i_plus(failL, live, lhs, rhs, dst);
                break;
            case T2LirKind::Sub:
                emit_i_minus(failL, live, lhs, rhs, dst);
                break;
            default:
                fail("unhandled binary arithmetic op");
                break;
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
                             std::string *disasm) {
        BeamGlobalAssembler *ga = erts_t2_global_assembler();
        if (ga == nullptr || erts_t2_jit_allocator() == nullptr) {
            if (err) {
                *err = "T2 JIT not initialized";
            }
            return nullptr;
        }

        int num_labels = (int)fn.blocks.size() + 2;

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
        return entry;
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

        BeamT2ModuleAssembler ma(ga, empty.module, 2, empty);
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
    if (modp == NULL || modp->curr.t2_retained == NULL) {
        return am_undefined;
    }
    const ErtsT2RetainedCode *ret = modp->curr.t2_retained;

    /* T1 entry of the function, the fail-branch (side-exit) target. */
    const Export *e =
            erts_find_function(mod, func, (unsigned)ar, erts_active_code_ix());
    const void *t1_entry =
            e ? (const void *)e->dispatch.addresses[erts_active_code_ix()]
              : NULL;

    /* Build HIR -> isel -> regalloc -> emit, all while the module decode
     * (and any literals the HIR references) is still alive. */
    const void *blob = NULL;
    std::string build_err;
    std::string pipe_err;

    T2BuildStatus status = t2_build_for_debug(
            ret,
            func,
            (unsigned)ar,
            [&](const T2Function &hir) {
                T2LirFunction lir;
                if (!t2_isel(hir, lir, &pipe_err)) {
                    return;
                }
                lir.t1_entry = t1_entry;
                if (!t2_regalloc(lir, &pipe_err)) {
                    return;
                }
                blob = t2_emit_blob(lir, &pipe_err, nullptr);
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
