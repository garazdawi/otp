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
}

#include "t2_lir.hpp"
#include "t2_emit.hpp"

#include <cstdio>
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

} /* namespace erts_t2 */

/* ------------------------------------------------------------------ *
 * Selftest (T2_EMIT_SELFTEST)                                        *
 * ------------------------------------------------------------------ */

using namespace erts_t2;

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
