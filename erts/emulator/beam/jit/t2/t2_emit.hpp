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
 * T2-Full tier-2 JIT: LIR -> asmjit emission entry points
 * (PLAN/T2FULL/08 §1, §2).
 *
 * The emitter reuses the aarch64 BeamModuleAssembler's register defs,
 * mov_arg, emit_enter/leave_runtime, emit_gc_test, veneers, and the
 * `protected` per-op T1 emitters by *subclassing* it
 * (BeamT2ModuleAssembler, defined privately in t2_emit.cpp). For an
 * identity op it synthesizes the loader `ArgVal` family from the LIR
 * slot and calls the T1 emitter with `Fail` redirected to the op's T1
 * PC. A *fresh* assembler is instantiated per T2 blob, never the
 * loader's (so no per-module loader state leaks in).
 *
 * This header keeps the class itself out of the public surface (it drags
 * in the whole arm beam_asm.hpp); it exposes only C++-level result APIs
 * over t2_lir.hpp plus the selftest hooks.
 */

#ifndef _JIT_T2_EMIT_HPP
#define _JIT_T2_EMIT_HPP

#include <cstdint>
#include <string>
#include <vector>

#include "t2_lir.hpp"

namespace erts_t2 {

    /* Emit a standalone, self-contained executable blob for `fn` into the
     * global JitAllocator (a full mini-function: enter-frame prologue,
     * body, emit_return, plus the Fail->T1-PC trampolines). Returns the
     * blob's entry code pointer, or nullptr on failure with *err filled.
     * If `disasm` is non-null it receives the asmjit textual disassembly.
     *
     * P1 exec-harness note: unlike an installed blob (P1 install wave),
     * this test blob carries its own enter_erlang_frame prologue instead
     * of the entry stub, so it can be entered directly by the re-entry
     * trampoline (see erts_t2_exec, t2_emit.cpp) as if freshly called. */
    struct T2EmitResult;

    const void *t2_emit_blob(const T2LirFunction &fn,
                             std::string *err,
                             std::string *disasm,
                             T2EmitResult *out = nullptr);

    /* Result of an emission that must be owned/installed afterwards:
     * `entry` is the branch target (the entry stub for install-mode
     * blobs), [base, base+size) the JitAllocator span holding it (the
     * unit of release/tombstoning and of t2_ranges registration). */
    struct T2EmitResult {
        const void *entry = nullptr;
        const void *base = nullptr;
        size_t size = 0;

        /* The JitAllocator span's writable alias (== base on dual-map
         * platforms' rx view counterpart; on Apple the same VA). Needed
         * post-install to write the resume stubs' tombstone flags at
         * jettison (P2 commit 5). */
        void *rw_base = nullptr;

        /* Number of admitted+emitted fused byte scan-run regions
         * (P2.6 blocker B; PLAN/T2FULL/10). The install-quality gate's
         * "eliminated-work" bs signal: a fused scan-run is the only bs
         * shape that beats T1 (single guarded clause + catch-all over an
         * integer accumulator). An un-fused per-byte match loop -- a
         * multi-clause classifier whose recovered loop admit_scan_loop
         * rejects -- re-emits T1's bs ops 1:1 and is measured slower, so
         * it must NOT count as work eliminated. */
        unsigned scan_runs = 0;

        /* Blob-relative offsets of each recovered loop's back-edge
         * resume PC (the address a back-edge yield stores into c_p->i
         * via i_test_yield_shared), ascending, each with its own
         * jettison-translation target: the T1 PC a parked c_p->i at
         * this resume point is rewritten to. For a self-recursion back
         * edge that is the function's own post-yield entry body; for an
         * intrinsic (callee-demote) back edge it is the intrinsic call
         * site's T1 PC, which re-executes call_ext over the saved
         * fresh-call vector (P2 commit 8). Empty for loop-free
         * functions. Each resume PC sits TEST_YIELD_RETURN_OFFSET
         * bytes past its in-blob tombstone flag word. */
        struct ResumePoint {
            uint32_t offset;
            const void *t1_demote;
        };
        std::vector<ResumePoint> resume_points;
    };

    /* Emit an *installable* blob for `fn`: instead of the exec-harness
     * enter_erlang_frame prologue, the blob head is the T2 entry stub of
     * PLAN/T2/06 §2.3 — the patched `b` at L_f+4 lands here with the
     * frame already pushed by L_f+0 and FCALLS not yet decremented, so
     * the stub mirrors i_test_yield only: ARG3 = `install_entry` (the
     * function's public T1 entry L_f, the MFA-derivation contract), own
     * `subs FCALLS, #1`, and a b.le into i_test_yield_shared whose
     * computed resume PC (L_f + TEST_YIELD_RETURN_OFFSET) demotes an
     * entry-yielded invocation to T1. Returns false with *err filled on
     * failure; on success fills *out. */
    bool t2_emit_blob_install(const T2LirFunction &fn,
                              const void *install_entry,
                              T2EmitResult *out,
                              std::string *err,
                              std::string *disasm);

    /* The re-entry trampoline: a JIT-emitted stub, entered from C, that
     * loads the Erlang machine registers (E/HTOP/FCALLS/X regs) from the
     * process the way process_main's schedule-in does, jumps into a
     * standalone T2 blob, and returns the blob's result (XREG0) to C.
     * This is the P1 pre-install exec path (erts_debug t2_exec).
     *
     * Signature at the ABI level: (Process *c_p, const void *entry,
     * ErtsSchedulerRegisters *regs) -> Eterm. Types are void* here to keep
     * this header off the JIT-only include path. The blob's fail branches
     * are never taken for a non-raising input; a raising input would
     * side-exit into T1's raise path, which does not return to this
     * trampoline (see the note in t2_emit.cpp), so exec is used only for
     * non-raising validation before the install wave. Lazily built +
     * cached; returns nullptr if the JIT is not initialized. */
    typedef Eterm (*ErtsT2ReentryFn)(void *c_p,
                                     const void *entry,
                                     void *sched_regs);
    ErtsT2ReentryFn t2_get_reentry_trampoline(void);

} /* namespace erts_t2 */

/* ------------------------------------------------------------------ *
 * Debug BIF + selftest hooks (extern "C")                            *
 * ------------------------------------------------------------------ */

extern "C"
{
    /* True iff T2_EMIT_SELFTEST is set (read once). */
    int erts_t2_emit_selftest_enabled(void);

    /* Hand-builds a 1-op LIR (a Move + Return), emits it through a fresh
     * BeamT2ModuleAssembler, and disassembles it. Returns 0 on success,
     * non-zero on failure. Wired into beamasm_init() behind
     * T2_EMIT_SELFTEST. */
    int erts_t2_emit_selftest(void);

} /* extern "C" */

#endif /* _JIT_T2_EMIT_HPP */
