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

#include <string>

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
    const void *t2_emit_blob(const T2LirFunction &fn,
                             std::string *err,
                             std::string *disasm);

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
