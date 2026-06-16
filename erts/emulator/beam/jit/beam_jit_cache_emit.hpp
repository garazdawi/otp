/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2026. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * ...
 *
 * %CopyrightEnd%
 */

#ifndef _BEAM_JIT_CACHE_EMIT_HPP
#define _BEAM_JIT_CACHE_EMIT_HPP

/*
 * Prototype sketch for IDEAS/07 #1 — emit-side helpers that the
 * JIT call sites use INSTEAD of bare `a.mov(reg, imm(p))`.
 *
 * The wrapper does exactly what the original `a.mov(reg, imm(p))`
 * does today — emits the same machine code — but ALSO records the
 * (code_offset, kind, symbolic_ref) triple on the per-module
 * relocation list. If caching is disabled at build time, the
 * recording is a no-op and the helper compiles down to the same
 * code as today's bare emit.
 *
 * Suggested integration: add these as protected member functions
 * of BeamModuleAssembler (in beam_asm.hpp). Then replace the
 * bare emits one site at a time, starting with the highest-payoff
 * ones (BIF calls and atom immediates dominate the boot path).
 *
 * Sites to convert (counted from a grep over instr_*.cpp):
 *
 *   imm(Bif.get())                  — BIF fn pointer        (≈ 30 sites)
 *   imm(BIF_TRAP_EXPORT(...))       — bif trap export       (≈ 10 sites)
 *   imm(&bif_mfa)                   — bif mfa struct        (≈ 20 sites)
 *   imm(&the_active_code_index)     — vm static             (≈ 5 sites)
 *   imm(make_atom(...))             — atom value            (≈ 40 sites)
 *   imm(literal_pool_ptr)           — literal pool member   (≈ 15 sites)
 *
 * That's ~120 call sites total to migrate; each is a single-line
 * change. The wrapper API is structured so the call site reads
 * almost identically (`a.mov(...)` → `emit_mov_*(...)`), so the
 * mechanical change is low-risk and reviewable.
 */

#include <cstdint>
#include "beam_jit_cache.h"

namespace asmjit { namespace x86 {
    class Gp;
}}

/*
 * Wrapper helpers — see comment block above.
 *
 * Each helper takes the same `(dst, value)` shape as the
 * existing `a.mov(dst, imm(value))` call, plus the symbolic
 * reference needed for cache resolution.
 *
 * The class methods live on BeamModuleAssembler so they have
 * access to the asmjit emitter `a`, the relocation list, and the
 * per-module string tables.
 */
class BeamJitCacheEmitterMixin {
public:
    /*
     * Emit `mov dst, imm(atom_value)` and record an ATOM relocation.
     *
     * atom_str_idx is an index into the per-module atom string
     * table — typically allocated lazily the first time a given
     * atom string is referenced during compilation.
     */
    void emit_mov_atom(asmjit::x86::Gp dst,
                       uint64_t atom_value,
                       uint32_t atom_str_idx);

    /*
     * Emit `mov dst, imm(bif_fn_ptr)` and record a BIF relocation.
     *
     * mfa_str_idx → "erlang:send/2" etc. in the string table.
     */
    void emit_mov_bif(asmjit::x86::Gp dst,
                      void *bif_fn_ptr,
                      uint32_t mfa_str_idx);

    /*
     * Emit `mov dst, imm(export_entry_ptr)` and record an EXPORT
     * relocation.
     */
    void emit_mov_export(asmjit::x86::Gp dst,
                         void *export_entry_ptr,
                         uint32_t mfa_str_idx);

    /*
     * Emit `mov dst, imm(literal_ptr)` and record a LITERAL
     * relocation.
     *
     * literal_offset is the offset within the module's literal pool
     * blob (the same blob that will be serialised to the cache file).
     */
    void emit_mov_literal(asmjit::x86::Gp dst,
                          void *literal_ptr,
                          uint32_t literal_offset);

    /*
     * Emit `mov dst, imm(static_ptr)` and record a VM_STATIC
     * relocation.
     *
     * `which` is one of the BEAM_JIT_VM_STATIC_* enum values;
     * the loader maps each to the address of the corresponding
     * runtime static in the live VM.
     */
    void emit_mov_vm_static(asmjit::x86::Gp dst,
                            void *static_ptr,
                            BeamJitVmStatic which);

    /*
     * Emit `mov dst, imm(&bif_mfa[entry])` and record a BIF_MFA
     * relocation. Specialised because the bif_mfa table layout is
     * deterministic — the loader doesn't need the address, just the
     * MFA string and offset within the table.
     */
    void emit_mov_bif_mfa(asmjit::x86::Gp dst,
                          void *bif_mfa_ptr,
                          uint32_t mfa_str_idx);

protected:
    /*
     * Allocator helpers that look up (or insert) a string in the
     * per-module string table. These are called by the wrappers
     * above. Real implementation lives in beam_asm_module.cpp.
     */
    uint32_t intern_atom_string(uint64_t atom_value);
    uint32_t intern_mfa_string(void *export_or_bif_ptr);

    /*
     * Append a single record to the per-module relocation list.
     * The code_offset comes from `a.offset()` immediately AFTER the
     * mov is emitted, minus the immediate's width.
     */
    void record_reloc(uint32_t code_offset,
                      BeamJitRelocKind kind,
                      uint16_t imm_width,
                      uint32_t symbolic_ref);

    /* The per-module list. Empty if caching is disabled. */
    BeamJitRelocList reloc_list_;
};

/*
 * Inline-able implementations. For brevity these are written as
 * comments showing the pattern; the real versions go in
 * beam_asm_module.cpp (per-arch where needed).
 *
 *   void emit_mov_atom(Gp dst, uint64_t atom_value, uint32_t str_idx) {
 *       size_t off_before = a.offset();
 *       a.mov(dst, imm(atom_value));
 *       size_t imm_off = a.offset() - 8;     // 64-bit immediate
 *       record_reloc(imm_off, BEAM_JIT_RELOC_ATOM, 8, str_idx);
 *   }
 *
 * The other wrappers follow exactly the same shape with their
 * corresponding kind.
 *
 * IMPORTANT: the wrappers must NOT change the asmjit instruction
 * selection — that would alter the generated code's correctness
 * guarantees vs the baseline today. They just observe the offset
 * after emit. asmjit guarantees that `a.offset()` reflects the
 * end of the most recently emitted instruction.
 *
 * For x86_64 a `mov reg, imm64` is a 10-byte instruction
 * (REX + opcode + 8-byte imm). The 8-byte imm starts at
 * (off_after - 8) and that's what we record. For aarch64 the
 * sequence is MOVZ + MOVK*3 covering a 64-bit constant; the
 * reloc-aware version emits four immediates and records a
 * 64-bit-piecewise reloc that the loader patches at all four
 * MOV[Z|K] offsets. See `beam_jit_cache.h` for the kind enum
 * which intentionally does not encode arch-specific layout —
 * imm_width and the arch ID in the cache header carry that.
 */

#endif /* _BEAM_JIT_CACHE_EMIT_HPP */
