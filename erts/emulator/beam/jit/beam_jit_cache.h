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

#ifndef _BEAM_JIT_CACHE_H
#define _BEAM_JIT_CACHE_H

/*
 * Prototype sketch for IDEAS/07 #1 — persistent on-disk JIT cache.
 *
 * The JIT today bakes runtime pointers directly into the generated
 * machine code: pointers to BIF functions, export entries, atom
 * values, and per-runtime globals such as &the_active_code_index or
 * &bif_mfa. Those pointers are valid only for the VM instance that
 * produced the code, so a cached blob loaded into a different VM
 * needs them re-patched.
 *
 * This header defines:
 *
 *   - Relocation kinds (what kind of address to patch)
 *   - A per-module relocation list that the emitter appends to as
 *     each affected `mov reg, imm(...)` is emitted
 *   - Wrapper helpers that the JIT call sites use INSTEAD of bare
 *     `a.mov(reg, imm(p))`, so the relocation is recorded as a
 *     side effect of emission
 *
 * Nothing here changes the generated machine code today; the
 * relocation list is collected and discarded. The point is to make
 * the "what does this immediate point at" question answerable at
 * cache-write time, which is the prerequisite for everything else
 * in #1a.
 *
 * Once the data is collected, two consumers are easy to add:
 *
 *   (a) Cache writer: serialise (code_offset, kind, symbolic_ref)
 *       per entry.
 *   (b) Cache loader: walk the deserialised list, look up the
 *       symbolic_ref against the *current* runtime, write the
 *       resolved pointer into the loaded code at code_offset.
 *
 * Both consumers live in a separate translation unit; this header
 * is the shared schema.
 */

#include "sys.h"

#ifdef __cplusplus
extern "C" {
#endif

/* What kind of runtime address an immediate slot wants resolved. */
typedef enum {
    /* Atom value. The symbolic_ref is an index into the per-module
     * atom string table; resolved by erts_atom_put + make_atom. */
    BEAM_JIT_RELOC_ATOM = 1,

    /* BIF function pointer (e.g. &erts_send). The symbolic_ref is
     * an index into the per-module MFA string table; resolved via
     * the bif table at load. */
    BEAM_JIT_RELOC_BIF,

    /* Pointer to an Export entry for a cross-module call or BIF
     * trap (e.g. &bif_trap_export[...]). Resolved via the export
     * table lookup at load. */
    BEAM_JIT_RELOC_EXPORT,

    /* Pointer to a per-VM static (the_active_code_index, bif_mfa,
     * etc.). The symbolic_ref is a small enum value identifying
     * which static; resolved by a per-VM table at load time. */
    BEAM_JIT_RELOC_VM_STATIC,

    /* Pointer to a literal in this module's literal pool. The
     * symbolic_ref is an offset within the cache file's literal
     * blob; resolved to the live literal-area address at load. */
    BEAM_JIT_RELOC_LITERAL,

    /* Pointer to a BIF MFA struct (the {module, function, arity}
     * tuple used for stack-trace annotation on error). The
     * symbolic_ref is an MFA string-table index. */
    BEAM_JIT_RELOC_BIF_MFA,

    BEAM_JIT_RELOC_KIND_COUNT
} BeamJitRelocKind;

/* One immediate slot in the emitted code that needs runtime
 * resolution. The actual MOV-imm at `code_offset` is currently
 * emitted with the resolved pointer baked in; this record lets the
 * cache writer / loader re-do the resolution. */
typedef struct {
    uint32_t  code_offset;     /* byte offset within the function's
                                * emitted code where the immediate
                                * starts */
    uint16_t  kind;            /* BeamJitRelocKind */
    uint16_t  imm_width;       /* 4 or 8 bytes — affects patcher */
    uint32_t  symbolic_ref;    /* interpretation depends on kind */
} BeamJitReloc;

/* Per-module accumulator. Lives on the BeamModuleAssembler for the
 * duration of compilation; its contents end up in the cache file
 * (or are discarded if caching is disabled). */
typedef struct {
    BeamJitReloc *entries;
    size_t        count;
    size_t        capacity;
} BeamJitRelocList;

/* Selected VM-static targets for BEAM_JIT_RELOC_VM_STATIC. Stable
 * enum values so the cache file format doesn't drift if internals
 * are renamed. */
typedef enum {
    BEAM_JIT_VM_STATIC_ACTIVE_CODE_INDEX = 1,
    BEAM_JIT_VM_STATIC_BIF_MFA           = 2,
    BEAM_JIT_VM_STATIC_DBG_FLAGS         = 3,
    /* … */
} BeamJitVmStatic;

void beam_jit_reloc_list_init(BeamJitRelocList *list);
void beam_jit_reloc_list_free(BeamJitRelocList *list);
void beam_jit_reloc_append(BeamJitRelocList *list,
                           uint32_t code_offset,
                           BeamJitRelocKind kind,
                           uint16_t imm_width,
                           uint32_t symbolic_ref);

#ifdef __cplusplus
}
#endif

#endif /* _BEAM_JIT_CACHE_H */
