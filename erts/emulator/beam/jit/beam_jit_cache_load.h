/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2026. All Rights Reserved.
 *
 * %CopyrightEnd%
 */

#ifndef _BEAM_JIT_CACHE_LOAD_H
#define _BEAM_JIT_CACHE_LOAD_H

#include <stdint.h>
#include <stddef.h>

#include "beam_jit_cache.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Opaque handle. */
typedef struct BeamJitCache_ BeamJitCache;

/* Per-module view returned by find_module — pointers into the
 * underlying mmap, valid for the cache's lifetime. */
typedef struct {
    const char         *name;
    uint8_t             beam_sha256[32];
    const uint8_t      *code;
    size_t              code_size;
    const BeamJitReloc *relocs;
    size_t              reloc_count;
    /* atom_indices[i] = strtab_index of the i-th atom referenced by
     * this module. Reloc.symbolic_ref indexes into this array. */
    const uint8_t      *atom_indices;
    size_t              atom_count;
    const uint8_t      *mfa_indices;
    size_t              mfa_count;
    int                 has_on_load;
} BeamJitCacheModule;

/* Host hooks — supplied by either the runtime (live VM tables) or by
 * the cache_tool (its own stubbed tables, for round-trip-test). The
 * ctx pointer is opaque and passed through to each callback. */
typedef struct {
    void *ctx;
    /* For RELOC_ATOM: name + length → live atom Eterm. */
    uintptr_t (*atom_eterm_for_name)(void *ctx, const char *name, size_t n);
    /* For RELOC_EXPORT MFA: "M:F/A" string → live Export* pointer. */
    void *(*export_ptr_for_mfa)(void *ctx, const char *mfa);
    /* For RELOC_BIF MFA: "M:F/A" string → live BIF function pointer. */
    void *(*bif_fn_for_mfa)(void *ctx, const char *mfa);
    /* For RELOC_BIF "<bif:N>": bif_table index → live function pointer. */
    void *(*bif_fn_for_index)(void *ctx, uint32_t bif_index);
    /* For RELOC_VM_STATIC: stable enum id → live runtime symbol addr. */
    void *(*vm_static_for_id)(void *ctx, uint32_t which);
    /* For RELOC_FRAGMENT_BRANCH "<frag:NAME>": live address of that
     * BeamGlobalAssembler fragment in the current process. */
    void *(*fragment_addr_for_name)(void *ctx, const char *name);
    /* For RELOC_LITERAL: per-module literal index → live Eterm of
     * the literal in the current process. For the runtime, this
     * resolves via the cache's serialised literal blob; for the
     * validator, by re-reading the BEAM file and looking up index
     * in beam->static_literals. */
    uintptr_t (*literal_eterm_for_index)(void *ctx, uint32_t index);
} BeamJitCacheHostHooks;

BeamJitCache *beam_jit_cache_open(const char *path);
void          beam_jit_cache_close(BeamJitCache *c);

/* Find one module by name, copy its code into a fresh buffer, and
 * walk the reloc list applying patches. Returns 0 on success and
 * populates *out_code / *out_size; caller frees *out_code. */
int beam_jit_cache_load_module(BeamJitCache *c,
                               const char *module_name,
                               const BeamJitCacheHostHooks *hooks,
                               void **out_code,
                               size_t *out_size);

#ifdef __cplusplus
}
#endif

#endif /* _BEAM_JIT_CACHE_LOAD_H */
