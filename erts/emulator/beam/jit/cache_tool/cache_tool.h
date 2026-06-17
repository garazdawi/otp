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

#ifndef _CACHE_TOOL_H
#define _CACHE_TOOL_H

#include <stddef.h>
#include <stdint.h>

/* Shared with the runtime cache loader. Defines the reloc kinds
 * (atom, BIF, export, vm_static, literal, bif_mfa) and the
 * BeamJitReloc record format that ends up in the cache file. */
#include "../beam_jit_cache.h"

/* ---- Host arch & defaults ------------------------------------- */

#if defined(__x86_64__) || defined(_M_X64)
#  define HOST_ARCH_STRING "x86_64"
#elif defined(__aarch64__) || defined(_M_ARM64)
#  define HOST_ARCH_STRING "aarch64"
#else
#  define HOST_ARCH_STRING "unsupported"
#endif

/* Compile-time-resolved defaults (Makefile injects via -D). */
#ifndef ERTS_VSN_DEFAULT
#  define ERTS_VSN_DEFAULT "unknown"
#endif
#ifndef JIT_VSN_DEFAULT
#  define JIT_VSN_DEFAULT  "unknown"
#endif
#ifndef CFLAGS_HASH_DEFAULT
#  define CFLAGS_HASH_DEFAULT "00000000000000000000000000000000"
#endif

typedef enum {
    CACHE_ARCH_UNKNOWN = 0,
    CACHE_ARCH_X86_64  = 1,
    CACHE_ARCH_AARCH64 = 2,
} CacheArch;

CacheArch arch_from_string(const char *s);

/* ---- BEAM input ----------------------------------------------- */

typedef struct {
    const char *path;
    uint8_t    *data;    /* mmap'd or malloc'd .beam bytes */
    size_t      size;
    uint8_t     sha256[32];
    /* Parsed pieces filled in by cache_tool_read_beam: atom chunk,
     * code chunk, imports, exports, literals, etc. Same shape as
     * the live emulator's BeamFile structure (reused via the
     * common header so the loader's read path works unmodified). */
    void       *beamfile_state;
} BeamInput;

int  cache_tool_read_beam(const char *path, BeamInput *out);
void cache_tool_free_input(BeamInput *in);

/* ---- Compiled module (loader + asmjit output) ----------------- */

typedef struct {
    /* The module's identity. */
    const char *module_name;
    uint8_t     beam_sha256[32];

    /* Raw machine code bytes produced by asmjit. */
    uint8_t    *code;
    size_t      code_size;

    /* Relocation lists. Populated by the emit_mov_* wrappers
     * during cache_tool_compile_module. The runtime's cache
     * loader walks the same lists in reverse. */
    BeamJitReloc *relocs;
    size_t        reloc_count;

    /* Atom string table: atoms referenced by this module, by
     * their per-module index. The cache writer serialises the
     * strings; the runtime cache loader interns them in the
     * live atom table on load. */
    const char  **atom_strings;
    size_t        atom_count;

    /* MFA string table: BIF and cross-module export references.
     * Same per-module index scheme as atoms. */
    const char  **mfa_strings;
    size_t        mfa_count;

    /* Literal pool: pre-built heap-format Eterms. The runtime
     * copies this to the literal area on load, then patches the
     * atom and pointer relocs to point at the live VM's atoms
     * and to the new pool's base. */
    uint8_t      *literal_blob;
    size_t        literal_size;
    BeamJitReloc *literal_relocs;   /* atom + ptr relocs in the blob */
    size_t        literal_reloc_count;

    /* Func / export / import tables — offsets into the code blob,
     * plus string-table indices for symbolic identity. */
    struct {
        uint32_t name_str_idx;  /* bundle strtab index, set at write */
        uint32_t arity;
        uint32_t code_offset;
        char    *name;          /* owned C string. Reader sets this from
                                 * exports/locals scan; writer interns
                                 * into bundle strtab. NULL when the
                                 * label isn't in either table. */
    } *funcs;
    size_t func_count;

    /* on_load chunk presence (the actual handler runs at load
     * time as today; cache just records "this module has one"). */
    int has_on_load;

    /* Opaque magic-binary handle for the loader state. Kept alive by
     * the compile path so the validator can resolve per-module data
     * (literal Eterms, fragment names) against the same loader run
     * that produced the code blob. Owned by the CompiledModule. */
    void *loader_magic;
} CompiledModule;

int  cache_tool_compile_module(const BeamInput *in, CompiledModule *out);
void cache_tool_free_compiled(CompiledModule *cm);

/* ---- Cache file writer --------------------------------------- */

typedef struct {
    const char *erts_vsn;
    const char *jit_vsn;
    const char *cflags_hash;
    CacheArch   arch;
    int         bundle;
} CacheHeader;

typedef struct CacheWriter CacheWriter;

CacheWriter *cache_writer_open(const char *path, const CacheHeader *hdr);
int          cache_writer_emit_module(CacheWriter *w, const CompiledModule *cm);
void         cache_writer_close(CacheWriter *w);

/* ---- Tool lifecycle ------------------------------------------ */

int  cache_tool_init(const char *target_arch);
void cache_tool_shutdown(void);

/* Round-trip validator: load a .jc file written by an earlier run,
 * walk the reloc list applying patches, report counts.  Used to
 * verify the writer / loader pair without the runtime emulator in
 * the loop. */
int  cache_tool_validate(const char *jc_path, const char *module_name,
                         int verbose);

/* Cross-process determinism regression guard: invoke self twice on
 * the same .beam, compare resulting .jc files byte-for-byte. Returns
 * 0 on PASS (identical), non-zero on FAIL (with diagnostics on
 * stderr). */
int  cache_tool_validate_deterministic(const char *self_exe,
                                       const char *arch,
                                       const char *beam_path,
                                       int verbose);

#endif /* _CACHE_TOOL_H */
