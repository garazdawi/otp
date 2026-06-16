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
 * Runtime-hook stubs for the build-time beam_jit_compile tool.
 *
 * These replace the parts of the live emulator that the loader and
 * asmjit emitters call into. The strategy is:
 *
 *   - "host the compile" stubs: trivial implementations sufficient
 *     to run the loader + asmjit emit (atom_put, alloc, etc.)
 *   - "record symbolic ref" stubs: invoked by the emit_mov_* path
 *     in beam_jit_cache_emit.hpp. Each records (offset, kind,
 *     symbolic_ref) on a per-module list that the cache writer
 *     serialises.
 *
 * NOT FUNCTIONAL — file shows the intended pattern, with the most
 * impactful hooks sketched out. Building this for real means walking
 * through the loader / emitter source until every external symbol
 * either has a stub here or is satisfied by linking in the unmodified
 * source file from beam/ or jit/.
 *
 * The "guide" methodology: link without these stubs, the linker
 * complains about missing symbols, decide for each whether it's
 * (a) trivial enough to copy in from the runtime, (b) needs a
 * recording stub, or (c) shouldn't be reachable on the compile path
 * (stub with abort()).
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cache_tool.h"

/* ----------------------------------------------------------------
 * Memory: malloc-backed replacements for the live VM's allocators.
 * The compile path doesn't need the runtime's specialised allocator
 * categories or pool semantics — only "give me N bytes that live
 * long enough".
 * ---------------------------------------------------------------- */

void *erts_alloc(int type, size_t sz)            { (void)type; return malloc(sz); }
void *erts_realloc(int type, void *p, size_t sz) { (void)type; return realloc(p, sz); }
void  erts_free(int type, void *p)               { (void)type; free(p); }

/* ----------------------------------------------------------------
 * Atom table: sequential indexer + string table. The emit_mov_atom
 * wrappers from beam_jit_cache_emit.hpp call intern_atom_string()
 * to look up (or insert) an atom by string; the returned index is
 * what gets recorded in the reloc list.
 *
 * The runtime cache loader does the inverse: for each interned
 * string in the cache file, it calls erts_atom_put() against the
 * live atom table and builds an atom_value[] mapping.
 * ---------------------------------------------------------------- */

#define MAX_ATOMS 65536
static const char *atom_strings[MAX_ATOMS];
static size_t      atom_count;

uint32_t cache_tool_intern_atom_string(const char *str) {
    /* Linear scan for now; build-time tool, doesn't need to be
     * fast. Replace with hash table when atom counts exceed ~1k. */
    for (size_t i = 0; i < atom_count; i++) {
        if (strcmp(atom_strings[i], str) == 0) return (uint32_t)i;
    }
    if (atom_count >= MAX_ATOMS) {
        fprintf(stderr, "cache_tool: atom table overflow\n");
        abort();
    }
    atom_strings[atom_count] = strdup(str);
    return (uint32_t)atom_count++;
}

/* erts_atom_put is what the loader calls during BEAM atom-chunk
 * parsing. We intercept and route through our indexer; the value
 * we return is opaque to the loader (it just stores it in the
 * atom_value[] for the module). */
uint32_t erts_atom_put(const uint8_t *name, int len,
                       int enc, int trunc) {
    (void)enc; (void)trunc;
    char buf[256];
    int n = len < (int)sizeof(buf)-1 ? len : (int)sizeof(buf)-1;
    memcpy(buf, name, n); buf[n] = 0;
    return cache_tool_intern_atom_string(buf);
}

/* ----------------------------------------------------------------
 * BIF / export resolution: recording stubs.
 *
 * In the live VM, looking up "erlang:send/2" in bif_table[] returns
 * a function pointer; that pointer gets baked into emitted code via
 * `a.mov(RET, imm(Bif.get()))`. Here we intercept the lookup,
 * record the symbolic MFA, and return a sentinel pointer that the
 * emit path stores but doesn't dereference.
 *
 * The emit wrapper (emit_mov_bif from beam_jit_cache_emit.hpp)
 * separately records the offset of the resulting `mov` so the
 * cache file can later patch the right slot.
 * ---------------------------------------------------------------- */

uint32_t cache_tool_intern_mfa_string(const char *mfa) {
    /* Reuses the atom interner for now; could be separate. */
    return cache_tool_intern_atom_string(mfa);
}

void *cache_tool_record_bif_ref(const char *mfa) {
    /* The sentinel value is opaque — the runtime cache loader
     * will overwrite it via the bif_relocs list. */
    return (void *)(uintptr_t)(0xBE5F0000 + cache_tool_intern_mfa_string(mfa));
}

/* ----------------------------------------------------------------
 * Process * and scheduler state: unreachable on the compile path.
 *
 * Some emit paths take a Process* argument but never dereference
 * it during emission (only during execution of the emitted code).
 * The stubs return NULL and rely on the compile path not touching
 * the value. If a future code change starts dereferencing, the
 * abort() catches it in dev/CI before drift ships.
 * ---------------------------------------------------------------- */

void *erts_get_current_process(void) {
    /* The compile path should never need the current process.
     * If we hit this it means a runtime-only code path leaked
     * into the build-time tool. Likely a stub-drift bug. */
    fprintf(stderr,
            "cache_tool: unexpected erts_get_current_process() call\n");
    abort();
}

/* ----------------------------------------------------------------
 * Executable memory: regular mmap.
 *
 * The runtime has its own executable-memory allocator (with sealing
 * for W^X). The tool just uses PROT_READ|PROT_WRITE — we never
 * EXECUTE the emitted code in the tool, we just dump it to disk.
 * ---------------------------------------------------------------- */

#include <sys/mman.h>

void *cache_tool_alloc_exec(size_t sz) {
    void *p = mmap(NULL, sz, PROT_READ|PROT_WRITE,
                   MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
    return p == MAP_FAILED ? NULL : p;
}

void cache_tool_free_exec(void *p, size_t sz) { munmap(p, sz); }

/* ----------------------------------------------------------------
 * Global fragments (BeamGlobalAssembler).
 *
 * The runtime calls per-module emit code, which in turn calls
 * fragments defined in beam_asm_global.cpp (e.g.
 * get_call_light_bif_shared). These fragments are themselves
 * emitted at runtime init and live at specific runtime addresses.
 *
 * For the tool, we emit them too (the same source file is linked
 * in) but record their *names* in the cache. The runtime cache
 * loader patches each per-module reference to point at the live
 * fragment address.
 * ---------------------------------------------------------------- */

const char *cache_tool_fragment_name(void *frag_ptr) {
    /* Reverse-lookup the BeamGlobalAssembler symbol that this
     * pointer corresponds to. In a real implementation this is
     * a small table built at startup. */
    (void)frag_ptr;
    return "TODO";
}

/* ----------------------------------------------------------------
 * Tool lifecycle.
 * ---------------------------------------------------------------- */

CacheArch arch_from_string(const char *s) {
    if (!s) return CACHE_ARCH_UNKNOWN;
    if (strcmp(s, "x86_64")  == 0) return CACHE_ARCH_X86_64;
    if (strcmp(s, "aarch64") == 0) return CACHE_ARCH_AARCH64;
    return CACHE_ARCH_UNKNOWN;
}

int cache_tool_init(const char *target_arch) {
    /* Initialise asmjit's CodeHolder template for the target arch.
     * Initialise the loader's static state (atom 0 reserved, etc.).
     * Build the BeamGlobalAssembler fragments for this arch so
     * per-module emit can reference them. */
    (void)target_arch;
    return 0;
}

void cache_tool_shutdown(void) {
    for (size_t i = 0; i < atom_count; i++) free((void *)atom_strings[i]);
    atom_count = 0;
}

/* ----------------------------------------------------------------
 * Stubs to add as the integration surfaces:
 *
 * - erts_alloc_permanent_cache_aligned()
 * - erts_send_warning_to_logger_str_nogl()  (loader warns about
 *   obsolete instructions — route to fprintf(stderr))
 * - The handful of process_main entry points referenced by
 *   beam_asm_module's fragment generation
 * - Coverage instrumentation hooks (compile path should never
 *   trigger these; abort stubs)
 * - Trace instrumentation hooks (same)
 * - Hibernate / GC entry points (same — only emitted code calls)
 *
 * Following the "let the linker guide us" methodology: keep
 * linking, add stubs for each missing symbol, decide per stub
 * whether it's recordable, trivial, or unreachable.
 * ---------------------------------------------------------------- */
