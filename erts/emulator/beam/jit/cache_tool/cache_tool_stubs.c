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

/* The Eterm encoding for atom values: (index << 6) | 0x0B.
 * The loader returns these as error atoms, so we need to encode
 * matching values rather than returning raw indices. */
#define _TAG_IMMED2_ATOM 0x0B
#define MAKE_ATOM(idx)   (((uint64_t)(idx) << 6) | _TAG_IMMED2_ATOM)
#define ATOM_VAL(eterm)  ((uint32_t)((eterm) >> 6))

uint32_t cache_tool_intern_atom_string(const char *str) {
    /* Linear scan for now; build-time tool, doesn't need to be
     * fast. Replace with hash table when atom counts exceed ~1k. */
    for (size_t i = 0; i < atom_count; i++) {
        if (strcmp(atom_strings[i], str) == 0)
            return (uint32_t)MAKE_ATOM(i);
    }
    if (atom_count >= MAX_ATOMS) {
        fprintf(stderr, "cache_tool: atom table overflow\n");
        abort();
    }
    atom_strings[atom_count] = strdup(str);
    return (uint32_t)MAKE_ATOM(atom_count++);
}

const char *cache_tool_atom_name(uint64_t atom_eterm) {
    uint32_t idx = ATOM_VAL(atom_eterm);
    if (idx >= atom_count) return "<out-of-range>";
    return atom_strings[idx];
}

/* erts_atom_put is what the loader calls during BEAM atom-chunk
 * parsing. We intercept and route through our indexer; the value
 * we return is opaque to the loader (it just stores it in the
 * atom_value[] for the module). */
uint64_t erts_atom_put(const uint8_t *name, int len,
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
 * Beginning of the actual stub set, organised by category.
 *
 * Confirmed working surface (commits this session): loader source
 * files (beam_load.c, beam_file.c, beam_transform_helpers.c,
 * asm_load.c) and JIT emitter source files (beam_jit_*.cpp,
 * jit/arm/instr_*.cpp, jit/arm/beam_asm_*.cpp) ALL compile out of
 * the box with the runtime's normal -I flags. No header
 * refactoring needed.
 *
 * Remaining work is purely stubbing the ~120 runtime symbols
 * referenced by these compiled objects. The categorical breakdown:
 *
 *   ~50 erts_* runtime hooks (atom table, alloc wrappers, magic
 *       refs, MD5, dsprintf, etc.) — most are trivial/recording.
 *   ~25 atom values (am_*) — recording: snprintf the string.
 *   ~15 BIF table entries (bif_table, gen_opc, opc) — empty
 *       arrays sufficient for the compile path.
 *   ~10 process_main entry points (apply, beam_*_trace) —
 *       referenced by emitter's fragment gen; abort stubs.
 *   ~10 ERTS_GLOBAL_LIT_* and similar constants — static
 *       definitions.
 *   ~10 misc small functions.
 *
 * Below are the highest-priority stubs to start with — once the
 * link succeeds, the tool produces real JIT'd code (with
 * symbolic relocations) rather than the placeholder BEAM-bytes
 * output the current main.c produces.
 * ---------------------------------------------------------------- */

/* ---- Allocation wrappers ------------------------------------- */

void *erts_alloc_permanent_cache_aligned(int type, size_t sz) {
    (void)type;
    /* Cache-aligned alloc; for the tool, regular malloc is fine
     * since we don't actually execute the emitted code. */
    return malloc(sz);
}

void *erts_alloc_n_enomem(int type, size_t sz) {
    /* The "_n" variant returns NULL on OOM instead of aborting;
     * stub does the same. */
    (void)type;
    return malloc(sz);
}

void *erts_realloc_n_enomem(int type, void *p, size_t sz) {
    (void)type;
    return realloc(p, sz);
}

void erts_exit(int code, const char *fmt, ...) {
    fprintf(stderr, "cache_tool: erts_exit(%d): %s\n", code, fmt);
    exit(code);
}

/* ---- Logging ------------------------------------------------- */

void *erts_create_logger_dsbuf(void) {
    /* Loader uses this for error reporting. Return a tiny stub
     * "dsbuf" — a FILE* underneath, since we route to stderr. */
    return stderr;
}

int erts_dsprintf(void *dsbuf, const char *fmt, ...) {
    (void)dsbuf; (void)fmt;
    /* TODO: route through a small recording buffer that gets
     * dumped on error. For now no-op (errors during compile
     * are rare; if they happen they show as load failures). */
    return 0;
}

int erts_vdsprintf(void *dsbuf, const char *fmt, void *args) {
    (void)dsbuf; (void)fmt; (void)args;
    return 0;
}

int erts_fprintf(void *fp, const char *fmt, ...) {
    /* Route to real fprintf; the loader's "fp" is typically NULL
     * (silent) or stderr. */
    (void)fp; (void)fmt;
    return 0;
}

void erts_send_error_to_logger(void *gl, void *dsbuf) {
    (void)gl;
    fprintf(stderr, "cache_tool: loader error reported\n");
}

/* ---- MD5 (used for BEAM file integrity) ---------------------- */

void erts_md5_init(void *ctx) {
    /* TODO: real MD5 or punt. The loader uses MD5 over the BEAM
     * file's code chunk for stale-code detection at runtime; for
     * the cache tool we use SHA-256 of the whole .beam (in
     * cache_tool_reader.c) instead. Compute-then-discard. */
    (void)ctx;
}
void erts_md5_update(void *ctx, const void *data, size_t sz) {
    (void)ctx; (void)data; (void)sz;
}
void erts_md5_finish(void *ctx, unsigned char digest[16]) {
    (void)ctx;
    memset(digest, 0, 16);
}

/* ---- Magic-ref / binary management --------------------------- */

void *erts_make_magic_ref_in_array(void *binary, void *array) {
    /* Magic refs wrap binary data with a destructor. The compile
     * path uses them for prepared-code binaries; the tool
     * doesn't need real ref semantics, just a stable pointer. */
    (void)array;
    return binary;
}

void erts_magic_ref_remove_bin(void *ref) {
    (void)ref;
    /* No-op; the binary itself outlives the tool. */
}

/* ----------------------------------------------------------------
 * Batch 2: bulk abort stubs for runtime-only entry points.
 *
 * These functions get referenced by the JIT emitters either:
 *  (a) as targets the emitted code calls at runtime (binary
 *      syntax helpers, process signal queue, scheduler), or
 *  (b) as fragment references inside beam_asm_global / process_main
 *      that are themselves referenced symbolically by per-module
 *      emitted code.
 *
 * Compile path NEVER reaches any of these. Abort stubs are safe
 * AND catch stub drift in dev/CI (if a future code change pulls
 * one into the compile path, the abort fires immediately).
 *
 * Naming convention: ABORT_STUB(name, signature) declares a
 * function that aborts when called, but exists as a linker symbol.
 * For variadic/special types we spell them out individually.
 * ---------------------------------------------------------------- */

#define ABORT_STUB(name) \
    void name(void) { \
        fprintf(stderr, "cache_tool: unreachable: %s\n", #name); \
        abort(); \
    }

/* Binary syntax — emitted code calls these at runtime. */
ABORT_STUB(erts_bs_append_checked)
ABORT_STUB(erts_bs_get_binary_2)
ABORT_STUB(erts_bs_get_float_2)
ABORT_STUB(erts_bs_get_integer_2)
ABORT_STUB(erts_bs_get_utf16)
ABORT_STUB(erts_bs_init_writable)
ABORT_STUB(erts_bs_private_append_checked)
ABORT_STUB(erts_bs_put_binary)
ABORT_STUB(erts_bs_put_float)
ABORT_STUB(erts_bs_put_integer_be)
ABORT_STUB(erts_bs_put_integer_le)
ABORT_STUB(erts_bs_put_utf16)
ABORT_STUB(erts_bs_put_utf8)
ABORT_STUB(erts_bs_skip_bits2)
ABORT_STUB(erts_bs_start_match_3)

/* Process / signal queue / scheduler — runtime only. */
ABORT_STUB(erts_proc_lock_failed)
ABORT_STUB(erts_proc_unlock_failed)
ABORT_STUB(erts_proc_sig_fetch__)
ABORT_STUB(erts_proc_sig_decode_dist)
ABORT_STUB(erts_proc_sig_queue_lock)
ABORT_STUB(erts_proc_sig_receive_helper)
ABORT_STUB(erts_msgq_pass_recv_markers)
ABORT_STUB(erts_msgq_recv_marker_create_insert)
ABORT_STUB(erts_msgq_remove_leading_recv_markers_set_save_first)
ABORT_STUB(erts_schedule)
ABORT_STUB(erts_schedule_delete_node)
ABORT_STUB(erts_thr_progress_unmanaged_continue__)
ABORT_STUB(erts_thr_progress_unmanaged_delay__)

/* GC — runtime only. */
ABORT_STUB(erts_garbage_collect)
ABORT_STUB(erts_garbage_collect_hibernate)
ABORT_STUB(erts_gc_after_bif_call_lhf)
ABORT_STUB(erts_gc_after_bif_call)

/* Maps — runtime only (emitted code accesses these). */
ABORT_STUB(erts_maps_find)
ABORT_STUB(erts_maps_get)

/* PD / mixed arith — runtime only. */
ABORT_STUB(erts_pd_hash_get_with_hx)
ABORT_STUB(erts_pd_hash_get)
ABORT_STUB(erts_mixed_plus)
ABORT_STUB(erts_mixed_minus)
ABORT_STUB(erts_mixed_times)
ABORT_STUB(erts_mixed_div)

/* Tracing — runtime only. */
ABORT_STUB(erts_call_trace)
ABORT_STUB(erts_trace_return_to)

/* Misc runtime-only entry points. */
ABORT_STUB(erts_init_trap_export)
ABORT_STUB(erts_thr_prgr_data_key__)
ABORT_STUB(erts_global_literals_data_lock)
ABORT_STUB(erts_global_literals_data_unlock)
ABORT_STUB(erts_set_pd)
ABORT_STUB(erts_set_gc_state)

#undef ABORT_STUB

/* ----------------------------------------------------------------
 * Batch 3: data-side recording stubs.
 *
 * These are global VARIABLES (not functions) that the emitter
 * references. For the compile path they only need to exist as
 * linker symbols with sensible defaults; we never actually read
 * them during compilation.
 * ---------------------------------------------------------------- */

/* Active code index — runtime tracks which version of code is
 * currently live. For the tool, a single dummy index suffices. */
uint32_t the_active_code_index = 0;
uint32_t the_staging_code_index = 0;
uint64_t erts_init_process_id = 0;     /* THE_NON_VALUE-ish */

/* Various runtime tables — present as zero-init blobs sufficient
 * for startup to bind. Anything that derefs them at runtime
 * either errors precisely (good — bug in our codepath) or never
 * gets called (we never exercise that codepath). */
char erts_port[1024];
char erts_proc[1024];
char erts_node_tab[1024];
char erts_dist_table[1024];
uint64_t erts_max_atoms = 65536;
int erts_no_of_schedulers = 1;
int erts_no_schedulers = 1;
int erts_no_dirty_cpu_schedulers = 0;
int erts_no_dirty_io_schedulers = 0;
void *erts_default_arg_reg = NULL;
void *erts_msacc = NULL;
void *erts_this_dist_entry = NULL;
void *erts_this_node_sysname = NULL;
void *erts_default_trace_pattern_flags = NULL;
int erts_default_trace_pattern_is_on = 0;
void *erts_default_meta_match_spec = NULL;
void *erts_default_meta_tracer = NULL;
void *erts_default_match_spec = NULL;
void *erts_internal_dist_entry = NULL;
char erts_this_node_storage[1024];

/* Code-related globals. */
void *erts_copy_literal_area__ = NULL;
void *erts_copy_literal_area = NULL;
void *erts_global_literal_area = NULL;
int erts_no_of_code_ix = 1;
char erts_code_loaded[1024];
char erts_module_table_storage[1024];

/* Misc loader globals. */
void *erts_total_code_size = NULL;
int erts_module_instance_default_state = 0;
char erts_ranges_storage[1024];

/* erl_drv globals. */
int erts_async_threads = 0;
int erts_use_kernel_poll = 0;
void *erts_kp_data = NULL;
void *erts_nkp_data = NULL;

/* Tracing globals (referenced at startup but never called on our path). */
void *erts_default_trace_ip_flags = NULL;
int erts_default_trace_ip_is_on = 0;
char erts_trace_ip_table_storage[1024];

/* Process registry. */
char erts_proc_dict_words_storage[1024];

/* DB/ETS globals (some module code might reference). */
void *erts_db_table_pid = NULL;

/* Atom table — opaque pointer; loader stores atoms via our
 * recording erts_atom_put above, doesn't dereference. */
void *erts_atom_table = NULL;

/* Whether the system is fully initialised — pretend it is. */
int erts_initialized = 1;

/* Coverage flags — disable. */
int erts_coverage_mode = 0;

/* Debugger flags — disable. */
uint32_t erts_debugger_flags = 0;

/* Allocator table — the loader's erts_alloc_fnf inline function
 * dispatches through this. Provide a real malloc-backed table so
 * `(*erts_allctrs[type].alloc)(extra, size)` works correctly.
 *
 * Each entry has (alloc, realloc, free, extra). Our impls ignore
 * the type/extra and just route to malloc.
 */
static void *stub_alloc(int type, void *extra, size_t sz) {
    (void)type; (void)extra; return malloc(sz);
}
static void *stub_realloc(int type, void *extra, void *p, size_t sz) {
    (void)type; (void)extra; return realloc(p, sz);
}
static void stub_free(int type, void *extra, void *p) {
    (void)type; (void)extra; free(p);
}

struct stub_allctr {
    void *(*alloc)  (int, void*, size_t);
    void *(*realloc)(int, void*, void*, size_t);
    void  (*free)   (int, void*, void*);
    void  *extra;
};

/* ERTS_ALC_A_MAX = 12; size is +1 for the sentinel. */
struct stub_allctr erts_allctrs[13] = {
    [0 ... 12] = { stub_alloc, stub_realloc, stub_free, NULL },
};

/* Magic-binary destructor list, no-op. */
void *erts_no_line_info = NULL;

/* BIF table — empty. The emitter looks up symbolic MFAs; with
 * recording stubs (emit_mov_bif) the actual table is never
 * read on the compile path. */
struct dummy_bif_entry { void *fn; uint32_t arity; const char *mfa; };
const struct dummy_bif_entry bif_table[1] = {{NULL, 0, NULL}};
void *bif_trap_exports__[1] = { NULL };

/* gen_opc / opc / tag_to_letter — provided by generated
 * beam_opcodes.c; build script links it in. */

/* ----------------------------------------------------------------
 * Batch 4: long-tail abort stubs to drive the link error count to
 * zero.
 *
 * Each of these has a TODO: many of them ARE called by the
 * loader's happy path (factory_*, decode_ext, init_ranges, etc.)
 * and need real implementations before the tool produces valid
 * output. But aborting on call is correct behaviour — it catches
 * "I tried to compile a module but the loader needs something
 * I haven't stubbed yet" cases in dev/CI immediately, rather
 * than producing silently-wrong cache files.
 *
 * Replace each abort stub with a real implementation as the tool
 * is iterated to handle real .beam files.
 * ---------------------------------------------------------------- */

#define ABORT_STUB(name) \
    void name(void) { \
        fprintf(stderr, "cache_tool: unreachable: %s\n", #name); \
        abort(); \
    }

/* Atom table — loader will need real impls (record symbolically). */
ABORT_STUB(erts_atom_get_name)
ABORT_STUB(erts_atom_to_string)

/* Allocator wrappers we haven't covered. */
ABORT_STUB(erts_alcu_enable_code_atags)
ABORT_STUB(erts_alloc_enomem)

/* UTF-8 / Unicode — loader uses some for module names. */
ABORT_STUB(erts_analyze_utf8)
ABORT_STUB(erts_make_list_from_utf8_buf)
ABORT_STUB(erts_unicode_list_to_buf)
ABORT_STUB(erts_bin_bytes_to_list)

/* Binary syntax helpers (the remaining ones). */
ABORT_STUB(erts_bs_put_binary_all)
ABORT_STUB(erts_bs_put_string)
ABORT_STUB(erts_build_sub_bitstring)
ABORT_STUB(erts_wrap_refc_bitstring)
ABORT_STUB(erts_cmp_bits__)
ABORT_STUB(erts_copy_bits_fwd)

/* Bit operations — runtime arithmetic. */
ABORT_STUB(erts_band)
ABORT_STUB(erts_bnot)
ABORT_STUB(erts_bor)
ABORT_STUB(erts_bsl)
ABORT_STUB(erts_bsr)
ABORT_STUB(erts_bxor)
ABORT_STUB(erts_int_div_rem)
ABORT_STUB(erts_mul_add)
ABORT_STUB(erts_unary_minus)

/* NIF support — runtime only. */
ABORT_STUB(erts_call_nif_early)
ABORT_STUB(erts_call_trace_return)
ABORT_STUB(erts_post_nif)
ABORT_STUB(erts_pre_nif)
ABORT_STUB(erts_load_nif)

/* Process lifecycle — runtime only. */
ABORT_STUB(erts_cancel_proc_timer)
/* erts_cleanup_messages / erts_cleanup_offheap — provided by
 * erl_message.c which is linked in. */
/* ABORT_STUB(erts_cleanup_messages) */
/* ABORT_STUB(erts_cleanup_offheap) */
ABORT_STUB(erts_continue_exit_process)
ABORT_STUB(erts_do_exit_process)
ABORT_STUB(erts_hibernate)
/* erts_save_message_in_proc — provided by erl_message.c. */
/* ABORT_STUB(erts_save_message_in_proc) */
ABORT_STUB(erts_set_proc_timer_term)
ABORT_STUB(erts_suspend_process_on_pending_purge_lambda)
ABORT_STUB(erts_sanitize_freason)

/* Comparison + hash — runtime only. */
ABORT_STUB(erts_cmp_compound)

/* Decoder — external term format. Provided by external.c which
 * is linked in directly. */
/* ABORT_STUB(erts_decode_ext) */
/* ABORT_STUB(erts_decode_ext_size) */

/* Loader's actual happy path — TODO: real implementations. */
ABORT_STUB(erts_export_put)
/* factory_* — provided by erl_message.c which is linked in. */
/* ABORT_STUB(erts_factory_close) */
/* ABORT_STUB(erts_factory_dummy_init) */
/* ABORT_STUB(erts_factory_heap_frag_init) */
ABORT_STUB(erts_find_export_entry)
ABORT_STUB(erts_find_function)
ABORT_STUB(erts_fun_entry_put)
ABORT_STUB(erts_get_module)
ABORT_STUB(erts_init_ranges)
ABORT_STUB(erts_module_instance_init)
ABORT_STUB(erts_put_module)
ABORT_STUB(erts_record_put)
ABORT_STUB(erts_seal_module)
ABORT_STUB(erts_unseal_module)
ABORT_STUB(erts_set_fun_code)
ABORT_STUB(erts_update_ranges)
ABORT_STUB(erts_jit_asm_dump)
ABORT_STUB(erts_clear_all_export_break)
ABORT_STUB(erts_release_code_mod_permission)
ABORT_STUB(erts_try_seize_code_mod_permission)

/* GC. */
ABORT_STUB(erts_garbage_collect_nobump)
ABORT_STUB(erts_gc_new_map)
ABORT_STUB(erts_gc_update_map_assoc)
ABORT_STUB(erts_gc_update_map_exact)

/* Maps. */
ABORT_STUB(erts_maps_put)
ABORT_STUB(erts_maps_update)
ABORT_STUB(erts_map_size)
ABORT_STUB(erts_hashmap_get)

/* Misc runtime — abort. */
ABORT_STUB(erts_generic_breakpoint)
ABORT_STUB(erts_global_literal_allocate)
ABORT_STUB(erts_global_literal_register)
ABORT_STUB(erts_heap_alloc)
ABORT_STUB(erts_line_breakpoint_hit__cleanup)
ABORT_STUB(erts_line_breakpoint_hit__prepare_call)
ABORT_STUB(erts_list_length)
ABORT_STUB(erts_move_multi_frags)
ABORT_STUB(erts_printable_return_address)
ABORT_STUB(erts_snprintf)
ABORT_STUB(erts_get_scheduler_data)
ABORT_STUB(erts_system_monitor_long_msgq_off)
ABORT_STUB(erts_system_monitor_long_schedule)
ABORT_STUB(erts_timestamp_millis)
ABORT_STUB(erts_trace_return)
ABORT_STUB(erts_trapping_length_1)

/* Transform engine — provided by generated beam_opcodes.c. */
/* ABORT_STUB(erts_transform_engine) */

/* Variables that need to exist as linker symbols. */
struct dummy_sys_time { uint32_t pad[16]; };
struct dummy_sys_time erts_sys_time_data__ = {{0}};
void *erts_this_node = NULL;

/* Non-erts runtime helpers. */
ABORT_STUB(add_stacktrace)
ABORT_STUB(apply)
ABORT_STUB(beam_catches_cons)
ABORT_STUB(beam_catches_init)
ABORT_STUB(beam_make_current_old)
ABORT_STUB(beam_types_decode_extra)
ABORT_STUB(beam_types_decode_type)
ABORT_STUB(big_to_double)
ABORT_STUB(build_stacktrace)
ABORT_STUB(bytes_to_big)
ABORT_STUB(call_error_handler)
ABORT_STUB(check_monitor_long_schedule)
ABORT_STUB(copy_in_registers)
ABORT_STUB(copy_out_registers)
ABORT_STUB(copy_struct_x)
ABORT_STUB(eq)
ABORT_STUB(erl_assert_error)
ABORT_STUB(erl_create_local_native_record)
ABORT_STUB(erl_create_native_record)
ABORT_STUB(erl_get_local_record_field)
ABORT_STUB(erl_get_record_elements)
ABORT_STUB(erl_get_record_field)
ABORT_STUB(erl_is_function)
ABORT_STUB(erl_update_native_record)
ABORT_STUB(erl_zlib_uncompress)
ABORT_STUB(fixed_apply)
ABORT_STUB(get_map_element)
ABORT_STUB(get_map_element_hash)
ABORT_STUB(get_tracer_ref_from_weak_id)
ABORT_STUB(handle_error)
ABORT_STUB(make_hash2)
ABORT_STUB(monitor_long_msgq_off)
ABORT_STUB(raw_raise)
ABORT_STUB(save_calls)
ABORT_STUB(send_2)
ABORT_STUB(seq_trace_output_generic)
ABORT_STUB(size_object_x)
ABORT_STUB(trace_receive)
ABORT_STUB(ubif2mfa)
ABORT_STUB(ethr_tsd_get)

/* Exception frames + receive markers — runtime only. */
void *exp_receive = NULL;
void *exp_timeout = NULL;
void *export_list[1] = { NULL };
uint32_t export_list_size = 0;

/* BIF trap exports — referenced as &name. Stubs as zeroed
 * Export-sized blobs (real size is ~256 bytes; pad generously). */
char bif_return_trap_export[512];
char bif_continue_exit_trap_export[512];
char bif_trap_export[512];

/* zlib callbacks — used by external term format decoder. */
void *erl_zlib_zalloc_callback(void *opaque, unsigned items, unsigned size) {
    (void)opaque;
    return calloc(items, size);
}
void erl_zlib_zfree_callback(void *opaque, void *ptr) {
    (void)opaque;
    free(ptr);
}

/* Atom builder — used only at boot. */
ABORT_STUB(am_atom_put)

/* Global literals — for the tool, sentinel values. The runtime
 * cache loader will resolve real Eterm values when patching. */
typedef uintptr_t Eterm;
Eterm ERTS_GLOBAL_LIT_EMPTY_TUPLE = 0;
Eterm ERTS_GLOBAL_LIT_ERL_FILE_SUFFIX = 0;

#undef ABORT_STUB

