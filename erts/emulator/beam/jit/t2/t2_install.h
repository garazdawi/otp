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
 * T2-Full tier-2 JIT: dynamic install / jettison of tier-2 blobs
 * (PLAN/T2FULL/08 §3-§4, grounded in PLAN/T2/06 §§1-3, §5.1-5.2).
 *
 * Install is the NIF/trace model: the single patchable `b next`
 * instruction at L_f+4 in the function prologue (the word
 * 0x14000002 on aarch64, laid down by emit_i_breakpoint_trampoline)
 * is rewritten to branch to the blob's entry stub. Every caller kind
 * -- external via Export.dispatch, intra-module direct branch, fun,
 * apply -- converges on L_f and runs enter_erlang_frame followed by
 * the patched branch, so one 4-byte store redirects them all.
 * Export.addressv is never touched.
 *
 * Reach policy (map §3, correctness-first): direct `b` when the blob
 * entry is within +-128MB of L_f+4; else a near-side bridge veneer
 * (`ldr x14, .+8; br x14; .quad entry`) allocated from the global
 * JitAllocator *if* that lands in range; else the install is
 * REJECTED and the function stays on T1.
 *
 * Strict trace/NIF mutual exclusion (T2/06 §1.1, §2.4): install only
 * proceeds on a clean prologue (breakpoint flag zero, no staged
 * GenericBp, the +4 word bit-exact 0x14000002); conversely the
 * breakpoint/NIF installers call erts_t2_jettison_function *before*
 * erts_asm_bp_set_flag ("trace always wins").
 *
 * Jettison (uninstall) reverts the +4 word, deregisters the blob
 * from t2_ranges, unlinks it from the instance's install list and
 * schedules the span release behind a code barrier
 * (erts_schedule_code_barrier: thread progress + instruction
 * barriers on all schedulers), which guarantees no scheduler is
 * still executing inside the blob -- P1 blobs contain no CPs and no
 * resume PCs (PLAN/T2/08 §4.3), so in-flight invocations leave the
 * blob on their next call/return/side-exit and can never re-enter
 * once the prologue is reverted.
 *
 * Everything here runs under code modification permission (or
 * single-threaded early boot / blocked thread progress), NOT load
 * permission: the patch mutates an existing module in place and
 * needs no code_ix staging.
 */

#ifndef ERL_T2_INSTALL_H__
#define ERL_T2_INSTALL_H__

#include "sys.h"
#include "beam_code.h"
#include "code_ix.h"

#include "t2_ranges.h"

struct erl_module_instance;

/* One installed tier-2 blob. Linked into the owning module instance's
 * t2_installs list; all fields are protected by code modification
 * permission. */
typedef struct ErtsT2Install {
    struct ErtsT2Install *next;

    const ErtsCodeInfo *ci; /* function header, executable view       */
    ErtsCodeMFA mfa;        /* copied for introspection               */

    const void *patch_addr; /* L_f + 4, executable view               */
    Uint32 patched_word;    /* the `b` written at install (debug)     */

    const void *blob_entry; /* the entry stub (branch target)         */
    const void *blob_base;  /* JitAllocator span                      */
    size_t blob_size;
    void *blob_rw;          /* the span's writable alias (tombstones) */

    void *bridge_base;      /* near-side bridge veneer span, or NULL  */
    size_t bridge_size;

    /* The blob's resume table (P2 commit 5), or NULL when the function
     * has no recovered loops. Also registered on the t2_ranges blob
     * descriptor, which owns/frees it at deregistration; this pointer
     * is only for jettison's tombstone writes + retire scheduling. */
    ErtsT2ResumeTab *resume_tab;

    /* Cross-module dependencies (P2 commit 8): the BeamCodeHeaders of
     * OTHER module instances whose T1 addresses are baked into the
     * blob (the lists helper an intrinsic demotes to) plus the blob's
     * own instance header when it inlined a fun body (trace on the fun
     * implementation must kill the blob). erts_t2_jettison_deps scans
     * these. */
    const void *dep_hdrs[2];
    Uint32 dep_count;
} ErtsT2Install;

typedef enum {
    ERTS_T2_INSTALL_OK = 0,
    /* Trace/NIF owns the prologue (flag byte set or GenericBp staged);
     * strict mutual exclusion, T2/06 §2.4. */
    ERTS_T2_INSTALL_REJECTED_BP,
    /* The word at L_f+4 is not the pristine `b next` (0x14000002). */
    ERTS_T2_INSTALL_REJECTED_PROLOGUE,
    /* Neither a direct `b` nor a reachable bridge veneer can span the
     * distance from L_f+4 to the blob entry. */
    ERTS_T2_INSTALL_REJECTED_REACH,
    /* The function already has a tier-2 blob installed. */
    ERTS_T2_INSTALL_REJECTED_DUP,
    /* Registration/bookkeeping failure (range overlap, ...). */
    ERTS_T2_INSTALL_REJECTED_STATE
} ErtsT2InstallResult;

/* Install `blob` over the function `ci_exec` of instance `mi`. All six
 * preconditions of T2/06 §2.6 are (re)checked here, under the
 * permission that serializes every prologue writer. On OK the install
 * takes ownership of the blob span (jettison/purge releases it); on
 * any rejection ownership stays with the caller.
 *
 * Works both when `mi` is sealed (unseals + reseals + flushes around
 * the store) and when the caller (the loader) already holds it
 * unsealed (writes through the RW view; the caller's final
 * erts_seal_module provides the flush). */
ErtsT2InstallResult erts_t2_install(struct erl_module_instance *mi,
                                    const ErtsCodeInfo *ci_exec,
                                    const void *blob_entry,
                                    const void *blob_base,
                                    size_t blob_size,
                                    void *blob_rw,
                                    const ErtsT2ResumeEntry *resume_points,
                                    Uint32 resume_count,
                                    const void *const *dep_hdrs,
                                    Uint32 dep_count);

/* Jettison every installed blob (across all modules, both instances)
 * that recorded \p code_hdr as a dependency (P2 commit 8): called when
 * the instance owning \p code_hdr is deleted/overwritten, traced or
 * NIF-patched — its T1 addresses baked into dependent blobs must stop
 * being reachable. Runs under code modification permission. */
void erts_t2_jettison_deps(const void *code_hdr);

/* Jettison the tier-2 blob installed over `ci_exec`, if any. Returns 1
 * if a blob was jettisoned, 0 otherwise. Callers on the trace/NIF path
 * should guard with mi->t2_installs != NULL to keep the common case a
 * single load+test. */
int erts_t2_jettison_function(struct erl_module_instance *mi,
                              const ErtsCodeInfo *ci_exec);

/* Jettison every tier-2 blob of `mi`. `revert_prologue` is false only
 * on the purge path, where the T1 code itself is about to be freed and
 * no caller can reach it anymore (purge's own quiescence guarantee). */
void erts_t2_jettison_instance(struct erl_module_instance *mi,
                               int revert_prologue);

/* Defer the release of up to two JitAllocator spans until after a code
 * barrier (thread progress + instruction barriers on all schedulers).
 * Also used by the exec-harness debug BIF to free its throwaway blobs.
 * Either span may be NULL. */
void erts_t2_free_spans_after_barrier(void *span1,
                                      size_t size1,
                                      void *span2,
                                      size_t size2);

/* Total bytes currently held by installed (non-tombstoned) tier-2
 * blobs + bridges, for erlang:memory(code) style accounting. */
UWord erts_t2_installed_sz(void);

/* Debug BIF backing (erl_bif_info.c, erts_debug:get_internal_state):
 *   {t2_install, M, F, A}   -> ok | {error, Reason::atom()}
 *   {t2_jettison, M, F, A}  -> ok | not_installed
 *   {t2_installed, M, F, A} -> false | {BlobStart, BlobSize, FindMfa}
 * where FindMfa is the MFA erts_t2_find_blob resolves for a PC in the
 * blob's interior (exercises the t2_ranges population). Implemented in
 * t2_compile.cpp (needs the C++ build pipeline). */
Eterm erts_t2_debug_install(Process *p, Eterm mod, Eterm func, Eterm arity);
Eterm erts_t2_debug_jettison(Process *p, Eterm mod, Eterm func, Eterm arity);
Eterm erts_t2_debug_installed(Process *p, Eterm mod, Eterm func, Eterm arity);

/* P2 commit 5 introspection (t2_install.c):
 *   {t2_in_blob, Pid} -> true | false | undefined
 *     whether Pid's saved instruction pointer (c_p->i) currently lies
 *     inside a registered T2 blob — i.e. the process is yielded at a
 *     recovered loop's back edge, to be resumed INTO T2;
 *   t2_yield_stats -> {BackEdgeYields, BackEdgeResumes}
 *     the racy monitoring counters bumped by the blob stubs. */
Eterm erts_t2_debug_in_blob(Process *p, Eterm pid);
Eterm erts_t2_debug_yield_stats(Process *p);

/* t2_compile.cpp: the +JT2enable synchronous compile-at-load driver
 * (map §5). Called from beam_load_finalize_code right after the pctab
 * is built, while the loader still holds the module unsealed and load
 * permission (which includes code modification permission). Builds,
 * lowers, emits and installs every eligible function of the module;
 * any per-function failure degrades that function to T1. No-op unless
 * erts_jit_t2_force. */
struct ErtsT2RetainedCode;
void erts_t2_compile_module(const struct ErtsT2RetainedCode *ret,
                            const void *code_hdr,
                            struct erl_module_instance *mi);

/* Cumulative driver statistics, for erts_debug:get_internal_state(
 * t2_stats):
 * {Modules, FunctionsBuilt, Installed, IselUnsupported, EmitFailed,
 *  InstallRejected, BuildFailed, CompileMicros} */
Eterm erts_t2_debug_stats(Process *p);

/* Cumulative P1/P2/P3 fire-counters (one bump per committed
 * transform, at the passes' trace/commit points), for
 * erts_debug:get_internal_state(t2_opt_stats):
 * {P1SitesInlined, P1LoopsRecovered, P2AccUnboxed, P2IvUnboxed,
 *  P3GuardsRemoved, P3IvOvfRemoved} */
Eterm erts_t2_debug_opt_stats(Process *p);

/* beam_jit_main.cpp: C bridge over the global JitAllocator. */
int beamasm_t2_jit_alloc(void **rx, void **rw, size_t size);
void beamasm_t2_jit_release(void *rx);

#endif /* ERL_T2_INSTALL_H__ */
