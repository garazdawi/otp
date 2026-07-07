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
 * T2-Full tier-2 JIT: per-module retention of the decoded-code tables
 * that the tier-2 SSA reconstructor needs after loading has finished
 * (PLAN/T2FULL/07 §1).
 *
 * Everything the loader parses out of a BEAM file dies right after T1
 * emission (`beamfile_free` via `beam_load_prepared_free`), and the raw
 * code-chunk bytes are never owned by the BeamFile at all -- they point
 * into the transient module binary. So retention COPIES what the
 * builder needs into one module-lifetime allocation:
 *
 *   - the raw code chunk bytes (re-decoded by the SSA builder),
 *   - the atom index -> Eterm map (atoms themselves are interned),
 *   - the import table,
 *   - the Type-chunk table (freed at beamfile_free otherwise;
 *     nothing type-related survives load today),
 *   - the static-literal index -> literal-area-term map (the terms
 *     already survive via beamfile_move_literals; only the map from
 *     the chunk's literal indices is needed),
 *   - the per-function eligibility bitmap.
 *
 * Retention only happens when tier-2 is enabled (the T2_RETAIN
 * environment variable, so default builds and runs carry zero cost)
 * and the eligibility scan finds at least one function whose full op
 * set the tier supports.
 */

#ifndef ERL_T2_RETAIN_H__
#define ERL_T2_RETAIN_H__

#include "sys.h"
#include "beam_file.h"

struct erl_module_instance;

typedef struct ErtsT2RetainedCode {
    /* Copied raw code chunk bytes plus the header fields needed to
     * re-decode them with beamfile_get_code(). */
    byte *code;
    Sint code_size;
    Sint32 function_count;
    Sint32 label_count;
    Sint32 max_opcode;

    /* Copied atom index -> tagged atom map. */
    Eterm *atoms;
    Sint32 atom_count;

    /* Copied import table. */
    BeamFile_ImportEntry *imports;
    Sint32 import_count;

    /* Copied type table (entry 0 is always "any"). */
    BeamType *types;
    Sint32 type_count;
    char types_fallback;

    /* Static literal index -> literal-area term. Valid for the lifetime
     * of the module instance (the literal area dies at purge, after
     * this struct is released). Dynamic (negative) literal indices are
     * not mapped; a re-decode rebuilds those terms from the chunk. */
    Eterm *literal_map;
    Sint32 literal_count;

    /* One bit per function, set iff every op in the function is in the
     * tier-2 supported set. */
    Uint32 *eligible_bitmap;

    /* Lambda (FunT chunk) retention (P2 commit 8): make_fun3's first
     * argument indexes this table. The meta half is copied at prepare;
     * the ErlFunEntry pointers are captured at finalize from the
     * loader's fun-patch loop (asm_load.c), so the tier never has to
     * re-derive the staged fun-table key. An entry pointer is valid
     * for the lifetime of the module instance (the instance's FunRef
     * literals hold the entries alive; blobs die at jettison, before
     * purge). NULL/0 when the module has no lambdas. */
    struct ErtsT2Lambda *lambdas;
    Sint32 lambda_count;

    /* One bit per function, set iff the function contains a local
     * self-recursive tail call (call_only/call_last back to its own
     * entry label) — the loop shape tier-up profiles. Computed by the
     * same eligibility scan; only meaningful where the eligible bit is
     * set. */
    Uint32 *loop_bitmap;

    /* Tier-up profile block (P2 commit 9), or NULL: one cache-line
     * record per function (ERTS_T2_PROFILE_STRIDE bytes, indexed by
     * function index), armed (threshold != 0) only for eligible loop
     * functions. Allocated separately at prepare time — before T1
     * codegen, which bakes the record addresses into the profiling
     * sequence — and freed with the retained struct. */
    struct ErtsT2Profile *profiles;

    /* Total size of this allocation, for accounting. */
    Uint bytes;

    /* T1 PC side table (PLAN/T2FULL/07 §4), built at retain-commit after
     * codegen from the assembler's collected offsets. NULL until then,
     * or when the module has no eligible functions. Separately
     * allocated; freed and un-accounted in erts_t2_release. */
    struct ErtsT2PcTable *pc_table;
} ErtsT2RetainedCode;

/* One retained lambda-table entry (see ErtsT2RetainedCode.lambdas). */
typedef struct ErtsT2Lambda {
    Sint32 label;    /* the implementation function's entry label     */
    Sint32 arity;    /* TOTAL arity (fun arity + num_free), as in FunT */
    Sint32 num_free;
    const void *fun_entry; /* the loaded instance's ErlFunEntry*, set
                            * at finalize; NULL until then             */
} ErtsT2Lambda;

/* Capture lambda i's ErlFunEntry at finalize (asm_load.c fun-patch
 * loop). No-op when \p ret is NULL or i is out of range. */
void erts_t2_retain_lambda_entry(ErtsT2RetainedCode *ret,
                                 int i,
                                 const void *fun_entry);

/* True iff tier-2 retention is enabled (T2_RETAIN=1; read once). */
int erts_t2_enabled(void);

/* One-time initialization; called from beamasm_init(). */
void erts_t2_init(void);

/* Phase 1, during prepare (erts_prepare_loading, after load_code):
 * runs the eligibility scan and, if any function is eligible, copies
 * every table above except the literal-map values, which do not exist
 * until beamfile_move_literals runs at finalize.
 *
 * MUST run during prepare: beam->code.data points into the caller's
 * module binary, which is not guaranteed to stay alive through
 * erts_finish_loading.
 *
 * Returns the half-built struct (owned by the loader state), or NULL
 * when disabled / nothing eligible. */
ErtsT2RetainedCode *erts_t2_prepare(BeamFile *beam);

/* Phase 2, at finalize (beam_load_finalize_code, after
 * beamfile_move_literals): fills the literal map from the moved
 * literal entries, attaches the struct to \p inst_p and starts
 * accounting for it. Takes ownership from the loader state. */
void erts_t2_retain_commit(ErtsT2RetainedCode *ret,
                           BeamFile *beam,
                           struct erl_module_instance *inst_p);

/* Frees a prepared-but-never-committed struct (loader error paths). */
void erts_t2_retained_free(ErtsT2RetainedCode *ret);

/* Frees \c inst_p->t2_retained , if any. Called at module purge. */
void erts_t2_release(struct erl_module_instance *inst_p);

/* Total retained bytes, for erlang:memory(code) accounting. */
UWord erts_t2_retained_sz(void);

/* Adjust the retained-bytes total by \p delta (may be negative). Used by
 * t2_pctab.c to fold the separately-allocated PC side table into the same
 * erlang:memory(code) accounting. */
void erts_t2_account_bytes(Sint delta);

/* t2_eligible.c: true iff the tier supports this generic opcode. */
int erts_t2_genop_supported(int genop);

/* --- the byte-aligned bs_match subset (P2 commit 7) ------------------
 *
 * bs_match/3 carries a variadic command list; only the byte-aligned
 * subset below is tier-2 supported (PLAN/T2FULL/09 §7: bit-unaligned
 * commands are rejected at the *scan*, never discovered at isel). The
 * parser is the single source of truth shared by the eligibility scan
 * and the SSA builder, exactly like erts_t2_genop_supported. */

typedef enum {
    ERTS_T2_BS_ENSURE = 0,  /* ensure_at_least Size Unit (Size%8==0)    */
    ERTS_T2_BS_READ_INT8,   /* integer, Size*Unit==8, no flags          */
    ERTS_T2_BS_SKIP,        /* skip Size (Size%8==0)                    */
    ERTS_T2_BS_GET_TAIL     /* get_tail                                 */
} ErtsT2BsCmdKind;

typedef struct ErtsT2BsCmd {
    int kind;     /* ErtsT2BsCmdKind                                    */
    UWord size;   /* bits (ENSURE/SKIP/READ_INT8)                       */
    UWord unit;   /* ENSURE trailing unit                               */
    UWord live;   /* READ_INT8 / GET_TAIL GC live count                 */
    int dst_arg;  /* generic-op arg index of the Dst operand, or -1     */
} ErtsT2BsCmd;

/* At most this many decoded commands per bs_match op (the scan subset
 * needs 2-3; a longer list is rejected as outside the subset). */
#define ERTS_T2_BS_MAX_CMDS 8

/* Parse + validate bs_match/3's command args (the generic op's args
 * 3..nargs-1, given as parallel type/val arrays indexed by *generic op
 * arg position*). `lit` resolves a TAG_q literal index (both callers
 * resolve against a different literal table). On success returns the
 * command count (>= 0), fills `out` (when non-null; sized
 * ERTS_T2_BS_MAX_CMDS) and `*dst_count`. Returns -1 when any command
 * falls outside the byte-aligned subset, more than one command
 * produces a destination, or the list is malformed. */
int erts_t2_bs_match_check(const UWord *types,
                           const UWord *vals,
                           int first,
                           int nargs,
                           Eterm (*lit)(void *env, SWord idx),
                           void *env,
                           ErtsT2BsCmd *out,
                           int *dst_count);

/* t2_emit.cpp: P1 commit-3 structural selftest (T2_EMIT_SELFTEST=1).
 * Called from beam_load_finalize_code right after the pctab is built;
 * for the t2_mvp corpus module it runs total/2 through the full
 * build->isel->verify->emit pipeline and asserts the blob's cross-tier
 * structure (CP == pctab CONT, sync-map Y stores, op-specific side
 * exits, self tail target) — aborting on any mismatch. No-op for all
 * other modules or when the env var is unset. `code_hdr` is the
 * instance's BeamCodeHeader. */
void erts_t2_emit_selftest_module(const ErtsT2RetainedCode *ret,
                                  const void *code_hdr);

/* t2_hir_builder.cpp: true iff T2_BUILD=1 (build SSA for every eligible
 * function right after retention commits, as a load-time corpus
 * crash/validation test). */
int erts_t2_build_enabled(void);

/* t2_hir_builder.cpp: builds + validates SSA for every eligible
 * function in \p ret. Returns the number of functions that failed to
 * build or validate (0 = green). Reports failures on stderr.
 * When T2_ISEL=1 is also set and \p code_hdr (the instance's
 * BeamCodeHeader) is non-null, every built function is additionally
 * pushed through isel + LIR verification and per-module coverage is
 * reported (isel failures are expected — the P1 identity table is
 * partial — and are not counted as build failures). */
int erts_t2_build_all(const ErtsT2RetainedCode *ret, const void *code_hdr);

/* t2_eligible.c: scans every function's generic ops; returns a bitmap
 * with one bit per function (caller frees, ERTS_ALC_T_T2_CODE), or
 * NULL when the module has no functions. Sets \p *any_eligible . When
 * \p loop_bitmap_out is non-null it receives a second same-sized
 * bitmap (caller frees) marking functions with a local self-recursive
 * tail call — the loop shape tier-up profiles. */
Uint32 *erts_t2_eligibility_scan(BeamFile *beam,
                                 int *any_eligible,
                                 Uint32 **loop_bitmap_out,
                                 int *on_load_out,
                                 Uint32 *arities_out,
                                 Uint32 *sizes_out);

/* --------------------------------------------------------------------
 * Profile-driven tier-up (P2 commit 9; PLAN/T2FULL/09 §1,
 * PLAN/T2/02 §§7.1-7.4, PLAN/T2/05 §15).
 *
 * Per eligible loop function, T1 emits a short profiling sequence at
 * function entry (arm/instr_common.cpp emit_i_test_yield): a call
 * counter, a "non-small argument observed" bitmask (exactly the fact
 * the speculation inserter's T2FactSource consumes — the full
 * seen-types bitmask of 02 §7.2 carries nothing the v1 consumer
 * reads), and a threshold compare whose trip path calls
 * erts_t2_profile_trip. Scheduler-1-only writes (02 §7.3): every
 * scheduler's registers carry a redirect pointer that is NULL on
 * scheduler 1 and the shared throwaway block elsewhere, so the store
 * traffic of other schedulers hits one dead cache line and the real
 * records stay uncontended. The throwaway block's threshold is
 * ~0, so it never trips.
 * ------------------------------------------------------------------ */

typedef struct ErtsT2Profile {
    Uint32 count;     /* call counter (scheduler-1 writes, racy)      */
    Uint32 threshold; /* trip when count >= threshold; 0 = not armed  */
    Uint32 nonsmall;  /* bit i set: argument i observed non-small
                       * (sampled at each trip; see the trip handler) */
    Uint32 snapshot;  /* nonsmall at the previous trip (stability)    */
    Uint32 retries;   /* stability retries so far                     */
    Uint32 fn_index;  /* function index within the module             */
    Uint32 arity;     /* function arity (masks the sampled bits)      */
    Eterm module;     /* module atom (worker re-lookup + identity)    */

    /* Counter self-disarm (P2 commit 10): where the T1 profiling
     * sequence landed (executable view) and its byte length, written
     * at finalize once the module base is known. Disarming patches
     * the sequence's first instruction to `b <seq_size>` — a tripped
     * (or failed) function's counter then costs one taken branch on
     * every T1 entry/demote instead of the full sequence. seq_addr is
     * NULLed after the patch (single writer: the tier worker / the
     * loader, both under code permission). */
    ErtsCodePtr seq_addr;
    Uint32 seq_size;
} ErtsT2Profile;

/* One cache line per record: scheduler-1's stores never share a line
 * with a neighbouring function's record. */
#define ERTS_T2_PROFILE_STRIDE 64

/* threshold value while a compile is pending/consumed: never trips
 * again (the counter stays below it until a 2^32 wrap). */
#define ERTS_T2_TIER_PENDING 0xFFFFFFFFu

/* True iff counter-triggered tier-up is on: the tier is enabled and
 * +JT2enable (forced compile-at-load) is not. */
int erts_t2_tier_enabled(void);

/* The default trip threshold (05 §15.1's `base`; the size/recompile/
 * cache-pressure terms are deferred with the recompile machinery).
 * Overridable with T2_TIER_THRESHOLD for testing and for the tax
 * measurement's "thresholds never tripped" leg. */
Uint32 erts_t2_tier_threshold(void);

/* base * sqrt(size + 1) (05 s15.1's size term; P2 commit 10), size =
 * the function's generic-op count from the eligibility scan; a flat
 * T2_TIER_THRESHOLD override skips the size term for determinism. The
 * recompile and cache-pressure terms stay deferred. */
Uint32 erts_t2_tier_threshold_for(Uint32 size);

/* t2_tier.c: queue-lock init; called from erts_t2_init at boot. */
void erts_t2_tier_init(void);

/* t2_tier.c: trip handler, called from the T1 profiling sequence's
 * cold path (scheduler 1 only). Applies the stability rule
 * (05 §15.2, simplified to the nonsmall mask), CASes the record to
 * pending and enqueues a compile job for the single worker. */
void erts_t2_profile_trip(ErtsT2Profile *p,
                          Eterm a0,
                          Eterm a1,
                          Eterm a2,
                          Eterm a3);

/* t2_tier.c: {trips, stability_resets, enqueued, dropped, compiled,
 * installed, failed} for the t2_tier_stats debug BIF. */
Eterm erts_t2_tier_stats_term(Process *p);

/* t2_tier.c: diagnostic census (debug-only, backs
 * erts_debug:get_internal_state(t2_profile_census)) of every ARMED
 * profiling record across the active instances of all loaded modules,
 * bucketed by how close its scheduler-1 counter got to the trip
 * threshold. Answers which armed-but-never-tripped functions pay the
 * steady-state profiling tax and why (legitimately cold vs. just-under
 * threshold). Returns a flat integer tuple; layout documented at the
 * definition. Advisory: reads racy counters without stopping the
 * world. */
Eterm erts_t2_profile_census_term(Process *p);

/* t2_tier.c: address of the throwaway record, for the scheduler
 * redirect initialization (a plain-prototype-friendly accessor). */
UWord erts_t2_profile_throwaway_addr(void);

/* beam_jit_main.cpp: hand the module assembler the profile block so
 * the per-function entry sequences can be emitted (bridge in the
 * beamasm_t2_pc_raw_* style). NULL/0 emits nothing. */
void beamasm_set_t2_profiles(void *ba,
                             struct ErtsT2Profile *profiles,
                             int function_count);

/* beam_jit_main.cpp: write each armed record's profiling-sequence
 * address/length (see ErtsT2Profile.seq_addr) once codegen is done and
 * the executable base is known. Called at finalize, before
 * erts_t2_disarm_module_profiles can run. */
void beamasm_t2_fill_profile_seqs(void *ba, const char *base);

/* t2_install.c: patch the function's T1 profiling sequence to a single
 * `b` over it (P2 commit 10) — called by the tier worker under code
 * modification permission once the record's outcome is terminal
 * (installed OR failed: a failed shape cannot improve without new
 * code). Idempotent; no-op when the record has no sequence. */
void erts_t2_profile_disarm(struct erl_module_instance *mi,
                            struct ErtsT2Profile *rec);

/* t2_install.c: force-disarm every armed profiling sequence of a
 * freshly loaded module (T2_TIER_DISARM=1) — the tax-measurement
 * lever: the "never tripped" leg with the sequences patched out
 * measures the residual cost of the disarmed state. Runs at finalize
 * while the loader holds the module unsealed. */
int erts_t2_tier_disarm_forced(void);
void erts_t2_disarm_module_profiles(struct erl_module_instance *mi,
                                    struct ErtsT2RetainedCode *ret);

/* t2_compile.cpp: compile + install a batch of profiled functions of
 * one module (the tier worker's per-module body; the caller holds
 * code-modification permission). The module is re-looked-up by name —
 * a purge/reload between trip and compile drops the batch. The module
 * decode is shared across the whole batch (PLAN/T2FULL/09 §1's queue
 * decode caching). Returns the number of functions installed; on return
 * `*rejected_out` (may be NULL) holds how many the install-quality gate
 * refused (P2.6 blocker B) -- terminal, so counted apart from failures. */
unsigned erts_t2_tier_compile_batch(Eterm module,
                                    const Uint32 *fn_indices,
                                    unsigned n,
                                    unsigned *rejected_out);

/* t2_debug.cpp: debug BIF backing erts_debug:get_internal_state(
 * {t2_build_ssa, M, F, A}). Looks up module M's active instance, runs the
 * SSA builder on the retained chunk for F/A, and serializes the
 * reconstructed T2Function into a structured Erlang term (see the format
 * comment in t2_debug.cpp). Returns:
 *   am_undefined              -- module not loaded / nothing retained
 *   {error, not_found}        -- no such F/A in the module
 *   {error, not_eligible}     -- F/A found but not tier-2 eligible
 *   {error, build_failed, Rsn}-- decode/build/validate failed
 *   {ok, SsaTerm}             -- success
 * Requires the module to have been loaded with T2_RETAIN=1. */
Eterm erts_t2_debug_build_ssa(Process *p, Eterm mod, Eterm func, Eterm arity);

#endif /* ERL_T2_RETAIN_H__ */
