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

    /* Total size of this allocation, for accounting. */
    Uint bytes;

    /* T1 PC side table (PLAN/T2FULL/07 §4), built at retain-commit after
     * codegen from the assembler's collected offsets. NULL until then,
     * or when the module has no eligible functions. Separately
     * allocated; freed and un-accounted in erts_t2_release. */
    struct ErtsT2PcTable *pc_table;
} ErtsT2RetainedCode;

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

/* t2_hir_builder.cpp: true iff T2_BUILD=1 (build SSA for every eligible
 * function right after retention commits, as a load-time corpus
 * crash/validation test). */
int erts_t2_build_enabled(void);

/* t2_hir_builder.cpp: builds + validates SSA for every eligible
 * function in \p ret. Returns the number of functions that failed to
 * build or validate (0 = green). Reports failures on stderr. */
int erts_t2_build_all(const ErtsT2RetainedCode *ret);

/* t2_eligible.c: scans every function's generic ops; returns a bitmap
 * with one bit per function (caller frees, ERTS_ALC_T_T2_CODE), or
 * NULL when the module has no functions. Sets \p *any_eligible . */
Uint32 *erts_t2_eligibility_scan(BeamFile *beam, int *any_eligible);

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
