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
} ErtsT2RetainedCode;

/* True iff tier-2 retention is enabled (T2_RETAIN=1; read once). */
int erts_t2_enabled(void);

/* One-time initialization; called from beamasm_init(). */
void erts_t2_init(void);

/* Runs the eligibility scan over \p beam and, if any function is
 * eligible, copies the tables above into \c inst_p->t2_retained .
 *
 * Must be called after beamfile_move_literals() (so the literal map
 * resolves to literal-area terms) and while the BeamFile is still
 * alive, i.e. from beam_load_finalize_code(). */
void erts_t2_retain(BeamFile *beam, struct erl_module_instance *inst_p);

/* Frees \c inst_p->t2_retained , if any. Called at module purge. */
void erts_t2_release(struct erl_module_instance *inst_p);

/* Total retained bytes, for erlang:memory(code) accounting. */
UWord erts_t2_retained_sz(void);

/* t2_eligible.c: true iff the tier supports this generic opcode. */
int erts_t2_genop_supported(int genop);

/* t2_eligible.c: scans every function's generic ops; returns a bitmap
 * with one bit per function (caller frees, ERTS_ALC_T_T2_CODE), or
 * NULL when the module has no functions. Sets \p *any_eligible . */
Uint32 *erts_t2_eligibility_scan(BeamFile *beam, int *any_eligible);

#endif /* ERL_T2_RETAIN_H__ */
