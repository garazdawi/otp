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
 * T2-Full tier-2 JIT: T1 PC side table (PLAN/T2FULL/07 §4).
 *
 * For every tier-2-eligible function, this records the T1 (baseline JIT)
 * machine-code offsets of the four "re-entry" kinds a tier-2 deopt needs
 * to resume into: the function entry, each call site, each post-call
 * continuation, and each post-BIF/effect boundary. Each entry is tagged
 * with the BEAM op's decode ordinal (`beam_idx`) so the tier-2 deopt
 * resolver can map an SSA op back to the T1 resume PC (T2/08 §4.2).
 *
 * How beam_idx is assigned (the subtle part). The offsets are collected
 * at codegen time from the arm emitters, but codegen runs on the
 * loader's post-transform *specific* op stream, whose ordinals do not
 * line up with the SSA builder's *generic* pre-transform decode ordinals
 * (transforms fuse `move`+`call`, lower `gc_bif` to dedicated arith ops,
 * fuse `rem`+`div`, ...). So beam_idx is NOT taken from codegen. Instead,
 * at retain-commit the retained code chunk is decoded once more -- the
 * exact same decode the SSA builder uses, so the ordinals are shared by
 * construction -- to produce, per function and in program order, the
 * decode ordinal of every call op and every gc_bif op. Those are then
 * zipped, per kind and by position, onto the codegen-collected offsets:
 * the k-th emitted call gets the k-th decoded call op's ordinal, etc.
 * The function entry is ordinal 0 by convention (the entry label is the
 * SSA builder's beam_idx 0), and a post-call continuation shares its
 * call's ordinal.
 *
 * The zip is exact whenever the per-kind emit and decode counts agree,
 * which holds for the common case (calls and non-fused arithmetic).
 * A special-BIF call transform (apply/yield/...) or a fused rem/div can
 * make the counts disagree; when that happens for a kind, that kind's
 * entries in that function keep their offsets but carry
 * ERTS_T2_PC_BEAM_IDX_UNKNOWN rather than a guessed ordinal. Everything
 * here is gated on the module's eligibility bitmap, so ineligible
 * functions (and default, non-T2 loads) build no table at all.
 */

#ifndef ERL_T2_PCTAB_H__
#define ERL_T2_PCTAB_H__

#include "sys.h"

struct ErtsT2RetainedCode;

/* The re-entry kinds (PLAN/T2FULL/07 §4; ERROR added in P1). */
typedef enum {
    ERTS_T2_PC_ENTRY = 0,  /* function entry (i_test_yield)          */
    ERTS_T2_PC_CALL = 1,   /* call site (before the call emitter)    */
    ERTS_T2_PC_CONT = 2,   /* post-call continuation (after a return)*/
    ERTS_T2_PC_EFFECT = 3, /* gc_bif/effect op site (before the op)  */
    ERTS_T2_PC_ERROR = 4   /* error-exit op site (badmatch/case_end/
                            * if_end; before the op, so a T2 side
                            * exit re-executes it and T1 raises)     */
} ErtsT2PcKind;

/* Sentinel for a beam_idx that could not be reconciled (see header). */
#define ERTS_T2_PC_BEAM_IDX_UNKNOWN 0xFFFFFFFFU

typedef struct {
    Uint32 offset;   /* module-code-base-relative T1 PC              */
    Uint32 beam_idx; /* SSA builder decode ordinal, or UNKNOWN       */
    byte kind;       /* ErtsT2PcKind                                 */
} ErtsT2PcEntry;

typedef struct {
    Eterm function;
    Uint32 arity;
    Uint32 entry_count;
    ErtsT2PcEntry *entries; /* sorted by offset; points into the block */
} ErtsT2PcFunc;

typedef struct ErtsT2PcTable {
    const byte *module_base;    /* offset -> absolute code address     */
    Sint32 func_count;          /* == retained function_count          */
    ErtsT2PcFunc *funcs;        /* indexed by function index           */
    Uint bytes;                 /* whole-allocation size, for accounting */
} ErtsT2PcTable;

/* Build the PC side table for \p ret from the codegen-collected raw
 * entries held on the assembler \p ba, rebasing offsets against
 * \p module_base. Attaches the result to ret->pc_table and folds its
 * size into the retained-bytes accounting. No-op if ret is NULL or has
 * no eligible functions. Called from erts_t2_retain_commit. */
void erts_t2_pctab_build(struct ErtsT2RetainedCode *ret,
                         void *ba,
                         const byte *module_base);

/* Free ret->pc_table (if any) and un-account its bytes. Called from
 * erts_t2_release. */
void erts_t2_pctab_free(struct ErtsT2RetainedCode *ret);

/* Deopt re-call target resolver (T2/08 §4.2): the T1 PC of the call
 * site with decode ordinal \p beam_idx in function \p fn_index, or NULL
 * if there is no such entry. Binary search by offset after locating the
 * matching (kind==CALL, beam_idx) entry. */
ErtsCodePtr erts_t2_pc_lookup(const struct ErtsT2RetainedCode *ret,
                              Uint fn_index,
                              Uint beam_idx);

/* Kind-aware variant (P1 backend): the T1 PC of the entry with the
 * given decode ordinal and kind — CONT gives the post-call continuation
 * a non-tail T2 call pushes as its CP; EFFECT gives a gc_bif op's own
 * site for a side exit; ERROR gives a badmatch/case_end/if_end site.
 * NULL when there is no such entry (including UNKNOWN beam_idx from a
 * zip mismatch — the caller must treat that as "cannot lower"). */
ErtsCodePtr erts_t2_pc_lookup_kind(const struct ErtsT2RetainedCode *ret,
                                   Uint fn_index,
                                   Uint beam_idx,
                                   ErtsT2PcKind kind);

/* Debug BIF backing erts_debug:get_internal_state({t2_pc_table,M,F,A}):
 * returns [ {Offset, BeamIdx, KindAtom, Addr} ] for the function, or
 * 'undefined' when nothing is retained / no such function. */
Eterm erts_t2_debug_pc_table(Process *p, Eterm mod, Eterm func, Eterm arity);

/* --- assembler-side raw collection bridge (beam_jit_main.cpp) --------
 * The arm emitters push {offset, fn_index, kind} in emission order into
 * a vector on the module assembler; these C entry points let t2_pctab.c
 * read them back without touching C++ types. */
size_t beamasm_t2_pc_raw_count(void *ba);
void beamasm_t2_pc_raw_get(void *ba,
                           size_t i,
                           Uint32 *offset,
                           Uint32 *fn_index,
                           byte *kind);

/* Enable/disable the assembler's PC-table offset collection. The
 * eligibility bitmap does not exist yet at codegen time (erts_t2_prepare
 * runs after load_code), so collection is enabled whenever T2 retention
 * is on and the entries are filtered to eligible functions later, at
 * erts_t2_pctab_build. Called from beam_load_prepare_emit; off (the
 * default) records nothing. */
void beamasm_set_t2_collect(void *ba, int on);

#endif /* ERL_T2_PCTAB_H__ */
