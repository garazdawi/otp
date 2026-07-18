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
 * T2-Full tier-2 JIT: T1 PC side table build/lookup/free. See t2_pctab.h
 * for the design (how codegen offsets are paired with SSA decode ordinals).
 */

#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "erl_alloc.h"
#include "beam_file.h"
#include "beam_opcodes.h"
#include "code_ix.h"
#include "module.h"
#include "export.h"
#include "erl_bif_table.h"

#include "t2_retain.h"
#include "t2_pctab.h"

/* ------------------------------------------------------------------ *
 * Re-decode of the retained chunk (shares the SSA builder's ordinals) *
 * ------------------------------------------------------------------ */

typedef struct {
    Eterm function;
    Uint32 arity;
    Uint32 call_count;   /* number of call generic ops                  */
    Uint32 bif_count;    /* number of light-BIF call_ext generic ops    */
    Uint32 effect_count; /* number of gc_bif generic ops                */
    Uint32 error_count;  /* number of badmatch/case_end/if_end ops      */
    Uint32 *call_ords;   /* their decode ordinals, in program order     */
    Uint32 *bif_ords;
    Uint32 *effect_ords;
    Uint32 *error_ords;
} PctabFnDecode;

static void pctab_view_init(BeamFile *view, const ErtsT2RetainedCode *ret) {
    sys_memset(view, 0, sizeof(*view));

    view->atoms.count = ret->atom_count;
    view->atoms.entries = ret->atoms;
    view->module = ret->atom_count > 1 ? ret->atoms[1] : NIL;

    view->types.count = ret->type_count;
    view->types.fallback = ret->types_fallback;
    view->types.entries = ret->types;

    view->static_literals.count = ret->literal_count;

    view->code.data = ret->code;
    view->code.size = (Sint32)ret->code_size;
    view->code.function_count = ret->function_count;
    view->code.label_count = ret->label_count;
    view->code.max_opcode = ret->max_opcode;
}

/* The closed set of call_ext targets the loader transforms into
 * dedicated instructions (ops.tab `u$func:` rules): i_yield /
 * i_hibernate / i_apply* / i_load_nif / i_call_on_load_function /
 * i_perf_counter. None of their emissions record a pctab entry, so the
 * decode side must classify them as "nothing" too (mirrors
 * is_transformed_call_ext in t2_isel.cpp). */
static int pctab_is_transformed_call_ext(Eterm m, Eterm f, Uint arity) {
    /* Non-predefined atoms interned once. */
    static Eterm hibernate = THE_NON_VALUE;
    static Eterm load_nif = THE_NON_VALUE;
    static Eterm on_load_fn = THE_NON_VALUE;
    static Eterm os_mod = THE_NON_VALUE;
    static Eterm perf_counter = THE_NON_VALUE;

    if (hibernate == THE_NON_VALUE) {
        hibernate = ERTS_MAKE_AM("hibernate");
        load_nif = ERTS_MAKE_AM("load_nif");
        on_load_fn = ERTS_MAKE_AM("call_on_load_function");
        os_mod = ERTS_MAKE_AM("os");
        perf_counter = ERTS_MAKE_AM("perf_counter");
    }

    if (m == am_erlang) {
        if ((f == am_yield && arity == 0) ||
            (f == hibernate && arity == 0) ||
            (f == am_apply && (arity == 2 || arity == 3)) ||
            (f == load_nif && arity == 2) ||
            (f == on_load_fn && arity == 1)) {
            return 1;
        }
    } else if (m == os_mod) {
        if (f == perf_counter && arity == 0) {
            return 1;
        }
    }
    return 0;
}

/* Classify a generic call op by the pctab kind codegen records for the
 * specific op it becomes, or -1 for none -- the two sides must agree
 * per kind for the zip to hold (t2_pc_classify is the emit-side mirror).
 * Mirrors the ops.tab call_ext transform exactly:
 *
 *   - the loader-transformed specials become dedicated instructions
 *     that record nothing;
 *   - heavy BIFs (is_heavy_bif) become i_call_ext, a plain export
 *     call, recorded as CALL;
 *   - all remaining BIF targets become call_light_bif (including the
 *     is_exit_bif tails, which drop the trailing deallocate/return but
 *     keep the one call_light_bif), recorded as BIF;
 *   - everything else is a real Erlang call, recorded as CALL. */
static int pctab_genop_call_kind(const ErtsT2RetainedCode *ret,
                                 const BeamOp *op) {
    UWord import_index;
    const Export *ep;

    switch (op->op) {
    case genop_call_2:
    case genop_call_last_3:
    case genop_call_only_2:
        return ERTS_T2_PC_CALL;
    case genop_call_ext_2:
    case genop_call_ext_last_3:
    case genop_call_ext_only_2:
        import_index = op->a[1].val;
        if (import_index >= (UWord)ret->import_count) {
            return ERTS_T2_PC_CALL;
        }
        if (pctab_is_transformed_call_ext(
                    ret->imports[import_index].module,
                    ret->imports[import_index].function,
                    ret->imports[import_index].arity)) {
            return -1;
        }
        ep = erts_active_export_entry(ret->imports[import_index].module,
                                      ret->imports[import_index].function,
                                      ret->imports[import_index].arity);
        if (ep == NULL || ep->bif_number < 0) {
            return ERTS_T2_PC_CALL;
        }
        if (bif_table[ep->bif_number].kind == BIF_KIND_HEAVY) {
            return ERTS_T2_PC_CALL;
        }
        return ERTS_T2_PC_BIF;
    default:
        return -1;
    }
}

static int pctab_is_effect(int op) {
    switch (op) {
    case genop_gc_bif1_5:
    case genop_gc_bif2_6:
    case genop_gc_bif3_7:
        return 1;
    /* Map-read sites (S1b.3): decode-side mirror of the three
     * i_get_map_element* EFFECT records (beam_asm_module.cpp). The
     * generic get_map_elements is retained pre-transform (single-key
     * reads reach the builder as genop_get_map_elements_3 too), so a LIVE
     * read zips 1:1 with the one specific op T1 emits. This decode walk
     * counts raw genops (pre-DCE), while emit reflects post-DCE code, so
     * a DEAD get_map_elements (e.g. an unreachable read the loader elides
     * — seen in unicode_util:compose/1) is counted here but not emitted:
     * decode >= emit. That surplus only trips the count-inexact fail-safe
     * (the whole function's effect PCs become BEAM_IDX_UNKNOWN, so it
     * simply is not specialized), exactly as the pre-existing gc_bif
     * effect mismatches do. emit is always a subset of decode (every
     * emitted op survived DCE and is in the raw stream), so an emit-only
     * surplus — the only way a matching count could hide a wrong-order
     * zip — cannot occur. */
    case genop_get_map_elements_3:
        return 1;
    default:
        return 0;
    }
}

/* The guard-BIF subset (WIN 3): decode-side mirror of the specific
 * ops t2_pc_classify records as EFFECT (beam_asm_module.cpp),
 * following the loader's ops.tab transforms exactly:
 *
 *   - element/2, map_get/2, is_map_key/2 lower to bif_element /
 *     bif_map_get / bif_is_map_key (or i_bif2 for a non-register map
 *     operand) — all recorded, so every instance classifies;
 *   - node/1 lowers to bif_node (register source) or i_bif1 — both
 *     recorded;
 *   - hd/tl with a real fail label and an xy source lower to
 *     is_nonempty_list + get_hd/get_tl, which record nothing; the
 *     {f,0} shape lowers to bif_hd/bif_tl and any other shape to
 *     i_bif1 — recorded.
 *
 * Only the allowlisted MFAs are classified: any other bif1/bif2
 * makes its function T2-ineligible (t2_eligible.c), and only
 * eligible functions' entries enter the table, so the two sides are
 * only ever zipped over admitted shapes. */
static int pctab_guard_bif_effect(const ErtsT2RetainedCode *ret,
                                  const BeamOp *op) {
    const BeamFile_ImportEntry *e;

    if ((op->op != genop_bif1_4 && op->op != genop_bif2_5) ||
        op->a[1].type != TAG_u || op->a[1].val >= (UWord)ret->import_count) {
        return 0;
    }
    e = &ret->imports[op->a[1].val];
    if (e->module != am_erlang) {
        return 0;
    }
    if (op->op == genop_bif1_4 && e->arity == 1) {
        if (e->function == am_hd || e->function == am_tl) {
            if (op->a[0].type == TAG_f &&
                (op->a[2].type == TAG_x || op->a[2].type == TAG_y)) {
                return 0;
            }
            return 1;
        }
        return e->function == am_node;
    }
    if (op->op == genop_bif2_5 && e->arity == 2) {
        return e->function == am_element || e->function == am_map_get ||
               e->function == am_is_map_key;
    }
    return 0;
}

/* Clause-entry match starts (P-C B1, the roll-back deopt): decode-side
 * mirror of the op_i_bs_start_match3_Stjd EFFECT record
 * (beam_asm_module.cpp), following the loader's ops.tab transforms
 * exactly:
 *
 *   - bs_start_match3 with a non-register Bin transforms to `jump`
 *     (i/c/a operand classes) and records nothing; a register Bin
 *     lowers to exactly one i_bs_start_match3;
 *   - bs_start_match4 lowers to bs_start_match3 (both the {f,Fail}
 *     and the no_fail forms), except `resume`, which becomes a move
 *     (or nothing) and records nothing. */
static int pctab_start_match_effect(const BeamOp *op) {
    if (op->op == genop_bs_start_match3_4) {
        return op->a[1].type == TAG_x || op->a[1].type == TAG_y;
    }
    if (op->op == genop_bs_start_match4_4) {
        if (op->a[0].type == TAG_a && op->a[0].val == am_resume) {
            return 0;
        }
        return op->a[2].type == TAG_x || op->a[2].type == TAG_y;
    }
    return 0;
}

/* utf8 scan sites (P-C L1, the ASCII-speculation side exit):
 * decode-side mirror of the op_i_bs_get_utf8_Sfd /
 * op_i_bs_skip_utf8_Sf EFFECT records (beam_asm_module.cpp),
 * following the loader's ops.tab transforms exactly: each genop has
 * ONE rule (bs_get_utf8 Fail=f Ms=xy u u Dst=d => i_bs_get_utf8;
 * bs_skip_utf8 Fail=f Ms=xy u u => i_bs_skip_utf8) and no fallback,
 * so a register-context op lowers to exactly one specific op and a
 * non-register context cannot load at all. The fail label is not
 * discriminated (the decoder normalizes {f,0} to TAG_p; the loader
 * pattern matches either). */
static int pctab_utf8_effect(const BeamOp *op) {
    if (op->op == genop_bs_get_utf8_5 || op->op == genop_bs_skip_utf8_4) {
        return op->a[1].type == TAG_x || op->a[1].type == TAG_y;
    }
    return 0;
}

/* Error-exit ops. Note the case_end/badmatch NotInX=cy transform emits a
 * separate leading move, so the specific op count still matches 1:1. */
static int pctab_is_error(int op) {
    switch (op) {
    case genop_badmatch_1:
    case genop_case_end_1:
    case genop_if_end_0:
    case genop_badrecord_1:
        return 1;
    default:
        return 0;
    }
}

/* T2_PRESCAN task #92: the born-8-wide byte-class scanners' clause entry is
 * `bs_start_match4 resume` (dropped when Ctx==Dst, records nothing) then a
 * standalone bs_get_position; the SWAR byte-class guard rolls back to that
 * position save (cursor un-advanced), so under the opt-in lever a STANDALONE
 * bs_get_position is an EFFECT re-entry. Emit-side mirror: the
 * op_i_bs_get_position_SS case in beam_asm_module.cpp (both backends).
 *
 * The one loader transform that removes a separate i_bs_get_position is the
 * fusion `bs_start_match3 F Bin Live Ctx | bs_get_position Ctx2 Pos x _ |
 * equal(Ctx,Ctx2) => i_bs_start_match3_gp` (ops.tab). That case emits NO
 * standalone get_position, so it must NOT be counted here either — mirror
 * the fusion exactly by inspecting the immediately preceding genop. Any
 * other bs_get_position lowers 1:1 to i_bs_get_position (arch-64), so the
 * decode/emit effect counts zip. Off by default (returns 0 unless the lever
 * is set), so an unset environment records nothing new. */
static int pctab_get_position_effect(const BeamOp *op,
                                     int prev_op,
                                     const BeamOpArg *prev_ctx) {
    if (!erts_t2_prescan_enabled() || op->op != genop_bs_get_position_3) {
        return 0;
    }
    /* Fused with the preceding start_match3 (same context)? Then the loader
     * absorbs it — no standalone i_bs_get_position, so do not count. */
    if (prev_op == genop_bs_start_match3_4 && op->arity >= 1 &&
        prev_ctx->type == op->a[0].type && prev_ctx->val == op->a[0].val) {
        return 0;
    }
    return 1;
}

/* Decode the retained code chunk and record, per function, the decode
 * ordinals (matching t2_hir_builder's numbering: entry label == 0, then
 * one per generic op) of every call/bif/gc_bif/error op. Returns a
 * function_count-long array (caller frees via pctab_decode_free), or NULL
 * when there are no functions. */
static PctabFnDecode *pctab_decode(const ErtsT2RetainedCode *ret) {
    PctabFnDecode *fns;
    BeamFile view;
    BeamOpAllocator op_alloc;
    BeamCodeReader *reader;
    BeamOp *op;
    Sint32 fc = ret->function_count;
    int fn_idx, done, pass;

    if (fc <= 0) {
        return NULL;
    }

    fns = erts_alloc(ERTS_ALC_T_T2_CODE, fc * sizeof(PctabFnDecode));
    sys_memset(fns, 0, fc * sizeof(PctabFnDecode));

    /* Two passes: pass 0 counts (and captures each function's MFA), then
     * per-function ordinal arrays are allocated, and pass 1 fills them. */
    for (pass = 0; pass < 2; pass++) {
        Uint32 idx = 0;
        Uint32 call_cur = 0, bif_cur = 0, eff_cur = 0, err_cur = 0;

        /* T2_PRESCAN: the immediately preceding genop and (when it is a
         * bs_start_match3) its match-context operand, for the standalone-
         * bs_get_position effect test (pctab_get_position_effect). Reset at
         * every function boundary. */
        int prev_op = -1;
        BeamOpArg prev_ctx;

        sys_memset(&prev_ctx, 0, sizeof(prev_ctx));

        pctab_view_init(&view, ret);
        beamopallocator_init(&op_alloc);
        reader = beamfile_get_code(&view, &op_alloc);

        fn_idx = -1;
        done = 0;

        while (!done && beamcodereader_next(reader, &op)) {
            switch (op->op) {
            case genop_int_func_start_5:
                fn_idx++;
                idx = 0;
                call_cur = 0;
                bif_cur = 0;
                eff_cur = 0;
                err_cur = 0;
                prev_op = -1;
                if (pass == 0 && fn_idx < fc) {
                    fns[fn_idx].function = (Eterm)op->a[3].val;
                    fns[fn_idx].arity = (Uint32)op->a[4].val;
                }
                break;
            case genop_int_func_end_2:
                break;
            case genop_int_code_end_0:
                done = 1;
                break;
            default:
                if (fn_idx >= 0 && fn_idx < fc) {
                    Uint32 ord = idx;
                    int call_kind = pctab_genop_call_kind(ret, op);

                    if (call_kind == ERTS_T2_PC_CALL) {
                        if (pass == 0) {
                            fns[fn_idx].call_count++;
                        } else {
                            fns[fn_idx].call_ords[call_cur++] = ord;
                        }
                    } else if (call_kind == ERTS_T2_PC_BIF) {
                        if (pass == 0) {
                            fns[fn_idx].bif_count++;
                        } else {
                            fns[fn_idx].bif_ords[bif_cur++] = ord;
                        }
                    } else if (pctab_is_effect(op->op) ||
                               pctab_guard_bif_effect(ret, op) ||
                               pctab_start_match_effect(op) ||
                               pctab_utf8_effect(op) ||
                               pctab_get_position_effect(op,
                                                         prev_op,
                                                         &prev_ctx)) {
                        if (pass == 0) {
                            fns[fn_idx].effect_count++;
                        } else {
                            fns[fn_idx].effect_ords[eff_cur++] = ord;
                        }
                    } else if (pctab_is_error(op->op)) {
                        if (pass == 0) {
                            fns[fn_idx].error_count++;
                        } else {
                            fns[fn_idx].error_ords[err_cur++] = ord;
                        }
                    }
                    idx++;

                    /* Track the previous BODY genop for the fusion test
                     * above (capture the start_match3 context operand
                     * before the op is freed). Only body ops advance it;
                     * the func_start case resets it to -1. */
                    prev_op = op->op;
                    if (op->op == genop_bs_start_match3_4 && op->arity >= 4) {
                        prev_ctx = op->a[3];
                    }
                }
                break;
            }

            beamopallocator_free_op(&op_alloc, op);
        }

        beamcodereader_close(reader);
        beamopallocator_dtor(&op_alloc);

        if (pass == 0) {
            Sint32 i;

            for (i = 0; i < fc; i++) {
                if (fns[i].call_count > 0) {
                    fns[i].call_ords =
                            erts_alloc(ERTS_ALC_T_T2_CODE,
                                       fns[i].call_count * sizeof(Uint32));
                }
                if (fns[i].bif_count > 0) {
                    fns[i].bif_ords =
                            erts_alloc(ERTS_ALC_T_T2_CODE,
                                       fns[i].bif_count * sizeof(Uint32));
                }
                if (fns[i].effect_count > 0) {
                    fns[i].effect_ords =
                            erts_alloc(ERTS_ALC_T_T2_CODE,
                                       fns[i].effect_count * sizeof(Uint32));
                }
                if (fns[i].error_count > 0) {
                    fns[i].error_ords =
                            erts_alloc(ERTS_ALC_T_T2_CODE,
                                       fns[i].error_count * sizeof(Uint32));
                }
            }
        }
    }

    return fns;
}

static void pctab_decode_free(PctabFnDecode *fns, Sint32 fc) {
    Sint32 i;

    if (fns == NULL) {
        return;
    }
    for (i = 0; i < fc; i++) {
        if (fns[i].call_ords != NULL) {
            erts_free(ERTS_ALC_T_T2_CODE, fns[i].call_ords);
        }
        if (fns[i].bif_ords != NULL) {
            erts_free(ERTS_ALC_T_T2_CODE, fns[i].bif_ords);
        }
        if (fns[i].effect_ords != NULL) {
            erts_free(ERTS_ALC_T_T2_CODE, fns[i].effect_ords);
        }
        if (fns[i].error_ords != NULL) {
            erts_free(ERTS_ALC_T_T2_CODE, fns[i].error_ords);
        }
    }
    erts_free(ERTS_ALC_T_T2_CODE, fns);
}

/* ------------------------------------------------------------------ *
 * Build                                                              *
 * ------------------------------------------------------------------ */

static int pctab_offset_cmp(const void *a, const void *b) {
    Uint32 oa = ((const ErtsT2PcEntry *)a)->offset;
    Uint32 ob = ((const ErtsT2PcEntry *)b)->offset;

    return (oa < ob) ? -1 : (oa > ob) ? 1 : 0;
}

static int pctab_fn_eligible(const ErtsT2RetainedCode *ret, Uint32 fn_index) {
    if (fn_index >= (Uint32)ret->function_count) {
        return 0;
    }
    return (ret->eligible_bitmap[fn_index >> 5] &
            (((Uint32)1) << (fn_index & 31))) != 0;
}

static int pctab_debug_enabled(void) {
    static int enabled = -1;

    if (enabled < 0) {
        const char *env = getenv("T2_DEBUG");
        enabled = (env != NULL && env[0] == '1') ? 1 : 0;
    }
    return enabled;
}

void erts_t2_pctab_build(ErtsT2RetainedCode *ret,
                         void *ba,
                         const byte *module_base) {
    PctabFnDecode *dec;
    ErtsT2PcTable *tab;
    byte *base;
    size_t raw_all, elig_n, total, off, i;
    Sint32 fc, f;
    Uint32 *emit_count;

    if (ret == NULL || ret->function_count <= 0) {
        return;
    }
    ASSERT(ret->pc_table == NULL);

    fc = ret->function_count;
    raw_all = beamasm_t2_pc_raw_count(ba); /* covers all functions */

    dec = pctab_decode(ret);
    if (dec == NULL) {
        return;
    }

    /* Offsets were collected for every function during codegen (the
     * eligibility bitmap did not exist yet); the table keeps only the
     * entries of eligible functions. */
    elig_n = 0;
    for (i = 0; i < raw_all; i++) {
        Uint32 offset, fn_index;
        byte kind;

        beamasm_t2_pc_raw_get(ba, i, &offset, &fn_index, &kind);
        if (pctab_fn_eligible(ret, fn_index)) {
            elig_n++;
        }
    }

    /* One block: table header, then the per-function array, then all the
     * (eligible) entries (each function's slice is contiguous). */
    total = sizeof(ErtsT2PcTable) + (size_t)fc * sizeof(ErtsT2PcFunc) +
            elig_n * sizeof(ErtsT2PcEntry);
    base = erts_alloc(ERTS_ALC_T_T2_CODE, total);
    sys_memset(base, 0, total);

    tab = (ErtsT2PcTable *)base;
    tab->module_base = module_base;
    tab->func_count = fc;
    tab->funcs = (ErtsT2PcFunc *)(base + sizeof(ErtsT2PcTable));
    tab->bytes = total;

    {
        ErtsT2PcEntry *entries =
                (ErtsT2PcEntry *)(base + sizeof(ErtsT2PcTable) +
                                  (size_t)fc * sizeof(ErtsT2PcFunc));

        /* Per-function eligible-emit counts. */
        emit_count = erts_alloc(ERTS_ALC_T_T2_CODE, fc * sizeof(Uint32));
        sys_memset(emit_count, 0, fc * sizeof(Uint32));

        for (i = 0; i < raw_all; i++) {
            Uint32 offset, fn_index;
            byte kind;

            beamasm_t2_pc_raw_get(ba, i, &offset, &fn_index, &kind);
            if (pctab_fn_eligible(ret, fn_index)) {
                emit_count[fn_index]++;
            }
        }

        /* Per-function MFA + entry-slice assignment. */
        off = 0;
        for (f = 0; f < fc; f++) {
            tab->funcs[f].function = dec[f].function;
            tab->funcs[f].arity = dec[f].arity;
            tab->funcs[f].entry_count = 0; /* used as fill cursor below */
            tab->funcs[f].entries = emit_count[f] > 0 ? &entries[off] : NULL;
            off += emit_count[f];
        }
        ASSERT(off == elig_n);

        /* Append every eligible raw entry to its function's slice, in
         * emission order (offset/kind now; beam_idx after the zip). */
        for (i = 0; i < raw_all; i++) {
            Uint32 offset, fn_index;
            byte kind;
            ErtsT2PcFunc *fn;
            ErtsT2PcEntry *e;

            beamasm_t2_pc_raw_get(ba, i, &offset, &fn_index, &kind);
            if (!pctab_fn_eligible(ret, fn_index)) {
                continue;
            }
            fn = &tab->funcs[fn_index];
            e = &fn->entries[fn->entry_count++];
            e->offset = offset;
            e->kind = kind;
            e->beam_idx = ERTS_T2_PC_BEAM_IDX_UNKNOWN;
        }

        erts_free(ERTS_ALC_T_T2_CODE, emit_count);
    }

    /* Per-function zip: pair emit-order call/bif/effect entries with
     * decode ordinals by position, when the per-kind counts agree. */
    for (f = 0; f < fc; f++) {
        ErtsT2PcFunc *fn = &tab->funcs[f];
        Uint32 call_n = 0, bif_n = 0, eff_n = 0, err_n = 0;
        Uint32 call_i = 0, bif_i = 0, eff_i = 0, err_i = 0;
        /* A CONT entry is recorded immediately after its site (CALL or
         * BIF) in emission order, so it takes the last site's ordinal. */
        Uint32 last_site_bi = ERTS_T2_PC_BEAM_IDX_UNKNOWN;
        Uint32 e;
        int call_exact, bif_exact, eff_exact, err_exact;

        for (e = 0; e < fn->entry_count; e++) {
            switch (fn->entries[e].kind) {
            case ERTS_T2_PC_CALL:
                call_n++;
                break;
            case ERTS_T2_PC_BIF:
                bif_n++;
                break;
            case ERTS_T2_PC_EFFECT:
                eff_n++;
                break;
            case ERTS_T2_PC_ERROR:
                err_n++;
                break;
            default:
                break;
            }
        }

        call_exact = (call_n == dec[f].call_count);
        bif_exact = (bif_n == dec[f].bif_count);
        eff_exact = (eff_n == dec[f].effect_count);
        err_exact = (err_n == dec[f].error_count);

        if (pctab_debug_enabled() && fn->entry_count > 0 &&
            (!call_exact || !bif_exact || !eff_exact || !err_exact)) {
            erts_fprintf(stderr,
                         "t2_pctab: %T/%u count mismatch "
                         "(emit call=%u bif=%u eff=%u err=%u; "
                         "decode call=%u bif=%u eff=%u err=%u)\n",
                         fn->function,
                         (unsigned)fn->arity,
                         (unsigned)call_n,
                         (unsigned)bif_n,
                         (unsigned)eff_n,
                         (unsigned)err_n,
                         (unsigned)dec[f].call_count,
                         (unsigned)dec[f].bif_count,
                         (unsigned)dec[f].effect_count,
                         (unsigned)dec[f].error_count);
        }

        for (e = 0; e < fn->entry_count; e++) {
            ErtsT2PcEntry *ent = &fn->entries[e];

            switch (ent->kind) {
            case ERTS_T2_PC_ENTRY:
                ent->beam_idx = 0;
                break;
            case ERTS_T2_PC_CALL:
                ent->beam_idx =
                        call_exact ? dec[f].call_ords[call_i++]
                                   : ERTS_T2_PC_BEAM_IDX_UNKNOWN;
                last_site_bi = ent->beam_idx;
                break;
            case ERTS_T2_PC_BIF:
                ent->beam_idx =
                        bif_exact ? dec[f].bif_ords[bif_i++]
                                  : ERTS_T2_PC_BEAM_IDX_UNKNOWN;
                last_site_bi = ent->beam_idx;
                break;
            case ERTS_T2_PC_CONT:
                ent->beam_idx = last_site_bi;
                break;
            case ERTS_T2_PC_EFFECT:
                ent->beam_idx =
                        eff_exact ? dec[f].effect_ords[eff_i++]
                                  : ERTS_T2_PC_BEAM_IDX_UNKNOWN;
                break;
            case ERTS_T2_PC_ERROR:
                ent->beam_idx =
                        err_exact ? dec[f].error_ords[err_i++]
                                  : ERTS_T2_PC_BEAM_IDX_UNKNOWN;
                break;
            default:
                break;
            }
        }

        /* Sort each function's slice by offset for a stable, searchable
         * table (offsets are strictly increasing within a function). */
        if (fn->entry_count > 1) {
            qsort(fn->entries,
                  fn->entry_count,
                  sizeof(ErtsT2PcEntry),
                  pctab_offset_cmp);
        }
    }

    pctab_decode_free(dec, fc);

    ret->pc_table = tab;
    erts_t2_account_bytes((Sint)total);

    if (pctab_debug_enabled()) {
        erts_fprintf(stderr,
                     "t2_pctab: built table, %u entries, %lu bytes\n",
                     (unsigned)elig_n,
                     (unsigned long)total);
    }
}

void erts_t2_pctab_free(ErtsT2RetainedCode *ret) {
    ErtsT2PcTable *tab;

    if (ret == NULL || ret->pc_table == NULL) {
        return;
    }
    tab = ret->pc_table;
    erts_t2_account_bytes(-(Sint)tab->bytes);
    erts_free(ERTS_ALC_T_T2_CODE, tab);
    ret->pc_table = NULL;
}

/* ------------------------------------------------------------------ *
 * Lookup                                                             *
 * ------------------------------------------------------------------ */

ErtsCodePtr erts_t2_pc_lookup_kind(const ErtsT2RetainedCode *ret,
                                   Uint fn_index,
                                   Uint beam_idx,
                                   ErtsT2PcKind kind) {
    const ErtsT2PcTable *tab;
    const ErtsT2PcFunc *fn;
    Uint32 e;

    if (ret == NULL || ret->pc_table == NULL ||
        beam_idx == ERTS_T2_PC_BEAM_IDX_UNKNOWN) {
        return NULL;
    }
    tab = ret->pc_table;
    if (fn_index >= (Uint)tab->func_count) {
        return NULL;
    }
    fn = &tab->funcs[fn_index];

    /* Entries are offset-sorted; within a function offset and beam_idx
     * both follow program order, but the table is small so a linear scan
     * is used. */
    for (e = 0; e < fn->entry_count; e++) {
        if (fn->entries[e].kind == (byte)kind &&
            fn->entries[e].beam_idx == (Uint32)beam_idx) {
            return (ErtsCodePtr)(tab->module_base + fn->entries[e].offset);
        }
    }
    return NULL;
}

ErtsCodePtr erts_t2_pc_lookup(const ErtsT2RetainedCode *ret,
                              Uint fn_index,
                              Uint beam_idx) {
    /* The re-call target is the call site with this decode ordinal. */
    return erts_t2_pc_lookup_kind(ret, fn_index, beam_idx, ERTS_T2_PC_CALL);
}

/* ------------------------------------------------------------------ *
 * Debug BIF: {t2_pc_table, M, F, A}                                  *
 * ------------------------------------------------------------------ */

static Eterm pctab_kind_atom(byte kind) {
    switch (kind) {
    case ERTS_T2_PC_ENTRY:
        return erts_atom_put((const byte *)"entry", 5,
                             ERTS_ATOM_ENC_LATIN1, 1);
    case ERTS_T2_PC_CALL:
        return erts_atom_put((const byte *)"call", 4,
                             ERTS_ATOM_ENC_LATIN1, 1);
    case ERTS_T2_PC_CONT:
        return erts_atom_put((const byte *)"cont", 4,
                             ERTS_ATOM_ENC_LATIN1, 1);
    case ERTS_T2_PC_EFFECT:
        return erts_atom_put((const byte *)"effect", 6,
                             ERTS_ATOM_ENC_LATIN1, 1);
    case ERTS_T2_PC_ERROR:
        return erts_atom_put((const byte *)"error", 5,
                             ERTS_ATOM_ENC_LATIN1, 1);
    case ERTS_T2_PC_BIF:
        return erts_atom_put((const byte *)"bif", 3,
                             ERTS_ATOM_ENC_LATIN1, 1);
    default:
        return am_undefined;
    }
}

Eterm erts_t2_debug_pc_table(Process *p, Eterm mod, Eterm func, Eterm arity) {
    Module *modp;
    const ErtsT2RetainedCode *ret;
    const ErtsT2PcTable *tab;
    const ErtsT2PcFunc *fn = NULL;
    Sint32 f;
    Uint ar;
    Uint sz;
    Eterm *hp, res, unknown;
    Uint32 e;

    if (!is_atom(mod) || !is_atom(func) || !is_small(arity)) {
        return am_undefined;
    }
    ar = unsigned_val(arity);

    modp = erts_get_module(mod, erts_active_code_ix());
    if (modp == NULL || modp->curr.t2_retained == NULL) {
        return am_undefined;
    }
    ret = modp->curr.t2_retained;
    if (ret->pc_table == NULL) {
        return am_undefined;
    }
    tab = ret->pc_table;

    for (f = 0; f < tab->func_count; f++) {
        if (tab->funcs[f].function == func && tab->funcs[f].arity == ar) {
            fn = &tab->funcs[f];
            break;
        }
    }
    if (fn == NULL || fn->entry_count == 0) {
        return am_undefined;
    }

    /* [ {Offset, BeamIdx, KindAtom, Addr} ] -- size then build. */
    sz = 0;
    unknown = erts_atom_put((const byte *)"unknown", 7, ERTS_ATOM_ENC_LATIN1, 1);
    for (e = 0; e < fn->entry_count; e++) {
        sz += 2 /* list cons */ + 5 /* 4-tuple */;
        (void)erts_bld_uint(NULL, &sz, fn->entries[e].offset);
        if (fn->entries[e].beam_idx != ERTS_T2_PC_BEAM_IDX_UNKNOWN) {
            (void)erts_bld_uint(NULL, &sz, fn->entries[e].beam_idx);
        }
        (void)erts_bld_uword(NULL,
                             &sz,
                             (UWord)(tab->module_base + fn->entries[e].offset));
    }

    hp = HAlloc(p, sz);
    res = NIL;
    for (e = fn->entry_count; e > 0; e--) {
        const ErtsT2PcEntry *ent = &fn->entries[e - 1];
        Eterm off_t = erts_bld_uint(&hp, NULL, ent->offset);
        Eterm bi_t = ent->beam_idx != ERTS_T2_PC_BEAM_IDX_UNKNOWN
                             ? erts_bld_uint(&hp, NULL, ent->beam_idx)
                             : unknown;
        Eterm kind_t = pctab_kind_atom(ent->kind);
        Eterm addr_t = erts_bld_uword(
                &hp, NULL, (UWord)(tab->module_base + ent->offset));
        Eterm tup = TUPLE4(hp, off_t, bi_t, kind_t, addr_t);

        hp += 5;
        res = CONS(hp, tup, res);
        hp += 2;
    }

    return res;
}
