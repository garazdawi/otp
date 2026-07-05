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

#include "t2_retain.h"
#include "t2_pctab.h"

/* ------------------------------------------------------------------ *
 * Re-decode of the retained chunk (shares the SSA builder's ordinals) *
 * ------------------------------------------------------------------ */

typedef struct {
    Eterm function;
    Uint32 arity;
    Uint32 call_count;   /* number of call generic ops                  */
    Uint32 effect_count; /* number of gc_bif generic ops                */
    Uint32 *call_ords;   /* their decode ordinals, in program order     */
    Uint32 *effect_ords;
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

/* A generic call op counts as a "call" for the zip iff codegen would emit
 * one of the hooked call ops for it. Local calls always do. A call_ext to
 * a BIF (including apply/yield/hibernate/... which are themselves BIFs)
 * lowers to a bif/apply dispatch that codegen does not tag as a call, so
 * only non-BIF call_ext targets count -- matching t2_pc_classify's
 * emit-side op set exactly. */
static int pctab_genop_is_call(const ErtsT2RetainedCode *ret,
                               const BeamOp *op) {
    UWord import_index;
    const Export *ep;

    switch (op->op) {
    case genop_call_2:
    case genop_call_last_3:
    case genop_call_only_2:
        return 1;
    case genop_call_ext_2:
    case genop_call_ext_last_3:
    case genop_call_ext_only_2:
        import_index = op->a[1].val;
        if (import_index >= (UWord)ret->import_count) {
            return 1;
        }
        ep = erts_active_export_entry(ret->imports[import_index].module,
                                      ret->imports[import_index].function,
                                      ret->imports[import_index].arity);
        return (ep == NULL || ep->bif_number < 0) ? 1 : 0;
    default:
        return 0;
    }
}

static int pctab_is_effect(int op) {
    switch (op) {
    case genop_gc_bif1_5:
    case genop_gc_bif2_6:
    case genop_gc_bif3_7:
        return 1;
    default:
        return 0;
    }
}

/* Decode the retained code chunk and record, per function, the decode
 * ordinals (matching t2_hir_builder's numbering: entry label == 0, then
 * one per generic op) of every call op and every gc_bif op. Returns a
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
        Uint32 call_cur = 0, eff_cur = 0;

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
                eff_cur = 0;
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

                    if (pctab_genop_is_call(ret, op)) {
                        if (pass == 0) {
                            fns[fn_idx].call_count++;
                        } else {
                            fns[fn_idx].call_ords[call_cur++] = ord;
                        }
                    } else if (pctab_is_effect(op->op)) {
                        if (pass == 0) {
                            fns[fn_idx].effect_count++;
                        } else {
                            fns[fn_idx].effect_ords[eff_cur++] = ord;
                        }
                    }
                    idx++;
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
                if (fns[i].effect_count > 0) {
                    fns[i].effect_ords =
                            erts_alloc(ERTS_ALC_T_T2_CODE,
                                       fns[i].effect_count * sizeof(Uint32));
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
        if (fns[i].effect_ords != NULL) {
            erts_free(ERTS_ALC_T_T2_CODE, fns[i].effect_ords);
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

    /* Per-function zip: pair emit-order call/effect entries with decode
     * ordinals by position, when the per-kind counts agree. */
    for (f = 0; f < fc; f++) {
        ErtsT2PcFunc *fn = &tab->funcs[f];
        Uint32 call_n = 0, eff_n = 0;
        Uint32 call_i = 0, eff_i = 0, last_call_bi = ERTS_T2_PC_BEAM_IDX_UNKNOWN;
        Uint32 e;
        int call_exact, eff_exact;

        for (e = 0; e < fn->entry_count; e++) {
            switch (fn->entries[e].kind) {
            case ERTS_T2_PC_CALL:
                call_n++;
                break;
            case ERTS_T2_PC_EFFECT:
                eff_n++;
                break;
            default:
                break;
            }
        }

        call_exact = (call_n == dec[f].call_count);
        eff_exact = (eff_n == dec[f].effect_count);

        if (pctab_debug_enabled() && fn->entry_count > 0 &&
            (!call_exact || !eff_exact)) {
            erts_fprintf(stderr,
                         "t2_pctab: %T/%u count mismatch "
                         "(emit call=%u eff=%u; decode call=%u eff=%u)\n",
                         fn->function,
                         (unsigned)fn->arity,
                         (unsigned)call_n,
                         (unsigned)eff_n,
                         (unsigned)dec[f].call_count,
                         (unsigned)dec[f].effect_count);
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
                last_call_bi = ent->beam_idx;
                break;
            case ERTS_T2_PC_CONT:
                ent->beam_idx = last_call_bi;
                break;
            case ERTS_T2_PC_EFFECT:
                ent->beam_idx =
                        eff_exact ? dec[f].effect_ords[eff_i++]
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

ErtsCodePtr erts_t2_pc_lookup(const ErtsT2RetainedCode *ret,
                              Uint fn_index,
                              Uint beam_idx) {
    const ErtsT2PcTable *tab;
    const ErtsT2PcFunc *fn;
    Uint32 e;

    if (ret == NULL || ret->pc_table == NULL) {
        return NULL;
    }
    tab = ret->pc_table;
    if (fn_index >= (Uint)tab->func_count) {
        return NULL;
    }
    fn = &tab->funcs[fn_index];

    /* The re-call target is the call site with this decode ordinal. Entries
     * are offset-sorted; within a function offset and beam_idx both follow
     * program order, but the table is small so a linear scan is used. */
    for (e = 0; e < fn->entry_count; e++) {
        if (fn->entries[e].kind == ERTS_T2_PC_CALL &&
            fn->entries[e].beam_idx == (Uint32)beam_idx) {
            return (ErtsCodePtr)(tab->module_base + fn->entries[e].offset);
        }
    }
    return NULL;
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
