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
 * T2-Full tier-2 JIT: per-function eligibility scan (PLAN/T2FULL/07 §7).
 *
 * A function is eligible iff every generic op in its body is in the
 * tier-2 supported set below. The scan is a separate pre-pass over the
 * decoded generic ops -- it cannot ride the emitter's function-entry
 * hook, which fires before the body is seen.
 *
 * The supported-op table is the single source of truth shared with the
 * SSA builder (t2_hir_builder.cpp): the builder handles exactly the ops
 * accepted here, so bitmap and builder coverage cannot drift.
 */

#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "erl_alloc.h"
#include "beam_file.h"
#include "beam_opcodes.h"

#include "t2_retain.h"

int erts_t2_genop_supported(int genop) {
    switch (genop) {
    /* Function scaffolding, synthesized by the code reader. */
    case genop_int_func_start_5:
    case genop_int_func_end_2:
    case genop_int_code_end_0:
    case genop_label_1:
    case genop_line_1:
    case genop_executable_line_2:

    /* Register and stack-frame plumbing. */
    case genop_move_2:
    case genop_swap_2:
    case genop_init_yregs_1:
    case genop_allocate_2:
    case genop_allocate_heap_3:
    case genop_deallocate_1:
    case genop_trim_2:
    case genop_test_heap_2:

    /* Calls and returns. */
    case genop_call_2:
    case genop_call_last_3:
    case genop_call_only_2:
    case genop_call_ext_2:
    case genop_call_ext_last_3:
    case genop_call_ext_only_2:
    case genop_return_0:

    /* Control flow. */
    case genop_jump_1:
    case genop_select_val_3:
    case genop_select_tuple_arity_3:

    /* Type tests and comparisons (guards). */
    case genop_is_integer_2:
    case genop_is_atom_2:
    case genop_is_nil_2:
    case genop_is_list_2:
    case genop_is_nonempty_list_2:
    case genop_is_tuple_2:
    case genop_test_arity_3:
    case genop_is_tagged_tuple_4:
    case genop_is_lt_3:
    case genop_is_ge_3:
    case genop_is_eq_3:
    case genop_is_ne_3:
    case genop_is_eq_exact_3:
    case genop_is_ne_exact_3:

    /* Tuples and lists. */
    case genop_get_list_3:
    case genop_get_hd_2:
    case genop_get_tl_2:
    case genop_get_tuple_element_3:
    case genop_put_list_3:
    case genop_put_tuple2_2:

    /* GC BIFs (generic arithmetic lowers to these). */
    case genop_gc_bif1_5:
    case genop_gc_bif2_6:
    case genop_gc_bif3_7:

    /* Clause-failure exits (modelled as error-exit blocks). */
    case genop_badmatch_1:
    case genop_if_end_0:
    case genop_case_end_1:

    /* The byte-aligned binary scan subset (P2 commit 7; PLAN/T2FULL/09
     * §7, PLAN/T2/08 §3). bs_match/3 is additionally argument-checked
     * (erts_t2_bs_match_check) — only its byte-aligned command subset
     * is supported, and the scan rejects anything else up front. */
    case genop_bs_start_match3_4:
    case genop_bs_match_3:
    case genop_bs_test_tail2_3:
    case genop_bs_get_tail_3:
        return 1;

    default:
        return 0;
    }
}

/* ------------------------------------------------------------------ *
 * The byte-aligned bs_match command subset (see t2_retain.h)          *
 * ------------------------------------------------------------------ */

int erts_t2_bs_match_check(const UWord *types,
                           const UWord *vals,
                           int first,
                           int nargs,
                           Eterm (*lit)(void *env, SWord idx),
                           void *env,
                           ErtsT2BsCmd *out,
                           int *dst_count) {
    int i = first;
    int ncmds = 0;
    int dsts = 0;

    while (i < nargs) {
        ErtsT2BsCmd cmd;

        if (ncmds == ERTS_T2_BS_MAX_CMDS) {
            return -1;
        }
        if (types[i] != TAG_a) {
            return -1;
        }

        sys_memset(&cmd, 0, sizeof(cmd));
        cmd.dst_arg = -1;

        if (vals[i] == am_ensure_at_least) {
            /* ensure_at_least Size Unit */
            if (i + 2 >= nargs || types[i + 1] != TAG_u ||
                types[i + 2] != TAG_u) {
                return -1;
            }
            cmd.kind = ERTS_T2_BS_ENSURE;
            cmd.size = vals[i + 1];
            cmd.unit = vals[i + 2];
            if (cmd.size == 0 || (cmd.size % 8) != 0 ||
                (cmd.unit != 1 && cmd.unit != 8)) {
                return -1;
            }
            i += 3;
        } else if (vals[i] == am_integer) {
            /* integer Live Flags Size Unit Dst */
            UWord size, unit;
            Eterm flags;

            if (i + 5 >= nargs || types[i + 1] != TAG_u ||
                types[i + 3] != TAG_u || types[i + 4] != TAG_u ||
                (types[i + 5] != TAG_x && types[i + 5] != TAG_y)) {
                return -1;
            }
            size = vals[i + 3];
            unit = vals[i + 4];
            if (size * unit != 8) {
                return -1; /* bit-unaligned / multi-byte: outside v1 */
            }
            /* Flags must be empty (unsigned, big): NIL or a literal
             * resolving to NIL. */
            if (types[i + 2] == TAG_n) {
                flags = NIL;
            } else if (types[i + 2] == TAG_q) {
                flags = lit(env, (SWord)vals[i + 2]);
            } else {
                return -1;
            }
            if (flags != NIL) {
                return -1;
            }
            cmd.kind = ERTS_T2_BS_READ_INT8;
            cmd.size = 8;
            cmd.unit = 1;
            cmd.live = vals[i + 1];
            cmd.dst_arg = i + 5;
            dsts++;
            i += 6;
        } else if (vals[i] == am_skip) {
            /* skip Size */
            if (i + 1 >= nargs || types[i + 1] != TAG_u) {
                return -1;
            }
            cmd.kind = ERTS_T2_BS_SKIP;
            cmd.size = vals[i + 1];
            if (cmd.size == 0 || (cmd.size % 8) != 0) {
                return -1;
            }
            i += 2;
        } else if (vals[i] == am_get_tail) {
            /* get_tail Live Unit Dst */
            if (i + 3 >= nargs || types[i + 1] != TAG_u ||
                (types[i + 3] != TAG_x && types[i + 3] != TAG_y)) {
                return -1;
            }
            cmd.kind = ERTS_T2_BS_GET_TAIL;
            cmd.live = vals[i + 1];
            cmd.dst_arg = i + 3;
            dsts++;
            i += 4;
        } else {
            /* ensure_exactly, '=:=', binary, non-byte shapes, ...:
             * outside the subset. */
            return -1;
        }

        if (out != NULL) {
            out[ncmds] = cmd;
        }
        ncmds++;
    }

    if (ncmds == 0 || dsts > 1) {
        /* No commands is malformed; two destinations would need a
         * second result home (the HIR op has one) — stays T1. */
        return -1;
    }
    if (dst_count != NULL) {
        *dst_count = dsts;
    }
    return ncmds;
}

/* Literal resolver over the load-time BeamFile (the eligibility scan
 * runs at retain-commit, when the static literal table is live). */
static Eterm scan_lit(void *env, SWord idx) {
    BeamFile *beam = (BeamFile *)env;

    if (idx < 0 || idx >= beam->static_literals.count) {
        return THE_NON_VALUE;
    }
    return beamfile_get_literal(beam, idx);
}

/* bs_match/3: Fail Ctx N Cmds... — args 3.. are the command list. */
static int bs_match_op_supported(BeamFile *beam, const BeamOp *op) {
    UWord types[64], vals[64];
    int n = op->arity;
    int i;

    if (n < 3 || n > 64) {
        return 0;
    }
    if (op->a[0].type != TAG_f) {
        return 0;
    }
    for (i = 0; i < n; i++) {
        types[i] = op->a[i].type;
        vals[i] = op->a[i].val;
    }
    return erts_t2_bs_match_check(types,
                                  vals,
                                  3,
                                  n,
                                  scan_lit,
                                  beam,
                                  NULL,
                                  NULL) >= 0;
}

Uint32 *erts_t2_eligibility_scan(BeamFile *beam, int *any_eligible) {
    BeamOpAllocator op_alloc;
    BeamCodeReader *reader;
    BeamOp *op;

    Uint32 *bitmap;
    size_t bitmap_words;
    int fn_idx = -1;
    int fn_ok = 0;
    int done = 0;

    *any_eligible = 0;

    if (beam->code.function_count == 0) {
        return NULL;
    }

    bitmap_words = (beam->code.function_count + 31) / 32;
    bitmap = erts_alloc(ERTS_ALC_T_T2_CODE, bitmap_words * sizeof(Uint32));
    sys_memset(bitmap, 0, bitmap_words * sizeof(Uint32));

    beamopallocator_init(&op_alloc);
    reader = beamfile_get_code(beam, &op_alloc);

    while (!done && beamcodereader_next(reader, &op)) {
        switch (op->op) {
        case genop_int_func_start_5:
            fn_idx++;
            fn_ok = 1;
            break;
        case genop_int_func_end_2:
            if (fn_ok && fn_idx >= 0 && fn_idx < beam->code.function_count) {
                bitmap[fn_idx / 32] |= ((Uint32)1) << (fn_idx % 32);
                *any_eligible = 1;
            }
            break;
        case genop_int_code_end_0:
            done = 1;
            break;
        default:
            if (fn_ok && !erts_t2_genop_supported(op->op)) {
                fn_ok = 0;
            } else if (fn_ok && op->op == genop_bs_match_3 &&
                       !bs_match_op_supported(beam, op)) {
                /* Byte-aligned subset only: a bit-unaligned or
                 * multi-destination command list stays T1, decided
                 * here at the scan (PLAN/T2FULL/09 §7 surprise 4). */
                fn_ok = 0;
            }
            break;
        }

        beamopallocator_free_op(&op_alloc, op);
    }

    beamcodereader_close(reader);
    beamopallocator_dtor(&op_alloc);

    return bitmap;
}
