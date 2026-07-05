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
        return 1;

    default:
        return 0;
    }
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
            }
            break;
        }

        beamopallocator_free_op(&op_alloc, op);
    }

    beamcodereader_close(reader);
    beamopallocator_dtor(&op_alloc);

    return bitmap;
}
