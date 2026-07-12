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
 *
 * Eligible (buildable) and standalone-installable are distinct: a few
 * supported ops exist only to be erased by the P1 caller
 * specialization and have no isel lowering (erts_t2_genop_build_only).
 * The scan emits a second, stricter install bitmap without them; only
 * that bitmap admits a function to a standalone compile attempt.
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

    /* Fun creation (P2 commit 8). The decode resolves the lambda index
     * against the retained lambda table. */
    case genop_make_fun3_3:

    /* Fun application (P1a general call-site specialization,
     * PLAN/T2FULL/census/p1_design.md §2). Decoded to the CallFun HIR
     * op so a self-recursive fold's loop is recoverable by
     * t2_loop_recover and its per-element call devirtualizable when
     * inlined at a caller with a statically-known fun. There is
     * deliberately NO isel lowering: build-only (see
     * erts_t2_genop_build_only below), so a fun-containing function is
     * buildable as a P1 chain callee but never standalone-installed. */
    case genop_call_fun_1:
    case genop_call_fun2_3:

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
    /* is_function2 with an immediate arity only (argument-checked in
     * the scan below, like bs_match). Decoded to the IsFunction HIR op
     * so the P1c wrapper classifier (t2_intrinsics.cpp) can see
     * guard-carrying fold wrappers like lists:foldl/3. Like CallFun
     * there is deliberately NO isel lowering: build-only. */
    case genop_is_function2_3:
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

    /* Map matching (eligibility widening, eligibility_wins.md WIN 1).
     * is_map is a plain type test; get_map_elements is additionally
     * argument-checked (get_map_elements_op_supported below) and is
     * decomposed by the builder into one read-only lookup per key.
     * Both are read-only, no alloc, no trap — fails branch to the
     * decoded fail label, never raise. */
    case genop_is_map_2:
    case genop_get_map_elements_3:

    /* GC BIFs (generic arithmetic lowers to these). */
    case genop_gc_bif1_5:
    case genop_gc_bif2_6:
    case genop_gc_bif3_7:

    /* Clause-failure exits (modelled as error-exit blocks). badrecord
     * rides the same error-exit machinery as badmatch/case_end (WIN 4). */
    case genop_badmatch_1:
    case genop_if_end_0:
    case genop_case_end_1:
    case genop_badrecord_1:

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

/* The build-only subset of the supported set above: ops the SSA
 * builder decodes -- so P1 can build a chain callee (a fold loop's
 * CallFun, a wrapper's IsFunction guard) and erase the op when it
 * specializes the caller -- but that have no isel lowering. A function
 * containing one must stay buildable, yet must NOT be
 * standalone-installable: its blob would still hold the op and degrade
 * to T1 at isel, wasting the compile. The eligibility scan folds this
 * into the install bitmap; the (permissive) eligible bitmap keeps
 * gating what may be built. */
int erts_t2_genop_build_only(int genop) {
    switch (genop) {
    case genop_call_fun_1:
    case genop_call_fun2_3:
    case genop_is_function2_3:
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

/* Value-producing comparison bif2 (P2 commit 8): `bif2 {f,0}
 * erlang:CMP/2 A B D` where CMP is a total (never-failing) term
 * comparison the backend lowers via T1's bif_is_* emitters. '=='/'/='
 * (arith equality) are excluded — T1 routes them through the generic
 * i_bif2 C call, which needs a T1 PC a blob does not have. */
/* is_function2 Fail Src Arity: only the guard shape with a register
 * source and an immediate small arity is decoded (the T2 builder puts
 * the arity in the IsFunction op's index field). A register/abstract
 * arity keeps the function T1. */
static int is_function2_op_supported(const BeamOp *op) {
    if (op->arity < 3 || op->a[0].type != TAG_f ||
        (op->a[1].type != TAG_x && op->a[1].type != TAG_y) ||
        (op->a[2].type != TAG_i && op->a[2].type != TAG_u)) {
        return 0;
    }
    return op->a[2].val <= MAX_ARG;
}

/* get_map_elements Fail Src N K1 D1 ... (the {list,...} expanded):
 * only the register-source, real-fail-label shape with X/Y
 * destinations is decoded. Keys may be any constant, or a register
 * in the single-pair shape (the loader itself rejects multi-pair
 * register keys, beam_load_map_key_sort). Mirrored 1:1 by the
 * builder's decode — a mismatch there is scan/builder drift. */
static int get_map_elements_op_supported(const BeamOp *op) {
    UWord count;
    UWord i;

    if (op->arity < 5 || op->a[0].type != TAG_f ||
        (op->a[1].type != TAG_x && op->a[1].type != TAG_y) ||
        op->a[2].type != TAG_u) {
        return 0;
    }
    count = op->a[2].val;
    if (count < 2 || (count % 2) != 0 || (UWord)op->arity != 3 + count) {
        return 0;
    }
    for (i = 0; i < count / 2; i++) {
        const BeamOpArg *k = &op->a[3 + 2 * i];
        const BeamOpArg *d = &op->a[3 + 2 * i + 1];

        switch (k->type) {
        case TAG_q:
            if ((SWord)k->val < 0) {
                /* A dynamic literal (bignum synthesized by the decode):
                 * not retained, so the blob could never embed it —
                 * isel would reject the compile (safe_literal_term).
                 * Filter it here instead. */
                return 0;
            }
            break;
        case TAG_a:
        case TAG_i:
        case TAG_n:
            break;
        case TAG_x:
        case TAG_y:
            /* A register key only occurs in the single-pair shape. */
            if (count != 2) {
                return 0;
            }
            break;
        default:
            return 0;
        }
        if (d->type != TAG_x && d->type != TAG_y) {
            return 0;
        }
    }
    return 1;
}

static int t2_bif2_op_supported(BeamFile *beam, const BeamOp *op) {
    const BeamFile_ImportEntry *e;

    /* The decoder normalizes a zero fail label to TAG_p. */
    if (op->arity < 5 || op->a[0].type != TAG_p || op->a[1].type != TAG_u ||
        op->a[1].val >= (UWord)beam->imports.count) {
        return 0;
    }
    e = &beam->imports.entries[op->a[1].val];
    if (e->module != am_erlang || e->arity != 2) {
        return 0;
    }
    return e->function == am_Ge || e->function == am_Lt ||
           e->function == am_Le || e->function == am_Gt ||
           e->function == am_Eq || e->function == am_Neq;
}

/* A guard-BIF source in the shapes the builder decodes: a register,
 * or (when the operand's T1 instruction accepts any `s`) an
 * embeddable constant — immediates, or a *static* literal; dynamic
 * literals (bignums synthesized by the decode) are not retained, so
 * the blob could never embed them (same rule as get_map_elements
 * keys above). */
static int t2_guard_bif_src_ok(const BeamOpArg *a, int reg_only) {
    switch (a->type) {
    case TAG_x:
    case TAG_y:
        return 1;
    case TAG_q:
        return !reg_only && (SWord)a->val >= 0;
    case TAG_a:
    case TAG_i:
    case TAG_n:
        return !reg_only;
    default:
        return 0;
    }
}

/* The read-only guard-BIF subset (eligibility_wins.md WIN 3): bif1/
 * bif2 to erlang:hd/1, tl/1, node/1, element/2, map_get/2 and
 * is_map_key/2. All are read-only, no alloc, no trap; a real fail
 * label branches (clause select) and a {f,0} fail side-exits to the
 * op's own T1 EFFECT site, which re-executes and raises (T2 never
 * raises). Admitted shapes are mirrored 1:1 by the builder's decode
 * (t2_hir_builder.cpp) and by the pctab's decode-side classification
 * (pctab_guard_bif_effect in t2_pctab.c) — a mismatch there is
 * scan/builder drift. The map/list/node operand must be a register
 * (the dedicated T1 instructions' own ops.tab constraint). */
static int t2_guard_bif_op_supported(BeamFile *beam, const BeamOp *op) {
    const BeamFile_ImportEntry *e;
    const BeamOpArg *fail, *dst;

    if (op->op == genop_bif1_4) {
        if (op->arity < 4) {
            return 0;
        }
        dst = &op->a[3];
    } else if (op->op == genop_bif2_5) {
        if (op->arity < 5) {
            return 0;
        }
        dst = &op->a[4];
    } else {
        return 0;
    }
    fail = &op->a[0];
    if ((fail->type != TAG_f && fail->type != TAG_p) ||
        op->a[1].type != TAG_u ||
        op->a[1].val >= (UWord)beam->imports.count ||
        (dst->type != TAG_x && dst->type != TAG_y)) {
        return 0;
    }
    e = &beam->imports.entries[op->a[1].val];
    if (e->module != am_erlang) {
        return 0;
    }
    if (op->op == genop_bif1_4) {
        if (e->arity != 1) {
            return 0;
        }
        if (e->function == am_hd || e->function == am_tl ||
            e->function == am_node) {
            /* Register source only: real-fail hd/tl decompose into
             * is_nonempty_list + get_hd/get_tl (the loader's own
             * transform), and bif_node's S operand is a register. */
            return t2_guard_bif_src_ok(&op->a[2], 1);
        }
        return 0;
    }
    if (e->arity != 2) {
        return 0;
    }
    if (e->function == am_element) {
        return t2_guard_bif_src_ok(&op->a[2], 0) &&
               t2_guard_bif_src_ok(&op->a[3], 0);
    }
    if (e->function == am_map_get || e->function == am_is_map_key) {
        /* Any embeddable key; the map must be a register (ops.tab
         * Src2=xy -> bif_map_get / bif_is_map_key). */
        return t2_guard_bif_src_ok(&op->a[2], 0) &&
               t2_guard_bif_src_ok(&op->a[3], 1);
    }
    return 0;
}

/* gc_bifs the backend can actually lower: the arithmetic kinds
 * (bif_kind in t2_hir_builder.cpp) plus the read-only guard-BIF
 * subset carried by gc_bif1 (map_size/byte_size/bit_size — T1
 * itself discards Live for these, they never GC). Anything else
 * (length/1 traps mid-op; abs/1 & co. lower to the generic i_bif
 * runtime call) still *builds* (translate_gc_bif's T2OpKind::Bif) as
 * a P1-chain callee, but has no isel lowering — keeping it in the
 * install set was a latent installable-but-fails-isel gap. */
static int t2_gc_bif_inst_supported(BeamFile *beam, const BeamOp *op) {
    const BeamFile_ImportEntry *e;

    if (op->arity < 3 || op->a[2].type != TAG_u ||
        op->a[2].val >= (UWord)beam->imports.count) {
        return 0;
    }
    e = &beam->imports.entries[op->a[2].val];
    if (e->module != am_erlang) {
        return 0;
    }
    if (e->arity == 2) {
        return e->function == am_Plus || e->function == am_Minus ||
               e->function == am_Times || e->function == am_div ||
               e->function == am_rem || e->function == am_band ||
               e->function == am_bor || e->function == am_bxor ||
               e->function == am_bsl || e->function == am_bsr;
    }
    if (e->arity == 1) {
        return e->function == am_Minus || e->function == am_bnot ||
               e->function == am_map_size || e->function == am_byte_size ||
               e->function == am_bit_size;
    }
    return 0;
}

Uint32 *erts_t2_eligibility_scan(BeamFile *beam,
                                 int *any_eligible,
                                 Uint32 **install_bitmap_out,
                                 Uint32 **loop_bitmap_out,
                                 int *on_load_out,
                                 Uint32 *arities_out,
                                 Uint32 *sizes_out) {
    BeamOpAllocator op_alloc;
    BeamCodeReader *reader;
    BeamOp *op;

    Uint32 *bitmap;
    Uint32 *installs = NULL;
    Uint32 *loops = NULL;
    size_t bitmap_words;
    int fn_idx = -1;
    int fn_ok = 0;
    int fn_inst = 0;
    int fn_loop = 0;
    Uint32 fn_size = 0;
    int expect_entry = 0;
    UWord entry_label = 0;
    int done = 0;

    *any_eligible = 0;
    if (install_bitmap_out != NULL) {
        *install_bitmap_out = NULL;
    }
    if (loop_bitmap_out != NULL) {
        *loop_bitmap_out = NULL;
    }
    if (on_load_out != NULL) {
        *on_load_out = 0;
    }

    if (beam->code.function_count == 0) {
        return NULL;
    }

    bitmap_words = (beam->code.function_count + 31) / 32;
    bitmap = erts_alloc(ERTS_ALC_T_T2_CODE, bitmap_words * sizeof(Uint32));
    sys_memset(bitmap, 0, bitmap_words * sizeof(Uint32));
    if (install_bitmap_out != NULL) {
        installs =
                erts_alloc(ERTS_ALC_T_T2_CODE, bitmap_words * sizeof(Uint32));
        sys_memset(installs, 0, bitmap_words * sizeof(Uint32));
        *install_bitmap_out = installs;
    }
    if (loop_bitmap_out != NULL) {
        loops = erts_alloc(ERTS_ALC_T_T2_CODE, bitmap_words * sizeof(Uint32));
        sys_memset(loops, 0, bitmap_words * sizeof(Uint32));
        *loop_bitmap_out = loops;
    }

    beamopallocator_init(&op_alloc);
    reader = beamfile_get_code(beam, &op_alloc);

    while (!done && beamcodereader_next(reader, &op)) {
        switch (op->op) {
        case genop_int_func_start_5:
            fn_idx++;
            fn_ok = 1;
            fn_inst = 1;
            fn_loop = 0;
            fn_size = 0;
            expect_entry = 1;
            entry_label = 0;
            if (arities_out != NULL && fn_idx >= 0 &&
                fn_idx < beam->code.function_count) {
                arities_out[fn_idx] = (Uint32)op->a[4].val;
            }
            break;
        case genop_int_func_end_2:
            if (fn_idx >= 0 && fn_idx < beam->code.function_count) {
                if (sizes_out != NULL) {
                    sizes_out[fn_idx] = fn_size;
                }
                if (fn_ok) {
                    bitmap[fn_idx / 32] |= ((Uint32)1) << (fn_idx % 32);
                    *any_eligible = 1;
                }
                if (installs != NULL && fn_ok && fn_inst) {
                    installs[fn_idx / 32] |= ((Uint32)1) << (fn_idx % 32);
                }
                if (loops != NULL && fn_loop) {
                    loops[fn_idx / 32] |= ((Uint32)1) << (fn_idx % 32);
                }
            }
            break;
        case genop_int_code_end_0:
            done = 1;
            break;
        case genop_on_load_0:
            if (on_load_out != NULL) {
                *on_load_out = 1;
            }
            break;
        case genop_label_1:
            if (expect_entry) {
                /* The first label after int_func_start is the entry
                 * label callers (and self tail calls) target. */
                entry_label = op->a[0].val;
                expect_entry = 0;
            }
            break;
        case genop_call_only_2:
        case genop_call_last_3:
            /* A local self-recursive tail call: the loop shape the
             * tier-up counter profiles (PLAN/T2/08 §3 class 1). */
            if (op->a[1].type == TAG_f && op->a[1].val == entry_label &&
                entry_label != 0) {
                fn_loop = 1;
            }
            break;
        default:
            /* The threshold formula's size term (05 Â§15.1): count
             * the function's generic ops. */
            fn_size++;
            if (fn_ok && op->op == genop_bif2_5) {
                /* Comparison subset (t2_bif2_op_supported) or the
                 * guard-BIF subset (WIN 3). */
                if (!t2_bif2_op_supported(beam, op) &&
                    !t2_guard_bif_op_supported(beam, op)) {
                    fn_ok = 0;
                }
            } else if (fn_ok && op->op == genop_bif1_4) {
                /* Guard-BIF subset only (WIN 3). */
                if (!t2_guard_bif_op_supported(beam, op)) {
                    fn_ok = 0;
                }
            } else if (fn_ok && !erts_t2_genop_supported(op->op)) {
                fn_ok = 0;
            } else if (fn_ok && op->op == genop_is_function2_3 &&
                       !is_function2_op_supported(op)) {
                /* Register/abstract arity: outside the decoded shape. */
                fn_ok = 0;
            } else if (fn_ok && op->op == genop_bs_match_3 &&
                       !bs_match_op_supported(beam, op)) {
                /* Byte-aligned subset only: a bit-unaligned or
                 * multi-destination command list stays T1, decided
                 * here at the scan (PLAN/T2FULL/09 §7 surprise 4). */
                fn_ok = 0;
            } else if (fn_ok && op->op == genop_get_map_elements_3 &&
                       !get_map_elements_op_supported(op)) {
                /* Constant source / malformed shape: outside the
                 * decoded subset, stays T1. */
                fn_ok = 0;
            }
            if (fn_inst && erts_t2_genop_build_only(op->op)) {
                /* Buildable as a P1 chain callee, but not
                 * standalone-installable (no isel lowering). */
                fn_inst = 0;
            }
            if (fn_inst &&
                (op->op == genop_gc_bif1_5 || op->op == genop_gc_bif2_6 ||
                 op->op == genop_gc_bif3_7) &&
                !t2_gc_bif_inst_supported(beam, op)) {
                /* Same build-only treatment for non-lowerable gc_bif
                 * targets (see t2_gc_bif_inst_supported). */
                fn_inst = 0;
            }
            break;
        }

        beamopallocator_free_op(&op_alloc, op);
    }

    beamcodereader_close(reader);
    beamopallocator_dtor(&op_alloc);

    return bitmap;
}

/* ==================================================================== *
 * The addressable-share census (PLAN/T2FULL/17 §3 + 19 §2 S0).          *
 *                                                                       *
 * A measurement-only sibling of erts_t2_eligibility_scan: the same walk *
 * and the same oracle, but where the eligibility scan drops a function  *
 * on its first unsupported op, the census records the whole *set* of    *
 * blocking classes a function trips, and splits each blocker in-loop vs *
 * out-of-loop by the terminator of the basic block it sits in.          *
 * ==================================================================== */

/* Bucket one generic op into a blocker class. Only meaningful for ops the
 * oracle rejects; the caller asks only once an op is known unsupported. */
int erts_t2_blocker_class(BeamFile *beam, const BeamOp *op) {
    (void)beam;
    switch (op->op) {
    case genop_call_fun_1:
    case genop_call_fun2_3:
    case genop_apply_1:
    case genop_apply_last_2:
        return ERTS_T2_BLK_CALL_FUN;

    /* is_map and the supported get_map_elements shapes moved to the
     * supported set (WIN 1); a *rejecting* get_map_elements (constant
     * source / malformed) still lands here, like bs_match below. */
    case genop_get_map_elements_3:
    case genop_put_map_assoc_5:
    case genop_put_map_exact_5:
    case genop_has_map_fields_3:
        return ERTS_T2_BLK_MAPS;

    case genop_bs_create_bin_6:
    case genop_bs_init_writable_0:
        return ERTS_T2_BLK_BS_CONSTRUCTION;

    /* General bit matching + match-context position ops. A rejecting
     * bs_match/3 (outside the byte-aligned subset) lands here too. */
    case genop_bs_match_3:
    case genop_bs_get_integer2_7:
    case genop_bs_get_float2_7:
    case genop_bs_get_binary2_7:
    case genop_bs_skip_bits2_5:
    case genop_bs_test_unit_3:
    case genop_bs_match_string_4:
    case genop_bs_get_utf8_5:
    case genop_bs_skip_utf8_4:
    case genop_bs_get_utf16_5:
    case genop_bs_skip_utf16_4:
    case genop_bs_get_utf32_5:
    case genop_bs_skip_utf32_4:
    case genop_bs_get_position_3:
    case genop_bs_set_position_2:
    case genop_bs_start_match4_4:
    case genop_bs_scan_5:
        return ERTS_T2_BLK_BS_POSITION;

    case genop_catch_2:
    case genop_catch_end_1:
    case genop_try_2:
    case genop_try_end_1:
    case genop_try_case_1:
    case genop_try_case_end_1:
    case genop_raise_2:
    case genop_raw_raise_0:
    case genop_build_stacktrace_0:
        return ERTS_T2_BLK_EXCEPTIONS;

    case genop_send_0:
    case genop_remove_message_0:
    case genop_timeout_0:
    case genop_loop_rec_2:
    case genop_loop_rec_end_1:
    case genop_wait_1:
    case genop_wait_timeout_2:
    case genop_recv_marker_bind_2:
    case genop_recv_marker_clear_1:
    case genop_recv_marker_reserve_1:
    case genop_recv_marker_use_1:
        return ERTS_T2_BLK_RECEIVE;

    case genop_fmove_2:
    case genop_fconv_2:
    case genop_fadd_4:
    case genop_fsub_4:
    case genop_fmul_4:
    case genop_fdiv_4:
    case genop_fnegate_3:
        return ERTS_T2_BLK_FLOAT_REG;

    case genop_bif0_2:
    case genop_bif1_4:
    case genop_bif2_5:
    case genop_bif3_6:
        return ERTS_T2_BLK_GENERAL_BIF;

    default:
        return ERTS_T2_BLK_OTHER;
    }
}

/* Is this generic op a basic-block terminator, for the placement split? */
static int census_is_terminator(int genop) {
    switch (genop) {
    case genop_return_0:
    case genop_jump_1:
    case genop_select_val_3:
    case genop_select_tuple_arity_3:
    case genop_call_only_2:
    case genop_call_last_3:
    case genop_call_ext_only_2:
    case genop_call_ext_last_3:
    case genop_apply_last_2:
    case genop_badmatch_1:
    case genop_if_end_0:
    case genop_case_end_1:
    case genop_raise_2:
    case genop_raw_raise_0:
    case genop_badrecord_1:
        return 1;
    default:
        return 0;
    }
}

/* Move the current block's buffered blockers to the in- or out-of-loop
 * tally and clear the accumulator (a no-op when the block is empty). */
static void census_flush_block(ErtsT2CensusFn *f, Uint32 *block_cnt, int out) {
    int c;
    for (c = 0; c < ERTS_T2_BLK__COUNT; c++) {
        if (out) {
            f->out_loop[c] += block_cnt[c];
        } else {
            f->in_loop[c] += block_cnt[c];
        }
        block_cnt[c] = 0;
    }
}

int erts_t2_census_scan(BeamFile *beam, ErtsT2CensusFn **out, int *count_out) {
    BeamOpAllocator op_alloc;
    BeamCodeReader *reader;
    BeamOp *op;

    ErtsT2CensusFn *fns;
    int nfns = beam->code.function_count;
    int fn_idx = -1;
    int expect_entry = 0;
    UWord entry_label = 0;
    int done = 0;
    int c;

    /* Per-basic-block blocker accumulator, distributed to in/out-loop at
     * the block terminator. The default (in-loop) is the conservative
     * choice: it never overstates the region-compilation opportunity. */
    Uint32 block_cnt[ERTS_T2_BLK__COUNT];

    *out = NULL;
    *count_out = 0;
    if (nfns <= 0) {
        return 0;
    }

    fns = erts_alloc(ERTS_ALC_T_T2_CODE, (size_t)nfns * sizeof(ErtsT2CensusFn));
    sys_memset(fns, 0, (size_t)nfns * sizeof(ErtsT2CensusFn));
    for (c = 0; c < ERTS_T2_BLK__COUNT; c++) {
        block_cnt[c] = 0;
    }

    beamopallocator_init(&op_alloc);
    reader = beamfile_get_code(beam, &op_alloc);

    while (!done && beamcodereader_next(reader, &op)) {
        int in_fn = (fn_idx >= 0 && fn_idx < nfns);

        switch (op->op) {
        case genop_int_func_start_5:
            /* A body always ends in a terminator, so block_cnt is
             * normally already zero here; flush defensively (in-loop). */
            if (in_fn) {
                census_flush_block(&fns[fn_idx], block_cnt, 0);
            }
            fn_idx++;
            expect_entry = 1;
            entry_label = 0;
            if (fn_idx >= 0 && fn_idx < nfns) {
                fns[fn_idx].name = (Eterm)op->a[3].val;
                fns[fn_idx].arity = (Uint32)op->a[4].val;
                fns[fn_idx].eligible = 1;
            }
            break;

        case genop_int_func_end_2:
            if (in_fn) {
                census_flush_block(&fns[fn_idx], block_cnt, 0);
            }
            break;

        case genop_int_code_end_0:
            done = 1;
            break;

        case genop_label_1:
            if (expect_entry) {
                entry_label = op->a[0].val;
                expect_entry = 0;
            }
            /* A label starts a new block; any un-terminated fall-through
             * blockers are flushed conservatively as in-loop. */
            if (in_fn) {
                census_flush_block(&fns[fn_idx], block_cnt, 0);
            }
            break;

        default:
            if (in_fn) {
                ErtsT2CensusFn *f = &fns[fn_idx];
                int supported;

                f->size++;

                /* The exact 3-way eligibility predicate (kept in lockstep
                 * with erts_t2_eligibility_scan). */
                if (op->op == genop_bif2_5) {
                    supported = t2_bif2_op_supported(beam, op) ||
                                t2_guard_bif_op_supported(beam, op);
                } else if (op->op == genop_bif1_4) {
                    supported = t2_guard_bif_op_supported(beam, op);
                } else if (op->op == genop_bs_match_3) {
                    supported = bs_match_op_supported(beam, op);
                } else if (op->op == genop_get_map_elements_3) {
                    supported = get_map_elements_op_supported(op);
                } else {
                    supported = erts_t2_genop_supported(op->op);
                }

                if (!supported) {
                    int cls = erts_t2_blocker_class(beam, op);
                    f->eligible = 0;
                    f->total[cls]++;
                    block_cnt[cls]++;
                }
            }
            break;
        }

        /* Terminator: pick a placement and flush the block. Calls and
         * returns pass through `default` first (size/blocker accounting),
         * so a rejecting terminator (raise/badrecord) is counted before
         * it is flushed here. */
        if (in_fn && census_is_terminator(op->op)) {
            int placement_out;

            if (op->op == genop_return_0 || op->op == genop_call_ext_only_2 ||
                op->op == genop_call_ext_last_3 ||
                op->op == genop_apply_last_2 || op->op == genop_badmatch_1 ||
                op->op == genop_if_end_0 || op->op == genop_case_end_1 ||
                op->op == genop_raise_2 || op->op == genop_raw_raise_0 ||
                op->op == genop_badrecord_1) {
                /* Returns, tail calls away, and cold error exits leave the
                 * function's loop. */
                placement_out = 1;
            } else if (op->op == genop_call_only_2 ||
                       op->op == genop_call_last_3) {
                if (op->a[1].type == TAG_f && op->a[1].val == entry_label &&
                    entry_label != 0) {
                    fns[fn_idx].loop_shaped = 1;
                    placement_out = 0; /* the recursion edge: in-loop */
                } else {
                    placement_out = 1; /* tail call away: leaves the loop */
                }
            } else {
                /* jump / select: intra-function control flow, kept in-loop
                 * (conservative). */
                placement_out = 0;
            }

            census_flush_block(&fns[fn_idx], block_cnt, placement_out);
        }

        beamopallocator_free_op(&op_alloc, op);
    }

    beamcodereader_close(reader);
    beamopallocator_dtor(&op_alloc);

    *out = fns;
    *count_out = nfns;
    return 1;
}
