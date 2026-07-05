/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2020-2025. All Rights Reserved.
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

#include <algorithm>
#include <cstring>
#include <sstream>
#include <float.h>

#include "beam_asm.hpp"
extern "C"
{
#include "beam_bp.h"
#include "erl_bits.h"
#include "erl_map.h"
#include "t2_pctab.h" /* ErtsT2PcKind constants for the PC side table */
}

using namespace asmjit;

#ifdef BEAMASM_DUMP_SIZES
#    include <mutex>

typedef std::pair<Uint64, Uint64> op_stats;

static std::unordered_map<char *, op_stats> sizes;
static std::mutex size_lock;

extern "C" void beamasm_dump_sizes() {
    std::lock_guard<std::mutex> lock(size_lock);

    std::vector<std::pair<char *, op_stats>> flat(sizes.cbegin(), sizes.cend());
    double total_size = 0.0;

    for (const auto &op : flat) {
        total_size += op.second.second;
    }

    /* Sort instructions by total size, in descending order. */
    std::sort(
            flat.begin(),
            flat.end(),
            [](std::pair<char *, op_stats> &a, std::pair<char *, op_stats> &b) {
                return a.second.second > b.second.second;
            });

    for (const auto &op : flat) {
        fprintf(stderr,
                "%34s:\t%zu\t%f\t%zu\t%zu\r\n",
                op.first,
                op.second.second,
                op.second.second / total_size,
                op.second.first,
                op.second.first ? (op.second.second / op.second.first) : 0);
    }
}
#endif

ErtsCodePtr BeamModuleAssembler::getCode(BeamLabel label) {
    ASSERT(label < rawLabels.size() + 1);
    return (ErtsCodePtr)getCode(rawLabels[label]);
}

ErtsCodePtr BeamModuleAssembler::getLambda(unsigned index) {
    const auto &lambda = lambdas[index];
    return (ErtsCodePtr)getCode(lambda.trampoline);
}

BeamModuleAssembler::BeamModuleAssembler(BeamGlobalAssembler *ga,
                                         Eterm mod,
                                         int num_labels,
                                         int num_functions,
                                         const BeamFile *file)
        : BeamModuleAssembler(ga, mod, num_labels, file) {
    _veneers.reserve(num_labels + 1);

    code_header = a.new_label();
    a.align(AlignMode::kCode, 8);
    a.bind(code_header);

    embed_zeros(sizeof(BeamCodeHeader) +
                sizeof(ErtsCodeInfo *) * num_functions);

#ifdef DEBUG
    last_stub_check_offset = a.offset();
#endif
}

void BeamModuleAssembler::embed_vararg_rodata(const Span<const ArgVal> &args,
                                              a64::Gp reg) {
    /* Short sequences are inlined in the .text section for slightly better
     * speed. */
    bool inlineData = args.size() <= 6;

    Label data = a.new_label(), next = a.new_label();

    if (inlineData) {
        a.adr(reg, data);
        a.b(next);
    } else {
        a.ldr(reg, embed_label(data, disp32K));

        a.section(rodata);
    }

    a.align(AlignMode::kData, 8);
    a.bind(data);

    for (const ArgVal &arg : args) {
        a.align(AlignMode::kData, 8);
        switch (arg.getType()) {
        case ArgVal::Type::Literal: {
            auto &patches = literals[arg.as<ArgLiteral>().get()].patches;
            Label patch = a.new_label();

            a.bind(patch);
            a.embed_uint64(LLONG_MAX);
            patches.push_back({patch, 0});
            break;
        }
        case ArgVal::Type::XReg:
            a.embed_uint64(make_loader_x_reg(arg.as<ArgXRegister>().get()));
            break;
        case ArgVal::Type::YReg:
            a.embed_uint64(make_loader_y_reg(arg.as<ArgYRegister>().get()));
            break;
        case ArgVal::Type::Label:
            a.embed_label(rawLabels[arg.as<ArgLabel>().get()]);
            break;
        case ArgVal::Type::Immediate:
            a.embed_uint64(arg.as<ArgImmed>().get());
            break;
        case ArgVal::Type::Word:
            a.embed_uint64(arg.as<ArgWord>().get());
            break;
        default:
            ERTS_ASSERT(!"error");
        }
    }

    if (!inlineData) {
        a.section(code.text_section());
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_i_nif_padding() {
    const size_t minimum_size = sizeof(UWord[BEAM_NATIVE_MIN_FUNC_SZ]);
    size_t prev_func_start, diff;

    prev_func_start =
            code.label_offset_from_base(rawLabels[functions.back() + 1]);
    diff = a.offset() - prev_func_start;

    if (diff < minimum_size) {
        embed_zeros(minimum_size - diff);
    }
}

void BeamGlobalAssembler::emit_i_breakpoint_trampoline_shared() {
    constexpr ssize_t flag_offset =
            sizeof(ErtsCodeInfo) + BEAM_ASM_FUNC_PROLOGUE_SIZE -
            offsetof(ErtsCodeInfo, u.metadata.breakpoint_flag);

    Label bp_and_nif = a.new_label(), bp_only = a.new_label(),
          nif_only = a.new_label();

    a.ldrb(ARG1.w(), a64::Mem(a64::x30, -flag_offset));

    a.cmp(ARG1, imm(ERTS_ASM_BP_FLAG_BP_NIF_CALL_NIF_EARLY));
    a.b_eq(bp_and_nif);
    ERTS_CT_ASSERT((1 << 0) == ERTS_ASM_BP_FLAG_CALL_NIF_EARLY);
    a.tbnz(ARG1, imm(0), nif_only);
    ERTS_CT_ASSERT((1 << 1) == ERTS_ASM_BP_FLAG_BP);
    a.tbnz(ARG1, imm(1), bp_only);

#ifndef DEBUG
    a.ret(a64::x30);
#else
    Label error = a.new_label();

    /* ARG1 must be a valid breakpoint flag. */
    a.cbnz(ARG1, error);
    a.ret(a64::x30);

    a.bind(error);
    a.udf(0xBC0D);
#endif

    a.bind(bp_and_nif);
    {
        emit_enter_runtime_frame();
        a.bl(labels[generic_bp_local]);
        emit_leave_runtime_frame();

        /* !! FALL THROUGH !! */
    }

    a.bind(nif_only);
    {
        /* call_nif_early returns on its own, unlike generic_bp_local. */
        a.b(labels[call_nif_early]);
    }

    a.bind(bp_only);
    {
        emit_enter_runtime_frame();
        a.bl(labels[generic_bp_local]);
        emit_leave_runtime_frame();

        a.ret(a64::x30);
    }
}

/* MVP T2 hook: hardcoded list of (Mod, Fun, Arity) tuples that get T2
 * dispatch injected at function entry. Real implementation will read
 * the t2_assume_smallints attribute from the BEAM file; for the MVP we
 * recognise only the demo benchmark functions.
 *
 * Defined as a member function so per-arch hooks (currently
 * emit_i_test_yield in arm/instr_common.cpp) can reach it via the
 * usual `this->` lookup. */
bool BeamModuleAssembler::t2_mvp_is_target() const {
    static const struct {
        const char *mod;
        const char *fun;
        unsigned arity;
    } targets[] = {
            {"t2_mvp", "total", 2},
            {"t2_mvp", "diff", 2},
    };

    if (!is_atom(mod) || !is_atom(current_function)) {
        return false;
    }

    for (const auto &t : targets) {
        if (erts_is_atom_str(t.mod, mod, 0) &&
            erts_is_atom_str(t.fun, current_function, 0) &&
            t.arity == current_arity) {
            return true;
        }
    }

    /* G3 experiment target, gated on the T2_G3 env var so a single
     * build provides the T1 baseline (off) and both stages. */
    if (t2_g3_mode() != 0 && current_arity == 2 &&
        erts_is_atom_str("erl_types", mod, 0) &&
        erts_is_atom_str("are_all_limited", current_function, 0)) {
        return true;
    }

    /* G-bin experiment targets, gated on T2_GBIN. */
    if (t2_gbin_enabled() && current_arity == 7 &&
        erts_is_atom_str("json", mod, 0) &&
        (erts_is_atom_str("number", current_function, 0) ||
         erts_is_atom_str("number_frac_cont", current_function, 0) ||
         erts_is_atom_str("string_ascii", current_function, 0))) {
        return true;
    }

    /* G3 subject 1 targets, gated on T2_G31. */
    if (t2_g31_enabled() && erts_is_atom_str("t2_g31", mod, 0) &&
        ((current_arity == 2 &&
          erts_is_atom_str("dispatch", current_function, 0)) ||
         (current_arity == 3 &&
          erts_is_atom_str("handle_call", current_function, 0)))) {
        return true;
    }

    /* G-map target, gated on T2_GMAP. */
    if (t2_gmap_enabled() && current_arity == 2 &&
        erts_is_atom_str("t2_gmap", mod, 0) &&
        erts_is_atom_str("sum_scores", current_function, 0)) {
        return true;
    }

    return false;
}

bool BeamModuleAssembler::t2_g31_enabled() const {
    static bool enabled = []() {
        const char *env = getenv("T2_G31");
        return env != nullptr && env[0] == '1';
    }();
    return enabled;
}

bool BeamModuleAssembler::t2_gmap_enabled() const {
    static bool enabled = []() {
        const char *env = getenv("T2_GMAP");
        return env != nullptr && env[0] == '1';
    }();
    return enabled;
}

bool BeamModuleAssembler::t2_gbin_enabled() const {
    static bool enabled = []() {
        const char *env = getenv("T2_GBIN");
        return env != nullptr && env[0] == '1';
    }();
    return enabled;
}

int BeamModuleAssembler::t2_g3_mode() const {
    static int mode = []() {
        const char *env = getenv("T2_G3");
        if (env == nullptr) {
            return 0;
        } else if (env[0] == 'a') {
            return 1;
        } else if (env[0] == 'b') {
            return 2;
        }
        return 0;
    }();
    return mode;
}

void BeamModuleAssembler::emit_i_breakpoint_trampoline() {
    /* This little prologue is used by nif loading and tracing to insert
     * alternative instructions. */
    Label next = a.new_label();

    emit_enter_erlang_frame();

    /* This branch is modified to jump to the BL instruction when the
     * breakpoint is enabled. */
    a.b(next);

    if (code_header.is_valid()) {
        a.bl(resolve_fragment(ga->get_i_breakpoint_trampoline_shared(),
                              disp128MB));
    } else {
        /* NIF or BIF stub; we're not going to use this trampoline as-is, but
         * we need to reserve space for it. */
        a.udf(0xB1F);
    }

    a.bind(next);

    ASSERT((a.offset() - code.label_offset_from_base(current_label)) ==
           BEAM_ASM_FUNC_PROLOGUE_SIZE);

    /* MVP T2 hook is emitted in emit_i_test_yield (which follows the
     * prologue) rather than here, because the runtime yield-resume
     * code (i_test_yield_shared) computes the resume PC as
     * `current_label + PROLOGUE_SIZE + 12`, hardcoding the assumption
     * that i_test_yield immediately follows the prologue. Any code
     * emitted here would shift that offset and corrupt yield resume. */
}

void BeamGlobalAssembler::emit_i_line_breakpoint_trampoline_shared() {
    Label exit_trampoline = a.new_label();
    Label dealloc_and_exit_trampoline = a.new_label();
    Label after_gc_check = a.new_label();
    Label dispatch_call = a.new_label();

    const auto &saved_live = TMP_MEM1q;
    const auto &saved_pc = TMP_MEM2q;
    const auto &saved_stack_needed = TMP_MEM3q;

    emit_enter_erlang_frame();

    /* NB. TMP1 = live */
    a.str(TMP1, saved_live); /* stash live */

    /* Pass return address of trampoline, will be used to find current function
     * info */
    a.sub(ARG1, a64::x30, imm(8)); /* ARG1 := pc */
    a.str(ARG1, saved_pc);         /* Stash pc */

    /* START allocate live live */
    a.mov(ARG4, TMP1);         /* ARG4 := live */
    a.lsl(TMP1, TMP1, imm(3)); /* TMP1 := stack-needed = live * sizeof(Eterm) */
    a.str(TMP1, saved_stack_needed); /* stash stack-needed */
    a.add(ARG3,
          TMP1,
          imm(S_RESERVED *
              8)); /* ARG3 := stack-needed + S_RESERVED * sizeof(Eterm) */

    a.add(ARG3, ARG3, HTOP);
    a.cmp(ARG3, E);
    a.b_ls(after_gc_check);

    /* gc needed */
    aligned_call(labels[garbage_collect]);
    a.ldr(TMP1, saved_stack_needed); /* TMP1 := (stashed) stack-needed */
    a.bind(after_gc_check);

    a.sub(E, E, TMP1);
    /* END allocate live live */

    a.mov(ARG1, c_p);
    a.ldr(ARG2, saved_pc); /* pc */
    a.ldr(ARG3, saved_live);
    load_x_reg_array(ARG4);
    a.mov(ARG5, E); /* stk */

    emit_enter_runtime<Update::eXRegs>();
    runtime_call<
            const Export *(*)(Process *, ErtsCodePtr, Uint, Eterm *, UWord *),
            erts_line_breakpoint_hit__prepare_call>();
    emit_leave_runtime<Update::eXRegs>();

    /* If non-null, ARG1 points to error_handler:breakpoint/4 */
    a.cbnz(ARG1, dispatch_call);
    a.ldr(ARG1, saved_stack_needed); /* ARG1 := (stashed) stack-needed */
    a.b(dealloc_and_exit_trampoline);

    a.bind(dispatch_call);
    erlang_call(emit_setup_dispatchable_call(ARG1));

    a.bind(labels[i_line_breakpoint_cleanup]);
    load_x_reg_array(ARG1);
    a.mov(ARG2, E); /* stk */

    emit_enter_runtime<Update::eXRegs>();
    runtime_call<Uint (*)(Eterm *, UWord *),
                 erts_line_breakpoint_hit__cleanup>();
    emit_leave_runtime<Update::eXRegs>();

    a.lsl(ARG1, ARG1, imm(3)); /* ARG1 = stack-needed */

    a.bind(dealloc_and_exit_trampoline); /* ASUMES ARG1 = stack-needed */
    a.add(E, E, ARG1);

    a.bind(exit_trampoline);
    emit_leave_erlang_frame();
    a.ret(a64::x30);
}

void BeamModuleAssembler::emit_i_line_breakpoint_trampoline() {
    /* This prologue is used to implement line-breakpoints. The "b next" can
     * be replaced by nops when the breakpoint is enabled, which will instead
     * trigger the breakpoint when control goes through here */
    Label next = a.new_label();
    a.b(next);

    a.bl(resolve_fragment(ga->get_i_line_breakpoint_trampoline_shared(),
                          disp128MB));

    a.bind(next);
}

enum erts_is_line_breakpoint BeamGlobalAssembler::is_line_breakpoint_trampoline(
        ErtsCodePtr addr) {
    auto pc = static_cast<const int32_t *>(addr);
    enum erts_is_line_breakpoint line_bp_type;

    /* The b and bl opcodes take 6 bits, the remaining 26 bits are a
     * a signed offset, given in 32-bit words. */
    const auto opcode6_mask = 0xFC000000;
    const auto b_opcode = 0x14000000;
    const auto bl_opcode = 0x94000000;

    int32_t instr = *pc;
    switch (instr) {
    /* B .next .enabled: BL breakpoint_handler, .next: */
    case b_opcode | 2:
        line_bp_type = IS_DISABLED_LINE_BP;
        break;

    /* B .enabled .enabled: BL breakpoint_handler, .next: */
    case b_opcode | 1:
        line_bp_type = IS_ENABLED_LINE_BP;
        break;

    default:
        return IS_NOT_LINE_BP;
    }

    instr = *++pc;

    /* We expect a bl here. The target is a signed 26-bit offset */
    if ((instr & opcode6_mask) != bl_opcode) {
        return IS_NOT_LINE_BP;
    }
    const int32_t bl_offset = (instr << 6) >> 6;

    /* Offset is expressed in 32-bit words, not bytes */
    pc = pc + bl_offset;

    const auto expected_target = get_i_line_breakpoint_trampoline_shared();
    if (pc == (const int32_t *)expected_target)
        return line_bp_type;

    /* Now we expect to be in a veneer, that will encode a jump
     * to the actual function based on the distance to the pc
     * This can be a direct branch if close enough or branch-to-register after
     * loading the expected_address (see emit_veneer() method) */
    instr = *pc;

    if ((instr & opcode6_mask) == b_opcode) {
        /* using relative branch when expected_target is close enough */
        const int32_t b_offset = (instr << 6) >> 6;
        return (pc + b_offset == (const int32_t *)expected_target)
                       ? line_bp_type
                       : IS_NOT_LINE_BP;
    }

    const auto super_tmp_reg = SUPER_TMP.id();
    /* we expect to see up to four MOVs into SUPER_TMP to load expected_address,
     * followed by a `br SUPER_TMP` */
    auto mov_opcode =
            0xD2800000 | super_tmp_reg; /* movz SUPER_TMP, #0, lsl #0 */

    uint64_t expected_target_addr = (uint64_t)expected_target;
    for (int32_t hw = 0; hw < 4; hw++) {
        uint32_t chunk = expected_target_addr & 0xFFFF;
        expected_target_addr >>= 16;
        if (chunk == 0)
            continue;

        if ((uint32_t)instr != (mov_opcode | (hw << 21) | (chunk << 5))) {
            return IS_NOT_LINE_BP;
        }

        instr = *++pc;
        mov_opcode =
                0xF2800000 | super_tmp_reg; /* movk SUPER_TMP, #0, lsl #0 */
    };

    const int32_t expected_br_instr =
            0xd61f0000 | (super_tmp_reg << 5); /* br SUPER_TMP */
    return (instr == expected_br_instr) ? line_bp_type : IS_NOT_LINE_BP;
}

static void i_emit_nyi(const char *msg) {
    erts_exit(ERTS_ERROR_EXIT, "NYI: %s\n", msg);
}

void BeamModuleAssembler::emit_nyi(const char *msg) {
    emit_enter_runtime(0);

    a.mov(ARG1, imm(msg));
    runtime_call<void (*)(const char *), i_emit_nyi>();

    /* Never returns */
}

void BeamModuleAssembler::emit_nyi() {
    emit_nyi("<unspecified>");
}

bool BeamModuleAssembler::emit(unsigned specific_op,
                               const Span<const ArgVal> &args) {
    check_pending_stubs();

#ifdef BEAMASM_DUMP_SIZES
    size_t before = a.offset();
#endif

    /* T2-Full P0 (PLAN/T2FULL/07 §4): bracket the op's emission to record
     * T1 re-entry offsets (filtered to eligible functions at retain-commit). */
    bool t2_pc_elig = t2_pc_collecting();
    uint32_t t2_pc_before = t2_pc_elig ? (uint32_t)a.offset() : 0;

    comment(opc[specific_op].name);

#define InstrCnt()
    switch (specific_op) {
#include "beamasm_emit.h"
    default:
        ERTS_ASSERT(0 && "Invalid instruction");
        break;
    }

    if (t2_pc_elig) {
        t2_pc_classify(specific_op, t2_pc_before, (uint32_t)a.offset());
    }

#ifdef BEAMASM_DUMP_SIZES
    {
        std::lock_guard<std::mutex> lock(size_lock);

        sizes[opc[specific_op].name].first++;
        sizes[opc[specific_op].name].second += a.offset() - before;
    }
#endif

    return true;
}

/* Map a just-emitted specific op to a T1 re-entry kind and record its
 * offset (PLAN/T2FULL/07 §4). Only the four re-entry kinds are recorded;
 * everything else is ignored. See t2_pctab.h for how these offsets are
 * later paired with SSA decode ordinals. */
void BeamModuleAssembler::t2_pc_classify(unsigned specific_op,
                                         uint32_t before,
                                         uint32_t after) {
    switch (specific_op) {
    case op_i_test_yield:
        /* Function entry: the SSA builder's beam_idx 0. */
        t2_pc_record(before, ERTS_T2_PC_ENTRY);
        break;

    /* Non-tail calls: the call site, plus the continuation the callee
     * returns to. */
    case op_i_call_f:
    case op_i_call_ext_e:
        t2_pc_record(before, ERTS_T2_PC_CALL);
        t2_pc_record(after, ERTS_T2_PC_CONT);
        break;

    /* Tail calls (including the move-fused variants): call site only.
     * These specific ops are emitted only for local calls and call_ext to
     * non-BIF targets; call_ext to a BIF lowers to a bif dispatch instead,
     * which the decode side likewise excludes from its call count so the
     * zip stays aligned (see pctab_genop_is_call in t2_pctab.c). */
    case op_i_call_last_ft:
    case op_i_call_only_f:
    case op_i_call_ext_last_et:
    case op_i_call_ext_only_e:
    case op_move_call_last_ydft:
    case op_move_call_ext_last_ydet:
        t2_pc_record(before, ERTS_T2_PC_CALL);
        break;

    /* Post-BIF/effect boundaries: the gc_bif-lowered arithmetic ops and
     * the generic i_bif fallbacks. Recorded at the op's site (`before`) so
     * the offset stays distinct from the following op's site — a
     * post-effect offset would coincide exactly with an immediately
     * adjacent call site (both name the same instruction boundary). */
    case op_i_plus_jIssd:
    case op_i_minus_jIssd:
    case op_i_mul_add_jssssd:
    case op_i_unary_minus_jIsd:
    case op_i_m_div_jIssd:
    case op_i_int_div_jIssd:
    case op_i_rem_jIssd:
    case op_i_band_jIssd:
    case op_i_bor_jIssd:
    case op_i_bxor_jIssd:
    case op_i_bnot_jIsd:
    case op_i_bsr_jIssd:
    case op_i_bsl_jIssd:
    case op_i_rem_div_jIssdd:
    case op_i_div_rem_jIssdd:
    case op_i_bif1_sjbd:
    case op_i_bif2_ssjbd:
    case op_i_bif3_sssjbd:
    /* gc_bif1 to the size/length BIFs; i_length lowers to a setup + the
     * op below, so only the latter (one per source gc_bif) is counted. */
    case op_bif_bit_size_jsd:
    case op_bif_byte_size_jsd:
    case op_bif_map_size_jsd:
    case op_i_length_jtd:
        t2_pc_record(before, ERTS_T2_PC_EFFECT);
        break;

    default:
        break;
    }
}

/*
 * Here follows meta instructions.
 */

void BeamGlobalAssembler::emit_i_func_info_shared() {
    /* a64::x30 now points 4 bytes into the ErtsCodeInfo struct for the
     * function. Put the address of the MFA into ARG1. */
    a.add(ARG1, a64::x30, offsetof(ErtsCodeInfo, mfa) - 4);

    mov_imm(TMP1, EXC_FUNCTION_CLAUSE);
    a.str(TMP1, a64::Mem(c_p, offsetof(Process, freason)));
    a.str(ARG1, a64::Mem(c_p, offsetof(Process, current)));

    mov_imm(ARG2, 0);
    mov_imm(ARG4, 0);

    a.b(labels[raise_exception_shared]);
}

void BeamModuleAssembler::emit_i_func_info(const ArgWord &Label,
                                           const ArgAtom &Module,
                                           const ArgAtom &Function,
                                           const ArgWord &Arity) {
    ErtsCodeInfo info = {};

    /* `op_i_func_info_IaaI` is used in various places in the emulator, so this
     * label is always encoded as a word, even though the signature ought to
     * be `op_i_func_info_LaaI`. */
    functions.push_back(Label.get());

    info.mfa.module = Module.get();
    info.mfa.function = Function.get();
    info.mfa.arity = Arity.get();

    /* MVP T2 hook: stash MFA so emit_i_breakpoint_trampoline can decide
     * whether the function is T2-targeted. */
    current_function = info.mfa.function;
    current_arity = info.mfa.arity;

    comment("%T:%T/%d", info.mfa.module, info.mfa.function, info.mfa.arity);

    /* This is an ErtsCodeInfo structure that has a valid ARM opcode as its `op`
     * field, which *calls* the `raise_function_clause` fragment so we can trace
     * it back to this particular function.
     *
     * We also use this field to store the current breakpoint flag, as ARM is a
     * bit more strict about modifying code than x86: only branch instructions
     * can be safely modified without issuing an ISB. By storing the flag here
     * and reading it in the fragment, we don't have to change any code other
     * than the branch instruction. */
    if (code_header.is_valid()) {
        /* We avoid using the `fragment_call` helper to ensure a constant
         * layout, as it adds code in certain debug configurations. */
        a.bl(resolve_fragment(ga->get_i_func_info_shared(), disp128MB));
    } else {
        a.udf(0xF1F0);
    }

    ERTS_CT_ASSERT(ERTS_ASM_BP_FLAG_NONE == 0);
    a.embed_uint32(0);

    ASSERT(a.offset() % sizeof(UWord) == 0);
    a.embed(&info.gen_bp, sizeof(info.gen_bp));
    a.embed(&info.mfa, sizeof(info.mfa));
}

void BeamModuleAssembler::emit_label(const ArgLabel &Label) {
    ASSERT(Label.isLabel());

    current_label = rawLabels[Label.get()];
    bind_veneer_target(current_label);

    reg_cache.invalidate();
}

void BeamModuleAssembler::emit_aligned_label(const ArgLabel &Label,
                                             const ArgWord &Alignment) {
    a.align(AlignMode::kCode, Alignment.get());
    emit_label(Label);
}

void BeamModuleAssembler::emit_i_func_label(const ArgLabel &Label) {
    flush_last_error();
    emit_aligned_label(Label, ArgVal(ArgVal::Type::Word, sizeof(UWord)));
}

void BeamModuleAssembler::emit_on_load() {
    on_load = current_label;
}

void BeamModuleAssembler::bind_veneer_target(const Label &target) {
    auto veneer_range = _veneers.equal_range(target.id());
    for (auto it = veneer_range.first; it != veneer_range.second; it++) {
        const Veneer &veneer = it->second;

        ASSERT(veneer.target == target);

        if (!code.is_label_bound(veneer.anchor)) {
            ASSERT((ssize_t)a.offset() <= veneer.latestOffset);
            a.bind(veneer.anchor);

            /* TODO: remove from pending stubs? */
        }
    }

    a.bind(target);
}

void BeamModuleAssembler::emit_int_code_end() {
    /* MVP T2: emit specialized bodies for every function the hook
     * registered. Must happen *before* code_end because the dispatch
     * table that follows assumes everything past it is unreachable. */
    emit_t2_specializations();

    /* This label is used to figure out the end of the last function */
    code_end = a.new_label();
    a.bind(code_end);

    emit_nyi("int_code_end");

    /* We emit the dispatch table before all remaining stubs to bind veneers
     * directly in the table itself, avoiding a painful extra jump.
     *
     * Since the table is potentially very large, we'll emit all stubs that are
     * due within it so we won't have to check on every iteration. */
    mark_unreachable();
    flush_pending_stubs(_dispatchTable.size() * sizeof(Uint32[8]) +
                        dispUnknown);

    for (auto pair : _dispatchTable) {
        bind_veneer_target(pair.second);

        a.mov(SUPER_TMP, imm(pair.first));
        a.br(SUPER_TMP);
    }

    mark_unreachable();

    /* Emit all remaining stubs. */
    flush_pending_stubs(dispMax);
}

void BeamModuleAssembler::emit_line(const ArgWord &Loc) {
    /* There is no need to align the line instruction. In the loaded code, the
     * type of the pointer will be void* and that pointer will only be used in
     * comparisons. */

    flush_last_error();
}

/* ============================================================
 * MVP T2 specialized codegen.
 *
 * For each function the hook in emit_i_test_yield registered, we
 * emit a hand-coded specialized body here. Functions that don't have
 * a specialization fall back to T1 immediately (b side_exit).
 *
 * The specialized bodies live between the last function's body and
 * the dispatch table at code_end. Callers reach them via
 *   public_label -> prologue -> i_test_yield -> b t2_entry
 * and side-exit back via
 *   t2_entry -> ... guard fail ... -> b side_exit
 * where side_exit is bound immediately after the hook's branch, so
 * the existing T1 body emitted by the JIT becomes the fallback.
 * ============================================================ */

void BeamModuleAssembler::emit_t2_specializations() {
    if (t2_entries.empty()) {
        return;
    }

    comment("===== MVP T2 specialized bodies =====");

    for (const auto &entry : t2_entries) {
        comment("T2 body for %T:%T/%d", mod, entry.function, entry.arity);
        a.bind(entry.t2_entry);

        if (erts_is_atom_str("total", entry.function, 0) && entry.arity == 2) {
            emit_t2_total_2(entry);
        } else if (erts_is_atom_str("are_all_limited", entry.function, 0) &&
                   entry.arity == 2) {
            emit_t2_are_all_limited_2(entry);
        } else if (entry.arity == 7 &&
                   (erts_is_atom_str("number", entry.function, 0) ||
                    erts_is_atom_str("number_frac_cont", entry.function, 0))) {
            emit_t2_json_scan(entry, true);
        } else if (entry.arity == 7 &&
                   erts_is_atom_str("string_ascii", entry.function, 0)) {
            emit_t2_json_scan(entry, false);
        } else if (entry.arity == 2 &&
                   erts_is_atom_str("dispatch", entry.function, 0)) {
            emit_t2_g31_dispatch_2(entry);
        } else if (entry.arity == 3 &&
                   erts_is_atom_str("handle_call", entry.function, 0)) {
            emit_t2_g31_handle_call_3(entry);
        } else if (entry.arity == 2 &&
                   erts_is_atom_str("sum_scores", entry.function, 0)) {
            emit_t2_gmap_sum_scores_2(entry);
        } else {
            /* No specialization for this function (yet). Fall back to T1. */
            comment("(no specialization — fall back to T1)");
            a.b(entry.side_exit);
        }
    }
}

void BeamModuleAssembler::emit_t2_total_2(const T2FunctionEntry &entry) {
    /* Specialized body for `total/2` with `diff/2` *inlined*:
     *   total([], Net) -> Net;
     *   total([{A, F} | Rest], Net) -> total(Rest, Net + (A - F)).
     *
     * Entry contract (matches T1's body entry — i.e. immediately
     * after i_test_yield's reduction decrement):
     *   XREG0 = list (tagged)
     *   XREG1 = Net  (tagged smallint)
     *   x30   = LR
     *   CP already on E stack
     *   FCALLS already decremented
     *
     * Because diff is inlined, *every* guard fires before any
     * XREG mutation — there's no call frame to save/restore around,
     * no post-call recovery to worry about. Side-exit is a direct
     * branch to entry.side_exit (the T1 body label) with iter-start
     * XREG0/XREG1 still live in their canonical registers. */
    Label nil_check = a.new_label();
    Label loop_top = a.new_label();
    Label yield_setup = a.new_label();

    /* T2 internal loop top. Tail-calls land here, *not* at the public
     * function entry — that means CP stays on the Erlang stack from
     * the original prologue all the way through the loop, and we save
     * one push+pop pair per iteration. We do our own reduction
     * accounting in place of i_test_yield's. */
    a.bind(loop_top);

    a.subs(FCALLS, FCALLS, imm(1));
    a.b_le(yield_setup);

    /* List shape: non-empty cons (LIST primary tag = 0b01). tbnz bit
     * 1 → not a cons cell (NIL/boxed/immed all have bit 1 set
     * relative to LIST's 0b01). */
    a.tbnz(XREG0, imm(1), nil_check);

    /* Untag list pointer, load head/tail. */
    a.and_(TMP1, XREG0, imm(~Uint64{0x7}));
    a.ldp(TMP2, TMP3, a64::Mem(TMP1)); /* head -> TMP2, tail -> TMP3 */

    /* Head must be boxed (BOXED primary tag = 0b10). tbnz bit 0 → not
     * boxed. */
    a.tbnz(TMP2, imm(0), entry.side_exit);

    /* Untag tuple, load + check arity-2 header. */
    a.and_(TMP1, TMP2, imm(~Uint64{0x7}));
    a.ldr(TMP4, a64::Mem(TMP1));
    a.cmp(TMP4, imm(make_arityval(2)));
    a.b_ne(entry.side_exit);

    /* Load A (TMP4) and F (TMP5) from the tuple. */
    a.ldp(TMP4, TMP5, a64::Mem(TMP1, sizeof(Eterm)));

    /* Combined fixnum check on (A & F). Both must be smallint, i.e.
     * both must have low 4 bits = 0b1111. AND is correct here, not
     * OR: with OR, `0b10 | 0b1111 = 0b1111` would falsely accept a
     * boxed value as a smallint. With AND, `0b10 & 0b1111 = 0b10`
     * correctly fails. */
    a.and_(SUPER_TMP, TMP4, TMP5);
    a.and_(SUPER_TMP, SUPER_TMP, imm(_TAG_IMMED1_MASK));
    a.cmp(SUPER_TMP, imm(_TAG_IMMED1_SMALL));
    a.b_ne(entry.side_exit);

    /* Inlined `diff(A, F) -> A - F`. Use the same "detag one operand,
     * subtract from the other" trick T1's gc_bif2 uses: the result
     * carries A's tag bits (which are guaranteed smallint by the
     * combined check above), so it's still a tagged smallint if no
     * overflow occurred. V flag set on signed overflow. */
    a.and_(TMP6, TMP5, imm(~_TAG_IMMED1_MASK)); /* F detag */
    a.subs(TMP1, TMP4, TMP6);                   /* A - F (tagged) */
    a.b_vs(entry.side_exit);

    /* Inlined `Net + (A - F)`. Same trick, the other way: detag Net,
     * add to the still-tagged diff result. Result is a tagged
     * smallint (or overflows). */
    a.and_(TMP6, XREG1, imm(~_TAG_IMMED1_MASK)); /* Net detag */
    a.adds(TMP1, TMP6, TMP1);                    /* Net + diff (tagged) */
    a.b_vs(entry.side_exit);

    /* Commit: XREG1 = new Net, XREG0 = tail. CP is *not* popped — it
     * stays on the Erlang stack from the original prologue all the
     * way through the loop. */
    a.mov(XREG1, TMP1);
    a.mov(XREG0, TMP3);

    /* Tail-call: branch to T2 loop top (no CP touch, no public
     * dispatch round-trip). */
    a.b(loop_top);

    /* === NIL check (X[0] is nil → empty list, return Net) === */
    a.bind(nil_check);
    a.cmp(XREG0, imm(NIL));
    a.b_ne(entry.side_exit);

    /* Result = Net. Pop CP (the one push we did at the original
     * prologue) and return. */
    a.mov(XREG0, XREG1);
    a.ldr(a64::x30, a64::Mem(E).post(8));
    a.b_mi(resolve_fragment(ga->get_dispatch_return(), disp1MB));
    a.ret(a64::x30);

    /* === Yield setup: route through the standard yield trampoline
     * with ARG3 = fn_entry, so the resume PC computed by
     * i_test_yield_shared (= fn_entry + TEST_YIELD_RETURN_OFFSET)
     * lands at the hook position, which is `b t2_entry`. The hook
     * sends control back into this loop on resume.
     *
     * The MFA-pointer derivation in i_test_yield_shared (`sub ARG2,
     * ARG3, sizeof(ErtsCodeMFA)`) is also satisfied by ARG3 =
     * fn_entry, since the public function entry is preceded by the
     * MFA struct laid down by emit_i_func_info. */
    a.bind(yield_setup);
    comment("T2 yield: route through i_test_yield_shared");
    a.adr(ARG3, entry.fn_entry);
    a.b(resolve_fragment(ga->get_i_test_yield_shared(), disp1MB));
}

void BeamModuleAssembler::emit_t2_are_all_limited_2(
        const T2FunctionEntry &entry) {
    /* G3 experiment: specialized body for
     *
     *   are_all_limited([E | Es], K) ->
     *     is_limited(E, K) andalso are_all_limited(Es, K);
     *   are_all_limited([], _) ->
     *     true.
     *
     * with is_limited/2's dispatch *header* inlined:
     *   - E =:= any                    -> true   (clause 1)
     *   - K =< 0                       -> false  (clause 2)
     *   - E not boxed / not a 4-tuple
     *     / not an #c{} record         -> true   (catch-all clause)
     *   - stage b: #c{} with a leaf
     *     tag (atom | number | var)    -> true   (catch-all clause)
     *   - anything else                -> real call to T1
     *                                     is_limited/2, returning
     *                                     into this loop.
     *
     * The call is the experiment: loop state (Tl, K) crosses a real
     * Erlang call via two Y slots and execution resumes in the T2
     * loop — the call-crossing structure the v1 loop tier forbids
     * and G3 gates.
     *
     * Entry contract (after prologue + i_test_yield via the hook):
     *   XREG0 = list (tagged), XREG1 = K, CP on E stack,
     *   FCALLS decremented once.
     *
     * Iteration-start-stays-live: XREG0 keeps the *current* cons
     * cell until the element is fully resolved; XREG1 keeps K
     * always (restored from Y after calls). Every side-exit
     * (non-list, low-stack) therefore lands at the T1 body with
     * pristine arguments and T1 re-executes the element from
     * scratch.
     *
     * is_limited/2's clause order is respected: the any-check comes
     * before the K-check (any is limited even at K =< 0). */
    Label loop_top = a.new_label();
    Label nil_check = a.new_label();
    Label commit = a.new_label();
    Label ret_false = a.new_label();
    Label ret_xreg0 = a.new_label();
    Label yield_setup = a.new_label();

    if (!t2_is_limited_entry.is_valid()) {
        comment("(is_limited/2 label missing — fall back to T1)");
        a.b(entry.side_exit);
        return;
    }

    const Eterm atom_any =
            erts_atom_put((byte *)"any", 3, ERTS_ATOM_ENC_LATIN1, 1);
    const Eterm atom_c =
            erts_atom_put((byte *)"c", 1, ERTS_ATOM_ENC_LATIN1, 1);
    const Eterm atom_tag_atom =
            erts_atom_put((byte *)"atom", 4, ERTS_ATOM_ENC_LATIN1, 1);
    const Eterm atom_tag_number =
            erts_atom_put((byte *)"number", 6, ERTS_ATOM_ENC_LATIN1, 1);
    const Eterm atom_tag_var =
            erts_atom_put((byte *)"var", 3, ERTS_ATOM_ENC_LATIN1, 1);
    const Eterm atom_tag_identifier =
            erts_atom_put((byte *)"identifier", 10, ERTS_ATOM_ENC_LATIN1, 1);
    const Eterm atom_tag_binary =
            erts_atom_put((byte *)"binary", 6, ERTS_ATOM_ENC_LATIN1, 1);
    const Eterm atom_tag_record =
            erts_atom_put((byte *)"record", 6, ERTS_ATOM_ENC_LATIN1, 1);
    const Eterm atom_tag_nil =
            erts_atom_put((byte *)"nil", 3, ERTS_ATOM_ENC_LATIN1, 1);

    a.bind(loop_top);

    /* Reduction accounting: T1 charges 3 per element (is_limited
     * entry, is_limited return, self-call re-entry). Match it so
     * scheduling never sees longer timeslices than T1. */
    a.subs(FCALLS, FCALLS, imm(3));
    a.b_le(yield_setup);

    /* Non-empty cons check (LIST primary tag = 0b01; bit 1 set for
     * boxed/immediate, clear for cons). */
    a.tbnz(XREG0, imm(1), nil_check);

    /* Load E (head) and Tl (tail). */
    a.and_(TMP1, XREG0, imm(~Uint64{0x7}));
    a.ldp(TMP2, TMP3, a64::Mem(TMP1));

    /* --- inlined is_limited/2 dispatch header --- */

    /* Clause 1: E =:= any -> limited. */
    mov_imm(TMP5, atom_any);
    a.cmp(TMP2, TMP5);
    a.b_eq(commit);

    /* Clause 2: K =< 0 -> not limited. Tagged smallint compare is
     * order-preserving; non-smallint K side-exits to T1 instead of
     * guessing. */
    a.and_(TMP4, XREG1, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP4, imm(_TAG_IMMED1_SMALL));
    a.b_ne(entry.side_exit);
    a.cmp(XREG1, imm(make_small(0)));
    a.b_le(ret_false);

    /* Catch-all: non-boxed terms (atoms, immediates, conses) are
     * limited. BOXED primary tag = 0b10 -> bit 0 clear. */
    a.tbnz(TMP2, imm(0), commit);

    /* Catch-all: boxed but not a 4-tuple (bignum, float, map, ...)
     * or a 4-tuple that isn't an #c{} record. */
    a.and_(TMP1, TMP2, imm(~Uint64{0x7}));
    a.ldr(TMP4, a64::Mem(TMP1));
    a.cmp(TMP4, imm(make_arityval(4)));
    a.b_ne(commit);
    a.ldr(TMP4, a64::Mem(TMP1, sizeof(Eterm)));
    mov_imm(TMP5, atom_c);
    a.cmp(TMP4, TMP5);
    a.b_ne(commit);

    if (t2_g3_mode() == 2) {
        /* Stage b: leaf tags resolve to the catch-all without the
         * call. Unrecognised tags simply take the call — the inline
         * set doesn't need to be complete to be correct. */
        a.ldr(TMP4, a64::Mem(TMP1, 2 * sizeof(Eterm)));
        mov_imm(TMP5, atom_tag_atom);
        a.cmp(TMP4, TMP5);
        a.b_eq(commit);
        mov_imm(TMP5, atom_tag_number);
        a.cmp(TMP4, TMP5);
        a.b_eq(commit);
        mov_imm(TMP5, atom_tag_var);
        a.cmp(TMP4, TMP5);
        a.b_eq(commit);
        mov_imm(TMP5, atom_tag_identifier);
        a.cmp(TMP4, TMP5);
        a.b_eq(commit);
        mov_imm(TMP5, atom_tag_binary);
        a.cmp(TMP4, TMP5);
        a.b_eq(commit);
        mov_imm(TMP5, atom_tag_record);
        a.cmp(TMP4, TMP5);
        a.b_eq(commit);
        mov_imm(TMP5, atom_tag_nil);
        a.cmp(TMP4, TMP5);
        a.b_eq(commit);
    }

    /* --- slow path: real call to T1 is_limited(E, K) --- */

    /* Stack room for two Y slots; if tight, side-exit and let T1's
     * own allocate trigger the GC. XREG0/XREG1 are still pristine. */
    a.add(SUPER_TMP, HTOP, imm((2 + S_RESERVED) * sizeof(Eterm)));
    a.cmp(SUPER_TMP, E);
    a.b_hi(entry.side_exit);

    sub(E, E, 2 * sizeof(Eterm));
    a.stp(TMP3, XREG1, a64::Mem(E)); /* save Tl, K */
    a.mov(XREG0, TMP2);              /* arg 0 = E; arg 1 = K in place */
    a.bl(t2_is_limited_entry);       /* T1 call; result in XREG0 */
    a.ldp(TMP3, XREG1, a64::Mem(E).post(2 * sizeof(Eterm)));

    mov_imm(SUPER_TMP, am_true);
    a.cmp(XREG0, SUPER_TMP);
    a.b_ne(ret_xreg0); /* false -> return it (booleans only) */

    /* --- element resolved as limited: advance the spine --- */
    a.bind(commit);
    a.mov(XREG0, TMP3);
    a.b(loop_top);

    /* --- [] -> true; anything else non-cons -> T1 raises --- */
    a.bind(nil_check);
    a.cmp(XREG0, imm(NIL));
    a.b_ne(entry.side_exit);
    mov_imm(XREG0, am_true);
    a.b(ret_xreg0);

    a.bind(ret_false);
    mov_imm(XREG0, am_false);

    a.bind(ret_xreg0);
    emit_leave_erlang_frame();
    a.subs(FCALLS, FCALLS, imm(1));
    a.b_mi(resolve_fragment(ga->get_dispatch_return(), disp1MB));
    a.ret(a64::x30);

    /* --- yield: same shape as emit_t2_total_2 --- */
    a.bind(yield_setup);
    comment("T2 yield: route through i_test_yield_shared");
    a.adr(ARG3, entry.fn_entry);
    a.b(resolve_fragment(ga->get_i_test_yield_shared(), disp1MB));
}

void BeamModuleAssembler::emit_t2_json_scan(const T2FunctionEntry &entry,
                                            bool digits) {
    /* G-bin experiment: fast-forward scanner for stdlib json's
     * byte-class loops.
     *
     *   digits=true  — json:number/7 and json:number_frac_cont/7:
     *                  consume 0-9.
     *   digits=false — json:string_ascii/7: consume "plain" bytes
     *                  (0x20..0x7F except `"` and `\`), 8 at a time
     *                  via SWAR, bytewise at chunk/end boundaries.
     *
     * Entry contract: XREG0 = match context (or not — guarded),
     * XREG1..XREG5 untouched pass-through state, X register 6
     * (memory-resident) = Len, a tagged smallint counting consumed
     * bytes.
     *
     * The body is pure fast-forward: it consumes bytes of the hot
     * class with base/position/end in registers, then *every* exit
     * — terminator byte, end of input, yield — syncs state (write
     * the bit position back into the context, X6 = Len + consumed)
     * and leaves. Terminator/end exits side-exit to the T1 entry,
     * which re-matches the first unconsumed byte and takes whatever
     * clause applies; this body needs no knowledge of the clause
     * structure at all. Nothing is committed before sync, so any
     * guard failure side-exits with pristine entry state.
     *
     * What this eliminates per byte vs T1: the function re-entry
     * (prologue + i_test_yield), the bs_match position load/store
     * and base load+mask, and the per-byte sub-binary bookkeeping —
     * the "match-context dance". */
    Label loop = a.new_label();
    Label bytewise = a.new_label();
    Label exit_scan = a.new_label();
    Label yield_sync = a.new_label();

    const a64::Gp ctxu = ARG1; /* untagged context pointer */
    const a64::Gp base = ARG2; /* byte base pointer          */
    const a64::Gp pos = ARG3;  /* current bit offset          */
    const a64::Gp end_ = ARG4; /* end bit offset              */
    const a64::Gp pos0 = ARG5; /* entry bit offset            */
    const a64::Gp w = ARG6;    /* byte / SWAR word            */

    /* --- guards: X0 must be a byte-aligned match context, Len a
     * smallint. Side-exit leaves everything untouched. --- */
    a.tbnz(XREG0, imm(0), entry.side_exit); /* not boxed */
    a.and_(ctxu, XREG0, imm(~Uint64{0x7}));
    a.ldr(TMP1, a64::Mem(ctxu));
    mov_imm(TMP2, HEADER_SUB_BITS);
    a.cmp(TMP1, TMP2);
    a.b_ne(entry.side_exit);
    a.ldr(base, a64::Mem(ctxu, offsetof(ErlSubBits, base_flags)));
    a.and_(TMP1, base, imm(ERL_SUB_BITS_FLAG_MASK));
    a.cmp(TMP1, imm(ERL_SUB_BITS_FLAG_MUTABLE));
    a.b_ne(entry.side_exit); /* not a match context */
    a.and_(base, base, imm(~Uint64{ERL_SUB_BITS_FLAG_MASK}));
    a.ldr(pos, a64::Mem(ctxu, offsetof(ErlSubBits, start)));
    a.ldr(end_, a64::Mem(ctxu, offsetof(ErlSubBits, end)));
    a.tst(pos, imm(7));
    a.b_ne(entry.side_exit); /* bit-unaligned start */
    mov_arg(TMP1, ArgXRegister(6));
    a.and_(TMP2, TMP1, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP2, imm(_TAG_IMMED1_SMALL));
    a.b_ne(entry.side_exit); /* Len not a smallint */

    a.mov(pos0, pos);

    if (digits) {
        /* --- digit scan: one byte per iteration --- */
        a.bind(loop);
        a.subs(FCALLS, FCALLS, imm(1));
        a.b_le(yield_sync);
        a.add(TMP1, pos, imm(8));
        a.cmp(TMP1, end_);
        a.b_hi(exit_scan); /* fewer than 8 bits left */
        a.lsr(TMP2, pos, imm(3));
        a.ldrb(w.w(), a64::Mem(base, TMP2));
        a.sub(TMP3, w, imm('0'));
        a.cmp(TMP3, imm(9));
        a.b_hi(exit_scan); /* not 0-9 */
        a.add(pos, pos, imm(8));
        a.b(loop);
    } else {
        /* --- plain-ASCII scan, SWAR 8 bytes per iteration ---
         * Stop byte: < 0x20, == 0x22 ("), == 0x5C (\), >= 0x80.
         * Constants live in TMP1-TMP5; acc in ARG8; no calls in the
         * body, so the ARG registers are plain scratch. */
        const a64::Gp acc = ARG8;

        mov_imm(TMP1, 0x0101010101010101ull);
        mov_imm(TMP2, 0x8080808080808080ull);
        mov_imm(TMP3, 0x2020202020202020ull);
        mov_imm(TMP4, 0x2222222222222222ull);
        mov_imm(TMP5, 0x5C5C5C5C5C5C5C5Cull);

        a.bind(loop);
        a.subs(FCALLS, FCALLS, imm(1));
        a.b_le(yield_sync);
        a.add(TMP6, pos, imm(64));
        a.cmp(TMP6, end_);
        a.b_hi(bytewise); /* fewer than 8 bytes left */
        a.lsr(TMP6, pos, imm(3));
        a.ldr(w, a64::Mem(base, TMP6)); /* 8 bytes, little-endian */

        /* bad |= bytes < 0x20 or >= 0x80:
         *   ((w - 0x2020..) & ~w | w) & 0x8080.. */
        a.sub(TMP6, w, TMP3);
        a.bic(TMP6, TMP6, w);
        a.orr(TMP6, TMP6, w);
        a.and_(acc, TMP6, TMP2);
        /* bad |= zero byte in (w ^ 0x2222..)  [byte == `"`]:
         *   (x - 0x0101..) & ~x & 0x8080.. */
        a.eor(SUPER_TMP, w, TMP4);
        a.sub(TMP6, SUPER_TMP, TMP1);
        a.bic(TMP6, TMP6, SUPER_TMP);
        a.and_(TMP6, TMP6, TMP2);
        a.orr(acc, acc, TMP6);
        /* bad |= zero byte in (w ^ 0x5C5C..)  [byte == `\`] */
        a.eor(SUPER_TMP, w, TMP5);
        a.sub(TMP6, SUPER_TMP, TMP1);
        a.bic(TMP6, TMP6, SUPER_TMP);
        a.and_(TMP6, TMP6, TMP2);
        a.orr(acc, acc, TMP6);

        a.cbnz(acc, bytewise); /* stop byte within this chunk */
        a.add(pos, pos, imm(64));
        a.b(loop);

        /* Bounded tail: entered only when a stop byte or the end of
         * input lies within the next 8 bytes, so this loops at most
         * 8 times — no reduction check needed. */
        a.bind(bytewise);
        a.add(TMP6, pos, imm(8));
        a.cmp(TMP6, end_);
        a.b_hi(exit_scan); /* fewer than 8 bits left */
        a.lsr(TMP6, pos, imm(3));
        a.ldrb(w.w(), a64::Mem(base, TMP6));
        a.sub(TMP6, w, imm(0x20));
        a.cmp(TMP6, imm(0x5F));
        a.b_hi(exit_scan); /* < 0x20 or >= 0x80 */
        a.cmp(w, imm(0x22));
        a.b_eq(exit_scan);
        a.cmp(w, imm(0x5C));
        a.b_eq(exit_scan);
        a.add(pos, pos, imm(8));
        a.b(bytewise);
    }

    /* --- exit: sync consumed bytes, side-exit to T1 entry --- */
    a.bind(exit_scan);
    a.sub(TMP1, pos, pos0); /* consumed bits */
    mov_arg(TMP2, ArgXRegister(6));
    a.adds(TMP3, TMP2, TMP1, a64::lsl(1)); /* Len + k, still tagged */
    a.b_vs(entry.side_exit); /* Len + k overflows: redo in T1 */
    a.str(pos, a64::Mem(ctxu, offsetof(ErlSubBits, start)));
    mov_arg(ArgXRegister(6), TMP3);
    a.b(entry.side_exit);

    /* --- yield: sync first, then the standard trampoline --- */
    a.bind(yield_sync);
    a.sub(TMP1, pos, pos0);
    mov_arg(TMP2, ArgXRegister(6));
    a.adds(TMP3, TMP2, TMP1, a64::lsl(1));
    a.b_vs(entry.side_exit);
    a.str(pos, a64::Mem(ctxu, offsetof(ErlSubBits, start)));
    mov_arg(ArgXRegister(6), TMP3);
    comment("T2 yield: route through i_test_yield_shared");
    a.adr(ARG3, entry.fn_entry);
    a.b(resolve_fragment(ga->get_i_test_yield_shared(), disp1MB));
}

void BeamModuleAssembler::emit_t2_g31_dispatch_2(
        const T2FunctionEntry &entry) {
    /* G3 subject 1: cold-arm-pruned t2_g31:dispatch/2. Only the hot
     * arm is emitted, with its guards fused:
     *
     *   dispatch({get, I}, State)
     *       when is_integer(I), I >= 1, I =< 10 ->
     *     element(I, State);
     *
     * Everything else — eleven cold arms — side-exits to the full
     * T1 clause dispatch with pristine arguments. */
    const Eterm atom_get =
            erts_atom_put((byte *)"get", 3, ERTS_ATOM_ENC_LATIN1, 1);

    a.tbnz(XREG0, imm(0), entry.side_exit); /* Msg not boxed */
    a.and_(TMP1, XREG0, imm(~Uint64{0x7}));
    a.ldr(TMP2, a64::Mem(TMP1));
    a.cmp(TMP2, imm(make_arityval(2)));
    a.b_ne(entry.side_exit);
    a.ldr(TMP2, a64::Mem(TMP1, sizeof(Eterm)));
    mov_imm(TMP3, atom_get);
    a.cmp(TMP2, TMP3);
    a.b_ne(entry.side_exit);
    a.ldr(TMP4, a64::Mem(TMP1, 2 * sizeof(Eterm))); /* I, tagged */
    a.and_(TMP5, TMP4, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP5, imm(_TAG_IMMED1_SMALL));
    a.b_ne(entry.side_exit);

    a.tbnz(XREG1, imm(0), entry.side_exit); /* State not boxed */
    a.and_(TMP1, XREG1, imm(~Uint64{0x7}));
    a.ldr(TMP2, a64::Mem(TMP1));
    a.cmp(TMP2, imm(make_arityval(10)));
    a.b_ne(entry.side_exit);

    a.asr(TMP5, TMP4, imm(_TAG_IMMED1_SIZE)); /* raw I */
    a.cmp(TMP5, imm(1));
    a.b_lt(entry.side_exit);
    a.cmp(TMP5, imm(10));
    a.b_gt(entry.side_exit);

    a.add(TMP1, TMP1, TMP5, a64::lsl(3)); /* element I at offset 8*I */
    a.ldr(XREG0, a64::Mem(TMP1));

    emit_leave_erlang_frame();
    a.subs(FCALLS, FCALLS, imm(1));
    a.b_mi(resolve_fragment(ga->get_dispatch_return(), disp1MB));
    a.ret(a64::x30);
}

void BeamModuleAssembler::emit_t2_g31_handle_call_3(
        const T2FunctionEntry &entry) {
    /* Same hot arm inside the gen_server callback:
     *
     *   handle_call({get, I}, _From, State)
     *       when is_integer(I), I >= 1, I =< 10 ->
     *     {reply, element(I, State), State};
     *
     * Allocates the 3-tuple reply, so the GC test runs first (the
     * fragment may move terms, invalidating derived pointers). */
    const Eterm atom_get =
            erts_atom_put((byte *)"get", 3, ERTS_ATOM_ENC_LATIN1, 1);
    const Eterm atom_reply =
            erts_atom_put((byte *)"reply", 5, ERTS_ATOM_ENC_LATIN1, 1);

    emit_gc_test(ArgWord(0), ArgWord(4), ArgWord(3));

    a.tbnz(XREG0, imm(0), entry.side_exit);
    a.and_(TMP1, XREG0, imm(~Uint64{0x7}));
    a.ldr(TMP2, a64::Mem(TMP1));
    a.cmp(TMP2, imm(make_arityval(2)));
    a.b_ne(entry.side_exit);
    a.ldr(TMP2, a64::Mem(TMP1, sizeof(Eterm)));
    mov_imm(TMP3, atom_get);
    a.cmp(TMP2, TMP3);
    a.b_ne(entry.side_exit);
    a.ldr(TMP4, a64::Mem(TMP1, 2 * sizeof(Eterm)));
    a.and_(TMP5, TMP4, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP5, imm(_TAG_IMMED1_SMALL));
    a.b_ne(entry.side_exit);

    a.tbnz(XREG2, imm(0), entry.side_exit);
    a.and_(TMP1, XREG2, imm(~Uint64{0x7}));
    a.ldr(TMP2, a64::Mem(TMP1));
    a.cmp(TMP2, imm(make_arityval(10)));
    a.b_ne(entry.side_exit);

    a.asr(TMP5, TMP4, imm(_TAG_IMMED1_SIZE));
    a.cmp(TMP5, imm(1));
    a.b_lt(entry.side_exit);
    a.cmp(TMP5, imm(10));
    a.b_gt(entry.side_exit);

    a.add(TMP1, TMP1, TMP5, a64::lsl(3));
    a.ldr(TMP6, a64::Mem(TMP1)); /* V = element(I, State) */

    /* Build {reply, V, State} on the heap. */
    mov_imm(TMP2, make_arityval(3));
    mov_imm(TMP3, atom_reply);
    a.stp(TMP2, TMP3, a64::Mem(HTOP));
    a.stp(TMP6, XREG2, a64::Mem(HTOP, 2 * sizeof(Eterm)));
    a.orr(XREG0, HTOP, imm(TAG_PRIMARY_BOXED));
    a.add(HTOP, HTOP, imm(4 * sizeof(Eterm)));

    emit_leave_erlang_frame();
    a.subs(FCALLS, FCALLS, imm(1));
    a.b_mi(resolve_fragment(ga->get_dispatch_return(), disp1MB));
    a.ret(a64::x30);
}

void BeamModuleAssembler::emit_t2_gmap_sum_scores_2(
        const T2FunctionEntry &entry) {
    /* G-map experiment: region-level flatmap shape specialization
     * for
     *
     *   sum_scores([M | T], Acc) ->
     *     #{active := A, score := S} = M,
     *     case A of
     *       true  -> sum_scores(T, Acc + S);
     *       false -> sum_scores(T, Acc)
     *     end;
     *   sum_scores([], Acc) -> Acc.
     *
     * The shape guard is two key-slot compares (keys[0] == active,
     * keys[4] == score, the observed layout for the 5-key template);
     * after it, both values are direct offset loads — no per-key
     * scan. Any other shape (different layout/size, hashmap,
     * non-map) side-exits to T1's generic get_map_elements. */
    Label loop = a.new_label();
    Label nil_check = a.new_label();
    Label do_add = a.new_label();
    Label yield_setup = a.new_label();

    const Eterm atom_active =
            erts_atom_put((byte *)"active", 6, ERTS_ATOM_ENC_LATIN1, 1);
    const Eterm atom_score =
            erts_atom_put((byte *)"score", 5, ERTS_ATOM_ENC_LATIN1, 1);

    a.bind(loop);
    a.subs(FCALLS, FCALLS, imm(2));
    a.b_le(yield_setup);
    a.tbnz(XREG0, imm(1), nil_check);
    a.and_(TMP1, XREG0, imm(~Uint64{0x7}));
    a.ldp(TMP2, TMP3, a64::Mem(TMP1)); /* M, Tl */

    a.tbnz(TMP2, imm(0), entry.side_exit); /* M not boxed */
    a.and_(TMP1, TMP2, imm(~Uint64{0x7}));
    a.ldr(TMP4, a64::Mem(TMP1));
    mov_imm(TMP5, MAP_HEADER_FLATMAP);
    a.cmp(TMP4, TMP5);
    a.b_ne(entry.side_exit); /* not a flatmap */
    a.ldr(TMP4, a64::Mem(TMP1, offsetof(flatmap_t, size)));
    a.cmp(TMP4, imm(5));
    a.b_ne(entry.side_exit);
    a.ldr(TMP4, a64::Mem(TMP1, offsetof(flatmap_t, keys)));
    a.and_(TMP4, TMP4, imm(~Uint64{0x7}));
    a.ldr(TMP5, a64::Mem(TMP4, sizeof(Eterm))); /* keys[0] */
    mov_imm(TMP6, atom_active);
    a.cmp(TMP5, TMP6);
    a.b_ne(entry.side_exit);
    a.ldr(TMP5, a64::Mem(TMP4, 4 * sizeof(Eterm))); /* keys[3] */
    mov_imm(TMP6, atom_score);
    a.cmp(TMP5, TMP6);
    a.b_ne(entry.side_exit);

    /* Direct value loads: values start after the flatmap_t header.
     * NOTE: flatmap key order is atom-index-dependent (it differed
     * between two VMs of the same build during this experiment!).
     * The guards above make a wrong guess safe (side-exit, T1
     * handles it); production must guard the keys-tuple *pointer*
     * recorded by runtime feedback (02 §7.6), never positions
     * assumed at codegen time. */
    a.ldr(TMP5, a64::Mem(TMP1, sizeof(flatmap_t)));                   /* A */
    a.ldr(TMP6, a64::Mem(TMP1, sizeof(flatmap_t) + 3 * sizeof(Eterm))); /* S */

    mov_imm(SUPER_TMP, am_true);
    a.cmp(TMP5, SUPER_TMP);
    a.b_eq(do_add);
    mov_imm(SUPER_TMP, am_false);
    a.cmp(TMP5, SUPER_TMP);
    a.b_ne(entry.side_exit); /* non-boolean: T1 raises case_clause */
    a.mov(XREG0, TMP3);      /* skip: just advance */
    a.b(loop);

    a.bind(do_add);
    /* Combined smallint check on (S & Acc), then the one-detag add. */
    a.and_(SUPER_TMP, TMP6, XREG1);
    a.and_(SUPER_TMP, SUPER_TMP, imm(_TAG_IMMED1_MASK));
    a.cmp(SUPER_TMP, imm(_TAG_IMMED1_SMALL));
    a.b_ne(entry.side_exit);
    a.and_(SUPER_TMP, TMP6, imm(~Uint64{_TAG_IMMED1_MASK}));
    a.adds(SUPER_TMP, XREG1, SUPER_TMP);
    a.b_vs(entry.side_exit);
    a.mov(XREG1, SUPER_TMP);
    a.mov(XREG0, TMP3);
    a.b(loop);

    a.bind(nil_check);
    a.cmp(XREG0, imm(NIL));
    a.b_ne(entry.side_exit);
    a.mov(XREG0, XREG1);
    emit_leave_erlang_frame();
    a.subs(FCALLS, FCALLS, imm(1));
    a.b_mi(resolve_fragment(ga->get_dispatch_return(), disp1MB));
    a.ret(a64::x30);

    a.bind(yield_setup);
    comment("T2 yield: route through i_test_yield_shared");
    a.adr(ARG3, entry.fn_entry);
    a.b(resolve_fragment(ga->get_i_test_yield_shared(), disp1MB));
}

void BeamModuleAssembler::emit_func_line(const ArgWord &Loc) {
}

void BeamModuleAssembler::emit_empty_func_line() {
}

void BeamModuleAssembler::emit_executable_line(const ArgWord &Loc,
                                               const ArgWord &Index) {
}

/*
 * Here follows stubs for instructions that should never be called.
 */

void BeamModuleAssembler::emit_i_debug_breakpoint() {
    emit_nyi("i_debug_breakpoint should never be called");
}

void BeamModuleAssembler::emit_i_generic_breakpoint() {
    emit_nyi("i_generic_breakpoint should never be called");
}

void BeamModuleAssembler::emit_trace_jump(const ArgWord &) {
    emit_nyi("trace_jump should never be called");
}

void BeamModuleAssembler::emit_call_error_handler() {
    emit_nyi("call_error_handler should never be called");
}

const Label &BeamModuleAssembler::resolve_beam_label(const ArgLabel &Lbl,
                                                     enum Displacement disp) {
    ASSERT(Lbl.isLabel());

    const Label &beamLabel = rawLabels.at(Lbl.get());
    const auto &labelEntry = code.label_entry_of(beamLabel);

    if (labelEntry.has_name()) {
        return resolve_label(rawLabels.at(Lbl.get()), disp, labelEntry.name());
    } else {
        return resolve_label(rawLabels.at(Lbl.get()), disp);
    }
}

const Label &BeamModuleAssembler::resolve_label(const Label &target,
                                                enum Displacement disp,
                                                const char *labelName) {
    ssize_t currOffset = a.offset();

    ssize_t minOffset = currOffset - disp;
    ssize_t maxOffset = currOffset + disp;

    ASSERT(disp >= dispMin && disp <= dispMax);
    ASSERT(target.is_valid());

    if (code.is_label_bound(target)) {
        ssize_t targetOffset = code.label_offset_from_base(target);

        /* Backward reference: skip veneers if it's already in range. */
        if (targetOffset >= minOffset) {
            return target;
        }
    }

    /* If a previously created veneer is reachable from this point, we can use
     * it instead of creating a new one. */
    auto range = _veneers.equal_range(target.id());
    for (auto it = range.first; it != range.second; it++) {
        const Veneer &veneer = it->second;

        if (code.is_label_bound(veneer.anchor)) {
            ssize_t veneerOffset = code.label_offset_from_base(veneer.anchor);

            if (veneerOffset >= minOffset && veneerOffset <= maxOffset) {
                return veneer.anchor;
            }
        } else if (veneer.latestOffset <= maxOffset) {
            return veneer.anchor;
        }
    }

    Label anchor;

    if (!labelName) {
        anchor = a.new_label();
    } else {
        /* This is the entry label for a function. Create an unique
         * name for the anchor label. It is necessary to include a
         * sequence number in the label name because if the module is
         * huge more than one veneer can be created for each entry
         * label. */
        std::stringstream name;
        name << '@' << labelName << '-' << labelSeq++;
        anchor = a.new_named_label(name.str().c_str());
    }

    auto it = _veneers.emplace(target.id(), Veneer{maxOffset, anchor, target});

    const Veneer &veneer = it->second;
    _pending_veneers.emplace(veneer);

    return veneer.anchor;
}

const Label &BeamModuleAssembler::resolve_fragment(void (*fragment)(),
                                                   enum Displacement disp) {
    auto it = _dispatchTable.find(fragment);

    if (it == _dispatchTable.end()) {
        it = _dispatchTable.emplace(fragment, a.new_label()).first;
    }

    return resolve_label(it->second, disp);
}

a64::Mem BeamModuleAssembler::embed_constant(const ArgVal &value,
                                             enum Displacement disp) {
    ssize_t currOffset = a.offset();

    ssize_t minOffset = currOffset - disp;
    ssize_t maxOffset = currOffset + disp;

    ASSERT(disp >= dispMin && disp <= dispMax);
    ASSERT(!value.isRegister());

    /* If a previously embedded constant is reachable from this point, we
     * can use it instead of creating a new one. */
    auto range = _constants.equal_range(value);
    for (auto it = range.first; it != range.second; it++) {
        const Constant &constant = it->second;

        if (code.is_label_bound(constant.anchor)) {
            ssize_t constOffset = code.label_offset_from_base(constant.anchor);

            if (constOffset >= minOffset && constOffset <= maxOffset) {
                return a64::Mem(constant.anchor);
            }
        } else if (constant.latestOffset <= maxOffset) {
            return a64::Mem(constant.anchor);
        }
    }

    auto it = _constants.emplace(value,
                                 Constant{maxOffset, a.new_label(), value});
    const Constant &constant = it->second;
    _pending_constants.emplace(constant);

    return a64::Mem(constant.anchor);
}

a64::Mem BeamModuleAssembler::embed_label(const Label &label,
                                          enum Displacement disp) {
    ssize_t currOffset = a.offset();

    ssize_t maxOffset = currOffset + disp;

    ASSERT(disp >= dispMin && disp <= dispMax);

    auto it = _embedded_labels.emplace(
            label.id(),
            EmbeddedLabel{maxOffset, a.new_label(), label});
    ASSERT(it.second);
    const EmbeddedLabel &embedded_label = it.first->second;
    _pending_labels.emplace(embedded_label);

    return a64::Mem(embedded_label.anchor);
}

void BeamModuleAssembler::emit_i_flush_stubs() {
    /* Flush all stubs that are due within the next two check intervals
     * to prevent them from being emitted inside function prologues or
     * NIF padding. */
    flush_pending_stubs(STUB_CHECK_INTERVAL * 2);
    last_stub_check_offset = a.offset();
}

void BeamModuleAssembler::check_pending_stubs() {
    size_t currOffset = a.offset();

    /* We shouldn't let too much space pass between checks. */
    ASSERT((last_stub_check_offset + dispMin) >= currOffset);

    if (last_stub_check_offset + STUB_CHECK_INTERVAL < currOffset ||
        (is_unreachable() &&
         last_stub_check_offset + STUB_CHECK_INTERVAL_UNREACHABLE <
                 currOffset)) {
        last_stub_check_offset = currOffset;

        flush_pending_stubs(STUB_CHECK_INTERVAL * 2);
    }

    if (is_unreachable()) {
        flush_pending_labels();
    }
}

void BeamModuleAssembler::flush_pending_stubs(size_t range) {
    ssize_t effective_offset = a.offset() + range;
    Label next;

    if (!_pending_labels.empty()) {
        next = a.new_label();

        comment("Begin stub section");
        if (!is_unreachable()) {
            a.b(next);
        }

        flush_pending_labels();
    }

    while (!_pending_veneers.empty()) {
        const Veneer &veneer = _pending_veneers.top();

        if (veneer.latestOffset > effective_offset) {
            break;
        }

        if (!code.is_label_bound(veneer.anchor)) {
            if (!next.is_valid()) {
                next = a.new_label();

                comment("Begin stub section");
                if (!is_unreachable()) {
                    a.b(next);
                }
            }

            emit_veneer(veneer);

            effective_offset = a.offset() + range;
        }

        _pending_veneers.pop();
    }

    while (!_pending_constants.empty()) {
        const Constant &constant = _pending_constants.top();

        if (constant.latestOffset > effective_offset) {
            break;
        }

        /* Unlike veneers, we never bind constants ahead of time. */
        ASSERT(!code.is_label_bound(constant.anchor));

        if (!next.is_valid()) {
            next = a.new_label();

            comment("Begin stub section");
            if (!is_unreachable()) {
                a.b(next);
            }
        }

        emit_constant(constant);

        effective_offset = a.offset() + range;

        _pending_constants.pop();
    }

    if (next.is_valid()) {
        comment("End stub section");
        a.bind(next);
    }
}

void BeamModuleAssembler::flush_pending_labels() {
    if (!_pending_labels.empty()) {
        a.align(AlignMode::kCode, 8);
    }

    while (!_pending_labels.empty()) {
        const EmbeddedLabel &embedded_label = _pending_labels.top();

        a.bind(embedded_label.anchor);
        a.embed_label(embedded_label.label, 8);

        _pending_labels.pop();
    }
}

void BeamModuleAssembler::emit_veneer(const Veneer &veneer) {
    const Label &anchor = veneer.anchor;
    const Label &target = veneer.target;
    bool directBranch;

    ASSERT(!code.is_label_bound(anchor));
    a.bind(anchor);

    /* Prefer direct branches when possible. */
    if (code.is_label_bound(target)) {
        auto targetOffset = code.label_offset_from_base(target);
        directBranch = (a.offset() - targetOffset) <= disp128MB;
    } else {
        directBranch = false;
    }

#ifdef DEBUG
    directBranch &= (a.offset() % 512) >= 256;
#endif

    if (ERTS_LIKELY(directBranch)) {
        a.b(target);
    } else {
        Label pointer = a.new_label();

        a.ldr(SUPER_TMP, a64::Mem(pointer));
        a.br(SUPER_TMP);

        a.align(AlignMode::kCode, 8);
        a.bind(pointer);
        a.embed_label(veneer.target);
    }
}

void BeamModuleAssembler::emit_constant(const Constant &constant) {
    const Label &anchor = constant.anchor;
    const ArgVal &value = constant.value;

    ASSERT(!code.is_label_bound(anchor));
    a.align(AlignMode::kData, 8);
    a.bind(anchor);

    ASSERT(!value.isRegister());

    if (value.isImmed()) {
        a.embed_uint64(value.as<ArgImmed>().get());
    } else if (value.isWord()) {
        a.embed_uint64(value.as<ArgWord>().get());
    } else if (value.isLabel()) {
        a.embed_label(rawLabels.at(value.as<ArgLabel>().get()));
    } else {
        switch (value.getType()) {
        case ArgVal::Type::BytePtr:
            strings.push_back({anchor, 0, value.as<ArgBytePtr>().get()});
            a.embed_uint64(LLONG_MAX);
            break;
        case ArgVal::Type::Catch: {
            auto handler = rawLabels[value.as<ArgCatch>().get()];
            catches.push_back({{anchor, 0, 0}, handler});

            /* Catches are limited to 32 bits, but since we don't want to load
             * 32-bit argument values due to displacement limits, we'll store
             * this as a 64-bit value with the upper bits cleared. */
            a.embed_uint64(INT_MAX);
            break;
        }
        case ArgVal::Type::Export: {
            auto index = value.as<ArgExport>().get();
            imports[index].patches.push_back({anchor, 0, 0});
            a.embed_uint64(LLONG_MAX);
            break;
        }
        case ArgVal::Type::FunEntry: {
            auto index = value.as<ArgLambda>().get();
            lambdas[index].patches.push_back({anchor, 0, 0});
            a.embed_uint64(LLONG_MAX);
            break;
        }
        case ArgVal::Type::Literal: {
            auto index = value.as<ArgLiteral>().get();
            literals[index].patches.push_back({anchor, 0, 0});
            a.embed_uint64(LLONG_MAX);
            break;
        }
        default:
            ASSERT(!"error");
        }
    }
}

void BeamModuleAssembler::flush_last_error() {
    /* When there's a possibility of raising an exception at the very end of the
     * preceding instruction (and thus pointing at the start of this one) and
     * this instruction has a new line registered, the error would erroneously
     * refer to this instead of the preceding line.
     *
     * By adding a nop when we detect this condition, the error will correctly
     * refer to the preceding line. */
    if (a.offset() == last_error_offset) {
        a.nop();
    }
}
