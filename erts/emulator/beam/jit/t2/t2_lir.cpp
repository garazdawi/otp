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
 * T2-Full tier-2 JIT: LIR helpers (kind names, terminator classification,
 * a debug dump). See t2_lir.hpp.
 */

#include "t2_lir.hpp"

#include <sstream>

namespace erts_t2 {

    const char *t2_lir_kind_name(T2LirKind kind) {
        switch (kind) {
        case T2LirKind::Move: return "move";
        case T2LirKind::Swap: return "swap";
        case T2LirKind::GetList: return "get_list";
        case T2LirKind::Allocate: return "allocate";
        case T2LirKind::Deallocate: return "deallocate";
        case T2LirKind::Trim: return "trim";
        case T2LirKind::IsInteger: return "is_integer";
        case T2LirKind::IsFloat: return "is_float";
        case T2LirKind::IsNumber: return "is_number";
        case T2LirKind::IsAtom: return "is_atom";
        case T2LirKind::IsBoolean: return "is_boolean";
        case T2LirKind::IsTuple: return "is_tuple";
        case T2LirKind::IsList: return "is_list";
        case T2LirKind::IsNonemptyList: return "is_nonempty_list";
        case T2LirKind::IsNil: return "is_nil";
        case T2LirKind::IsBinary: return "is_binary";
        case T2LirKind::IsBitstring: return "is_bitstring";
        case T2LirKind::IsMap: return "is_map";
        case T2LirKind::IsPid: return "is_pid";
        case T2LirKind::IsPort: return "is_port";
        case T2LirKind::IsReference: return "is_reference";
        case T2LirKind::IsFunction: return "is_function";
        case T2LirKind::IsTaggedTuple: return "is_tagged_tuple";
        case T2LirKind::TestArity: return "test_arity";
        case T2LirKind::CmpEqExact: return "cmp_eq_exact";
        case T2LirKind::CmpNeExact: return "cmp_ne_exact";
        case T2LirKind::CmpEq: return "cmp_eq";
        case T2LirKind::CmpNe: return "cmp_ne";
        case T2LirKind::CmpLt: return "cmp_lt";
        case T2LirKind::CmpLe: return "cmp_le";
        case T2LirKind::CmpGt: return "cmp_gt";
        case T2LirKind::CmpGe: return "cmp_ge";
        case T2LirKind::Add: return "add";
        case T2LirKind::Sub: return "sub";
        case T2LirKind::Mul: return "mul";
        case T2LirKind::IDiv: return "idiv";
        case T2LirKind::Rem: return "rem";
        case T2LirKind::Band: return "band";
        case T2LirKind::Bor: return "bor";
        case T2LirKind::Bxor: return "bxor";
        case T2LirKind::Bsl: return "bsl";
        case T2LirKind::Bsr: return "bsr";
        case T2LirKind::Bnot: return "bnot";
        case T2LirKind::Neg: return "neg";
        case T2LirKind::GuardBif: return "guard_bif";
        case T2LirKind::GetTupleElement: return "get_tuple_element";
        case T2LirKind::GetHd: return "get_hd";
        case T2LirKind::GetTl: return "get_tl";
        case T2LirKind::MakeList: return "make_list";
        case T2LirKind::MakeTuple: return "make_tuple";
        case T2LirKind::MakeFun: return "make_fun";
        case T2LirKind::CmpBool: return "cmp_bool";
        case T2LirKind::GcTest: return "gc_test";
        case T2LirKind::Jump: return "jump";
        case T2LirKind::Branch: return "branch";
        case T2LirKind::Switch: return "switch";
        case T2LirKind::Return: return "return";
        case T2LirKind::Call: return "call";
        case T2LirKind::CallExt: return "call_ext";
        case T2LirKind::TailCall: return "tail_call";
        case T2LirKind::TailCallExt: return "tail_call_ext";
        case T2LirKind::CallBif: return "call_bif";
        case T2LirKind::SideExit: return "side_exit";
        case T2LirKind::ReductionCheck: return "reduction_check";
        case T2LirKind::ReductionCheckCallee: return "reduction_check_callee";
        case T2LirKind::ChargeReds: return "charge_reds";
        case T2LirKind::DemoteCallee: return "demote_callee";
        case T2LirKind::IsTupleOfArity: return "is_tuple_of_arity";
        case T2LirKind::SpeculateSmall: return "speculate_small";
        case T2LirKind::AddSmall: return "add_small";
        case T2LirKind::SubSmall: return "sub_small";
        case T2LirKind::StartMatch: return "start_match";
        case T2LirKind::BsMatch: return "bs_match";
        case T2LirKind::BsGetTail: return "bs_get_tail";
        case T2LirKind::BsTestTail: return "bs_test_tail";
        case T2LirKind::Invalid: return "invalid";
        }
        return "?";
    }

    bool t2_lir_kind_is_terminator(T2LirKind kind) {
        switch (kind) {
        case T2LirKind::Jump:
        case T2LirKind::Branch:
        case T2LirKind::Switch:
        case T2LirKind::Return:
        case T2LirKind::TailCall:
        case T2LirKind::TailCallExt:
        case T2LirKind::SideExit:
        case T2LirKind::DemoteCallee:
            return true;
        default:
            return false;
        }
    }

    static void dump_loc(std::ostream &os, const PhysLoc &l) {
        switch (l.kind) {
        case PhysLoc::Kind::None: os << "_"; break;
        case PhysLoc::Kind::XReg: os << "x" << l.num; break;
        case PhysLoc::Kind::YReg: os << "y" << l.num; break;
        case PhysLoc::Kind::Phys: os << "p" << l.num; break;
        }
    }

    static void dump_src(std::ostream &os, const T2LirSrc &s) {
        if (s.is_const) {
            os << "#" << (SWord)s.term;
        } else {
            dump_loc(os, s.loc);
        }
    }

    std::string t2_lir_dump(const T2LirFunction &fn) {
        std::ostringstream os;

        os << "t2_lir " << (SWord)fn.module << ":" << (SWord)fn.function << "/"
           << fn.arity << " (" << fn.blocks.size() << " blocks)\n";

        for (const T2LirBlock &b : fn.blocks) {
            os << "  b" << b.id << ":\n";
            for (const T2LirOp &op : b.ops) {
                os << "    ";
                if (!op.dst.is_none()) {
                    dump_loc(os, op.dst);
                    if (!op.dst2.is_none()) {
                        os << ",";
                        dump_loc(os, op.dst2);
                    }
                    os << " = ";
                }
                os << t2_lir_kind_name(op.kind);
                for (uint8_t i = 0; i < op.num_srcs; i++) {
                    os << " ";
                    dump_src(os, op.srcs[i]);
                }
                for (uint32_t i = 0; i < op.num_srcs_ext; i++) {
                    os << " ";
                    dump_src(os, fn.src_pool[op.pool_first + i]);
                }
                if (op.kind == T2LirKind::Allocate ||
                    op.kind == T2LirKind::Deallocate ||
                    op.kind == T2LirKind::Trim || op.kind == T2LirKind::GcTest) {
                    os << " " << op.imm;
                    if (op.imm2 != 0) {
                        os << "/" << op.imm2;
                    }
                    os << " live=" << op.live;
                }
                if (op.succ_then != T2_LIR_NO_BLOCK) {
                    os << " -> b" << op.succ_then;
                }
                if (op.succ_else != T2_LIR_NO_BLOCK) {
                    os << " / b" << op.succ_else;
                }
                if (op.kind == T2LirKind::Switch) {
                    for (uint32_t i = 0; i < op.num_cases; i++) {
                        const T2LirSwitchCase &c =
                                fn.switch_cases[op.first_case + i];
                        os << " [" << (SWord)c.value << "->b" << c.target
                           << "]";
                    }
                    os << " default b" << op.default_target;
                }
                if (op.t1_pc_cont != nullptr) {
                    os << " [cont=" << op.t1_pc_cont << "]";
                }
                if (op.target != nullptr) {
                    os << " [target=" << op.target << "]";
                }
                if (op.exp != nullptr) {
                    os << " [export=" << op.exp << "]";
                }
                if (op.t1_pc_fail != nullptr) {
                    os << " [fail=" << op.t1_pc_fail << "]";
                }
                os << "\n";
            }
        }

        return os.str();
    }

} /* namespace erts_t2 */
