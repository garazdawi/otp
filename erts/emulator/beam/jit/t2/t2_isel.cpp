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
 * T2-Full tier-2 JIT: instruction selection (HIR -> LIR). See t2_isel.hpp.
 */

#include "t2_isel.hpp"

extern "C"
{
#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "code_ix.h"
#include "export.h"
#include "beam_code.h"
#include "erl_bif_table.h"

#include "t2_retain.h"
#include "t2_pctab.h"
}

#include <vector>

namespace erts_t2 {

    namespace {

        /* HIR arithmetic/bitwise op -> LIR kind. Returns Invalid if the op
         * is not a generic arithmetic op. */
        T2LirKind arith_kind(T2OpKind k) {
            switch (k) {
            case T2OpKind::Add:
                return T2LirKind::Add;
            case T2OpKind::Sub:
                return T2LirKind::Sub;
            case T2OpKind::Mul:
                return T2LirKind::Mul;
            case T2OpKind::IDiv:
                return T2LirKind::IDiv;
            case T2OpKind::Rem:
                return T2LirKind::Rem;
            case T2OpKind::Band:
                return T2LirKind::Band;
            case T2OpKind::Bor:
                return T2LirKind::Bor;
            case T2OpKind::Bxor:
                return T2LirKind::Bxor;
            case T2OpKind::Bsl:
                return T2LirKind::Bsl;
            case T2OpKind::Bsr:
                return T2LirKind::Bsr;
            case T2OpKind::Bnot:
                return T2LirKind::Bnot;
            case T2OpKind::Neg:
                return T2LirKind::Neg;
            default:
                return T2LirKind::Invalid;
            }
        }

        /* HIR type test / comparison -> LIR kind (all lower to a reused
         * T1 emitter with Fail redirected to the in-blob else-block). */
        T2LirKind guard_kind(T2OpKind k) {
            switch (k) {
            case T2OpKind::IsInteger:
                return T2LirKind::IsInteger;
            case T2OpKind::IsFloat:
                return T2LirKind::IsFloat;
            case T2OpKind::IsNumber:
                return T2LirKind::IsNumber;
            case T2OpKind::IsAtom:
                return T2LirKind::IsAtom;
            case T2OpKind::IsBoolean:
                return T2LirKind::IsBoolean;
            case T2OpKind::IsTuple:
                return T2LirKind::IsTuple;
            case T2OpKind::IsList:
                return T2LirKind::IsList;
            case T2OpKind::IsNonemptyList:
                return T2LirKind::IsNonemptyList;
            case T2OpKind::IsNil:
                return T2LirKind::IsNil;
            case T2OpKind::IsBinary:
                return T2LirKind::IsBinary;
            case T2OpKind::IsBitstring:
                return T2LirKind::IsBitstring;
            case T2OpKind::IsMap:
                return T2LirKind::IsMap;
            case T2OpKind::IsPid:
                return T2LirKind::IsPid;
            case T2OpKind::IsPort:
                return T2LirKind::IsPort;
            case T2OpKind::IsReference:
                return T2LirKind::IsReference;
            case T2OpKind::IsTaggedTuple:
                return T2LirKind::IsTaggedTuple;
            case T2OpKind::TestArity:
                return T2LirKind::TestArity;
            case T2OpKind::CmpEqExact:
                return T2LirKind::CmpEqExact;
            case T2OpKind::CmpNeExact:
                return T2LirKind::CmpNeExact;
            case T2OpKind::CmpEq:
                return T2LirKind::CmpEq;
            case T2OpKind::CmpNe:
                return T2LirKind::CmpNe;
            case T2OpKind::CmpLt:
                return T2LirKind::CmpLt;
            case T2OpKind::CmpLe:
                return T2LirKind::CmpLe;
            case T2OpKind::CmpGt:
                return T2LirKind::CmpGt;
            case T2OpKind::CmpGe:
                return T2LirKind::CmpGe;
            default:
                return T2LirKind::Invalid;
            }
        }

        bool is_const_kind(T2OpKind k) {
            switch (k) {
            case T2OpKind::ConstInt:
            case T2OpKind::ConstAtom:
            case T2OpKind::ConstNil:
            case T2OpKind::ConstLiteral:
                return true;
            default:
                return false;
            }
        }

        struct Isel {
            const T2Function &hir;
            const T2IselContext &ctx;
            T2LirFunction &lir;
            std::string *err;

            /* Per-value use counts, for the guard-fold pattern checks. */
            std::vector<uint32_t> uses;

            bool fail(const std::string &msg) {
                if (err) {
                    *err = msg;
                }
                return false;
            }

            bool fail_op(const T2Op *op, const char *msg) {
                return fail(std::string(msg) + " (" +
                            t2_op_kind_name(op->kind) + ")");
            }

            /* ---- slots and operands ----------------------------------- */

            static PhysLoc reg_loc(int32_t reg) {
                if (t2_reg_is_y(reg)) {
                    return PhysLoc::yreg((uint16_t)t2_reg_index(reg));
                }
                return PhysLoc::xreg((uint16_t)t2_reg_index(reg));
            }

            /* A boxed ConstLiteral term is safe to embed in a standalone
             * blob as a bare immediate (a tagged pointer into the literal
             * area) only when it is a *persistent static* literal of this
             * module: the retained literal_map keeps it alive for as long
             * as the module — and therefore the blob — is loaded. Dynamic
             * literals (bignum immediates synthesized during this decode;
             * the builder recorded index 0 for them, PLAN/T2FULL/07 §"read
             * TAG_q") live only in the transient decode view and must never
             * be embedded. A T2 blob cannot use ArgLiteral either: the JIT
             * literal ArgVal defers to a loader patch (beam_asm_module.cpp
             * emit_constant embeds LLONG_MAX + records literals[i].patches),
             * which standalone blobs never run — so the retained term is the
             * only usable handle. Returns the retained term on success. */
            bool safe_literal_term(const T2Op *def, Eterm *out) {
                if (def->kind != T2OpKind::ConstLiteral || ctx.ret == nullptr) {
                    return false;
                }
                if (def->index >= (Uint32)ctx.ret->literal_count) {
                    return false;
                }
                Eterm term = ctx.ret->literal_map[def->index];
                if (term != def->imm_term) {
                    /* Index-0 placeholder for a dynamic literal, or a stale
                     * mismatch: not the persistent static literal. */
                    return false;
                }
                *out = term;
                return true;
            }

            /* The canonical home of operand i of `op`: its decoded source
             * register when it came from one, otherwise the operand must
             * be an inline-able constant. */
            bool src_of(const T2Op *op, uint16_t i, T2LirSrc *out) {
                int32_t reg = op->operand_regs != nullptr ? op->operand_regs[i]
                                                          : T2_REG_NONE;

                if (reg != T2_REG_NONE) {
                    *out = T2LirSrc::slot(reg_loc(reg), op->operands[i]->id);
                    return true;
                }

                const T2Op *def = op->operands[i]->def;

                if (!is_const_kind(def->kind)) {
                    return fail_op(op,
                                   "operand without a canonical home is not "
                                   "a constant");
                }
                Eterm term = def->kind == T2OpKind::ConstInt
                                     ? make_small(def->imm_int)
                                     : def->imm_term;
                if (!is_immed(term)) {
                    /* Boxed literal operand: only a persistent static
                     * literal may be materialized as a bare immediate (a
                     * tagged literal-area pointer). Correct for value-carrying
                     * contexts (move, put_list/put_tuple2 elements, mixed
                     * arithmetic — the tagged pointer IS the term). Callers
                     * that need term-deep semantics (exact/ordering compares)
                     * reject boxed operands separately (guard_reject_boxed). */
                    if (!safe_literal_term(def, &term)) {
                        return fail_op(
                                op,
                                "boxed literal operand unsupported in P1 "
                                "commit-3 isel");
                    }
                }
                *out = T2LirSrc::immediate(term);
                return true;
            }

            /* Type tests and comparisons must never receive a boxed literal
             * as a bare immediate: emit_is_eq_exact/… only take the term-deep
             * path for an ArgLiteral (loader-resolved), and fall back to a
             * *shallow pointer* compare for a plain immediate — wrong for a
             * boxed term. A T2 blob has no ArgLiteral, so reject and let the
             * function stay T1 for these sites. */
            bool guard_has_boxed_literal(const T2Op *op) {
                for (uint16_t i = 0; i < op->num_operands; i++) {
                    bool from_reg = op->operand_regs != nullptr &&
                                    op->operand_regs[i] != T2_REG_NONE;
                    if (from_reg) {
                        continue;
                    }
                    const T2Op *def = op->operands[i]->def;
                    if (!is_const_kind(def->kind)) {
                        continue;
                    }
                    Eterm term = def->kind == T2OpKind::ConstInt
                                         ? make_small(def->imm_int)
                                         : def->imm_term;
                    if (!is_immed(term)) {
                        return true;
                    }
                }
                return false;
            }

            bool fill_srcs(const T2Op *op, T2LirOp *lop) {
                if (op->num_operands > T2_LIR_MAX_SRCS) {
                    return fail_op(op, "too many operands for inline srcs");
                }
                lop->num_srcs = (uint8_t)op->num_operands;
                for (uint16_t i = 0; i < op->num_operands; i++) {
                    if (!src_of(op, i, &lop->srcs[i])) {
                        return false;
                    }
                }
                return true;
            }

            /* ---- cross-tier resolution -------------------------------- */

            const BeamCodeHeader *code_hdr() const {
                return (const BeamCodeHeader *)ctx.code_hdr;
            }

            const void *pc_lookup(uint32_t beam_idx, ErtsT2PcKind kind) {
                return (const void *)erts_t2_pc_lookup_kind(ctx.ret,
                                                            hir.fn_index,
                                                            beam_idx,
                                                            kind);
            }

            /* Local call target: MFA -> the function's T1 entry (the
             * patchable prologue, where T1's own `bl` lands). */
            const void *local_target(Eterm f, uint32_t arity) {
                const BeamCodeHeader *hdr = code_hdr();

                for (Uint i = 0; i < hdr->num_functions; i++) {
                    const ErtsCodeInfo *ci = hdr->functions[i];

                    if (ci->mfa.function == f && ci->mfa.arity == arity) {
                        return (const void *)erts_codeinfo_to_code(ci);
                    }
                }
                return nullptr;
            }

            /* A speculative op's deopt PC (PLAN/T2FULL/09 §2). Boundary
             * shape: the op's own T1 EFFECT site (T1 re-executes just
             * that op from its sync-map state). Window shape: the
             * function's own T1 entry body past the prologue patch and
             * the i_test_yield charge — T1 re-executes the whole
             * iteration/invocation from the fresh-call vector in
             * X0..arity-1 (charging happened at the T2 back edge /
             * entry stub, so reductions stay identical to T1). */
            const void *spec_deopt_pc(const T2Op *op) {
                if (op->flags & T2_OP_SPEC_BOUNDARY) {
                    return pc_lookup(op->beam_idx, ERTS_T2_PC_EFFECT);
                }

                if (op->flags & T2_OP_SPEC_CALLSITE) {
                    /* Callsite class (maps:fold Stage 1): re-execute
                     * the ERASED CALL — the side exit branches to the
                     * call site's own T1 PC (no CP push; the sync map
                     * is the call-boundary state, physically intact by
                     * the callsite rule in t2_validate_windows). */
                    return pc_lookup(op->beam_idx, ERTS_T2_PC_CALL);
                }

                if (op->flags & T2_OP_SPEC_REDISPATCH) {
                    /* Re-dispatch class (P1a): re-invoke the generic
                     * callee with the LOOP-CARRIED state. Inner mode
                     * (imm_int = the terminal inlined loop function's
                     * L_f): the side exit enters that function's body
                     * past its entry check — the back edge pre-charged
                     * this iteration's entry — over its own fresh-call
                     * vector in X0..arity-1 (no CP push at a tail
                     * site; a body site's trampoline pushes the site's
                     * T1 CONT — fill_spec_cont), so T1 continues the
                     * fold exactly where generic execution would be,
                     * with T1-exact reductions and error frames.
                     * Otherwise the side exit branches to the call
                     * site's own T1 PC over the current X vector (no
                     * CP push; T1's own call instruction re-establishes
                     * the CP for a body site and tail-transfers for a
                     * tail site), so T1 continues the fold from
                     * element k with no redo. */
                    if (op->imm_int != 0) {
                        return (const void
                                        *)((const char *)(UWord)op->imm_int +
                                           erts_t2_test_yield_return_offset());
                    }
                    return pc_lookup(op->beam_idx, ERTS_T2_PC_CALL);
                }

                if (op->flags & T2_OP_WINDOW_CALLEE) {
                    /* Intrinsic-loop window deopt (P2 commit 8): the
                     * iteration re-executes as a fresh CALLEE call —
                     * the callee body past its entry check (imm_int =
                     * callee L_f). The trampoline additionally pushes
                     * the call site's CONT as CP (fill_spec_cont). */
                    if (op->imm_int == 0) {
                        return nullptr;
                    }
                    return (const void *)((const char *)(UWord)op->imm_int +
                                          erts_t2_test_yield_return_offset());
                }

                /* Window shape, and the entry class (T2_OP_SPEC_ENTRY,
                 * the make_fun sink): both branch to the function's
                 * own T1 entry body and re-execute the invocation from
                 * the fresh-call vector in X0..arity-1. */
                const void *lf = local_target(hir.function, hir.arity);

                if (lf == nullptr) {
                    return nullptr;
                }
                return (const void *)((const char *)lf +
                                      erts_t2_test_yield_return_offset());
            }

            /* For a spec op whose deopt trampoline must push a CP —
             * a window-callee op, or a BODY-site inner re-dispatch op
             * (T2_OP_SPEC_REDISPATCH with a callee L_f but no
             * T2_OP_TAIL_SITE; the skipped callee prologue would have
             * pushed it) — resolve the intrinsic call site's T1
             * continuation; no-op otherwise. Returns false when the
             * lookup fails. */
            bool fill_spec_cont(const T2Op *op, T2LirOp *lop) {
                bool need = (op->flags & T2_OP_WINDOW_CALLEE) != 0 ||
                            ((op->flags & T2_OP_SPEC_REDISPATCH) != 0 &&
                             op->imm_int != 0 &&
                             (op->flags & T2_OP_TAIL_SITE) == 0);

                if (!need) {
                    return true;
                }
                lop->t1_pc_cont = pc_lookup(op->beam_idx, ERTS_T2_PC_CONT);
                return lop->t1_pc_cont != nullptr;
            }

            /* The function's own func_info: on aarch64 the ErtsCodeInfo's
             * first word is a valid `bl <i_func_info_shared>`, so branching
             * to it raises function_clause exactly as T1's guard fails do. */
            const void *func_info_target() {
                const BeamCodeHeader *hdr = code_hdr();

                if ((Sint)hir.fn_index >= (Sint)hdr->num_functions) {
                    return nullptr;
                }

                const ErtsCodeInfo *ci = hdr->functions[hir.fn_index];

                /* Chunk order and header order must agree (drift check). */
                if (ci->mfa.function != hir.function ||
                    ci->mfa.arity != hir.arity) {
                    return nullptr;
                }
                return (const void *)ci;
            }

            /* The closed set of call_ext targets the loader transforms
             * into dedicated instructions (ops.tab `u$func:` rules) that
             * are NOT plain BIFs, so ep->bif_number cannot flag them:
             * i_yield / i_hibernate / i_apply* / the load_nif and
             * on_load sequences / the os:perf_counter inline. T1 never
             * emits an export call for these — their instructions have
             * their own scheduling and reduction accounting (found the
             * hard way: a generic export call for erlang:yield/0 charges
             * ~1000 extra reductions per call vs T1's i_yield, breaking
             * process_SUITE:yield's measurement). */
            bool is_transformed_call_ext(Eterm m, Eterm f, Uint arity) {
                /* Non-predefined atoms interned once. */
                static const Eterm hibernate = ERTS_MAKE_AM("hibernate");
                static const Eterm load_nif = ERTS_MAKE_AM("load_nif");
                static const Eterm on_load_fn =
                        ERTS_MAKE_AM("call_on_load_function");
                static const Eterm os_mod = ERTS_MAKE_AM("os");
                static const Eterm perf_counter = ERTS_MAKE_AM("perf_counter");

                if (m == am_erlang) {
                    if ((f == am_yield && arity == 0) ||
                        (f == hibernate && arity == 0) ||
                        (f == am_apply && (arity == 2 || arity == 3)) ||
                        (f == load_nif && arity == 2) ||
                        (f == on_load_fn && arity == 1)) {
                        return true;
                    }
                } else if (m == os_mod) {
                    if (f == perf_counter && arity == 0) {
                        return true;
                    }
                }
                return false;
            }

            /* Classification of a call_ext-family target, mirroring the
             * loader's transform (ops.tab): a real Erlang call (export
             * dispatch, identical to T1's i_call_ext) or a light BIF
             * (T1's call_light_bif; lowered via the T2 fragment with the
             * yield/trap redirections). Heavy BIFs (is_heavy_bif ->
             * i_call_ext to the BIF's export trampoline) and the
             * loader-transformed specials are rejected — out of the P1
             * lowering's scope. */
            enum class ExtTarget { Erlang, LightBif };

            bool export_target(const T2Op *op,
                               const Export **out,
                               ExtTarget *kind) {
                const Export *ep;

                if (is_transformed_call_ext(op->mfa_m, op->mfa_f, op->index)) {
                    return fail_op(op,
                                   "call_ext to a loader-transformed "
                                   "special (i_yield/i_hibernate/...) "
                                   "unsupported in P1 isel");
                }

                ep = erts_active_export_entry(op->mfa_m, op->mfa_f, op->index);

                if (ep == nullptr) {
                    /* Not-yet-loaded remote callee: mirror T1's load-time
                     * erts_export_get_or_make_stub (asm_load.c import
                     * resolution) — the same permanent Export* T1
                     * dispatches through, whose entry is filled when the
                     * target module loads (until then it dispatches to the
                     * undefined-function handler, exactly as T1). This
                     * mutates the *staging* export table, so it is only
                     * safe while we hold the loader's code-load permission
                     * (the corpus sweep + the P1 compile-at-load driver run
                     * inside beam_load_finalize_code, which asserts it).
                     * Without it (e.g. the standalone debug-exec BIF) we
                     * must not touch staging — reject and stay T1. A BIF
                     * always has an active export from init, so a missing
                     * active entry is never a BIF; the stub's bif_number is
                     * -1 and the check below passes it through as a normal
                     * remote call. */
                    if (!ctx.allow_stub) {
                        return fail_op(op, "no export entry for remote target");
                    }
                    ep = erts_export_get_or_make_stub(op->mfa_m,
                                                      op->mfa_f,
                                                      op->index);
                    if (ep == nullptr) {
                        return fail_op(op, "no export entry for remote target");
                    }
                }
                if (ep->bif_number >= 0) {
                    if (bif_table[ep->bif_number].kind == BIF_KIND_HEAVY) {
                        /* T1 emits i_call_ext (a plain export call) for
                         * these; the pctab CALL/CONT entries exist, but
                         * lowering them is outside the decided P1 BIF
                         * design — reject, the function stays T1. */
                        return fail_op(op,
                                       "call_ext to a heavy BIF "
                                       "(i_call_ext dispatch) unsupported "
                                       "in P1 isel");
                    }
                    *kind = ExtTarget::LightBif;
                    *out = ep;
                    return true;
                }
                *kind = ExtTarget::Erlang;
                *out = ep;
                return true;
            }

            /* Rewrite a lowered CallExt-shaped LIR op into a CallBif
             * (T1's call_light_bif). The caller has resolved lop->exp
             * and lop->t1_pc_cont (the trap/trace CP); this adds the
             * BIF's C function and the site's own T1 PC — the yield
             * resume/raise address (the BIF has not run when a yield
             * fires, so T1 re-executing the whole site is correct). */
            bool lower_bif_call(const T2Op *op, T2LirOp *lop) {
                const Export *ep = (const Export *)lop->exp;

                ASSERT(ep != nullptr && ep->bif_number >= 0);

                lop->kind = T2LirKind::CallBif;
                lop->target = (const void *)bif_table[ep->bif_number].f;
                lop->t1_pc_fail = pc_lookup(op->beam_idx, ERTS_T2_PC_BIF);
                if (lop->t1_pc_fail == nullptr) {
                    return fail_op(op,
                                   "no BIF pctab entry for the bif call "
                                   "site");
                }
                return true;
            }

            /* ---- guard folding ---------------------------------------- */

            /* True iff `op`'s boolean result is consumed exactly once, by
             * this block's Branch terminator. */
            bool feeds_branch(const T2Op *op) {
                const T2Op *term = op->block->terminator;

                return term != nullptr && term->kind == T2OpKind::Branch &&
                       term->num_operands == 1 &&
                       term->operands[0] == op->result &&
                       uses[op->result->id] == 1;
            }

            /* ---- op lowering ------------------------------------------ */

            /* Returns false on failure; sets *consumed_terminator when the
             * lowered op absorbed the block's Branch terminator. */
            bool emit_op(T2LirBlock &b,
                         const T2Op *op,
                         const T2Op **skip_until,
                         bool *consumed_terminator) {
                T2LirOp lop;

                lop.beam_idx = op->beam_idx;
                /* The originating op's sync map rides along (null on
                 * non-sync ops): it is the P2 allocator's pin set. */
                lop.sync = op->sync;

                /* P2 loop unboxing: the re-tag mask (cold-path ORR set
                 * for the emitter), the raw result marker, and the
                 * per-operand rawness bits ride along 1:1. The HIR
                 * validator (run_raw_checks + the sync-map raw_mask
                 * rule) proved the discipline; raw_srcs bits on ops
                 * whose emitters ignore them are inert. */
                lop.raw_mask = op->raw_mask;
                lop.raw_dst = (op->flags & T2_OP_RAW_MODE) != 0 &&
                              op->result != nullptr &&
                              t2_value_is_raw_home(op->result);
                for (uint16_t i = 0;
                     i < op->num_operands && i < T2_LIR_MAX_SRCS;
                     i++) {
                    if (t2_value_is_raw_home(op->operands[i])) {
                        lop.raw_srcs |= (uint8_t)(1u << i);
                    }
                }

                /* Generic arithmetic (gc_bif): fail edge in-blob when the
                 * builder's Succeeded/Branch follows; else side exit to the
                 * op's own T1 EFFECT site. */
                T2LirKind ak = arith_kind(op->kind);
                if (ak != T2LirKind::Invalid) {
                    lop.kind = ak;
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "arith result without a home");
                    }
                    lop.dst = reg_loc(op->dst_reg);
                    lop.dst_value = op->result->id;
                    lop.mfa_m = op->mfa_m;
                    lop.mfa_f = op->mfa_f;
                    lop.live = op->live;
                    if (!fill_srcs(op, &lop)) {
                        return false;
                    }
                    if (lop.raw_srcs != 0) {
                        /* P-C B2 backstop: a raw operand may reach only
                         * the flag-checked AddSmall/SubSmall (the
                         * converted ROLLBACK accumulator). If the
                         * speculation pass did not convert it (struck
                         * candidate), refuse loudly — the generic
                         * gc_bif would consume the raw word as a term. */
                        return fail_op(op,
                                       "raw operand reaching generic "
                                       "arithmetic (unconverted roll-back "
                                       "accumulator)");
                    }

                    const T2Op *succ = op->next;
                    if (succ != nullptr && succ->kind == T2OpKind::Succeeded &&
                        succ->num_operands == 1 &&
                        succ->operands[0] == op->result && feeds_branch(succ)) {
                        const T2Op *term = op->block->terminator;

                        lop.succ_then = term->succ_then->id;
                        lop.succ_else = term->succ_else->id;
                        *skip_until = succ; /* consume the Succeeded */
                        *consumed_terminator = true;
                    } else if (succ != nullptr &&
                               succ->kind == T2OpKind::Succeeded) {
                        return fail_op(op,
                                       "succeeded not consumed by the block "
                                       "branch");
                    } else {
                        lop.t1_pc_fail =
                                pc_lookup(op->beam_idx, ERTS_T2_PC_EFFECT);
                        if (lop.t1_pc_fail == nullptr) {
                            return fail_op(op,
                                           "no EFFECT pctab entry for arith "
                                           "side exit");
                        }
                    }
                    b.ops.push_back(lop);
                    return true;
                }

                /* Type tests / comparisons: must fold into the Branch. */
                T2LirKind gk = guard_kind(op->kind);
                if (gk == T2LirKind::TestArity &&
                    (op->flags & T2_OP_TUPLE_ARITY_FUSED) != 0) {
                    /* The shape-up's is_tuple+test_arity fusion: lower
                     * to T1's own fused emitter. */
                    gk = T2LirKind::IsTupleOfArity;
                }
                if (gk != T2LirKind::Invalid && op->dst_reg != T2_REG_NONE &&
                    op->result != nullptr) {
                    /* Value-producing total comparison (P2 commit 8;
                     * bif2 {f,0} erlang:CMP/2): lower via T1's
                     * bif_is_* boolean emitters. Only the subset the
                     * eligibility scan admits reaches here. eq/ne
                     * additionally reject boxed-literal operands
                     * (their T1 emitters shallow-compare immediates,
                     * which a baked literal pointer is not). */
                    switch (op->kind) {
                    case T2OpKind::CmpGe:
                    case T2OpKind::CmpLt:
                    case T2OpKind::CmpLe:
                    case T2OpKind::CmpGt:
                        break;
                    case T2OpKind::CmpEqExact:
                    case T2OpKind::CmpNeExact:
                        if (guard_has_boxed_literal(op)) {
                            return fail_op(op,
                                           "boxed literal in value "
                                           "eq/ne unsupported");
                        }
                        break;
                    default:
                        return fail_op(op,
                                       "unsupported value-producing "
                                       "guard");
                    }
                    lop.kind = T2LirKind::CmpBool;
                    lop.imm = (Sint64)op->kind;
                    lop.dst = reg_loc(op->dst_reg);
                    lop.dst_value = op->result->id;
                    if (!fill_srcs(op, &lop)) {
                        return false;
                    }
                    if (op->kind == T2OpKind::CmpEqExact ||
                        op->kind == T2OpKind::CmpNeExact) {
                        /* T1's bif_is_eq/ne_exact emitters take a
                         * register first operand; both comparisons are
                         * symmetric, so swap a constant to the right. */
                        if (lop.srcs[0].is_const && lop.srcs[1].is_const) {
                            return fail_op(op,
                                           "constant-constant eq/ne "
                                           "unsupported");
                        }
                        if (lop.srcs[0].is_const) {
                            T2LirSrc tmp = lop.srcs[0];

                            lop.srcs[0] = lop.srcs[1];
                            lop.srcs[1] = tmp;
                        }
                    }
                    b.ops.push_back(lop);
                    return true;
                }
                if (gk != T2LirKind::Invalid) {
                    if (!feeds_branch(op) || op->next != nullptr) {
                        return fail_op(op,
                                       "guard result not consumed by this "
                                       "block's branch");
                    }
                    if (guard_has_boxed_literal(op)) {
                        return fail_op(op,
                                       "boxed literal operand in comparison/"
                                       "test unsupported (needs literal-pool "
                                       "deep compare) in P1 isel");
                    }
                    const T2Op *term = op->block->terminator;

                    lop.kind = gk;
                    lop.imm = (Sint64)op->index; /* arity for TestArity /
                                                  * IsTaggedTuple      */
                    lop.imm_term = op->imm_term; /* IsTaggedTuple tag  */
                    if (!fill_srcs(op, &lop)) {
                        return false;
                    }
                    lop.succ_then = term->succ_then->id;
                    lop.succ_else = term->succ_else->id;
                    *consumed_terminator = true;
                    b.ops.push_back(lop);
                    return true;
                }

                switch (op->kind) {
                case T2OpKind::Param:
                    /* Argument already in its X register at entry. */
                    if (op->dst_reg != (int32_t)op->index) {
                        return fail_op(op, "param home mismatch");
                    }
                    if (op->result->id < lir.num_values) {
                        lir.param_x[op->result->id] = (int32_t)op->index;
                    }
                    return true;

                case T2OpKind::ConstInt:
                case T2OpKind::ConstAtom:
                case T2OpKind::ConstNil:
                case T2OpKind::ConstLiteral:
                    /* Materialized only when decoded into a register
                     * (init_yregs kills, move-of-constant); pure operand
                     * constants emit nothing. */
                    if (op->dst_reg == T2_REG_NONE) {
                        return true;
                    }
                    {
                        Eterm term = op->kind == T2OpKind::ConstInt
                                             ? make_small(op->imm_int)
                                             : op->imm_term;

                        if (!is_immed(term) && !safe_literal_term(op, &term)) {
                            return fail_op(op,
                                           "boxed literal move unsupported "
                                           "in P1 commit-3 isel");
                        }

                        /* P2 loop unboxing: a raw ConstInt materializes
                         * in the tag-cleared representation (checked
                         * above in its tagged form — the cleared word
                         * is not a term and must dodge the immediate
                         * check). */
                        if (op->kind == T2OpKind::ConstInt &&
                            (op->flags & T2_OP_RAW_MODE) != 0) {
                            term &= ~(Eterm)_TAG_IMMED1_MASK;
                        }
                        lop.kind = T2LirKind::Move;
                        lop.dst = reg_loc(op->dst_reg);
                        lop.dst_value = op->result->id;
                        lop.num_srcs = 1;
                        lop.srcs[0] = T2LirSrc::immediate(term);
                        b.ops.push_back(lop);
                    }
                    return true;

                case T2OpKind::Copy: {
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "copy without a destination home");
                    }

                    /* A flagged pair is a decoded swap: reads precede
                     * writes; emit fused. */
                    if (op->flags & T2_OP_PAIR_HEAD) {
                        const T2Op *tail = op->next;

                        if (tail == nullptr || tail->kind != T2OpKind::Copy ||
                            tail->dst_reg == T2_REG_NONE) {
                            return fail_op(op, "dangling swap pair");
                        }
                        lop.kind = T2LirKind::Swap;
                        lop.dst = reg_loc(op->dst_reg);
                        lop.dst2 = reg_loc(tail->dst_reg);
                        lop.dst_value = op->result->id;
                        lop.dst2_value = tail->result->id;
                        /* Allocator annotations: both reads precede both
                         * writes (emit ignores srcs for Swap). */
                        lop.num_srcs = 2;
                        if (!src_of(op, 0, &lop.srcs[0]) ||
                            !src_of(tail, 0, &lop.srcs[1])) {
                            return false;
                        }
                        b.ops.push_back(lop);
                        *skip_until = tail;
                        return true;
                    }

                    lop.kind = T2LirKind::Move;
                    lop.dst = reg_loc(op->dst_reg);
                    lop.dst_value = op->result->id;
                    lop.num_srcs = 1;
                    if (!src_of(op, 0, &lop.srcs[0])) {
                        return false;
                    }
                    b.ops.push_back(lop);
                    return true;
                }

                case T2OpKind::UntagInt:
                case T2OpKind::TagInt:
                    /* P2 loop unboxing: one AND (clear the small tag) /
                     * one ORR (restore it), in the value's home. Only
                     * the RAW-IN-HOME form has a lowering; the legacy
                     * phys-only UntagInt never reaches isel. */
                    if (op->kind == T2OpKind::UntagInt &&
                        (op->flags & T2_OP_RAW_MODE) == 0) {
                        return fail_op(op,
                                       "phys-discipline untag_int has no "
                                       "P1 lowering");
                    }
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "un/tag without a home");
                    }
                    lop.kind = op->kind == T2OpKind::UntagInt
                                       ? T2LirKind::UntagInt
                                       : T2LirKind::TagInt;
                    lop.dst = reg_loc(op->dst_reg);
                    lop.dst_value = op->result->id;
                    lop.num_srcs = 1;
                    if (!src_of(op, 0, &lop.srcs[0])) {
                        return false;
                    }
                    if (lop.srcs[0].is_const) {
                        return fail_op(op, "un/tag of a constant");
                    }
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::GetHd:
                case T2OpKind::GetTl: {
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "list access without a home");
                    }

                    if (op->flags & T2_OP_PAIR_HEAD) {
                        /* Decoded get_list: destinations may alias the
                         * source; emit fused (emit_get_list handles all
                         * overlaps, as T1 does). */
                        const T2Op *tail = op->next;

                        if (op->kind != T2OpKind::GetHd || tail == nullptr ||
                            tail->kind != T2OpKind::GetTl ||
                            tail->dst_reg == T2_REG_NONE) {
                            return fail_op(op, "dangling get_list pair");
                        }
                        lop.kind = T2LirKind::GetList;
                        lop.dst = reg_loc(op->dst_reg);
                        lop.dst2 = reg_loc(tail->dst_reg);
                        lop.dst_value = op->result->id;
                        lop.dst2_value = tail->result->id;
                        lop.num_srcs = 1;
                        if (!src_of(op, 0, &lop.srcs[0])) {
                            return false;
                        }
                        b.ops.push_back(lop);
                        *skip_until = tail;
                        return true;
                    }

                    lop.kind = op->kind == T2OpKind::GetHd ? T2LirKind::GetHd
                                                           : T2LirKind::GetTl;
                    lop.dst = reg_loc(op->dst_reg);
                    lop.dst_value = op->result->id;
                    lop.num_srcs = 1;
                    if (!src_of(op, 0, &lop.srcs[0])) {
                        return false;
                    }
                    b.ops.push_back(lop);
                    return true;
                }

                case T2OpKind::GetTupleElement:
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "tuple access without a home");
                    }
                    lop.kind = T2LirKind::GetTupleElement;
                    lop.dst = reg_loc(op->dst_reg);
                    lop.dst_value = op->result->id;
                    lop.imm = (Sint64)op->index;
                    lop.num_srcs = 1;
                    if (!src_of(op, 0, &lop.srcs[0])) {
                        return false;
                    }
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::MakeList:
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "put_list without a home");
                    }
                    lop.kind = T2LirKind::MakeList;
                    lop.dst = reg_loc(op->dst_reg);
                    lop.dst_value = op->result->id;
                    if (!fill_srcs(op, &lop)) {
                        return false;
                    }
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::MakeFun: {
                    /* make_fun3 (P2 commit 8): the ErlFunEntry comes
                     * from the retained lambda table, captured at
                     * finalize — always present for a loaded
                     * instance. */
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "make_fun3 without a home");
                    }
                    if (ctx.ret->lambdas == NULL ||
                        op->index >= (uint32_t)ctx.ret->lambda_count ||
                        ctx.ret->lambdas[op->index].fun_entry == NULL) {
                        return fail_op(op,
                                       "make_fun3 without a resolved "
                                       "fun entry");
                    }
                    lop.kind = T2LirKind::MakeFun;
                    lop.dst = reg_loc(op->dst_reg);
                    lop.dst_value = op->result->id;
                    lop.imm = (Sint64)(ctx.ret->lambdas[op->index].arity -
                                       ctx.ret->lambdas[op->index].num_free);
                    lop.imm2 = (Sint64)op->live;
                    lop.target = ctx.ret->lambdas[op->index].fun_entry;
                    if (op->num_operands <= T2_LIR_MAX_SRCS) {
                        if (!fill_srcs(op, &lop)) {
                            return false;
                        }
                    } else {
                        lop.pool_first = (uint32_t)lir.src_pool.size();
                        lop.num_srcs_ext = op->num_operands;
                        for (uint16_t i = 0; i < op->num_operands; i++) {
                            T2LirSrc s;

                            if (!src_of(op, i, &s)) {
                                return false;
                            }
                            lir.src_pool.push_back(s);
                        }
                    }
                    b.ops.push_back(lop);
                    return true;
                }

                case T2OpKind::MakeTuple: {
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "put_tuple2 without a home");
                    }
                    lop.kind = T2LirKind::MakeTuple;
                    lop.dst = reg_loc(op->dst_reg);
                    lop.dst_value = op->result->id;
                    if (op->num_operands <= T2_LIR_MAX_SRCS) {
                        if (!fill_srcs(op, &lop)) {
                            return false;
                        }
                    } else {
                        lop.pool_first = (uint32_t)lir.src_pool.size();
                        lop.num_srcs_ext = op->num_operands;
                        for (uint16_t i = 0; i < op->num_operands; i++) {
                            T2LirSrc s;

                            if (!src_of(op, i, &s)) {
                                return false;
                            }
                            lir.src_pool.push_back(s);
                        }
                    }
                    b.ops.push_back(lop);
                    return true;
                }

                case T2OpKind::UpdateRecord: {
                    /* Operands [Src, cidx0, val0, ...] copied verbatim into
                     * the LIR (src_pool when they overflow the inline
                     * array, exactly like MakeTuple). Each cidxK lowers to
                     * an is_const small carrying the 1-based position;
                     * emit reconstructs the updates span from it. imm =
                     * the tuple arity (Size), imm2 = the hint code. */
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "update_record without a home");
                    }
                    lop.kind = T2LirKind::UpdateRecord;
                    lop.dst = reg_loc(op->dst_reg);
                    lop.dst_value = op->result->id;
                    lop.imm = op->imm_int;         /* tuple arity (Size) */
                    lop.imm2 = (Sint64)op->index;  /* hint: 0 copy, 1 reuse */
                    if (op->num_operands <= T2_LIR_MAX_SRCS) {
                        if (!fill_srcs(op, &lop)) {
                            return false;
                        }
                    } else {
                        lop.pool_first = (uint32_t)lir.src_pool.size();
                        lop.num_srcs_ext = op->num_operands;
                        for (uint16_t i = 0; i < op->num_operands; i++) {
                            T2LirSrc s;

                            if (!src_of(op, i, &s)) {
                                return false;
                            }
                            lir.src_pool.push_back(s);
                        }
                    }
                    b.ops.push_back(lop);
                    return true;
                }

                case T2OpKind::PutMap: {
                    /* Single-pair put_map_assoc: srcs [Map, Key, Value]
                     * (all inline), imm = the decoded Live. */
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "put_map without a home");
                    }
                    lop.kind = T2LirKind::PutMap;
                    lop.dst = reg_loc(op->dst_reg);
                    lop.dst_value = op->result->id;
                    lop.imm = op->imm_int; /* decoded Live */
                    if (!fill_srcs(op, &lop)) {
                        return false;
                    }
                    b.ops.push_back(lop);
                    return true;
                }

                case T2OpKind::CatchSetup:
                case T2OpKind::TryEnd:
                    /* try / try_end (exceptions, Strategy 2): each produces
                     * the Y catch-tag slot value (the tag / NIL). No source
                     * operands — the stored immediate rides in imm_term
                     * (the make_catch tag, or NIL for try_end). */
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "catch op without a Y home");
                    }
                    lop.kind = op->kind == T2OpKind::CatchSetup
                                       ? T2LirKind::CatchSetup
                                       : T2LirKind::TryEnd;
                    lop.dst = reg_loc(op->dst_reg);
                    lop.dst_value = op->result->id;
                    lop.imm_term = op->imm_term;
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::GcTest:
                    lop.kind = T2LirKind::GcTest;
                    lop.imm = (Sint64)op->index; /* heap words */
                    lop.live = op->live;
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::Allocate:
                    lop.kind = T2LirKind::Allocate;
                    lop.imm = (Sint64)op->index; /* stack slots */
                    lop.imm2 = op->imm_int;      /* fused heap words */
                    lop.live = op->live;
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::Deallocate:
                    lop.kind = T2LirKind::Deallocate;
                    lop.imm = (Sint64)op->index;
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::Trim:
                    lop.kind = T2LirKind::Trim;
                    lop.imm = (Sint64)op->index;
                    lop.imm2 = op->imm_int;
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::Call:
                case T2OpKind::CallExt: {
                    bool is_ext = op->kind == T2OpKind::CallExt;

                    lop.kind = is_ext ? T2LirKind::CallExt : T2LirKind::Call;
                    lop.mfa_m = op->mfa_m;
                    lop.mfa_f = op->mfa_f;
                    lop.arity = op->index;
                    lop.live = op->live;
                    lop.dst = PhysLoc::xreg(0);
                    lop.dst_value = op->result->id;

                    /* The CP: the T1 post-call continuation of this call's
                     * decode ordinal — never a T2 address (08 §4.3). The
                     * callee returns into T1; the rest of the invocation
                     * runs T1 (demote-on-return). A light-BIF site needs
                     * the same address for its trap/trace CP. */
                    lop.t1_pc_cont = pc_lookup(op->beam_idx, ERTS_T2_PC_CONT);
                    if (lop.t1_pc_cont == nullptr) {
                        return fail_op(op,
                                       "no CONT pctab entry for the call's "
                                       "T1 continuation");
                    }

                    if (is_ext) {
                        const Export *ep = nullptr;
                        ExtTarget kind = ExtTarget::Erlang;

                        if (!export_target(op, &ep, &kind)) {
                            return false;
                        }
                        lop.exp = (const void *)ep;

                        if (kind == ExtTarget::LightBif) {
                            if (!lower_bif_call(op, &lop)) {
                                return false;
                            }
                        }
                    } else {
                        lop.target = local_target(op->mfa_f, op->index);
                        if (lop.target == nullptr) {
                            return fail_op(op, "local call target not found");
                        }
                    }
                    b.ops.push_back(lop);
                    return true;
                }

                case T2OpKind::GetMapElement: {
                    /* One decoded map-key lookup (WIN 1): read-only, no
                     * GC, no trap; the dst is written on the success
                     * edge only, and the builder always guards it with
                     * Succeeded/Branch to the decoded fail label — both
                     * edges stay in the blob. */
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "get_map_element without a home");
                    }
                    lop.kind = T2LirKind::GetMapElement;
                    lop.dst = reg_loc(op->dst_reg);
                    lop.dst_value = op->result->id;
                    if (!fill_srcs(op, &lop)) {
                        return false;
                    }
                    if (lop.srcs[0].is_const) {
                        /* The scan admits only register sources; a
                         * constant map here is drift. */
                        return fail_op(op, "get_map_element of a constant");
                    }

                    const T2Op *succ = op->next;
                    if (succ != nullptr && succ->kind == T2OpKind::Succeeded &&
                        succ->num_operands == 1 &&
                        succ->operands[0] == op->result && feeds_branch(succ)) {
                        const T2Op *term = op->block->terminator;

                        lop.succ_then = term->succ_then->id;
                        lop.succ_else = term->succ_else->id;
                        *skip_until = succ;
                        *consumed_terminator = true;
                    } else {
                        /* The builder always guards get_map_element with
                         * a real fail label; anything else is drift. */
                        return fail_op(op,
                                       "get_map_element without a folded "
                                       "fail edge");
                    }
                    b.ops.push_back(lop);
                    return true;
                }

                case T2OpKind::GuardBif: {
                    /* The read-only guard-BIF subset (WIN 3): lowered
                     * onto T1's dedicated guard-BIF emitters. Fail
                     * edge like arith: in-blob when the builder's
                     * Succeeded/Branch follows (a real fail label);
                     * else a side exit to the op's own T1 EFFECT site
                     * ({f,0} — T1 re-executes and raises, T2 never
                     * raises). Read-only, no GC, no trap; the dst is
                     * written on the success path only. */
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "guard bif result without a home");
                    }
                    lop.kind = T2LirKind::GuardBif;
                    lop.dst = reg_loc(op->dst_reg);
                    lop.dst_value = op->result->id;
                    lop.mfa_m = op->mfa_m;
                    lop.mfa_f = op->mfa_f;
                    lop.arity = op->index;
                    if (!fill_srcs(op, &lop)) {
                        return false;
                    }
                    /* T1's emitters take a *register* for bif_node's S
                     * operand, the map argument of bif_map_get /
                     * bif_is_map_key and the hd/tl list — all admitted
                     * as registers by the scan; a constant here is
                     * drift. */
                    if (lop.num_srcs < 1 || lop.num_srcs > 2 ||
                        ((op->mfa_f == am_node || op->mfa_f == am_hd ||
                          op->mfa_f == am_tl) &&
                         lop.srcs[0].is_const) ||
                        ((op->mfa_f == am_map_get ||
                          op->mfa_f == am_is_map_key) &&
                         lop.srcs[1].is_const)) {
                        return fail_op(op,
                                       "guard bif operand outside the "
                                       "decoded shape");
                    }
                    {
                        /* The BIF's C entry: is_map_key's T1 emitter
                         * falls back to the generic i_bif2 runtime
                         * call for an untyped map operand and needs
                         * the function pointer. Guard BIFs always
                         * have an active export from init. */
                        const Export *ep = erts_active_export_entry(op->mfa_m,
                                                                    op->mfa_f,
                                                                    op->index);

                        if (ep == nullptr || ep->bif_number < 0) {
                            return fail_op(op,
                                           "guard bif without a bif export");
                        }
                        lop.target = (const void *)bif_table[ep->bif_number].f;
                    }

                    const T2Op *succ = op->next;
                    if (succ != nullptr && succ->kind == T2OpKind::Succeeded &&
                        succ->num_operands == 1 &&
                        succ->operands[0] == op->result && feeds_branch(succ)) {
                        const T2Op *term = op->block->terminator;

                        lop.succ_then = term->succ_then->id;
                        lop.succ_else = term->succ_else->id;
                        *skip_until = succ;
                        *consumed_terminator = true;
                    } else if (succ != nullptr &&
                               succ->kind == T2OpKind::Succeeded) {
                        return fail_op(op,
                                       "succeeded not consumed by the block "
                                       "branch");
                    } else {
                        lop.t1_pc_fail =
                                pc_lookup(op->beam_idx, ERTS_T2_PC_EFFECT);
                        if (lop.t1_pc_fail == nullptr) {
                            return fail_op(op,
                                           "no EFFECT pctab entry for the "
                                           "guard-bif side exit");
                        }
                    }
                    b.ops.push_back(lop);
                    return true;
                }

                case T2OpKind::StartMatch: {
                    /* bs_start_match3 (P2 commit 7): conditional dst
                     * write; the builder's Succeeded/Branch folds into
                     * the op's edges. A decoded {f,0} fail emits with
                     * no fail edge (T1 skips the checks the compiler
                     * proved away). */
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "start_match without a home");
                    }
                    lop.kind = T2LirKind::StartMatch;
                    lop.dst = reg_loc(op->dst_reg);
                    lop.dst_value = op->result->id;
                    lop.live = op->live;
                    lop.num_srcs = 1;
                    if (!src_of(op, 0, &lop.srcs[0])) {
                        return false;
                    }
                    if (lop.srcs[0].is_const) {
                        return fail_op(op, "start_match of a constant");
                    }

                    const T2Op *succ = op->next;
                    if (succ != nullptr && succ->kind == T2OpKind::Succeeded &&
                        succ->num_operands == 1 &&
                        succ->operands[0] == op->result && feeds_branch(succ)) {
                        const T2Op *term = op->block->terminator;

                        lop.succ_then = term->succ_then->id;
                        lop.succ_else = term->succ_else->id;
                        *skip_until = succ;
                        *consumed_terminator = true;
                    } else if (succ != nullptr &&
                               succ->kind == T2OpKind::Succeeded) {
                        return fail_op(op,
                                       "succeeded not consumed by the "
                                       "block branch");
                    }
                    b.ops.push_back(lop);
                    return true;
                }

                case T2OpKind::BsMatch: {
                    /* The byte-aligned bs_match subset: commands ride
                     * the function-level pool; the (single) dst is
                     * written on the success edge only. */
                    static_assert(ERTS_T2_BS_ENSURE == 0 &&
                                          ERTS_T2_BS_READ_INT8 == 1 &&
                                          ERTS_T2_BS_SKIP == 2 &&
                                          ERTS_T2_BS_GET_TAIL == 3,
                                  "T2LirBsCmd mirrors ErtsT2BsCmdKind");
                    lop.kind = T2LirKind::BsMatch;
                    if (op->dst_reg != T2_REG_NONE) {
                        lop.dst = reg_loc(op->dst_reg);
                        lop.dst_value = op->result->id;
                    }
                    lop.live = op->live;
                    lop.imm = op->imm_int; /* heap words */
                    lop.num_srcs = 1;
                    if (!src_of(op, 0, &lop.srcs[0])) {
                        return false;
                    }
                    if (lop.srcs[0].is_const) {
                        return fail_op(op, "bs_match of a constant");
                    }
                    if (op->bs_cmds == nullptr || op->num_bs_cmds == 0) {
                        return fail_op(op, "bs_match without commands");
                    }
                    lop.first_bs_cmd = (uint32_t)lir.bs_cmds.size();
                    lop.num_bs_cmds = op->num_bs_cmds;
                    for (uint16_t i = 0; i < op->num_bs_cmds; i++) {
                        const ErtsT2BsCmd &c = op->bs_cmds[i];

                        lir.bs_cmds.push_back(T2LirBsCmd{(uint8_t)c.kind,
                                                         (uint32_t)c.size,
                                                         (uint32_t)c.unit,
                                                         (uint32_t)c.live});
                    }

                    const T2Op *succ = op->next;
                    if (succ != nullptr && succ->kind == T2OpKind::Succeeded &&
                        succ->num_operands == 1 &&
                        succ->operands[0] == op->result && feeds_branch(succ)) {
                        const T2Op *term = op->block->terminator;

                        lop.succ_then = term->succ_then->id;
                        lop.succ_else = term->succ_else->id;
                        *skip_until = succ;
                        *consumed_terminator = true;
                    } else {
                        /* The builder always guards bs_match with a
                         * real fail label; anything else is drift. */
                        return fail_op(op,
                                       "bs_match without a folded fail "
                                       "edge");
                    }
                    b.ops.push_back(lop);
                    return true;
                }

                case T2OpKind::BsGetTail:
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "bs_get_tail without a home");
                    }
                    lop.kind = T2LirKind::BsGetTail;
                    lop.dst = reg_loc(op->dst_reg);
                    lop.dst_value = op->result->id;
                    lop.live = op->live;
                    lop.num_srcs = 1;
                    if (!src_of(op, 0, &lop.srcs[0])) {
                        return false;
                    }
                    if (lop.srcs[0].is_const) {
                        return fail_op(op, "bs_get_tail of a constant");
                    }
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::BsTestTail: {
                    /* A pure size guard: must fold into the branch,
                     * exactly like the type tests. */
                    if (!feeds_branch(op) || op->next != nullptr) {
                        return fail_op(op,
                                       "bs_test_tail result not consumed "
                                       "by this block's branch");
                    }
                    const T2Op *term = op->block->terminator;

                    lop.kind = T2LirKind::BsTestTail;
                    lop.imm = (Sint64)op->index; /* size in bits */
                    lop.num_srcs = 1;
                    if (!src_of(op, 0, &lop.srcs[0])) {
                        return false;
                    }
                    if (lop.srcs[0].is_const) {
                        return fail_op(op, "bs_test_tail of a constant");
                    }
                    lop.succ_then = term->succ_then->id;
                    lop.succ_else = term->succ_else->id;
                    *consumed_terminator = true;
                    b.ops.push_back(lop);
                    return true;
                }

                case T2OpKind::BsBase:
                case T2OpKind::BsLimit:
                case T2OpKind::BsCursor: {
                    /* Cursor-IV context projections (PLAN/T2FULL/14
                     * P-A): 1-2 instruction field loads off the boxed
                     * ErlSubBits into an X-homed raw temp. */
                    lop.kind = op->kind == T2OpKind::BsBase
                                       ? T2LirKind::BsBase
                                       : (op->kind == T2OpKind::BsLimit
                                                  ? T2LirKind::BsLimit
                                                  : T2LirKind::BsCursor);
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "bs projection without a home");
                    }
                    lop.dst = reg_loc(op->dst_reg);
                    lop.dst_value = op->result->id;
                    lop.num_srcs = 1;
                    if (!src_of(op, 0, &lop.srcs[0])) {
                        return false;
                    }
                    if (lop.srcs[0].is_const) {
                        return fail_op(op, "bs projection of a constant");
                    }
                    b.ops.push_back(lop);
                    return true;
                }

                case T2OpKind::BsEnsure: {
                    /* Separable bounds guard: folds into the branch
                     * exactly like BsTestTail. */
                    if (!feeds_branch(op) || op->next != nullptr) {
                        return fail_op(op,
                                       "bs_ensure result not consumed by "
                                       "this block's branch");
                    }
                    const T2Op *term = op->block->terminator;

                    lop.kind = T2LirKind::BsEnsure;
                    lop.imm = op->imm_int;        /* need, bits */
                    lop.imm2 = (Sint64)op->index; /* mode/unit bits */
                    if (!fill_srcs(op, &lop)) {
                        return false;
                    }
                    if (lop.srcs[0].is_const || lop.srcs[1].is_const) {
                        return fail_op(op, "bs_ensure of a constant");
                    }
                    lop.succ_then = term->succ_then->id;
                    lop.succ_else = term->succ_else->id;
                    *consumed_terminator = true;
                    b.ops.push_back(lop);
                    return true;
                }

                case T2OpKind::BsRead: {
                    /* Pure non-allocating extraction at base+cursor;
                     * only the byte-integer form exists today. */
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "bs_read without a home");
                    }
                    if (op->imm_int <= 0 || (op->imm_int % 8) != 0 ||
                        op->imm_int > 56) {
                        return fail_op(op, "bs_read outside the byte subset");
                    }
                    lop.kind = T2LirKind::BsRead;
                    lop.dst = reg_loc(op->dst_reg);
                    lop.dst_value = op->result->id;
                    lop.imm = op->imm_int; /* size, bits */
                    if (!fill_srcs(op, &lop)) {
                        return false;
                    }
                    if (lop.srcs[0].is_const || lop.srcs[1].is_const) {
                        return fail_op(op, "bs_read of a constant");
                    }
                    b.ops.push_back(lop);
                    return true;
                }

                case T2OpKind::BsLoadWord: {
                    /* P-C B2: ONE 64-bit load at base+cursor (the FC
                     * alignment guard proved the cursor byte-aligned). */
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "bs_load_word without a home");
                    }
                    if (op->imm_int != 64) {
                        return fail_op(op,
                                       "bs_load_word outside the one-word "
                                       "subset");
                    }
                    lop.kind = T2LirKind::BsLoadWord;
                    lop.dst = reg_loc(op->dst_reg);
                    lop.dst_value = op->result->id;
                    lop.imm = op->imm_int; /* size, bits */
                    if (!fill_srcs(op, &lop)) {
                        return false;
                    }
                    if (lop.srcs[0].is_const || lop.srcs[1].is_const) {
                        return fail_op(op, "bs_load_word of a constant");
                    }
                    b.ops.push_back(lop);
                    return true;
                }

                case T2OpKind::SwarByteSum: {
                    /* P-C B2: the horizontal byte sum (raw <<4 dst). */
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "swar_byte_sum without a home");
                    }
                    lop.kind = T2LirKind::SwarByteSum;
                    lop.dst = reg_loc(op->dst_reg);
                    lop.dst_value = op->result->id;
                    lop.num_srcs = 1;
                    if (!src_of(op, 0, &lop.srcs[0])) {
                        return false;
                    }
                    if (lop.srcs[0].is_const) {
                        return fail_op(op, "swar_byte_sum of a constant");
                    }
                    b.ops.push_back(lop);
                    return true;
                }

                case T2OpKind::BsSync: {
                    /* Cursor write-back to ErlSubBits.start. */
                    lop.kind = T2LirKind::BsSync;
                    if (!fill_srcs(op, &lop)) {
                        return false;
                    }
                    if (lop.srcs[0].is_const || lop.srcs[1].is_const) {
                        return fail_op(op, "bs_sync of a constant");
                    }
                    b.ops.push_back(lop);
                    return true;
                }

                case T2OpKind::BsGetPosition: {
                    /* Dst := make_small(ErlSubBits.start) — the
                     * backtrack anchor (P-B). A plain tagged value op
                     * (dst home from regalloc, not a raw temp). */
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "bs_get_position without a home");
                    }
                    lop.kind = T2LirKind::BsGetPosition;
                    lop.dst = reg_loc(op->dst_reg);
                    lop.dst_value = op->result->id;
                    lop.num_srcs = 1;
                    if (!src_of(op, 0, &lop.srcs[0])) {
                        return false;
                    }
                    if (lop.srcs[0].is_const) {
                        return fail_op(op, "bs_get_position of a constant");
                    }
                    b.ops.push_back(lop);
                    return true;
                }

                case T2OpKind::BsSetPosition: {
                    /* ErlSubBits.start := unsigned_val(pos) — the
                     * backtrack restore (P-B); srcs = context,
                     * tagged-small position. */
                    lop.kind = T2LirKind::BsSetPosition;
                    if (!fill_srcs(op, &lop)) {
                        return false;
                    }
                    if (lop.srcs[0].is_const || lop.srcs[1].is_const) {
                        return fail_op(op, "bs_set_position of a constant");
                    }
                    b.ops.push_back(lop);
                    return true;
                }

                case T2OpKind::SpeculateType:
                    /* The fused tag-bit deopt guard (P2 commit 4): AND
                     * the values, require every small-tag bit, deopt on
                     * failure. */
                    lop.kind = T2LirKind::SpeculateSmall;
                    /* Entry-class exits (the sunk-fun re-invocation,
                     * T2_OP_SPEC_ENTRY) and re-dispatch exits (P1a)
                     * count in the same monitoring counter as callsite
                     * exits: all are "the specialization bailed". */
                    lop.spec_callsite =
                            (op->flags &
                             (T2_OP_SPEC_CALLSITE | T2_OP_SPEC_ENTRY |
                              T2_OP_SPEC_REDISPATCH)) != 0;
                    if (!fill_srcs(op, &lop)) {
                        return false;
                    }
                    for (uint8_t i = 0; i < lop.num_srcs; i++) {
                        if (lop.srcs[i].is_const) {
                            return fail_op(op,
                                           "constant operand in a "
                                           "speculation guard (must be "
                                           "proven, never guarded)");
                        }
                    }
                    lop.t1_pc_fail = spec_deopt_pc(op);
                    if (lop.t1_pc_fail == nullptr) {
                        return fail_op(op,
                                       "no deopt PC for the speculation "
                                       "guard");
                    }
                    if (!fill_spec_cont(op, &lop)) {
                        return fail_op(op,
                                       "no CONT for the callee-window "
                                       "guard");
                    }
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::SpeculateRange:
                    /* The ASCII range deopt guard (P-C L1): side-exit
                     * when the tagged-small operand >= imm_int.
                     * Boundary class only — the deopt PC is the utf8
                     * op's own T1 EFFECT site, reached with the bs
                     * cursor still unadvanced (the guard precedes the
                     * advance/BsSync by construction). */
                    lop.kind = T2LirKind::SpeculateRange;
                    if (op->num_operands != 1) {
                        return fail_op(op,
                                       "range guard without exactly one "
                                       "operand");
                    }
                    if (op->imm_int <= 0) {
                        return fail_op(op, "range guard without a bound");
                    }
                    if ((op->flags & T2_OP_SPEC_BOUNDARY) == 0) {
                        return fail_op(op,
                                       "range guard outside the boundary "
                                       "deopt class");
                    }
                    lop.imm = op->imm_int;
                    if (!fill_srcs(op, &lop)) {
                        return false;
                    }
                    if (lop.srcs[0].is_const) {
                        return fail_op(op,
                                       "constant operand in a "
                                       "speculation guard (must be "
                                       "proven, never guarded)");
                    }
                    lop.t1_pc_fail = spec_deopt_pc(op);
                    if (lop.t1_pc_fail == nullptr) {
                        return fail_op(op,
                                       "no deopt PC for the speculation "
                                       "guard");
                    }
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::SwarAsciiTest:
                    /* P-C L2: the fused ASCII guard over the wide word.
                     * Boundary + ROLL-BACK class — the deopt PC is the
                     * loop header start_match's own T1 EFFECT site (the
                     * clause entry), reached with the bs cursor still
                     * unadvanced (the guard precedes the advance/BsSync
                     * by construction, exactly like the B1 fused add it
                     * shares a trampoline shape with). The single raw
                     * word operand is never a proven small (it is the
                     * tag-cleared BsLoadWord result) — spec_check_op
                     * skips it and run_raw_checks covers it. */
                    lop.kind = T2LirKind::SwarAsciiTest;
                    if (op->num_operands != 1) {
                        return fail_op(op,
                                       "ascii guard without exactly one "
                                       "operand");
                    }
                    if ((op->flags & T2_OP_SPEC_BOUNDARY) == 0) {
                        return fail_op(op,
                                       "ascii guard outside the boundary "
                                       "deopt class");
                    }
                    if (op->sync == nullptr) {
                        return fail_op(op,
                                       "ascii guard without a sync map "
                                       "(the roll-back pin set)");
                    }
                    if (!fill_srcs(op, &lop)) {
                        return false;
                    }
                    if (lop.srcs[0].is_const) {
                        return fail_op(op, "ascii guard of a constant");
                    }
                    lop.t1_pc_fail = spec_deopt_pc(op);
                    if (lop.t1_pc_fail == nullptr) {
                        return fail_op(op, "no deopt PC for the ascii guard");
                    }
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::AddSmall:
                case T2OpKind::SubSmall:
                    /* Flag-checked one-untag arithmetic; deopt (b.vs)
                     * fires before the commit — except the roll-back
                     * class (P-C B1), which may commit in place and
                     * un-commit in its trampoline. */
                    lop.kind = op->kind == T2OpKind::AddSmall
                                       ? T2LirKind::AddSmall
                                       : T2LirKind::SubSmall;
                    lop.rollback = (op->flags & T2_OP_ROLLBACK) != 0;
                    if (lop.rollback && op->sync == nullptr) {
                        return fail_op(op,
                                       "roll-back arithmetic without a "
                                       "sync map");
                    }
                    lop.spec_callsite =
                            (op->flags &
                             (T2_OP_SPEC_CALLSITE | T2_OP_SPEC_ENTRY |
                              T2_OP_SPEC_REDISPATCH)) != 0;
                    /* P3: validated no-overflow claim — the emitter
                     * omits the b.vs and its trampoline. The deopt-PC
                     * plumbing below is kept (the LIR verifier's
                     * "every speculative op deopts somewhere" rule),
                     * it is just never emitted. */
                    lop.no_ovf = (op->flags & T2_OP_NO_OVF) != 0;
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "arith result without a home");
                    }
                    lop.dst = reg_loc(op->dst_reg);
                    lop.dst_value = op->result->id;
                    lop.mfa_m = op->mfa_m;
                    lop.mfa_f = op->mfa_f;
                    lop.live = op->live;
                    if (!fill_srcs(op, &lop)) {
                        return false;
                    }
                    lop.t1_pc_fail = spec_deopt_pc(op);
                    if (lop.t1_pc_fail == nullptr) {
                        return fail_op(op,
                                       "no deopt PC for flag-checked "
                                       "arithmetic");
                    }
                    if (!fill_spec_cont(op, &lop)) {
                        return fail_op(op,
                                       "no CONT for the callee-window "
                                       "arithmetic");
                    }
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::ReductionCheck:
                    /* Recovered-loop back edge (t2_loop.cpp): the
                     * demote target is the function's own T1 entry
                     * L_f, resolved exactly like a local tail
                     * call's transfer target; the sync map (already
                     * attached above) is the fresh-call vector the
                     * yield saves. */
                    if (op->sync == nullptr) {
                        return fail_op(op,
                                       "back-edge without a sync "
                                       "map");
                    }
                    if (op->flags & T2_OP_RC_CALLEE) {
                        /* Intrinsic back edge (P2 commit 8): callee
                         * demote class — see T2LirKind's comment. A
                         * TAIL site (T2_OP_TAIL_SITE; P1a) has no T1
                         * continuation: the tombstone demote enters
                         * the callee body with no CP push (the blob's
                         * own return address already points at the
                         * caller's caller). */
                        lop.kind = T2LirKind::ReductionCheckCallee;
                        lop.imm = 1 + (Sint64)op->index;
                        lop.mfa_m = op->mfa_m;
                        lop.mfa_f = op->mfa_f;
                        lop.arity = op->live;
                        lop.tail_site = (op->flags & T2_OP_TAIL_SITE) != 0;
                        lop.target = (const void *)(UWord)op->imm_int;
                        lop.t1_pc_fail =
                                pc_lookup(op->beam_idx, ERTS_T2_PC_CALL);
                        if (!lop.tail_site) {
                            lop.t1_pc_cont =
                                    pc_lookup(op->beam_idx, ERTS_T2_PC_CONT);
                        }
                        if (lop.target == nullptr ||
                            lop.t1_pc_fail == nullptr ||
                            (lop.t1_pc_cont == nullptr && !lop.tail_site)) {
                            return fail_op(op,
                                           "unresolved callee back-edge "
                                           "addresses");
                        }
                        b.ops.push_back(lop);
                        return true;
                    }
                    lop.kind = T2LirKind::ReductionCheck;
                    /* 1 + the charges of any calls the inliner erased
                     * on this back edge (t2_inline.cpp). */
                    lop.imm = 1 + (Sint64)op->index;
                    lop.target = local_target(hir.function, hir.arity);
                    if (lop.target == nullptr) {
                        return fail_op(op,
                                       "no T1 entry for the back-edge "
                                       "demote");
                    }
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::ChargeReds:
                    lop.kind = T2LirKind::ChargeReds;
                    lop.imm = op->imm_int;
                    if (lop.imm <= 0 || lop.imm >= 4096) {
                        return fail_op(op, "charge_reds out of range");
                    }
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::IsFlatmapBounded: {
                    /* Fused shape test + branch (maps:fold Stage 1):
                     * both edges stay in the blob — non-flatmap is the
                     * general case, not an error. */
                    if (!feeds_branch(op) || op->next != nullptr) {
                        return fail_op(op,
                                       "flatmap guard result not consumed "
                                       "by this block's branch");
                    }
                    const T2Op *term = op->block->terminator;

                    lop.kind = T2LirKind::IsFlatmapBounded;
                    lop.num_srcs = 1;
                    if (!src_of(op, 0, &lop.srcs[0])) {
                        return false;
                    }
                    if (lop.srcs[0].is_const) {
                        return fail_op(op, "flatmap guard of a constant");
                    }
                    lop.succ_then = term->succ_then->id;
                    lop.succ_else = term->succ_else->id;
                    *consumed_terminator = true;
                    b.ops.push_back(lop);
                    return true;
                }

                case T2OpKind::FlatmapSize:
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "flatmap_size without a home");
                    }
                    lop.kind = T2LirKind::FlatmapSize;
                    lop.dst = reg_loc(op->dst_reg);
                    lop.dst_value = op->result->id;
                    lop.num_srcs = 1;
                    if (!src_of(op, 0, &lop.srcs[0])) {
                        return false;
                    }
                    if (lop.srcs[0].is_const) {
                        return fail_op(op, "flatmap_size of a constant");
                    }
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::FlatmapKeyAt:
                case T2OpKind::FlatmapValAt:
                    if (op->dst_reg == T2_REG_NONE) {
                        return fail_op(op, "flatmap access without a home");
                    }
                    lop.kind = op->kind == T2OpKind::FlatmapKeyAt
                                       ? T2LirKind::FlatmapKeyAt
                                       : T2LirKind::FlatmapValAt;
                    lop.dst = reg_loc(op->dst_reg);
                    lop.dst_value = op->result->id;
                    if (!fill_srcs(op, &lop)) {
                        return false;
                    }
                    if (lop.srcs[0].is_const || lop.srcs[1].is_const) {
                        return fail_op(op, "flatmap access of a constant");
                    }
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::FoldBudget:
                    /* The whole-fold reduction batch: uncharged side
                     * exit when FCALLS does not cover it. Callsite
                     * class: to the erased call's own T1 PC (T1
                     * re-executes the call and does its own charging/
                     * yielding). Entry class (the make_fun sink
                     * dissolved the call boundary): to the function's
                     * T1 entry body — T1 re-runs the whole invocation,
                     * fun construction included, charging exactly as
                     * it always does (the budget was not taken). */
                    if (op->sync == nullptr) {
                        return fail_op(op, "fold budget without a sync map");
                    }
                    lop.kind = T2LirKind::FoldBudget;
                    lop.num_srcs = 1;
                    if (!src_of(op, 0, &lop.srcs[0])) {
                        return false;
                    }
                    if (lop.srcs[0].is_const) {
                        return fail_op(op, "fold budget of a constant");
                    }
                    lop.imm = (Sint64)op->index; /* per-element charge */
                    lop.imm2 = op->imm_int;      /* constant charge    */
                    if (lop.imm <= 0 || lop.imm >= 128 || lop.imm2 <= 0 ||
                        lop.imm2 >= 4096) {
                        return fail_op(op, "fold budget out of range");
                    }
                    lop.spec_callsite = true;
                    lop.t1_pc_fail = spec_deopt_pc(op);
                    if (lop.t1_pc_fail == nullptr) {
                        return fail_op(op,
                                       "no T1 PC for the fold budget "
                                       "deopt");
                    }
                    b.ops.push_back(lop);
                    return true;

                default:
                    return fail_op(op, "unsupported HIR op in P1 isel");
                }
            }

            /* A tail call_ext to a light BIF. T1's transform makes these
             * *body* calls followed by an epilogue:
             *
             *   call_ext_last Ar Bif D =>
             *       call_light_bif Bif | deallocate D | return
             *   call_ext_only Ar Bif =>
             *       allocate 0 Ar | call_light_bif Bif
             *                     | deallocate 0 | return
             *
             * i.e. the BIF runs with the frame *intact*, and only then
             * the frame is dropped. The builder split the fused
             * call_ext_last into Deallocate + TailCallExt (for the tail
             * sync map); mirror T1 by popping that Deallocate back off
             * and re-ordering to CallBif + Deallocate + Return. For the
             * frameless call_ext_only shape, T1 inserts `allocate 0 Ar`
             * (a stack/heap guard with live=arity) before the call;
             * synthesize the same Allocate. Every T1 address then lines
             * up: the site's BIF PC expects the frame intact (yield
             * re-executes the call), and the CONT PC is T1's own
             * deallocate+return epilogue (the trap/trace CP). */
            bool emit_tail_bif(T2LirBlock &b, const T2Op *t, T2LirOp &lop) {
                if (!lower_bif_call(t, &lop)) {
                    return false;
                }
                lop.t1_pc_cont = pc_lookup(t->beam_idx, ERTS_T2_PC_CONT);
                if (lop.t1_pc_cont == nullptr) {
                    return fail_op(t,
                                   "no CONT pctab entry for the tail bif "
                                   "call's T1 epilogue");
                }
                lop.dst = PhysLoc::xreg(0);
                lop.live = t->index;

                Sint64 dealloc = 0;

                if (!b.ops.empty() &&
                    b.ops.back().kind == T2LirKind::Deallocate &&
                    b.ops.back().beam_idx == t->beam_idx) {
                    /* The split call_ext_last: un-split it. */
                    dealloc = b.ops.back().imm;
                    b.ops.pop_back();
                } else {
                    /* call_ext_only: T1's `allocate 0 Ar` guard. */
                    T2LirOp alloc;

                    alloc.kind = T2LirKind::Allocate;
                    alloc.imm = 0;
                    alloc.imm2 = 0;
                    alloc.live = t->index;
                    alloc.beam_idx = t->beam_idx;
                    b.ops.push_back(alloc);
                }

                b.ops.push_back(lop);

                T2LirOp de;

                de.kind = T2LirKind::Deallocate;
                de.imm = dealloc;
                de.beam_idx = t->beam_idx;
                b.ops.push_back(de);

                T2LirOp ret;

                ret.kind = T2LirKind::Return;
                ret.beam_idx = t->beam_idx;
                ret.num_srcs = 1;
                ret.srcs[0] = T2LirSrc::slot(PhysLoc::xreg(0));
                b.ops.push_back(ret);

                return true;
            }

            bool emit_terminator(T2LirBlock &b, const T2Op *t) {
                T2LirOp lop;

                if (t == nullptr) {
                    return fail("block without terminator");
                }
                lop.beam_idx = t->beam_idx;
                /* Return/tail-transfer boundary map (null on error exits
                 * by design; see T2_OP_ERR_EXIT_*). For the reordered
                 * tail-BIF shape this map rides on the CallBif op. */
                lop.sync = t->sync;
                /* P2 loop unboxing: the re-tag mask rides along (the P1
                 * fallback re-dispatch consumes the raw accumulator as
                 * a real call argument; its emission re-tags first). */
                lop.raw_mask = t->raw_mask;

                switch (t->kind) {
                case T2OpKind::Return:
                    lop.kind = T2LirKind::Return;
                    if (t->num_operands != 1) {
                        return fail("return without a single value");
                    }
                    lop.num_srcs = 1;
                    if (!src_of(t, 0, &lop.srcs[0])) {
                        return false;
                    }
                    if (!lop.srcs[0].is_const &&
                        lop.srcs[0].loc != PhysLoc::xreg(0)) {
                        return fail("return value not in x0");
                    }
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::Jump:
                    lop.kind = T2LirKind::Jump;
                    lop.succ_then = t->succ_then->id;
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::DemoteCallee:
                    /* Intrinsic-loop demote (P2 commit 8): push the
                     * call site's T1 continuation as CP and enter the
                     * callee body — the callee L_f (imm_int) past its
                     * entry check (the loop's charges already paid
                     * it). A TAIL site (T2_OP_TAIL_SITE; the P1 inner
                     * re-dispatch) has no T1 continuation: the demote
                     * enters the callee body with no CP push (the
                     * blob's own return address already points at the
                     * caller's caller). */
                    if (t->sync == nullptr) {
                        return fail("demote-callee without a sync map");
                    }
                    lop.kind = T2LirKind::DemoteCallee;
                    lop.mfa_m = t->mfa_m;
                    lop.mfa_f = t->mfa_f;
                    lop.arity = t->live;
                    lop.tail_site = (t->flags & T2_OP_TAIL_SITE) != 0;
                    lop.target =
                            (const void *)((const char *)(UWord)t->imm_int +
                                           erts_t2_test_yield_return_offset());
                    if (!lop.tail_site) {
                        lop.t1_pc_cont =
                                pc_lookup(t->beam_idx, ERTS_T2_PC_CONT);
                    }
                    if (t->imm_int == 0 ||
                        (lop.t1_pc_cont == nullptr && !lop.tail_site)) {
                        return fail("demote-callee without a resolved "
                                    "callee/continuation");
                    }
                    b.ops.push_back(lop);
                    return true;

                case T2OpKind::Switch: {
                    lop.kind = T2LirKind::Switch;
                    lop.num_srcs = 1;
                    /* imm=1 marks a select_tuple_arity switch: emission
                     * compares the tuple header against make_arityval(N)
                     * instead of the value against the case term
                     * (T2_OP_SWITCH_ARITY, t2_hir.hpp). */
                    lop.imm = (t->flags & T2_OP_SWITCH_ARITY) ? 1 : 0;
                    if (!src_of(t, 0, &lop.srcs[0])) {
                        return false;
                    }
                    lop.first_case = (uint32_t)lir.switch_cases.size();
                    lop.num_cases = t->num_cases;
                    for (uint32_t i = 0; i < t->num_cases; i++) {
                        if (!is_immed(t->cases[i].value)) {
                            return fail("boxed switch value unsupported in "
                                        "P1 commit-3 isel");
                        }
                        lir.switch_cases.push_back(
                                T2LirSwitchCase{t->cases[i].value,
                                                t->cases[i].target->id});
                    }
                    lop.default_target = t->default_target->id;
                    b.ops.push_back(lop);
                    return true;
                }

                case T2OpKind::TailCall:
                case T2OpKind::TailCallExt: {
                    bool is_ext = t->kind == T2OpKind::TailCallExt;

                    /* Error exits lower to a side exit: an unconditional
                     * branch to a T1 PC. T2 never raises (surprise #7). */
                    if (t->flags & T2_OP_ERR_EXIT_SHARED) {
                        lop.kind = T2LirKind::SideExit;
                        lop.t1_pc_fail = func_info_target();
                        if (lop.t1_pc_fail == nullptr) {
                            return fail("cannot resolve func_info for the "
                                        "shared error exit");
                        }
                        b.ops.push_back(lop);
                        return true;
                    }
                    if (t->flags & T2_OP_ERR_EXIT_OP) {
                        /* The `NotInX=cy` transform splits a leading move
                         * off badmatch/case_end, so the recorded T1 PC
                         * assumes an X-register source; only that shape
                         * side-exits exactly. */
                        if (t->num_operands == 1 &&
                            (t->operand_regs == nullptr ||
                             !t2_reg_is_x(t->operand_regs[0]))) {
                            return fail_op(t,
                                           "error-exit source not in an X "
                                           "register");
                        }
                        lop.kind = T2LirKind::SideExit;
                        lop.t1_pc_fail =
                                pc_lookup(t->beam_idx, ERTS_T2_PC_ERROR);
                        if (lop.t1_pc_fail == nullptr) {
                            return fail_op(t,
                                           "no ERROR pctab entry for the "
                                           "error exit");
                        }
                        /* Carry the frame-materializing sync map (builder
                         * attaches it when a real frame is live) so the
                         * allocator pins every Y slot to its home before the
                         * branch into T1's raise path. */
                        lop.sync = t->sync;
                        b.ops.push_back(lop);
                        return true;
                    }
                    if (t->flags & T2_OP_GARBAGE_DEALLOC) {
                        return fail_op(t,
                                       "garbage-dealloc transfer unsupported "
                                       "in P1 isel (no-return call)");
                    }

                    lop.kind = is_ext ? T2LirKind::TailCallExt
                                      : T2LirKind::TailCall;
                    lop.mfa_m = t->mfa_m;
                    lop.mfa_f = t->mfa_f;
                    lop.arity = t->index;
                    if (is_ext) {
                        const Export *ep = nullptr;
                        ExtTarget kind = ExtTarget::Erlang;

                        if (!export_target(t, &ep, &kind)) {
                            return false;
                        }
                        lop.exp = (const void *)ep;

                        if (kind == ExtTarget::LightBif) {
                            return emit_tail_bif(b, t, lop);
                        }
                    } else {
                        /* Self-recursion included: identity only, the
                         * transfer goes to the (own) T1 entry — no
                         * back-edge optimization in P1. */
                        lop.target = local_target(t->mfa_f, t->index);
                        if (lop.target == nullptr) {
                            return fail_op(t, "local tail target not found");
                        }
                    }
                    b.ops.push_back(lop);
                    return true;
                }

                default:
                    return fail(std::string("unsupported terminator in P1 "
                                            "isel (") +
                                t2_op_kind_name(t->kind) + ")");
                }
            }

            bool run() {
                if (!hir.sync_complete) {
                    return fail("P1 isel requires builder sync metadata");
                }

                /* A function whose own MFA is a BIF is a *stub*: the
                 * loader's is_mfa_bif transform replaces its body with
                 * call_bif_mfa (the real BIF trampoline) and the chunk
                 * body the builder decoded is emitted only as dead
                 * code. Compiling — let alone installing — that body
                 * would put the stub's erlang:error/nif_error fallback
                 * over the real BIF (found the hard way: +JT2enable
                 * boot replaced erts_internal:garbage_collect/1 with
                 * `nif_error(undefined)`). Mirror is_mfa_bif exactly. */
                {
                    const Export *self_ep =
                            erts_active_export_entry(hir.module,
                                                     hir.function,
                                                     hir.arity);

                    if (self_ep != nullptr && self_ep->bif_number >= 0) {
                        return fail("function is a BIF stub (T1 loads "
                                    "call_bif_mfa, not the chunk body)");
                    }
                }

                lir.module = hir.module;
                lir.function = hir.function;
                lir.arity = hir.arity;
                lir.num_values = (uint32_t)hir.values.size();
                lir.param_x.assign(lir.num_values, -1);
                lir.entry_sync = hir.entry_sync;

                /* Per-value facts for the allocator (const defs are not
                 * liveness-tracked; untagged values are barred from X/Y
                 * homes and sync maps — defensive until the speculation
                 * phase emits them). */
                lir.value_flags.assign(lir.num_values, 0);
                for (const T2Value *v : hir.values) {
                    uint8_t f = 0;

                    if (is_const_kind(v->def->kind)) {
                        f |= T2_LIR_VF_CONST;
                    }
                    if ((v->def->kind == T2OpKind::UntagInt ||
                         v->def->kind == T2OpKind::MulRaw) &&
                        (v->def->flags & T2_OP_RAW_MODE) == 0) {
                        /* The phys-only discipline. A RAW_MODE UntagInt
                         * (P2 loop unboxing) is slot-homed by design;
                         * its placement is governed by the HIR raw
                         * checks + the sync-map raw_mask rule instead. */
                        f |= T2_LIR_VF_UNTAGGED;
                    }
                    lir.value_flags[v->id] = f;
                }

                if (ctx.ret == nullptr || ctx.code_hdr == nullptr) {
                    return fail("isel context incomplete");
                }

                /* Use counts for the guard-fold checks. */
                uses.assign(hir.values.size(), 0);
                for (const T2BasicBlock *hb : hir.blocks) {
                    auto count = [&](const T2Op *op) {
                        for (uint16_t i = 0; i < op->num_operands; i++) {
                            uses[op->operands[i]->id]++;
                        }
                    };
                    for (const T2Op *p = hb->phis_head; p != nullptr;
                         p = p->next) {
                        count(p);
                    }
                    for (const T2Op *op = hb->ops_head; op != nullptr;
                         op = op->next) {
                        count(op);
                    }
                    if (hb->terminator != nullptr) {
                        count(hb->terminator);
                    }
                }

                for (const T2BasicBlock *hb : hir.blocks) {
                    T2LirBlock &lb = lir.new_block();
                    bool consumed_terminator = false;

                    /* Phis are no-ops under identity placement: every
                     * predecessor materialized the value in the phi's
                     * home register (proven by the HIR validator's
                     * merge-slot walk). They are carried onto the LIR
                     * block as allocator annotations only: the def at
                     * block entry and the per-edge inputs are what make
                     * liveness correct across merges and loop
                     * back-edges (HIR and LIR block ids agree 1:1). */
                    for (const T2Op *phi = hb->phis_head; phi != nullptr;
                         phi = phi->next) {
                        T2LirPhi lp;

                        if (phi->dst_reg == T2_REG_NONE) {
                            return fail_op(phi, "phi without a home");
                        }
                        lp.value = phi->result->id;
                        lp.home = reg_loc(phi->dst_reg);
                        for (uint16_t i = 0; i < phi->num_operands; i++) {
                            lp.ins.push_back(
                                    T2LirPhi::In{phi->phi_blocks[i]->id,
                                                 phi->operands[i]->id});
                        }
                        lb.phis.push_back(std::move(lp));
                    }

                    for (const T2Op *op = hb->ops_head; op != nullptr;
                         op = op->next) {
                        const T2Op *skip_until = nullptr;

                        if (!emit_op(lb,
                                     op,
                                     &skip_until,
                                     &consumed_terminator)) {
                            return false;
                        }
                        if (skip_until != nullptr) {
                            op = skip_until;
                        }
                    }

                    if (!consumed_terminator &&
                        !emit_terminator(lb, hb->terminator)) {
                        return false;
                    }
                }

                return true;
            }
        };

    } /* anonymous namespace */

    bool t2_isel(const T2Function &hir,
                 const T2IselContext &ctx,
                 T2LirFunction &lir,
                 std::string *err) {
        Isel isel{hir, ctx, lir, err, {}};
        return isel.run();
    }

} /* namespace erts_t2 */
