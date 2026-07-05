# T2-Full P1 — Source-Grounded Implementation Map

Scope: the **identity transform through the full pipeline** — P0's HIR →
LIR → asmjit emission with *no optimizations*, producing T2 code
behaviorally identical to T1; dynamic install via the prologue patch;
jettison + thread-progress free; `t2_ranges` population + `c_p->i`
handling; a `+JT2enable`-style forcing flag compiling every eligible
function at load. Exit gate: **full OTP emulator suite green with the
flag on**. Paths under `/Users/lukas/code/otp-beamjit2/`. aarch64
(`jit/arm/`) only; x86 deferred to P7. New code in `beam/jit/t2/`.

**Read-before-build surprises flagged inline as ⚠.** The governing
inheritance: rung-1 (re-call) deopt, no CPs into blobs, entry-yield
demotes to T1 (T2/08 §4.2–4.5); P1 has **no** loop/back-edge resume.

---

## 1. LIR + emission

**Findings.** The backend seam (`04_backend.md` §4): HIR is
backend-neutral (`t2_hir.hpp:36` — *"must not depend on asmjit types"*);
LIR's whole contract is *isel + regalloc + encode + framestate
metadata*. The emission substrate to reuse is the aarch64
`BeamModuleAssembler`. Register mapping (`jit/arm/beam_asm.hpp:85–172`):
`E=x20 c_p=x21 FCALLS=w22 HTOP=x23 active_code_ix=x24`; register-backed
`XREG0..3 = x25..x28` (callee-save), `XREG4/5 = x15/x16` (caller-save,
must flush before C calls); `ARG1..8 = x0..x7`, `TMP1..6 = x8..x13`,
`SUPER_TMP=x14`; **x18 reserved on Apple**. Reusable primitives:
`emit_enter_runtime`/`emit_leave_runtime` (`beam_asm.hpp:460,540`, with
`Update::{eStack,eHeap,eReductions,eXRegs}` sync flags), `emit_enter/
leave_erlang_frame` (`:429,433`), `getXRef`/`getArgRef`, `runtime_call<>`,
`emit_gc_test` (`instr_common.cpp:137`), the veneer machinery
(`resolve_label`/`emit_veneer`, `beam_asm_module.cpp:1616,1855`;
`disp128MB` = `beam_asm.hpp:902`).

⚠ **The asmjit-operand seam is `mov_arg`, not the per-op emitters.**
The generated `emit_*` (from `beamasm_protos.h`, included at
`beam_asm.hpp:1463`) are all `protected` and take the loader's `ArgVal`
family (`ArgSource/ArgRegister/ArgLabel/ArgWord/ArgExport`) — **none
take asmjit `Label`/`a64::Gp`**. The one register-level seam is the
`mov_arg` overload block (`beam_asm.hpp:~1814,1840` —
`mov_arg(a64::Gp, const ArgVal&)`).

**Change spec.** New `t2_lir.hpp/.cpp`: a flat vector of `T2LirOp
{ kind; PhysLoc dst; PhysLoc srcs[]; imm; mfa; t1_pc_fail; beam_idx }`
where `PhysLoc` is *either* a canonical BEAM slot (Xn/Yn) *or* a pinned
physical register. **Isel** (`t2_isel.cpp`) walks HIR ops 1:1 → LIR ops.
**Regalloc** (`t2_regalloc.cpp`): a Wimmer linear-scan-on-SSA skeleton
whose P1 policy is *sync-everything* — every op boundary is a sync point
that pins each live X/Y value to its canonical slot, so cross-op live
ranges never span registers and allocation is trivial (TMP1..6 are
per-op scratch). The intervals/pin-constraint API is real; **P2 relaxes
it** by demoting non-sync-point boundaries so ranges live in registers
across ops. **Emit** (`t2_emit.cpp`): a `BeamT2ModuleAssembler` *deriving
from* `BeamModuleAssembler`, so it inherits register defs, `mov_arg`,
`emit_enter/leave_runtime`, `emit_gc_test`, veneers, and the `protected`
per-op emitters. For identity ops it **synthesizes `ArgVal`** (e.g.
`ArgXRegister(n)`, `ArgWord(imm)`, `ArgExport(exp)`) from the LIR slot
and calls the T1 emitter with `Fail` redirected to the op's T1 PC (§2).
Calling-convention invariants from `beam_asm.hpp`: args in X0.. (register
slots), `x30`=RA, frame on the E-stack, HTOP/FCALLS live in registers,
untagged values never cross sync points.

**LOC** ~1600 (lir 300 / isel 500 / regalloc 400 / emit-scaffold 400).
**Risks:** subclassing `BeamModuleAssembler` drags per-module loader
state (`current_label`, `_dispatchTable`) — instantiate a *fresh*
assembler per T2 blob, not the loader's; `ArgVal` synthesis must
reproduce exact register/type-slot encodings; keeping the pin API
genuinely P2-relaxable rather than a throwaway.

---

## 2. Identity lowering table

The eligible op set is fixed by `t2_eligible.c:48–120` (the single source
of truth shared with the builder), so the table is closed. HIR op kinds
from `t2_hir.hpp:66–167`.

| HIR op(s) | T1 emitter (`jit/arm/`) | P1 emission |
|---|---|---|
| `ConstInt/Atom/Nil/Literal`, `Param`, move | `mov_arg`, `emit_i_move` (`instr_common.cpp:544`) | reuse `mov_arg` |
| `Phi` | — | **no-op under full sync**: predecessors already wrote the canonical slot; isel drops the phi (⚠ verify the merge slot) |
| `IsInteger/Atom/Nil/List/NonemptyList/Tuple/Binary/Map`, `TestArity`, `IsTaggedTuple` | `emit_is_*`/`emit_i_is_tuple` (`instr_common.cpp:1147–1681`) | reuse, `Fail`→op's T1 PC |
| `Succeeded` | folded into the guard-BIF it follows | consumed by the arith/guard op |
| `CmpEqExact/NeExact/Eq/Ne/Lt/Le/Gt/Ge` | `emit_is_eq_exact` … (`instr_common.cpp:1777–2319`) | reuse, `Fail`→T1 PC |
| `Add/Sub/Mul/IDiv/Rem/Band/Bor/Bxor/Bsl/Bsr/Bnot/Neg` (via `gc_bif`) | `emit_i_plus/minus/mul_add/bsl…` (`instr_arith.cpp`), generic `emit_i_bif1/2/3` (`instr_bif.cpp:110–151`) | reuse, `Fail`→T1 PC (**error side-exits, never raises**) |
| `GetTupleElement/GetHd/GetTl/MakeList/MakeTuple` | `emit_i_get_tuple_element:406`, `emit_get_hd/tl:327,337`, `emit_put_list:735`, `emit_put_tuple2:820` | reuse (heap covered by `GcTest`) |
| `GuardBif` | `emit_i_bif`, `emit_bif_*` (`instr_guard_bifs.cpp`) | reuse inline (CP-less leaf, `04` §10.7) |
| `Branch/Jump/Switch/Return` | branch folded into test; `emit_jump:1153`; `emit_i_select_val_lins/bins` (`instr_select.cpp:283,338`); `emit_return` (`instr_call.cpp:59`) | reuse |
| `GcTest` | `emit_gc_test:137`/`emit_test_heap:259` | reuse (see below) |
| `ReductionCheck` | — | the entry stub's own `subs FCALLS` (§3) |

**⚠ Genuinely hard ops + resolutions:**

1. **Calls (`Call`, `CallExt`, non-tail).** *Cannot* reuse `emit_i_call`
   (`instr_call.cpp:73`), which does `erlang_call`→`bl` and pushes the
   **T2** post-call address as CP. `08` §4.3 mandates the CP be the **T1
   continuation**. Hand-emit: `adr/mov x30 = <T1 CONT PC>` (from the
   pctab `ERTS_T2_PC_CONT` entry, `t2_pctab.h:70`), then `b`/`br` to the
   resolved callee entry. The T2 region *ends* at the call; the callee
   returns into T1 and the rest of the invocation runs T1 (demote-on-
   return — behaviorally identical). Local-call target resolution is by
   MFA→`ErtsCodePtr` (via `erts_active_export_entry`/local map), not the
   loader's `ArgLabel` graph.
2. **Tail calls (`TailCall*`, terminator).** Equivalent to
   `emit_i_call_only` (`instr_call.cpp:117`): leave the Erlang frame,
   `b` to the callee's public entry — whose **patched prologue** re-enters
   T2 (self-recursion) or T1. Reductions charged at the callee entry stub,
   identical to T1. `CallFun/TailCallFun/MakeFun` HIR kinds exist but the
   eligibility set has **no fun/`make_fun` genops**, so eligible P1
   functions never contain them — nothing to emit.
3. **GC tests.** `emit_gc_test` may trigger GC, which walks X/Y+HTOP.
   P1's full-sync guarantees a canonical post-call BEAM state at the site
   (`06` §4.5), so `emit_gc_test` is reused as-is.
4. **Error paths** (`badmatch/if_end/case_end`, and any guard/arith
   error). **T2 never raises** (`08` §5.1). The builder already models
   these as error-exit blocks (tail-call `erlang:error`); in P1 emission
   they instead lower to a **branch to the op's T1 PC**, letting T1
   re-execute and raise a byte-identical stacktrace.

The whole table reduces to one principle: **reuse the T1 emitter with
`Fail`→T1-PC and calls redirected to T1-continuation CPs.** Speculative
kinds (`UntagInt/AddSmall/SpeculateType/…`) and `FrameState` are **never
emitted in P1** — the validator already rejects framestates (`t2_hir.cpp`).

**LOC** ~1200 (dispatch + the hand-emitted call/tail-call/error lowerings).
**Risks:** `Fail`-label plumbing into reused emitters that expect an
`ArgLabel` (may need a thin `ArgLabel`→bound-address adapter or a private
emit variant); `call_ext`-to-BIF (inline) vs `call_ext`-to-Erlang
(demote) discrimination — mirror pctab's `erts_active_export_entry` test;
`select_val` operand `Span` construction.

---

## 3. Install — the prologue patch

**Findings.** The patchable prologue is `emit_i_breakpoint_trampoline`
(`beam_asm_module.cpp:339`): `enter_erlang_frame` / `b next` at **+4**
(word `0x14000002`) / `bl shared` at +8 / `next:` at +12
(`BEAM_ASM_FUNC_PROLOGUE_SIZE`=12, `beam_asm.h:127`). The existing
trace/NIF patch machinery is the template: `erts_asm_bp_enable/disable`
(`beam_asm.h:185,206`) rewrites the +4 word (`0x14000002`↔`0x14000001`);
`erts_asm_bp_set_flag` (`:227`) locates +4 via `rw_p = codeinfo+1` and
ORs a flag into `ErtsCodeInfo.u.metadata.breakpoint_flag`. The RW↔RX gate
is `beamasm_unseal_module`/`erts_seal_module` (`beam_jit_main.cpp:453`;
`module.c:230`), and `erts_seal_module` calls `beamasm_flush_icache`
(`beam_jit_main.cpp:473`) → `sys_icache_invalidate` on macOS aarch64
(**issues full memory + instruction barriers across threads**), or manual
`dc cvau`/`dsb ish`/`ic ivau` on bare aarch64. `erts_seal_module` asserts
the caller holds code-mod permission (`module.c:232`).

**Change spec.** New `t2_install.c`:
`erts_t2_install(inst, fn, blob_start)` — under
`erts_try_seize_code_mod_permission` (`code_ix.c:272`; lighter than
load-permission — no code_ix staging, we mutate an existing module in
place): (1) check the six preconditions (`06` §2.6): `breakpoint_flag==0`
(`erts_asm_bp_get_flags`, `beam_asm.h:180`), the +4 word is the unmodified
`0x14000002`, watched-module generations unchanged, not trace-patterned,
reach OK, pctab present; (2) `beamasm_unseal_module`; (3) write +4 =
`b <reach target>` (a single aligned 4-byte store; **no** flag bit —
strict mutual exclusion, `06` §2.4); (4) `erts_seal_module` (flush +
barrier); (5) `erts_t2_register_blob` (§4); release permission. The
**entry stub** (emitted at the blob head, `06` §2.3): `adr ARG3, L_f`
(MFA contract) / `subs FCALLS,#1` / `b.le t2_yield_setup` / fall into the
body — **no** `enter_erlang_frame` (frame already pushed by `L_f+0`),
**own** FCALLS decrement (T1's `i_test_yield` at +12 is bypassed).
`t2_yield_setup`: `adr ARG3, L_f; b i_test_yield_shared` — resume PC =
`L_f+TEST_YIELD_RETURN_OFFSET` = `L_f+24` = T1 body (entry-yield demotes).

⚠ **Reach is the biggest install surprise.** The `JitAllocator` is a
**single process-wide global**, 32 MB blocks, and its `alloc(Out<Span>,
size)` takes **no address hint** (`jitallocator.h:152–177` — no near/base
field; every `mmap` passes `nullptr`). So `06` §2.7(a) co-location is
**not free**: a post-load T2 blob lands at an arbitrary address, possibly
>128 MB from `L_f+4`, out of `b` range. **P1 reach policy (correctness-
first):** try a direct `b` if the blob is in `±disp128MB`; else emit the
proven indirect **bridge veneer** (`emit_veneer`'s `ldr x16,=target; br
x16`, `beam_asm_module.cpp:1878–1885`) into a near slot *if one is
reachable*; else **reject the install** (function stays T1 — bounded,
behavior-preserving). Co-location (extend the vendored asmjit
`JitAllocator` with a hinted `mmap`, or reserve a ≤128 MB contiguous JIT
arena) is a **measured P1 sub-task** on Linux/macOS/Windows ARM64, not a
correctness blocker — reject-if-far still passes the suite.

**LOC** ~500 (install/revert) + ~150 (entry-stub emit) + ~250 (reach/
bridge pool). **Risks:** unseal/seal races against concurrent trace
install on the same module (the code-mod permission serializes, but the
precondition recheck must be *inside* the lock); the asmjit
allocator-hint extension touches vendored code (keep it a wrapper);
`0x14000002` literal must be asserted, not assumed.

---

## 4. Jettison + `c_p->i`

**Findings.** Purge's free-after-quiesce is the template: `erts_internal_
purge_module_2` schedules `erts_schedule_thr_prgr_later_op(resume_purger,
…)` + `erts_suspend` (`beam_bif_load.c:2122,2125`), and only frees
(`beamasm_purge_module`, `:2232`) once thread progress confirms no
scheduler is inside the old code. `c_p->i` is `Process.i` (`erl_process.h:
1131`); it is written on context switch in `process_main.cpp:133` (from
ARG3) and re-read + `br`'d on schedule-in (`:258,269`).

**Change spec.** `erts_t2_jettison(inst, fn)` (triggers: watchpoint,
trace-enable, eviction, exit-counter — `06` §5.1): under code-mod
permission, (1) revert +4 = `0x14000002` + `beamasm_flush_icache`; (2)
free any bridge slot; (3) `erts_t2_deregister_blob` (`t2_ranges.h:76`) and
move the blob to a tombstone list; (4) `erts_schedule_thr_prgr_later_op`
to release the `JitAllocator` span once no scheduler is inside (`06`
§5.5). O(1) in-lock; new callers hit T1 immediately; in-flight T2 callers
finish naturally.

⚠ **P1's `c_p->i` translation table is trivial — by construction.**
`08` §4.3: P1 blobs contain **no CPs** (non-tail calls push T1
continuations; there is no back-edge resume stub — that's P2). The only
T2 address the runtime could hold is `c_p->i` for a process yielded *at
the entry stub* — but `t2_yield_setup` routes through `i_test_yield_shared`
with `ARG3=L_f`, so the stored resume PC is **already `L_f+24` (a T1
address)**. Therefore no live `c_p->i` ever points *into* a P1 blob;
"c_p->i translation at uninstall" collapses to a **generation-checked
safety assertion** (schedule-in compares `p->t2_scan_gen` to a global
`t2_gen`; on mismatch, verify `c_p->i` is not in any tombstoned blob range
via `erts_t2_find_blob` — expected empty). The `t2_ranges` registration
*does* become real in P1 (so `find_blob` resolves PC→MFA for crash dumps /
`process_info(current_stacktrace)` if a scheduler is mid-blob), but its
`resume_tab` stays NULL. The real translation table lands with P2 stubs.

⚠ **Trace mutual-exclusion hook** (`06` §1.1, §2.4): `erts_install_
breakpoints` (`beam_bp.c:585`), before `erts_asm_bp_set_flag` (`:630`),
must call `erts_t2_jettison` for any T2-installed function ("trace always
wins"). Symmetric: `erts_t2_install` rejects when `breakpoint_flag!=0`.

**LOC** ~450 (jettison + tombstone + thr-progress free) + ~80 (trace
hook). **Risks:** the thr-progress-later free must not run under the same
suspended process that triggered it (mirror the purger's two-phase
resume); tombstone accounting vs `erlang:memory(code)`; the trace hook
sits on a hot path (guard it on a cheap "any T2 installed in module" flag).

---

## 5. The tier-forcing flag (`+JT2enable`)

**Findings.** `+J` parsing is `erl_init.c:1700` (`case 'J'`, under
`#ifdef BEAMASM`), a `switch(sub_param[0])` at `:1705` with arms `D`
(`+JDdump`→`erts_jit_asm_dump`), `P` (`+JPperf`), `M` (`+JMsingle`→
`erts_jit_single_map`). ⚠ **T2 currently rides env vars** (`T2_RETAIN`
`t2_retain.c:46`, `T2_BUILD`, `T2_SELFTEST`) — there is **no `+J` flag and
no runtime call-counter / dynamic tier-up**; selection is the static
`t2_mvp_is_target()` hardcoded list (`beam_asm_module.cpp:242`).

**Change spec.** Add `case 'T':` to the `:1705` switch → parse
`T2enable`/`T2force`, setting a new `int erts_jit_t2_force`
(define in `beam_jit_main.cpp`, extern in `beam_asm.h`; `+JT2enable`
implies retention). Usage line near `erl_init.c:600`. **Compile-at-load
driver** (`t2_compile.cpp`, **synchronous — no compile queue in P1**): at
the existing retain/pctab site (`jit/asm_load.c:1279–1290`, already holding
load permission), if `erts_jit_t2_force`, iterate the eligibility bitmap;
per eligible function: `t2_build_function` (P0) → isel → regalloc → emit to
a fresh `JitAllocator` blob → `erts_t2_install`. Eligibility interaction:
the bitmap (`t2_eligible.c`) already gates retention + pctab; the flag
compiles exactly that set; ineligible functions cost nothing.

⚠ **Bound the load-time cost.** This is a *load-time*, not steady-state,
cost — production uses the counter-triggered async path (P2+). P0 built
+ validated **8,661 functions / 161 modules with zero failures,
deterministically**; build is fast, emit adds asmjit codegen (µs–low-ms
per function, `04` matrix). For the full OTP boot+suite (thousands of
functions) synchronous compile-at-load must be **measured and reported**
(target: bounded, e.g. ≤2× module-load latency); if a module blows the
budget, fall back to lazy/skip. State it as a known flag cost, not a
regression.

**LOC** ~120 (flag) + ~350 (synchronous driver). **Risks:** compiling
under load permission blocks other loaders — chunk work or accept the
serialization for the forcing flag only; a build/emit failure on any
function must degrade to T1, never abort the load.

---

## 6. Test plan

**Highest-risk suites for state-model bugs** (`07_delivery.md` §16A.1 —
the identity transform's permanent regression harness):

- **`process_SUITE`** — reductions (`process_info(_,reductions)` must
  match T1: entry stub + `emit_dispatch_return` charge identically),
  spawn/exit/links, `current_stacktrace`/`backtrace` (byte-identical by
  the no-CPs-in-blobs construction).
- **`exception_SUITE`** + `error_handler` — the **T2-never-raises** proof:
  every would-raise (`badmatch`, arith on non-numbers, guard failure)
  must side-exit to the T1 PC and let T1 raise identical class/reason/
  stacktrace/line.
- **trace suites** — `trace_SUITE`, `trace_call_*`, `trace_bif_SUITE`,
  `seq_trace_SUITE`: the jettison-on-trace-enable mutual-exclusion (§4).
- **`code_SUITE`** — purge/reload/upgrade with processes mid-execution:
  watchpoint jettison + thread-progress free (§4).
- **`nif_SUITE`**, `fun_SUITE`, `bs_*_SUITE` (mostly ineligible → dense
  side-exit/demote coverage), `gc via` `beam_*` — the sync-point paths.

**Run mechanics (verified `HOWTO/DEVELOPMENT.md:206–228`):**
`ERL_ARGS="+JT2enable true" make emulator_test ARGS="-suite process_SUITE"
TEST_NEEDS_RELEASE=false` (ERL_ARGS injects the emulator flag,
`:222–228`). Stdlib smoke: `ERL_ARGS="+JT2enable true" make stdlib_test
ARGS="-suite lists_SUITE"` for `lists/maps/binary/string/base64/json/
unicode`.

**Identity-transform acceptance:** full `make emulator_test` + the stdlib
smoke slice, **all green with `+JT2enable true`**. Because P1 does zero
optimization, *any* behavioral delta vs T1 is a state-preservation bug and
blocks every later phase.

---

## Work order — PR-sized commits (each compiles + is testable)

1. **LIR + T2 assembler scaffold + build wiring.** `t2_lir.hpp/.cpp`,
   `BeamT2ModuleAssembler` subclass, Makefile/`BEAM_CPP_SRC` extension
   (the P0 `beam/jit/t2/` gap). *Test:* clean `make`; a hand-built 1-op
   LIR emits and disassembles under a `T2_EMIT_SELFTEST` hook.
2. **Isel + regalloc (sync-everything) + straight-line emission.**
   Arithmetic/guard/comparison/data ops via reused T1 emitters,
   `Fail`→T1-PC. *Test:* emit `t2_mvp:diff/2`; a harness calls the blob
   and asserts result == T1.
3. **Terminators + calls + error exits.** Tail-call re-enter, non-tail
   demote-on-return CP, return, `select_val`, error-block→T1-PC. *Test:*
   `t2_mvp:total/2` emits and runs correct over a reference list.
4. **Blob alloc + reach + prologue install/revert + entry stub +
   `t2_ranges` population.** *Test:* install `total/2`, call via T2, get
   correct result; revert, call via T1; `find_blob` resolves the MFA.
5. **`+JT2enable` flag + synchronous compile-at-load + jettison +
   thr-progress free + trace mutual-exclusion hook.** *Test:* boot
   `+JT2enable`, load `t2_mvp`, functions installed; `trace_pattern`
   jettisons; `code:purge` frees after progress.
6. **Identity-transform gate.** Run full `emulator_test` + stdlib smoke
   under `+JT2enable`; drive state-model bugs to green. *Test:* the gate
   itself (green = P1 exit).

---

## Consolidated surprises

1. Per-op T1 emitters are `protected` and take loader `ArgVal` types, not
   asmjit operands — reuse via a subclass that **synthesizes `ArgVal`**;
   the true register seam is `mov_arg` (`beam_asm.hpp:~1840`).
2. `JitAllocator` is a single global with **no address hint**
   (`jitallocator.h:152`) — co-location (`06` §2.7a) is *not free*; P1
   reach = direct-`b`-else-bridge-veneer-else-reject (correctness-first),
   with co-location a measured sub-task.
3. P1's **`c_p->i` translation is empty by construction** — entry-yield
   already stores a T1 resume PC (`L_f+24`); `t2_ranges` is populated only
   for PC→MFA introspection, `resume_tab`=NULL until P2.
4. Non-tail calls **cannot** reuse `emit_i_call` (`bl` pushes a T2 CP) —
   hand-emit with the T1-continuation CP from the pctab (`08` §4.3).
5. T2 has **no `+J` flag and no counter yet** — everything rides env vars
   and a hardcoded static target list; P1 adds `case 'T'` at
   `erl_init.c:1705` and a synchronous compile-at-load driver.
6. Use `erts_try_seize_code_mod_permission` (`code_ix.c:272`), not
   load-permission — the prologue patch mutates an existing module,
   needing no code_ix staging.
7. Errors/`badmatch`/`if_end`/`case_end` lower to a **branch to T1**, not
   the builder's `erlang:error` tail-call — T2 never raises.
