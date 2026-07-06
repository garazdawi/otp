# T2-Full P2 — Source-Grounded Implementation Map

Scope: the **loop tier** — the phase where the measured wins live. P1
closed with the full paired sweep green (`06_phases.md`), delivering the
identity pipeline (HIR→LIR→isel→*sync-everything* regalloc→emit via
reused T1 emitters), prologue-patch install/jettison, `t2_ranges`
(PC→MFA, `resume_tab`=NULL), the pctab with six re-entry kinds, and
`+JT2enable` synchronous compile-at-load. P2 turns that identity blob
into an *optimized* blob and makes tier-up profile-driven. Rung-1 deopt
(re-call, no CPs/framestates) is unchanged; the one new runtime address
class is the back-edge resume PC (§5). Paths under
`/Users/lukas/code/otp-beamjit2/`. aarch64 only. **Read-before-build
surprises flagged ⚠.** Governing invariant (`08_v1_loop_tier.md` §4):
T2 matches T1's X/Y/HTOP/FCALLS/stack at every **sync point**; between
sync points registers are free — P2 is the first phase that *uses* that
freedom.

Exit gate **G2**: MVP ≥1.8×, G-bin *bytewise* ≥2.5× isolated, ≤1 % tax
on the app corpus. **G4**: `foldl`/`map` vs the `inline_list_funcs`-on
baseline.

---

## 1. Profile-driven tier-up (replacing compile-everything)

**Findings.** P1 has **no counter and no dynamic tier-up**: selection is
`erts_jit_t2_force` (`beam_asm.h:58`, `beam_jit_main.cpp:57`), parsed at
`erl_init.c:1798`, driving synchronous compile-at-load at
`asm_load.c:1307` → `erts_t2_compile_module` (`t2_compile.cpp:238`),
which walks `t2_build_each` and installs every eligible function under
load permission. Eligibility is a per-module bitmap
(`erts_t2_eligibility_scan`, `t2_eligible.c:122`) built by a decode
pre-pass. The T2 entry stub already owns its reduction check
(`emit_t2_entry_stub`, `t2_emit.cpp:276`: `mov ARG3=L_f; subs FCALLS,#1;
b.le i_test_yield_shared`). The debug hook proves the off-load-path
install works: it seizes **code-mod** permission (not load) via the
purge-style yield (`t2_compile.cpp:381`, `erts_t2_debug_install`).

**Change spec.** (a) **Counter + entry-type slots** (`02_profiling.md`
§§7.2–7.4). Per eligible function, a `T2FunctionProfile { u32 call_count;
u16 num_slots; T2ProfileSlot slots[] }` sized by arity. Emit the counter
increment + per-arg `{seen_types|=tag; count++}` in the **T1**
function-entry sequence, right after `emit_i_test_yield`, at the existing
hook `t2_mvp_is_target()` (`arm/instr_common.cpp:3160`,
`beam_asm_module.cpp:242`) — the six-instruction sequence of `02` §7.3.
**Per-scheduler-sharded, scheduler-1-only writes** (`02` §7.3): one
cache-line-aligned shard vector per scheduler; the counter increment is
unconditional but scheduler≠1 stores to a throwaway shard, so no
contention and the ≤1 % budget holds. ⚠ **The counter must be emitted
only for eligible functions, but the eligibility bitmap does not exist at
codegen time** (`t2_pctab.h:163` note — `erts_t2_prepare` runs *after*
`load_code`). Resolution: run `erts_t2_eligibility_scan` **before**
codegen (hoist the decode pre-pass ahead of `beam_load_prepare_emit`), so
the entry emitter gates counter+slot emission on the bit. This is a
load-ordering change, verified cheap (the scan already decodes once).
(b) **Threshold** (`05_runtime.md` §15.1):
`base·sqrt(size+1)·2^recompiles·M/(M−U)`, `base=1000`. On trip, CAS
`call_count` to a "pending" sentinel to suppress duplicate enqueues.
(c) **Profile stability** (§15.2): ≥75 % slots observed + bitmasks stable
for N=64 ticks, else reset (≤5 retries). (d) **Compile queue** (§15.3):
a C-side MPSC ring + **one dirty-CPU worker**; the worker seizes code-mod
permission and runs the existing `t2_compile_install_one`
(`t2_compile.cpp:105`) path — already proven off-load in the debug hook.
High-water → drop (counter retrips later). `+JT2enable` stays as the
forced compile-everything mode; production is counter-triggered.

**LOC** ~250 (profile side-table + sharded emit in T1) + ~200 (threshold
+ stability) + ~300 (MPSC queue + dirty worker) + ~80 (eligibility
hoist). **Risks:** the counter store on every eligible-function entry is
the one always-on tax — gate G2's ≤1 % is measured here first; the
sharded/sched-1 design is the mitigation but must be benched on the app
corpus. Duplicate-enqueue and jettison-reset races against the worker
(the pending sentinel + single worker serialize it).

---

## 2. Entry speculation + flag-checked arithmetic

**Findings.** The HIR already declares the speculative op family —
`UntagInt/TagInt/AddSmall/SubSmall/MulRaw` (`t2_hir.hpp:157`) and
`SpeculateType/SpeculateRange` (`:164`) — **reserved, never emitted in
P1** (the validator rejects them; the LIR has no counterpart, only
generic `Add/Sub/...`, `t2_lir.hpp:111`). Generic arith lowers to
`emit_i_plus/...` with `Fail`→T1 EFFECT-PC (the pctab `ERTS_T2_PC_EFFECT`
entry). The overflow route is **flags, not `speculate_range`** (`08`
§4.4, superseding `03` §9.3): compute with flag-setting instructions,
`b.vs` (overflow) to the side-exit, commit after — "deopt before the
*commit*". T1's own small-int fast path (`arm/instr_arith.cpp`) is the
template.

**Change spec.** A **speculation-insertion pass** (HIR, after re-inference)
reads entry-type slots + `Type` chunk: for a `gc_bif` arith op whose
operands are profiled/proven small-int, replace generic `Add/Sub` with
`AddSmall/SubSmall` guarded by the *one-untag* trick (`03` §9.4: `tagged +
untag(b)` preserves the tag; `Mul` untags both, retags). Emit the untag
into scratch, the flag-checked op, `b.vs` → EFFECT-PC side-exit, commit
the tagged result to the canonical slot. New LIR kinds `AddSmall/SubSmall/
MulRaw/UntagInt/TagInt` + emitters mirroring `instr_arith.cpp`. Entry
speculation proper: a `SpeculateType` at function entry (or hoisted to a
loop preheader, §4) that `b.ne`→ the ENTRY-PC re-call boundary if a param
isn't the profiled type — a single tag-bit test (`03` §9.1). ⚠ **The
untagged scratch must never be a `PhysLoc::XReg/YReg` and never live
across a sync point** (GC would misread it, `11.2`): the regalloc/validator
untagged-tracking rule (§4) enforces `tag_int` before the next sync
boundary.

**LOC** ~400 (spec-insertion pass) + ~350 (flag-checked emitters) + ~120
(entry-guard emit). **Risks:** the corruption class (§4) — an untagged
value escaping to a sync map, or a speculation whose type isn't
dominating-guarded. The IR validator extension is the guard.

---

## 3. Guard fusion (AND-combining, the MVP rule)

**Findings.** P1 lowers each type test to its own `emit_is_*` with
`Fail`→T1-PC (`t2_lir.hpp:78`). The MVP's validated rule (`08` §4.4,
closing): *when fusing type checks into one mask, **AND not OR** — the
combined predicate must require every input to satisfy every bit.*

**Change spec.** A **guard-fusion pass** (HIR, after guard strength
reduction, `03_optimizer.md` §1 pipeline slot) with the MVP MVP rule
only: when consecutive guards on the *same* value (or a preheader's
several param guards) all target the *same* fail boundary, combine into a
single masked test — `and tmp, val, MASK; cmp tmp, PATTERN; b.ne fail` —
where MASK/PATTERN are the **conjunction** of the per-guard tag bits. A
guard that targets a *different* fail PC cannot fuse (its re-execution
window differs). This is the enabler for LICM's preheader hoist (§8): N
per-iteration param checks become one preheader check.

**LOC** ~250. **Risks:** OR-fusion is the Finding-5 corruption — the pass
must assert the combined mask is a strict AND and that all fused guards
share one fail target; the validator re-checks.

---

## 4. Loop recovery + the regalloc relaxation (the core)

This is the phase's load-bearing change and its highest-risk one.

**Findings.** `t2_regalloc.cpp` is today a **verifier, not an allocator**
(`Verify::run`, `:78`) — isel places every value in its canonical slot
(`dst_reg`/`operand_regs` from the builder), so sync-everything holds by
construction and there is nothing to allocate. The interval/pin vehicle
exists but is unused: `PhysLoc::Phys` (`t2_lir.hpp:194`), reserved "for
P2's per-op scratch and range-carrying values" (`:190`). The builder
already attaches, per sync-point op, a `T2SyncMap { x_live; x[]; frame_size;
y[] }` (`t2_hir.hpp:347`) naming the exact BEAM state — **this map is the
pin-constraint set the allocator consumes**. Loop structure is *side
data* (`04` §10.5 `LoopInfo`): the builder does not model loops; P2 adds
the analysis.

**Change spec — loop recovery** (HIR): a `LoopInfoPass` (dominators →
back-edges → block groups, `04` §10.5). For a self-tail-recursive
function, rewrite the recursive `TailCall`/`TailCallExt` to self into a
**back-jump** to a loop header, with **phi** nodes at the header merging
formal params (entry values from the preheader, updated values from the
latch). Entry guards + invariant param-type checks **hoist to a
synthesized preheader** (run once per invocation). Reductions are charged
at the **back-edge**, identical to T1's per-call charge (`08` §5.4).

**Change spec — the regalloc relaxation** (the interval API made real).
Replace the verifier with a **Wimmer linear-scan-on-SSA** allocator
(`03` §1 pipeline, `11.2`):

- **Which boundaries stay sync points** (pinned): every op carrying
  `op->sync != null` — function entry, `Call/CallExt/TailCall*`,
  `CallBif`, `Return`, `GcTest`, `Allocate`, `gc_bif` arith, the
  back-edge yield check (§5), and each re-call/side-exit boundary. At
  each, every live X/Y value **must** occupy its canonical slot (the
  `sync->x[i]`/`sync->y[j]` value); the allocator treats the sync map as
  a set of *fixed-register-use* constraints (LSRA fixed intervals) and
  inserts resolution moves (spill to `Yn` / reload) where an interval
  sits in a `Phys` at a boundary that pins it to a slot.
- **What relaxes** (between sync points): a value whose live interval
  crosses **no** sync point may be assigned a `PhysLoc::Phys` and stay in
  a CPU register across ops. The win: a loop-carried accumulator that
  only crosses the header phi, the body arithmetic, and the back-edge
  keeps living in a register through the straight-line body instead of
  the P1 store-to-`Yn`/reload every op. `TMP1..6` remain per-op scratch;
  `XREG0..3` (`x25..x28`, callee-save) are the natural Phys pool for
  loop-carried values.
- **Interval representation**: `T2Interval { value_id; start_lir; end_lir;
  fixed_slot (canonical Xn/Yn from the sync maps, or None); assigned_phys;
  bool untagged }`. Sync maps seed `fixed_slot` at their op positions;
  the allocator honors them and colors the rest.

⚠ **The deopt/corruption rule (MVP Finding-5).** Registerizing a
loop-carried value is only legal when *its type is established by a
dominating guard or proof* (`08` §4.4). Loop-header phis are where this
is easy to get wrong: the phi merges the **preheader-proven** entry value
with the **latch** value; if the latch value's type isn't re-proven (by
the flag-checked op that produced it, §2), a later speculative consumer
reads a mistyped register. Two validator invariants enforce it: **(i)**
no `untagged` interval appears in any `op->sync` map (the `11.2` untagged
rule — untagged values are excluded from sync constraints *and* the
walker asserts it); **(ii)** every speculative consumer's operand type is
dominated by a guard on **every** incoming phi edge (the AND, not OR, of
§3). The existing HIR register-state walk (commit `ce9b0729df`: "the
validator hard-errors when a sync map names a value the walked register
state does not hold") extends to walk *typed* register state across the
back-edge.

⚠ **Re-execution window legality** (`08` §4.2, guards-before-effects).
The loop deopt shape is "re-execute the iteration from the header-phi
values as a fresh call" (`foldl` at element k ≡ `foldl(F, Acc_k,
Rest_k)`). So the header-phi values are the re-call argument vector, and a
**window validator** partitions each loop body at effect boundaries
(`CallBif` with a `!`/`ets`/`put` effect, non-tail `Call`) and asserts
**every deopt-able guard precedes the first effect** in its window. A
guard after an effect must deopt at the post-effect boundary (a fresh
window whose re-call target is the CONT pctab entry), never re-execute the
effect. Allocation is **not** an effect (abandoned partial iteration =
garbage — `08` §3), so `MakeList/MakeTuple` (heap covered by a preceding
`GcTest` sync point) live freely inside a window.

**LOC** ~600 (linear-scan allocator + resolution moves) + ~350
(`LoopInfoPass` + recovery rewrite + preheader synth) + ~250 (window +
typed-state validator extensions). **Risks:** the entire state-model
class. The forced-deopt harness (`07` §16A) and the extended IR validator
are the gates; the identity suite still runs (a recovered loop with
speculation *off* must stay byte-identical).

---

## 5. Back-edge yield-resume + the `resume_tab`

**Findings.** P1's entry stub demotes entry-yields to T1 (resume PC =
`L_f+24`, `t2_emit.cpp:276`), so **no `c_p->i` ever points into a P1
blob** and `resume_tab` stays NULL (`t2_ranges.h:60`; jettison only
DEBUG-asserts no `c_p->i` in the span, commit `34b8a88883`).
`process_main` writes `c_p->i` from ARG3 on context-out and re-reads +
`br`s on schedule-in (`process_main.cpp:133,258,269`). Without back-edge
resume, any invocation longer than one timeslice runs its remainder in T1
— forfeiting exactly the long loops the tier targets (`08` §4.5).

**Change spec.** At each recovered loop's back-edge, emit `subs FCALLS;
b.le <back_edge_yield_setup>`. The setup **syncs loop state to canonical
slots** (the back-edge sync map — the allocator's resolution moves land
here), sets `c_p->i` to a **T2 resume PC** (into the blob, at the
post-check loop-header re-entry), and yields. This is the **first T2
address in `c_p->i`**. `resume_tab` becomes real: a per-blob sorted array
`{ resume_pc_offset → (beam_idx, t1_resume_pc) }`, passed to
`erts_t2_register_blob` (`t2_ranges.h:69`, the currently-NULL arg),
populated from emitter-recorded back-edge points. Schedule-in just `br`s
the T2 resume PC (executable). ⚠ **`c_p->i` translation becomes real at
jettison**: the P1 DEBUG assert "no `c_p->i` in span" is *replaced* by a
walk of the process table — for any `c_p->i` in a tombstoned span,
`erts_t2_find_blob` → `resume_tab` lookup → rewrite to the equivalent T1
resume PC. Because the saved state *is* a valid fresh-call vector, that
target is just the loop header's T1 PC (the `ERTS_T2_PC_ENTRY`/loop-head
pctab entry) — "point it at T1 entry" (`08` §4.5). Jettison stays O(1) +
one process-table pass (mirror the purger's thread-progress two-phase,
`beam_bif_load.c:2122`).

**LOC** ~300 (resume stub emit + `resume_tab` build/register) + ~200
(jettison `c_p->i` translation). **Risks:** a resume PC surviving into
`c_p->i` across a trace-jettison race — the code-barrier
(`erts_schedule_code_barrier`, already used) plus the translation pass
close it; the translation must run *before* the span free, under code-mod
permission.

---

## 6. Local leaf inlining (the `diff/2`→`total/2` class)

**Findings.** The MVP fused `diff/2` into `total/2`'s loop for its 1.97×
(`08` §2.1). Eligibility already admits local `call`/`call_last`/`call_only`
(`t2_eligible.c:69`); isel resolves local targets by MFA against the code
header (`t2_isel.hpp:47`). No fun ops are eligible.

**Change spec.** An **inliner pass** (HIR, `03` §2, Maglev-tiny budgets:
size ≤24 IR ops, depth ≤3, call-free leaf). Splice the callee's blocks
into the caller, rename SSA values, renumber the callee's Y slots into the
caller's frame (or reject if it needs a frame the caller can't host).
**Re-call deopt (rung-1, no framestates):** a guard in the inlined leaf
side-exits to the **enclosing window's** re-call boundary (the loop
header, or the caller's entry) — *not* the callee's T1 entry, because the
T2 call never happened. The inlined leaf is "covered by the enclosing
iteration's re-execution" (`08` §4.2), so its guards obey the same
guards-before-effects rule (§4). Skip previously-deopted callees
(`03` §2.4, per-site keys).

**LOC** ~450 (inliner + Y-renumber + re-call retargeting). **Risks:** Y
renumbering vs the sync-map contract — the caller's post-inline sync maps
must still name valid frame slots; the validator's frame-dataflow
fixpoint (commit `ce9b0729df`) re-checks. Deopt targeting the *caller's*
boundary, not the callee's, is the subtle correctness point.

---

## 7. The byte-aligned binary scan subset (eligibility widening)

**Findings.** Eligibility today has **zero `bs_*` ops** (`t2_eligible.c:48`
— no binaries, no maps, no funs); this is the closed set the builder
mirrors. The G-bin win (5.6× isolated; ≥2.5× bytewise is the P2 gate,
SWAR is the §7 package post-P2) needs match-context registerization. T1's
emitters exist: `emit_i_bs_start_match3` (`arm/instr_bs.cpp:134`) creates
the context; the modern unified `emit_i_bs_match` (`:3116`/`:3122`) walks
a **command list** (`ops.tab:960` `bs_match Fail Ctx Size Rest=*`) whose
byte-aligned subset is `ensure_at_least` (`:3150`), `integer` (size 8,
unit 1), `skip`, `get_tail` (`:3384`); `emit_bs_test_tail` (`:589`) and
`emit_bs_skip_bits` round it out.

**Change spec.** Widen `erts_t2_genop_supported` to admit
`bs_start_match3`, `i_bs_match`/`i_bs_match_test_heap` (byte-aligned
commands only — reject any command with a non-8 size/unit or a variable
size at the eligibility scan, so mis-aligned matches stay T1), and
`bs_test_tail`. Add HIR ops `StartMatch`, `BsMatch` (carrying the decoded
command subset), a **match-context SSA value** that is loop-carried.
Isel reuses `emit_i_bs_start_match3`/`emit_i_bs_match` with `Fail`→T1-PC.
The G-bin registerization is the §4 relaxation applied to the
match-context: the context term stays in its canonical slot at sync
points (it is GC-visible), but position/end are held in registers across
the window between sync points, and across iterations the context is the
loop's IV (`03` §5: "binaries use the match context as the IV,
registerized by loop recovery"). ⚠ **G1 re-runs with the wider corpus**:
the 120/295 P0 functions marked `not_eligible` at the narrow op set
(`06_phases.md` P0 note) now partially become eligible; `t2_g1_SUITE`
re-runs to confirm SSA-reconstruction fidelity for the `bs_*` shapes
before the win is trusted.

**LOC** ~300 (eligibility + HIR ops) + ~350 (isel + match-context
loop-carry) + ~150 (byte-align rejection at scan). **Risks:** the match
context is a heap object with GC-visible internal pointers — the sync-map
contract must keep the *whole context* in a slot at every GC/sync point;
only pos/end registerize. Getting this wrong is a GC-corruption bug, not a
wrong-answer bug. Bit-unaligned commands must be rejected at the *scan*,
not discovered at isel.

---

## 8. `lists:*` intrinsics with literal funs + LICM-lite

**Findings.** Eligibility has no `make_fun`/`call_fun` and no `lists:*`
inlining. `03` §2.3 / `04` §10.4: hand-port the `sys_core_fold_lists.erl`
expansions (the 10 higher-order BIFs) into C++ HIR-builder calls; a
`lists:foldl(LiteralFun, Acc, List)` site with an SSA-constant fun inlines
the helper (loop-recovered) + the fun body (constant propagation), no
speculation, no deopt (`03` §9.6 constant-fun case).

**Change spec.** Widen eligibility to admit `call_ext` to the annotated
`lists:*` wrappers and `make_fun2`/`call_fun` **only when the fun is a
literal** (SSA-constant `MakeFun`). Port `foldl/map/foreach/filter/all/any`
expansions to `t2_hir_builder`-level emitters. On such a site: inline the
wrapper, loop-recover its recursive helper (§4), inline the literal fun's
body by constant propagation. **LICM-lite** (`03` §4, `04` §10.6): hoist
invariant param-type guards, the captured-fun environment loads, and
invariant pure computation to the preheader (nothing hoists across a sync
point unless provably re-executable — the window rule, §4). ⚠ Elixir
`Enum` wraps funs as *parameters*, so these intrinsics fire for direct
`lists:*` (pervasive in Erlang) but **not** through `Enum` — that is the
P3 cross-module case (`08` §6).

**LOC** ~600 (six ported expansions) + ~250 (literal-fun eligibility +
inline) + ~250 (LICM-lite). **Risks:** drift between the C++ port and
`sys_core_fold_lists.erl` — caught by the identity suite (`04` §10.4). G4
**must** bench against `inline_list_funcs`-on, or it is a strawman
(`04` §10.4 note).

---

## Work order — PR-sized commits (each compiles + is gated)

1. **Real regalloc: linear-scan-on-SSA, sync-map-pinned, no relaxation
   yet.** Replace the `t2_regalloc.cpp` verifier with the allocator, but
   keep every interval pinned to its canonical slot (behaviorally = P1).
   *Gate:* identity suite still byte-identical (proves the allocator +
   resolution moves are sound before any register is freed).
2. **Loop recovery + preheader + back-edge (no relaxation, no
   speculation).** `LoopInfoPass`, recovery rewrite, preheader synth,
   back-edge reduction charge. *Gate:* identity suite green on
   self-recursive corpus; reductions byte-identical.
3. **Regalloc relaxation + typed-state/window validators.** Loop-carried
   values in registers across the body; the two corruption-class
   invariants (§4). *Gate:* forced-deopt harness green; identity suite
   green with speculation still off.
4. **Entry speculation + flag-checked arithmetic + guard fusion (AND).**
   §§2–3. *Gate:* MVP `total/2` reproduces **≥1.8×** (G2 half-one)
   through the full pipeline; wrong-type inputs side-exit byte-identically.
5. **Back-edge yield-resume + real `resume_tab` + `c_p->i`
   translation.** §5. *Gate:* a >1-timeslice recovered loop resumes into
   T2 (not T1); jettison mid-loop translates `c_p->i`, correct result;
   `code:purge`/`trace_pattern` across a mid-loop yield green.
6. **Local leaf inlining, re-call deopt.** §6. *Gate:* MVP `diff/2`
   fused into `total/2`; the full **≥1.8×** MVP number lands (G2).
7. **Binary scan subset + eligibility widening + G1 re-run.** §7.
   *Gate:* **G-bin bytewise ≥2.5× isolated** through the pipeline;
   `t2_g1_SUITE` fidelity green on the wider corpus.
8. **`lists:*` intrinsics + LICM-lite.** §8. *Gate:* **G4** —
   `foldl`/`map` beat the `inline_list_funcs`-on baseline.
9. **Profile-driven tier-up (counter + slots + queue + worker); make it
   default, `+JT2enable` = forced mode.** §1. *Gate:* **≤1 % tax on the
   application corpus** (the G2 third-clause; the profiling budget is
   measured here, on the whole pipeline, last — so the tax is real
   steady-state, not load-time).

**G2** is met at commits 4+6 (MVP ≥1.8×), 7 (G-bin bytewise ≥2.5×), and 9
(≤1 % tax). **G4** at commit 8. Ordering rationale: the allocator and
loop recovery land *behaviorally identical* first (commits 1–2, still
gated by the identity suite), so the state-model risk is isolated from the
optimization risk; speculation (the corruption class) rides on validators
that exist before it (commit 3); tier-up profiling lands last so its ≤1 %
budget is measured against the finished pipeline, not a moving target.

---

## Consolidated surprises

1. **`t2_regalloc.cpp` is a verifier, not an allocator** — P2's single
   biggest build is writing the real one, and the `T2SyncMap` the builder
   already attaches *is* its pin-constraint set (no new metadata needed).
2. **The counter cannot be gated at codegen** — the eligibility bitmap is
   built after `load_code`; P2 must hoist the eligibility scan ahead of
   codegen so T1 emits the counter only for eligible functions.
3. **`resume_tab` and `c_p->i` translation go from stubs to real** the
   moment back-edge resume lands — this is the first T2 address in
   `c_p->i`, and the P1 jettison "no `c_p->i` in span" *assert* becomes a
   *translation pass*.
4. **The match context is GC-visible** — only pos/end registerize; the
   context term itself stays in its canonical slot at every sync point, or
   it is a GC-corruption bug. Bit-unaligned `bs_match` commands must be
   rejected at the eligibility scan, not at isel.
5. **The corruption class is enforced by two validator invariants**, not
   by care: no untagged interval in any sync map, and every speculative
   consumer's type dominated on *every* phi edge (AND, never OR). Both
   extend the existing HIR register-state walk from `ce9b0729df`.
