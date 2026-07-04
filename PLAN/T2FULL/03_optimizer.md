# T2-Full — The Optimizer

> The pass pipeline and the design of each mandated optimization:
> cross-module inlining, LICM, unrolling, GVN, escape
> analysis/allocation sinking, float unboxing. Mechanics shared
> with the reference design are cited into
> [`../T2/03_compilation_and_speculation.md`](../T2/03_compilation_and_speculation.md)
> and [`../T2/04_optimization.md`](../T2/04_optimization.md).
> Calibration numbers are from the July 2026 prior-art survey
> (per-optimization ablations in JSC/V8/Graal/Renaissance).

## 1. Pipeline

Inherited 16-pass skeleton
([`../T2/03_compilation_and_speculation.md`](../T2/03_compilation_and_speculation.md)
§8.1) with the additions marked •:

```
build IR ← loaded BEAM code + Type chunk
type inference (forward dataflow, beam_ssa_type port)
profile overlay (entry/site types, targets, shapes, branch weights)
inlining (elimination-scored, cross-module; framestates + eager CP)   ← §2
re-inference over the fused region
speculation insertion (guards from profile, 03 §9)
• GVN/CSE (backward-chained, LuaJIT/Maglev-style) + const-fold + DCE
guard strength reduction + guard fusion (AND-combining, MVP rule)
loop analysis (LoopInfo) + loop recovery of tail recursion (04 §10.5)
LICM                                                                   ← §4
unrolling + SWAR recipes + test_heap re-placement (04 §10.6)          ← §5
• escape analysis → allocation sinking → float unboxing               ← §6
speculative-arith lowering (one-untag, 03 §9.4)
LIR: isel → linear-scan-on-SSA regalloc (sync-point pins) → emit
```

Budget: ~1 ms median / 10 ms cap per compilation unit; abort → T1
(03 §8.4). Every pass must be O(n log n)-ish in region size; the
inliner's expansion cap (§2.4) is what keeps n bounded.

## 2. Inlining — the enabler, scored by elimination

### 2.1 Policy

The G3-2 null is a design input, not an obstacle: call overhead is
worth ~zero on BeamAsm (a predicted `bl/ret` pair), so the inliner
**never inlines for overhead**. A candidate call site is inlined
only when its **elimination score** clears a threshold. The score is
computed on facts, not vibes — each term corresponds to a concrete
optimization the fused region will perform:

| score term | fact source | what gets eliminated |
|---|---|---|
| subsumed callee guards | caller-side inferred/speculated types vs callee entry checks | the callee's `is_*` prologue re-checks — the boundary tax the Type chunk cannot cross |
| constant arguments | SSA constants at the site; **literal funs score highest** | dead branches, folded arithmetic, `call_fun` → direct body (the `lists:*`/`Enum` collapse) |
| construct/deconstruct pairs | callee returns a fresh tuple/cons the caller immediately destructures (or vice versa for arguments) | the allocation itself — feeds §6; the `{ok, X}` tax |
| loop-recovery unlock | callee is self-tail-recursive and the site sits in/feeds a loop | the whole loop-tier win class opens across the boundary |
| reduction/CP pair removal | always present but weighted near zero | the ~5-6 instruction call tax (kept honest by G3-2) |

Sites score zero → not inlined, ever, regardless of hotness.
Callee bodies come from the same SSA-reconstruction path as root
compilation (works on every loaded module, including other
applications' — *cross-module by construction*). Callee identity at
remote/fun sites comes from the monomorphic-target profile slots
([`02_profiling.md`](02_profiling.md) §2); polymorphic sites up to
a small fan-out get guarded dispatch (the PIC design,
[`../T2/03_compilation_and_speculation.md`](../T2/03_compilation_and_speculation.md)
§9.6) only when every arm scores.

### 2.2 Correctness machinery

Inherited whole from the reference design: framestates +
`parent_fs` chains (codegen-only), eager-CP-push, per-region deopt
stubs, remapped callee world
([`01_architecture.md`](01_architecture.md) §2 rung 2). Deopt
target invariants and the guards-before-effects window rule are
unchanged. Module dependency: every inlined callee registers a
watchpoint on its defining module; reload/trace jettisons the blob
(the granted simplification).

### 2.3 Intrinsics

The `lists:*` higher-order intrinsics
([`../T2/04_optimization.md`](../T2/04_optimization.md) §10.4)
remain the curated fast path for the highest-value shape, shipping
in the loop-tier phase before general inlining exists. General
inlining then subsumes the mechanism (an `Enum.map/2` call with a
literal fun is just a high-scoring site) — the intrinsic table
stays as the guarantee that the top-10 shapes never regress on
inliner heuristics.

### 2.4 Heuristics

Size ≤ 24 IR ops per callee (Phase-0 calibrated), depth ≤ 3,
region expansion cap 5×, skip previously-deopted callees
(per-call-site keys, App C H10), never inline unaudited
effect-classes. Production calibration range (verified from
sources, July 2026): HotSpot C2 inlines ≤35 bytecode bytes
normally / ≤325 when frequent, depth ≤15 (the 9→15 bump bought up
to +70 % on Scala benchmarks — abstraction-heavy code rewards
depth); V8 Maglev (the mid-tier analog of T2) uses deliberately
tiny budgets — depth 1, ≤100 bytecode bytes, ≥0.95 call frequency;
JSC DFG uses depth ≤5, cost ≤80. Our budgets sit deliberately at
the Maglev end for P3, with depth/size increases gated on measured
compile-time and win data, not taste.

### 2.5 Why this policy is credible — the enabler evidence

The strongest published evidence agrees with our own nulls, in
both directions:

- **Graal's CGO'19 "deep inlining trials"** — the inliner that
  *models unlocked optimizations* (propagate argument types, run
  the optimizer across the candidate scope, score by triggered
  optimizations) — gained 5 %–3× over HotSpot C2 on 21/28
  benchmarks; its ablation shows the enabler-modeling component is
  worth +8–59 % on abstraction-heavy Scala code and **negligible on
  plain Java** — exactly the G3-2 shape ([Prokopec et al., CGO
  2019](http://aleksandar-prokopec.com/resources/docs/prio-inliner-final.pdf)).
  Elixir `Enum`/protocol pipelines are BEAM's Scala-shaped code.
- **OCaml Flambda** — the closest language analogue (cheap calls,
  immutable data): Jane Street reports inlining's value is scope
  growth for the simplifier, measuring **20–30 % allocation
  reduction and similar latency wins on real applications**
  ([Flambda launch](https://blog.janestreet.com/flambda/)) — the
  inlining→allocation-elimination coupling is pillar 2 feeding
  pillar 3.
- **Self/type-feedback lineage** (Hölzle & Ungar 1.5–3×; Dean &
  Chambers' inlining trials) established the
  profile-speculation + inlining pairing this plan inherits.

The elimination-scored policy of §2.1 is a lightweight
implementation of the inlining-trials idea: score by what the
optimizer will provably do with the fused region, not by call
counts or body size alone.

## 3. GVN/CSE, constant folding, DCE

Backward-chained CSE over the fused region (LuaJIT/Maglev shape —
~a week of work, not a research project), value-numbering the
BEAM-specific primitives: tag tests, `element/2` loads, untag/tag
pairs, guard BIFs (pure by manifest,
[`../T2/04_optimization.md`](../T2/04_optimization.md) §10.7),
map-shape checks. The invariant from the guard-BIF manifest holds
here: split fast/slow lowerings are atomic — DCE may not orphan a
slow tail. Redundant-load elimination across immutable terms is
free game (BEAM terms never mutate — no alias analysis needed for
loads from term memory; the only clobbers are GC moves, which are
sync points anyway). This immutability edge is real: ZJIT's
load-store forwarding needed alias reasoning Ruby objects make
hard; BEAM gets it structurally.

## 4. LICM

Hoists, in priority order: (1) invariant guards — parameter type
checks, map-shape guards, fun-identity guards — to the loop
preheader (one check per invocation instead of per iteration; this
is where the G-map guard amortizes); (2) invariant loads —
captured-fun environment, tuple elements of loop-invariant terms,
match-context base/end; (3) invariant pure computation, including
guard BIFs. Constraint: nothing hoists across a sync point unless
provably re-executable (the window rule); guard hoisting *into* a
preheader is exactly the MVP's validated entry-guard model.
Prior-art anchor: Graal's speculative guard motion measured +8–15 %
on loop-heavy Renaissance benchmarks (guards executed −83 % on
log-regression) — the same mechanism, and the one general-tier
optimization with a directly measured double-digit analog.

## 5. Unrolling + SWAR

Inherited from the binary expansion package
([`../T2/08_v1_loop_tier.md`](../T2/08_v1_loop_tier.md) §7) and
generalized: ×4 default, ×8 for byte-class bodies; wins are
`test_heap` coalescing (single worst-case reservation), bounds-check
coalescing with scalar epilogue, adjacent-load merging, and the
~5-recipe SWAR library for byte-class predicates (measured ~2× on
top of fused scanning). Reduction accounting batches per chunk
(`subs FCALLS, K`), observably equivalent. Limits stand: per-byte
loop-carried dependences stay scalar; UTF-8 doesn't unroll. IV
analysis stays deliberately weak (cons-walking has no induction
variable to strength-reduce; binaries use the match context as the
IV, registerized by loop recovery).

## 6. Escape analysis, allocation sinking, float unboxing

The pillar-3 pass group, attacking the measured GC pool (9.5–12.4 %
of busy everywhere; 387–1 750 MB/s garbage; survival 0.18–0.39).

### 6.1 Escape analysis on BEAM terms

Flow-based escape analysis over the fused region. BEAM semantics
make this unusually clean: terms are **immutable** (no field-write
escapes, no aliasing lattice), and the escape points are a short
closed list — message send, ETS/persistent_term/process-dict write,
exception payload, return/tail-call argument, closure capture,
passing to a non-inlined call, becoming GC-visible across a sync
point that can deopt. Everything else is region-local by
construction.

### 6.2 Two-stage sinking (matching the deopt ladder)

- **P5a — deopt-dead sinking** (no rematerialization): sink an
  allocation only if it is dead on *every* deopt/exit path from the
  region. The construct-then-deconstruct pairs the inliner scored
  (§2.1) are typically exactly this — the tuple exists only to be
  taken apart three instructions later in the fused region. Zero
  new deopt machinery; the framestate simply never mentions the
  value.
- **P5b — framestate virtuals** (rematerialization on deopt): the
  HotSpot/PEA + PyPy-virtuals model — framestates describe sunk
  allocations symbolically; the deopt stub materializes them before
  re-entering T1. Funded only if P5a's gate measurement shows a
  residual pool that justifies it. Partial-escape form (sink into
  the cold branch where the value escapes only there) is the
  known-best variant (Graal PEA: +10.4 % Scala-DaCapo average,
  allocated bytes −15–58 % on allocation-heavy code; +1–2 % on
  plain-Java-shaped code — honest bounds for what to expect).
  The dynamic-language precedents are stronger and explain *why*
  P5b exists: classic escape analysis fails in dynamic languages
  because values "escape" through rare exit paths — sinking into
  the exit snapshot fixes it. PyPy's allocation removal deleted
  **70 % of all allocation ops and ~95 % of guards, making every
  benchmark ≥20 % faster** ([Bolz et al., PEPM'11](https://stups.hhu-hosting.de/downloads/pdf/BoCuFiLePeRi2011.pdf));
  LuaJIT's allocation sinking took a point-arithmetic loop from
  26.9 s to 0.2 s — parity with C++
  ([LuaJIT wiki](https://web.archive.org/web/2016/http://wiki.luajit.org/Allocation-Sinking-Optimization)).
  BEAM's immutability makes the analysis *easier* than either.
  Pyrlang's counter-lesson: its meta-traced BEAM ran 25 % slower
  than HiPE, dominated by term allocation — allocation is where
  BEAM JITs live or die.

### 6.3 Float unboxing

Floats are the one BEAM number type that heap-allocates per value
(64-bit doubles boxed on the heap; T1 already register-backs eight
f-regs within `fclearerror` blocks). In fused loops with
float-typed loop-carried values (profile + Type chunk agree), keep
them in FP registers across iterations, box only at escapes —
LuaJIT/V8's classic win, shape-bound (numeric loops), sized in M0
by the corpus float census (flagged unpriced in the evidence
dossier).

### 6.4 Honest sizing

The Amdahl chain for pillar 3: (sinkable share of allocation
volume) × (GC share of cycles) + direct allocation-instruction
savings. With GC at ~12 % and Graal-calibrated volume reductions of
15–50 % on allocation-heavy code, the end-to-end yield is 2–6 % on
services — meaningful against the ≥10 % service target, not a
20 %-carrier alone. That is why it is one pillar of three, and why
M0 measures the sinkable share (idea-#50 lifetime instrumentation)
before P5 is funded.

## 7. What is deliberately absent

- **Trace compilation** — method-at-a-time won
  ([`../research/`](../research/) settled it; ZJIT's 2025-26
  experience reconfirms: 18 months in, the method-based optimizing
  tier still trails its template tier on macro-benchmarks — build
  cost calibration, not a reason to copy the architecture).
- **Cold-arm pruning / branch-driven dispatch specialization** —
  G3-1 null; branch weights serve layout only.
- **Bounds-check strength reduction beyond binaries** — no measured
  pool (tuple `element/2` is guarded by arity checks the Type chunk
  mostly removes already).
- **Auto-vectorization beyond the SWAR recipes** — the recipes
  cover the measured byte-class pool; general SLP is out of scope.
