# M0.1 — The elimination-rich boundary pool

> Measurement M0.1 of [`PLAN/T2FULL/06_phases.md`](../T2FULL/06_phases.md):
> a static eliminability classifier over hot BEAM code, weighted by the
> cycle profiles of [`PROFILE_RESULTS.md`](PROFILE_RESULTS.md), sizing the
> pool that funds phase **P3** (elimination-scored cross-module inlining,
> [`03_optimizer.md`](../T2FULL/03_optimizer.md) §2). The G3-2 experiment
> proved call-overhead inlining is worth ~0; this measures how much hot
> code has *eliminable* boundaries — the open question G3_OUTCOME flagged.
>
> Tools: `elimscan.erl` (classifier), `elimdrv.erl` (weighting/aggregation),
> `run_scan.escript` (corpus driver). Reproduce: `escript run_scan.escript`
> then `erl -pa . -eval 'elimdrv:report()'`. Raw output in `report.txt`.

## Headline

**The elimination-rich cross-module boundary pool is thin.** Across every
leg, the high-confidence Pillar-2 eliminable share of *hot call sites* is
**7–13 %**, and at genuine cross-module (remote) boundaries **10–17 %**,
with **80–90 % of remote sites classifying `none`** — the G3-2 shape.

- **Dialyzer (compute application).** The two profiled-hot families that
  own ~21 % of busy CPU are **~7 % cycle-weighted core-eliminable**
  (Family A `is_limited`/`are_all_limited`/`t_has_var*`, 12 % busy →
  **10.5 %** of its sites core-eliminable, 7.9 % high-confidence;
  Family B `oc_mark`+comprehensions, 9 % busy → **2.9 %**, dominant `none`
  at 79 %). This **quantitatively confirms the G3-2 0 ± 1 % null**: there
  is almost nothing to eliminate across those boundaries.
- **stdlib `json` (compute kernel).** Core-eliminable **6.5 %**. Its many
  literal arguments (37 % of sites, *liberal* count) fold **~0 %** of
  callee branches — they are loop-carried positions/state, not dispatch
  keys. json's win is **Pillar-1 fusion (G-bin), not P3 elimination** —
  consistent with the profile (`Jason.Decoder` = the G-bin shape).
- **Whole-corpus census (all stdlib+kernel, 24 770 fns, 80 656 sites).**
  Core-eliminable **10–12 %**, dominated by `none` (76–79 %); at
  cross-module boundaries **13–14 %** core, **85 % `none`**.

**Reading for the G-M0 gate:** the pool does **not** clear the bar for P3
general inlining to be a **≥10 % standalone carrier on compute
applications**. It *does* support funding P3 in its narrow, high-confidence
form — literal-fun collapse (2–11 % of sites, the plan's highest-scoring
category) plus guard-subsumption at profiled-monomorphic remote sites — as
the **enabler** for Pillars 1 & 3, exactly the plan's three-pillar thesis
and its G5 fallback ("narrow to intrinsics + literal-fun sites"). See §7.

---

## 1. What is measured, and on what representation

For every **call site** in every corpus function, the classifier records
which categories of [`03_optimizer.md`](../T2FULL/03_optimizer.md) §2.1
apply (a site may be in several):

| category | fires when | confidence |
|---|---|---|
| **guard-subsumable** | callee begins with type tests on parameter *i*, and the caller *provably* passes a subtype at site (known-true only) | precise |
| **constant-args (effective)** | a literal (non-fun) argument lands on a parameter the callee **dispatches on** (folds a callee branch) | precise |
| — constant-args (liberal) | *any* literal argument (upper bound; most do **not** fold work) | upper bound |
| **literal-fun** | a `make_fun`/fun-literal argument (collapses `call_fun` → direct body) | precise |
| **construct/deconstruct** | callee's every return is a freshly-built tuple/cons the caller only destructures, **or** an arg is locally built and the callee only destructures it | precise |
| **loop-unlock** | callee is self-tail-recursive **and** the caller is itself recursive (site sits in a loop) | Pillar-1 enabler; see §3 |
| **none** | resolvable callee, no category (the G3-2 shape) | — |
| *opaque* | dynamic remote / `call_fun` / `apply` / callee outside corpus | counted separately |

Derived pools reported per group:
- **PRECISE** = guard ∪ literal-fun ∪ construct/deconstruct — the
  high-confidence floor (excludes constant-args entirely).
- **CORE** = PRECISE ∪ constant-args(effective) — the Pillar-2 pool.
- **ANY** = CORE ∪ loop-unlock.
- **remote-only mirror** (`r-*`) of every metric — the true P3 pool, since
  intra-module subsumption is already harvested (see §2).

**Representation — optimized SSA.** Each module is compiled independently
to *post-`beam_ssa_opt`* SSA (source → `beam_core_to_ssa` → `beam_ssa_opt`).
This is deliberately the representation the loaded-BEAM **Type chunk
serializes** — it carries `arg_types`/`result_type`/`func_info` annotations
and, critically, reflects the AOT compiler's whole-module optimization.
It is a faithful proxy for what T2 reconstructs from loaded BEAM.

**Why this matters — intra-module subsumption is already gone.** A local
callee whose callers all pass (say) lists has its `is_list` entry guard
*stripped* by `beam_ssa_opt`'s inter-procedural type propagation before it
reaches loaded code (verified directly: a two-clause local `f/1` called
only with lists optimizes down to the list body, no guard). So the
residual guard-subsumption pool the classifier finds lives almost entirely
at **cross-module (remote)** boundaries, where the callee is compiled
without caller knowledge and its guards survive. This is the plan's thesis
("the Type chunk already eliminated the intra-module tax; the residual
clusters at module/fun boundaries") turned into a measurement — and it is
why the `r-*` cross-module numbers are the ones that size P3.

## 2. Corpus and weighting

- **Corpus (resolution set):** all of `lib/dialyzer/src`,
  `lib/compiler/src`, `lib/stdlib/src`, `lib/kernel/src`,
  `erts/preloaded/src` — **306 of 311 modules compile** (5 skipped:
  `beam_asm`, `dialyzer_cl_parse`, `dialyzer_cplt`, `dialyzer_iplt`,
  `typer` — missing generated includes; none are hot). **24 770
  functions, 80 656 resolvable call sites, 1 706 opaque (97.9 %
  resolution).** Callee resolution crosses module boundaries within this
  set — that is the point (cross-module inlining). A remote call to a
  module outside the set is opaque, never eliminable (conservative).
- **Dialyzer leg (cycle-weighted).** From
  [`PROFILE_RESULTS.md`](PROFILE_RESULTS.md): Family A =
  {`is_limited/2`, `are_all_limited/2`, `t_has_var/1`, `t_has_var_list/1`}
  weighted **12 % of busy**; Family B = {`oc_mark/3` + its 7 list-
  comprehension helper functions} weighted **9 %**. Both families live in
  `erl_types`. Where the profile gives a family-level number, the family's
  eliminable fraction is the **aggregate over its member functions' sites**
  (i.e. size-proportional distribution within the family) — *stated as an
  approximation*: the profile does not resolve cycles below the family, so
  the ≤2.5 %/function tail (`dialyzer_*`, `cerl`, `lists`, `maps`) has **no
  per-function cycle weight** and is reported **structurally, labelled
  not-pool**, never folded into the cycle-weighted headline.
- **json leg.** `lib/stdlib/src/json.erl`, one hot module end-to-end,
  weighted uniformly; the module-aggregate site distribution ≈ its cycle
  distribution. Per-function-equal weighting reported as a cross-check.
- **Unweighted census.** All `lib/stdlib/src` + `lib/kernel/src`,
  % of call sites per category — structural shape beyond the weighted legs.

## 3. Approximations (every one, per the honesty rule)

1. **Cycle-weight where possible, else labelled structural.** Only the two
   dialyzer families and (uniformly) json carry cycle weight. Module-level
   and census numbers are **structural (call-site %), not pool** — they say
   what fraction of *sites* look eliminable, which is an **upper bound on
   the cycle fraction** (call sites are not cycles; G3-2 proved instruction
   removal ≠ time). Every such number is marked.
2. **Conservative caller-side types.** A site is guard-subsumable only on a
   **known-true** subsumption (caller type ⊑ callee-tested type). Caller
   types come only from *sound* sources: a variable's defining op (holds
   everywhere in SSA), dominating type-test guards (sound reverse-postorder
   dataflow with back-edge widening — loop headers drop to `any`), and
   `result_type` annotations on call results. Dead-clause elimination
   (caller type *disjoint* from tested type) is detected but **excluded**
   from the headline.
3. **This is a STATIC lower bound on guard-subsumption.** T2's inliner runs
   *after* profile-guided type speculation, which can prove monomorphic
   argument types the static view leaves `any`. The true P3-addressable
   guard pool is **≥** the measured `r-guard` (9–16 %). Sizing the increment
   needs runtime profiles (M0.3's job), not static analysis — this works in
   the plan's favour and is not credited here.
4. **constant-args is bracketed, not point-estimated.** *Liberal* (any
   literal) over-counts; *effective* (literal on a dispatched parameter)
   under-counts because it misses *folded arithmetic* (a literal consumed by
   `+`/`-` rather than a branch). Truth is between; CORE uses the effective
   (conservative) count and both bounds are shown.
5. **loop-unlock is a Pillar-1 enabler, not a Pillar-2 elimination, and is
   optimistic here.** It requires callee self-tail-recursion + a recursive
   caller, but (a) the self-tail proxy over-counts *tree*-recursive walkers
   (dialyzer's type traversals) that loop-recovery does not cleanly own, and
   (b) **G3-2 tested exactly the family where loop-unlock dominates (Family
   A, 81.6 %) and measured 0 ± 1 %.** It is reported but kept *out* of CORE
   and flagged wherever it dominates ANY.
6. **construct/deconstruct is conservative:** requires the callee's *every*
   return to be a direct `put_tuple`/`put_list` (phi-of-constructed ok) and
   the caller to use the result via destructuring/`match_fail` only. The
   dynamic allocation-sinking pool is **M0.2's** job (this counts only the
   statically-visible-both-ends shape).
7. **Exception-raise trampolines excluded.** Compiler-generated
   `erlang:error/exit/throw/nif_error` clause-failure calls are neither a
   site nor opaque — they are not inlining candidates.

## 4. Dialyzer leg — cycle-weighted (the spine)

Metric = % of the family's **resolvable** call sites.

| family (busy weight) | sites | guard | const-eff | lit-fun | c/d | loop | **PRECISE** | **CORE** | none |
|---|---|---|---|---|---|---|---|---|---|
| **A** `is_limited`+`are_all_limited`+`t_has_var*` (**12 %**) | 38 | 7.9 | 2.6 | 5.3 | 0 | 81.6 | **7.9** | **10.5** | 10.5 |
| **B** `oc_mark`+comprehensions (**9 %**) | 34 | 0 | 2.9 | 0 | 0 | 20.6 | **0** | **2.9** | 79.4 |

**Cycle-weighted over the 21 % named hot core: CORE-eliminable = 7.3 %**
(ANY, incl. loop-unlock = 60 %, but loop-unlock is the exact G3-2-null
family — read as ~7 %). Dominant Pillar-2 category in Family A is
guard-subsumption (3 sites), then literal-fun (2); Family B is 79 % `none`
with a single effective-constant site.

> **Headline (dialyzer):** *~7 % of the profiled-hot dialyzer call sites
> (cycle-weighted over the 21 % that the profile names) are Pillar-2
> core-eliminable; the hotter half (Family B, 9 % of busy) is 79 % `none`.
> This is the G3-2 shape, now quantified.*

**Structural support (not cycle-weighted):**

| group | sites | guard | const-eff | lit-fun | c/d | loop | PRECISE | CORE | none | **remote CORE / none** |
|---|---|---|---|---|---|---|---|---|---|---|
| `erl_types` whole module | 1 691 | 7.5 | 3.1 | 5.4 | 0.4 | 24.7 | 11.3 | 13.0 | 66.5 | **16.3 / 82.8** |
| `dialyzer/src` whole (≤2.5 %/fn TAIL) | 10 826 | 7.0 | 3.6 | 4.1 | 0.6 | 12.0 | 9.7 | 10.5 | 78.9 | **9.8 / 89.6** |

47.5 % of dialyzer's calls are cross-module; of those, **~90 % are `none`**
and only ~10 % core-eliminable — the residual boundary tax P3 could reach is
small and mostly literal-fun + guard.

## 5. stdlib `json` leg (compute kernel)

| group | sites (opaque) | guard | const-**lib** | const-**eff** | lit-fun | c/d | loop | PRECISE | CORE | none |
|---|---|---|---|---|---|---|---|---|---|---|
| `json.erl` | 261 (45) | 6.1 | **37.2** | **0.4** | 3.1 | 0 | 21.1 | 6.5 | **6.5** | 72.4 |
| — remote only | 47 | 14.9 | — | 2.1 | 10.6 | 0 | 0 | 17.0 | 17.0 | 83.0 |

> **Headline (json):** *json.erl is 6.5 % core-eliminable. The 37 %→0.4 %
> collapse from liberal to effective constant-args is the finding: json's
> literals are loop-carried scan positions, not branch keys, so P3
> elimination is not its lever. Its measured win (`Jason.Decoder` scan
> loops, ~7 % of the Bandit profile) is **Pillar-1 fusion**. At cross-module
> boundaries the live P3 sub-pools are guard-subsumption (14.9 %) and
> literal-fun (10.6 %).*

## 6. Unweighted census — structural shape (all stdlib + kernel)

% of resolvable call sites. `const` shown as liberal / effective.

| module set | sites (opaque) | guard | const lib/eff | lit-fun | c/d | loop | **PRECISE** | **CORE** | none |
|---|---|---|---|---|---|---|---|---|---|
| `stdlib/src` (98 mods) | 31 739 (778) | 6.9 | 28.5 / 3.4 | 2.5 | 1.0 | 16.0 | **8.2** | **10.1** | 76.0 |
| `kernel/src` (103 mods) | 15 529 (369) | 8.7 | 31.9 / 5.8 | 1.9 | 0.6 | 11.2 | **9.3** | **11.8** | 78.7 |

Cross-module (remote) sub-view — the P3-relevant slice:

| module set | remote sites (% of resolvable) | r-guard | r-const-eff | r-lit-fun | r-c/d | **r-CORE** | **r-none** |
|---|---|---|---|---|---|---|---|
| `stdlib/src` | 9 163 (28.9 %) | 12.2 | 3.7 | 5.5 | 0.6 | **13.4** | **85.2** |
| `kernel/src` | 6 909 (44.5 %) | 12.2 | 6.7 | 3.4 | 0.4 | **13.9** | **85.3** |

Structural shape beyond the weighted legs: **~10–12 % of all call sites are
core-eliminable, ~13–14 % at cross-module boundaries, ~85 % `none`.**
Dominant *precise* category corpus-wide is **guard-subsumption** (6.9–8.7 %,
and 12.2 % at remote boundaries), then literal-fun.

## 7. Reading for P3 — does the pool clear the G-M0 bar?

The G5 gate ([`06_phases.md`](../T2FULL/06_phases.md) P3) requires **≥10 %
end-to-end on targeted hot functions attributable to inlining-enabled
elimination**, and G-M0 asks whether Pillars 1–3 can *arithmetically* reach
the per-class targets before P3 is funded.

**What the data says:**

1. **P3 elimination-inlining is NOT a ≥10 % standalone carrier on compute
   applications.** Dialyzer's profiled-hot core is ~7 % cycle-weighted
   core-eliminable *call sites*; cycles-eliminated is a fraction of that
   (G3-2: instructions removed, 0 ± 1 % time). 80–90 % of cross-module
   sites are `none`. This is a **"scope, not kill"** result exactly as the
   gate anticipates: it keeps dialyzer-class in the plan's **honest 5–15 %
   band**, reachable only if Pillars 1 (fusion) and 3 (allocation) compound,
   with P3 as connective tissue — **not** as a pillar in its own right.
   This is precisely the plan's own thesis (§3: "Cross-module inlining is
   not a pillar by itself; it is the enabler all three pillars share"), now
   evidenced rather than asserted.

2. **The compute-kernel win does not come from P3 either.** json's P3 pool
   is 6.5 %; its win is Pillar-1 fusion. So the ≥20 % compute-kernel target
   (G6) rests on P4 fusion, and M0.1 confirms P3 should not be scoped to
   deliver it.

3. **The one P3 sub-pool with clear, precise positive signal is
   literal-fun** (2–11 % of sites; **higher at cross-module boundaries**,
   where Elixir `Enum`/`lists` pipelines live: r-lit-fun 3.4–11.3 %). This
   is the plan's *highest-scoring* category and its explicit G5 fallback
   ("the inliner narrows to its measured-positive subset: intrinsics +
   literal-fun sites"). **The data supports funding that subset.**

4. **Guard-subsumption at cross-module boundaries is real but modest
   statically (r-guard 9–16 %) and is a lower bound** — profile-guided type
   speculation (which T2 adds and this static pass cannot model) widens it.
   Sizing that increment is a runtime-profile follow-up (M0.3-shaped), and
   is the single largest source of upside not credited here.

**Verdict.** M0.1 prices the elimination-rich boundary pool at **single-digit
to low-teens % of hot call sites, 80–90 % `none` cross-module** — thin, and
thinnest exactly where dialyzer is hottest (confirming G3-2). It **funds P3
in its narrow enabler form** (literal-fun collapse + monomorphic-site
guard-subsumption) and **withholds funding P3 as a standalone ≥10 % lever on
compute applications**. The compute-application 20 % target must be carried
by Pillars 1 and 3 compounding — take this finding to the project owner at
G-M0 before P3+ is funded, with the loop tier (P0–P2) unaffected.
