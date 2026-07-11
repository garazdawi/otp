# 20 — S0 census results: the addressable-share numbers (2026-07-12)

> **What this memo reports.** The [`19`](19_general_tier_path.md) §2 **S0**
> census — memo [`17`](17_more_opcodes_value_census.md) §3's frontend-only
> addressable-share measurement, on the corpus broadened per 17 §4 (a
> map/term-heavy service was never tested) and extended with 19's
> blocker-placement dimension. It replaces memo 14's *argument* ("adding
> opcodes wouldn't help either") with a *number*, per class and for the
> union, on three workloads. It changes no disposition; it feeds the
> memo-17 §5 decision rule that gates S1.

## 0. TL;DR

- **The instrument shipped** (`9e393d8865`): `erts_debug:get_internal_state(
  {t2_census, BeamBin})` runs the real eligibility oracle over any `.beam`,
  bucketing every function's blockers into the nine classes and splitting
  each in-loop vs out-of-loop. Validated against memo 14 (json 17/92
  eligible vs 16 measured; blocker classes match the 17 §2.1 hand table).
- **Union addressable coverage clears 15% on all three workloads**, so the
  S0 **kill criterion does not fire** — "0% on services" does *not* convert
  to measured. Whole-workload own-time addressable by the union
  {`call_fun`, maps, bs-construction, bs-position}:

  | workload | eligible now | **union coverage** | dominant classes |
  |---|---|---|---|
  | json (byte-slinging service) | 0.0% | **59.0%** | bs-position 66% gross, call_fun 54% gross |
  | mapwl (map/term-heavy service) | 19.7% | **19.6%** | maps only |
  | compiler (analysis) | 43.1% | **30.7%** | call_fun 26%, maps 22% gross |

- **But the census measures the *addressable ceiling* (assume every
  eligible function then wins), and three findings temper it hard:**
  1. **The map "surprise" is muted.** mapwl's opcode-addressable map work
     is only **19.6%**; **55% of its own-time is in the `maps` *library*
     (fold/merge/iterators)** — work the `get_map_elements`/`put_map_assoc`
     *opcodes* do not touch. That prize needs an inliner (S3/S4), a far
     bigger build than an opcode class.
  2. **Region compilation (S5) is not a shortcut.** Out-of-loop
     union-blocked own-time is **14.3% (json) / 0.0% (mapwl) / 0.5%
     (compiler)** — blockers are overwhelmingly loop-carried or in
     non-loop functions, so side-exiting around them buys almost nothing.
  3. **Addressable ≠ realizable, still.** The high ceilings sit behind
     memo 17 §2.2's twice-measured "eligible ≠ win" (lex_wl +38% slower,
     dialyzer 3–6%). json's ceiling is bs-matching + call_fun — the classes
     14 §4 already found eligible-but-slower.
- **Verdict: advance to S1, with the map opcodes as the *only* prototype
  worth building** (17 §2.3: the sole class with a work-removal story) —
  and a low prior. S0 did its job: it kept the door open on a number, and
  named exactly one experiment that can still close it cheaply.

## 1. The instrument

Frontend-only, measurement-only, reuses the real oracle so its `eligible`
column is bit-identical to `erts_t2_eligibility_scan`:

- **`erts_t2_census_scan`** (`t2_eligible.c`) mirrors the eligibility walk
  but records the whole *set* of blocking classes per function and, via a
  linear basic-block terminator heuristic, splits each blocker in-loop
  (on the self-recursion path) vs out-of-loop (base case / cold clause /
  error exit). The nine classes: `call_fun`, `maps`, `bs_construction`,
  `bs_position`, `exceptions`, `recv`, `float_reg`, `general_bif`, `other`.
- **`{t2_census, BeamBin}`** parses a throwaway `BeamFile` from a raw
  `.beam` (`code:get_object_code/1`) and returns
  `[{Name, Arity, Size, Eligible, LoopShaped, [{Class,Total,In,Out}]}]`.
  No compile, no retention, works for any module.
- **The driver** (`PLAN/T2FULL/census/`): `call_time` own-time tracing
  (own-time is memo 14's metric, captured programmatically) joined to the
  census per `{M,F,A}`. Reproduce with
  `erlc -o /tmp PLAN/T2FULL/census/*.erl && cerl -jit -noshell -pa /tmp
  -eval 'census_run:all(),halt().'`; captured output in
  [`census/RESULTS.txt`](census/RESULTS.txt).

**Denominator.** Two framings, both reported. *Module-local* = own-time of
the T2-candidate module only (what fraction of *its* time is addressable).
*Whole-workload* = own-time over the candidate module **plus the
Erlang-level library modules it drives** (maps/lists/binary/…), so
library-call time dilutes coverage to whole-workload terms. The
BIF-stub `erlang` module is excluded: `call_time` misattributes exception
unwind to `erlang:raise/3` (a spurious 33% of the compiler denominator)
and picks up the harness's own `trace_info`. C-BIF time is out of the
denominator either way — a known, stated limit.

## 2. Per-workload findings

### 2.1 json — the canonical byte-slinging service

Reproduces memo 14: **0% eligible own-time**, hot path entirely in
refused functions. Union ceiling **59.0%** (whole-workload; module-local
59.5%), the highest of the three — but it is *bit-syntax matching plus
higher-order dispatch*, not the arithmetic loops T2 removes work from:

- `bs-position` gross **66%**, marginal **31%** (the matching/position
  family beyond the byte-aligned scan subset);
- `call_fun` gross **54%**, marginal **18%** (encoder dispatch);
- `general_bif` gross **25%** — **not in the union**, and it entangles the
  single hottest true function: `json:string/7` (12.9%) needs
  `call_fun` **and** `bs_construction` **and** `bs_position` **and**
  `general_bif` at once (reproducing 17 §2.1 over the full module).

So the 59% ceiling is real but doubly discounted: it is exactly the
eligible-but-slower class (14 §4 lex_wl), and its top function needs a
class (`general_bif`) that is not a buildable unit.

### 2.2 mapwl — the map/term-heavy service (the 17 §4 gap)

The class 17 §4 "would not pre-judge." The census pre-judges it, and the
result is a **muted** surprise:

- **Opcode-addressable map work = 19.6%** — `get_map_elements` (map
  matching in heads, `route/2`) + `put_map_assoc` (the compiler lowers
  `maps:put/3`, `decode_pairs/2`) + friends. Clean: `maps` marginal 19.6%
  ≈ gross 25.0% (little entanglement), and it is the one class with a
  work-removal story (monomorphic shape → fixed-offset load, 17 §2.3).
- **But 55% of own-time is in the `maps` *library*** — `maps:fold_1/4`
  (26.7%), `maps:try_next/2` iterators (8.1%), `maps:fold/3`,
  `maps:merge/2`. These are `call_ext` calls, *not* map opcodes; adding
  `get_map_elements`/`put_map_assoc` does nothing for them. Capturing them
  needs cross-module inlining of the maps library (S3/S4), the "much
  bigger bet" (14 §6).

The module-local framing (52.9%) *overstates* by hiding this: it counts
`decode_pairs`'s recursion overhead as addressable while its `maps:put`
payload runs in the library. Whole-workload is the honest number.

### 2.3 compiler — the analysis class

**43% eligible already**; union ceiling **30.7%**, from `call_fun`
(marginal 18%, the higher-order list drivers `lists:foldl_1`,
`mapfoldl_1`) and `maps` (marginal 12%, because `sets` is now map-backed —
`sets:is_element/2`, `add_element/2`). This is the same territory memo 13
measured at **3–6% realized** on irreducible analysis code: addressable
30%, realizable single digits — the "eligible ≠ win" gap, in one workload.

## 3. Applying the decision rule (17 §5)

**Condition (a) — union coverage materially above the specialist floor,
proposed ≥ 15% whole-workload on ≥ 1 canonical service:** **MET**, on all
three (59.0% / 19.6% / 30.7%). The S0 kill criterion (union < 15% on
*every* workload *including* the map-heavy one, and out-of-loop share also
small) is **not** triggered on the coverage axis.

**Condition (b) — a realizable prototype shows work actually removed:**
**not yet measured** — that is S1, and it is now the whole ballgame. The
census has bounded the *ceiling*; every discount above (eligible-slower
twice, the 55% library residue, region ≈ 0) says the *realizable* number
lives far below it.

**The blocker-placement (region) result stands on its own:** out-of-loop
union-blocked own-time is 14.3% / 0.0% / 0.5%. Even json — the only
workload with meaningful out-of-loop share — is majority loop-carried
(31.6% in-loop). **S5 region compilation is priced and it is cheap value**;
12 §8's "bare parity buys ≤ 0, loop-carried blockers kill a region"
applies directly. Deprioritize S5.

## 4. Recommendation

- **Proceed to S1, scoped to one experiment.** Hand-write the memo-13-method
  prototype for **maps only** — a monomorphic-shape `get_map_elements` as
  fixed-offset loads, driven by captured real arguments, plus the piggyback
  monomorphism sampler (no map-shape data exists; the 02 profiler was never
  built, 17 §2.3). Gate: ≥ 10% isolated with work demonstrably removed.
  This is the sole class clearing (a) that also has a work-removal story
  *and* clean (unentangled) addressable share.
- **Do not prototype json/bs.** Its ceiling is the highest but it is the
  twice-measured eligible-but-slower class; building it to re-measure a
  known negative inverts the M0 discipline.
- **Deprioritize S5 (regions).** Priced at ≈ 0 out-of-loop addressable.
- **Honest prior, updated.** S0 came back *above* the kill line, which is
  more than memo 14's argument predicted — but the map prize is mostly
  library-call time behind an inliner, the compiler prize is the
  eligible-but-irreducible gap, and json is eligible-but-slower. The modal
  outcome remains a moderately broadened specialist tier (maps, one niche);
  a clean measured no at S1 is still live. S1 ≈ 2–4 weeks, zero backend,
  decides it.

## 5. Artifacts

- Instrument: `jit/t2/t2_eligible.c` (`erts_t2_census_scan`,
  `erts_t2_blocker_class`), `t2_debug.cpp` (`erts_t2_debug_census`),
  `erl_bif_info.c` (`{t2_census, Bin}`). Commit `9e393d8865`.
- Corpus + driver + captured output: [`census/`](census/) —
  `mapwl.erl`, `t2_census.erl`, `census_run.erl`, `RESULTS.txt`.
