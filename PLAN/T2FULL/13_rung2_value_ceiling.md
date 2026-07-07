# Rung-2 VALUE ceiling — does the inliner+elimination prize pay for the build? (2026-07-07)

> **Question this memo answers:** the P3 rung-2 correctness risk is retired (the
> CP-on-stack de-risk spike returned **GO**, memo 12). Before committing the
> ~12–19 week rung-2 + inliner build, measure the realistic **CEILING** of what
> the inliner + T2 elimination could actually buy on the hottest real
> body-recursive pairs, and weight it to a total-workload projection. Does the
> value support the ~10–25% projection (memo 11), is it marginal, or does it
> fall short?
>
> Built off `lukas/erts/beamjit2` tip `0e6fc2c4f2`, isolated worktree
> (`otp-p3value`), `otp-beamjit2` untouched. Apple Silicon (aarch64/macOS).
> Measurement artifacts + raw numbers: `p3value_bench/` (RESULTS.md + modules).

## 0. TL;DR — the NUMBER and the verdict

**Source-level fusion of the hottest rejected pairs buys ~3.5–6% per pair
(measured, robust). T2 elimination on top is small (+0–6%, estimated) because
the eliminable work is negligible. Weighted by the prize's ~59%/40%
(dialyzer/compiler) coverage, the realistic total-workload speedup is ~3–6% on
dialyzer and ~2–4% on the compiler; optimistic ceiling ~6–8%.**

**VERDICT: the VALUE FALLS SHORT of / is MARGINAL against the ~10% bar the
12–19-week build is predicated on.** The 10–25% projection in memo 11 conflates
large *coverage* with large *per-unit speedup*. The covered code is
pointer-chasing tree/graph traversal, tag dispatch, user-closure calls, and hash
lookups — work whose cost is largely **irreducible**, and the near-opposite of
the arithmetic/binary loops where P2 elimination delivered 2–3×. Fusion removes
only the loop↔helper call barrier, which on this hardware is cheap (~3% even when
the call *is* the work — memo 12 §8). **The correctness risk was worth
retiring; the value does not support the flagship-priority framing.**

## 1. Method

Time is tier-independent for a *source-fusion* measurement: fusing two Erlang
functions and recompiling to T1 measures the algorithmic call-barrier win
directly, and is a robust **lower bound** on the rung-2 ceiling (it excludes T2
unboxing/guard-elim, which are added as an estimated range on top).

1. **Reconstruct the hot pair faithfully** and drive it with **real captured
   arguments** (not synthetic).
2. **Measurement A — source fusion (measured):** hand-write the fused/simplified
   version, prove it equivalent on the whole corpus, compile normally, measure
   vs the original with the P2 protocol (bench pinned to scheduler 1, best-of-N,
   default schedulers). Cross-checked on asdf OTP 29.0.
3. **Measurement B — T2 elimination delta (estimated range):** the stale
   checked-in `beam.smp` predates the `+JT2` flag and no inliner exists, so an
   end-to-end T2 measurement of the *fused+eliminated* code is not buildable
   without building the very thing under evaluation. B is therefore reasoned
   from the fused disasm (which ops are eliminable), memo 12's parity result,
   and the P2 elimination results. Reported as a range.
4. **Weight:** per-pair speedup × time-share → total.

## 2. The corpus (real, not synthetic)

100,000 `{Term,K}` args to `erl_types:t_limit/2` — the **sole** entry into the
`is_limited`/`are_all_limited` walk (verified: one caller) — reservoir-sampled
from a real `dialyzer --build_plt erts kernel stdlib`. `t_limit/2` was seen
**>6,000,000 times** (sampling cap), independently confirming the pair is hot.

Corpus shape — decision-relevant:
- **K (depth limit) ∈ {3,4,5}** only (86.8k at K=5). The walk is **shallow by
  construction** — `t_limit` exists precisely to cap type depth — so per-term
  work is bounded and there is little arithmetic to unbox.
- 99.3% of terms return `true` (a full traversal is the common case).
- ~16.0 `is_limited` + ~15.3 `are_all_limited` invocations per top-level term:
  moderate, branchy, shallow. ~6 ns of real dispatch work per `is_limited`
  invocation — so the call/return that fusion removes is a **small slice** of it.

## 3. Pair 1 — `are_all_limited/2` ↔ `is_limited/2` (10.8% of dialyzer)

### Measurement A — fusion (MEASURED)
`il_fused` inlines `is_limited` into `are_all_limited`'s per-element position,
turns the `andalso` into an explicit short-circuit, and lets sub-recursions
re-enter the fused loop (transitive). Equivalence: **0 mismatches / 100k terms**.
Disasm confirms the per-element `{call,2,{is_limited,2}}` is gone for the
tuple/union/product/tuple_set cases, replaced by inline `is_ne_exact any` /
`is_lt 0 K` / `is_tagged_tuple 4 c` / `select_val`.

| corpus            | fusion speedup |
|-------------------|----------------|
| full (100k)       | **~5.0–5.5%** (4.8/5.2/5.0/5.7/6.1) |
| fusible subset    | ~6.4% (top-level tuple/union/…) |
| scalar subset     | ~3.5% (fusion still helps via transitive descent) |
| asdf OTP 29.0     | ~3.0% (current compiler, cross-check) |

### Depth-arith specialization *hurts*
The method suggested "hoist invariants / specialize the depth arithmetic." A
variant that hoists the loop-invariant `K =< 0` out of the element loop
(`all_any/1` specialization) is **slower** than plain fusion (~2% vs ~5%): the
extra entry guard costs more than the per-element `is_lt` it removes.
**⇒ the K counter is not the bottleneck; "unbox K" buys ≈0.**

### Measurement B — elimination delta (ESTIMATE)
Eliminable ops per fused element, from the disasm, and their honest headroom:
- **unbox K** (`is_lt 0 K`, `gc_bif '-' K 1`): ≈0 — confirmed above; K arithmetic
  is already cheap and shallow.
- **tuple-tag guard removal** (`is_tagged_tuple 4 c`): **limited** — terms are a
  genuine `atom | #c{}` mix (`any`/`none`/`unit` are atoms; K decrements to atom
  leaves), so the guard is *real dispatch*, not a redundant re-check; speculation
  cannot prove "always `#c{}`."
- register/stack traffic: the fused body's `allocate`/`init_yregs`/`move y→x` is
  mostly **irreducible** — values live across the recursive calls that clobber
  caller-saved regs, so they must be saved.
- baseline: memo 12 §8 — bare T2 for this shape is **−9%..+3%** vs T1; elimination
  must first climb out of that hole.
- P2 evidence: elimination gave 2–3× **only** on arithmetic/binary loops;
  pointer-chasing tree walks are classified "moderate."

**⇒ B ≈ +0% to +6% net on top of fusion; most likely +1–3%.**
**Pair-1 realistic ceiling: ~6% likely, ~11% optimistic upper bound.**

## 4. Pair 2 — `cerl_trees:fold_1/3` (MEASURED) + pairs 3–4 (structural)

**Pair 2 (measured).** Corpus: 11 real Core Erlang trees (stdlib modules compiled
`to_core`; 225,204 nodes/pass), folded with `fun(_,S)->S+1 end` (the real
`cerl_trees:size/1` use). `ct_fused` splices `fold_1`'s dispatch into `fold`
(removes one call per node). Equivalence: 0 mismatches. **Fusion speedup ~3.5%**
(2.7/4.2/3.55) — *lower* than pair 1, because every node also does an
**irreducible `F(T,…)` call** (a runtime user closure the compiler cannot inline)
on top of the `type()` pointer-chasing. Fusion removes only the `fold_1` call.

**Pair 3 — `sets:is_element/2` + `add_element/2` (structural).** Default `sets`
is v2 = **map-based**: `is_element` is a single `#{E:=_}` match, `add_element` a
`S#{E=>…}` update. The "helper" is a primitive hash op — **nothing to inline**;
the cost is the irreducible lookup. Fusion ceiling = call-overhead only, a few %.

**Pair 4 — `beam_ssa:rpo_1/4` (structural).** Reverse-postorder successor
traversal + set bookkeeping — same pointer-chasing / moderate-low class.

**Generality: the ceiling is uniformly moderate-to-low.** No sampled pair wins
big, and there is a structural reason: the prize buckets are *by construction*
the non-numeric code (numeric/tail kernels are already loop-tier-eligible — the
`pure_loop` bucket), i.e. exactly the code with the least eliminable arithmetic.

## 5. Weighting → total-workload projection

Total speedup = coverage × per-covered-pair speedup. Coverage (memo 11, eprof
own-time): ~59% of dialyzer, ~40% of compiler.

| scenario | per-covered speedup | dialyzer total | compiler total |
|----------|---------------------|----------------|----------------|
| fusion only (measured)   | ~5%  | ~3.0% | ~2.0% |
| fusion + likely elim     | ~7%  | ~4.1% | ~2.8% |
| fusion + optimistic elim | ~11% | ~6.5% | ~4.4% |
| **needed to hit 10% total** | **~17% (dia) / ~25% (comp)** | 10% | 10% |

No measured pair approaches the ~17–25% per-covered speedup the 10–25% total
projection requires. Even under the charitable reading that "10–25%" meant the
*covered subset* (not total), the measured subset speedup is ~3.5–6% (fusion),
~≤11% only for the single best pair under the most optimistic, unmeasured
elimination — so it still sits at the very bottom edge.

## 6. VERDICT — FALLS SHORT / MARGINAL on value

Realistic total-workload speedup from the full rung-2 + inliner build:
**dialyzer ~3–6%, compiler ~2–4%; optimistic ceiling ~6–8%.** It does **not
robustly clear the ~10%** the 12–19-week investment is predicated on.

The single most decision-relevant finding: **fusion barely helps because the
real cost is irreducible** — pointer-chasing tree/graph walks, `select_val` tag
dispatch, user-closure calls (`cerl_trees`), and hash lookups (`sets`), with the
counter arithmetic negligible (and its "optimization" measurably *harmful*) and
the structural guards genuine `atom|record` dispatch (not redundant). Rung-2's
inliner removes the loop↔helper call barrier, and on Apple Silicon that barrier
is cheap (memo 12: ~3% even when the call is the entire body). The prize is a
large slice of *time*, but it is the *wrong kind of code* for T2 elimination.

### Honest uncertainty (what could move the number, and by how much)
- **B is estimated, not measured end-to-end.** A true measurement needs the
  inliner + T2 elimination — the artifact under evaluation. But every proxy
  (disasm op-headroom, the K-hoist regression, memo-12 parity, P2's arithmetic-
  only wins) points the same way: small. It is very unlikely B alone turns
  ~5% into ~17%.
- **Hardware.** These numbers are Apple Silicon, where calls/branches are cheap
  (memo 12 measured the demote "mispredict tax" at ~0 for monomorphic returns).
  On a machine with pricier indirect branches / deeper mispredict penalties the
  call-barrier fusion could be worth more — but the *irreducible* pointer-chasing
  and dispatch scale with it too, so the *ratio* is unlikely to double. Worth a
  one-off x86 spot-check before any reversal.
- **Polymorphic returns.** Memo 12 flagged that helper inlining puts *distinct*
  blob CPs on the stack (mispredict economics only bite there). If real dialyzer
  return sites are badly polymorphic in T1, demote-tax avoidance could add a few
  points — but the measured fusion already captures removing those calls
  entirely, so this is subsumed, not additive.

### Recommendation
- **Do not commit the 12–19-week rung-2 + inliner build on the current value
  case.** Re-scope P3 around wins with a measured payoff, or gather a
  higher-fidelity B (a minimal T2 elimination prototype on the fused
  `are_all_limited` — unbox K + speculate `#c{}`) *before*, not after, funding
  the inliner: if that prototype cannot beat ~10% on the single best pair, the
  weighted total cannot reach the bar.
- The **TMC / compiler-level** track (memo 11 §2) remains orthogonal and helps
  T1; it is unaffected by this finding.
