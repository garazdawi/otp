# T2-Full — Profiling in T1

> What T1 records so the optimizer has something to speculate on.
> This un-shelves the **full** profiling design of
> [`../T2/02_profiling.md`](../T2/02_profiling.md) (the loop-tier
> plan had cut it to entry-only); mechanism details live there and
> are cited, not restated. Everything here is staged with its
> consumer: no profile site is emitted before the phase that reads
> it ships.

## 1. Design constants (inherited)

- **Eligibility-gated**: only functions with a compilable region get
  counters/slots; ineligible functions carry zero overhead
  ([`../T2/02_profiling.md`](../T2/02_profiling.md) §7.1).
- **Cost budget: ≤1 % steady-state on every tracked benchmark**,
  measured at each phase gate. Sites are ~2 ns each,
  per-scheduler-sharded, scheduler-1-only writes; the JIT manager
  reads unions at tier-up ([`../T2/02_profiling.md`](../T2/02_profiling.md)
  §§7.2–7.3). The HiPErJiT precedent — profiling overhead killing
  the tier's own win — is the named risk this budget exists against
  ([`../T2/07_delivery.md`](../T2/07_delivery.md) App C L5).
- **Profile narrows, never contradicts**: the AOT `Type` chunk is
  ground truth; feedback refines it
  ([`../T2/07_delivery.md`](../T2/07_delivery.md) App C H7).

## 2. The site inventory, by phase that consumes it

| site kind | records | consumer (phase) | spec |
|---|---|---|---|
| call counter | 32-bit, tier-up trigger | P2 | [`02`](../T2/02_profiling.md) §7.4 |
| entry type slots | per-arg `{seen_types, count}` bitmask | P2 speculation | §7.2 |
| arithmetic / call-return / switch type slots | interior type feedback | P2/P3 re-inference | §7.2 |
| **monomorphic-target slots + frequency** | last-seen callee (`Export*`/fun ptr/MFA), CAS-to-`POLY`, count | **P3 inliner** — site selection, inline priority, tier-up target selection (compile the dominant caller) | §7.5 + App C M1/M5 |
| map-shape slots | hash-consed keys-tuple ptr, `POLY` on second shape | P4 shape-guarded regions | §7.6 |
| branch counters | 16-bit taken/not-taken | P3+ block layout, cold-path placement, inline scoring input — **no win claims** (G3-1) | §7.7 |
| allocation-site lifetime sampling | see §3 below | **M0 sizing + P5 escape analysis validation** | new |

The v1-loop-tier cut (entry-only + flag-checked overflow) remains
exactly what P2 ships; the interior rows land with P3's compile
pipeline, so the loop tier's ≤1 % budget is not taxed for consumers
that don't exist yet.

## 3. New: allocation-site lifetime evidence (for pillar 3)

Escape analysis is compile-time static; it needs no runtime profile
to be *correct*. What it needs profiling for is **sizing and
validation**: which allocation sites produce short-lived garbage
worth sinking, and did sinking actually reduce allocation volume.

- **M0 (sizing, offline)**: the term-lifetime instrumentation
  designed in otp-ideas #50 (per-allocation-site lifetime
  histograms via a shadow-heap side array; `gc_gen`-based
  timestamps) is the tool. It runs on instrumented builds against
  the app corpus, answering: what fraction of allocation volume is
  (a) same-minor-GC-dead and (b) attributable to sites inside
  T2-eligible hot code? That number funds or kills P5 —
  before any escape-analysis code is written.
- **P5 (validation, online)**: per-blob allocation counters
  (words allocated in T2 code vs the T1 baseline for the same
  MFAs) reported through `erlang:t2_stats/0`, plus the existing
  GC msacc share as the end-to-end check.

No always-on per-allocation runtime profiling ships in production
builds; the ≤1 % budget is not spent here.

## 4. Profile lifecycle

Unchanged from the reference design: saturating counts, stability
check before compile (≥75 % saturation + stable bitmasks,
[`../T2/05_runtime.md`](../T2/05_runtime.md) §15.2 with App C I5
calibration), reset on jettison so recompiles see post-deopt
reality, per-site failure memory (`{fail_count, last_seen_mask}`)
feeding the widen-or-drop recompile policy
([`../T2/03_compilation_and_speculation.md`](../T2/03_compilation_and_speculation.md)
§9.5). Stable site IDs hash `{BEAM PC, kind, narrowed_type}` (App C
H6); per-call-site inline-skip keys `{caller_PC, callee_MFA}` (App C
H10).
