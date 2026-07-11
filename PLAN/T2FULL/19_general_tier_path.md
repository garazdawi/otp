# 19 — The general-tier path: next step and staged plan (2026-07-12)

> **Question this memo answers.** The owner wants to explore pushing T2 from
> its landed specialist disposition ([`15`](15_scope_and_disposition.md))
> toward a **generally usable tier** — real wins on typical applications,
> eventually default-on. What is the next step, and the staged path after
> it? This memo applies the evidence base (memos 11–17) to the new goal; it
> changes no disposition by itself. S0 is funded; every later stage is
> gated on the numbers the stage before it produces.

## 0. TL;DR

- **Next step (S0): the addressable-share census of memo [`17`](17_more_opcodes_value_census.md) §3,
  on a corpus broadened per 17 §4 (a map/term-heavy service class was never
  tested), extended with a blocker-placement dimension** (is each blocking
  op inside or outside the hot loop?) that prices region compilation with
  the same scan. Days of work, zero codegen. This is not a judgment call:
  16 §7 option (C) and 17 §5 both make the census the precondition for
  funding any frontend broadening.
- After S0: prototypes (S1), gate rework (S2), the type-feedback profiler
  (S3), census-ordered frontend classes (S4), region compilation on the
  proven rung-2 machinery (S5), default-on productization (S6). Each stage
  carries an explicit kill criterion.
- Honest odds: ~15–25% that "real wins on typical applications" is
  reached; modal outcome is a moderately broadened specialist tier
  (maps + regions, one or two service niches at 5–10%). S0+S1 ≈ 3–6 weeks
  and zero codegen buys a confident go/no-go either way.

## 1. Why the census is THE next step

- 16 §7 (C): "Do not fund without running the opcode-eligibility census
  (§6) on real service `.beam` first."
- 17 §5 decision rule: build the service frontend only if **(a)** union
  addressable coverage {`call_fun`, maps, bs-construction, bs-position}
  ≥ 15% whole-workload own-time on ≥ 1 canonical service **and (b)** a
  realizable prototype shows work actually removed.
- 16 §3.2: the census "should have been the *first* measurement, not the
  last." `erts_t2_eligibility_scan` (`t2_eligible.c`) already walks every
  generic op; the eprof own-time joins exist from the memo-14 program.

Two cheap extensions to 17 §3's spec:

1. **Corpus** (17 §4): add at least one map/term-heavy workload
   (map-lookup router, ETS-record transform, term→map decoder). The
   memo-14 corpus is all byte-slinging — T2's worst case — and 17 §4 calls
   the map-heavy class "the one place I would not pre-judge."
2. **Blocker placement**: per function, record whether each blocking op
   sits inside the hot loop body or outside it (prologue/cold clause/error
   path). This turns "implement every opcode vs compile regions around the
   blockers" into a number, for free.

**Kill criterion for the whole push at S0:** union coverage < 15% on every
workload *including* the map-heavy one, and the out-of-loop-blocker share
also small ⇒ "0% on services" converts from argued (17 §1) to measured,
and the ambition moves to the VM-internal/GC track (16 §7 P3).

## 2. The staged path

**S0 — census + corpus (1–2 wk).** As §1. Output: per-class and per-union
addressable own-time shares, the entanglement matrix (17 §3), and the
in-loop/out-of-loop blocker split, per workload.

**S1 — realizable prototypes, memo-13 method (2–4 wk).** For each class
clearing the bar, hand-write the specialized version driven by real
captured arguments (13 §1) — e.g. a monomorphic-shape
`get_map_elements` as fixed-offset loads — and measure the hot function
isolated. Piggyback a diagnostic-only monomorphism sampler (no data
exists on real-site map-shape/`call_fun` monomorphism; the 02 profiler
was never built). Gate: ≥ 10% isolated with work demonstrably removed —
the guard against the twice-measured "eligible ≠ win" failure (lex_wl
+38% slower installed, 14 §4; dialyzer 3–6%, 13). Kill: parity-or-slower,
or hot sites polymorphic.

**S2 — cost-model gate rework + remaining P0 hardening (2–3 wk,
value-independent, do regardless).** The path-blind histogram signals are
a known soundness gap (15 "known residual"; 16 §3.3/§5); a general tier
needs per-shape cost modeling, not `bs≥1` histograms (15). The CI safety
net half of P0 landed 2026-07-11 (fuzzer leg, ARM matrix leg, T2 CT
suites); the parity/smoke suites are committed. Kill: none.

**S3 — production type-feedback profiler ([`02`](02_profiling.md); ~4–6 wk).**
Map-shape slots, monomorphic-target slots for `call_fun`
devirtualization/inlining (the P3 inliner's site-selection input, 03
§2.1); staged with its consumer. Gate: 02 §1's own ≤ 1% steady-state
budget on every tracked benchmark; S1's monomorphism confirmed online.
Kill: budget blown, or real sites saturate to POLY.

**S4 — frontend class build, census-ordered (~4–8 wk per class; the full
union is a bigger bet than rung-2 was).** Eligibility + isel + emit +
deopt for the winning class(es); maps first if S1's surprise
materialized (the only class with a work-removal story, 17 §2.3);
bs-construction/`call_fun` only if the entanglement matrix requires them
(17 §2.1). Gate: ≥ 10% whole-workload on the motivating service, floor
intact. Kill: installed hot functions don't clear the floor.

**S5 — region/partial-function compilation (2–3 wk CP lifecycle + 4–6 wk
framestates, per 12 §9).** Today one unsupported op condemns the whole
function (16 §6). Regions side-exit at the blocker and multiply the
addressable share of every already-supported class. Memo [`12`](12_rung2_derisk_spike.md)
**proved** the hazardous half (persistent T2 CPs, purge UAF closed,
tombstone + two-phase retire); memo [`13`](13_rung2_value_ceiling.md)'s
negative verdict priced a *different question* (elimination value on
irreducible analysis code, not addressable share) and does not transfer
automatically — but 12 §8 does: bare parity buys ≤ 0, so loop-carried
blockers kill a region. Gate: S0 shows ≥ 10% own-time in "hot loop fully
eligible, blockers out-of-loop" functions AND an S1-style prototype wins
on one. Kill: blockers are loop-carried (JSON's per-token `call_fun`
likely is).

**S6 — default-on productization (only after ≥ 1 measured whole-workload
win; ~3–6 months).** Retention memory at fleet scale (`ErtsT2RetainedCode`
copies per module; needs lazy/evictable retention), tier-up tax ≤ 1% on
every tracked benchmark (06 P6 floor), trace/reload storm behavior,
observability, and the x86_64 port (06 P7, 8–10 wk) — generally usable
cannot be aarch64-only. Kill: memory or tax budget blown at scale.

## 3. Measured vs argued — where this push bets against evidence

**Against measurements:** services ~0% with the current set (14 §0–3,
per-function census); *eligible ≠ faster*, twice (lex_wl +38%, dialyzer
3–6%); bare parity ≤ 0 (+9% spike regression, 12 §8); call-overhead
removal ≈ 0 (G3-2); sinkable-allocation and Enum-wrapper pools ≈ 0 (M0).

**On unmeasured territory (where any upside must live):** whether *adding*
opcode classes helps — 17 §1's "one honest soft spot"; the map/term-heavy
corpus (17 §4); real-site monomorphism (no profiler ever built);
region-compilation addressable share (never asked until S0 asks it).

## 4. Honest assessment

Long odds — every measured pool came up thin or zero, and the remaining
upside sits entirely in the corpus gap, behind an unbuilt profiler and a
gate rework. Probability-weighted: ~15–25% for "real wins on typical
applications" (≥ 10% whole-workload on more than one service class);
modal outcome a moderately broadened specialist tier; a clean measured no
is a real possibility and is itself a valuable outcome. The path's virtue
is its cost shape: S0+S1 ≈ 3–6 weeks, zero codegen, and memo 17 §5's own
two-condition rule then either funds a numbered build plan or redirects
the ambition — with a clear conscience — to the GC/VM-internal track
where 16 §7 P3 says the service cycles actually are.
