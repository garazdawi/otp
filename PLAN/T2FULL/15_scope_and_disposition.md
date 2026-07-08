# T2-Full — Scope & Disposition (2026-07-08)

> **Read this first for the conclusion.** The rest of `PLAN/T2FULL/` is the
> plan-of-record and the mechanism specs; this memo is what the measurement
> program *concluded* T2-Full actually is. It supersedes the "≥20 % on most
> applications" mandate in [`README.md`](README.md) / [`00_goal_and_thesis.md`](00_goal_and_thesis.md).

## Disposition (owner decision, 2026-07-08)

**T2-Full ships as a SPECIALIST optimizing second tier — not a general one.**
The "20 % on most applications" thesis is **measured-dead** (evidence below), and
the phase that was supposed to carry it (P3 / rung-2 general inlining) is
**decided-against**. What remains is a genuinely valuable, correct, bounded tier:
it makes the code it *can* speed up substantially faster, and it never makes
anything slower.

## What T2-Full delivers (measured)

| shape | speedup | evidence |
|---|---|---|
| **single-clause byte scan-and-count** (validators/lexers: one guarded clause + catch-all, integer/count accumulator, **no output binary**) | **2.5–3.1×** (Apple Silicon); up to 16× digit-scan on server ARM | scanbench, [`14`](14_real_service_value.md) §4, [`10`](10_p26_install_gate.md), M0.5 ARM |
| **integer/float tail loops** (arith accumulation, `tsum`-class; foldl-class via lists intrinsics) | **1.4–2.0×** | [`10`](10_p26_install_gate.md), [`09_p2_implementation.md`](09_p2_implementation.md) |
| **everything else** | **never slower than T1** (P2.6 install gate keeps non-winning blobs from installing) | [`10`](10_p26_install_gate.md), [`14`](14_real_service_value.md) |

The tier is **counter-triggered** (tier-up threshold `1000·√(size+1)`, profiled on
scheduler 1 only — P2.5), **async-compiled** off the hot scheduler (P2.6-A), and
**install-gated** so only work-removing blobs install (P2.6-B). Default config:
gate ON. Behavioral parity verified (identical `phash2` across T1 / counter /
force-compile modes over spawn/link/monitor, exits, GC, receive, exceptions,
dispatch, bitstrings).

## What T2-Full does NOT deliver, and why (the measured death of the 20 % thesis)

The general-tier ambition rested on two pools (M0) and one enabling phase (P3).
All three came up short, each **measured, not assumed**:

1. **Analysis/compiler code — marginal (~3–6 %).** dialyzer/compiler are hot but
   *eligible-yet-irreducible*: pointer-chasing tree/graph traversal, `select_val`
   dispatch, closures, hash lookups. T2 wins by *removing work*; this code's cost
   is intrinsic, so fusion removes only a cheap call barrier. Rung-2's value
   ceiling on the hottest real pair (`erl_types:are_all_limited`↔`is_limited`,
   10.8 % of a dialyzer run) measured **~3–6 %** — below the 10 % bar the
   12–19-week rung-2 build was predicated on. ([`11`](11_body_recursion_prize.md),
   [`13`](13_rung2_value_ceiling.md))

2. **Production services — ~0 % (ineligible at the frontend).** JSON, HTTP,
   binary codecs, base64: **97–100 % of own-time is in functions T2 cannot even
   compile.** Their vocabulary — binary *construction* (`bs_create_bin`/`bs_put*`),
   map ops, `call_fun` continuations, `bs_get/set_position` — **is not in T2's
   eligible-opcode set**. This is a *frontend eligibility* wall, worse than the
   irreducibility of (1). Whole-workload deltas: JSON +0.6 %, frame codec +0.9 %,
   HTTP −1.7 %, base64 +0.2 %, estone −3.8 %→gate-recovered — all ±2 % noise,
   **zero hot-path installs.** ([`14`](14_real_service_value.md))

3. **Rung-2 (P3) rescues neither.** Rung-2 = framestates + eager-CP-push /
   CP-on-stack — a *backend* change that keeps the T2 *ascent* alive across a
   non-tail call. It adds **zero opcode coverage**. So it is **measured-irrelevant
   to services** (cannot make one ineligible JSON/HTTP function eligible) and only
   the marginal ~3–6 % on analysis. The correctness spike was GO
   ([`12`](12_rung2_derisk_spike.md)) — rung-2 is *buildable* — but the *value*
   isn't there. Not funded.

**Net:** T2's "remove work" model pays only where there is boxed arithmetic,
allocation, or a fusible scan to remove. Real applications spend their time
either in intrinsically-irreducible symbolic code (analysis) or in opcodes T2's
frontend doesn't cover (services). The 20 % general win is not reachable by any
currently-scoped T2 work.

## The install gate — the one measured hole closed (2026-07-08, `cf33c4cee8`)

Memo 14 §4 found the one hole in the never-slower floor: a **multi-clause** byte
classifier (`lex_wl:classify/4`) *installed* yet ran **+40 %** slower, because the
gate's "eliminated-work" signals over-accepted — `bs_scan≥1` fired on any bs
match op, and `fused_arith≥2` summed three *mutually-exclusive* per-clause
increments. Fixed by (a) tightening the bs arm to the emitter's **fused
scan-run** admission (`scan_runs≥1`, plumbed from `admit_scan_loop` in
`t2_emit.cpp`) instead of any `BsMatch`, and (b) adding a disqualifier
`bs_unfused = (bs_scan≥1 && scan_runs==0)` so an un-fused per-byte bs loop is
rejected regardless of which arm accepted. Verified: `lex_wl` now rejects and
recovers to the T1 floor; scanbench (2.5×/3.2×), tsum (1.65×), mvp (1.63×) all
still install and win; estone +0.4 %; phash2 identical. The distinguishing signal
(single-path fused loop vs multi-path per-byte) fully separates winners from the
loser with no winner lost.

**Known residual (unmeasured, scoped out).** `bs_unfused` closes the one
*measured* hole. Its root — the histogram signals sum statically-counted,
path-mutually-exclusive ops with no path awareness (`fused_arith` sums every
`AddSmall`/`SubSmall` across all blocks; `lex_wl`'s three per-clause increments
run one-per-byte yet count as 3) — is patched only where a bs op co-occurs. A
*non-bs* multi-clause integer accumulator (e.g. `loop([a|T],X,Y)->loop(T,X+1,Y);
loop([b|T],X,Y)->loop(T,X,Y+1)`) has `bs_scan==0`, so `bs_unfused` does not fire
and `fused_arith≥2` could over-accept via the same mechanism. This is unmeasured
and may not even be a real regression (T1 also boxes per clause, so a multi-path
arith loop can still clear the T1 floor absent a co-occurring per-op tax like the
bs match). The general fix is cheap — surface the "recovered loop body is
single-path" bit the emitter already computes (`admit_scan_loop`, `t2_emit.cpp`
"two in-region successors → reject") and require it for *any* histogram-summed win
signal, not just bs — but on a wound-down specialist tier it is scoped out until
(and unless) the non-bs case is measured to regress. So: the floor's one measured
hole is closed; the path-blind histogram over-count is a documented residual.

## What "T2 for services" would actually require (not funded)

To make T2 relevant to the service class you would need a **different, larger
frontend project** than rung-2: teach the eligibility scan + isel + emit to
handle **binary construction, map operations, and `call_fun`**, and — because the
`lex_wl` case proves eligible code is not reliably faster — replace the path-blind
histogram signals with **real per-shape cost modeling** (the summing defect is
signal-general, not bs-specific; see the known residual above). That is a bigger
bet than rung-2, and this evidence gives it no support. Recorded as an option, not a
plan.

## Evidence trail

M0 measurement → [`06_phases.md`](06_phases.md) M0 table + the PROGRAM STATUS
banner. Build waves → [`07`](07_p0_implementation.md)–[`09`](09_p2_implementation.md).
The gate → [`10`](10_p26_install_gate.md). The prize + technique survey →
[`11`](11_body_recursion_prize.md). Rung-2 correctness (GO) →
[`12`](12_rung2_derisk_spike.md). Rung-2 value (FALLS SHORT) →
[`13`](13_rung2_value_ceiling.md). Real-service value (~0 %, SPECIALIST) →
[`14`](14_real_service_value.md).

## Recommendation

1. **Consolidate + present T2-Full as a specialist tier**: 2.5–3.1× on byte
   scan-and-count kernels and integer/float tail loops, never-slower floor
   elsewhere (the one measured hole closed; one documented path-blind-histogram
   residual). Do not market or ship it as a general 20 % tier.
2. **Do not build rung-2 / P3, P4, P5** on the current evidence — decided-against
   (retained in the plan docs for the evidence record only).
3. If broad-service value ever becomes a hard requirement, scope the **frontend
   eligibility expansion** above as a separate, larger program — with its own M0.
