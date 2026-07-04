# T2-Full — Roadmap, Gates, Risks

> Phases with entry evidence, exit gates, and effort. The governing
> rule is inherited: **no infrastructure lands ahead of a validated
> win that needs it** — but where 08 shelved on nulls, this plan
> runs the *missing measurements first* and funds phases on their
> results. Effort figures are single-engineer implementation weeks
> (the reference design's currency; multiply ~×1.5–2 for calendar).

## M0 — Measurement & scope-setting (6–8 weeks, mostly tooling)

Prices the two pools the 20 % thesis depends on and closes the
"unpriced" list from the evidence dossier. Nothing here is wasted
if T2 changes shape — it is all reusable profiling tooling and
corpus infrastructure.

| # | measurement | method | feeds |
|---|---|---|---|
| M0.1 | **Elimination-rich boundary pool**: cycle share of hot cross-module/fun call sites where caller facts would eliminate callee work (guard subsumption, constant args — literal funs above all, construct/deconstruct pairs) | cycle profiles (perf `+JPperf`) joined with a static eliminability classifier over the hot functions' BEAM code | P3 go/scope |
| M0.2 | **Sinkable-allocation pool**: fraction of allocation volume from T2-eligible hot code that dies young and region-locally | idea-#50 term-lifetime instrumentation (otp-ideas repo; shadow-array design) on the app corpus | P5 go/scope |
| M0.3 | Elixir `Enum`/protocol wrapper dynamic weight | cycle profile of an Elixir corpus (Elixir compiler, Phoenix app, Broadway pipeline) | P3 scoring table |
| M0.4 | Float-heavy loop share | corpus census + cycle profiles | P5 float-unboxing scope |
| M0.5 | Graviton re-run of the experiment kit | existing kit | validity of Apple-Silicon nulls/wins on server ARM |
| M0.6 | Corpus extension: MongooseIM under Amoc, an Ecto-heavy service, Elixir compiler, plus the existing suite | [`../T2/08_v1_loop_tier.md`](../T2/08_v1_loop_tier.md) §6 | all gates |
| M0.7 | Dialyzer/compiler perf DSO split (JIT vs C share of the 75.8 % emulator time) | Linux perf leg | compute-class targets |

**Gate G-M0** (scope, not kill): sum the measured pools against the
per-class targets of [`00_goal_and_thesis.md`](00_goal_and_thesis.md)
§1.1. If pillars 1–3 cannot arithmetically reach ≥20 % on the
compute corpus, the finding goes to the project owner *before* P3+
is funded — with the loop tier (P0–P2) unaffected, since its
evidence is already banked.

## P0–P2 — The loop tier (16–20 weeks; ≡ 08's Track B, absorbed)

Identical scope, gates, and estimates to
[`../T2/08_v1_loop_tier.md`](../T2/08_v1_loop_tier.md) §8 Track B —
P0 foundations (SSA reconstruction + **G1 fidelity gate**, PC side
tables, trace/inspection matrices), P1 identity transform (full OTP
suite green under `+JT2enable` — the state-preservation proof), P2
speculation + loop recovery + back-edge resume + leaf inlining +
the binary scan subset + `lists:*` intrinsics (**G2: MVP ≥1.8×,
G-bin bytewise ≥2.5×, ≤1 % tax; G4: intrinsics vs
`inline_list_funcs`-on baseline**).

Differences from 08: (a) Track A/re-baseline is subsumed by M0 —
the A1 scan-run superinstruction was built, proven, and
deliberately not merged (its win ships through T2 instead); A2/A3
remain optional upstreamable spikes that M0's corpus runs will
price incidentally; (b) the IR is built HIR/LIR-seamed from day one
([`04_backend.md`](04_backend.md) §4) so P3+ never reworks P2 code.

**P2 is a shippable tier**: if everything after it stalled, the
loop tier stands alone on its own evidence.

## P3 — General inlining (10–12 weeks)

*Entry evidence*: G-M0's elimination-rich pool number; the Graal
deep-trials and Flambda calibrations
([`03_optimizer.md`](03_optimizer.md) §2.5).

Contents: interior profiling (call-return/switch type slots,
monomorphic-target slots with frequency counts, branch counters —
[`02_profiling.md`](02_profiling.md) §2); framestates +
`parent_fs` + eager-CP-push + per-region deopt stubs
([`01_architecture.md`](01_architecture.md) §2 rung 2); the
tombstone/lazy-scan lifecycle + trace own-stack scan
([`05_runtime.md`](05_runtime.md) §3); the elimination-scored
cross-module inliner with Maglev-class budgets + guarded two-target
PIC-lite; tier-up target selection (compile the dominant caller).

**Gate G5** (cycle-measured, on the M0-identified corpus):
≥10 % on the targeted hot functions end-to-end attributable to
inlining-enabled elimination; compile budget held (≤1 ms median /
10 ms cap); eligibility coverage at target; deopt rate bounded
(steady-state exits ≈ 0 on stable workloads, per the V8/IISWC
finding that guards should essentially never fire after warmup).
Miss → the inliner narrows to its measured-positive subset
(intrinsics + literal-fun sites) and P4 proceeds on loop-tier
shapes only.

## P4 — Classic optimization suite (6–8 weeks)

GVN/CSE (backward-chained), full LICM including speculative guard
motion (Graal-calibrated: the one general optimization with
measured double-digit analogs on loop code), unrolling ×4/×8 with
`test_heap` re-placement and the SWAR recipe library (the binary
expansion package — **gate: G-bin full ≥4× isolated scan**), block
layout from branch weights. Runs on fused regions from P3, which is
why it follows rather than precedes.

**Gate G6**: the compute-kernel class hits **≥20 % end-to-end**
(stdlib `json` family, base64, parser corpus) vs current master T1,
and the compute-application class (dialyzer, Elixir compiler)
reports its measured number against the 5–15 % expectation.

## P5 — Allocation elimination (8–12 weeks)

*Entry evidence*: G-M0's sinkable-allocation pool; PyPy/Graal/
LuaJIT calibrations ([`03_optimizer.md`](03_optimizer.md) §6).

P5a: escape analysis + deopt-dead sinking + construct/deconstruct
fusion + float unboxing (no rematerialization). **Gate G7**:
allocation volume in T2-covered code down ≥10 % on the service
corpus, with a measurable GC-share drop (msacc) and the service
class moving toward its ≥10 % target. P5b (framestate virtuals +
deopt-path rematerialization) funded only if G7 shows a residual
concentrated in cold-path escapes — the Graal-PEA shape.

## P6 — Hardening & ship (6–8 weeks)

Eviction at scale, watchpoint storms (many-module reload),
observability polish (inline-tree introspection, eligibility
metric, allocation counters), docs, the full inspection matrix
executed, suite-wide floor: **≤1 % regression on every tracked
benchmark, ≤2 % CI gate** on the regression suite; forced-deopt +
lifecycle + concurrency stress suites green
([`../T2/07_delivery.md`](../T2/07_delivery.md) §16A).

## P7 — x86_64 port (8–10 weeks, separable)

LIR backend only — isel/regalloc constraints/encodings; HIR and
all optimizations untouched (the seam's payoff). Entry condition:
the tier has cleared G6 on aarch64. The Cranelift option is
re-evaluated here as the port vehicle
([`04_backend.md`](04_backend.md) §3).

## Effort summary

| phase | weeks | cumulative |
|---|---|---|
| M0 | 6–8 | 8 |
| P0–P2 (loop tier) | 16–20 | 28 |
| P3 (inlining) | 10–12 | 40 |
| P4 (classic opts) | 6–8 | 48 |
| P5 (allocation) | 8–12 | 60 |
| P6 (ship) | 6–8 | 68 |
| P7 (x86_64) | 8–10 | 78 |

~60–68 engineering weeks to the shipped aarch64 tier (~78 with the
port), before the ×1.5–2 calendar factor — i.e. a 2–3 year program
for one engineer, proportionally less for two. Three ship points
(P2, P4/P6, P7) keep value landing along the way. For calibration:
ZJIT (several engineers) took ~12 months to a merged
still-behind-template tier and ~18 to selective wins — the phase
gates here exist precisely so that outcome is detected at G5/G6,
not after P7.

## Risks

| risk | signal | mitigation |
|---|---|---|
| Elimination-rich pool too small (the 20 % thesis fails at M0) | G-M0 arithmetic | loop tier still ships; goal restated with data; VM-internal track re-prioritized |
| ZJIT-shaped outcome: tier trails T1 broadly for months | G2/G5 misses | shippable P2 first; every phase gated on its own pool; T1 floor guaranteed by architecture (side-exit, never slower — the BEAMJIT failure mode is designed out) |
| Deopt storms / recompile loops | deopt-rate metric in t2_stats | production-proven policy set: per-site failure memory + widen-or-drop, exponential backoff (100·2^R), permanent demotion (HotSpot's 400-recompile cutoff analog); profile poisoning on jettison |
| Compile-time slip with inlining | budget metric at G5 | Maglev-class inline budgets; abort-to-T1; queue back-pressure (HotSpot-style threshold scaling) |
| Profiling overhead eats the win (HiPErJiT precedent) | ≤1 % budget gate each phase | eligibility-gated sites, sharded counters, staged with consumers |
| State-model bugs (the MVP's corruption class) | identity suite + IR validator | dominating-guard invariant enforced by validator; AND-fusion rule; forced-deopt harness |
| Maintenance concentration | — | same competency as BeamAsm, same team, shared substrate; HIR/LIR seam bounds any future backend swap |
| Apple-Silicon-only evidence | M0.5 | Graviton runs before P3 funding |

## Open questions

1. **Receive loops — RESOLVED (M0.R, 2026-07-04,
   [`../verification/RECV_RESULTS.md`](../verification/RECV_RESULTS.md))**:
   `receive` stays a region-terminator; no first-class receive IR.
   Measured with exact receive-instance classification counters
   (per-scheduler, terminal-based): the message-already-queued "hit"
   rate is **anti-correlated with JIT-addressability** — 99 %+ on
   the messaging class the tier already concedes (ring/flood),
   structurally capped at 50 % for gen_server call/reply (the
   client's reply-receive is a synchronous round-trip), and receive
   density is negligible on the compute class (dialyzer: 124 k
   instances in 7 s, 18.7 % hit). The one workload with both high
   hit rate (89.9 %) and real scan depth (Bandit: 5.3 % of
   instances scan ~15 deep) sizes to sub-1 % of a core. Hedge
   recorded: a narrow `loop_rec`-scan-ownership optimization for
   Bandit-shaped selective receives (pillar-1 shape, not receive
   IR), gated on an M0.6 cycle profile.
2. **PIC width**: guarded dispatch beyond 2 targets rarely pays
   (HotSpot bimorphic precedent) — confirm on the Elixir protocol
   corpus before building wider.
3. **Fun identity across upgrades**: literal-fun inlining guards on
   fun identity; module upgrade jettisons via watchpoints — but
   funs stored in state (ETS, process state) crossing generations
   need the guard, not just the watchpoint. Covered by rung-1
   guards; verify in the P3 inspection matrix.
4. **NIF-heavy boundaries**: calls into NIFs are opaque
   region-terminators; is there a pool in NIF-adjacent glue worth
   region-splitting for? M0.7 will show it if so.
5. **Two-engineer split**: profiling/runtime (ERTS-side) vs
   optimizer (HIR) partitions cleanly at the IR interface; the
   plan's phases serialize on gates, not on files.
