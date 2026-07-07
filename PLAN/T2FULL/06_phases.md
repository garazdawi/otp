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
| M0.1 | **Elimination-rich boundary pool** — **DONE** ([`../verification/ELIM_RESULTS.md`](../verification/ELIM_RESULTS.md), tool in [`../verification/elimscan/`](../verification/elimscan/)): **thin**. Dialyzer's two hot families (~21 % of busy): **~7.3 %** cycle-weighted core-eliminable (family A 10.5 %, family B 2.9 % with 79 % `none` — the G3-2 null quantitatively confirmed). json 6.5 % (its 37 % liberal constant-args collapses to 0.4 % effective — json's win is pillar-1 fusion, not P3). Structural census: 10–14 % at remote boundaries, ~85 % `none`; live sub-pools there are guard-subsumption (9–16 %) and literal funs (3–11 %). Intra-module subsumption is already stripped by AOT — the pool lives at remote boundaries only. **Consequence: P3 in its broad form is not a ≥10 % standalone carrier on compute apps; fund the narrow enabler form** (literal-fun collapse + monomorphic guard-subsumption + enabler-for-P5), per the G5 fallback. Static lower bound — profile-guided speculation widens the guard pool. | static SSA classifier (post-`beam_ssa_opt`), 306 modules / 80 656 call sites, 97.9 % cross-module resolution, cycle-weighted | P3 go/scope |
| M0.2 | **Sinkable-allocation pool** — **DONE** ([`../verification/SINK_RESULTS.md`](../verification/SINK_RESULTS.md), tool in [`../verification/sinkscan/`](../verification/sinkscan/); static-escape × GALLOC-volume hybrid instead of the invasive idea-#50 tool): **the classic pool is missing**. Thin-Bandit ≈ **0.0 %** volume-weighted sinkable (89 % coverage incl. real Jason); RabbitMQ t1 3.2 %/t2 0.0 %; structural ceiling t1 0.6 % + t2 7.3 % → all legs < 1 % end-to-end via the GC chain. Why: AOT already scalar-replaces the trivial pairs (they produce no allocation site at all); the top allocator is BIF-internal (`maps:from_list`); hot escapes ARE the result being built — reachable only by whole-loop fusion with cross-iteration virtuals (P5b shape). Float-box sites: 91 corpus-wide, zero in every hot leg — **float-unboxing pool on services is nil** (also answers M0.4 for services). **Consequence: P5 is not funded standalone** — P5a-lite (construct/deconstruct fusion + deopt-dead sinking) folds into P3/P4; P5b stays unfunded until a fused-loop prototype exists, then re-run sinkscan on fused-region IR (the tool accepts any SSA). | static SSA escape classifier weighted by measured galloc volumes (three legs, full fidelity, phi-use bug found+fixed mid-run) | P5 go/scope |
| M0.3 | Elixir `Enum`/protocol wrapper dynamic weight — **DONE** ([`../verification/M0_PROFILES.md`](../verification/M0_PROFILES.md)): ≈0 % on the Elixir-compiler leg; ~8.7 % even on a pure-pipeline workload, whose real cost is **allocator churn ~15 % + GC ~6 % from per-stage intermediate lists**. Consequence: keep Enum low-weighted in the P3 scoring table; its value routes through pillar 3 — Enum inlining is the *enabler for sinking the intermediate lists*, further coupling P3→P5. Running-app (Phoenix/Broadway) profile still owed under M0.6. | cycle profile (perf-in-colima) | P3 scoring table |
| M0.4 | Float-heavy loop share | corpus census + cycle profiles | P5 float-unboxing scope |
| M0.5 | Server-ARM re-run of the experiment kit — **DONE** ([`../verification/ARM_RESULTS.md`](../verification/ARM_RESULTS.md); GHA ubuntu-24.04-arm per owner decision, standing CI on the fork's armci branch): **all nulls hold** (G3-2 0.87, G3-1 1.03), **all positives hold and the scan class strengthens** (plain 3.71× vs 2.93× Apple, digit 16.52× vs 4.08×, json e2e 1.28× vs 1.10×) — pillar 1 is stronger on server ARM; recv classification arch-invariant | GHA arm64 workflow (t2-arm-bench.yaml) | validity of Apple-Silicon nulls/wins on server ARM |
| M0.6 | Corpus extension: MongooseIM under Amoc, an Ecto-heavy service, a running Phoenix/Broadway app, plus the existing suite | [`../T2/08_v1_loop_tier.md`](../T2/08_v1_loop_tier.md) §6 | all gates |
| M0.7 | Dialyzer/compiler perf DSO split — **DONE** ([`../verification/M0_PROFILES.md`](../verification/M0_PROFILES.md)): dialyzer [JIT] = **34 %** of on-CPU (≈49 % of non-GC emulator; GC ≈ 18 % of the C bucket), Elixir compiler = **23.8 %**; compute-application addressable band is **24–34 %**, confirming the 5–15 % honest target with 20 % as the all-pillars stretch. `erl_types` family alone is ≥13–17 % of dialyzer CPU — the G3-2 subject; elimination-rich inlining (M0.1) is what must move it. | Linux perf leg (per-pid `-p`, not `-a` — idle mis-attribution) | compute-class targets |

**Gate G-M0 — RESOLVED (2026-07-05, owner decision: rescope).**
The measured pools (M0.1 thin, M0.2 missing, M0.7 addressable
24–34 %) do not fund P3-broad or P5-standalone. Owner-approved
rescope:

- **P0–P2 loop tier: unchanged** (evidence banked; build underway).
- **P3 narrowed** to the measured-positive enabler form:
  literal-fun collapse + monomorphic-site guard-subsumption +
  construct/deconstruct fusion (the P5a-lite subset folded in).
  Framestates + eager-CP-push ship only in the scope this needs.
  ~6–8 weeks (was 10–12).
- **P4 unchanged** — the classic loop-opt suite carries the
  biggest measured wins (unrolling/SWAR ≥4× gate).
- **P5 deleted as a standalone phase.** P5b (cross-iteration
  virtuals in fused loops — the only shape M0.2 found the real
  pool in) is re-priced by running sinkscan on fused-region IR
  after P2; it reopens only on that evidence.
- Revised total: **~44–52 engineering weeks** to the shipped
  aarch64 tier (was 60–68); P7 port unchanged.
- Goal restatement stands as measured: **≥20 % on compute
  kernels/parsers; 5–10 % on compute applications; JSON-class
  compound on services; single digits on brokers.** The 20 %-on-
  servers route remains the VM-internal track, outside this plan.

## P0–P2 — The loop tier (16–20 weeks; ≡ 08's Track B, absorbed)

> **P0 status: COMPLETE, G1 PASSED (2026-07-05).** All eight
> work-order commits merged (`a89664c804..0ff215f5c5`): jit/t2/
> HIR + lattice + validator, two-phase code-chunk/Type retention
> (prepare-copies/finalize-attaches — the map's finalize-only
> placement was a use-after-free, found and fixed), Braun SSA
> builder (8,661 functions / 161 modules, zero failures,
> deterministic), `t2_build_ssa` debug BIF, T1 PC side table,
> `t2_ranges` blob class, and the G1 comparator as a standing CT
> suite (`t2_g1_SUITE`). **G1 verdict**: criterion amended from
> "identical CFG vs `dssaopt`" (unachievable and wrong across the
> codegen boundary) to "zero genuine reconstruction loss +
> content-faithful at the `dprecg` point modulo a documented
> equivalence table". Measured: CFG 64.6 %, strict content 62.3 %,
> equivalence-credited **92 %**, with the residual 14 functions
> individually verified as comparison-spelling deltas → **100 %
> content-faithful; zero loss found**. SSA-chunk fallback not
> needed. Caveat: 120/295 corpus functions were `not_eligible` at
> P0's op set (binaries/maps/complex guards) — the suite re-runs
> as eligibility widens in P2. Next: P1.
>
> **P1 status: functionally COMPLETE (2026-07-06, HEAD
> `6920ba6306`), full-sweep confirmation pending.** Identity
> pipeline (HIR→LIR→isel→sync-everything regalloc→emission via
> reused T1 emitters), sync-point register maps + frame ops (7
> pinned design decisions), prologue-patch install/revert +
> `+JT2enable` (compile-at-load ~75 ms, 4 808 blobs at boot),
> trace-always-wins jettison, full purge integration, light-BIF
> demote-on-yield/trap (`ERTS_T2_PC_BIF`; yield ARG3 = T1 site PC,
> trap CP = T1 CONT; shared emission helper with T1 so fragments
> cannot drift). Coverage **87.7 %** of eligible functions (94.3 %
> excluding never-lowerable BIF stubs). Gate slice green
> (process/exception/trace×6/code/nif + stdlib smoke); reductions
> byte-identical incl. multi-timeslice BIF yields; G1 unchanged.
> Real bugs the gate caught: `select_tuple_arity` header-vs-value
> compare, loader-transformed pseudo-BIF reduction inflation,
> BIF-stub dead-chunk install, debug-emulator lock-order (incl. a
> pre-existing branch-wide `alloc_profile` one). Rejected-in-P1
> (documented): heavy-BIF sites (~23), non-arith gc_bifs (~91),
> exact-compare literal operands (~97), on_load modules.
>
> **P1 CLOSED (2026-07-06): the full-sweep gate is green.** All
> 105 emulator suites + stdlib/kernel slices, run PAIRED (baseline
> vs `+JT2enable`, 115 suite-runs each side, ~2 450 cases): **zero
> T2-only failures after triage**. Every apparent delta
> dispositioned with evidence: the one T2-only sweep failure
> (`signal_SUITE:parallel_signal_enqueue_race_2`) soaked 20×/config
> — T2 0 failures, baseline 3, i.e. config-independent flaky (the
> PR-7822-class wedge firing on baseline is worth an upstream
> note); `z_SUITE` was a shared-peer-node naming artifact,
> byte-identical when re-paired; `nif_SUITE` a harness-ordering
> artifact. The one genuine bug found was config-independent, in
> the branch's old A1-0 scan pass (float compare literal →
> `lists:seq/2` crash) — fixed, `9a388c9359`. State-model-critical
> suites (exception, process, trace×11, code load/purge, gc,
> signal, receive, bs, nif, fun, hibernate, distribution,
> scheduler) byte-identical between configs. Boot: 5 569 built /
> 4 808 installed / 0 failures / 73 ms compile. Server-ARM
> confirmation rides the M0.5 workflow. **Next: P2 — the loop
> tier.**

> **P2 behavioral gate CLOSED (2026-07-06); G2 tax gate OPEN.**
> Full paired sweep on `lukas/erts/beamjit2` HEAD `294222cecd`:
> **290 suite-legs — Phase A (24 state-model-critical suites ×3
> legs: base / `+JT2enable` / `T2_RETAIN` prod) + Phase B (full
> emulator ×2) + Phase C (stdlib/kernel slices) — with ZERO
> base-vs-t2 mismatches.** Every non-green suite (process ei-node
> 96/1/1; a_SUITE/bif/dump/z_SUITE environmental; gen_server/binary
> harness parse) fails IDENTICALLY in the T1 base — no T2-only
> delta. The one flagged cross-leg item (`signal_SUITE` prod
> timetrap) was proven machine contention by a clean re-run (prod
> 38/0 at 89–91 s = base). With the build gates (matrix + reductions
> byte-identity), G1 fidelity (181/114 identical Apple/Neoverse), and
> json cross-mode hash identity, the **P2 exit criterion — no
> observable difference except speed — holds.**
>
> Performance: **G2 kernel wins MET** — mvp `total/2` 1.95×,
> `mk_txns` 2.01×, scanbench 2.67×/3.05× (organic tier-up);
> ARM-robust (Neoverse 2.83×/3.36×). **G2 ≤1 % tax gate: entry tax
> FIXED (P2.5), two NEW blockers OPEN.** The P2.5 decomposition
> (2026-07-06) vindicated the −20 % suspicion and split it:
> - **Steady entry-sequence tax was real** — −17 % spread / −22 %
>   pinned (estone), +35 % (dialyzer PLT), isolated cleanly by the
>   zero-churn armed-never-trip leg (`tier_stats {0,…}`), so NOT core
>   noise — and is now **FIXED** (`cb8b084082`): scheduler-1 early-out
>   (`cbnz`; counting was already sched-1-only, so skipping off sched-1
>   is free + behaviorally identical) + FCALLS-sampled counting
>   (STRIDE 64, +STRIDE bump keeps trip timing). Recovers to **~0 %
>   spread / −9 % pinned** (estone), dialyzer never-trip **+35 %→
>   +1.2 %**. Kernel wins preserved (mvp 1.82×, scan 2.68× on the P2.5
>   harness), prod behavioral smoke clean. Census: never-trippers
>   95 %(estone)/81 %(dialyzer) legitimately cold — fix correctly
>   scoped.
> - **BUT the production headline is NOT the entry tax.** With the fix
>   in, prod estone still ≈−30 % / dialyzer +11 %, dominated by two
>   SEPARATE problems P2.5 cannot touch:
>   - **(A) Synchronous inline compilation on the hot path — FIXED
>     (2026-07-07, `3b89ce7c5f`).** The trip on sched-1 now only enqueues
>     + schedules the compile as misc aux work on another normal
>     scheduler (re-seizing code-mod trampoline `t2_tier_seize_and_run`),
>     returning to Erlang immediately. (Dirty scheduler ruled out: can't
>     run aux work / can't hold code-mod permission that install+disarm
>     require.) Estone pattern first-run **1717 µs → 727 µs** (≈ T1 554);
>     prod estone TOTAL back to T1 (441 k vs 442 k µs); dialyzer
>     5.46 → 5.31 s. Kernel wins byte-identical (only dispatch moved);
>     behavioral smoke CLEAN incl. the code-loading race suites
>     (code/code_parallel_load/multi_load/trace). Also fixed a latent
>     no-reseize bug in the old async path.
>   - **(B) Net-negative T2 blobs — DIAGNOSED (2026-07-07,
>     [10_p26_install_gate.md](10_p26_install_gate.md)).** Principle: T2
>     beats T1 *iff it removes work*; losers re-emit T1's ops + a tax.
>     Two mechanisms — (i) non-tail-call demote-on-return lowering
>     (`t2_emit.cpp:1342`): body-recursive functions run their real work
>     in T1 anyway and pay a ~3.5 ns/call indirect-branch tax (append/2
>     3.1× slower) — NOT codegen-fixable, gate it; (ii) T2 lowers a big
>     `select_val` as a 27-way linear scan vs T1's binary search — IS
>     fixable (port `i_select_val_bins`) but only reaches parity, gate it
>     too. **Reframe:** the −52 %/−60 % were `+JT2enable` artifacts —
>     under production `T2_RETAIN` these shapes are BREAK-EVEN (losers
>     mostly never trip), so B only bites when a loser actually installs.
>     **Static install-quality gate designed + verified** (INSTALL iff
>     eliminated-work ∧ ¬disqualifying-tax; passes all winners
>     mvp/scan/tsum, rejects all losers append/reverse/nsum/pat_loop) —
>     lands after A. dialyzer's +11 % is body-recursive installs (the
>     gate's prime target) + churn (A's target); 14 tripped-then-failed
>     = P3 backlog.
> **P2.6 COMPLETE (2026-07-07): (A) async compile `3b89ce7c5f` + (B)
> install-quality gate `41af0c4985` both landed.** The gate restores the
> never-slower floor — losers no longer install (dialyzer: 20 of 25
> tripped functions rejected, recover to T1; lists micro 1103→443 µs =
> T1), winners preserved (mvp 1.85–2.0×, scan 2.5–2.9×, fold intact),
> behaviorally safe by construction (strictly subtractive; phash2
> identical across modes). Dialyzer PLT **+13 %→+3.3 %** over T1.
> **Residual + fundamental limit:** the +3.3 % is profiling +
> wasted-compile overhead on an all-body-recursive workload T2 *cannot
> accelerate* — dialyzer gets ~0 T2 benefit regardless of tax, because
> demote-on-return runs the ascent (the real work) in T1. That's the
> real story for body-recursion-heavy code, tracked for P2.7/P3
> (early-reject-before-compile to kill the wasted compile; body-recursion
> is a later-tier problem). **P2 substantially CLOSED** — behavioral +
> G1 gates hold, kernel wins hold (≥20 % class met), never-slower floor
> restored. Next: **P3-narrow** (pending user go).

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
