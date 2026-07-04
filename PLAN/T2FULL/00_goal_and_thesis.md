# T2-Full — Goal, Evidence, Thesis

> Part of [`PLAN/T2FULL/`](README.md), the plan for the full
> profile-driven optimizing tier. This file defines what "≥20 % on
> most applications" means, confronts it with the measured evidence,
> and states the thesis for where the cycles come from.

## 1. The goal

**≥20 % end-to-end performance improvement on most applications**,
driven by profile data collected in T1 (BeamAsm), with cross-module
inlining, LICM, loop unrolling, and the classic optimization
repertoire as mandated capabilities. aarch64 first; x86_64 as a port
phase. No time cap: the plan is the technically credible path to the
goal, structured so every expensive step is preceded by the
measurement that justifies it.

Two scope simplifications are granted up front and used throughout:
**hot code loading and tracing force deoptimization** — a module
upgrade or trace enablement jettisons every affected T2 blob (the
watchpoint + jettison machinery of [`../T2/05_runtime.md`](../T2/05_runtime.md)
§14 and the trace-mutual-exclusion rule of
[`../T2/06_dispatch_and_sideexit.md`](../T2/06_dispatch_and_sideexit.md)
§1.1). No attempt is made to keep optimized code alive across either
event.

### 1.1 What "most applications" must mean

The measured cycle profiles
([`../verification/PROFILE_RESULTS.md`](../verification/PROFILE_RESULTS.md))
split real workloads into three classes, and the goal must be stated
per class, because the JIT-addressable fraction differs by 6× across
them:

| class | exemplar | JIT-addressable share of cycles | 1.20× end-to-end requires |
|---|---|---|---|
| **compute kernels** | stdlib `json`, base64, parsers, dataflow inner loops | 60–90 % | ~1.3–1.5× on Erlang code — **reachable** (fusion alone measured 5.6× on scans) |
| **compute applications** | dialyzer, compiler, Elixir compiler | **24–34 % measured** (M0.7: dialyzer [JIT] = 34 % of on-CPU, ≈49 % of non-GC emulator time; Elixir compiler 23.8 % — [`../verification/M0_PROFILES.md`](../verification/M0_PROFILES.md)) | ~2.0× on all JIT code at ~100 % eligibility — **stretch confirmed**; 5–15 % is what 34 % addressable supports |
| **serialization-heavy service** | Bandit/Phoenix JSON API | 20.7 % ([JIT] perf share) + 12.3 % GC | ~5.1× on all JIT code alone — **not reachable by execution speed alone**; reachable only in combination with the GC pool (§3.3) |
| **messaging/broker** | RabbitMQ at 489 k msg/s | 12.2 % ([JIT]) + 9.5 % GC | infinite speedup on JIT code caps at 1.14× — **not reachable by a JIT** |

The honest goal statement this plan commits to:

1. **≥20 % on compute kernels and serialization/parsing code** —
   the class where the measured mechanisms (fusion 5.6×, shape
   guards 1.64×, elimination-inlining 2×) stack on a 60–90 %
   addressable share. This is the class the tier is *for*, and it
   is a real class: JSON/protocol codecs, compilers, template
   engines, Ecto query building, message codecs sit on the hot
   path of most production BEAM systems.
2. **5–15 % on compute applications** (dialyzer-class), honestly
   stated — with ≥20 % as the stretch outcome if eligibility
   coverage and the elimination-rich boundary pool (M0's central
   measurement) come in at the top of their ranges.
3. **≥10 % on serialization-heavy services**, achieved as the
   compound of execution wins on the [JIT] share *plus* measured GC
   reduction from allocation elimination (§3.3).
4. **Single digits on messaging/brokers**, stated openly. Getting
   20 % on RabbitMQ requires the VM-internal track (signal/message
   path, ETS, copy) that
   [`../T2/08_v1_loop_tier.md`](../T2/08_v1_loop_tier.md) §8 already
   identified as a separate program; no JIT design can change that
   arithmetic, and this plan does not pretend to.

If "most applications" is read as "most deployed servers", no JIT
tier can deliver 20 % — the plan says so with numbers rather than
discovering it after two years of build. If it is read as "most
CPU-bound Erlang/Elixir code", the plan's pillars are sized for it.

### 1.3 "BeamAsm gave ~20 % on RabbitMQ — why can't T2?"

Because BeamAsm's win is precisely what shrank the pool T2 draws
from. The interpreter taxed **every executed BEAM instruction**
with fetch/decode/dispatch — a uniform ~2× tax on all Erlang code,
including the glue code around the message path. Under the
interpreter, Erlang-code execution therefore occupied roughly
**twice** its current share of RabbitMQ's cycles (~24 % rather than
today's measured 12.2 %); halving that pool's cost removed ~12 % of
total cycles, which — with icache and dispatch-table side effects —
is the reported win. The mechanism was: *uniform tax × large
addressable share*.

A second tier can plausibly repeat the **mechanism-level** win:
JSC's DFG runs ~2× its template baseline suite-wide (per-bytecode
1.71 ns → 0.35 ns), TurboFan similar on compute suites. But it
cannot repeat the **end-to-end** win on RabbitMQ, because the
addressable share is now 12 %: a full 2× on *all* Erlang code
yields ~6 % end-to-end, and even an infinitely fast tier caps at
~14 %. There is also no remaining uniform tax of interpreter-
dispatch magnitude: T1 already register-backs the hot X registers,
and the OTP 25+ `Type` chunk already eliminated the intra-module
type-check tax. What remains is boundary-bound (calls, funs,
module edges) and shape-bound (binaries, maps, floats) — real, but
harvested per-shape, not per-instruction.

The place where "another BeamAsm-sized win" *is* available is the
class where Erlang code still owns the cycles: kernels and
compute applications (60–90 % addressable), where a 2×-class tier
result translates to 20 %+ end-to-end — plus the GC pool, which is
the one pool that grew back after BeamAsm (allocation got faster;
collection didn't).

Phase M0 (§ [`06_phases.md`](06_phases.md)) re-derives these numbers
on a broadened corpus (MongooseIM, an Ecto-heavy app, the Elixir
compiler, Graviton hardware) before the build starts; if the class
boundaries move, the targets move with them.

### 1.2 Relationship to the loop-tier plan

This plan **supersedes**
[`../T2/08_v1_loop_tier.md`](../T2/08_v1_loop_tier.md) as the
authoritative direction. The loop tier is absorbed intact as the
first shippable milestone (phase P2 in
[`06_phases.md`](06_phases.md)): everything 08 validated — the
state-preservation model, prologue-patch install, re-call deopt,
SSA-from-loaded-BEAM, the binary scan subset — is built exactly as 08
specifies, and this plan adds the phases 08 shelved, in an order
that honors 08's governing rule: *no infrastructure lands ahead of a
validated win that needs it*. The reference designs
[`../T2/00_overview.md`](../T2/00_overview.md)–[`07`](../T2/07_delivery.md)
remain the detailed specs for the machinery this plan un-shelves;
sections are cited precisely rather than restated.

What changes versus 08 is the *destination*: 08 stopped at the loop
tier and treated general inlining as indefinitely shelved on the
G3-2/G3-1 null results. This plan reads those nulls narrowly — as
they were written (§2.2) — and builds the general tier on the pools
the nulls do not touch.

## 2. Evidence inheritance

### 2.1 What is measured and stands

From the completed experiment program
([`../T2/08_v1_loop_tier.md`](../T2/08_v1_loop_tier.md) §2,
writeups in [`../verification/`](../verification/)):

| finding | number | consequence for this plan |
|---|---|---|
| MVP: loop recovery + **elimination-rich** leaf inlining | 1.97× min / 1.85× median vs T1 | the elimination-inlining mechanism works and is the model for the general inliner |
| MVP step 3 (guard fusion + registerization, `diff` still called) | ~flat vs T1 | registerization alone does not pay while a call punctures the loop body — inlining is the *enabler* |
| G-bin: match-context registerization + loop ownership + SWAR | 5.56× isolated; 6–10 % end-to-end on JSON | the largest single win class; per-byte state traffic is the one tax no OoO core hides |
| G-map: shape guard + offset loads | 1.64× isolated; wrong shape 1.45× *slower* | shape speculation works but needs runtime-recorded identity + exit counters |
| G3-2: call-overhead-only inlining | **0 ± 1 %** on real dialyzer despite proven engagement | never inline for call overhead; T1 calls are ~20 predicted cycles |
| G3-1: cold-arm pruning / dispatch specialization | 1.01–1.02× | branch counters are infrastructure, not a win source |
| Real-app profiles | [JIT] = 12.2 % (RabbitMQ) / 20.7 % (Bandit); GC 9.5–12.4 % everywhere | the Amdahl table of §1.1 |
| Allocation profiles | diffuse: no allocator > ~4 % once a framework is in the path; 387–1 750 MB/s garbage | allocation elimination must be a *general* optimization, not a spot fix |
| GC survival | 0.18 (RabbitMQ) / 0.39 (Bandit) — most garbage dies young | minor-GC cost tracks allocation volume; eliminating allocations converts directly into GC time |

### 2.2 What the nulls do and do not constrain

G3-2's own conclusion, quoted from
[`../verification/G3_OUTCOME.md`](../verification/G3_OUTCOME.md):

> "This experiment measured inlining's **floor**: the subject has
> nothing to *eliminate* across the call boundary… Inlining's real
> payoff is the optimization it unlocks — duplicate type tests
> eliminated, constants propagated into the callee, dead branches
> dropped — and the MVP already demonstrated that case."

The nulls therefore constrain the inliner's *policy*, not its
existence: **inline only where the optimizer can prove something is
eliminated** — subsumed guards, propagated constants (literal funs
above all), sinkable allocations, hoistable invariants. The
mandated cross-module inlining is built exactly under this policy
(§ [`03_optimizer.md`](03_optimizer.md) §2). What has *never* been
measured — and is priced in M0 before the general-inlining phase is
funded — is the cycle share of elimination-rich cross-module call
boundaries in real hot code
([`../verification/G3_OUTCOME.md`](../verification/G3_OUTCOME.md)
flags exactly this as the open question).

### 2.3 The HiPE calibration — why this is not HiPE again

The strongest historical caution comes from OTP's own past: HiPE —
a whole-function optimizing native compiler with real register
allocation — delivered **≈0 % over BeamAsm on dialyzer** ("dialyzer
runs roughly as fast with the JIT as it did with HiPE", [The Road
to the JIT](https://www.erlang.org/blog/the-road-to-the-jit/)), and
on program-like benchmarks of its era managed 1.07–1.36× over the
*interpreter* while numeric kernels got 1.9–3.7×. T1 has since
closed most of what HiPE exploited: on aarch64 it register-backs
six X registers plus eight float registers with a 16-entry
register cache, and the OTP 25+ `Type` chunk already eliminates
the intra-module type-check tax
(the T1 tax inventory in [`06_phases.md`](06_phases.md) M0
references the full analysis). "Compile the same code better" is a
harvested field.

This plan's case rests on the four things HiPE did **not** have,
each attached to a pool HiPE could not touch:

1. **Profile-guided speculation** — HiPE compiled statically;
   T2 speculates on observed types/targets/shapes with deopt,
   reaching facts no static compiler can prove.
2. **Cross-module inlining** — HiPE was module-local by
   construction. The residual type-check and call-convention taxes
   cluster precisely at module/fun boundaries (the intra-module
   ones are already gone), and only inlining reaches them.
3. **Allocation elimination** — HiPE never did escape analysis;
   the GC pool (9.5–12.4 % everywhere) was invisible to it.
4. **Data-access fusion** (binaries/maps) — the bit-syntax and map
   instruction sets HiPE-era code barely exercised are today's
   measured 5.6×/1.64× pools.

Equally, the arithmetic warning from the same history: dynamic
instruction reduction is not time on 8-wide OoO cores (G3-2:
engagement proven, instructions removed, **0 ± 1 %** time). Every
gate in this plan is a cycle measurement on real workloads, never
an instruction count.

## 3. The thesis: three pillars

The tier's ≥20 % on compute-bound code is carried by three
mutually-reinforcing mechanisms, each anchored to a measured pool.
Cross-module inlining is not a pillar by itself: it is the **enabler
that all three pillars share**, which is why it is mandatory
infrastructure even though its isolated effect measured zero.

### 3.1 Pillar 1 — Fused data-access loops (measured: 5.6×/1.64×/2×)

The loop tier's win class, inherited from 08 and expanded: binary
scan fusion (match-context registerization, loop ownership, SWAR
unrolling), map shape specialization, tuple/list loop-carried state
in registers. The corpus numbers say this pool is broad: 46 % of
RabbitMQ's loop functions touch `bs_*` ops. Inlining widens the
pool: helper loops behind `lists:*`/`Enum` wrappers and
cross-module parser helpers only become fusable once the boundary
is dissolved. LICM and unrolling
([`03_optimizer.md`](03_optimizer.md) §§4–5) are the loop-tier
optimizations generalized: guard hoisting out of recovered loops,
`test_heap` coalescing across unrolled iterations, the SWAR recipe
library for byte-class loops.

### 3.2 Pillar 2 — Elimination-rich inlining (measured: the MVP's 2×)

The general form of what the MVP hand-built: at profiled-monomorphic
call sites (the [`../T2/02_profiling.md`](../T2/02_profiling.md)
§7.5 target slots), inline the callee **when the elimination score
clears a threshold** — callee entry guards subsumed by caller facts,
constant arguments (literal funs make `Enum`/`lists` pipelines
collapse), return-value destructuring fused with callee construction.
This is where the `{ok, X}` construct-then-deconstruct tax dies, and
it is the enabler for pillar 3: an allocation can only be sunk when
both its construction and its consumption are visible in one
compilation unit. Elixir is the strategic beneficiary: its `Enum`
pipelines are cross-module by construction, which is precisely the
case 08 shelved and this plan reinstates.

### 3.3 Pillar 3 — Allocation elimination (the unpriced 10–12 %)

The one VM-internal pool a JIT *can* reach: GC is 9.5–12.4 % of busy
on every profiled workload, garbage volume is 387 MB/s (RabbitMQ) to
1 750 MB/s (Bandit), and survival rates of 0.18–0.39 mean minor-GC
cost scales with allocation volume. Escape analysis + allocation
sinking + float unboxing
([`03_optimizer.md`](03_optimizer.md) §6) attack the volume:
tuples/conses built and consumed inside one fused region are never
materialized; boxed floats in numeric loops live in FP registers.
Because real-app allocation is **diffuse** (GALLOC's central
finding), this must be a general optimization applied everywhere the
tier compiles — which is exactly what escape analysis is, and
exactly what no spot-fix (Track A-style) alternative can capture.
BEAM semantics are unusually favorable: terms are immutable (no
aliasing writes), sends/ETS/exceptions are crisp escape points, and
the compiler already tracks types. Sizing this pool per-app is an M0
deliverable (the idea-#50 term-lifetime instrumentation from the
otp-ideas repo is the designed-for tool); the pillar carries its own
gate before its build phase is funded.

### 3.4 What is deliberately *not* a pillar

- **Call-overhead removal and dispatch specialization** — measured
  null (G3-2, G3-1). The inliner's scoring function explicitly
  assigns them zero value.
- **Uniform registerization without fusion** — MVP step 3 measured
  flat. Register allocation pays *inside* fused regions, not as a
  freestanding win on call-punctured code.
- **VM-internal pools** (message/signal path, ETS, send-copy) — a
  JIT cannot reach them; they stay with the separate VM-internal
  track flagged in 08 §8.

## 4. The 20 % arithmetic, per pillar

Compute-bound class, using dialyzer's profile as the stress case
(hottest function family ~12 % of busy, long tail ≤2.5 % each —
i.e. the *hard* case for a spot optimizer and the motivating case
for a general one):

- Pillar 1 on the loop/scan share, pillar 2 on the
  elimination-rich boundaries, pillar 3 on allocation-heavy
  construction code: the tier needs a **~1.28× aggregate on
  emulator execution**. The MVP's 1.85–1.97× on its shape, G-bin's
  5.6× on scans, and HotSpot/TurboFan-class priors for
  profile-guided inlining + escape analysis on the remainder make
  1.3× aggregate credible *if* eligibility (the fraction of hot
  code the tier compiles rather than side-exits) is high. That makes
  **eligibility coverage the load-bearing engineering metric**, and
  it is gated per phase in [`06_phases.md`](06_phases.md).
- Serialization services: 20.7 % [JIT] share × pillar-1/2 wins
  (JSON decode is the measured 6–10 % compound) + GC share × pillar
  3 = the ≥10 % target, with both terms measured separately at the
  P5 gate.
- Brokers: whatever pillars 1–3 yield on 12 % — honest single
  digits.

The plan's core discipline, inherited from the experiment program:
every pillar keeps a *measured pool* attached to it, every phase
ends at a gate that re-measures the pool it claimed, and the first
phase (M0) exists to price the two pools this section could not:
elimination-rich boundary share and sinkable-allocation share.
