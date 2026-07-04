# T2-Full — Architecture

> How the tier sits in the VM. Nearly everything here is inherited
> from the validated reference designs; this file states what is
> inherited unchanged, what is staged (present in later phases
> only), and the handful of genuinely new decisions. Precise specs
> live in the cited sections of [`../T2/`](../T2/README.md).

## 1. Inherited unchanged (the validated core)

These carry over from the loop-tier plan and reference design
verbatim; the general tier does not touch them:

- **The sync-point state-preservation invariant**
  ([`../T2/01_ir_and_state.md`](../T2/01_ir_and_state.md) §6): T2
  code matches T1's abstract machine state (X/Y, HTOP, FCALLS,
  stack) at function entry, calls, returns, GC sites, BIF
  boundaries, guards, receive safe points; registers are free
  between sync points. Every mechanism below — deopt, GC, yield,
  jettison — is bounded by this invariant. The MVP validated it;
  the identity-transform suite (full OTP test suite through a
  no-optimization T2 pipeline,
  [`../T2/07_delivery.md`](../T2/07_delivery.md) §16A) is its
  permanent regression harness.
- **Install/uninstall via the prologue patch**
  ([`../T2/06_dispatch_and_sideexit.md`](../T2/06_dispatch_and_sideexit.md)
  §§1–3): a single 4-byte store at `L_f+4` under the `code_ix`
  write lock catches every caller kind, including intra-module
  direct branches; the T1 body stays intact as the side-exit
  landing zone; strict mutual exclusion with trace/NIF prologues,
  trace always wins.
- **SSA IR reconstructed from loaded BEAM code** — no SSA chunk, no
  recompilation, works on every existing `.beam` including Elixir
  ([`../T2/08_v1_loop_tier.md`](../T2/08_v1_loop_tier.md) §4.1);
  the archived SSA-chunk design
  ([`../T2/02_profiling.md`](../T2/02_profiling.md) §7.8) remains
  the G1-gate fallback.
- **Calling convention and GC discipline**
  ([`../T2/05_runtime.md`](../T2/05_runtime.md) §12): T2 code is
  callable as if it were T1; the GC walker is unchanged; untagged
  values never live across sync points.
- **The compatibility contract**
  ([`../T2/08_v1_loop_tier.md`](../T2/08_v1_loop_tier.md) §5): no
  observable difference vs T1 except speed — T2 never raises,
  reductions are counted identically, stack introspection is
  byte-identical, and the inspection matrix is a deliverable, not
  an aspiration.
- **Code cache, tier-up trigger, JIT compile queue**
  ([`../T2/05_runtime.md`](../T2/05_runtime.md) §§13, 15):
  separate evictable region, single compile writer,
  `1000·sqrt(size+1)·2^recompiles·M/(M−U)` threshold with profile
  stability checks.
- **Self-correction**: per-blob exit counters with loop-weighted
  jettison budgets and recompile-with-widened-speculation
  ([`../T2/03_compilation_and_speculation.md`](../T2/03_compilation_and_speculation.md)
  §9.5).

## 2. The deopt ladder — the one architectural axis that moves

Deopt capability is the single biggest cost driver in the design
space, so the plan stages it in three rungs, each funded only when
the optimizations that need it clear their gates:

**Rung 1 — re-call deopt only** (phases P0–P2; exactly 08 §4.2).
Every side exit reconstructs a valid argument vector at a call
boundary and branches to a T1 address. No framestates, no CPs into
T2 blobs, O(1) uninstall, no stack scans. Legality rule:
guards-before-effects within each re-execution window. This rung
carries the loop tier (self-recursive loops, intrinsics with
literal funs, leaf inlining — the callee's state *is* the caller's
iteration state).

**Rung 2 — framestates + eager-CP-push** (phase P3, with general
inlining). The inlined-region machinery of
[`../T2/03_compilation_and_speculation.md`](../T2/03_compilation_and_speculation.md)
§9.2 and [`../T2/01_ir_and_state.md`](../T2/01_ir_and_state.md)
§§5.1–6.5: codegen-only framestates with `parent_fs` chains record
the would-be X/Y state per inlined boundary; the inliner pushes the
parent CP eagerly (~1–2 instructions per inlined call), making
every deopt stub uniform regardless of nesting depth — 5–10 restore
moves plus a branch to the T1 PC. Because blobs now contain CPs,
the tombstone + lazy-stack-scan lifecycle
([`../T2/06_dispatch_and_sideexit.md`](../T2/06_dispatch_and_sideexit.md)
§§5.3–5.5) returns with it: per-process scan generation, bounded
scan depth, high-water sweeps. This is the rung the mandated
cross-module inlining pays for; its cost is why P3 has a
gate in front of it, not behind it.

**Rung 3 — framestate virtuals** (phase P5b, with full escape
analysis). Framestates additionally describe *virtual objects* —
allocations the optimizer sank — so a deopt can rematerialize them
(HotSpot scalar-replacement / PyPy virtuals model). Rung 3 is
optional: phase P5a ships the deopt-dead subset of allocation
sinking (sink only where the value is provably dead on every deopt
path), which needs no rematerialization at all; P5b is funded only
if M0/P5a measurement shows the residual pool justifies it.

**The granted simplification.** Hot code loading and tracing force
deoptimization, full stop: module-watchpoint or trace enablement
jettisons every dependent blob (revert prologue, thread-progress
sync, tombstone if rung ≥2). No cross-upgrade code survival, no
trace-compatible T2 code. This removes the hardest lifecycle cases
from every rung.

## 3. Compilation unit and tier-up

**Unit: a root function plus its profile-selected inline closure.**
The tier compiles method-at-a-time (the Sista/HotSpot shape, not
tracing — [`../research/`](../research/) settled this): a root
function whose counter tripped, into which the inliner folds
callees selected by the elimination-scoring policy
([`03_optimizer.md`](03_optimizer.md) §2) using the
monomorphic-target profile slots. Loop recovery runs *after*
inlining, so an inlined `lists:foldl/3` + literal fun becomes a
flat loop in the root's IR
([`../T2/04_optimization.md`](../T2/04_optimization.md) §10.5).
Tier-up target selection prefers compiling the dominant *caller*
(≤2 levels up, [`../T2/07_delivery.md`](../T2/07_delivery.md)
App C M1) so helpers are seen in their inlining context rather than
compiled standalone.

**Function-entry tier-up only; no OSR-entry.** Erlang has no
intra-function loops at the BEAM level — every loop is a
(tail-)call through a function entry, so the prologue patch catches
iteration N+1 of any hot loop; there is no "stuck in a cold frame
for a billion iterations" case that forces V8/HotSpot-style
OSR-entry state mapping. The one long-residency case — a process
that yields mid-loop inside T2 — is handled by 08's back-edge
resume stubs (yield state saved as a fresh-call argument vector,
resume re-enters T2; [`../T2/08_v1_loop_tier.md`](../T2/08_v1_loop_tier.md)
§4.5). This is a permanent architectural decision, revisited only
if a measured workload shows a hot region unreachable through any
function entry.

**Compile budget**: ~1 ms median, 10 ms hard cap per unit, abort →
stay in T1
([`../T2/03_compilation_and_speculation.md`](../T2/03_compilation_and_speculation.md)
§8.1). General inlining raises median unit size; the budget is a
gate metric from P3 on, and the backend choice
([`04_backend.md`](04_backend.md)) is constrained by it.

## 4. Pipeline overview

```
        T1 (BeamAsm)                          T2 compile job (dirty-CPU worker)
  ┌───────────────────────┐             ┌─────────────────────────────────────┐
  │ profiling side tables │──tier-up──▶ │ BEAM code → SSA IR (loader-retained │
  │  · call counters      │   queue     │   chunk) + Type-chunk seeding       │
  │  · entry/site types   │             │ profile overlay (types, targets,    │
  │  · mono-target slots  │             │   shapes, branch weights)           │
  │  · map-shape slots    │             │ inline (elimination-scored,         │
  │  · branch counters    │             │   cross-module, framestates)        │
  └───────────────────────┘             │ speculation insertion → re-infer    │
        ▲          │                    │ GVN/CSE/const-fold/DCE              │
        │          │ side exits,        │ loop recovery → LICM → unroll/SWAR  │
        │          │ deopt, jettison    │ escape analysis → allocation sink   │
        │          ▼                    │ lowering → regalloc → emit → install│
  ┌───────────────────────┐             └─────────────────────────────────────┘
  │ T1 body (intact) =    │
  │ side-exit landing zone│   blob lifecycle: watchpoints (module reload),
  │ + deopt floor         │   trace-jettison, eviction, exit-counter recompile
  └───────────────────────┘
```

Pass ordering rationale and the abort policy are inherited from
[`../T2/03_compilation_and_speculation.md`](../T2/03_compilation_and_speculation.md)
§8; the new passes (GVN as an explicit pass, escape
analysis/sinking, float unboxing) slot in as specified in
[`03_optimizer.md`](03_optimizer.md).

## 5. New decisions in this plan (delta vs 08 + reference design)

1. **General inlining returns**, gated: rung 2 of the deopt ladder
   plus the tombstone lifecycle are funded at P3 after M0 prices
   the elimination-rich boundary pool (this is the plan's central
   bet, and it is measured before it is built).
2. **Escape analysis / allocation sinking / float unboxing are
   first-class phases** (P5) with their own profiling support and
   gates — they were "v2, no measured pool yet" in the reference
   design; the GC/allocation evidence
   ([`00_goal_and_thesis.md`](00_goal_and_thesis.md) §3.3) makes
   them a pillar.
3. **Branch counters and map-shape slots ship as infrastructure**
   in P1 profiling (cheap, per
   [`../T2/02_profiling.md`](../T2/02_profiling.md) §§7.6–7.7) but
   carry no win claims — they serve block layout, inline scoring,
   and the shape-guard placement policy.
4. **The backend is chosen by evaluation, not incumbency**:
   [`04_backend.md`](04_backend.md) carries the matrix; the IR is
   designed backend-portable regardless of the outcome.
5. **Eligibility coverage becomes a tracked metric** (share of hot
   cycles executing in T2 vs side-exited), reported by
   `erlang:t2_stats/0` and gated per phase — the 20 % arithmetic
   depends on it more than on any single optimization
   ([`00_goal_and_thesis.md`](00_goal_and_thesis.md) §4).
