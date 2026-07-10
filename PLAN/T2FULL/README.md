# T2-Full — The Profile-Driven Optimizing Tier

> ## ⟹ LANDING (2026-07-08): T2-Full is a SPECIALIST tier — see [`15_scope_and_disposition.md`](15_scope_and_disposition.md)
>
> The measurement program concluded. The **"≥20 % on most applications"
> mandate below is superseded** — it is measured-dead (marginal ~3–6 % on
> analysis/compiler, ~0 %/ineligible on services; rung-2/P3 rescues neither).
> **Owner decision: consolidate as a specialist tier** — 2.5–3.1× on byte
> scan-and-count kernels + integer/float tail loops, never-slower-than-T1 floor
> elsewhere. **P3–P7 are decided-against**, retained below for the evidence
> record only. Start at [`15`](15_scope_and_disposition.md) for the conclusion;
> the plan text below (00–09) is the historical plan-of-record and the mechanism
> specs. Evidence trail: memos [`10`](10_p26_install_gate.md)–[`14`](14_real_service_value.md).

**The authoritative T2 plan** (July 2026). Supersedes
[`../T2/08_v1_loop_tier.md`](../T2/08_v1_loop_tier.md) as the
project direction: the loop tier is absorbed as the first shippable
milestone (phases P0–P2), and the general-optimization phases that
08 shelved are reinstated — evidence-gated — on top of it. The
reference designs [`../T2/00`](../T2/00_overview.md)–[`07`](../T2/07_delivery.md)
remain the detailed mechanism specs and are cited throughout rather
than restated.

Mandate: profile data from T1 drives a second tier with
**cross-module inlining, LICM, loop unrolling, and the classic
optimization repertoire**, targeting **≥20 % on most applications**.
Granted simplifications: hot code loading and tracing force
deoptimization (jettison; no survival across either). aarch64
first. Backend open — evaluated in [`04_backend.md`](04_backend.md).

## Index

| file | what's in it |
|---|---|
| [`00_goal_and_thesis.md`](00_goal_and_thesis.md) | The goal made precise per application class (with the Amdahl arithmetic that forces the per-class statement), the "why can't T2 repeat BeamAsm's RabbitMQ win" analysis, the HiPE calibration ("why this is not HiPE again"), what the G3 nulls do/don't constrain, and the three-pillar thesis: fused data-access loops, elimination-rich inlining, allocation elimination — with cross-module inlining as the shared enabler. |
| [`01_architecture.md`](01_architecture.md) | What's inherited unchanged (sync-point state model, prologue-patch install, SSA-from-loaded-BEAM, compatibility contract), the three-rung **deopt ladder** (re-call → framestates+eager-CP → framestate virtuals) that staged-funds the expensive machinery, compilation unit + function-entry-only tier-up (why Erlang needs no OSR-entry), pipeline diagram, and the five deltas vs 08. |
| [`02_profiling.md`](02_profiling.md) | The full T1 profiling design un-shelved (entry + interior type feedback, monomorphic-target slots with frequency, map shapes, branch counters), staged with its consumers under the ≤1 % budget; plus the M0 allocation-lifetime measurement plan (idea #50 tooling). |
| [`03_optimizer.md`](03_optimizer.md) | The pass pipeline and each mandated optimization: the **elimination-scored cross-module inliner** (never for call overhead — G3-2 is a design input; calibrated against HotSpot/Maglev/JSC budgets, Graal deep-trials and Flambda evidence), GVN/CSE on immutable terms, LICM with speculative guard motion, unrolling + SWAR, and the escape-analysis/allocation-sinking/float-unboxing group with its two-stage deopt design and PyPy/LuaJIT/Graal calibration. |
| [`04_backend.md`](04_backend.md) | Backend evaluation matrix (asmjit+own mid-end, LLVM ORC, Cranelift, MIR, others) and the decision: **asmjit + BEAM-specific mid-end**, HIR/LIR hard seam, linear-scan-on-SSA regalloc — with the B3 structural argument and July-2026 evidence. |
| [`05_runtime.md`](05_runtime.md) | Delta map onto the inherited runtime integration: what applies as written, the code-load/trace jettison contract spelled out, what re-activates at P3 (CPs into blobs → tombstone lifecycle), the two new runtime obligations, observability additions (eligibility metric, inline-tree introspection). |
| [`06_phases.md`](06_phases.md) | The roadmap: **M0 measurement phase** (prices the two pools the thesis rests on) → P0–P2 loop tier (shippable) → P3 general inlining → P4 classic opts → P5 allocation elimination → P6 ship → P7 x86_64 port. Gates, effort (~60–68 weeks aarch64), risks, open questions. **Top banner = PROGRAM STATUS (consolidating as specialist tier); P3+ decided-against.** |
| [`10`](10_p26_install_gate.md)–[`14`](14_real_service_value.md) | **The measurement/de-risk memos** that drove the landing: P2.6 install gate (10), body-recursion prize (11), rung-2 correctness spike GO (12), rung-2 value FALLS SHORT (13), real-service value ~0 %/SPECIALIST (14). |
| [`15_scope_and_disposition.md`](15_scope_and_disposition.md) | **⟹ START HERE for the conclusion.** What T2-Full actually is (measured), what it does not deliver and why, the hole-free install gate, and the disposition (specialist tier; P3+ decided-against). |
| [`16_retrospective.md`](16_retrospective.md) | **After-action review.** What could have been done cheaper/better (technical + process), the verified eligible-opcode set (why services are ineligible), latent risks still in the tree, and the prioritized closeout (P0 harden / P1 strip dead weight / P2 disposition — Option A / P3 redirect ambition to the GC/VM-internal track). |
| [`17_more_opcodes_value_census.md`](17_more_opcodes_value_census.md) | **"Should we implement more opcodes to find the service value?" — no, measure first.** What memo 14 measured vs. argued; why building the opcodes is the wrong experiment (entangled/non-stageable classes, eligible≠win, the map win needs an unbuilt profiler); the cheap **frontend-only addressable-share census** that prices the pool in days; the byte-slinging corpus gap (add a map/term-heavy service); build/no-build decision rule. |

## Reading paths

- **Deciding whether to fund this**: [`00`](00_goal_and_thesis.md)
  then [`06`](06_phases.md).
- **Starting implementation**: [`06`](06_phases.md) M0/P0, with
  [`../T2/08_v1_loop_tier.md`](../T2/08_v1_loop_tier.md) as the
  loop-tier spec and [`01`](01_architecture.md) for what changed
  around it.
- **Reviewing the optimizer design**: [`03`](03_optimizer.md) +
  [`04`](04_backend.md).

## Status

**CONSOLIDATING AS A SPECIALIST TIER (2026-07-08).** M0–P2 built and
measured (loop tier, sched-1 profiling, async compile, install gate —
now hole-free, `cf33c4cee8`). The M0/de-risk program ran the missing
measurements *before* funding the expensive phases and found the 20 %
thesis unreachable (marginal on analysis, ineligible on services,
rung-2 rescues neither), so **P3–P7 are decided-against.** The plan's
central discipline — measure the pools before funding the phases that
exploit them — did its job: it retired the general-tier ambition on
evidence and saved the 12–19-week rung-2 build. See
[`15_scope_and_disposition.md`](15_scope_and_disposition.md) for the
disposition and [`06_phases.md`](06_phases.md) for the phase-by-phase
status. Remaining closeout: the definitive scope memo (this =
[`15`](15_scope_and_disposition.md), done) and a final default-config
confirmation.
