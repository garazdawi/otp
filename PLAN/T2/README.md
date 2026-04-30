# T2 — A Second-Tier Optimizing JIT for Erlang on aarch64

This is the project plan. Read [`../research/`](../research/) first if
you want the design rationale; these files are the concrete proposal.

The plan is structured as a *state-preservation* model with a
*speculate-range constraint*. The shape comes from the
[otp-ideas/IDEAS/21-jit-second-tier.md] precursor and the alternatives
surveyed in `../research/`. The MVP that demonstrates this can beat
T1 in practice lives in [`../T2_mvp.md`](../T2_mvp.md), with the
implementation status in [`../mvp/STATUS.md`](../mvp/STATUS.md).

## Index

| File | Sections | What's in it |
|------|----------|--------------|
| [`00_overview.md`](00_overview.md) | §§1–4 | Goals and non-goals, why T2 (with alternatives), the architecture diagram, and the prior-art-vs-departures choices. Start here. |
| [`01_ir_and_state.md`](01_ir_and_state.md) | §§5–6 | The T2 SSA IR (op categories, type lattice, where types come from, why not reuse BEAM SSA records) and the sync-point-based state-preservation model. |
| [`02_profiling.md`](02_profiling.md) | §7 | T1 profiling changes that produce the data T2 consumes — eligibility, type feedback vector, monomorphic-target slot, map-shape feedback, branch-frequency counters, SSA-in-BEAM-file. |
| [`03_compilation_and_speculation.md`](03_compilation_and_speculation.md) | §§8–9 | The T2 compilation pipeline (pass list, ordering rationale, abort policy) and speculation/guards/deopt (outer + inlined-region deopt with eager-CP-push, the deopt-at-sync-point constraint, the one-untag arithmetic trick, recompilation policy, funs). |
| [`04_optimization.md`](04_optimization.md) | §§10–11 | Inlining strategy (what we inline in v1, the higher-order-intrinsic annotations, loop recovery/unrolling, DOMJIT-style guard BIFs) and code generation (direct asmjit emission, sync-point-constrained register allocation). |
| [`05_runtime.md`](05_runtime.md) | §§12–15 | How T2 lives in the running VM: calling convention, GC, tracing, code cache and lifecycle, module reload + watchpoint invalidation, tier-up triggers. |
| [`06_dispatch_and_sideexit.md`](06_dispatch_and_sideexit.md) | — | Concrete mechanics of how T2 code is *installed* into a function (NIF-style prologue patch — single 4-byte store at L_f+4 catches both external and intra-module callers, generation-counter check, in-flight callers), how it is *uninstalled* again (revert the patched `b`, tombstones, lazy stack scan, refcounting), and exactly how each side-exit category works (outer/inlined/range/GC). Grounded in the MVP code. |
| [`07_delivery.md`](07_delivery.md) | §§16–19 + appendices | Observability hooks, testing strategy, implementation phases, risks and open questions, out-of-scope, and the file-layout / effort-estimate appendices. Appendix C consolidates critique resolutions. |

## Reading paths

- **Reviewing the design**: read in order, skipping
  [`07_delivery.md`](07_delivery.md) on a first pass.
- **Sanity-checking a specific decision**: jump to the relevant file
  by topic. Most cross-section references in the text are written as
  e.g. "§9.4" — those are now in
  [`03_compilation_and_speculation.md`](03_compilation_and_speculation.md).
- **Catching up after a break**: read [`00_overview.md`](00_overview.md)
  + the implementation status in
  [`../mvp/STATUS.md`](../mvp/STATUS.md). That's enough to ground
  most discussion.
- **Wanting the concrete install/side-exit mechanics**: jump to
  [`06_dispatch_and_sideexit.md`](06_dispatch_and_sideexit.md). It's
  grounded in actual MVP code and pins down the questions the
  design-level files leave abstract.

## Status

The previous unified `T2.md` has been split into the files above.
The pipeline file was further split (profiling vs. compilation
+ speculation), and a new
[`06_dispatch_and_sideexit.md`](06_dispatch_and_sideexit.md) was
added to nail down install/uninstall and side-exit mechanics. The
second-pass critique that drove most of the recent design decisions
has been merged into the relevant section files; the critique itself
is archived as `../T2_critique_v2.md`.
