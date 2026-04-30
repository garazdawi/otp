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
| [`02_pipeline.md`](02_pipeline.md) | §§7–9 | The data-flow story: profiling collects feedback in T1, the compilation pipeline consumes it in T2, speculation/guards/deopt close the loop. |
| [`03_optimization.md`](03_optimization.md) | §§10–11 | Inlining strategy (what we inline in v1, the higher-order-intrinsic annotations, loop recovery/unrolling, DOMJIT-style guard BIFs) and code generation (direct asmjit emission, sync-point-constrained register allocation). |
| [`04_runtime.md`](04_runtime.md) | §§12–15 | How T2 lives in the running VM: calling convention, GC, tracing, code cache and lifecycle, module reload + watchpoint invalidation, tier-up triggers. |
| [`05_delivery.md`](05_delivery.md) | §§16–19 + appendices | Observability hooks, testing strategy, implementation phases, risks and open questions, out-of-scope, and the file-layout / effort-estimate appendices. |

## Reading paths

- **Reviewing the design**: read in order, skipping
  [`05_delivery.md`](05_delivery.md) on a first pass.
- **Sanity-checking a specific decision**: jump to the relevant file
  by topic. Most cross-section references in the text are still
  written as e.g. "§9.4" — those are now in
  [`02_pipeline.md`](02_pipeline.md).
- **Catching up after a break**: read [`00_overview.md`](00_overview.md)
  + the implementation status in
  [`../mvp/STATUS.md`](../mvp/STATUS.md). That's enough to ground
  most discussion.

## Status

The previous unified `T2.md` has been split into the files above.
The second-pass critique that drove most of the recent design
decisions has been merged into the relevant section files; the
critique itself is archived as `../T2_critique_v2.md`.
