# T2-Full — Runtime Integration

> Almost entirely inherited. This file is the delta map: what
> applies as-written, what re-activates at which phase, and the two
> genuinely new runtime obligations.

## 1. Applies as written (no delta)

- **Calling convention, GC discipline, reductions/yield, trap-out**:
  [`../T2/05_runtime.md`](../T2/05_runtime.md) §12 — T2 code is
  T1-shaped at every observable boundary.
- **Install/uninstall**: prologue patch under `code_ix` lock,
  co-located allocation with bridge-pool fallback, in-flight
  callers finish naturally, six install preconditions —
  [`../T2/06_dispatch_and_sideexit.md`](../T2/06_dispatch_and_sideexit.md)
  §§1–3, §5.
- **Code cache + eviction**: separate evictable `JitAllocator`
  region, single compile writer, usefulness-decay eviction —
  [`../T2/05_runtime.md`](../T2/05_runtime.md) §13.
- **Tier-up trigger + compile queue**:
  [`../T2/05_runtime.md`](../T2/05_runtime.md) §15 with the App C
  I5 calibrations; C-side MPSC queue + one dirty-CPU worker until
  scheduling policy outgrows it.
- **Back-edge yield-resume stubs**:
  [`../T2/08_v1_loop_tier.md`](../T2/08_v1_loop_tier.md) §4.5 —
  promoted to v1 there, kept for all phases here (long-running
  loops must not forfeit the tier at every timeslice).
- **Self-correction**: exit counters, loop-weighted jettison
  budgets, widen-or-drop recompiles
  ([`../T2/03_compilation_and_speculation.md`](../T2/03_compilation_and_speculation.md)
  §9.5).

## 2. The granted simplification, spelled out

**Hot code loading and tracing force deoptimization.** Concretely:

- Module reload/purge: watchpoint fires → every dependent blob
  (root module *or* any inlined callee's module — the reverse
  index of [`../T2/05_runtime.md`](../T2/05_runtime.md) §14) is
  jettisoned before the load commits. Loader-notifies-server
  ordering and the in-flight compile generation check (App C M6)
  as designed.
- Trace enablement (`trace_pattern`, breakpoints, NIF load on a
  traced function): strict mutual exclusion on the prologue —
  trace always wins, jettison-then-trace, ordered before the
  breakpoint commit via the existing code-barrier machinery
  (verified against `erl_bif_trace.c` in
  [`../verification/RESULTS.md`](../verification/RESULTS.md) §1).
- No T2 code survives either event; no translation of live frames
  beyond the `c_p->i` fix-ups already designed. This deletes the
  hardest lifecycle rows wholesale.

## 3. Re-activates at P3 (with CPs into blobs)

General inlining pushes CPs into T2 blobs (eager-CP-push), so the
full lifecycle returns exactly as designed:

- **Tombstone tables + lazy stack scan**:
  [`../T2/06_dispatch_and_sideexit.md`](../T2/06_dispatch_and_sideexit.md)
  §§5.3–5.5 — per-process scan generation, 99 %+ single-compare
  schedule-in path, bounded scan depth with continuation
  trampoline, high-water proactive sweep, freeing gated on
  thread-progress + all-scanned watermark.
- **Per-blob CP→T1-PC side tables** sized by inlining depth.
- **The trace-path own-stack scan** that general inlining drags in
  ([`../T2/08_v1_loop_tier.md`](../T2/08_v1_loop_tier.md) §9 shelf,
  row 2) — budgeted as part of P3, not discovered during it.

Until P3, the loop-tier rule holds (no CPs into blobs, O(1)
uninstall) — one more reason the phases are ordered as they are.

## 4. New runtime obligations in this plan

1. **PC→MFA+line registration for blobs** and the T1 PC side table
   (function entries, call ops, post-call continuations,
   post-effect boundaries) — already identified as bounded ERTS
   changes ([`../T2/08_v1_loop_tier.md`](../T2/08_v1_loop_tier.md)
   §4.5); P3 extends the table to every inlined-call boundary the
   framestates reference. The full per-instruction table remains
   unnecessary (deopt only targets sync points).
2. **Rematerialization support (P5b only)**: deopt stubs that
   allocate — materializing sunk objects needs heap space on the
   deopt path. Design: reserve worst-case words in the stub via
   the standard GC test, then build the objects; this is the one
   place T2 allocates outside compiled code. If P5b is never
   funded (P5a suffices), this obligation vanishes.

## 5. Observability

Inherited surface ([`../T2/07_delivery.md`](../T2/07_delivery.md)
§16): `erlang:t2_stats/0`, `erlang:t2_info/3`, `+JT2*` flags,
`jit_t2_code` memory key, perf/gdb metadata via the existing
`beamasm_register_metadata` path. Additions:

- **`eligibility` in t2_stats**: share of profiled hot cycles
  executing in T2 vs side-exited vs never-compiled — the plan's
  load-bearing metric ([`00_goal_and_thesis.md`](00_goal_and_thesis.md) §4).
- **Per-blob allocation counters** (P5): words allocated vs T1
  baseline, feeding the pillar-3 gate.
- **Inlining introspection**: `erlang:t2_info(M, F, A)` reports the
  inline tree (what got fused into this blob and why — score
  terms), because debugging a cross-module inliner without this is
  archaeology.

The compatibility contract and inspection matrix
([`../T2/08_v1_loop_tier.md`](../T2/08_v1_loop_tier.md) §5) apply
to every phase gate: no observable difference except speed, with
the one documented residual (`current_function` inside an inlined
region reports the root — same as `erlc +inline` today).
