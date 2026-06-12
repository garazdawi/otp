# T2 — Delivery (Observability, Testing, Phases, Risks)

> **v1 phase plan superseded**: the §17 phases 0–4 and Appendix B's
> 48–50-week v1 estimate are replaced by the rescoped P0–P4 (~24–26
> weeks) in [`08_v1_loop_tier.md`](08_v1_loop_tier.md) §6, which also
> gates phases 5+ behind MVP-style experiments. §16 (observability)
> and §16A (testing strategy) carry into v1 unchanged.
>
> Part of the T2 design. See [`README.md`](README.md) for the full
> document index. This file covers §§16–19 and the appendices: the
> project-management material — observability hooks, testing
> strategy, implementation phases, risks and open questions, and
> what's explicitly out of scope.

## 16. Observability

Build observability from day one. New API `erlang:t2_stats/0`:

```erlang
#{
  t2_blobs_compiled    => N,
  t2_blobs_active      => N,
  t2_blobs_jettisoned  => N,
  t2_compile_time_us   => Total,
  t2_compile_failures  => N,
  t2_code_cache_size   => Bytes,
  t2_code_cache_used   => Bytes,
  t2_code_gc_count     => N,
  t2_deopts_total      => N,
  t2_deopts_by_reason  => #{ type_guard_fail => N, range_fail => N,
                              watchpoint => N, trace_enable => N, ... },
  t2_compile_failures_by_function => #{
      {M,F,A} => #{ reason => atom(),
                    count  => N,
                    last_failed_at => Timestamp }
  },
  ratio_in_t2          => Float
}
```

Per-function: `erlang:t2_info/3 :: (M,F,A) → tier1 | {tier2, Stats}`.
Returns counters, deopt history, current code address.

Debugging hooks:
- `+JT2trace_compiles` — log every compile (function, IR size,
  passes run, compile time, blob size).
- `+JT2trace_deopts`   — log every deopt (function, site, reason,
  observed type, in-loop flag).
- `+JT2dump_ir <Mod:Fun/Arity>` — dump optimized IR for a function.
- `+Jdump` — extended to dump T2 blobs alongside T1 (existing
  BeamAsm flag, augmented).
- Per-compile event log (configurable, default off, costs nothing
  when off): emits a structured event per compile with the same
  fields as `+JT2trace_compiles` plus speculation-site count and
  inlined-region count; same for deopts.

## 16A. Testing strategy

T2 is large enough that "the existing OTP test suite passes" is
necessary but not sufficient. Five categories of test, each with a
specific responsibility.

### 16A.1 Identity-transform correctness suite (Phase 1 onwards)

Run the full OTP test suite under `+JT2enable true`. T2 in Phase 1
performs no optimisations beyond IR build + asmjit emit, so any
behavioural difference vs. T1 is a bug in the state-preservation
model. This is the primary sanity check; failures here block
every later phase.

### 16A.2 Deopt stress tests (Phase 2 onwards)

For every `speculate_*` op T2 emits, the test harness must be able
to **force the speculation to fail**. Two mechanisms:

- A test-only BIF `t2_force_deopt(Site)` that flips a per-site
  deopt-trigger flag T2 codegen consults via a test-only emit.
- A `+JT2random_deopt P` flag that randomly fails speculations
  with probability P, used in stress runs.

Forced-deopt suites cover both outer-function deopt (jump to T1
PC) and inlined-region deopt (X/Y restore + jump). Phase 3 adds
nested-inlining variants.

### 16A.3 Watchpoint and lifecycle tests (Phase 1 onwards)

Test-only BIFs to drive the lifecycle deterministically:

- `t2_purge_blob(MFA)` — force jettison of a specific blob.
- `t2_advance_global_gen()` — bump `t2_global_gen` to exercise
  the schedule-in scan path.
- `t2_force_high_water_sweep()` — trigger the proactive sweep
  in §14.2 regardless of tombstone-set size.

Test cases stress:
- `code:purge/1` with processes mid-execution in inlined regions
  of the purged module.
- Hot-code upgrade with the §14.3 worked example.
- Trace enable/disable on T2-compiled functions and on inlined
  callees.
- Concurrent compile + purge of the same module.

### 16A.4 Concurrency stress (Phase 2 onwards)

A stress harness running:
- N processes calling tier-2-eligible functions concurrently.
- Background module reloads at random intervals.
- Tier-up triggers firing while target functions are mid-
  execution on other schedulers.
- Eviction triggered by undersized `+JT2cache`.

Detects: races in the JIT-server queue, watchpoint-table
corruption, missed-update on the export table, double-free of
tombstoned blobs.

### 16A.5 Regression bench suite (Phase 3 onwards)

A small set of benchmarks (RabbitMQ message dispatch, dialyzer
hot paths, Elixir compiler) tracked across builds. Two thresholds:

- **Hard floor (§1)**: T2 must not regress against T1 by more
  than ε (the measurement noise) on any tracked benchmark.
- **Target wins**: each benchmark has a target speedup at v1
  (set in Phase 3); shortfalls are tracked but don't block.

Phase 4's "production polish" task is largely about closing the
gap between the hard floor and the target wins on this suite.

## 17. Implementation phases

Each phase ships something measurable. Phases are sequential.

**Sequencing and integration commitments.**

- **Prototype-first.** Before committing to the full ~50-week
  v1, build a small prototype that demonstrates the core thesis:
  inlined `lists:foldl/3` with a constant fun and one
  `speculate_type` produces native code that beats T1 on a
  representative benchmark. The prototype's job is to validate
  the state-preservation model and the speculation pipeline
  end-to-end. If the prototype shows promise, the full plan is
  greenlit; if the wins evaporate at scale, we cut early.
- **All phases on a branch until v1 is done.** Phases are not
  shipped to master incrementally as separate user-visible
  features. The branch is kept rebased against master to
  minimise integration friction; we coordinate with upstream
  on any AOT-side change (the SSA chunk especially) so master-
  merges don't accumulate a single giant integration at the end.
- **No separate ship for T2.** T2 lands in master alongside the
  rest of OTP — opt-in via `+JT2enable` and gated until ready.
  The integration discipline is what keeps merge conflicts
  bounded; a long-lived branch is otherwise its own
  project-management hazard.
- **Maintainership.** T2 is owned by the OTP team after v1
  ships. The ~16 KLOC C++ optimizer plus AOT-side annotations
  is small enough to be maintainable by the existing team; the
  alternative (a separate sub-team, or external contributors)
  was the HiPE failure mode.

### Phase 0 — Plumbing (≈12 weeks)
Goal: nothing observable, but all infrastructure in place plus the
audits and measurements that gate the rest of the plan.

- **SSA-in-BEAM-file pipeline.** AOT chunk format, loader support,
  `erts_beam_get_ssa` C API, `+t2_compile_eligible` compile
  option. Roundtrip on a small test module.
- **T2 IR data structures and builder** for Phase A ops only.
  Bail at IR construction time on unsupported ops.
- **T1 changes**:
  - Eligibility check at module load.
  - Per-function call counter + type feedback vector for eligible
    functions only (per-scheduler-sharded; scheduler-1-only).
  - Profile-emission patches in BeamAsm emitters.
- **JIT server** Erlang process under `kernel/code` (does
  nothing yet); plumbing for dispatching to a dirty CPU scheduler.
- **`+JT2enable` flag** (does nothing yet).

**Audits and measurements (gating downstream phases):**

- **Corpus measurement.** Count BEAM ops in OTP + a representative
  application corpus (RabbitMQ, dialyzer, the Elixir compiler);
  bucket by phase coverage (A/B/C/D/E). Replace placeholder
  percentages in §17 Phase 5 / Phase 7 / Phase 8 with measured
  numbers. Methodology recorded so the analysis can be re-run.
- **Trace audit / matrix.** Walk every `erlang:trace/3` flag,
  every match-spec action, every `erlang:system_monitor` event,
  and `erlang:trace_pattern(_, true, [global])`; classify each as
  inline-preserved / Export-indirection-preserved / requires
  jettison / requires temporary jettison-and-recompile. Land the
  matrix in §12.5.
- **Fun-representation audit.** Verify constant-fun inlining is
  feasible given existing `erl_fun.[hc]` representation (§9.6);
  if blocking, restrict v1 to funs with empty capture.
- **T1 PC table audit.** Verify BeamAsm's per-instruction PC
  table is complete and stable enough to use as a deopt target
  index (§9.1). If sparse, decide on the augmentation strategy
  before Phase 1 starts.
  *Audit result (2026-06-12): the table does not exist — BeamAsm
  keeps per-function line tables only. T1 must emit one; v1 needs
  four entry kinds. See `08_v1_loop_tier.md` §4.5.*
- **Profile-cost micro-benchmark.** Measure the realistic per-
  site profile-update cost on Apple Silicon and Linux ARM64 on
  representative workloads (the corpus above). Validates the
  ~2 ns target in §7.3, surfaces cache-miss factors.
- **Inlining-thesis sizing.** Profile the corpus and bucket
  inline candidates by category (lists higher-order, local,
  cross-module monomorphic, polymorphic). Tells us up front
  whether v1's monomorphic-only inlining captures the bulk of
  the wins.

- **IR printer / dumper** for debugging.

All existing tests pass with `+JT2enable false`. **Phase 0 audits
must complete before Phase 1 starts** — a stale T1 PC table or a
miscounted trace matrix derails everything downstream.

### Phase 1 — Identity transform (≈4 weeks)
Goal: T2 compiles a function but produces equivalent native code
(same X/Y state at every sync point).

- T2 manager listens for compile requests.
- Optimizer runs only the IR builder + direct asmjit emission;
  no optimization passes.
- Sync-point identification pass (§6.3) producing the live-X-reg
  map at each sync point.
- Verify: hot functions get re-compiled into T2; output matches
  T1's X/Y state at every sync point; T2 → T1 jumps work (the
  outer-function deopt path is exercised by adding a dummy guard
  that sometimes fires).
- **Watchpoint + jettison plumbing.** Even though Phase 1
  doesn't speculate, jettison must work cleanly so trace-enable
  on a T2 function reverts cleanly.

This proves the state-preservation model end-to-end.

### Phase 2 — Type narrowing + speculation + guard BIFs (≈6 weeks)
Goal: T2-compiled functions are *faster* than T1.

- Type-inference pass (port of `beam_ssa_type:opt_continue`).
- `speculate_type` insertion at function entry from feedback
  vector.
- `speculate_range` at arithmetic operand boundaries.
- Speculative arithmetic lowering (one-untag trick).
- All guard BIFs as primitive IR ops (§10.7).
- Guard strength reduction.
- Outer-function deopt path: jcc to T1 PC.

Benchmark: ~10–20% speedup on type-narrowable hot paths (small-int
arithmetic, list traversal). If not, framework is wrong; iterate.

### Phase 3 — Inlining MVP + loop recovery + intrinsics (≈10 weeks)
Goal: monomorphic inlining + higher-order intrinsics +
constant-fun-target inlining all work end-to-end.

- Inlining pass with size/depth caps.
- Inlined-region framestate construction.
- Per-region deopt stubs.
- Linear-scan allocator inside inlined regions.
- AOT compiler change: emit `jit_inline` annotation on the
  `sys_core_fold_lists.erl` set.
- T2 recognises annotated callees with constant-known fun arg.
- Tail-recursion → loop recovery.
- LoopInfo analysis pass.
- Per-region deopt stubs emitted from codegen-time framestate
  metadata. (An earlier draft listed "multi-frame deopt dispatch
  (the hard part)" here — that machinery was eliminated by the
  eager-CP-push design; see Appendix C "State preservation and
  deopt".)

Benchmark: big wins on accessor-heavy code, on `lists:foldl`/
`lists:map`-heavy code, gen_server callback dispatch.

### Phase 3.5 — LICM + unrolling (≈4 weeks)
Goal: loop optimizations on T2-recovered loops.

- LICM consuming LoopInfo.
- Loop unrolling with `test_heap` re-coalescing.
- Heuristics for unroll factor.

Benchmark: list-comprehension and `lists:map`-heavy code with
allocation should show further measurable wins from `test_heap`
coalescing.

### Phase 4 — Production polish (≈8 weeks)
Goal: good enough to enable for testing.

- Code cache eviction.
- Watchpoint invalidation on module reload (full v1 set).
- Recompilation backoff.
- Observability infrastructure.
- Documentation.
- Integration testing (RabbitMQ, Cowboy, OTP test suite).

### Phase 5 — Phase B/C instructions (≈8 weeks)
Goal: extend coverage to maps, exceptions, plain BIFs, closures
(non-monomorphic), apply.

- New IR ops for maps, exception handling, full call_fun.
- **Map-shape feedback slots** (§7.6) — per-site shape pointer
  collected in T1.
- **Region-level shape specialisation** in T2: a single shape
  guard at the entry to a hot region with multiple map accesses,
  after which all accesses are direct offset loads. Shape
  mismatch deopts to T1.
- Phase B/C lowering.

This raises tier-2-eligibility from ~60–70% of functions to
~85–90%.

### Phase 6 — Polymorphic + speculative-fun (v2)
- Extended profile slot: small array of N observed targets per
  call site.
- Inliner emits PIC switch for 2-N receivers.
- Speculative-fun specialisation.
- Audit and resolve the fun deopt complexity (§9.6).

### Phase 7 — Phase D instructions (binary) (v2)
- Bitstring matching and construction in T2 IR.
- Binary-context value threading.

This unlocks protocol-handling code; brings coverage to ~90–95%.

### Phase 8 — Phase E instructions (messages, NIFs, floats) (v2)
- Receive optimization (unblocked-by-ref skip).
- NIF call IR ops.
- Float operations.

Coverage ~98–100%. T2 can compile essentially all Erlang code.

### Phase 9 — IV analysis + binary-loop opts + OSR-entry (v2)
The remaining loop-optimization tail.

### Phase 10 — x86_64 (v2)
- New emitters for x86_64-specific lowerings.
- Same calling convention as BeamAsm/x86_64.

### Sequencing rationale

**Each phase ships something measurable and useful.** Phase 1's
identity transform is the critical sanity check on the state-
preservation model — get this wrong and every subsequent phase is
built on sand. Phases 2 and 3 deliver the v1 wins. Phase 3.5
extracts the loop-shape wins. Phase 4 makes it production-ready.

Phases 0–2 (~18 weeks) validate or invalidate the entire approach.
If at any point the state-preservation model proves untenable, we
cut early.

## 18. Risks and open questions

### High-impact risks

1. **State-preservation model is impractical.** If sync-point
   identification or the constrained allocator (§6, §11.2) turn
   out to add more complexity than they save, we may have to
   choose between two fallbacks: (a) the strictest "identical at
   every BEAM instruction boundary" rule from earlier drafts —
   correct but constrains the allocator unnecessarily, or (b)
   full framestates everywhere, JSC-DFG style. **Mitigation**:
   Phase 1's identity transform ships option (a) by default;
   relaxation to the sync-point model happens when measurements
   show it's worth the engineering cost.

2. **Compile-time overhead too high.** ~1 ms target may slip,
   especially with inlining. **Mitigation**: hard cap per blob
   (abort + leave on T1); cheap pass selection; measure in Phase
   1 and tune.
3. **Deopt too frequent.** Phase changes in production make T2
   constantly bounce. **Mitigation**: strict recompile backoff;
   per-blob exit-rate telemetry; "demote permanently" if rate >
   threshold.
4. **Fun deopt audit blocks v1 inlining of foldl.** If
   `make_fun3`'s captured-environment shape can't be reconstructed
   for inlined deopt, even constant-fun inlining is hard.
   **Mitigation**: Phase 0 audit; if blocking, restrict v1 to funs
   with empty capture (which is the common case).
5. **Cross-module SSA chunk size unacceptable.** If the SSA chunk
   adds 80% to BEAM file size, deployment friction.
   **Mitigation**: investigate compact encoding (arena-allocated
   custom format vs ETF) in Phase 0.
   *Superseded by the third-pass rescope*: with IR built from the
   loaded BEAM code (`08_v1_loop_tier.md` §4.1) this risk disappears
   and is replaced by "SSA reconstruction loses material structure"
   — mitigated by the G1 fidelity gate, with the chunk as fallback.
6. **AOT changes break existing modules.** Adding the SSA chunk
   and `jit_inline` annotation must be backwards-compatible.
   **Mitigation**: chunk is optional; old loaders ignore unknown
   chunks; `jit_inline` is annotation-only.

### Decisions resolved during planning

These were open questions at design time, resolved during review:

- **T2 manager supervision tree.** Erlang JIT server process
  under `kernel/code` co-ordinates compile requests (queueing,
  deduplication, eligibility filtering, blob installation). The
  actual compile work runs on a dirty CPU scheduler. The server
  serialises requests; one worker thread runs at a time so
  asmjit's JitAllocator doesn't need a mutex. Detail: §3.
- **Optimizer language.** C++, not Erlang. Detail: §4, §11.1.
- **IR identity.** New T2 IR in C++, 1:1 with BEAM SSA in Phase
  A. Detail: §5.
- **State-preservation model.** Outer function matches T1's X/Y
  layout *at sync points only*; the register allocator is free
  between sync points. Inlined regions use framestates. Detail:
  §6.
- **BEAM SSA at runtime.** New BEAM chunk + new C API +
  mandatory compile option (`+t2_compile_eligible`). Detail: §7.6.
  *Superseded by the third-pass rescope*: v1 builds IR from the
  loaded BEAM code (no chunk, no compile option, works on existing
  beams); the chunk design remains the G1 fallback. See
  `08_v1_loop_tier.md` §4.1.
- **Counter / feedback vector placement.** Side table per
  module; emitted only for tier-2-eligible functions. Detail: §7.
- **OSR-exit in v1.** Yes, required for `erlang:trace/3`. In the
  outer function, deopt is a single jcc to T1 PC — trivial.
  Detail: §6, §12.5, §15.4.
- **BIF inlining scope.** All guard BIFs as primitive IR ops.
  Detail: §10.7.
- **Higher-order intrinsics.** AOT-annotated `jit_inline`,
  initial set = `sys_core_fold_lists.erl` 10 BIFs. Detail: §10.4.
- **Loop optimization v1.** Recovery + LICM + unrolling with
  `test_heap` coalescing. IV analysis is v2. Detail: §10.5–10.6.
- **SSA chunk format.** ETF — the BEAM file already uses ETF for
  other chunks, the AOT compiler already produces SSA as Erlang
  terms, and decoding into the C++ IR builder is straightforward.
  If chunk size becomes a deployment issue we revisit a compact
  encoding later.
- **Recompilation vs blacklisting on deopt-bouncy functions.**
  Exponential backoff. Each recompile doubles the threshold
  (`100 * 2^R`); after enough rounds the function effectively
  blacklists itself because the threshold becomes unreachable.
  No explicit blacklist needed. Detail: §9.5.
- **Profile-data lifetime across module reload.** Reset on
  reload in v1. Preservation across compatible reloads is v2.
- **Receive optimization** (`recv_mark`/`recv_remove` for
  ref-based receive shortcut). AOT-only, T2 never touches it.
  The AOT compiler is the sole owner of receive pattern
  recognition. T2 lowers `receive` ops as opaque side-effecting
  operations; the AOT-emitted shape is what reaches BeamAsm and
  T2 alike. Receive ops are sync points (§6.1) and stay generic.
- **Process dictionary.** `erlang:get/1` and `erlang:put/2` get
  explicit side-effect modelling in the IR. They're treated as
  opaque side-effecting ops that cannot be reordered with other
  side-effecting ops or with each other. Effectively the same
  model as a memory barrier for the process dictionary slot.
- **JSC-style inline caches for call sites.** No in-place IC
  patching; only the profile-source half. Per-call-site
  monomorphic-target slots (§7.5) give T2's inliner the per-
  site target information it needs without modifying T1's
  dispatch. The T1-speedup half of an IC has weak payoff on
  Erlang's already-cheap export-table indirection.
- **Idea #6 (`lukas/erts/implement-inline-caches`, for map
  keys).** Not resurrected. T2 needs the *data* (which map
  shapes appear at which sites) but doesn't need T1 patching to
  collect it: a feedback slot in the type vector (§7.6) is
  enough. T2 then compounds the shape data across inlined
  regions (one guard per region, not per access) and replaces
  the IC's slow path with a deopt to T1, which already has the
  generic code. The optimization belongs in Phase 5 alongside
  the map IR ops, not as a standalone T1 IC project.

### Still open

- **Trace audit specifics.** Per-flag preservation table from
  Phase 0. Methodology fixed; concrete table delivered at the
  end of Phase 0 (see §12.5).
- **Watchpoint granularity vs. cost.** Per-function planned;
  per-module fallback if table grows unmanageably. Measured in
  Phase 4.

## 19. Out of scope

Explicit non-goals so we don't scope-creep:

- A T3 / FTL-equivalent. Maybe one day; not now.
- LLVM dependency. Hard no.
- A separate IR family for register allocation (single linear-
  scan over the whole function with sync-point constraints is
  enough).
- **Cross-module deep optimizations beyond what watchpoint
  invalidation can revert.**
- **Removal or significant refactor of BeamAsm.** T1 stays.
- **Optimizations targeting message passing, ETS, GC, or NIFs.**
  These are runtime BIF concerns, not SSA-optimisable code.
  T2 will never make these optimizations — the wins live in the
  runtime, not in the JIT.
- **Cross-process speculation.** Never. Processes have
  independent heaps and isolated state; speculation across them
  is fundamentally wrong.
- **Receive-pattern optimization.** Owned by the AOT compiler.
  T2 lowers `receive` ops generically and never tries to
  recognise patterns the AOT compiler missed.
- Speedups for cold code. T2 serves long-running hot code.
- A new bytecode format. We extend the BEAM file with an SSA
  chunk and annotations; we do not change BEAM bytecode.
- An interpreter on aarch64. BeamAsm is the lowest tier.

---

## Appendix A — File layout (proposed)

```
erts/emulator/beam/jit/
├── arm/
│   ├── instr_t2.cpp          (NEW) emitters for T2-only ops
│   └── beam_asm_global.cpp   (extend) add t2_deopt_dispatch
├── t2/                       (NEW) the T2 compiler in C++
│   ├── t2_ir.{hpp,cpp}       IR data structures
│   ├── t2_builder.{hpp,cpp}  BEAM SSA → T2 IR
│   ├── t2_types.{hpp,cpp}    type lattice (ported from beam_types.hrl)
│   ├── t2_typeinfer.cpp      type inference pass
│   ├── t2_speculate.cpp      speculation insertion
│   ├── t2_inline.cpp         inlining + loop recovery
│   ├── t2_intrinsics.cpp     higher-order BIF expansions
│   ├── t2_loops.cpp          LoopInfo analysis
│   ├── t2_licm.cpp           LICM
│   ├── t2_unroll.cpp         loop unrolling + test_heap re-coalesce
│   ├── t2_cse.cpp            CSE
│   ├── t2_dce.cpp            DCE
│   ├── t2_codegen.cpp        IR → asmjit
│   ├── t2_framestate.cpp     framestate construction / deopt stubs
│   ├── t2_blob.cpp           code-cache management, blob lifecycle
│   ├── t2_watchpoint.cpp     watchpoint table + invalidation
│   └── t2_manager.cpp        compile-thread + queue
├── beam_jit_t2.h             (NEW) public C API for T2 manager
└── ... existing files ...

erts/emulator/beam/
├── beam_t2_load.c            (NEW) SSA chunk loader
├── beam_t2_profile.c         (NEW) profile counter / feedback vector
└── ... existing files ...

lib/compiler/src/
├── beam_asm.erl              (extend) emit SSA chunk under +t2 option
├── sys_core_fold_lists.erl   (extend) AOT emits jit_inline annotation
└── ... existing files ...

lib/kernel/src/
└── code_server.erl           (extend) call into T2 invalidation on load

PLAN/                          this directory
```

## Appendix B — Estimated total effort

Rough; for sanity-checking scope. Single engineer, doesn't include
benchmarking, regression analysis, code review, production
rollout. Multiply ~2× for total calendar time.

| Phase | LOC est. | Effort est. |
|-------|----------|-------------|
| 0 — Plumbing (incl. SSA-in-BEAM, audits, IR builder, corpus and trace audits, T1 PC table audit, profiling micro-benchmarks) | C++ ~3 KLOC, Erlang ~1 KLOC | 12 weeks |
| 1 — Identity transform + jettison plumbing + sync-point pass | C++ ~2 KLOC | 4 weeks |
| 2 — Type narrowing + speculation + guard BIFs | C++ ~3 KLOC | 6 weeks |
| 3 — Inlining MVP + loop recovery + intrinsics + inlined-region framestate/deopt + linear scan | C++ ~4 KLOC, Erlang ~0.5 KLOC | 14–16 weeks |
| 3.5 — LICM + unrolling | C++ ~1.5 KLOC | 4 weeks |
| 4 — Production polish | C++ ~1.5 KLOC, docs | 8 weeks |
| **v1 total** | **~16 KLOC** | **~48–50 weeks** |
| 5 — Phase B/C ops | C++ ~3 KLOC | 8 weeks |
| 6 — Polymorphic + speculative-fun | C++ ~2 KLOC | 6 weeks |
| 7 — Phase D (binary) | C++ ~3 KLOC | 8 weeks |
| 8 — Phase E (messages/NIFs/floats) | C++ ~2 KLOC | 6 weeks |
| 9 — IV + OSR-entry + binary loops | C++ ~2 KLOC | 6 weeks |
| 10 — x86_64 | C++ ~2 KLOC | 6 weeks |

The total is comfortably under HiPE's footprint. **The maintenance
argument that killed HiPE — too much code, too few maintainers,
divergence from the rest of the runtime — is the threat we have to
plan against. Keeping v1 around 16 KLOC, sharing global fragments
with BeamAsm, and reusing existing JIT infrastructure aggressively
is how we mitigate it.**

Erlang LOC enumerated (refines the per-phase estimates above):

- JIT server (`gen_server` under `kernel/code`): ~300 LOC
- SSA-chunk emit in `beam_asm.erl`: ~200 LOC
- `jit_inline` annotation propagation in `sys_core_fold_lists.erl`
  and friends: ~100 LOC
- `code_server.erl` invalidation hook: ~150 LOC
- Test-only BIFs and lifecycle drivers: ~300 LOC
- Observability (`erlang:t2_stats/0`, `erlang:t2_info/3`): ~200 LOC
- AOT-side type-information chunk additions (modifications, not net
  new file): ~250 LOC
- **Total: ~1500 LOC**, refining the older "~1 KLOC" estimate.

If AOT-side work balloons beyond ~250 LOC, the budget breaks;
revisit and re-scope.

---

## Appendix C — Resolutions from the second-pass critique

The post-merge state of the document reflects two rounds of critique
review. The first pass (sections A–F, ~45 items) was applied directly
to the original `T2.md` before the split. The second pass (sections
G–M, 51 items + 6 follow-ups) is captured here as a consolidated
record of substantive decisions, with cross-references to the files
that hold the resulting prose. The critique itself is archived as
[`../T2_critique_v2.md`](../T2_critique_v2.md).

### State preservation and deopt

- **Eager-CP-push at inlined-region entry** (B1, G3, G7, G8, G11).
  Inlined regions push the parent CP at region entry, so deopt is
  uniform regardless of nesting depth. Codegen-time framestate
  metadata records the live-X-reg map; no runtime CP materialisation,
  no chain walk, no multi-frame deopt dispatch.
  (`03_compilation_and_speculation.md` §9.2.)
- **Deopt is legal at sync points only** (G10), which include
  function entry, calls/returns, GC sites, BIF boundaries,
  speculation guards, tracing-relevant points, and receive safe
  points. Mid-arithmetic deopt is impossible by construction.
  (`03_compilation_and_speculation.md` §9.3.)
- **Eager deopt on GC inside inlined regions** (H1, M2). GC sites
  inside an inlined region behave as deopt points — restore X/Y
  from framestate metadata, branch to T1's PC, T1 handles the GC.
  Trades the per-GC inlining win for architectural simplicity (no
  inlined-region GC stackmap, no spill-tagged-to-Y machinery).
  (`03_compilation_and_speculation.md` §9.2; `05_runtime.md` §12.3.)

### Profile feedback and speculation

- **Profile-feedback conflict resolution: AOT is ground truth**
  (H7). Profile observations outside the AOT-proven type are
  dropped; profile narrows AOT but never contradicts it.
  (`01_ir_and_state.md` §5.4.)
- **Stable speculation-site IDs across recompiles** (H6). IDs are
  hashes of `{source_BEAM_PC, speculation_kind, narrowed_type}`,
  stable until the underlying BEAM SSA changes; module reload
  resets. (`03_compilation_and_speculation.md` §9.5.)
- **Speculation-range auto-selection** (H8). Range = `clamp(observed
  × 1.5, ±2^58)`. If observed range exceeds the cap, no
  `speculate_range` is emitted; arithmetic falls back to generic
  with overflow check. (`03_compilation_and_speculation.md` §9.4.)
- **Per-call-site monomorphic-target slot, with frequency** (M5).
  Each slot records target identity *and* incoming call count.
  Frequency drives both inlining priority and tier-up target
  selection. (`02_profiling.md` §7.5.)
- **Branch-frequency counters in v1, not v2** (M7). Move from "v2"
  to "v1 Phase 2", alongside type narrowing. Branchy Erlang code is
  the primary corpus; cold-arm pruning is one of the largest wins
  T2 can extract. (`02_profiling.md` §7.7.)
  *[REFUTED by measurement — G3-1
  (`../verification/G31_GMAP_OUTCOME.md`): a hand-built
  cold-arm-pruned dispatcher measured 1.01–1.02×. Branch counters
  and pruning are shelved; see `08_v1_loop_tier.md` §9.]*

### Inlining and code generation

- **AOT-inferred inlineability** (H9). A new compiler pass marks
  every Erlang function whose body is "T2-inlineable" with
  `jit_inline => #{...}` on `b_function.anno`. Manual `-jit_inline`
  attributes remain for the rare auto-conservative case. The BIF
  manifest stays manual (those need T2-side IR-op implementations).
  (`04_optimization.md` §10.1.)
- **`length/1` is not inlined** (I2). Treated as an opaque BIF call
  in v1 — the existing T1 BIF implementation handles trap-out and
  is fast enough that inline lowering doesn't pay the complexity.
  Removed from §10.7's manifest.
- **Per-call-site deopt-skip** (H10). Skip inlining at a *specific*
  call site if a prior compile saw a deopt traced to inlining the
  callee at that site. Other call sites of the same callee remain
  eligible. Keyed by `{caller_BEAM_PC, callee_MFA}` via the H6
  hash. (`04_optimization.md` §10.3.)
- **Cold-arm pruning during inlining** (M7). When inlining a callee
  with N clause heads, consult the §7.7 branch-frequency counters
  and inline only arms with ≥ 5% observed frequency; replace cold
  arms with a deopt to T1. (`04_optimization.md` §10.3.)
- **Tier-up target selection** (M1). The function that trips its
  call counter is not necessarily the right compile unit. The JIT
  server consults reverse call-frequency to identify a dominant
  caller and compile *up* the call chain (≤ 2 levels). Annotated
  higher-order helpers (`jit_inline => #{fun_arg_pos => N}`) don't
  tier up standalone. (`02_profiling.md` §7.5; `05_runtime.md`
  §15.1.)
- **Pass-list ordering** (I6). Speculative-arithmetic lowering moves
  before loop-info analysis (now step 9.5, was 14), so unrolling
  duplicates the already-lowered (smaller) form.
  (`03_compilation_and_speculation.md` §8.1.)
- **Sync-point markers are authoritative from IR construction** (I7).
  Marked at step 1 of the pipeline; subsequent passes may not move
  work across a sync-point op in a way that violates the
  constraints. (`01_ir_and_state.md` §6.3.)
- **Sync-constraint conflict policy** (H3). When SSA value V is
  live across two sync points with different T1-mandated X-reg
  constraints, the allocator emits a move (or spill+reload) between
  them. Phase 1 measures conflict frequency.
  (`04_optimization.md` §11.2.)
- **Use of T1-pinned X-regs inside inlined regions** (H2). The
  allocator may freely use x25–x28 / x15–x17 as scratch *inside* an
  inlined region; outer values they held at entry are spilled
  alongside H1/M2's eager-deopt mechanism.
  (`04_optimization.md` §11.3.)

### Reductions, scheduling, and the cache

- **Active-execution-counter sharding bound** (H4). Per-scheduler-
  sharded counter, capped at `MAX_SHARDS = 8` (default). Schedulers
  beyond the cap share shards. Same bound applies to C8 profile
  counters. (`05_runtime.md` §13.3.)
- **Counter-decay implementation** (J7). Periodic timer in the JIT
  server visits all blob counters every K seconds (default 60),
  applies `counter := counter / 2`. Tunable via
  `+JT2decay_interval`. (`05_runtime.md` §13.3.)
- **Queue-drop counter reset** (H5). On compile-queue drop, the
  function's call counter is reset to `threshold − probe_value`
  (default `probe_value = threshold / 4`) so it retrips after a
  small number of additional calls. (`05_runtime.md` §15.3.)
- **Saturation budget** (I5). `N = 256, max_retries = 8` (2048
  ticks total). Calibrated in Phase 0. (`05_runtime.md` §15.2.)

### Lifecycle and observability

- **Continuation-trampoline stack scan** (J3, L2). The lazy stack
  scan walks at most `+JT2lazy_scan_max_depth` frames per visit
  (default 1024). When the bound is hit, a trampoline is installed
  on the stack at the next CP; subsequent unwind to that depth
  triggers another bounded scan. Memory-for-latency knobs allow
  hard-RT operators to cap per-schedule-in latency.
  (`05_runtime.md` §14.2.)
- **In-flight compile generation check** (M6). Each compile job
  captures `module_load_gen[M]` at dispatch; install-time re-reads
  it and discards the job if it has changed.
  (`05_runtime.md` §14.2.)
- **Hibernation audit** (M3). Verify all per-process T2 metadata
  lives on the process struct directly, not on the heap. Phase 0
  audit task.
- **`erlang:memory()` reporting** (M4). New `jit_t2_code` key
  reports T2-blob bytes (active + tombstoned). Backwards-compatible
  addition. (§16 above.)
- **T2 IR validation failure handling** (J6). The C++ assertion in
  the IR builder disables T2 globally for the *module* on first
  failure (per-`{Mod,Fun,Arity}` blacklist applied to the whole
  module); module reload clears the blacklist. All other failure
  modes (asmjit reject, watchpoint race) still abort the BEAM —
  those genuinely shouldn't happen. (§8.4.)
- **Profile-quality metric** (J2). `t2_profile_quality` =
  `succeeded / (succeeded + deopted)`. Sustained values below 0.8
  indicate a profile-collection bug or workload pathology and
  trigger an alert. (§16.)

### Testing

- **CI gate** (J1). Every PR touching T2-relevant paths runs the
  regression bench suite; T2-enabled vs T2-disabled performance
  must not regress more than 2% on any tracked benchmark.
  (§16A.6.)
- **T2 feature exerciser suite** (J1). Modeled on BeamAsm's
  per-instruction exerciser; one targeted Common Test case per T2
  IR op kind, speculation kind, inlining shape, etc. Sub-second
  feedback per feature. (§16A.7.)

### Strategic / framing

- **Cumulative steady-state tax budget** (L1). Already captured in
  `00_overview.md` §1's hard floor: ≤ 3% regression vs T1-only on
  any tracked benchmark, including profile + counter overhead.
- **HiPErJiT findings** (L5). Profile-driven JITs on Erlang have
  historically lost on real workloads (HiPErJiT 1.46× vs HiPE 1.78×
  on Dialyzer; lost to BEAM on the ring benchmark) primarily
  because of profiling overhead. T2 differs by using inline counters
  (cheaper than `erlang:trace/3` by an order of magnitude). The
  risk is captured in §18 risks; Phase 0 measurement against the F2
  corpus including a message-passing benchmark verifies the per-call
  profile cost is within budget. If profile overhead exceeds
  budget, descope to type-narrowing-only (Phase 2) without inlining
  (Phase 3).
- **T2 ⇄ BeamAsm contract** (L3). The four invariants
  (per-instruction PC table — *which does not exist today and must
  be emitted by T1; see `08_v1_loop_tier.md` §4.5*;
  calling-convention register
  assignments; global runtime fragments; patchable function
  prologue) are documented in `05_runtime.md` §12.1, and
  `beam_jit_t2.h` enforces them via `static_assert`. CI fails the
  build on drift.
- **Branchy-Erlang plan rebalancing** (M7). The framing change is
  in `00_overview.md` §1's hard floor; the concrete pass changes
  are branch-frequency counters in v1 (above) and cold-arm pruning
  during inlining (above). The numeric corpus (CLBG) joins the F2
  application corpus as a separate measurement target — wins on
  the application corpus reflect today's Erlang; wins on CLBG
  reflect *expansion target* viability.

### Editorial cleanup folded in

K1–K6 cross-reference G items already applied above. K7 (Phase 4
threshold re-tuning) is reflected in §17 Phase 4. K8 (overbroad
optimization claim about message passing / ETS / GC / NIFs) is
rephrased in §19 to allow the JIT to re-batch / hoist / eliminate
triggers (e.g. `test_heap` coalescing via unrolling, redundant
guard-BIF CSE) where SSA structure permits; the runtime services
themselves remain out of scope. K9 (file layout for `instr_t2.cpp`)
is reflected in Appendix A — architecture-specific emitters live
in `arm/instr_t2.cpp`, architecture-agnostic IR-to-asmjit dispatch
in `t2/t2_codegen.cpp`.

### Items deferred or left open

- **L4** Erlang LOC budget broken down above; verify in Phase 0.
- **J5** Profile-overhead-on-cold-code micro-benchmark — added to
  Phase 0 audits.
- **J8** `apply/3` with monomorphic-by-observation target —
  speculative-target inlining is Phase 6; v1 collects the data
  but doesn't consume it for inlining decisions.
- **H11** `lists:foldl(MyFun, …)` with non-literal fun — addressed
  via M1's tier-up target selection (compile the caller, not the
  helper); v2 picks up the residual case via speculative-fun
  inlining.

---

End of plan. Discussion welcome.
