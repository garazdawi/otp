# Inline Cache Plan for Maps in erts

## Goals
- Improve throughput for map-heavy workloads.
- Start with a monomorphic inline cache design.
- Implement and validate in the interpreter first, then integrate with ARM JIT.
- Cover most/all relevant map read and write operations.
- Include write operations for both shape-preserving updates and shape-changing transitions.
- Keep the feature behind a VM flag.
- Avoid large per-process IC memory overhead by using shared runtime IC state.
- Scope this work to small maps (flatmaps) only; ignore hashmaps in this effort.

## Phase 0: Design Contract and Guardrails
- Define the cache contract and map shape model used by both interpreter and JIT paths.
- Establish per-callsite monomorphic cache behavior.
- Specify fallback behavior for misses/instability.
- Add a global VM flag to enable/disable map inline caches.
- Keep code immutable across schedulers/threads; store mutable IC state in runtime-owned shared metadata.
- Enable/populate IC entries on first miss at a callsite (no hotness threshold in v1).
- Use separate IC slot identity schemes for interpreter and ARM JIT (no cross-runtime slot coordination required).

## Phase 0.5: Operational Policies
- Miss policy: permanently disable IC for the callsite after first miss.
- Hotness policy: allocate/fill on first miss.
- Concurrency policy: start with simple global locking where needed; optimize later only if profiling demands it.
- Rollback policy: VM flag off path must preserve baseline behavior with zero semantic change.

## Phase 1: Shape Model and Operation Scope
- Define shape identity as canonicalized key-tuple pointers from a runtime-owned shape interning table.
- Use pointer equality for fast shape checks.
- Ensure coverage includes read and write operations used by BEAM flatmap instruction flows.
- Explicitly handle key-tuple-changing writes to keep cache behavior coherent and useful.
- Do not rely on module literals for shape ownership/lifetime.
- Treat hashmaps as out of scope and always use existing non-IC paths for them.

## Phase 1.5: Runtime Shape Interning and Lifetime
- Implement a global shape intern table keyed by key-tuple contents.
- Canonicalize key tuples on write paths that create/transition shapes.
- Start with a single lock protecting the intern table; revisit finer-grained locking after profiling.
- Maintain GC-safe lifetime with runtime refcounting and deferred reclamation strategy.
- Allocate canonicalized key tuples in runtime-owned long-lived storage (not module literal storage).
- Account for references from:
  - IC entries
  - live maps using canonicalized shape tuples
- Add bounded-growth controls and observability for the intern table.
- Define exact refcount touchpoints in implementation:
  - map creation/update paths that adopt/release canonical shapes
  - IC fill/overwrite/disable paths
  - cleanup/reclamation paths

## Phase 2: Interpreter Implementation
- Add callsite metadata and shared IC entries for relevant map operations.
- Implement fast path:
  - shape guard
  - direct cached operation path for reads
  - direct cached update path for writes
- Keep existing semantics as the slow path.
- Fill/update monomorphic entries on slow path success, including transition metadata.
- On any IC miss at a callsite, permanently disable IC for that callsite and use normal path for the remainder of execution (PIC/megamorphic strategies deferred).
- Cache update-specific metadata:
  - shape-preserving key index
  - shape-changing insert position
  - output shape pointer for transitions
- Use a compact monomorphic IC entry format containing:
  - enabled/disabled state
  - operation kind
  - input shape pointer
  - cached key/index metadata
  - optional output shape pointer and insert position for shape-changing writes

## Phase 3: Correctness, Safety, and Visibility
- Add counters for:
  - attempts
  - hits
  - misses
  - fills
  - shape-change fallbacks
  - disabled-path executions
  - intern table inserts/lookups/reclaims
- Expose counters through existing VM/internal stats plumbing used by runtime diagnostics.
- Add tests for:
  - read equivalence
  - write equivalence (shape-preserving and shape-changing)
  - flatmap-only behavior and guard correctness
  - stress and randomized mutation sequences
- Track memory overhead of cache metadata to avoid unbounded growth.
- Validate shape intern lifetime correctness under GC and concurrent schedulers.

## Phase 4: ARM JIT Integration
- Reuse the interpreter cache contract and fallback semantics.
- Add ARM JIT codegen for monomorphic guard + fast path.
- Keep slow-path behavior semantically identical to interpreter execution.
- Reuse the same runtime shape interning and shared IC entry layout.

## Phase 5: Benchmarking and Iteration
- Start with synthetic map-heavy benchmarks.
- Move to Elixir workloads that heavily use structs/maps.
- Measure:
  - throughput (primary)
  - hit rate
  - cache memory overhead
  - shape intern table memory overhead
- Gate progress with concrete acceptance criteria:
  - no semantic regressions with IC enabled
  - no regressions when IC is disabled
  - throughput improvement on targeted synthetic flatmap workloads
  - bounded memory growth for IC + shape intern structures
- Revisit PIC only if monomorphic hit rates are insufficient at important sites.

## Rollout
- Ship behind VM flag (initially off by default).
- Start with VM flag default `off`; exact final flag name will follow OTP naming conventions during implementation.
- Iterate based on benchmark and workload findings.
- Defer x86 JIT integration until interpreter and ARM JIT paths are validated.
- Defer all hashmap IC design/implementation to future work.
