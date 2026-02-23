# Inline Cache Plan for Maps in erts

## Goals
- Improve throughput for map-heavy workloads.
- Start with a monomorphic inline cache design.
- Implement and validate in the interpreter first, then integrate with ARM JIT.
- Cover most/all relevant map read and write operations.
- Include write operations for both shape-preserving updates and shape-changing transitions.
- Keep the feature behind a VM flag.

## Phase 0: Design Contract and Guardrails
- Define the cache contract and map shape model used by both interpreter and JIT paths.
- Establish per-callsite monomorphic cache behavior.
- Specify fallback behavior for misses/instability.
- Add a global VM flag to enable/disable map inline caches.

## Phase 1: Shape Model and Operation Scope
- Define a cheap and stable shape identity for map guard checks.
- Ensure coverage includes read and write operations used by BEAM map instruction flows.
- Explicitly handle key-tuple-changing writes to keep cache behavior coherent and useful.

## Phase 2: Interpreter Implementation
- Add cache metadata for relevant map operation callsites.
- Implement fast path:
  - shape guard
  - direct cached operation path
- Keep existing semantics as the slow path.
- Fill/update monomorphic entries on slow path success.
- Degrade to normal path at unstable/polymorphic callsites (PIC deferred).

## Phase 3: Correctness, Safety, and Visibility
- Add counters for:
  - attempts
  - hits
  - misses
  - fills
  - shape-change fallbacks
  - disabled-path executions
- Add tests for:
  - read equivalence
  - write equivalence (shape-preserving and shape-changing)
  - flatmap/hashmap transition behavior
  - stress and randomized mutation sequences
- Track memory overhead of cache metadata to avoid unbounded growth.

## Phase 4: ARM JIT Integration
- Reuse the interpreter cache contract and fallback semantics.
- Add ARM JIT codegen for monomorphic guard + fast path.
- Keep slow-path behavior semantically identical to interpreter execution.

## Phase 5: Benchmarking and Iteration
- Start with synthetic map-heavy benchmarks.
- Move to Elixir workloads that heavily use structs/maps.
- Measure:
  - throughput (primary)
  - hit rate
  - cache memory overhead
- Revisit PIC only if monomorphic hit rates are insufficient at important sites.

## Rollout
- Ship behind VM flag (initially off by default).
- Iterate based on benchmark and workload findings.
- Defer x86 JIT integration until interpreter and ARM JIT paths are validated.
