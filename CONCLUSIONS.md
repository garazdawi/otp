# Flatmap Inline Cache — Experiment Results and Conclusions

## Background

The flatmap inline cache (IC) caches the result of flatmap key lookups
on a per-scheduler basis. Each IC entry records a call site, key, shape
(key tuple pointer), and the index of the key in the flatmap. On a hit,
the linear key scan is skipped entirely.

The IC is restricted to literal (immediate) keys and uses interned shapes
(key tuples in literal memory) so that pointer comparison is a reliable
semantic equality check.

## Experiments

### 1. Baseline: ex_doc compile (no recaching)

**Workload**: Clean compilation of ex_doc (30 Elixir source files).
This exercises the Erlang compiler (beam_ssa_type, beam_ssa_alias,
beam_ssa_codegen, etc.) and the Elixir type checker heavily.

| Metric        | Value      |
|---------------|------------|
| attempts      | 1.20M      |
| hits          | 346K       |
| hit_rate      | **29%**    |
| disabled      | 758K (63%) |
| poly_shape    | 44K        |
| poly_key      | 0          |

**Conclusion**: Compiler code is inherently shape-polymorphic — the same
call site sees maps at different stages of construction (e.g., `#{}` →
`#{a => 1}` → `#{a => 1, b => 2}`). A single shape change permanently
disabled the entry, causing 63% of attempts to hit disabled entries.

### 2. Adaptive recaching (threshold = 3)

**Change**: Instead of permanently disabling on first shape mismatch,
allow re-caching with the new shape. Only permanently disable after 3
consecutive misses without an intervening hit. On a hit, reset the miss
counter to zero.

#### ex_doc compile results

| Metric        | Before   | After    | Change   |
|---------------|----------|----------|----------|
| attempts      | 1.20M    | 1.20M    | —        |
| hits          | 346K     | 420K     | **+21%** |
| hit_rate      | 29%      | **35.1%**| +6pp     |
| disabled      | 758K     | 629K     | -17%     |
| recaches      | —        | 97.5K    | new      |

**Conclusion**: Recaching recovers sites that had one-time shape
transitions but are otherwise monomorphic. The improvement is meaningful
but the hit rate remains low because the compiler workload is
fundamentally polymorphic.

#### Recaching ON vs OFF on stdlib docs

| Metric        | OFF (threshold=0) | ON (threshold=3) |
|---------------|-------------------|-------------------|
| hits          | 3.58M             | 3.66M             |
| hit_rate      | 88.2%             | **90.1%**         |
| disabled      | 385K              | 301K              |
| Disabled entries | 408            | 229               |

**Conclusion**: Recaching provides a modest +1.9pp improvement on
runtime workloads, cutting disabled entries nearly in half.

### 3. Index-based matching experiment

**Question**: If instead of comparing the shape pointer, we check
whether the key is at the cached index position in the current map's
key tuple, how many poly_shape misses would become hits?

**Instrumentation**: Added a `poly_shape_same_idx` counter that fires
when a poly_shape mismatch occurs but `flatmap_get_keys(mp)[entry->index] == key`.

| Workload       | poly_shape | poly_shape_same_idx | Rescued |
|----------------|------------|---------------------|---------|
| ex_doc compile | 147K       | 15.9K               | 10.8%   |
| stdlib docs    | 51K        | 2.4K                | 4.6%    |

**Conclusion**: Only ~5-11% of shape mismatches have the key at the same
index. Most poly_shape events involve genuinely different map structures
where the key moved position or is absent. Index-based matching would
add ~1.3pp hit rate improvement — not enough to justify the added
complexity.

### 4. Runtime workload: stdlib doc generation

**Workload**: Generate HTML documentation for OTP stdlib (~99 modules)
using the ex_doc library. This exercises runtime Elixir code: Regex,
EarmarkParser, string processing, ExDoc formatters.

| Metric        | Value       |
|---------------|-------------|
| attempts      | 4.07M       |
| hits          | 3.66M       |
| hit_rate      | **90.1%**   |
| disabled      | 301K (7.4%) |
| Active entries| 4364        |
| Disabled      | 229         |

Top sites are Elixir struct field accesses (`:__struct__`, `:options`,
`:value`) with perfect monomorphic behavior — the same call site always
sees the same struct shape.

**Conclusion**: The IC is extremely effective for runtime Elixir code
that accesses stable struct fields. This is the dominant pattern in
typical Elixir applications (web servers, data processing, etc.).

## Summary

| Workload       | Hit Rate | Character                          |
|----------------|----------|------------------------------------|
| ex_doc compile | 35%      | Compiler code, shape-polymorphic   |
| stdlib docs    | 90%      | Runtime code, struct-monomorphic   |

The IC design is sound for its intended use case: **runtime map access
patterns with stable shapes**. The compiler workload is an inherently
difficult case that no monomorphic IC can handle well.

## Where to go next

### Keep

- **Shape interning** — already implemented, makes pointer comparison
  reliable for semantic equality. Essential foundation.
- **Adaptive recaching (threshold=3)** — cheap, helps both workloads,
  no downside.
- **Literal key restriction** — eliminates poly_key entirely, avoids
  GC safety issues.

### Consider

- **Restrict IC to the JIT only** — the IC check runs on every flatmap
  access. For the JIT, the check can be inlined into generated code with
  minimal overhead. For the interpreter, the function call overhead may
  not be worth it on polymorphic workloads.
- **Warm-up before filling** — only fill an IC entry after N accesses
  to the same site+key+shape. This would avoid filling entries for
  transient shapes during map construction, reducing disabled entries
  on compiler workloads. The cost is delayed activation on runtime
  workloads.
- **Profile a production Elixir app** (e.g., Phoenix under load) to
  confirm the runtime hit rate is closer to 90% than 35% in real
  deployments.

### Drop

- **Index-based matching** — only rescues ~5-11% of poly_shape cases,
  adds complexity, marginal benefit.
- **Polymorphic IC (multiple cached shapes per entry)** — the top
  compiler sites show 20-130+ different shapes across schedulers.
  Caching 2-3 shapes wouldn't help and would multiply memory and
  check cost.
