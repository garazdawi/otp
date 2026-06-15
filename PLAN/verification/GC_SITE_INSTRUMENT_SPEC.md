# Heap-term garbage attribution — implementation spec (idea #50, grounded)

Concrete, code-grounded scoping for the per-allocation-site garbage
instrumentation that [`GC_RESULTS.md`](GC_RESULTS.md) §"Per-site
attribution attempt" established is *required* — the existing
`+M atags code` + `instrument:allocations` path is a live snapshot
that structurally cannot see short-lived churn. Expands
`otp-ideas/IDEAS/50-gc-term-lifetime-instrumentation.md` with the
real ERTS hook points and an honest cost/risk breakdown.

**Read this before building anything.** The full design is a large,
invasive GC change. This spec exists to make the build/no-build
decision with eyes open, and proposes a much cheaper Phase 0 that may
make the full build unnecessary.

## What we actually need

A worklist: *which `module:fun/arity` allocation sites produce the
most short-lived garbage*, so escape-analysis / unboxing / heap-sizing
work can target them. Two facts we already have for free:

- **Lifetime is already known to be short** — `gc_inst` measured
  survival 0.18 (RabbitMQ) / 0.39 (Bandit); the garbage is
  majority dead at the first minor GC.
- **A coarse site list already exists** from the `+JPperf` round:
  `Jason.Decoder:string/number/object`, `mc_mqtt`, the binary-build
  functions, etc. are hot, and hot allocators show up there.

So the *marginal* value of new instrumentation is: turning that coarse
perf list into a precise, allocation-weighted (not CPU-weighted)
ranking, and confirming the lifetime per site. That marginal value
must justify the build cost below. **It may not** — see "Decision".

## The ERTS facts the design must respect

Grounded in `erl_gc.c` / `erl_vm.h` (read 2026-06-15):

1. **Heap layout** (`erl_process.h`): young heap is `heap …
   high_water … htop … hend`; data above `high_water` was allocated
   since the last GC (generation 0), below it has survived ≥1 GC.
   Old generation is `old_heap … old_htop … old_hend`.
2. **Allocation is an inline HTOP bump.** `HAlloc`/`HeapOnlyAlloc`
   (`erl_vm.h:154-212`) do `HEAP_TOP(p) += sz`. **Under the JIT,
   `test_heap` bumps HTOP in generated code — no C call.** This is
   the crux: any *per-term* alloc-time recording needs a store
   emitted at the HTOP bump, i.e. **JIT codegen changes, per-arch**
   (arm + x86). This is the cost idea #50's sketch underplays.
3. **Copy is destructive with move-markers.** `do_minor` /
   `sweep_new_heap` / `full_sweep_heaps` copy survivors to the new
   heap and overwrite the survivor's old slot with a forwarding
   marker (`IS_MOVED_BOXED`/`IS_MOVED_CONS`, `erl_gc.h:36-37`). After
   a collection the from-heap is *corrupted* — headers are gone, so
   you cannot walk it term-by-term. (This is why idea #50 needs the
   separate term-start bitmap to recover boundaries.)
4. **Readout precedent**: `erts_alcu_gather_alloc_histograms`
   (`erl_alloc_util.c:7959`) + `instrument:allocations` show the
   shape a BIF + histogram readout should take.

## Phase 0 — event-driven allocation-volume profiler (recommended first)

**Goal**: allocation *volume* by site, **exact, not sampled**. Skips
per-term metadata, the GC-walk, and the GC-copy-loop changes
entirely. Delivers the worklist at a fraction of Phase 1's cost/risk.

**Mechanism** (the key realisation: hook the *heap-reserving
instructions*, where the allocation size is a compile-time constant
and the machine state is already GC-safe — verified against the JIT):

1. **`test_heap` / `allocate_heap`** (`emit_gc_test`,
   `instr_common.cpp:137`): `bytes_needed` (specifically `Nh`, the
   *heap* words) is a literal known at codegen, and `after_gc_check`
   is a GC-safe point. When eheap atags are enabled, record
   `(site, Nh_words)` there. The site is this instruction's own
   address (a codegen constant), mapped to `{M,F,A}` via
   `erts_find_function_from_pc` (`beam_ranges.c:299`) at readout.
2. **BIF returns** (`emit_i_bif_body_shared`, `instr_bif.cpp:70`):
   BIFs allocate variably via their own `HAlloc`. HTOP lives in
   x23 and there is already an `emit_leave_runtime` boundary —
   snapshot HTOP before the call, diff after, attribute
   `HTOP_after − HTOP_before` to the BIF's MFA. Covers the
   `binary_to_term`/`list_to_binary`/etc. allocators that have no
   preceding `test_heap`.
3. **`bs_create_bin` / `bs_*`** heap reservations: same as (1) —
   hook the size-known reservation point.

   (`trim` / `i_trim` is **not** a hook site — it adjusts the stack,
   not the heap, so it allocates nothing. The heap-reserving
   instructions above are the complete set.)

This is **exact and event-driven**: it fires only at allocation
points (zero cost for non-allocating code), and records the precise
words each site reserves, not a statistical estimate. It is the
heap-side analogue of the existing `+M atags code` (which only sees
`erts_alloc`), gated the same way (`+M…atags code` for eheap, or a
dedicated `+RGsites` flag).

**Two overhead variants:**

- *C call* (simplest to prototype): at the hook, call a recorder
  `erts_record_heap_alloc(site, words)`. Needs `enter/leave_runtime`
  (register flush) at each `test_heap` — heavy, but fine for a
  profiling mode that's off by default.
- *Inline per-site counter* (low overhead, recommended): give each
  `test_heap` site a 64-bit counter slot allocated at load time
  (same pattern as the per-function profiling counters already in the
  T2 plan); emit an inline `ldr/add/str` (3 instructions, no register
  flush, no safe-point dance) incrementing it by `Nh`. Readout walks
  the slots, maps each site→MFA. No C call on the hot path.

**Captures**: allocation *volume* by site (exact). **Does not
capture**: per-site *lifetime* — that still needs GC correlation
(Phase 1). But the **global** survival fraction from `gc_inst`
already establishes the garbage is short-lived, and the volume
ranking restricted to low-survival processes is the actionable
worklist.

**Cost**: the JIT hook at `emit_gc_test` (+ the bs/allocate variants)
and the BIF-return delta, a per-site counter table or a global
IP→words histogram, one readout BIF. Per-arch JIT touch, but *only*
at the allocation instructions and *only* an inline counter increment
— no GC-copy-loop change, no per-term metadata, no 2× heap memory.
**Risk: low–moderate** — additive, off by default; the only
correctness concern is that the counter increment must not perturb
the allocation/GC fast path (the inline variant is a plain memory
store, inert to GC).

**Deliverable test**: run against Bandit + RabbitMQ in colima (the
existing leg scripts), produce a ranked `MFA → allocated-bytes`
table. Compare against the `+JPperf` hot-function list — if it
agrees, the coarse list was already the worklist and Phase 1 is
unnecessary; if it surfaces allocators perf missed (allocation ≠
CPU — a function can allocate heavily while costing little CPU),
that's the new, valuable signal.

## Phase 1 — full per-term lifetime+site histogram (idea #50)

Build only if Phase 0 proves insufficient (e.g. lifetime-per-site is
genuinely needed to decide between unboxing vs stack-allocation vs
region GC for specific sites).

**Per-segment metadata array** (idea #50's settled design): alongside
each heap segment (young, old, and the GC to-space) allocate a
parallel array of the same word count, `meta[i]` describing the term
whose first word is `heap[i]`, plus a 1-bit-per-word term-start
bitmap. Entry = `{alloc_gen: u32, alloc_site_ip: u32-offset}`
(~8 bytes/word → the heap memory doubles; bounded and opt-in).

**Three hook sites:**

1. **Allocation** (`HAlloc`/`HeapOnlyAlloc` *and* JIT `test_heap`/
   `gc_bif`/`bs_*` emitters): on each bump, set the term-start bit at
   the base offset and store `{current_gen, current_ip}` into
   `meta[offset]`. The C macros are easy; **the JIT emitters are the
   real work** — per-arch codegen to emit the metadata store
   alongside the HTOP advance, only when instrumentation is enabled
   (a compile-time/boot-time gated alternate emit path, like the
   existing `erts_alcu_enable_code_atags` branch).
2. **Copy** (`do_minor`, `sweep_new_heap`, `full_sweep_heaps`): when a
   survivor is moved, propagate its `meta` entry from the from-offset
   to the to-offset (alongside the existing move). The move sites are
   well-defined (`IS_MOVED_*`), but there are *many* of them and they
   are the hottest, most correctness-critical loops in the runtime.
3. **Post-collection walk**: iterate the from-segment's term-start
   bitmap; for each term, if its old slot is *not* a move-marker, it
   died this generation — record `(current_gen − alloc_gen, alloc_site)`
   into a global histogram. (The bitmap gives boundaries; the
   corrupted from-heap is never read for headers.)

**Readout**: a BIF mirroring `gather_alloc_histograms`, returning
`#{ {M,F,A} => #{ lifetime_gen_bucket => {count, words} } }`.

**Cost**: ~1.5–2 KLOC C across `erl_gc.c` + per-arch JIT emitters,
2× heap memory when enabled, measurable per-allocation overhead even
when storing just two words. **Risk: high** — it instruments the GC
copy loops and the allocation fast path, the two places a bug
corrupts the heap silently. Demands the full GC test battery.

## Risk / test plan (Phase 1)

- **Correctness gate**: the metadata path must be *provably inert* to
  GC behaviour — same heap layout, same survivors, byte-identical
  results with instrumentation on vs off. Run the full
  `erts`/`emulator` GC suites and `+RG`/stress GC tests under both.
- **Differential heap check**: a debug mode that, with instrumentation
  on, asserts the post-walk "dead" set is exactly (allocated −
  survived) by word count, cross-checked against the existing
  `reclaimed` counter from `gc_inst`/`statistics`. Any mismatch =
  metadata desync bug.
- **JIT-emit parity**: per-arch, assert the instrumented `test_heap`
  emits the same HTOP advance and the metadata store is consistent
  with the C `HAlloc` path (a shared exerciser).
- **Memory bound**: confirm the 2× heap memory is released with the
  heap and accounted in `erlang:memory/0`; OOM behaviour under
  `max_heap_size`.
- **Off-by-default + boot flag** (`+RGsites` or similar), zero
  overhead when off (the JIT emits the normal path; the C macros
  branch on a global like atags does).

## Decision

**Recommendation: build Phase 0 (the event-driven volume profiler),
then re-decide.** It is the right first build: exact, low-risk, no
GC-copy-loop surgery, reuses the IP→MFA + histogram-readout infra,
and answers the actual question ("which sites allocate the churn").
The major-GC mystery turned out to be library config (not garbage at
all), which removed the most urgent motivation; what remains is
attributing the genuine minor-GC allocation churn, and Phase 0 does
that exactly. If its ranking matches the perf hot-function list,
**the full Phase 1 metadata-array build is not worth its cost and
risk**, and the next move is straight to acting on the sites (escape
analysis / heap-size tuning). Phase 1 (per-site *lifetime*) is
reserved for the case where lifetime precision is the actual blocker
for a specific optimization decision — which no current finding
requires yet.

In short: the heavy GC instrumentation is now *specified and
ready to build*, but the evidence says **measure cheaper first (Phase
0) and likely skip the heavy build**, consistent with the session's
discipline of not building infrastructure ahead of a validated need.

> **Phase 0 design credit / note**: the event-driven hook approach
> (record at `test_heap`/BIF-return rather than per-term or by
> sampling) replaced an earlier statistical-sampling sketch. It is
> better on every axis — exact instead of estimated, event-driven
> instead of timer-driven, zero cost for non-allocating code — and
> it is the heap-side mirror of the existing erts_alloc atags
> mechanism. This is the build to do.

## References

- `otp-ideas/IDEAS/50-gc-term-lifetime-instrumentation.md` — the
  original design (metadata array + bitmap + propagate-on-move).
- `GC_RESULTS.md` — why existing infra is insufficient; the survival
  fractions; the corrected (app-requested) major-GC story.
- ERTS: `erl_gc.c` (`do_minor`, `sweep_new_heap`, `full_sweep_heaps`,
  the `IS_MOVED_*` move sites), `erl_vm.h` (`HAlloc`/`HeapOnlyAlloc`),
  `erl_process.h` (heap layout), `beam_ranges.c:299`
  (`erts_find_function_from_pc`), `erl_alloc_util.c:7959`
  (histogram-readout precedent), `jit/arm/instr_common.cpp:3103`
  (the `c_p->i` / atags maintenance to reuse for Phase 0).
