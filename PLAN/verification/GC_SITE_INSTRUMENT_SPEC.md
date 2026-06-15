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

## Phase 0 — sampling allocation profiler (recommended first)

**Goal**: allocation *volume* by site, statistically. Skips per-term
metadata, the GC-walk, and (mostly) the JIT changes. Likely delivers
the worklist at ~10% of the full cost and risk.

**Mechanism**: a periodic sampler (reuse the existing reduction-based
yield check, or a per-scheduler timer) that, on each tick, reads the
running process's `c_p->i` (current BEAM IP — already maintained
cheaply when `erts_alcu_enable_code_atags` is set, per
`instr_common.cpp:3103`) and the words allocated since the last tick,
and credits those words to the IP. IP → `{M,F,A}`+line via
`erts_find_function_from_pc` (`beam_ranges.c:299`).

- *Allocated-since-last-tick* is the awkward part: `htop` resets at
  GC. Track a per-process cumulative `total_heap_allocated` counter
  (incremented at GC by the pre-GC `htop - heap` and between by
  reading `htop`); sample its delta. One counter, no per-term store.
- *Attribution granularity*: statistical (sample rate × allocation
  rate), like a CPU profiler. Good enough to rank sites; not exact.
- *Lifetime*: not captured per site, but the **global** survival
  fraction from `gc_inst` already bounds it, and Phase 0 can bucket
  samples by "process whose survival fraction is low" to separate
  garbage-heavy from retention-heavy processes.

**Cost**: a per-process counter, a sampler hook in the scheduler
loop, a global IP→words histogram, one BIF to read it. ~300–500 LOC
C, no GC-copy-loop change, no metadata arrays, no per-arch JIT
codegen. **Risk: low** — additive, off by default, touches no
correctness-critical GC path.

**Deliverable test**: run against Bandit + RabbitMQ in colima (the
existing leg scripts), produce a ranked `MFA → allocated-bytes/s`
table. Compare against the perf hot-function list — if it agrees,
the coarse list was already the worklist and Phase 1 is unnecessary;
if it surfaces allocators perf missed (allocation ≠ CPU), that's the
new signal.

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

**Recommendation: build Phase 0, then re-decide.** The major-GC
mystery turned out to be library config (not garbage at all), which
removed the most urgent motivation. What remains — attributing the
genuine minor-GC allocation churn — is real but we already have a
coarse worklist from perf + the global survival fractions. Phase 0
(low-risk sampling profiler) sharpens that into an allocation-weighted
ranking cheaply; if its ranking matches the perf list, **the full
Phase 1 build is not worth its cost and risk**, and the next move is
straight to acting on the sites (escape analysis / heap-size tuning).
Phase 1 is reserved for the case where per-site *lifetime* precision
is the actual blocker for a specific optimization decision — which no
current finding requires yet.

In short: the heavy GC instrumentation is now *specified and
ready to build*, but the evidence says **measure cheaper first (Phase
0) and likely skip the heavy build**, consistent with the session's
discipline of not building infrastructure ahead of a validated need.

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
