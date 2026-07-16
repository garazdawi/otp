# 22 — Where the time really goes: sampling vs `call_time`, and the ⅓-addressable ceiling

Status memo, 2026-07-16. Extends the phase reevaluation (doc [`15`](15_scope_and_disposition.md)/[`16`](16_retrospective.md)) and the S0 census (doc [`20`](20_census_results.md)) with a **sampling** view (Linux `perf`) of the same three workloads. It answers two questions that came up while re-examining the skipped phases and non-loop functions:

1. Is there a **non-loop hot-function** pool that inlining + type feedback (P3-class) could convert to speed?
2. Are the census's own-time numbers trustworthy, given that Erlang's built-in `call_time` tracing is deterministic per-call instrumentation?

It changes no disposition. It **sharpens** the decided-against call and names the one lever the evidence still supports: **monomorphic map-shape specialization** (doc [`19`](19_general_tier_path.md) S1).

## 0. TL;DR

- **Only ~⅓ of real execution time is in JIT-addressable Erlang code.** `perf` self-time DSO split: JIT'd Erlang **28% compiler / 36% mapwl / 39% json**; the rest is C runtime + GC + libc, which **no** JIT tier (T1, T2, or a hypothetical P3–P5 general tier) can touch.
- **The `call_time` census over-weights small, high-frequency functions**, exactly as suspected. `lists:foldl/3` reads **7.4%** own-time under `call_time` but **<0.8%** under sampling — it is a guard+tail-call wrapper whose "own-time" was mostly the per-call trace hook. Every census percentage is a share of the ~⅓ Erlang layer, not of real time.
- **Where the time actually goes is C BIFs + GC**: compiler `eq` 12% + `erts_maps_put` 7% + GC ~17% + map reads ~8% + `erts_cmp_compound` 6%; mapwl `erts_maps_put` **28%** + `maps_merge_2` 13% + `map_next` 6% (≈47% in C map primitives alone); json GC ~14% + binary/compare/map-build C helpers ~17%.
- **The non-loop "pool" is real but not P3/P4-addressable.** The instrument tags only *tail*-recursion as loop-shaped, so "non-loop" own-time is inflated by body-recursive loops, thin wrappers, BIF stubs, and single map/set ops. The genuine straight-line-with-optimizable-redundancy pool (the P4-GVN target) is **≈ 0**.
- **Forcing T2 onto the biggest genuine non-loop bucket regresses.** `sets:is_element` (map-backed) `timer:tc`: T1 6.27 ns → forced-T2 **14.62 ns (2.3× slower)**; `maps:get` 3.96 → 4.63 ns. The install gate rejects these; T2 delivers 0 there.
- **Verdict:** the phases stay decided-against, now on sampling evidence. The only lever the data supports is **map-shape specialization** — it drains the single biggest addressable pool (map access) *and* the `eq` hotspot (map-key comparison) at once. The productive framing is **intrinsify hot ops under type/shape feedback**, not reimplement BIFs in Erlang (Erlang loses to hand-C in the general case and on the T1 path).

## 1. Two instruments, and the deterministic-tracer bias

The S0 census (doc 20) weights own-time with **`erlang:trace_pattern(_, _, [call_time])`** — deterministic per-call accounting, exact call counts, exclusive time (nested traced calls banked out). It is `eprof`-grade and measures **only Erlang function frames**: C BIFs, NIFs, the GC, and libc are outside its denominator by construction.

`perf` samples the whole process at a timer interrupt, so it sees C, GC, and libc too. Cross-checking the two:

| function | `call_time` own-time | `perf` self-time | ratio |
|---|---|---|---|
| `lists:foldl/3` (guard+tailcall wrapper, compiler) | 7.4% | <0.8% | ~10× inflated |

The wrapper's `call_time` "own-time" is dominated by the fixed per-call trace hook (a wrapper does almost no work per call but is called millions of times). This is the predicted bias — the built-in tracer over-weights small high-frequency functions — and it means **the census distribution is a within-Erlang-layer distribution**, to be read against §2's finding that the layer is a minority of runtime.

The realized-speed numbers in this project (survey, §5's `timer:tc`) are **not** affected: they are untraced wall-clock. Only the *distribution* uses `call_time`.

## 2. The ⅓-addressable ceiling

`perf record --buildid-mmap -F 1999` (the `--buildid-mmap` flag is required under Docker/overlayfs, where beam.smp's exec mapping is reported as `(deleted)` — the kernel embeds the build-id in the mmap event so symbols still resolve), forced T2 **off** (the distribution is a workload property), `+JPperf true`:

| workload | JIT'd Erlang | C runtime + GC + libc | kernel |
|---|---|---|---|
| json (byte service) | **39%** | 60% | <1% |
| compiler (analysis) | **28%** | 61% | 5% |
| mapwl (map service) | **36%** | 53% | 10% |

The realizable ceiling for **any** JIT tier is that ~⅓. The census's addressable shares (59% / 19.6% / 30.7%, doc 20) are shares *of the Erlang layer*; as a share of real execution they are ~⅓ of that. This is the deepest form of "eligible ≠ win": most of the work is not even in the layer T2 compiles.

## 3. Where the time actually goes (perf self-time, `--buildid-mmap`)

**compiler (analysis)** — the best case for the elimination phases:

| self% | symbol | kind |
|---|---|---|
| 12.2% | `eq` | C (term equality — mostly map-key/`keyfind` comparison) |
| 8.9% | `do_minor` | C (GC) |
| 7.1% | `erts_maps_put` | C (map insertion — `sets:add_element`) |
| 7.0% | `sweep_new_heap` | C (GC) |
| 6.2% | `erts_cmp_compound` | C (comparison) |
| 4.8% | `$global::i_get_map_element_shared` | JIT fragment (map read) |
| 3.2% | `get_map_element` | C (map read) |

≈17% GC, ≈15% map ops, ≈18% comparison — all C or shared stubs. The straight-line Erlang bodies that P3/P4 would optimize are not in the top set.

**mapwl (map service)** — the knockout:

| self% | symbol | kind |
|---|---|---|
| **27.6%** | `erts_maps_put` | C |
| 13.0% | `maps_merge_2` | C |
| 5.6% | `erts_internal_map_next_3` | C |
| 5.4% | `$global::i_get_map_element_shared` | JIT fragment |
| 4.6% | `$maps:fold_1/4` | JIT (loop) |

≈47% in C map primitives alone. This is doc 20's "55% of map own-time is the maps *library*" made concrete and *sharper*: it is behind the **C BIF boundary**, not merely behind an inliner. Cross-module inlining of the maps library (the S3/S4 "bigger bet") would not reach `erts_maps_put`.

**json (byte service):** ~14% GC (`do_minor` 11.4%, `sweep_new_heap` 2.8%) + C helpers (`erts_binary_part` 5.0%, `erts_cmp_compound` 4.5%, `erts_build_sub_bitstring` 2.6%, `flatmap_from_validated_list` 2.2%); the JIT'd encoder spine (`json:string/string_ascii/escape_binary/do_encode`) is body-recursive (loop-shaped), ~25–30%.

## 4. The non-loop shape split

Re-slicing the census own-time by the instrument's per-function `LoopShaped` flag (driver: `census/shape_census.erl`):

| workload | non-loop own-time | of which BIF-stub | non-BIF candidate |
|---|---|---|---|
| compiler | 73% | 12% | 61% |
| mapwl | 47% | 4% | 42% |
| json | 20% | <1% | 19% |

**Calibration caveat (important):** the instrument's `loop_shaped` is set only for **self-*tail*-recursion / SCC** (`t2_eligible.c:1527`), so **body-recursive loops are tagged non-loop**. Probed synthetic + real functions confirm it: `body_rec([H|T]) -> [H+1|body_rec(T)]`, `lists:mapfoldl_1/3`, `lists:map/2`, `ordsets:union/2` all report `loop=false` despite being loops.

Decomposing the compiler's 61% non-BIF-non-loop candidate by *true* source shape:
- **~16% body-recursive loops** mis-tagged (`mapfoldl_1`, `foldr_1`, `ordsets` merges) — already loop-tier / fold-recognizer territory.
- **~13% thin dispatch wrappers** (`lists:foldl/3`, `reverse/1`) — inflated by the tracer (§1); only *inlining-away* helps, and the win is the eliminated call, which memo 17 measured at ~0.
- **~24% single map/set-op leaves** (`sets:*` is map-backed → one `maps:is_key`/`put`) — the map-shape (S1) pool, not P3/P4.
- **≈ 0% genuine straight-line bodies with GVN-able redundancy** — the actual P4 target. None in the top list.

So non-loop hot functions are *not scarce* (correcting an earlier claim), but their accelerable-by-a-new-optimizer content is ~0.

## 5. Realized regression on the biggest genuine non-loop bucket

Single map/set ops, `timer:tc` min-of-3, driver loop itself T2-compiled:

```
sets:is_element (1024-key):  T1 6.27 ns  →  forced-T2 14.62 ns   = 2.3× SLOWER
maps:get        (1024-key):  T1 3.96 ns  →  forced-T2  4.63 ns   = 1.17× slower
```

Even with the tail-recursive driver getting the T2 loop win, the per-iteration non-loop call's T2 tax (prologue + speculation guards, nothing to amortize) more than eats it. The P2.6 install gate rejects these blobs → they stay T1 → T2 delivers 0. This is the non-loop case of "eligible ≠ win," measured.

## 6. Conclusions

- **The skipped phases stay decided-against**, now on sampling evidence, not argument. P3 (inlining) as an enabler would admit exactly the non-loop map/wrapper code that §5 shows regresses; P5 (allocation elimination) faces a GC pool that is real output, not sinkable (doc 16); P4 has no straight-line pool to run on (§4).
- **The one lever the data supports is monomorphic map-shape specialization** (S1). It is the single biggest addressable pool (mapwl ~47%, compiler-via-sets ~15%), and specializing the map opcode also reclaims the `eq` hotspot (map-key comparison becomes a typed compare on the known key). It needs no Erlang reimplementation — `get_map_element`/`put_map_assoc` are already opcodes T2 sees; the work is emitting a fixed-offset sequence under shape feedback instead of the generic C helper.
- **Intrinsify under feedback, do not rewrite BIFs in Erlang.** Reimplementing `maps:merge`/`erts_maps_put`/binary helpers in Erlang loses to hand-C in the general case and on the T1 path, and the ops worth specializing (map access, comparison) are already opcodes T2 sees. The productive move is inline templates for hot `(op, type/shape)` pairs with deopt to the C BIF.

## 7. Profiling T2 itself — the perf-symbol hook

Until now T2-compiled code was **invisible** to perf: the `+JPperf` symbol machinery (`beam_jit_metadata.cpp`) is driven only by the T1 module-load path, and a T2 blob lives in its own `JitAllocator` span outside the T1 module range, so its samples resolved to a bare address.

Added (this memo's commit): `beamasm_t2_register_perf(name, base, size)` — declared in `t2_install.h`, defined in `beam_jit_metadata.cpp` (mirrors T1's `AsmRange` designated-init), called from `erts_t2_install` on the success path. It routes through the shared `perf.update()`, so a T2 blob gets **both** a `perf-<pid>.map` line and a `jit-<pid>.dump` record (instruction-level annotatable), named `$T2:Module:Function/Arity` (the `T2:` prefix distinguishes T1 vs T2 samples). Self-gated on the perf modes; no-op unless `+JPperf`, and on non-Linux.

**Recipe to profile T2 under colima/Linux:**
```
perf record --buildid-mmap -F 1999 -o t2.data -- \
  env T2_INSTALL_GATE=0 T2_RETAIN=1 ERL_AFLAGS="+JT2enable true" \
  erl +JPperf true -noshell -run <workload>
perf report -i t2.data          # T2 code resolves to $T2:Module:Fun/Arity
```
Known pre-existing caveat: the jitdump `FileHeader` hardcodes `elf_mach = EM_X86_64` (`beam_jit_metadata.cpp`), which mislabels aarch64 jitdump for T1 *and* T2; MAP mode is unaffected. Fixing it (pick the target arch) is a separate one-liner.

## 8. Artifacts / reproduce

- `census/shape_census.erl` — the loop-shape split driver (re-slices the S0 census by `LoopShaped`).
- `census/perf_work.erl` — bare workload driver for sampling (no tracing), one leg per run.
- perf env: Ubuntu 24.04 aarch64 container (`t2lin-img`), kernel 6.8.0, `linux-tools-6.8.0-*`, `perf record --buildid-mmap`.
- The instrument itself (`{t2_census, Bin}`) and the `call_time` driver are documented in doc 20 §5.
