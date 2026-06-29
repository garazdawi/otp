# Real open-source apps — how they fare under idle compression (idea #1)

I measured three large, real Erlang systems running **idle** in their official
Docker images, plus an attempt at Livebook. Because the feature branch is
OTP-30-rc0 and these apps pin to older OTP, I could not boot them directly on
the feature VM. Instead, for each app I:

1. ran it idle in its own Docker image (its own supported OTP),
2. measured **in-node** (its own runtime, no feature needed): the real
   per-process data (`Σ process_info(P, memory)`), the live heap
   (`Σ total_heap_size`), `erlang:memory(processes)`/`ets`, the saving from a
   major GC of every process (`garbage_collect(P, [{type,major}])`, a proxy for
   what hibernate-shrink reclaims), and the **real ETS compression saving** by
   cloning every readable table with the `compressed` option,
3. for RabbitMQ, also captured every process' real live data (gen_server state
   + dictionary + messages) and **replayed it on the feature VM**, running the
   actual `erlang:hibernate(Pid, [compressed])` to measure raw-heap compression
   on real data (and to confirm the feature is correct on real states).

Tooling: `capture.erl`, `probe_inline.erl`, `prober.escript`, `app_replay.erl`.

## Results

| App (idle) | OTP | procs | real proc data¹ | live heap | `memory(processes)`² | ETS | **ETS compressed** |
|---|---:|---:|---:|---:|---:|---:|---:|
| RabbitMQ 4 | 27 | 328 | **2.05 MB** | 1.74 MB | 14.3 MB | 2.89 MB | 2.65→0.57 = **2.08 MB (79%)** |
| MongooseIM 6.2 | 27 | 925 | **7.55 MB** | 6.66 MB | ~200 MB | 5.80 MB | 5.62→2.85 = **2.76 MB (49%)** |
| EMQX 6.2 | 28 | 945 | **4.08 MB** | 3.20 MB | 28.3 MB | 19.5 MB | 15.9→7.42 = **8.45 MB (53%)** |
| Livebook 0.19 | 28 | — | — | — | — | — | not measurable³ |

¹ `Σ process_info(P, memory)` — the actual bytes of process control blocks +
heaps + stacks + queues. ² `erlang:memory(processes)` — allocator carrier size.
³ Livebook disables standard distribution (`RELEASE_DISTRIBUTION=none` + a custom
`Elixir.Livebook.EPMD`) and rejects external connections, so it cannot be
probed this way (a finding in itself).

### RabbitMQ — feature actually run on real captured states (feature VM)

The 328 real process states replayed and compressed correctly. On the real
*live* process data (no artificial slack):

```
live process data : 1.07 MB
after hibernate    : 0.72 MB  (shrink: -32%)
after compress     : 0.57 MB  (compress: -46.5%)
```

Raw-heap compression of real BEAM process heaps is ~**46%** here — lower than
ETS's 79% because raw heaps carry pointers/headers/stack that compress less
than ETS's external-term encoding, and small heaps have fixed per-process
bookkeeping.

## Findings

1. **`erlang:memory(processes)` is mostly allocator carriers, not data.**
   The clearest case is MongooseIM: `erlang:memory(processes)` reports ~200 MB,
   but the actual process data is only **7.55 MB** and a major GC of every
   process frees just 4.3 MB. The ~190 MB gap is empty `eheap_alloc` carriers
   left over from startup churn — memory hibernation/compression does **not**
   target and the allocator does not return without carrier-release tuning
   (`+M…acul`, carrier migration). EMQX and RabbitMQ show the same pattern,
   milder.

2. **Idle infrastructure servers hold very little live process data** — 2–8 MB
   across hundreds/thousands of processes. Compressing it saves ~45–50% (≈1–4 MB
   per app). Real, but small in absolute terms. These apps are already lean:
   they keep little state resident per idle process (and several already
   hibernate their own connection processes).

3. **ETS compression is the consistently larger, fully-real win** — 2.1, 2.8 and
   **8.4 MB** (49–79%). EMQX in particular keeps 19.5 MB in ETS (routing /
   session / retained-message tables) and is 53% compressible. Notably this is
   *already available today* via the `compressed` table option — the apps just
   don't enable it; idea #1 doesn't change ETS.

4. **The feature's big wins need many processes with large live state.** The
   synthetic benchmark (`RESULTS.md`) reclaims 65–77 % / hundreds of MB with
   5k–20k idle workers holding ~8 KB live state each. Real idle brokers don't
   look like that; an app that parks thousands of fat idle sessions (large
   `gen_server` states, buffered data) would.

## Rough "reclaimable on an idle node" per app

Process compression at the measured ~46 % of live heap + the real ETS saving:

| App | process compress | ETS compress | combined |
|---|---:|---:|---:|
| RabbitMQ 4 | ~0.8 MB | 2.08 MB | **~2.9 MB** |
| MongooseIM 6.2 | ~3.1 MB | 2.76 MB | **~5.9 MB** |
| EMQX 6.2 | ~1.5 MB | 8.45 MB | **~9.9 MB** |

## Bottom line

On real, well-behaved idle infrastructure servers the *process* heaps hold
little, so process compression saves only a few MB; the headline
`erlang:memory(processes)` numbers are dominated by allocator carriers that this
feature doesn't reclaim. The more reliable idle-memory win on these apps is
**ETS compression** (up to ~8 MB / ~50–80 %), which the existing `compressed`
option already provides. Process compression pays off on workloads with many
processes carrying large idle state — exactly the synthetic case in
`RESULTS.md`.
