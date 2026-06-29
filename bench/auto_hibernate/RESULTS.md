# Idle-system compression — measured RAM savings (idea #1)

How much RAM can be saved on an *idle* Erlang/OTP system when all processes and
ETS tables are compressed?

This branch implements idea #1 (automatic hibernation of processes, with an
optional compressed-heap mode) and measures the savings with
`bench/auto_hibernate/hibernate_bench.erl`.

## How it was measured

* A node is started and (almost) every OTP application is started, for a
  realistic idle system.
* Optionally, `N` synthetic idle worker processes are spawned, each holding a
  typical `gen_server`-style state (~1000-word live heap + a process
  dictionary). This models what idea #1 actually targets: *"long-running
  Erlang systems often have many processes that sit idle"*.
* The system is left to settle, then memory is measured with
  `erlang:memory/0` (the allocator's accounted in-use bytes) and OS `RSS`.
* Every process is hibernated with `erlang:hibernate(Pid)` (shrink only), then
  with `erlang:hibernate(Pid, [compressed])` (compress), measuring
  `erlang:memory(processes)` after each step.
* ETS savings are measured by cloning every readable table with the
  `compressed` option and comparing `ets:info(Tab, memory)` (there is no
  in-place table conversion, so this is the *potential* ETS saving).

Run it yourself (after building this branch):

```
$ERL_TOP/bin/erl +P 2000000 -noshell -pa bench/auto_hibernate \
    -eval 'hibernate_bench:run(#{workers => 5000})' -s init stop
```

Numbers below are from an Apple-silicon macOS build (`erlang:memory(processes)`,
the memory that compression actually frees for reuse inside the VM).

## Results — process memory

| Idle processes | baseline | after `hibernate` (shrink) | after `compressed` |
|---:|---:|---:|---:|
| ~120 (bare node) | 25.3 MB | 25.3 MB (−0.2 %) | 25.7 MB (−1.5 %) |
| 5 120 | 128.7 MB | 107.9 MB (−16.1 %) | **44.5 MB (−65.4 %)** |
| 20 120 | 437.5 MB | 352.6 MB (−19.4 %) | **102.5 MB (−76.6 %)** |

* On a **bare idle node** there are only ~120 processes with already-tiny
  heaps. Shrinking is roughly neutral (most system processes already hibernate
  themselves), and compressing such tiny heaps costs slightly more (zlib output
  + per-process bookkeeping) than it saves. **Compression is not worthwhile for
  a handful of small processes.**
* On a **realistic long-running system** (thousands of idle processes holding
  real state), compression reclaims **65–77 % of the process heap memory** —
  e.g. **335 MB freed** with 20 000 idle workers. Plain shrinking alone
  reclaims ~16–19 %; the rest comes from compression.

## Results — ETS

On the bare OTP node, 36 of 41 tables were readable and compressible:

```
uncompressed : 0.46 MB
compressed   : 0.18 MB
potential    : 0.28 MB  (60.6 %)
```

ETS data compresses well (~60 %), but a bare OTP node holds very little in ETS,
so the absolute saving is small. The win scales with how much application data
lives in (uncompressed) tables.

## Caveats / honest notes

* `erlang:memory(processes)` is the right metric for *"how much RAM can be
  saved"*: it is the memory freed for reuse inside the VM. The reported numbers
  are that delta.
* **OS RSS does not drop immediately.** Freed heap blocks are returned to the
  emulator's allocators, not necessarily to the OS, so RSS stays high until
  carriers are released (allocator tuning such as `+Muacul`/carrier migration
  is needed to return it to the OS). The VM can immediately *reuse* the freed
  memory, however.
* The ETS figure is a *potential* saving measured via compressed clones; there
  is no in-place table-compression conversion in this branch (idea #1 is about
  processes — ETS already supports `compressed` at creation time).
* Compression uses `zlib` at `Z_BEST_SPEED`. Term sharing inside a heap is
  preserved (raw heap bytes are compressed, not re-encoded). The process
  dictionary is kept in a separately-decompressable form so
  `process_info(Pid, dictionary)` works without waking/decompressing the
  process.

## Conclusion

For the literal benchmark requested — *a bare idle node with all OTP
applications started* — there is little to gain from process compression
(~0 MB; the system is already small) and ~0.3 MB from ETS. The feature pays off
on the systems idea #1 is aimed at: **long-running nodes with many idle
processes, where 65–77 % of the idle process heap memory can be reclaimed.**
Shrinking (`hibernate/1`) is cheap and always safe; compression
(`hibernate/2, [compressed]`) should be applied to processes with non-trivial
idle heaps.
