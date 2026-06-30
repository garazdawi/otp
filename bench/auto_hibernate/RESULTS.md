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

Compression uses the `enc_term` encoding (`erts_encode_ext_ets`, the same
compact format ETS uses for `compressed` tables):

| Idle processes | baseline | after `hibernate` (shrink) | after `compressed` |
|---:|---:|---:|---:|
| ~120 (bare node) | 25.3 MB | 25.3 MB (−0.1 %) | 25.6 MB (−1.3 %) |
| 5 120 | 128.7 MB | 107.9 MB (−16.2 %) | **51.8 MB (−59.8 %)** |
| 20 120 | 437.4 MB | 352.6 MB (−19.4 %) | **131.6 MB (−69.9 %)** |

* On a **bare idle node** there are only ~120 processes with already-tiny
  heaps. Shrinking is roughly neutral (most system processes already hibernate
  themselves), and compressing such tiny heaps costs slightly more (encoded
  image + per-process bookkeeping) than it saves. **Compression is not
  worthwhile for a handful of small processes.**
* On a **realistic long-running system** (thousands of idle processes holding
  real state), compression reclaims **60–70 % of the process heap memory** —
  e.g. **306 MB freed** with 20 000 idle workers. Plain shrinking alone
  reclaims ~16–19 %; the rest comes from compression.

### `enc_term` vs the earlier zlib prototype

The first prototype zlib-compressed the raw heap block; it reclaimed 65–77 % on
this *synthetic* benchmark — a little more than `enc_term` (60–70 %) — because
the workers hold `lists:seq(1,1000)` and runs of sequential integers are an
ideal case for zlib's entropy coder, whereas `enc_term` spends ~1.5 bytes per
small integer plus per-process bookkeeping. On **real application data** the
ranking reverses: `enc_term` measured **37 % of heap** on real RabbitMQ process
states vs the raw-heap zlib's ~46–54 %, because real idle heaps are dominated by
atoms (encoded as 2–3 byte table indices) and refc binaries (kept as
references, never inlined) rather than long integer runs. `enc_term` is also
position independent (no pointer relocation on wake) and cheaper on CPU, which
is why it is the chosen mechanism.

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
* Compression re-encodes the live data with `enc_term` (`erts_encode_ext_ets`).
  Refc binaries and magic refs are kept as references (never inlined), atoms
  become table indices, and decoding needs no pointer relocation. Term sharing
  is **not** preserved (a shared subterm is expanded on decode); the worst case
  for that — large shared binaries — is avoided since binaries are kept as
  references. The process dictionary is kept in a separately-decodable form so
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
