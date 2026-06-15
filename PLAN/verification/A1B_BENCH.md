# A1-1b benchmark — scalar bs_scan on real json

Benchmark of the scalar `bs_scan` scan-loop optimization (Track A A1-1b)
on representative JSON documents, `scan_loop` on vs off. Built natively
(aarch64, OTP 29, JIT). Driver: `json_decode_bench.erl`.

## Isolated bytewise scan (`scanbench.erl`) — the P2 gate

| scan | stock | scan_loop | speedup |
|---|---|---|---|
| printable-ASCII `notset` (string/7 shape) | 0.89 GB/s | 2.61 GB/s | **2.93×** |
| digit `range` (number shape) | 1.02 GB/s | 4.17 GB/s | **4.08×** |

Both clear the ≥2.5× P2 bar. This is scalar; SWAR (A1-2) lifts toward
G-bin's 5.6×.

## End-to-end json decode (`json_decode_bench.erl`, 3 runs)

| document | profile | speedup |
|---|---|---|
| twitter (1.3 MB) | string-heavy | **1.03–1.04×** (~3–4 %) |
| canada (1.1 MB)  | number/float-heavy | **~1.0×** (within noise) |
| citm (2.4 MB)    | structure-heavy | **~1.02×** (~2 %) |

## Reading

The isolated 2.9–4.1× does **not** carry to end-to-end, for two reasons:
1. **Scanning is a fraction of decode.** Map/list construction, key
   hashing, float conversion, and copying dominate (the GALLOC profiling
   already showed map construction is the real allocation pool).
2. **Short runs don't amortize.** The per-`bs_scan` setup is paid once
   per run; long strings (twitter) amortize it and gain ~3–4 %, but the
   short 2–3 byte numeric scans in canada don't, netting ~0.

So **scalar A1-1b is a small, scan-length-dependent win** — real and
correct, but single-digit % only on string-heavy json. The path to a
compelling end-to-end number is **SWAR (A1-2)** (which roughly doubled
the isolated win in G-bin → ~6–10 % end-to-end there) plus targeting
workloads with long byte runs (protocol parsing, large string fields).
This grounds the re-baseline go/no-go: scalar alone is not compelling
end-to-end; the value is in SWAR + scan-heavy corpora.

## Correctness + safety

json `decode` byte-identical across 5000 randomized encode/decode
round-trips (`json_roundtrip.erl`); all 201 stdlib+kernel modules
compile clean with `+scan_loop` (the >6-bs-op recognizer bail skips the
3 complex-matching functions that trip a pre_codegen save-placement edge
case). `ERL_COMPILER_OPTIONS=scan_loop` enables it whole-build.
