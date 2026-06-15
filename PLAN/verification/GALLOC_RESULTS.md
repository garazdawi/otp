# Heap-allocation worklist — real Bandit JSON API (built instrumented OTP)

The payoff of the whole GC investigation: a precise per-function heap
allocation worklist for a real deployed-server workload, produced by
the heap-allocation profiler
([`GC_SITE_INSTRUMENT_SPEC.md`](GC_SITE_INSTRUMENT_SPEC.md)) running in
an OTP built **with the instrumentation** for Linux aarch64 in colima.

## Setup

- The instrumentation (this branch) built from a clean `git archive`
  into a Linux aarch64 container (full OTP, `make -j4`).
- Bandit + Jason via `Mix.install` on the built OTP, the same JSON
  API + pipelined-HTTP load as the earlier `perf` round.
- `ERL_ALLOC_PROFILE=1` at boot; under load, the **global switch**
  `erts_debug:set_internal_state(alloc_profile_all, true)` turns on
  profiling for *every* process, then `alloc_profile_sites` is read
  after a 12 s window. Words are exact regardless of profiling
  overhead (we measure allocation, not time).

## The worklist (% of all heap words allocated, 12 s window)

```
total 18.2 GB allocated

 46.05%  maps:from_list/1                     ← dominant
  9.98%  Jason.Decoder:object/6
  6.16%  Jason.Decoder:key/6
  5.03%  Jason.Decoder:array/6
  4.10%  String.Unicode:upcase/3
  3.61%  prim_inet:send/4                      (socket I/O)
  3.01%  Jason.Decoder:key/5
  2.33%  Jason.Decoder:value/5
  1.62%  Jason.Encode:encode_string/2
  1.48%  lists:reverse/1
  1.41%  Jason.Encode:list_loop/3
  1.25%  Jason.Encode:escape_json_chunk/5
  1.13%  erts_internal:garbage_collect/1
  1.11%  String:downcase_ascii/1
  0.93%  Enum.map (lists^map)
  ...    Bandit/Plug pipeline, gen_server, prim_inet:recv ≤ 1% each
```

## Reading

**`maps:from_list/1` is 46 % of all heap allocation** — the single
dominant source by a wide margin. JSON decoding (`Jason.Decoder`
object/key/array/value) adds ~25 %, and JSON encoding
(`Jason.Encode`) ~5 %. So **map construction + JSON ser/deser are
~75 % of the heap churn** of a JSON API. The socket path
(`prim_inet:send/recv`) and the Bandit/Plug framework are each ≤ a
few percent.

This is the precise, actionable confirmation of everything upstream
in the investigation:

- The msacc/perf rounds said GC is the dominant VM cost on Bandit.
- The `gc_inst` survival fractions said the garbage is short-lived →
  "less garbage" is the lever, not "faster GC".
- This profiler now names *which functions make the garbage*:
  **`maps:from_list/1` first, then JSON decode/encode.**

The `maps:from_list/1` dominance is the headline finding and a
concrete optimization target — it is the runtime/JIT manifestation
of the **G-map** shape (map construction) the gate program flagged.
Every `Jason.decode` of an object and every Plug/Bandit map build
funnels through it. Cutting its allocation — a cheaper
`maps:from_list` for the small-flatmap case, or compiler/JIT escape
analysis on map building where the map is consumed locally — attacks
nearly half the heap churn of the most JIT-friendly server workload
we measured.

Caveats: the C-path attribution uses `c_p->i` (steady-state
accurate, with bounded cross-attribution at function transitions —
negligible over a 12 s window); `maps:from_list` is a BIF/C
allocator, attributed to itself here (it *is* the allocation site,
which is what we want). The `erts_internal:garbage_collect/1` 1.1 %
is the allocation done *by* Bandit's forced GC path
([`GC_RESULTS.md`](GC_RESULTS.md) — the deliberate
`gc_every_n_keepalive_requests`).

## Significance

This closes the loop the GC investigation opened. The original
question — "where do deployed BEAM systems spend CPU, and what's the
lever" — resolved to: VM-internal cost dominates servers, GC is the
biggest coherent pool, the lever is reducing allocation, and **the
allocation is concentrated in map construction and JSON
ser/deser**. The tool that produced this is a working, tested,
gated heap-allocation profiler in the ARM JIT + runtime, and it runs
on real applications.

## Reproduction

Build script `/tmp/otp_build.sh` (git archive → Linux aarch64
container → configure → make); run script `bandit_galloc.sh` in this
directory (Elixir + Bandit on the built OTP, global-switch profiling
under load). The instrumentation is off by default (no
`ERL_ALLOC_PROFILE`), zero overhead.
