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

## Second app — RabbitMQ MQTT broker (instrumented OTP 28)

RabbitMQ 4.3.1 doesn't boot on OTP 29 (the Horus/Khepri
`unknown_instruction{line}` blocker), so the same instrumentation
patch was applied to **OTP 28.5.0.2** (clean apply + a 2-line fixup
adding the `current_function`/`current_arity` assembler members that
are beamjit2-branch additions) and built into a release. RabbitMQ
under MQTT load (75 k msg/s), global-switch profiling, 12 s window:

```
total 4.5 GB allocated

 9.38%  rabbit_db_topic_exchange:trie_match_try/9   ← topic routing
 6.40%  rabbit_mqtt_packet:parse_packet/4           ┐
 4.42%  rabbit_mqtt_packet:parse/2                  │ MQTT protocol
 3.13%  rabbit_mqtt_packet:parse_remaining_len/5    ┘ parsing (~14%)
 5.62%  mc:set_annotation/3                         ┐ message
 2.03%  mc_mqtt:protocol_state/2 ; mc_mqtt:init/1   ┘ containers (~10%)
 4.48%  rabbit_mqtt_qos0_queue:deliver/3            ┐ delivery /
 3.44%  rabbit_queue_type:-deliver0/4-fun-4-/3      ┘ queue type
 2.46%  rabbit_db_topic_exchange:trie_match/7       ┐ more routing
 2.17%  rabbit_db_topic_exchange:match/3            ┘ (trie ~14% total)
 ...    a long flat tail of rabbit_mqtt_* /
        rabbit_exchange / rabbit_queue_type, each 1–3 %
```

**The shape is the opposite of Bandit's.** Where the JSON API had one
dominant allocator (`maps:from_list/1`, 46 %), the broker's allocation
is **diffuse**: the top function is 9 %, and the rest is a long tail
of routing, parsing, message-container, and delivery functions each
contributing a few percent. The allocation *is* the application logic
— topic-trie routing (~14 %), MQTT protocol parsing (~14 %, a binary
G-bin shape), message-container construction (`mc`, ~10 %), and the
delivery path — spread across dozens of `rabbit_*` functions with no
single lever.

This is exactly the **concentrated-vs-diffuse** distinction that
decides whether allocation optimization pays: Bandit's
map-construction pool is a clear target (one function, ~half the
churn); RabbitMQ's is inherent, distributed message-broker work with
no hot spot to attack — consistent with the earlier `perf` finding
that the broker's cost is term-plumbing/routing/ETS, not a single
function. (Note `rabbit_mqtt_packet:parse_*` ~14 % *is* the binary
G-bin shape, the one slice a JIT could meaningfully fuse.)

## Significance

This closes the loop the GC investigation opened. The original
question — "where do deployed BEAM systems spend CPU, and what's the
lever" — resolved to: VM-internal cost dominates servers, GC is the
biggest coherent pool, the lever is reducing allocation, and **the
allocation is concentrated in map construction and JSON
ser/deser**. The tool that produced this is a working, tested,
gated heap-allocation profiler in the ARM JIT + runtime, and it runs
on real applications.

## Bandit vs RabbitMQ — the headline contrast

| | Bandit (JSON API) | RabbitMQ (MQTT broker) |
|---|---|---|
| total / 12 s | 18.2 GB | 4.5 GB |
| top allocator | `maps:from_list/1` **46 %** | `trie_match_try/9` **9 %** |
| shape | **concentrated** (1 fn ~½ the churn) | **diffuse** (long tail, no hot spot) |
| nature | map construction + JSON ser/deser | routing + parsing + msg containers |
| optimization lever | clear (attack `maps:from_list`) | none single; inherent broker work |

The same tool, run on two real deployed servers, shows that "reduce
allocation" is a sharp, actionable lever for one class (serialization
APIs: a few functions dominate) and a diffuse, low-yield one for
another (message brokers: allocation is spread across the routing /
parsing / delivery logic). That distinction — which only per-function
attribution on the real app can reveal — is the payoff.

## Reproduction

Bandit: build OTP 29 with the instrumentation (`git archive` → Linux
aarch64 container → configure → make), then `bandit_galloc.sh`.
RabbitMQ: the patch applied to OTP 28.5.0.2 (rabbit doesn't boot on
OTP 29) + a 2-line assembler-member fixup, `make release`, then
`rabbit_galloc.sh`. Both use the global `alloc_profile_all` switch
under load. The instrumentation is off by default (no
`ERL_ALLOC_PROFILE`), zero overhead.
