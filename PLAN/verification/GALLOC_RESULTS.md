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

## Third app — Bandit + **Ash** + Postgres (realistic framework)

The first Bandit run is a *thin* API: decode JSON → build one map →
`Ecto.insert_all` / `Repo.all`. Real Elixir web apps run a framework.
This run swaps the bare Ecto handler for **Ash 3 + AshPostgres** (one
of the explicitly-targeted frameworks) backed by a real Postgres in
the container: a `Bench.Item` resource, `Ash.Changeset.for_create →
Ash.create!` per record on `op:"create"`, `Ash.Query.sort/limit →
Ash.read!` on `op:"list"`. Same OTP 29 instrumented build (Elixir
1.19.2, `elixir-otp-28` zip runs cleanly on OTP 29), same global
`alloc_profile_all` switch, 12 s window, ~795 MB/s allocation rate
(9.5 GB total — heavy load).

```
total 9.5 GB allocated

 12.31%  erlang:is_atom/1            ┐
  7.71%  erlang:put/2                │ C-path cross-attribution
  4.97%  erlang:erase/1             ┘ (leaf guard + pdict BIFs)
  4.33%  prim_inet:recv0/3           ┐ socket I/O
  0.81%  prim_inet:send/4            ┘ (~5.6% incl. async_recv)
  3.34%  Spark.Dsl.Extension:do_fetch_opt/3    ┐ Ash reads its
  1.25%  Spark.Dsl.Extension:get_entities/2    │ compiled DSL at
  0.51%  Spark.Dsl.Extension:persisted!/3      ┘ runtime (~5%)
  3.12%  DBConnection.Holder:checkout_call/5   ┐
  1.26%  Ecto.Repo.Queryable:struct_load!/6    │
  1.07%  Ecto.Type:load/3                      │ Ecto / Postgrex /
  1.00%  Ecto.Adapters.SQL:log/5               │ DBConnection driver
  0.82%  Ecto.Type:process_loaders/3           │ stack (~11%, the
  0.77%  Postgrex.Messages:parse/3             │ DB round-trip)
  0.75%  Ecto.Type:adapter_load/3              │
  0.66%  Postgrex.Messages:encode/1            ┘
  0.96%  Ash.Actions.Create:commit/3           ┐ Ash action
  0.69%  Ash.Helpers:do_deep_merge_maps/2      │ pipeline +
  0.57%  Ash.CanOpts (validate/to_options)     │ authz (~4%)
  0.46%  Ash.Actions.Helpers:restrict_field_access/2 ┘
  1.20%  maps:from_list/1            ← was 46% in the thin run
  0.95%  Jason.Encode:map_naive_loop/3
  ...    long flat tail of Enum/Keyword/Access/maps, each ≤ 0.7%
```

**The headline: `maps:from_list/1` collapsed from 46 % to 1.2 %.**
Adding a real framework + real DB I/O *dissolves* the single hot
allocator into a long tail. The allocation is now **diffuse** — the
DB driver stack (Ecto/Postgrex/DBConnection, ~11 %), Spark reading
Ash's compiled DSL at runtime (~5 %), the Ash action/authorization
pipeline (~4 %), socket I/O (~5.6 %), and JSON encode (~1 %) — with
no structurally-meaningful allocator above ~4 %. The shape is
RabbitMQ's, not the thin Bandit's.

Caveat on the top three: `erlang:is_atom/1` (12 %), `erlang:put/2`
(7.7 %), `erlang:erase/1` (5 %) are leaf guard / process-dictionary
BIFs that do not themselves build terms on the process heap. They are
**C-path cross-attribution** — `erts_galloc_note_cpath` resolves the
allocator via `c_p->i`, and in Ash/Spark's extremely call-dense,
guard-heavy, pdict-using code (telemetry/logger metadata, DBConnection
state) these hot leaves are the frequent "landing spot" at a heap-
fragment boundary. The precisely inline-attributed entries (the
`Ash.*`/`Ecto.*`/`Spark.*`/`Jason.*` JIT-compiled functions) are the
trustworthy, structurally-meaningful signal; the diffuse conclusion
holds with or without the artifact.

## Significance

This closes the loop the GC investigation opened. The original
question — "where do deployed BEAM systems spend CPU, and what's the
lever" — resolved to: VM-internal cost dominates servers, GC is the
biggest coherent pool, the lever is reducing allocation. **But the
*concentration* of that allocation depends entirely on how thin the
app is.** A micro-thin JSON API funnels ~half its churn through
`maps:from_list`; a real framework (Ash) or a message broker
(RabbitMQ) spreads the same churn across dozens of framework / driver
/ protocol functions with no hot spot. The tool that produced this is
a working, tested, gated heap-allocation profiler in the ARM JIT +
runtime, and it runs on real applications.

## The headline contrast — thin API vs framework vs broker

| | Bandit thin (Jason+Ecto) | Bandit + **Ash** + PG | RabbitMQ (MQTT) |
|---|---|---|---|
| total / 12 s | 18.2 GB | 9.5 GB | 4.5 GB |
| top *real* allocator | `maps:from_list/1` **46 %** | DB stack ~11 %, no fn > 4 % | `trie_match_try/9` **9 %** |
| `maps:from_list/1` | **46 %** | **1.2 %** | — |
| shape | **concentrated** (1 fn ~½) | **diffuse** (framework + DB) | **diffuse** (routing + parse) |
| nature | map build + JSON ser/deser | Spark DSL + Ecto/Postgrex + Ash pipeline | routing + parsing + msg containers |
| optimization lever | clear (attack `maps:from_list`) | none single; framework + DB I/O | none single; inherent broker work |

The same tool, run on three workloads, shows that **"reduce
allocation" is only a sharp lever for thin serialization APIs** — and
even there, the 46 % `maps:from_list` concentration is partly an
artifact of the benchmark's thinness. The moment a realistic framework
(Ash) or a message broker (RabbitMQ) is in the path, the same churn
goes diffuse: spread across DSL interpretation, the DB driver stack,
the action pipeline, routing and protocol parsing — no hot spot to
attack. This is the grounding the investigation needed: the headline
`maps:from_list` finding does not survive contact with a real
framework. Per-function attribution on the *real* app is what reveals
that — a synthetic or thin benchmark would have kept us chasing
`maps:from_list`.

## Reproduction

Bandit (thin): build OTP 29 with the instrumentation (`git archive` →
Linux aarch64 container → configure → make), then `bandit_galloc.sh`.
Bandit + Ash + Postgres: same OTP 29 build, `bandit_ash.sh` —
`apt-get postgresql`, seed an `items` table, `Mix.install([bandit,
jason, ash ~> 3.0, ash_postgres ~> 2.0])` on Elixir 1.19.2
(`elixir-otp-28` zip runs on OTP 29 with `ELIXIR_ERL_OPTIONS=+fnu`),
`Bench.Item` Ash resource over the existing table (`integer_primary_key`,
no migrations needed), `http_db.erl` driving mixed create/list batches.
RabbitMQ: the patch applied to OTP 28.5.0.2 (rabbit doesn't boot on
OTP 29) + a 2-line assembler-member fixup, `make release`, then
`rabbit_galloc.sh`. All use the global `alloc_profile_all` switch
under load. The instrumentation is off by default (no
`ERL_ALLOC_PROFILE`), zero overhead.
