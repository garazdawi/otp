# Real-application profiling — where deployed BEAM systems spend CPU

The P0 cycle-weighted measurement from
[`../T2/08_v1_loop_tier.md`](../T2/08_v1_loop_tier.md) §8, run
2026-06-12, with the question deliberately widened per review: *is
the biggest pool JIT-addressable code at all, or something internal
to the VM?*

## Method

Three layers per workload, chosen for low distortion (per the §2.3
methodology rule: no per-call instrumentation):

1. **msacc** (base states) — exact cycle split
   emulator/GC/port/check_io/aux per scheduler class. Zero
   distortion.
2. **Statistical sampling** (`t2_prof.erl`) — every 5 ms, the
   `current_function` + top-3 stack of every process in status
   `running`. Attributes the emulator share by function.
3. **macOS `sample`** of the beam process — C-function-level view
   of VM internals (GC, copy, allocators, locks, ETS, syscalls);
   JIT-compiled Erlang code appears as anonymous frames.

**Tool finding (affects layer 2, discovered on the Bandit leg):**
`process_info(P, current_function)` on a *running* process is
served via a signal that the target itself processes — so processes
with busy signal queues report the signal-handling continuation
(`erlang:bif_handle_signals_return/2`) rather than the interrupted
code. Compute-bound processes (empty queues) attribute correctly;
message-heavy servers over-attribute to the signal path. The
inflated number is still *informative* (those processes genuinely
spend large time draining signal queues — the native sample
corroborates the costs underneath) but it cannot be read as a
precise share. A future sampler should attribute by the frames
*below* the signal continuation, or use scheduler-side sampling
(Linux `perf` + `+JPperf`).

Workloads (Apple Silicon, 10 cores; drivers on the same machine):

| leg | runtime | load | rate achieved |
|---|---|---|---|
| dialyzer PLT build | repo master | self-driving, `+S10` | — |
| RabbitMQ 4.3.1 | OTP 28.5 (asdf) | hand-rolled MQTT QoS0: 4 pubs flood + 4 subs, 512 B ([`mqtt_load.erl`](mqtt_load.erl)) | **489 k msg/s** |
| Bandit/Plug JSON API | OTP 29.0 + Elixir 1.19.5 | pipelined HTTP/1.1 keepalive POST, Jason decode → map/Enum work → Jason encode ([`bandit_server.exs`](bandit_server.exs), [`http_pipe.erl`](http_pipe.erl)) | **148.6 k req/s** |

## Results

### msacc — the top-level split (% of scheduler busy time)

| state | dialyzer | RabbitMQ | Bandit |
|---|---|---|---|
| emulator (Erlang code + BIFs) | 75.8 | 69.0 | 57.4 |
| GC | 12.4 | 9.5 | 12.3 |
| port (TCP driver work) | — | 17.8 | 12.4 |
| check_io | 5.3 | 1.9 | 5.9 |
| other/aux | 6.5 | 1.8 | 12.0 |
| *scheduler busy of total* | *56 %* | *73 %* | *96 %* |

### Within the emulator share

**Dialyzer** (clean attribution — compute-bound): the G3-2 family
(`are_all_limited` + `is_limited` + `t_has_var*`) is **~16 % of
running samples by time** (vs 47 % by calls — the call-count
overweighting directly measured at ~3×, not the ~10× I estimated
from the null result; with emulator at 76 % of busy this puts the
family at ~12 % of busy CPU, which makes G3-2's 0 ± 1 % outcome a
statement that the specialization recovered almost none of a
real pool, not that the pool was tiny — consistent with the
"containers dominate, leaf gains net against container losses"
analysis in `G3_OUTCOME.md`). Next: `oc_mark`+comprehensions ~9 %,
then a long tail of `dialyzer_*`, `cerl`, `lists`, `maps` functions
at ≤ 2.5 % each.

**RabbitMQ** (signal-path artifact applies): 51 % of samples in the
signal/message-handling continuation; then `ets:lookup_element` 5.5 %,
`persistent_term:get` 2.5 %, and the *application's* hot code —
topic-trie matching, `rabbit_mqtt_processor` publish path, `mc`
message containers — as a long tail of 0.5–2.5 % entries summing to
roughly 15–20 %. The native sample's busiest beam.smp frames:
`make_internal_hash` (ETS/term hashing), the GC family,
`_platform_memmove` + `copy_struct_x` (message-send copying),
`erts_cmp_compound`, allocator internals (`aoff_*`, `mbc_free`,
delayed dealloc), ETS rwlocks, and `writev`/`read` syscalls.

**Bandit** (artifact strongest — 92 % signal continuation, server
processes are pure message/socket reactors): the native sample
carries the attribution instead: GC, `memmove`,
`erts_cmp_compound`, `erts_binary_part`,
**`do_binary_match_compile`** (a `binary:match/split` call site
compiling its pattern on every call — likely in the HTTP header
path of the Plug/Bandit stack; a one-line library fix worth
hunting), timer/allocator red-black trees, `list_to_binary_copy`.

## Reading

1. **For deployed *server* workloads, VM-internal costs dominate.**
   Port work + GC alone are 22–25 % of busy CPU; the signal/message
   delivery path, term copying on send, ETS + its locks, term
   hashing/comparison, and allocator churn make up most of the
   emulator share. JIT-addressable application code is a long tail
   of ~0.5–2.5 % functions summing to perhaps 15–25 %. Even a
   uniform 1.5× on *all* of it moves these systems ≤ 10 % — matching
   the honest-expectations row in `08` §6, now with direct
   measurement.
2. **For compute workloads (dialyzer-class), Erlang code genuinely
   dominates** (76 % emulator) and the JIT pools are real but
   fragmented: the hottest single family is ~12 % of busy CPU, and
   G3-2 showed how hard even that is to harvest. GC at 12 % is the
   single biggest *coherent* pool on this leg too.
3. **The biggest coherent pools across all three legs are
   VM-internal**: the message/signal path (large on both servers;
   exact share needs better tooling), GC (9.5–12.4 % everywhere),
   send-side term copying, ETS hashing + locking, allocator
   pressure. This is where "noticeable performance for many
   deployed systems" would have to come from — consistent with
   `07` §19's "the wins live in the runtime", G3-1's 40:1
   gen_server measurement, and the maintainer review's
   keep-an-open-mind steer.
4. **Concrete VM-internal candidates this data points at** (each
   needs its own investigation; none is JIT work):
   - the signal-queue/message-delivery path (the
     `bif_handle_signals_return` weight, the `copy_struct_x` +
     `memmove` send costs);
   - GC tuning/policy for server-shaped heaps (10–12 % steady);
   - ETS read path: `make_internal_hash` + rwlock traffic on hot
     tables (RabbitMQ routing);
   - allocator churn visible as `aoff_*`/`mbc_free`/delayed-dealloc
     under message load;
   - and one library-level find: per-call `binary:match` pattern
     compilation in the HTTP path.
5. **Tooling follow-ups** for a precise second round: a Linux
   ARM64 `perf` + `+JPperf` run (resolves JIT frames *and* C
   internals, no signal artifact — the standing offer to install
   tools applies here, on a Linux box or colima VM), and an
   extra-msacc OTP build (`--with-microstate-accounting=extra`)
   for exact send/ETS/NIF/alloc state shares.

## Second round — perf in colima, artifact-free (the precise numbers)

The macOS round's `process_info` signal artifact is gone here: Linux
`perf` samples scheduler threads directly and `+JPperf map` resolves
JIT frames by name. Run in colima (aarch64 Ubuntu 24.04, 4 vCPU,
6 GB), `cpu-clock -F 499`, 15 s window under steady load. RabbitMQ
4.3.1 on OTP 28 (the generic-unix release **boot-fails on OTP 29** —
`{horus,extraction_denied,{unknown_instruction,{line,...}}}`: Khepri's
Horus bytecode extractor can't decode OTP 29's new `line` instruction
form; worth reporting upstream). Bandit/Jason on **OTP 29** via
`Mix.install`. Publisher throttled so subscriber queues stay bounded
(the unthrottled QoS0 firehose OOM-killed the broker in the smaller
VM — itself a data point about flow control, not about CPU).

### DSO split — the headline (% of all samples)

| layer | RabbitMQ (MQTT, 75 k msg/s) | Bandit (JSON API, 83 k req/s) |
|---|---|---|
| `beam.smp` (VM internals + BIFs) | **50.8 %** | **56.7 %** |
| `[kernel]` (wakeups, softirq, I/O) | **30.4 %** | 12.7 %¹ |
| `[JIT]` (all compiled Erlang/Elixir) | **12.2 %** | **20.7 %** |
| libc | 6.2 % | 9.2 % |

¹ Bandit's pipelined keepalive client amortises syscalls over 8
requests/recv, so its kernel share is lower than a
connection-per-request load would show; RabbitMQ's per-message
socket wakeups are the real thing.

**The number the whole effort was after: JIT-compiled code — the
entire surface a second tier could touch — is 12 % of CPU on a
loaded broker and 21 % on a JSON API.** The rest is the VM and the
kernel.

### RabbitMQ — `beam.smp` is term plumbing + ETS + GC + alloc

Hottest symbols (% of all samples): `make_internal_hash` 4.6 (ETS
routing-table term hashing — single hottest in the profile), `eq`
2.9, `__aarch64_cas4_acq` 2.8 (atomics, mostly ETS rwlocks),
`copy_struct_x` 1.9 (message-send copy), `do_minor` 1.5 (GC),
`erts_cmp_compound` 1.4, `size_object_x` 1.3, `copy_shallow_x` 0.9,
the `ethr_rwmutex_*` pair ~1.6, the allocator RBT
(`rbt_delete`/`rbt_insert`/`mbc_free`/`aoff_*`) ~2.6, `garbage_collect`
0.5, `db_get_element_hash` 0.4. The **hottest JIT symbol is
`call_light_bif_shared` (1.2 %)** — the *top* of the Erlang-code
bucket is the dispatch into BIFs, not application logic. The 30 %
kernel is `__wake_up_sync_key` + `try_to_wake_up` + softirq +
`writev`/`recvfrom`: a scheduler wakeup and socket write per message.

Coherent VM pools, ranked: term hashing+compare+copy (~13 %),
ETS locking/atomics (~5 %), GC (~3 %), allocator churn (~3 %).

### Bandit — closer to JIT-friendly, and it names the wins

The Elixir JSON API is the friendlier shape: 21 % JIT, and the JIT
frames are *real application work* a T2 tier or Track A could move:

- **`Jason.Decoder:string/6` 3.7 %, `number/6` 1.2 %, `object/6`
  1.1 %, `value/5` 0.9 %** — the JSON decode scan loops, ~7 % of
  total, **exactly the G-bin shape** (Jason is hand-written binary
  matching; the same fusion that gave stdlib `json` 5.6× applies).
  `Jason.Encode:escape_json_chunk/5` 0.7 % is the encode-side
  G-bin shape.
- **`String.Unicode:upcase/3` 1.1 %** — a binary/codepoint loop.
- `i_get_map_element_hash_shared` 1.4 % + `flatmap_from_validated_list`
  0.9 % + `erts_maps_update`/`erts_gc_update_map_exact` ~0.9 % — the
  map/struct build+access path, **the G-map shape**.

But the VM still dominates even here: `do_minor` 5.5 % (the single
hottest symbol — GC), `do_binary_match_compile` 3.5 %
(**`binary:match` recompiling its pattern every call — a library
bug in the HTTP header path, not inherent**), `erts_cmp_compound`
2.5, `erts_binary_part` 2.5, the allocator RBT ~4, `erts_schedule`
2.2, `sweep_new_heap`+`full_sweep_heaps`+`garbage_collect`+
`offset_heap` ~4 more GC, `list_to_binary_copy`+`_size` ~3,
`eq`/`memcmp` ~2. GC alone across its symbols is **~12 %**.

## Consequences for the plan

- `08` §6's honest-expectations table is confirmed by direct
  measurement; no change needed there.
- The Track A / re-baseline framing stands, but this data adds the
  missing third track to evaluate **before** committing to the
  Track B build: a **VM-internal investigation** (message path, GC,
  ETS, allocators) whose ceiling on deployed-server corpora is
  several times larger than the JIT's. Same discipline as the gate
  program: pick the hottest coherent pool, build the cheapest
  experiment that prices it, then decide where the next ~24 weeks
  actually go.

### Where the next experiment should go, ranked by measured pool

The two profiles agree on the ranking and disagree only on
magnitude (server-message-heavy vs compute-heavy):

1. **GC** — 3 % (RabbitMQ) to ~12 % (Bandit), the single largest
   coherent VM pool, and the *hottest individual symbol* on the
   Elixir leg (`do_minor` 5.5 %). A GC-policy or heap-sizing
   experiment for server-shaped allocation is the highest-EV probe.
2. **Term hashing + the ETS read path** — `make_internal_hash`
   4.6 % + atomics/rwlocks ~5 % on RabbitMQ: the routing-table
   lookup cost. Pure runtime; no JIT reach.
3. **Send-side term copying** — `copy_struct_x`/`copy_shallow_x`/
   `size_object_x` ~4 % on RabbitMQ: the per-message copy between
   process heaps. The process-sharing/off-heap-message direction.
4. **Allocator churn** — the RBT (`rbt_insert`/`delete`) +
   `mbc_free`/`aoff_*` ~3–4 % both legs: carrier management under
   message/connection turnover.
5. **One library bug, free**: `do_binary_match_compile` 3.5 % on
   Bandit — a `binary:match/2` call site recompiling its pattern
   per request in the Plug/Bandit HTTP path. A cached compiled
   pattern (`binary:compile_pattern/1`) is a one-line upstream fix
   worth ~3 % of this workload, independent of everything else.

**For the JIT specifically:** Bandit confirms the *one* server-class
workload where T2 has real reach is **JSON/serialization-heavy
APIs** — Jason's decode loops (~7 %, G-bin shape) plus the map
build/access path (G-map shape) are ~10 % of total and exactly the
shapes the gate program already validated. This is the concrete
deployed-system case for the binary+map expansions, and it argues
the **Track A captures** (a scan superinstruction reaching all of
Jason; the map-key IC) deliver more of this win, to more existing
deployments, than the tier does. RabbitMQ, by contrast, has almost
no JIT-addressable pool — its cost is term plumbing and ETS, full
stop.

## Reproduction

Harnesses in this directory: `t2_prof.erl` (sampler),
`mqtt_load.erl`, `http_pipe.erl`, `bandit_server.exs`. RabbitMQ
4.3.1 generic-unix release on OTP 28.5; Bandit/Plug deps from the
census Phoenix tree; raw reports under `/tmp/prof/*.report` during
the session.
