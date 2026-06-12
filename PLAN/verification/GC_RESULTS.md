# GC lifetime instrumentation — faster GC vs less garbage

Answers the lever question raised after the profiling rounds showed
GC is the single largest coherent VM pool (5.5 % `do_minor` on
Bandit, ~12 % GC across symbols; 9.5–12.4 % of busy in msacc). The
question: do we make the GC *faster*, or make code create *less
garbage*? Run 2026-06-12.

## Method — generation-granularity lifetime, no VM build

Idea #50's full per-allocation-site lifetime histogram needs an
invasive ERTS change (parallel metadata arrays, GC-walk changes) and
a custom OTP build that RabbitMQ can't easily run on. The
*lever-deciding* signal, though, is capturable at **generation
granularity** from the existing `garbage_collection` trace events,
with no rebuild — so it runs in stock `erlang:28`/`29` containers
against the real apps. Tool: [`gc_inst.erl`](gc_inst.erl).

The metric is the **survival fraction**: of the words a process
allocates into its young heap, what fraction live long enough to be
copied at least once by a minor GC. Per minor GC, from the trace
Info proplists: `processed` = `heap_size` at `gc_minor_start` (young
words scanned), `survived` = `heap_size` at `gc_minor_end` + old-gen
delta (promotions), `reclaimed` = `processed − survived`.

**Why this answers the lever.** A copying generational collector
pays in proportion to what it *copies* (the survivors); dead words
cost nothing to reclaim. So:

- **Low survival** → most allocated words are already dead at the
  first minor GC → short-lived garbage dominates → **"less garbage"
  is the lever**, and "faster GC" has little headroom because the
  collector is *already* only copying the small live set.
- **High survival** → the GC repeatedly copies live data → **"faster
  GC / better promotion / bigger young heap" is the lever.**

Validated locally against synthetic workloads of known shape (a
fixed-live-set churner vs an unbounded accumulator): the tool
discriminated GC frequency by ~6000× and computed survival in the
expected ranges.

Loads identical to the perf rounds: RabbitMQ 4.3.1/OTP 28 under MQTT
(~75 k msg/s), Bandit+Jason/OTP 29 under pipelined JSON HTTP. Tracing
the 60 busiest processes (by reduction delta) for 12 s.

## Results

| metric | RabbitMQ | Bandit |
|---|---|---|
| **survival fraction** | **0.18** | **0.39** |
| garbage fraction | 0.82 | 0.61 |
| garbage production | 387 MB/s | 1750 MB/s |
| minor GCs /proc/s | 423 | 3 994 |
| major GCs (12 s, 60 procs) | 6 760 | **359 510** |
| major:minor ratio | ~1:45 | **~1:8** |
| major-gc survival | 0.38 | 0.19 |
| heap block words (p50) | ~2 600 (~20 KB) | ~1 600 (~12 KB) |
| dominant allocator | `proc_lib:init_p/5` (gen_server/connection procs) | `proc_lib:init_p/5` (Bandit connection procs) |

### RabbitMQ — short-lived garbage, decisively

**82 % of every word allocated into a young heap is dead by the
first minor GC.** The collector copies only 18 % of what it scans —
it is already copying-efficient. You cannot make "copy 18 % of a
20 KB heap" meaningfully cheaper, so *faster GC* buys little here.
The cost is the *frequency* (423 minor GCs/proc/s), and frequency is
driven by the allocation *rate* (387 MB/s of garbage). Cut the rate
→ cut the GCs → win. **Lever: less garbage.**

Two secondary signals:

- **6 760 major GCs in 12 s** (~9/proc/s) is high for steady-state
  server processes — major (full-sweep) collections are expensive
  and shouldn't fire this often unless something forces them. Prime
  suspect: binary-vheap pressure from the refc MQTT payloads (the
  256 B messages are off-heap binaries; the binary virtual heap
  drives its own GC trigger). Worth a `bin_vheap`-specific look —
  potentially a tuning or policy win independent of the allocation
  work.
- **Small heaps** (p50 ~20 KB) mean these processes collect very
  often. `min_heap_size`/`fullsweep_after` tuning for connection
  processes is a cheap, orthogonal lever on the *frequency* side.

### Bandit — same shape, and a major-GC pathology

Survival **0.39**: the JSON path retains more than the broker (the
decoded map/struct and the encoded response live through the
request), but **61 % is still dead at the first minor GC** — "less
garbage" is the lever here too, just with a thinner margin and a
real sliver of "faster GC / tuning" headroom the broker doesn't
have.

The standout is the **major-GC rate: 359 510 full sweeps in 12 s
across 60 procs — a major every ~8 minor GCs, with only 19 %
old-gen survival.** Full sweeps scan the entire heap *and* old
generation and are far more expensive than minors; a 1:8 ratio is
abnormal. The cause is almost certainly **binary virtual-heap
pressure**: Jason decode/encode churns refc binaries and sub-binaries
per request (`erts_build_sub_bitstring`, `list_to_binary_copy`,
`erts_binary_part` were all hot in the perf round), the `bin_vheap`
trigger fills, and refc-binary reclamation forces a major collection.
RabbitMQ shows the same signature far milder (1:45, 6 760 majors) —
its 256 B MQTT payloads are also refc binaries.

This is a distinct, nameable, possibly high-value target *orthogonal
to allocation elimination*: the binary-vheap GC-trigger policy
(`bin_vheap` sizing, `fullsweep_after`, or making small
serialization outputs heap binaries rather than refc) — a runtime/GC
investigation that neither the JIT nor compiler escape analysis
touches. It may be the single cheapest GC win surfaced this session.

## Reading — which lever, and who pulls it

Both apps agree: **majority-garbage, tiny heaps, frequency-dominated
GC** (RabbitMQ 82 % garbage, Bandit 61 %). "Less garbage" is the
primary lever on both; "faster GC" has real but secondary headroom
only on Bandit (higher survival), and a *third* lever — the
binary-vheap major-GC trigger — turns out to matter on both and is
the cheapest to test.

"Less garbage" splits into three sub-projects (ranked by leverage on
this data):

1. **Compiler/JIT allocation elimination** — the highest-value and
   the one that reconnects to the JIT thread. Escape analysis +
   unboxing + destructive update on the hot per-message path
   (message decode intermediates, the `mc`/`mc_mqtt` containers the
   perf round flagged) attacks the 387 MB/s directly. This is a
   *better* JIT motivation than the gate program's speed wins: it
   targets the dominant VM pool, not the 12–21 % code pool.
2. **Runtime: cheaper allocation / fewer collections** — off-heap or
   shared messages to kill the send-side copy (`copy_struct_x` from
   the perf round), bigger initial heaps to cut frequency, and the
   binary-vheap major-GC investigation above.
3. **Library/app: stop generating avoidable garbage** — the
   per-allocation-site worklist that idea #50's *full* instrumentation
   would produce. This generation-granularity tool says the garbage
   is short-lived and concentrated in the gen_server processes; the
   full per-site histogram is the justified next build to name the
   exact `module:line` sites.

The strategic upshot threads the whole session together: the JIT's
biggest *deployed-server* opportunity is not making code faster but
making it **allocate less** — escape analysis on the GC pool — which
is a stronger second-tier motivation than anything the speed gate
program found, and squarely a compiler/JIT project rather than a
runtime one.

### Ranked next experiments (the cheapest first)

1. **Binary-vheap major-GC trigger** (runtime, smallest). Both apps
   full-sweep far more than expected (Bandit 1:8 major:minor). Price
   it: vary `bin_vheap`/`fullsweep_after`/`min_bin_vheap_size` on the
   Bandit leg and measure the major-GC rate and GC busy %. If the
   majors collapse, this is a near-free win on every binary-heavy
   server. No JIT, no compiler.
2. **Full per-allocation-site lifetime histogram (idea #50)** — the
   instrumentation this run proxied. Now justified: the garbage is
   short-lived *and* concentrated in `proc_lib` (gen_server/connection)
   processes, so the per-site histogram would name the exact
   `module:line` allocation sites to attack. The build cost (VM
   metadata arrays) is warranted because the lever is confirmed.
3. **Compiler/JIT escape analysis + unboxing** on the hot per-message
   / per-request path — the largest but heaviest lever, and the one
   that redirects the second-tier work at the dominant deployed-server
   pool.

## Reproduction

`gc_inst.erl` + the colima leg scripts `rabbit_gc.sh` / `bandit_gc.sh`
in this directory. `gc_inst:run(TopN, DurMs)` loaded into any running
node (RabbitMQ via `rabbitmqctl eval`, Bandit via the server script)
prints the report.
