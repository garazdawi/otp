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

- **6 760 major GCs in 12 s** (~9/proc/s) — milder than Bandit's
  storm, and same cause: RabbitMQ's own hibernate / `garbage_collect`
  calls in the channel/queue/reader/MQTT processes (see the
  Correction below). Not binary-vheap pressure (falsified there).
- **Small heaps** (p50 ~20 KB) mean these processes collect very
  often. Bigger `min_heap_size` for connection processes is a cheap
  lever on the minor-GC *frequency* side (Experiment 3 below).

### Bandit — same shape, and a major-GC pathology

Survival **0.39**: the JSON path retains more than the broker (the
decoded map/struct and the encoded response live through the
request), but **61 % is still dead at the first minor GC** — "less
garbage" is the lever here too, just with a thinner margin and a
real sliver of "faster GC / tuning" headroom the broker doesn't
have.

The standout is the **major-GC rate: 359 510 full sweeps in 12 s
across 60 procs — a major every ~8 minor GCs.** See the Correction
below: this is **not** a VM trigger — Bandit calls
`:erlang.garbage_collect()` deliberately, every 5 keepalive requests.

## Correction (validated 2026-06-15) — the major GCs are app-requested

An earlier draft of this file blamed binary virtual-heap pressure for
the major-GC storm. **That hypothesis is wrong, and was falsified by
experiment.** The real cause is deliberate application-level GC.

**Experiment 1 — bin_vheap is NOT the cause** (synthetic
[`binchurn.erl`](binchurn.erl), repo OTP 29, a worker churning 41
refc sub-binaries + a map per iteration — the Jason pattern):

| `min_bin_vheap_size` | major GCs (4 s) |
|---|---|
| default (46 422 w) | 38 929 |
| 4 194 304 w (90×) | 37 108 |

A 90× bigger binary vheap left the major rate unchanged. Binary-vheap
pressure does not drive the majors.

**Experiment 2 — `fullsweep_after` is NOT the cause** (same worker):
setting `fullsweep_after` to its max (65 535) still gave 37 177
majors. The majors are not generation-count driven; they come from
the `F_NEED_FULLSWEEP` *escalation* path (`erl_gc.c:808` — a minor
that can't make room escalates to a full sweep), which is itself a
function of **heap size**, not binaries.

**Experiment 3 — heap size collapses the escalation majors** (same
worker):

| `min_heap_size` | minor GCs/s | major GCs (3 s) |
|---|---|---|
| 233 w (tiny) | 224 000 | 29 267 |
| 2 584 w | 88 000 | **4** |
| 46 422 w | 9 000 | **0** |

Bigger heaps eliminate the escalation majors entirely and cut minor
frequency ~25×. So *the synthetic's* majors were tiny-heap
escalation — a real mechanism, but still not Bandit's cause.

**Experiment 4 — the real cause, found by reading the source and
validated on the real app.** `bandit/http1/handler.ex:28` calls
`:erlang.garbage_collect()` (a forced full sweep) every
`gc_every_n_keepalive_requests` keepalive requests — **default 5**.
Raising that option to 100 000 on the real Bandit workload:

| `gc_every_n_keepalive_requests` | major GCs (12 s, 60 procs) |
|---|---|
| 5 (default) | 359 510 |
| 100 000 (effectively off) | **12 747** — a 28× collapse |

The major-GC storm is Bandit deliberately bounding per-connection
memory across long-lived keepalive connections. **RabbitMQ does the
same**: `garbage_collect` appears in `rabbit_channel`,
`rabbit_amqqueue_process`, `rabbit_reader`, and the MQTT handler (the
standard hibernate / explicit-GC-to-release-binaries pattern), which
is why its majors are milder (1:45) — fewer forced points than
Bandit's every-5-requests.

**Corrected lever picture:**

- **Major GCs are application-requested**, not a VM pathology. They
  trade CPU for bounded memory in long-lived connection processes,
  primarily to release retained refc binaries. The lever is *library
  configuration* (Bandit's `gc_every_n_keepalive_requests`,
  RabbitMQ's hibernate policy) — a memory-vs-CPU tradeoff, not a VM
  change. With Bandit's default of 5, a meaningful slice of the
  measured GC % is self-inflicted and tunable.
- **Why they force GC at all** loops back to binary retention in
  long-lived processes — so "make binary handling cheaper / retain
  less" (the allocation/runtime story) would reduce the *need* for
  forced GC, which is the deeper, non-config lever.
- **Minor-GC frequency** is the genuine, non-requested cost, and it
  is driven by allocation rate + heap sizing (Experiment 3). "Less
  garbage" + bigger `min_heap_size` for hot processes are the levers
  there.

## Reading — which lever, and who pulls it

Both apps agree: **majority-garbage, tiny heaps, frequency-dominated
GC** (RabbitMQ 82 % garbage, Bandit 61 %). "Less garbage" is the
primary lever for the *minor*-GC cost; the *major*-GC cost is
application-requested and tuned at the library level (the Correction
above). "Faster GC" has real but secondary headroom only on Bandit
(higher survival).

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
   the perf round), and bigger initial heaps to cut minor frequency
   (Experiment 3: `min_heap_size` cut minor GCs ~25×).
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

### Per-site attribution attempt (existing infra) — insufficient

I tried to name the `module:fun/arity` garbage sites with the
existing **`+M<S>atags code` + `instrument:allocations([per_mfa])`**
infrastructure (binary/eheap/ETS allocators), which the agent
investigation showed can attribute `erts_alloc`-backed allocations
to their origin. Run on both apps under load ([`gc_sites.erl`](gc_sites.erl)):

- **Bandit**: top origins were `code_server`, `Module.ParallelChecker`,
  `prepare_loading` — Mix.install's persistent compilation data. The
  request garbage (`Jason.decode`, `Bandit.Adapter:read_req_body`,
  `unicode:characters_to_binary`) appeared but buried and small.
- **RabbitMQ**: top origins were ETS tables (`db_tab`/`db_term`) and
  startup infra (`ra_*`, `rabbit_disk_monitor`, `code_server`). The
  hot message-path binaries were absent.

**Conclusion: existing infra cannot do garbage attribution.**
`instrument:allocations` is a *live snapshot* — it reports what's
allocated *right now*, dominated by long-lived/persistent data (ETS,
code, startup). Short-lived churning garbage — exactly the 387 MB/s
we want to attribute — is allocated and freed *between* samples, so
it's structurally invisible. This is a real limitation, confirmed on
both apps, not a tuning issue. The cheap path for #2 does not exist.

### Ranked next experiments (the cheapest first)

1. **Bandit/RabbitMQ forced-GC tuning** — *already done* for Bandit
   (`gc_every_n_keepalive_requests`, default 5 → the major storm;
   raising it cut majors 28×). Actionable now: the default is
   aggressive for pipelined workloads; a higher default or
   per-deployment tuning trades a few % CPU for per-connection
   memory. A library-config win, upstreamable to Bandit, no VM work.
2. **Full per-allocation-site lifetime histogram (idea #50)** — now
   *required*, not just justified: the existing-infra path above is
   structurally insufficient, so naming the churn sites genuinely
   needs the GC-time per-term instrumentation (metadata arrays +
   copy-loop walk). It is a large, invasive ERTS change (multi-day,
   risk in the GC copy path) — to be scoped and built deliberately,
   not rushed. It remains the prerequisite for the targeted
   allocation-elimination work below.
3. **Compiler/JIT escape analysis + unboxing** on the hot per-message
   / per-request path — the largest lever, redirecting the
   second-tier work at the dominant deployed-server pool. Gated on
   #2 to know which sites to target.

## Reproduction

`gc_inst.erl` + the colima leg scripts `rabbit_gc.sh` / `bandit_gc.sh`
in this directory. `gc_inst:run(TopN, DurMs)` loaded into any running
node (RabbitMQ via `rabbitmqctl eval`, Bandit via the server script)
prints the report.
