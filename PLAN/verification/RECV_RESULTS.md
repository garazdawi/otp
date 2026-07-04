# M0.R — Receive-path classification (does `receive` need first-class IR?)

> Measurement for **T2FULL open question 1**
> ([`../T2FULL/06_phases.md`](../T2FULL/06_phases.md) §Open questions):
> `receive` currently stays AOT-owned (App C resolution) and T2 compiles
> only the non-receive windows. A future tier could instead give
> `receive` first-class IR and compile a **fast path for the case where a
> matching message is already in the queue when the receive is entered**
> (no scheduler round-trip). The value of that idea is proportional to
> how often that case actually occurs on real workloads. This document
> measures it.
>
> Project owner's framing, verbatim intent: *"the receive statement could
> be great to have in the IR for when there is a message in the queue —
> but measure to be sure."*

## 1. What a T2 receive fast path can and cannot touch

A receive-in-IR fast path can only ever help the **HIT** fraction of
receive instances (message already matchable without suspending), and
within a hit it can only remove the cost that is **not the match
itself**:

- **can touch**: the msg-queue lock/peek, `F_DELAY_GC` set/clear, the
  `enter/leave_runtime` bracketing around the `loop_rec` /
  `remove_message` helpers, `loop_rec` dispatch per scanned message, the
  y-register traffic of the receive loop, `remove_message` bookkeeping.
- **cannot touch**: the pattern/guard **match** (that work exists in any
  design), the message term, GC of the consumed message, and **every
  `wait_match` / `timeout` instance** (those genuinely had no matchable
  message and paid a real scheduler round-trip or after-clause).

So the upper bound on the idea is
`hit% × (per-hit non-match overhead) × (receive's share of workload
cycles)`. This document measures the first factor exactly, the scan
depth that prices the second, and reasons about the third against the
already-measured cycle profiles.

## 2. Method

### 2.1 Counters (exact semantics)

Always-on, per-scheduler, cache-line-padded counters in ERTS
(`erts/emulator/beam/erl_process.c`, commit *"erts: receive-path
classification counters (T2FULL M0.R)"*). A **receive instance** is one
execution of a receive statement by a process, from the first
message-queue inspection until it either matches+removes a message or
gives up via an after-clause. Every instance terminates in **exactly
one** of two C helpers on the BeamAsm path —
`beam_jit_remove_message` (match) or `beam_jit_timeout` (abandon) — and
is classified there from two words of per-process scratch:

| field | set where | meaning |
|---|---|---|
| `recv_waited` | `beam_jit_wait_locked` | the process suspended on an empty/exhausted queue in this instance (a genuine scheduler round-trip). Reduction-preemption yields do **not** pass through this helper, so they stay classified as hits. |
| `recv_scanned` | `erts_msgq_set_save_next` (one per `loop_rec_end`) | number of non-matching messages advanced past so far in this instance. |

Both are read, bucketed into the running scheduler's own slot, and
cleared at the terminal. Buckets:

| bucket | definition |
|---|---|
| `hit_first` | matched, **never waited**, scan depth 1 (matched the very first message inspected) |
| `hit_scan_2_4` | matched, never waited, scan depth 2..4 |
| `hit_scan_5p` | matched, never waited, scan depth ≥ 5 |
| `wait_match` | matched only after ≥ 1 suspend on an empty/exhausted queue |
| `timeout_imm` | after-clause fired, never waited (e.g. `after 0`) |
| `timeout_waited` | after-clause fired after having waited (`after N` elapsed) |
| `msgs_scanned` | total non-matching messages advanced past, all instances |

**hit** = `hit_first + hit_scan_2_4 + hit_scan_5p`.
**instances** = `hit + wait_match + timeout_imm + timeout_waited`.

Readout / reset via the existing debug channel (galloc profiler
pattern):
`erts_debug:get_internal_state(recv_stats)` → proplist of totals;
`erts_debug:set_internal_state(recv_stats, _)` → reset.

Cost: one non-atomic add on the per-message scan path
(`erts_msgq_set_save_next`), a store on the wait path, and a handful of
instructions at each terminal — always on, no start flag.

### 2.2 Scope / honesty caveats

- Terminal counting is in the **BeamAsm** helpers (default on aarch64);
  the interpreter path is not instrumented and not used. `recv_scanned`
  is bumped in a shared inline, so it is exact for the JIT path.
- **Reduction-preemption mid-scan is a hit** (no empty-queue suspend
  occurred). This is the semantically correct choice for the "no
  scheduler round-trip needed" question, and is negligible in the data
  (deep-scan hits are ~0 except Bandit).
- Counters are global-per-scheduler; background OTP system-process
  receives are included but are negligible at these volumes (see the
  validation block: background is 10¹–10² instances against 10⁵–10⁸ of
  workload).
- Harness: [`recv_bench.erl`](recv_bench.erl), [`recv_srv.erl`](recv_srv.erl),
  [`bandit_recv.exs`](bandit_recv.exs) + [`bandit_recv.sh`](bandit_recv.sh)
  (drives the existing [`http_pipe.erl`](http_pipe.erl)).
  Host: Apple Silicon (aarch64-apple-darwin), OTP 29 dev
  (`lukas/erts/beamjit2`), BeamAsm.

### 2.3 Validation (proves the classifier is exact)

Controlled receive shapes, each isolating one bucket:

| case | expected | measured |
|---|---|---|
| V1: 100 000 × `receive after 0` on empty queue | all `timeout_imm` | `timeout_imm` = 100 000 (99.99 %) |
| V2: pre-queue 100 000 msgs, receive-all (each matches first) | all `hit_first` | `hit_first` = 100 001 (100 %) |
| V3: synchronous ping-pong ×50 000 (receiver blocks each time) | all `wait_match` | `wait_match` = 99 358 (99.36 %) |
| V4: 20 000 × selective receive past 6 junk (depth 7) | `hit_scan_5p`; `msgs_scanned` = 6×20 000 | `hit_scan_5p` = 20 000; `msgs_scanned` = 120 000 (exact) |

The classifier is exact and the scan-depth split is exact.

## 3. Results

Per-workload, counters reset immediately before the measured window.

| workload | instances | hit % | hit_first % | deep-scan (≥5) % | wait_match % | timeout % | avg non-match scanned/inst | throughput |
|---|---:|---:|---:|---:|---:|---:|---:|---|
| **W1** gen_server, 64 clients (calls) | 9.70 M | **49.98** | 49.98 | 0.00 | **50.02** | 0.00 | ~0 | 965 k calls/s |
| **W2a** loaded ring, 200 procs / 4 000 tokens | 155.9 M | **99.38** | 99.38 | 0.00 | 0.62 | 0.00 | ~0 | 31.2 M hops/s |
| **W2b** burst flood → sink (saturated) | 8.00 M | **99.75** | 99.75 | 0.00 | 0.25 | 0.00 | ~0 | 11.1 M msg/s |
| **W3** Bandit JSON API, 32 conns × depth-8 | 5.39 M | **89.92** | 84.66 | **5.26** | 4.82 | 5.26 | 0.79 | 193 k req/s |
| **W4** dialyzer `--build_plt` (compute) | 0.124 M | **18.70** | 18.50 | 0.20 | **81.29** | 0.00 | 0.05 | 7.0 s run |

Notes per workload:

- **W1 (gen_server under load).** Exactly **2 receive instances per
  call**, split cleanly 50/50: the server's request-receive is a
  100 %-hit (there is always a backlog under 64 concurrent callers), and
  each client's reply-receive inside `gen_server:call` is a 100 %-wait
  (it just sent the request; the reply is a synchronous round-trip and
  is **structurally unhittable**). This is the definitive gen_server
  signature and it caps the hittable share of a call/reply server at
  **50 %** by construction.
- **W2a/W2b (messaging).** Loaded ring 99.4 %, saturated sink 99.8 %.
  This is the hit-rate ceiling — pure message-forwarding with a standing
  backlog. Scan depth is 1 throughout.
- **W3 (Bandit / serialization service).** 89.9 % hit — **and the only
  workload with real scan depth**: 5.26 % of instances are hits that
  scanned ~15 messages each (4.25 M messages scanned over the 12 s
  window). This is a genuine per-socket-read **selective receive** in the
  Bandit/thousand_island handler path (≈ one deep scan per depth-8
  pipelined batch). The `recv_marker` optimization already collapses the
  ref-based selective receives to depth-1; this residual scanning is what
  it does not cover.
- **W4 (dialyzer / compute app).** 18.7 % hit and **81.3 % wait** — the
  coordinator/worker pattern blocks waiting for results. Decisive point:
  the **absolute** receive volume is negligible — 124 k instances over a
  **7 s CPU-bound** run (vs. 10⁷–10⁸/s in the messaging workloads).
  Receive is simply not on dialyzer's hot path.

## 4. Reading — does this justify receive-in-IR?

**No — the measurements support keeping `receive` a region-terminator.**
The reasoning is a conjunction test, because a high hit rate is
necessary but nowhere near sufficient.

A receive-in-IR fast path pays only where **all three** hold at once:

1. **high hit %** — the fast path only touches hits;
2. **high receive-instance density on the hot path** — otherwise the
   fixed per-hit saving is multiplied by ~nothing;
3. **the workload is in the JIT-addressable compute class** — otherwise
   the receive/message path is VM-internal cycles a JIT cannot reach
   ([`../T2FULL/00_goal_and_thesis.md`](../T2FULL/00_goal_and_thesis.md)
   §3.4: "message/signal path … a JIT cannot reach them").

The data shows these three are **anti-correlated across the corpus**:

- **Messaging (ring/flood/gen_server-server-side): has (1), fails (3).**
  99 %+ hit, but this is exactly the class the plan already prices at
  single digits — RabbitMQ caps a JIT at 1.14× (§1.1), and receive-path
  fixed cost is VM-internal, not [JIT] Erlang-execution. Shaving a few
  instructions off already-minimal T1 helpers on a 12 %-addressable
  workload is deep sub-1 % end-to-end.
- **Compute app (dialyzer): has (3), fails (2).** 124 k receive
  instances over 7 s of compute — even 100 % hit × total elimination of
  the receive path is invisible against the run.
- **gen_server call/reply: capped at 50 % hit by construction.** Half of
  every request/reply exchange (the client's reply-receive) is a
  synchronous round-trip whose reply is genuinely not yet in the queue.
  No fast path can hit it.
- **Serialization service (Bandit): the one class with (1)+partial-(2)
  via scanning** — 89.9 % hit *and* real selective-receive depth. This is
  the plan's ≥10 % target class (§1.1 line 3). But sizing the reachable
  work: the 4.25 M scanned messages over 12 s = **354 k `loop_rec`
  dispatches/s**; even at a generous ~30 saved cycles per dispatch that
  is ~10 M cycles/s ≈ **0.3 % of one core** (spread over several
  schedulers → less), and the per-hit fixed overhead on the 4.85 M hits
  is already minimal helper code. Sub-1 % end-to-end.

**What hit-rate would justify it?** There is no hit-% threshold that flips
the decision on its own — it is the (1)∧(2)∧(3) conjunction that fails.
Concretely, receive-in-IR would clear the bar only if a **compute-class,
JIT-addressable** workload were found that spent a **material cycle
share** (say ≥ a few percent) in receive-path *fixed overhead* at a high
hit rate. Nothing in this corpus (or the plan's existing profiles) does:
where the hit rate is high the cycles are VM-internal; where the cycles
are JIT-addressable the receive density is negligible.

### 4.1 Implication for open question 1

Resolve open question 1 as the plan currently has it (App C):

> **`receive` stays AOT-owned / a region-terminator. T2 compiles the
> non-receive windows of receive-enclosing loops; it does not add
> first-class receive IR.**

This is the right call for gen_server-shaped servers too: their hot cost
is not the receive dispatch (the server-side hit is a single `loop_rec`
+ `remove_message`, depth 1), and their latency-bound half (the client
reply-receive) is an unhittable round-trip. Compiling the handler body
(the non-receive window) — which the region-terminator model already
does — captures everything a JIT can capture there.

### 4.2 The one hedge worth recording

Bandit is the only signal that a *narrow* future optimization could
exist, and it is **not** receive-in-IR: it is **`loop_rec`-scan
ownership** for the residual selective-receive that `recv_marker` does
not cover — registerizing the scan cursor and hoisting the
`enter/leave_runtime` out of the per-iteration `loop_rec_end`, i.e. the
same "loop owns its scan" mechanism the binary-scan subset
([`../T2FULL/03_optimizer.md`](../T2FULL/03_optimizer.md) §Pillar 1)
already uses. That would belong to **Pillar 1 (fused loops)**, gated on
an **M0.6 cycle profile** actually showing the Bandit selective-receive
scan as a measurable share (this document measured its *instance count*
and *depth*, not its cycle share). It does not require, and does not
argue for, making `receive` first-class IR.

## 5. Reproduce

```
# build the counter-patched emulator (from the worktree root)
./otp_build setup -a && make -j$(sysctl -n hw.ncpu) && make local_setup

# validation + workloads 1,2,4
cd PLAN/verification
../../bin/erlc recv_srv.erl recv_bench.erl
../../bin/erl -noshell -pa . -eval 'recv_bench:validate(),
  recv_bench:w1_genserver(64,5000),
  recv_bench:w2_ring(200,4000,5000),
  recv_bench:w2_flood(200000,40),
  recv_bench:w4_dialyzer("/tmp/recv_m0r.plt"), halt().'

# workload 3 (Bandit; Elixir on the custom OTP, Mix.install fetches deps)
bash PLAN/verification/bandit_recv.sh
```
