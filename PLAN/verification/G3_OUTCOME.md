# G3 experiment — outcome (subject 2: mutual/structural recursion)

The call-crossing gate from
[`../T2/08_v1_loop_tier.md`](../T2/08_v1_loop_tier.md) §6.1, subject 2:
hand-write a T2 specialisation of `erl_types:are_all_limited/2` +
`is_limited/2` — the shape that carried **47 % of all calls** in the
dialyzer PLT-build census ([`RESULTS.md`](RESULTS.md) §3) — and
measure whether call-crossing optimization moves it. Run 2026-06-12,
MVP methodology (hand-written asmjit body on the MVP scaffolding,
single build, mode selected via the `T2_G3` env var).

## Verdict

**The call-crossing infrastructure is NOT green-lit by this
evidence.** One-level callee inlining wins big at micro scale
(2.25× on leaf-biased spines) but moves the real workload **0 ± 1 %
CPU** — because the 47 %-of-calls family turns out to hold only a
few percent of cycles, and the real element distribution sets leaf
gains against container losses. The call-crossing structure *by
itself* (stage a) is a wash or a small loss. Two architecture
lessons below are worth more than the headline number.

## What was built

`emit_t2_are_all_limited_2` (`arm/beam_asm_module.cpp`), on the MVP
hook, env-gated so one build provides all modes:

- **T2_G3=off** — clean T1 baseline.
- **T2_G3=a** — recovered spine; loop state (`Tl`, `K`) in
  registers; *every* `#c{}` element handled by a **real call** to
  T1 `is_limited/2` that returns into the T2 loop (state crossing
  the call via two Y slots). This is the exact structure v1
  forbids (CPs into the blob) and the deferred infrastructure
  exists to enable.
- **T2_G3=b** — stage a + the `is_limited/2` dispatch *header*
  inlined: `any`, `K =< 0`, non-`#c{}` catch-all, and leaf tags
  resolve without calling; only true containers
  (tuple/union/list/…) call. A 7-leaf-tag variant bounds the
  best case.

Iteration-start-stays-live holds throughout (`XREG0` = current cons
until the element resolves; side-exits land at the T1 body with
pristine args; low-stack before the call side-exits and lets T1's
allocate run the GC). Reductions: 3/element (T1's two calls + one
return), so scheduling is never more generous than T1.

## Correctness

4 270-result corpus (random terms via `t_from_term` incl. bignums,
maps, deep nesting; structured leafy/mixed/deep corpora;
K ∈ {-1, 0, 1, 2, 3, 6, 50}): hash `3191953412` **identical across
off/a/b** and equal to the pre-change baseline. PLT builds complete
normally in all modes.

## Measurements (Apple Silicon, this repo's build)

Micro (`t2_g3:bench/1`, min of 15, µs; `t_limit/2` over 200 passes
of each corpus):

| corpus | element mix | off | a | b (3 tags) | b speedup |
|---|---|---|---|---|---|
| leafy | 100 % leaf `#c{}` | 686 | 656 | **305** | **2.25×** |
| mixed | ~75 % leaf | 1263 | 1386 | **931** | **1.36×** |
| deep | ~33 % leaf | 1391 | 1428 | 1435 | 0.97× |

The 7-tag variant: leafy 320, mixed 990 — slightly *worse* than 3
tags (extra compares), and it removed only 2 M of the remaining
100 M calls ⇒ **the residual calls are true containers**; the
inline set was already near-optimal.

Real workload (dialyzer PLT build over the compiler app):

| metric | off | a | b |
|---|---|---|---|
| wall, `+S2` (min of 3) | 7721 ms | 7908 ms | 7699 ms |
| wall, `+S10` (5 reps, median) | ~3.25 s | ~3.25 s | ~3.24 s |
| **CPU** (`statistics(runtime)`, 5 reps, median) | 14 356 ms | 14 424 ms | **14 366 ms** |

Engagement on the real workload (cprof): spine re-entries
280.9 M → 50.7 M (−82 %); `is_limited` calls 264.5 M → 98.2 M
(−63 % — i.e. ~63 % of real elements resolve in the inline header).
The specialization is doing exactly what it was designed to do —
and the CPU doesn't move.

## Why the micro win vanishes end-to-end

1. **Call counts overstated the cycle pool by ~an order of
   magnitude.** 2.4 B calls in ~14 s of CPU means the *average*
   call in this workload is ~20 cycles — `bl`/`ret` perfectly
   predicted, the 1-instruction frame push store-forwarded.
   BeamAsm's calling convention is already lean enough that
   tiny-body call overhead is a much smaller pool than the census's
   call-share suggested. The family's true CPU share is a few
   percent, not tens.
2. **The real element distribution (63 % leaf) sets gains against
   losses.** Leaf elements get ~2× cheaper; container elements get
   *more expensive* (inline-miss compares + stack guard + Y-slot
   save/restore + `true`-compare on top of the call they were
   always going to make). The deep corpus shows the loss case
   (0.97×); the real mix nets out near zero.

## Architecture lessons (the valuable part)

1. **Stage a's null result is evidence *against* the expensive
   half of the deferred infrastructure.** The win, where there is
   one, comes from *eliminating* calls (inlining the callee's
   header), not from keeping loop state in registers across calls.
   CPs-into-blobs, framestates, and the stack-scan lifecycle exist
   to enable the latter.
2. **Demote-on-return ≈ return-into-T2 for self-tail-recursive
   spines.** Had the per-container call pushed the *T1
   continuation* as CP (the v1 S3 rule), T1 would execute one
   `andalso` check + the self tail call, which re-enters T2
   through the patched prologue for the very next element. For
   the dominant Erlang loop shape, v1's no-CPs-into-blobs
   simplification costs one element's worth of T1 per call-bearing
   element — nearly nothing. The experiment needed CPs into the
   blob only because the MVP scaffolding has no prologue patch to
   re-enter through cheaply; production v1 does.
3. **Census methodology correction**: call-count weighting must not
   be used to size win pools — it over-weights tiny-body functions.
   Only cycle profiling (the planned perf leg) identifies pools.
   The static census's *structural* conclusions (what shapes exist)
   stand; its implied importance ranking does not.

## Gate disposition

- **Subject 2 (this experiment): no win.** General inlining with
  framestates + eager-CP + CP/stack-scan lifecycle is *not*
  justified by dialyzer-class mutual recursion. The ~25 deferred
  weeks stay deferred.
- **Subject 1 (branchy dispatch / cold-arm pruning) remains open**
  — it targets a different mechanism (redundant-check elimination,
  not call overhead) and is unaffected by this result. It should
  still run before any branchy-corpus investment, with
  cycle-profiling first to size its pool.
- **G-bin is now clearly the highest-value open gate.** Binary
  match loops are the one place T1 *demonstrably* pays heavy per-op
  overhead (the match-context dance), unlike call overhead, and
  RabbitMQ's corpus is 46 % binary-touching loop functions.

Caveats: one workload, one machine (Apple Silicon big cores — small
in-order cores price calls differently; re-check on Graviton when
the Linux ARM64 leg exists), `statistics(runtime)` granularity, and
a hand-written specialisation may under-represent what a production
optimizer could do across *many* such functions at once (per-blob
constant pools would cut the inline-miss cost, for instance).

## Reproduction

Experiment code is in-tree, env-gated, off by default:
`arm/beam_asm_module.cpp` (`emit_t2_are_all_limited_2`,
`t2_g3_mode`), `arm/instr_common.cpp` (label record),
`arm/beam_asm.hpp`. Harnesses: [`t2_g3.erl`](t2_g3.erl) (micro +
correctness), [`g3_cpu.erl`](g3_cpu.erl) (PLT wall+CPU),
[`g3_cprof.erl`](g3_cprof.erl) (engagement counts).

```bash
make -j$(sysctl -n hw.ncpu)
./bin/erlc -o /tmp/g3 PLAN/verification/t2_g3.erl
for m in off a b; do
  T2_G3=$m ./bin/erl -noshell -pa /tmp/g3 -pa lib/dialyzer/ebin \
      -eval 't2_g3:check(), t2_g3:bench(15), halt().'
  for i in 1 2 3 4 5; do T2_G3=$m ./bin/escript PLAN/verification/g3_cpu.erl; done
done
```
