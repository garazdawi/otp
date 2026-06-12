# T2 MVP Plan

Stop planning the universe. Pick the smallest experiment that retires
the central question — *can this architecture produce code that beats
T1 at all?* — and ship it. Everything else in `T2.md` is conditional
on the answer being yes.

## The bet

T2 inlines a small helper across a hot tail-recursive loop that walks
a list of 2-tuples. T1 cannot inline. The expected speedup on the
benchmark is 2–5×, dominated by:

1. Elimination of the per-iteration call frame for the helper.
2. Fusion of the redundant tag checks T1 emits per BEAM op (list
   pointer, tuple pointer, tuple arity, both elements, accumulator).
3. Direct fixnum arithmetic on detagged values, with the accumulator
   held in a register across iterations rather than reloaded from
   the X-register save area on every call boundary.

If T2 cannot reach **2×** on this benchmark, the architectural
assumptions in `T2.md` need revisiting before any further investment.

## The benchmark

```erlang
-module(t2_mvp).
-export([run/1]).

-compile({t2_assume_smallints, [{total, 2}, {diff, 2}]}).

run(N) ->
    %% Setup: build a list of {Amount, Fee} pairs.
    %% mk_txns is *not* T2-compiled — runs in T1.
    Txns = mk_txns(N, []),
    total(Txns, 0).

mk_txns(0, Acc) -> Acc;
mk_txns(N, Acc) -> mk_txns(N - 1, [{N, N rem 7} | Acc]).

%% Hot loop: walk the list, accumulate (Amount - Fee).
total([], Net) -> Net;
total([{A, F} | Rest], Net) ->
    total(Rest, Net + diff(A, F)).

diff(A, F) -> A - F.
```

Per iteration of `total/2`, three real BEAM operations run that all
of T1's per-op cost lands on:

1. Cons-cell decomposition (head/tail).
2. Tuple destructuring (extract two elements from a 2-tuple).
3. Non-tail function call to `diff/2`.

Plus the tail-call back to `total/2` and the accumulating add. This
is much closer to typical Erlang than a pure arithmetic loop —
processing a list of small records is one of the most common BEAM
shapes in production code (kv-lists, event streams, transaction
ledgers, parser tokens).

`run(1_000_000)` keeps every value within the 60-bit fixnum range,
so the hot loop touches no bignums and triggers no GC.

### Why this benchmark exposes T1's ceiling

Per iteration, T1 emits roughly:

```arm
total_t1_entry:
  subs REDS, REDS, #2
  b.le yield_stub
  ;; is_nonempty_list + get_list   (tag check + two loads)
  ;; is_tuple + test_arity 2       (tag check, header load, arity compare)
  ;; get_tuple_element × 2         (two loads, untagged via offsets)
  ;; call diff(A, F)               — full call frame
  mov  X[0], X(A)
  mov  X[1], X(F)
  stp  fp, lr, [sp, #-32]!
  bl   diff_t1_entry
  ldp  fp, lr, [sp], #32
  ;; gc_bif2 '+' Net, ret          (tag check on both, add, overflow→slow path)
  ;; tail to total_t1_entry        (X-reg flush already implicit at call boundary)
  ...
  b    total_t1_entry
```

Most of the per-iteration cost is:

- The `bl diff_t1_entry` round-trip and its prologue/epilogue.
- Six independent tag checks (list, tuple, arity, A, F, accumulator),
  each emitted by a separate BeamAsm template — there is no IR for
  the JIT to see across them.
- X-register flush across the call.
- Net reloaded from the X-register save area on every iteration.

T2 with `diff/2` inlined and the `t2_assume_smallints` directive:

```arm
total_t2_entry:
  ;; 2 reds/iter to match T1 (diff call + total tail call).
  subs REDS, REDS, #2
  b.le yield_stub                  ; R(list), R(net) = iteration-start

  ;; cons-cell: load head and tail (R(list) already untagged at entry).
  ldr  T(pair), [R(list), #0]
  ldr  T(rest), [R(list), #8]

  ;; tuple guard: combined header+arity check.
  ldr  T(hdr), [T(pair), #-BOXED_TAG]
  cmp  T(hdr), #TUPLE_ARITY_2_HDR
  b.ne sideexit

  ;; both elements; combined fixnum check on (A | F).
  ldr  T(a), [T(pair), #8 - BOXED_TAG]
  ldr  T(f), [T(pair), #16 - BOXED_TAG]
  orr  T(c), T(a), T(f)
  and  T(c), T(c), #FIXNUM_TAG_MASK
  cbnz T(c), sideexit

  ;; inlined diff(A, F) + accumulate, all in temps so iteration-start
  ;; values stay live in R(list), R(net).
  asr  T(ar), T(a), #FIXNUM_SHIFT
  asr  T(fr), T(f), #FIXNUM_SHIFT
  subs T(d),  T(ar), T(fr)
  b.vs sideexit
  adds T(nn), R(net), T(d)
  b.vs sideexit

  ;; commit
  mov  R(net),  T(nn)
  cmp  T(rest), #NIL_TAGGED
  b.eq done
  mov  R(list), T(rest)
  b    total_t2_entry
```

Win sources, in expected order of magnitude:

- The `bl diff_t1_entry` round-trip is gone (call frame, X-reg
  flush, prologue/epilogue, `bl`+`ret`).
- Six T1 tag checks collapse into two: one combined header-and-arity
  compare, one combined fixnum check on `(A | F)`.
- Net stays in `R(net)` across iterations; no per-iteration reload.
- Branch predictor learns the `b.vs` overflow side-exits and the
  `b.ne` shape-mismatch side-exits as not-taken, so the fast path
  flows linearly.

## Non-goals

The MVP exists to demonstrate the speedup, nothing else. These are
explicitly out of scope:

- Auto tier-up. The user adds the `t2_assume_smallints` attribute by
  hand.
- Profile collection. Type assumption is hardcoded by the attribute.
- Polymorphic specialisation. Side-exit on any deviation.
- A general SSA IR. Reuse `beam_ssa` and bolt a side-table for the
  fixnum assumption onto it.
- Lazy stack scan, generation tracking, deferred frame reification.
- DOMJIT-style guard BIFs.
- Tracing-aware inlining. (Will need to be addressed before T2 is
  ever turned on for non-experimental use, but it's not what the
  MVP is testing.)
- Map and record support. Only cons-cells, 2-tuples, fixnums.
- BIF support in the hot path. `mk_txns/2` calls `rem`; that's setup
  only and runs in T1.
- Multiple benchmarks. One is enough to retire the question.

## Architecture (minimum viable)

### Trigger
`-compile({t2_assume_smallints, [{F, A}, …]})`. Compiler backend emits
both T1 and T2 versions of listed functions. T2 entry is the public
dispatch target; T1 is reached only via side-exit.

### Frame layout
Identical to T1. Side-exit is "spill raw values back to tagged terms
in the standard X-register save area, then jump to T1 function
entry." Trades performance (extra spills at deopt) for infrastructural
simplicity (no separate frame walk, no register-map metadata).

### Type model
Three baked-in assumptions, all enforced by guards at function entry
and at structural-decomp sites:

1. Every fixnum-typed value stays fixnum throughout the function.
2. `total/2`'s list arg is a proper list of `{fixnum, fixnum}` 2-tuples
   (or empty).
3. Inlined calls only target T2-compiled local functions in the same
   module. Cross-module inlining is out of scope.

Any deviation side-exits to T1.

Compile-time refusal: T2-compile only succeeds if every BEAM op in the
function is one of: integer add/sub/mul, equal/not-equal compare
against a small int literal, cons-cell head/tail, 2-tuple destructure,
tail self-call, plain function call to a `t2_assume_smallints`-compiled
local in the same module. Anything else falls back to T1, no MVP
coverage. The compiler reports which listed functions actually got
T2-compiled.

### IR + passes
- Reuse `beam_ssa`. No new IR shape.
- Pass 1: walk SSA, mark integer ops, cons-decomp, and tuple-decomp
  as fast-path candidates.
- Pass 2: identify direct calls to T2-compiled local functions of
  size ≤ 8 SSA instructions; inline by SSA substitution.
- Pass 3: lower to asmjit emitter calls.

That's all three passes. No DCE pass, no constant folding pass — if
the inliner produces opportunities, the next iteration adds them.

### Codegen
Hand-written aarch64, in a new file separate from `arm/instr_*.cpp`.
Reuses asmjit and BeamAsm register conventions.

- Prologue: tag-check every typed argument. Side-exit on failure.
- List head/tail: load offsets 0 and 8 from the untagged list pointer.
- Tuple destructure: load header word, compare against the expected
  arity-tagged value, side-exit on mismatch; load elements at offsets
  8 and 16 from the untagged tuple pointer.
- Combined fixnum check: `orr` two values, mask, branch on non-zero
  (one check covers both).
- Arithmetic: `subs`/`adds`/`mul` on detagged values, `b.vs` to a
  shared side-exit stub on overflow.
- Tail self-call: re-tag if necessary, decrement reductions, branch
  to T2 entry.
- Epilogue: re-tag result, return.

### Side-exit and yield
Both side-exit and yield require a consistent frame state at the point
they fire. The MVP enforces a single rule in codegen:

> **Iteration-start values stay live in their canonical registers
> until every guard for the iteration has passed.** All in-progress
> arithmetic and structural extraction uses temporary registers;
> commit happens via register-rename at the end of the iteration.

Under this rule, every yield and side-exit point sees `R(list)` and
`R(net)` holding the values they had on entry to the current
iteration. There is no "partially committed" state to roll back.

**Side-exit stub** (per-site, but they share a single trampoline
since the live state is identical):

1. Re-tag iteration-start values from `R(list)`, `R(net)` into proper
   tagged terms.
2. Write them to the X-register save area at offsets matching T1.
3. Jump to T1's function entry. T1 re-executes the iteration from
   scratch, naturally taking whatever slow path triggered the exit.

**Yield stub**: same steps 1 and 2, but step 3 saves
`CP = total_t2_entry` (T2's loop top) and returns to the scheduler.
On resume, the process re-enters T2 code; no T1 round-trip.

Reduction accounting matches T1's exactly: T1 charges 2 reds/iter
(one for the `diff` call, one for the `total` tail call), so T2's
loop-top decrement is `subs REDS, REDS, #2`. Yield frequency at the
scheduler level is identical even though T2 yields at the loop top
instead of inside `diff/2`.

Side-exits land at function entry only for the MVP. In-function
partial-state preservation (sync points, deopt metadata, mid-iteration
deopt) is deferred — the iteration-start-stays-live rule is what
makes that simplification work.

## Sequencing

| Step | Effort | Output |
|------|--------|--------|
| 1. Benchmark + T1 baseline | 1 day | Module compiles, T1 disasm captured via `erts_debug:disassemble/1`, baseline ns/iter measured |
| 2. Side-exit round-trip | 3 days | A no-op T2 path that runs `total` with the same code as T1 but reached via the new dispatch entry, with a forced side-exit at iter 1. Verifies frame-layout compatibility and round-trip correctness. |
| 3. SSA → asmjit codegen (no inlining) | 1.5 weeks | T2 specializes integer ops, cons-cell decomp, tuple destructure, combined guards. Both `total` and `diff` compile under T2 but `diff` is still called, not inlined. Should already beat T1 by some margin from tag-check fusion alone. |
| 4. Inlining of `diff/2` | 3 days | T2 inlines `diff` into `total`. Benchmark runs end-to-end. |
| 5. Measurement + writeup | 2 days | Speedup number, disasm comparison, decision. |

Total: ~3.5 weeks of focused work.

Step ordering matters. Side-exit comes before any optimization
because the frame-layout coupling with T1 is the foundation everything
else depends on; if step 2 takes substantially longer than 3 days,
that itself is a useful signal that the "share T1's frame layout"
shortcut isn't viable and the MVP needs its own frame design (which
escalates scope significantly).

## What we measure

1. Wall-clock time of `run(1_000_000)`, median of 100 runs, T1 vs T2.
   The setup phase (`mk_txns`) is excluded from the measurement; only
   the `total/2` walk is timed.
2. Disassembly of both versions; instructions per loop iteration
   counted by hand.
3. Reduction count (must match between T1 and T2 — we are not changing
   reduction cost).
4. Side-exit correctness: a deliberate test that swaps a 3-tuple
   into the list mid-walk produces the same final result as pure T1.
5. Side-exit cost: time of `run(N)` where the first iteration's tuple
   has the wrong arity. This bounds the worst case for a
   profile-incorrect compilation.
6. Bignum correctness: a deliberate test where one element's amount
   overflows fixnum during the subtract; result must match T1.

## Decision criteria

**Success**: T2 runs the benchmark at least 2× faster than T1, and
side-exit produces correct results on the shape-mismatch and
bignum-overflow tests, and the side-exit-on-iter-1 case is no worse
than 1.5× pure T1.

**Failure**: T2 runs slower than 2× T1, or any correctness test
fails, or the side-exit-on-iter-1 case is more than 1.5× T1 (which
would mean profile-incorrect compilation is a net loss).

If success: write up the result, then pick the next benchmark from
`T2.md` (likely a polymorphic call site, then profile collection,
then map support). The MVP code itself is a discardable sketch —
what we keep is the answer to the question, the disassembly
comparison, and the measurement methodology.

If failure: stop. Re-examine `T2.md`'s assumptions before further
implementation. Likely failure modes worth naming up front:

- Side-exit overhead higher than expected (re-tagging and X-register
  reload at deopt costs more than the optimization wins).
- T1's inline fast-paths leave less room than expected (the gap on
  arithmetic alone is smaller than the call-elimination + tag-fusion
  gap, so the optimization is the call elimination + tag fusion,
  not the arithmetic).
- aarch64 microarch behaviour: branch prediction and OoO execution
  swallow the per-iteration check overhead in T1, making T2's
  optimizations invisible at the wall-clock level even when they're
  visible in the disassembly.

## Why this is the right MVP shape

- **Minimum infrastructure.** Hardcoded type, attribute trigger,
  side-exit at function entry. ~3.5 weeks of work; estimate may be
  wrong by 50% but not by 5×.
- **Targets T1's ceiling, not its weaknesses.** T1's per-op templates
  are well-tuned in isolation. The thing T1 *fundamentally* cannot
  do is fuse work across BEAM ops or eliminate calls. The benchmark
  is constructed so that fusion + call elimination dominate the
  per-iteration cost.
- **Realistic enough to be informative.** A list-of-records walk is
  one of the most common Erlang shapes; if T2 can't beat T1 here,
  it's hard to argue it'll beat T1 anywhere.
- **Falsifiable.** The decision criterion is a number, not a
  judgement. We do not need to argue about whether the result is
  "good enough" — 2× or stop.
- **A failed MVP is small enough to throw away.** A successful MVP
  gives us a working pipeline (SSA → asmjit + side-exit + inlining)
  that the next iteration extends rather than rewrites.
- **Aligns with `T2_critique.md` §M7.** A list-walk benchmark sits
  squarely in the "real Erlang workload shape" half of the corpus
  split — wins here generalise, unlike pure-arithmetic
  micro-benchmarks.
