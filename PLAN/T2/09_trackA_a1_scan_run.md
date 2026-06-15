# Track A · A1 — the binary scan-run capture

> **Implementation spec for the first build step.** Productionizes the
> G-bin experiment (`../verification/GBIN_OUTCOME.md`, measured 5.6×)
> into a real, always-on compiler+runtime capability — no T2 tier, no
> prologue patching, no deopt. Authoritative plan: [`08`](08_v1_loop_tier.md)
> §8 Track A (A1) and §2 (binaries are the one load-bearing,
> real-app-present win). This file holds the how.

## 1. Why this first

The three-app allocation profiling (`../verification/GALLOC_RESULTS.md`)
and the gate program converge on one conclusion: **binary parsing is
the only JIT-fusable optimization with both isolated wins (G-bin 5.6×)
and real-app presence** (RabbitMQ MQTT parse ~14%, JSON decode). Maps
went diffuse under Ash (46%→1.2%); control flow measured null. A1 is
the cheapest capture of that one win: it is upstreamable T1/AOT work,
needs none of the T2 infrastructure (IR builder, profiling, side-exit
machinery), and the §8 re-baseline gates the whole T2 build on whether
T2's *residual* still pays once A1 has harvested the head of the
distribution.

## 2. What A1 recognizes (the scan-loop idiom)

The stereotyped byte-class fast-forward loop, as written in
`lib/stdlib/src/json.erl` and every hand-rolled binary parser:

```erlang
%% json:number/7 family — consume a run of 0-9
number(<<Byte, Rest/bitstring>>, Original, Skip, Stack, Strs, Decode, Len)
        when ?is_0_to_9(Byte) ->
    number(Rest, Original, Skip, Stack, Strs, Decode, Len + 1);
number(<<Rest/bitstring>>, ...) ->          %% non-class byte / end
    ... handle the byte that stopped the scan ...
```

The SSA shape (per the code map) is a self-recursive function whose
back-edge block is exactly:

1. `bs_start_match`(new, Arg) → Ctx  *(hoisted/reused across iterations
   by `ssa_opt_bsm`)*,
2. `bs_match` with an `{integer}` command of size 8, unit 1 → Byte,
3. `{succeeded,guard}` on the match (fail → the stop clause),
4. a **byte-class guard** — one of: a range `Lo =< Byte, Byte =< Hi`
   (two `{bif,'=<'}`), an equality set `Byte =:= C1; …` , or a single
   `{bif,'=:='}` — fail → the stop clause,
5. an **advance** of the loop-carried accumulators that is *affine and
   effect-free*: the binary advances by `get_tail`, integer counters
   advance by a literal (`Len + 1`), all other args pass through
   unchanged,
6. a **self tail-call** `call #b_local{name=Name,arity=A}` on the
   back-edge.

**Recognition predicate** (all must hold): the back-edge block matches
1–6; the only per-iteration heap effect is the `get_tail` sub-binary
(which the scan eliminates — it never escapes); the loop-carried state
is {the match context, zero or more integer counters advancing by
literals, pass-through args}; the byte class is expressible in the
class lattice (§3). Anything else → leave the function untouched (the
boundary is a no-op, never a regression).

This is *idiom* recognition, not general loop optimization: it fires
only on the affine byte-class fast-forward and bails on everything
else. That narrowness is the point — A1 is pattern-bound by design
(`08` §8 A1); the long tail of non-stereotyped parsers is T2's case.

## 3. The byte-class lattice

The class is what the SWAR recipe library keys on. v-A1 covers the
classes that actually occur in the corpus scans:

| class | source guard | scalar test | SWAR recipe |
|---|---|---|---|
| `range(Lo,Hi)` | `Lo =< B, B =< Hi` | `(B-Lo) u<= (Hi-Lo)` | byte-subtract + unsigned-compare lanes |
| `eq(C)` | `B =:= C` | `B == C` | XOR-zero-byte detect |
| `notset({C1..Cn}, Lo, Hi)` | plain-ascii: `Lo =< B =< Hi` minus a few `=:=` exclusions | range AND ≠ each C | the `string_ascii` recipe (range lane + per-exclusion zero-byte lanes), already measured |

The proven `emit_t2_json_scan` covers `range('0','9')` (digits) and
`notset({$",$\\}, 0x20, 0x7F)` (plain ascii). A1 generalizes the
constants out of the emitter into a class descriptor carried by the
new command; the recipe set above is closed under what the byte-class
guards in the corpus produce. Unknown class → bail (§2).

## 4. The new `{scan,…}` bs_match command

Reuse the entire `bs_match` command-list machinery (`08` map: loader,
JIT command iteration, range registration) by adding **one command
type** rather than a whole new instruction:

```
{scan, Class, CounterUnit, CtxDst, CountDst}
  Class      :: {range,Lo,Hi} | {eq,C} | {notset,[C],Lo,Hi}
  CounterUnit:: the literal each integer accumulator adds per byte (1)
  CtxDst     :: match context after the run (start advanced)
  CountDst   :: number of bytes consumed (a smallint), for `Len + k`
```

Semantics: consume the maximal run of leading bytes of `Ctx` matching
`Class`, advance `Ctx`'s `start` past them, yield the run length in
`CountDst`. Reduction-faithful: one `subs FCALLS, k` per chunk (the §7
window rule in `08`; T1's source-unrolled loops already do this). The
command is **self-contained** — it commits nothing until the run ends,
so a guard-failed / end-of-input / yield exit leaves entry state
pristine, exactly as the experiment proves.

The compiler pass rewrites the recognized loop into: the back-edge
`bs_match` gains a `{scan,…}` command in place of the per-iteration
`{integer}`+class-guard+self-call; the stop clause receives `CountDst`
folded into its `Len` accumulator and re-matches the stopping byte
(its existing code, unchanged). The recursive function becomes
straight-line: scan-run, then the original stop-clause body.

## 5. The JIT emitter

Lift `emit_t2_json_scan` (arm `beam_asm_module.cpp`) into the
`bs_match` command loop (arm `instr_bs.cpp`, then x86) as the handler
for `{scan,…}`:

- drop the entry guards that the experiment needed (it was a blind
  prologue patch; here the command runs *inside* `bs_match`, which has
  already established a valid byte-aligned match context — the
  match-context dance is the loader/codegen's job, not ours),
- parameterize the stop test on `Class` (§3 recipes),
- the run length goes to `CountDst` instead of being folded into a
  hardcoded `X6`,
- exits go to the `bs_match` fail label / yield trampoline, not a
  T2 side-exit address.

What it eliminates per byte vs T1 is exactly the experiment's list:
the function re-entry (prologue + `i_test_yield`), the `bs_match`
position load/store and base load+mask, and the per-byte sub-binary
bookkeeping.

## 6. Staging (small functional commits, each compiles + tests clean)

- **A1-0 · recognizer, annotate-only.** New `ssa_opt_scan_loop` pass
  in `beam_ssa_opt.erl`, registered in `late_epilogue_passes/1` after
  `ssa_opt_bsm`/before `ssa_opt_bs_ensure`. Detects the §2 shape via
  `beam_ssa:dominators/2` + back-edge + the affine/class checks;
  attaches a debug annotation; changes **no** codegen. Gate: a
  compiler test asserts it fires on `json:number/7`,
  `number_frac_cont/7`, `string_ascii/7` and *not* on near-miss
  shapes (effectful body, non-affine advance, unknown class).
- **A1-1 · scalar scan command, end to end.** Add `{scan,…}` to
  `genop.tab` command set + `beam_ssa_codegen:bs_translate_instr/1`
  emission + loader (`ops.tab` both arches) + a **scalar** (one byte
  per iter) JIT handler. Gate: full stdlib/json correctness suite +
  byte-identical `json:decode` hashes on the nativejson trio; the
  bytewise layer ≥2.5× isolated on the G-bin bench (`08` P2 bar).
- **A1-2 · SWAR recipes.** The §3 lane recipes (8 bytes/iter) behind
  the scalar tail. Gate: G-bin full ≥4× isolated scan
  (`08` §7 acceptance bar); hashes unchanged.
- **A1-3 · encode-side + re-baseline.** `escape_binary_ascii`
  (same `notset` shape, construction side) where cheap; then run the
  §8 re-baseline suite against A1-improved T1 and record the T2
  residual go/no-go.

## 7. Correctness contract (unchanged-behaviour bar)

A1 changes speed only. The recognized loop is rewritten to a
*semantically identical* straight-line scan: same bytes consumed, same
stop byte, same accumulator values, same heap result (the eliminated
`get_tail` sub-binaries never escaped). Verification mirrors the
experiment's discipline (`08` §2.1): byte-identical result hashes
across A1-on/off on the full json corpus, the stdlib binary/match test
suites green, and reductions observably equivalent (per-chunk
decrement). The recognizer's bail-out is total — an unmatched shape
compiles exactly as today — so the worst case is "no speedup," never a
regression or a behaviour change. Tracing/stacktraces/inspection are
untouched: this is ordinary compiler output, not a tier.

## 8. Files

| concern | file | point |
|---|---|---|
| recognizer pass | `lib/compiler/src/beam_ssa_opt.erl` | new `ssa_opt_scan_loop/1`; register in `late_epilogue_passes/1` |
| loop/back-edge | `lib/compiler/src/beam_ssa.erl` | `dominators/2`, `predecessors/1`, `successors/1` (reuse) |
| command emission | `lib/compiler/src/beam_ssa_codegen.erl` | `bs_translate_instr/1` (~L2686): add `{scan,…}` |
| op/command decl | `lib/compiler/src/genop.tab` | `bs_match` command set doc + any loader-visible tag |
| loader lowering | `erts/emulator/beam/jit/arm/ops.tab`, `…/x86/ops.tab` | `i_bs_match` command pass-through |
| JIT emitter | `erts/emulator/beam/jit/arm/instr_bs.cpp` (then x86) | `{scan,…}` handler; lift `emit_t2_json_scan` |
| proven source | `erts/emulator/beam/jit/arm/beam_asm_module.cpp` | `emit_t2_json_scan` (G-bin experiment, the recipe) |
| perf gate | `PLAN/verification/` G-bin bench | ≥2.5× bytewise (A1-1), ≥4× SWAR (A1-2) |
