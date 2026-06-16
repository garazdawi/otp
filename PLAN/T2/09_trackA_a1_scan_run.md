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
   (comparison `{bif,…}`), an equality set `Byte =:= C1; …`, or — the
   common case for an enumerated printable-ASCII set — a **`switch`** on
   the byte that the compiler emits as a jump table. The switch is read
   path-sensitively: an arm is in-class only if it reaches the self-call
   block, so a function's *other* switch on the same byte (e.g.
   `number_frac_cont`'s `e`/`E` exponent handling) is correctly ignored
   (`beam_ssa_opt:scan_switch_invals/4`),
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
classes that actually occur in the corpus scans, derived from either
comparison guards or a switch's in-class arms (§2.4):

| class | source | scalar test | SWAR recipe |
|---|---|---|---|
| `{range,Lo,Hi}` | `Lo =< B =< Hi`, or a contiguous switch-arm set | `(B-Lo) u<= (Hi-Lo)` | byte-subtract + unsigned-compare lanes |
| `{set,Vals}` | `B =:= C; …`, or a sparse switch-arm set | one lane per value | XOR-zero-byte detect per value |
| `{notset,Excl,Lo,Hi}` | a switch-arm set that is a range minus ≤4 holes (plain-ascii) | range AND ≠ each excluded | the `string_ascii` recipe (range lane + per-exclusion zero-byte lanes), already measured |

The proven `emit_t2_json_scan` covers `{range,$0,$9}` (digits) and
`{notset,[$",$\\],0x20,0x7F}` (plain ascii). The recognizer
(`scan_canonical_class/1`) canonicalizes a recovered value set into the
tightest of these three. Unknown / unbounded class → bail (§2).

### What the corpus actually contains (measured)

A census of the recognizer over **all 201 stdlib+kernel modules** (0
crashes) found 34 scan loops, dominated by exactly these shapes:

- **`json` is the richest single source — 19 functions**: digit scans
  (`number*`, `{range,48,57}`), the printable-ASCII string scan
  (`string/7`, `{notset,[$",$\\],32,127}`) on both decode and the
  encode-side escape scans (`escape_binary`, `escape_all`), and
  **whitespace skips** (`object_key`, `array_start`, `value`, … —
  `{set,"\t\n\r "}`), the most pervasive parser idiom.
- Hand-written parsers elsewhere: `ansi_sgr` (SGR params), `uri_string`
  path normalization (`remove_dot_segments`/`join1b`, `/`), `erl_tar`
  octal fields, `re` replacement scanning, `string:bin_search_inv`.

**`string`/`unicode` themselves expose the byte idiom only in
`bin_search_inv`** (the ASCII fast path). Their real scanners
(`trim`/`take`/`lexemes`/`slice`) route through `unicode_util:gc/cp` (a
per-character function call) or variable-width UTF-8 — *not* the
fixed-byte class idiom. So A1's home is hand-written binary parsers
(json, and the RabbitMQ MQTT-parse ~14% the profiling flagged), not the
polymorphic string library. A UTF-8 skip-run and eliminating the
per-char `unicode_util` call are the adjacent, harder captures the
string library would need — out of A1's byte-class scope, noted for
later.

## 3b. The fast-forward-hint insight (A1-1b architecture)

The scan op is an **always-safe prefix consume**. The recognized loop
consumes a run of in-class bytes one at a time; a scan that consumes
*any prefix* of that run (including zero) and advances the match
context + counter accordingly leaves the *original loop* to handle
whatever remains and the exit byte — because the loop body re-reads the
context position and re-tests the class every iteration. So:

- **No CFG surgery.** The rewrite inserts the scan at loop entry (after
  `bs_start_match` establishes the context) and the existing
  self-recursion is left intact: after a maximal scan the very next
  byte is out-of-class, so the body runs once and exits to the stop
  clause (the self-call becomes dynamically unreached); after a partial
  scan the self-call re-enters and the scan fires again. Either way
  correct, by construction.
- **The match context advances in place.** A match context is a boxed
  mutable `ErlSubBits` whose `start` field is the bit position;
  `emit_t2_json_scan` already advances it with a single
  `str … offsetof(ErlSubBits,start)`. So the scan only needs to mutate
  `start` — the subsequent `bs_match` reads the new position
  automatically, and **no SSA context-variable substitution is needed**.
  Only the counter (`Len`) must be threaded: `Len' = Len + Count`, then
  substitute `Len → Len'` in the entry-dominated region.

### Vehicle: the `{scan}` bs_match command (a BIF is blocked)

A BIF was the first instinct (least codegen work) but **it is blocked**,
confirmed by reading the compiler (2026-06-15):

- A match context **cannot flow into a remote call**.
  `beam_ssa_bsm.erl:check_context_call/4` returns `{remote_call, Call}`
  for any `#b_remote{}` callee — the pass exists precisely to stop match
  contexts "leaking" into instructions that can't handle them
  (`beam_ssa_bsm.erl:49`). A BIF is a remote call.
- The save/restore alternative (`bs_get_position`/`bs_set_position`
  around a BIF taking the underlying binary) needs arithmetic on the
  *opaque* position term, which isn't available, and adds two ops +
  a position allocation per run.

So the vehicle is the **`{scan,…}` `bs_match` command** (§4) — and the
match-context restriction is *why*: the scan runs **inside** the
`bs_match` machinery, where the context is already in hand and is
advanced in place (the `ErlSubBits.start` write `emit_t2_json_scan`
already does), so nothing leaks. The byte `Count` leaves the command as
an ordinary bs_match output (`CountDst`), exactly like an `{integer,…,
Dst}` command — that is what lets the SSA rewrite thread it:
`Len' = Len + CountDst`, then substitute `Len → Len'` (the
fast-forward-hint above means that, plus the in-place context advance,
is the *entire* rewrite — no CFG change). Confirmed-feasible pieces
(investigation): `ErlSubBits` is a boxed, GC-safe, in-place-mutable
match context (`erl_bits.h:60`); `start` advance is the standard
pattern (`erl_bits.c:204,314`); a bs_match command already produces
register outputs. SWAR inside the JIT handler is A1-2.

## 4. The `{scan,…}` bs_match command

Reuse the entire `bs_match` command-list machinery (`08` map: loader,
JIT command iteration, range registration) by adding **one command
type** rather than a whole new instruction:

```
{scan, Class, CounterUnit, CtxDst, CountDst}
  Class      :: {range,Lo,Hi} | {set,[C]} | {notset,[C],Lo,Hi}
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

- **A1-0 · recognizer, annotate-only. DONE** (`ssa_opt_scan_loop` in
  `beam_ssa_opt.erl`, registered in `late_epilogue_passes/1` after
  `ssa_opt_bs_ensure`). Self-tail-recursion is a `call`, not a CFG
  back-edge, so the matcher is intra-function idiom matching anchored
  on the self-call (no dominator/loop analysis); it attaches a
  `scan_loops` function annotation and changes **no** codegen.
- **A1-0b · switch-class recognition. DONE.** Derives the class from a
  path-sensitive `switch` (the printable-ASCII enumerated set), not
  just comparison guards. Verified: fires on json's 19 scan loops with
  correct classes; recognizer runs over **all 201 stdlib+kernel
  modules, 0 crashes, 34 scan loops**, no junk false positives. Two
  soundness gaps to close in A1-1 before any rewrite (both benign while
  annotate-only): the **comparison** path is not yet path-sensitive
  (it folds every `>=`/`=<` on the byte regardless of which branch
  reaches the self-call — inverted ranges already rejected, but it must
  gain the switch path's reachability test); and recognition does not
  yet verify **bytes-matched-per-iteration == counter-increment**
  (`string_ascii/7` matches 8 bytes with `Len+8`; the fused scan must
  preserve that 1-byte-per-count invariant) nor that in-class arm
  blocks are side-effect-free between switch and self-call.
- **A1-1a · path-sensitive, rewrite-sound recognition. DONE**
  (`c1d953d973`). Classifier now orients every guard (switch arm or
  comparison branch) by whether its side reaches the self-call, so both
  polarities are modelled — *scan-while-in-class* (`{range}`/`{set}`/
  `{notset}` over the in-set) and *scan-until-stop* (`{notset,Stop,
  0,255}`, the corrected `bin_search_inv`/`strlen`/`collect_line_bin`
  shape). Plus the **single-byte gate**: every counter advances by 1,
  excluding the hand-unrolled multi-byte matchers (`string_ascii/7`,
  `Len+8`). Census: 201 stdlib+kernel modules, 0 crashes, 31 scan
  loops, all classes semantically correct. Still annotate-only.
  Remaining soundness item for A1-1b: verify the in-class arm blocks
  carry no side effect between the class test and the self-call (today
  only the call-arg affine/passthrough check guards this).
- **A1-1b · the `bs_scan` instruction, end to end.** A new BEAM
  instruction `bs_scan Ctx Class Dst Live`, modeled on `bs_ensure`
  (context→context) for ordering and `bs_get_position` for the JIT
  context access. Full implementation map (traced 2026-06-15; every
  touchpoint below confirmed against the `bs_get_position` analog):

  *Compiler:* `beam_ssa.erl` (add `bs_scan` to `cg_prim_op()`);
  `beam_ssa_codegen.erl` (`classify_heap_need` neutral, `produces_output`
  true, `cg_instr(bs_scan,…)` → `{bs_scan,Ctx,Class,Dst,Live}`);
  `genop.tab` (next opcode, `bs_scan/4`); `beam_opcodes.erl`
  (opcode/opname); `beam_validator.erl` (`vi({bs_scan,…})` — assert
  `#t_bs_context{}`, produce `#t_integer{}`, keep `mark_current_ms_position`
  so it orders with other context ops); `beam_disasm.erl`.
  *Runtime:* `emu/ops.tab` + `emu/bs_instrs.tab` (interpreter:
  scan `Ctx` start..end against the class, write count to `Dst`, advance
  `sb->start`); `jit/arm/ops.tab` + `jit/x86/ops.tab`; `jit/{arm,x86}/
  instr_bs.cpp` (`emit_i_bs_scan` — class-parameterized lift of
  `emit_t2_json_scan`: load `ErlSubBits.start`/`end`/`base`, scalar byte
  loop, write back `start`, tag count → `Dst`).
  *Rewrite* (`beam_ssa_opt.erl`, behind off-by-default `scan_loop`
  option): for each recognized scan, at loop entry after the context is
  established insert `Count = bs_scan(Ctx, ClassLit)`, `Len' = Len +
  Count`, and substitute `Len → Len'` in the entry-dominated region.

  **The critical correctness risk — ordering.** `bs_scan` advances the
  context *in place*; in-place mutation is invisible to SSA, so the
  optimizer must not reorder it relative to the byte `bs_match` it
  precedes. Resolution: `bs_scan` is a context op — it takes the context
  and the validator's `mark_current_ms_position` ties it into the
  match-state dataflow (same machinery that orders `bs_get_position`/
  `bs_set_position` against matches), so it stays pinned. This must be
  *verified* (a `+JDdump`/disasm check that the scan precedes the match
  on the corpus) before trusting the gate, and is the single highest-risk
  item: a reorder = silent miscompilation of binary matching.

  Stage tree-green: (1) define `bs_scan` everywhere + interpreter + JIT
  (dead, never emitted → build + full suite stay green); (2) the gated
  rewrite (default off → still green); (3) enable on the json subject,
  gate.

  **A1-1b COMPLETE (2026-06-15), gate met.** The scalar `bs_scan`
  instruction + recognizer + two-output rewrite are implemented,
  correct, and pass the bytewise speed gate. **Correctness:**
  `json:decode` byte-identical across 5000 randomized encode/decode
  round-trips (numbers/floats/escaped+unicode strings/arrays/maps/
  whitespace) with `scan_loop` on vs off; 198/201 stdlib+kernel modules
  compile clean. **Speed (isolated bytewise, `scanbench.erl`):**
  printable-ASCII `notset` scan **2.93×** (0.89→2.61 GB/s), digit
  `range` scan **4.08×** (1.02→4.17 GB/s) — both clear the **≥2.5×**
  P2 bar; SWAR (A1-2) lifts further toward G-bin's 5.6×. End-to-end
  json decode is ~1.03× (scanning is a fraction of decode; matches
  G-bin's ~6-10% with SWAR). **Known limitation:** 3 branchy/recursive
  functions (`uri_string:remove_dot_segments/2`, `epp:com/4`,
  `shell_docs_markdown:strip_list_line/1`) hit a `bs_pos_bsm3`
  save-placement edge case (a `bs_get_position` lands in a
  `[op,succeeded]` test block → `cg_test`); a conservative recognizer
  bail is the remaining crash-safety item before global enable. The
  two-output fix that made multi-clause json correct: `bs_scan` produces
  the advanced context (threaded downstream + `bs_extract` for the
  count), so pre_codegen's position tracker follows the advance.

  **Stage 1 DONE + committed (`c665569bbe`):**
  `bs_scan Ctx Kind Range VPack Dst` defined across genop/ssa/codegen/
  validator/disasm + interpreter + arm JIT (`emit_bs_scan`, class-
  specialized scalar loop) + x86 no-op stub; emulator + compiler build
  clean, dead code. Stage 2 DONE + committed (`6d8202aebc`, gated off):
  `ssa_opt_scan_loop_rewrite` inserts `bs_scan` + threads the counter.
  **Works for simple single-binary scans** (`count_ws`=3, `count_dig`=5,
  byte-identical). **KNOWN BUG on json multi-clause scans** — the
  root-caused soundness issue: pre_codegen brackets the match region in
  `bs_get_position`/`bs_set_position` for clause backtracking, and the
  inserted `bs_scan` lands *inside* that bracket, so the restore
  (`bs_set_position {x,0},{x,7}`) undoes the in-place advance while the
  counter stays bumped → context vs Skip/Len desync (atoms parse,
  numbers/strings mis-slice). **Fix:** `bs_scan` must produce the
  *advanced context* as a threaded value (the bs_match/`bs_extract`
  two-output pattern: `_X = bs_scan …` is the new context, `_Count =
  bs_extract _X`), registered in `beam_ssa_pre_codegen:fix_bs` as a
  context producer so the save captures the post-scan position and the
  downstream match threads `_X`. That is the remaining A1-1b work.

  **Fix design, refined (after reading pre_codegen's position tracker).**
  `bs_restores_is` keys entirely on the context-value chain: each match
  op reads from its context *argument* and advances the tracked position
  to its *dst*; backtracking restores to whatever dst was current at the
  dispatch. `bs_scan` is invisible (yields a count, not a context), so
  the tracker thinks the position never moved and the restore reverts
  the advance. The correct fix is a genuine **two-output op** touching
  five places: (1) the rewrite makes `bs_scan` produce the advanced
  context `_X` and renames downstream context users `Ctx→_X`, plus
  `_Count = bs_extract _X`, `Len' = Len + _Count`; (2) `fix_bs` CtxChain
  `bs_scan` dst → parent; (3) a `bs_restores_is` `bs_scan` clause that
  advances `SPos[root] := dst`; (4) the count register — `bs_subst_ctx`
  collapses context vars to the root register, so the count needs its
  own register via the `bs_combine`→`bs_get` machinery (the intricate
  part, today specific to adjacent-block `bs_match`); (5) `cg_instr`
  emits BEAM `bs_scan` with that count register. A real change to core
  match-context dataflow + codegen — miscompilation-sensitive, do it
  unhurried with `bs_match_SUITE` at each step. The Stage-2 commit
  (simple scans working, gated off) is the foundation.

  **Benchmark against the *naive* `string/7` path**, not the
  hand-unrolled `string_ascii/7`. Gate: full stdlib/json suite +
  byte-identical `json:decode` hashes on the nativejson trio; bytewise
  ≥2.5× on the G-bin bench. Needs an **emulator rebuild** + the ordering
  verification; large and miscompilation-sensitive — implement staged,
  building + testing at each step, not in one shot.
- **A1-2 · SWAR recipes. COMPLETE (2026-06-15), gate met.** The §3
  lane recipes (8 bytes/iter) behind the scalar tail, in arm
  `emit_bs_scan`. SWAR fires for `range [lo,hi]` with `hi<=0x7F` and
  for `notset hi==0x7F` with ≤2 exclusions; everything else falls to
  the scalar bytewise loop, which doubles as the SWAR tail (the last
  <8 bytes, and the bounded re-scan after a chunk flags a stop).
  Recipe per 8-byte chunk: lower mask `((w-lo_rep)&~w|w)&HIGH` (byte
  <lo or ≥0x80); a **carry-safe** upper term `((w&LOW7)+c_hi)&HIGH`
  (byte>hi, masked to low 7 bits so no cross-byte carry) when hi<0x7F;
  plus `haszero(w^Vi)` per exclusion. SWAR over-reports stops (false
  positives re-checked bytewise) but never misses one — so it is exact.
  **Correctness:** `json:decode` byte-identical across 5000 round-trips
  (unchanged); a 16000-case fuzz across all four variants (notset +
  digit/alpha/ctrl ranges) with bytes ≥0x80 and stops at every chunk
  offset, scalar-vs-SWAR identical, 0 mismatches. **Speed (isolated,
  1 MB single-run, `sb.erl`):** printable `notset` **3.06×**
  (0.90→2.75 GB/s), digit `range` **20.8×** (1.03→21.4 GB/s) — clears
  the **≥4×** bar (digit) and ~G-bin (notset). The notset path is
  **throughput-bound** at ~14 ALU ops/8 bytes, not latency-bound: an
  ILP tree-reduce of the 2 exclusion sub-masks was tried and measured
  **neutral** (register renaming already overlaps the chains), so it
  was reverted — fewer ops, not more ILP, is the only lever left, and
  no cheap SWAR fusion exists for two arbitrary exclusion bytes. **End-
  to-end json decode flat** (twitter 1.01×, canada 0.98×, citm 1.02×):
  the scan primitive is now 3–20× faster, but on these docs string runs
  are short and decode is dominated by map/number/structure work.

  **Showcase (scan-dominated workloads, `lex_scan_showcase.erl`).**
  Where scanning *is* the work, the win lands and **scales with run
  length** toward the isolated ceiling:
  | workload | run len | speedup | GB/s |
  |---|---|---|---|
  | log wc (printable lines, range 0x20–0x7E) | ~120 B | **3.7×** | 0.85→3.1 |
  | numeric tokens (12–20-digit IDs) | ~17 B | 1.2× | 0.31→0.37 |
  | big-int parse (200–800-digit, range 0x30–0x39) | ~500 B | **6.7×** | 0.91→6.2 |
  All results identical stock-vs-scan. Short runs stay framing-bound
  (per-token sub-binary + recursion dwarfs the scan); long runs (log
  lines, serialized big integers — crypto/financial/scientific) is
  where A1+SWAR pays off end-to-end. **The A1 value statement: any
  byte-class scan over long runs — line/field lexing, big-number
  ingestion, printable validation — gets 3–7× end-to-end today.**
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
