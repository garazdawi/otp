# T2 v1 — The Loop Tier

> **The authoritative T2 plan.** This document stands alone for
> evidence, scope, and decisions; low-level mechanics it shares with
> the reference designs are summarised here and detailed in
> [`00`](00_overview.md)–[`07`](07_delivery.md) (each pointer says
> what it holds). [`../verification/`](../verification/) holds the
> experiment writeups backing every claim marked **[measured]**.
> Where this file conflicts with 00–07, this file wins.
>
> *Changelog note:* this revision introduces three decisions that
> were not in earlier drafts: the **Track A** T1/AOT capture work,
> the **re-baseline go/no-go checkpoint** before the v1 build, and
> the **reversal** of the earlier decision to drop the T1 map-key
> inline cache in T2's favour (`02` §7.6's closing paragraph is
> superseded by §8 A2).

## 1. Executive summary

T2 is a second, optimizing JIT tier above BeamAsm (T1) on aarch64.
This plan scopes its first version to the **loop tier**: compile
hot loop-shaped and leaf functions into fused native loops, side-exit
to the intact T1 code for everything else, and change *nothing*
observable except speed.

The plan is built on a completed experiment program — five
hand-written, correctness-verified specializations plus a corpus
census, measured against T1 on real workloads. Their combined
verdict:

> **T2's wins come from fusing data access inside loops — binaries
> first, maps second, tuples/lists as the base — not from
> optimizing control flow or calls, which BeamAsm already executes
> at the cost floor of modern out-of-order cores.**

Three findings shape everything below: binary scan loops gain
**5.6×** [measured]; map shape specialization gains **1.64×**
[measured]; and the two control-flow bets — call-crossing
optimization and branchy-dispatch specialization — measured **zero**,
shelving roughly 25 weeks of general-inlining infrastructure that
earlier drafts treated as core.

Status: architecture validated, scope evidence-backed,
implementation not started. The recommended sequencing (§8) runs a
short **T1/AOT capture track** first — all three wins have at least
partial cheaper homes: binaries and maps substantially (a scan
superinstruction; a T1 inline cache), the MVP's call-elimination
half via AOT inlining defaults — then re-baselines T2's marginal
value before committing to the ~24-week v1 build.

## 2. Evidence

### 2.1 The experiment program

Each experiment is a hand-written asmjit specialization installed
via a codegen hook (the MVP scaffolding), env-gated so a single
build provides T1 baseline and treatment, with byte-identical
result hashes required across modes. Writeups under
`PLAN/verification/`.

| experiment | mechanism tested | result | writeup |
|---|---|---|---|
| MVP (`t2_mvp:total/2`) | loop recovery + elimination-rich leaf inlining + guard fusion | **1.97× min / 1.85× median**, within 2 % of instruction-count ceiling | `../mvp/OUTCOME.md` |
| G-bin (`json` scan loops) | binary scan fusion: match-context registerization + loop ownership + SWAR | **5.56×** isolated (2.0 → 11.8 GB/s); **6–10 %** end-to-end on real JSON | `GBIN_OUTCOME.md` |
| G-map (struct-shaped flatmaps) | shape guard + direct offset loads vs per-key scan | **1.64×**; mis-guessed shape ran 1.45× *slower* (every element side-exiting) | `G31_GMAP_OUTCOME.md` |
| G3-2 (`erl_types` mutual recursion) | call-crossing / overhead-only inlining | **null** (0 ± 1 % CPU on the PLT build, despite 2.25× micro and proven engagement) | `G3_OUTCOME.md` |
| G3-1 (gen_server-shaped dispatch) | cold-arm pruning + guard-fused hot arm | **null** (1.02× isolated, 1.01× through `gen_server:call`; engagement proven via `+JDdump`) | `G31_GMAP_OUTCOME.md` |
| Effect-shape census | static + call-weighted dynamic corpus structure | effects-in-loops negligible; calls-in-loops dominate statically; call counts ≠ cycles | `RESULTS.md` |

### 2.2 Why the wins land where they do

- **Binary matching pays a real per-op tax in T1**: every
  `bs_match` reloads position and end, masks the base, and stores
  the position back to the heap — around a full function re-entry
  per byte. ~10–14 cycles/byte, cut to ~3 by registerization and
  loop ownership alone and to ~0.3 with SWAR on top. No core hides
  that ratio. The fact that
  `json.erl` hand-unrolls 8 bytes in Erlang (`string_ascii`) shows
  libraries already pay complexity to dodge this cost.
- **Map access pays a per-key scan** that one shape check
  eliminates — but the shape is only knowable at runtime (flatmap
  key order is atom-index-dependent; it differed between two VMs of
  the same build during G-map).
- **Calls and clause dispatch are already at the floor.** A 12-clause
  dispatch is ~4 ns in T1; a tiny-body call is ~20 cycles, predicted
  and store-forwarded. There is no overhead pool; only *elimination*
  across a call boundary pays (the MVP's fused guards), never the
  call mechanics themselves.

### 2.3 Methodology rules (learned the hard way)

1. **Size win pools with cycle profiles, never call counts.** The
   census's 47 %-of-calls bucket held a few percent of cycles —
   call counting over-weights tiny bodies ~10×.
2. **Inline only where something can be eliminated** (callee checks
   subsumed by caller facts, constant arguments) — never for call
   overhead alone. Literal funs are the strongest constant argument
   (the intrinsics case).
3. **Shape guards must test runtime-recorded identity** (the
   keys-tuple pointer), never layouts assumed at codegen time.
4. Hand experiments must **prove engagement** (asm dump, counter
   deltas) before a null result counts.
5. All measurements so far are Apple Silicon big cores. Re-run the
   kit on server ARM (Graviton) before treating the two nulls as
   universal; the positives can only strengthen on smaller cores.

## 3. What v1 compiles

A function is compiled when its call counter trips **and** static
structure puts it in one of three classes:

1. **Self-tail-recursive functions** → loop recovery: the back-edge
   becomes an internal branch, entry guards hoist to a preheader
   (run once per invocation), loop-carried values stay in registers.
   The canonical Erlang loop idiom and the MVP's validated shape.
2. **Functions calling a `lists:*` higher-order helper with a
   literal fun** → intrinsic inlining (hand-ported expansions of
   the `sys_core_fold_lists` set) + loop recovery of the helper,
   fun body inlined by constant propagation. (Library-author opt-in
   via a `-jit_inline` module attribute is the later extension —
   `04` §10.4.)
3. **Leaf functions with guard-heavy destructuring** → entry
   speculation + fused guards + return; same machinery minus the
   back-edge.

**The v1 op set**: integer arithmetic (overflow handled by flag
checks, §4.4), comparisons, cons/tuple destructuring and
construction, guard BIFs as primitive ops (the `04` §10.7 manifest,
minus `length/1`), local calls to *inlineable leaves* (small,
call-free), **and the byte-aligned binary scan subset** —
match-context guards plus `bs_match` ensure/get-integer-8/skip in
recovered loops, the G-bin shape. The scan subset is in v1 because
it is the largest measured pool — at corpus scale too: 46 % of
RabbitMQ's loop functions touch `bs_*` ops — and needs no machinery
the loop tier doesn't already have (the match context is ordinary
loop-carried state).

Two rules keep the boundary cheap rather than cliff-shaped:

- **Unsupported ops terminate the optimized region** with a static
  side-exit — an unconditional branch to that op's T1 address. The
  function still compiles; the unsupported tail runs in T1.
- **Non-inlineable calls demote-on-return** (§4.3): emitted with
  the *T1 continuation* as their return address, so the callee
  returns into T1 and the rest of that invocation runs there. For
  self-tail-recursive spines this costs almost nothing: the next
  iteration re-enters T2 through the patched prologue — inferred
  from G3-2's structure (its slow path measured the *opposite*
  configuration; the re-entry dynamic itself is mechanism
  reasoning, to be confirmed in P2).

Functions with no optimizable region get no counters and no
profiling: zero overhead for code T2 can't improve.

**Effects.** Re-execution windows (§4.2) must be effect-free;
effects are window *boundaries*, not disqualifiers. **Allocation is
not an effect** — an abandoned partial iteration leaves garbage,
not state — so cons/tuple builds live freely inside windows. BIF
effects (`!`, `ets:*`, `put/2`, …) are CP-less leaf calls T2 emits
inline — the op is a sync point, X/Y synced — and a new window
starts after (deopt target: the post-effect boundary). A
send-in-the-loop compiles fully, as two windows per iteration.
Erlang-call effects (`gen_server:cast` is a call before it is an
effect) demote-on-return. Census data: effectful BIFs in loops are
a rounding error (derived from `RESULTS.md` §3: B+I+C buckets
combined ≈ 0.25 % of all dialyzer calls); calls are the real
boundary case, handled above.

## 4. Architecture

**The state-preservation invariant everything below relies on**
(full treatment: `01` §6): T2 code matches T1's abstract machine
state — X/Y register layout, HTOP, FCALLS, stack — at every **sync
point**: function entry, calls, returns, GC sites, BIF boundaries,
speculation guards, tracing-relevant points, receive safe points.
*Between* sync points, registers are free. This is what makes
"branch to a T1 address" a complete deopt, lets GC and tracing
observe T2 frames as ordinary T1 state, and bounds every mechanism
in this section.

The five load-bearing decisions, each self-contained here. (Deeper
treatments: install mechanics `06`, speculation `03` §9.)

### 4.1 Build the IR from loaded BEAM code — no SSA chunk

T2 builds its SSA IR by reconstruction from the module's loaded
BEAM code: the loader retains the raw code chunk plus
atom/literal/import tables for modules with eligible functions
(today `beamfile_free()` discards them after T1 emission — a small
loader change; retention is freed on purge and reported under an
`erlang:memory/0` key), and the compile job re-decodes and runs
standard Braun-style SSA construction. Types seed from the
existing `Type` chunk, which the compiler already emits by default
and BeamAsm already consumes.

Why: **T2 works on every `.beam` that exists** — applications,
dependencies, Elixir — with no recompilation, no new chunk, no
compile option, no file-size cost, no chunk/code version skew. This
is the single largest real-world-reach lever in the plan. What's
lost (beam_ssa-level annotations) is recovered by the forward
dataflow pass T2 runs anyway. Fallback if the fidelity gate (§8,
G1) fails: the SSA-chunk design archived in `02` §7.8.

### 4.2 One deopt shape: re-call

Every side-exit reconstructs **a valid argument vector at a call
boundary** and branches to a T1 address. Three cases, one shape:

- *Loop iteration*: re-execute from the function entry with
  iteration-start values (guards fire before any X-register
  commit, so entry state is always intact — the MVP model).
- *Inlined tail-recursive helper*: mid-loop state **is** a valid
  fresh call (`foldl` suspended at element k ≡
  `foldl(F, Acc_k, Rest_k)`); deopt materialises current loop state
  into the call's argument registers and branches to the T1 call
  instruction.
- *Inlined leaf*: covered by the enclosing iteration's re-execution.

The legality rule is **guards-before-effects**: within a window,
every deopt-able guard precedes the first effect, so re-execution
never repeats an effect. This is the rule HotSpot
(`Unpack_reexecute`) and JSC (OSR exit to the current bytecode)
enforce per bytecode; no software JIT re-executes effects
(`../research/deopt_reexecution.md`). The same invariant makes the
SWAR scalar-tail re-scan legal (§7) — deopt discipline and
vectorization legality coincide.

Deleted relative to the full design: framestates, `parent_fs`
chains, eager-CP-push, per-region deopt stubs, multi-level inlining
metadata (`03` §9.2 holds the designs, on the shelf).

What re-execution costs in the bad case: **[measured]** directly
once — G-map's wrong-shape variant ran 1.45× T1, bounded and
correct — and bounded by the MVP's all-side-exit run plus its
instruction-count analysis (exits land in native T1 under an
identical calling convention; the interpreter-format tax that made
pre-BeamAsm re-execution expensive is structurally absent). Exactly
what the exit-counter policy (§4.6) exists to jettison.

### 4.3 No return addresses into T2 blobs

Three rules: non-inlined calls push the **T1 continuation** as
their CP (CPs are unvalidated code addresses; two instructions
instead of `bl`); inlined regions push no CP; the only T2 addresses
the runtime ever holds are yield-resume PCs in `c_p->i` (§4.5),
never on the Erlang stack.

Consequences: stack-walking introspection (`current_stacktrace`,
`backtrace`, crash dumps) is **byte-identical to T1 by
construction**; uninstall/eviction collapses to: revert the
prologue patch, thread-progress sync, translate `c_p->i` for
processes yielded inside the blob (one field, one generation
compare at schedule-in). The tombstone CP tables, lazy whole-stack
scans, and continuation trampolines of `06` §5.3–5.5 are deleted
from v1 and return only with general inlining (shelved, §9).

Cost: the post-call rest of an invocation runs in T1 — near-zero
for tail-recursive spines (the next iteration re-enters T2 via the
prologue), and the compile filter avoids functions whose optimized
region is punctured by calls.

### 4.4 Profiling: a counter and entry types; overflow by flags

Kept: per-function call counter + function-entry per-argument type
slots, emitted only for functions with an optimizable region.
Dropped from v1: arithmetic/call-return/switch-site profiling,
monomorphic-target slots, map-shape slots, branch counters (all
shelved with their consumers, §9).

Interior and loop-carried types come from forward dataflow seeded
by entry speculation, the `Type` chunk, and proof carried by
**flag-checked arithmetic**: compute into scratch with flag-setting
instructions, branch on overflow to the side-exit, commit after —
T1's own fast-path pattern minus the hoisted type checks, measured
at ~zero cost in the MVP. (This satisfies `03` §9.3's
deopt-at-sync-point constraint with its wording amended from "deopt
before the operation" to "deopt before the *commit*".)
`speculate_range` and its (never-designed) range profiling are cut;
the one IR invariant that survives from the MVP's corruption bugs:
*every value a speculative lowering consumes must have its type
established by a dominating guard or proof — loop-header phis are
where this is easy to get wrong, and the IR validator enforces it.
When fusing type checks into one mask, AND not OR: the combined
predicate must require every input to satisfy every bit.*

Steady-state tax budget: **≤ 1 %** on every tracked benchmark
(entry-only sites are the cheap kind).

### 4.5 Install, yield, uninstall

**Install** is the NIF model (full mechanics: `06` §§1–3): patch the
single `b next` instruction at `L_f + 4` in the function prologue —
every caller kind (external, intra-module direct branch, fun,
apply) passes through it — to branch to the T2 entry stub. The
frame is already pushed when the patch runs; the stub does its own
reduction check. The T1 body stays intact as the side-exit landing
zone and the in-flight fallback. Strict mutual exclusion with
trace/NIF on the prologue: T2 installs only on a clean prologue;
trace-enable jettisons T2 first ("trace always wins").

**Yield**: function-entry yields demote the invocation to T1 (cheap,
rebounds on next call). **Loop back-edge yields resume into T2**
via a per-loop resume stub — without this, any invocation longer
than one timeslice (~4 000 reductions ≈ iterations) runs its entire
remainder in T1, which forfeits exactly the workloads the tier
targets. This is *not* OSR-entry: resumption happens at a
T2-defined yield point with fully synced state; there is no T1→T2
mid-loop state mapping anywhere. The state saved at a back-edge
yield is the loop state as a fresh-call argument vector, so
jettison-time translation of a stale `c_p->i` is "point it at the
T1 entry". Reductions are charged identically to T1 (per call, per
iteration at back-edges).

**Uninstall** (watchpoint fired, trace enabled, eviction,
exit-counter saturation): revert the prologue patch under the
`code_ix` lock, thread-progress sync, fix `c_p->i` stragglers
lazily. O(1), no stack scans (§4.3).

**Requirements this creates**, both bounded ERTS changes verified
against the code: T2 blobs must register for PC→MFA+line lookup
(`beam_ranges.c` is strictly per-module today), and T1 must emit a
small per-eligible-function PC side table — function entries, call
ops, post-call continuations, post-effect boundaries. (The
per-instruction PC table earlier drafts assumed **does not exist**
in BeamAsm; only per-function line tables do.)

### 4.6 Self-correction

Per-blob exit counters with the `03` §9.5 policy — jettison budget
`100·2^R` for non-loop speculation sites, `25·2^R` in loops, where
R is the recompile count; on recompile, sites that failed widen
their speculation or drop it. *Static* unsupported-op exits that
saturate demote the function permanently for the module's lifetime
(recompiling cannot improve them). The floor is always T1.

### 4.7 Carried over unchanged from the reference design

In-scope v1 components whose detailed designs live in 00–07, with
the one-line versions an implementer needs:

- **Install/side-exit mechanics** (`06` §§1–3, §5.1–5.2): the entry
  stub assumes the frame is already pushed (the prologue ran), does
  its own FCALLS decrement, and sets `ARG3 = L_f` so the
  MFA-from-PC contract holds; install requires a clean prologue
  under the `code_ix` write lock; co-located code allocation keeps
  the patched `b` in ±128 MB range, with a per-module bridge pool
  as fallback.
- **One-untag arithmetic** (`03` §9.4): tagged + untagged preserves
  the tag through addition; multiplication untags both and retags.
- **Guard BIFs as primitive ops** (`04` §10.7, minus `length/1`):
  pure, CSE-able, inline-lowered.
- **Code cache** (`05` §13): separate evictable region, default
  64 MB, single compile writer. **Tier-up** (`05` §15): call-counter
  threshold `base·sqrt(size+1)·2^recompiles·M/(M−U)`, profile
  stability check before compile, bounded queue.
- **Observability** (`07` §16): `erlang:t2_stats/0`,
  `erlang:t2_info/3`, trace/dump flags. **Testing** (`07` §16A):
  the identity-transform suite (full OTP suite under `+JT2enable`
  with optimizations off — the state-preservation sanity check),
  forced-deopt harness, lifecycle test BIFs, concurrency stress,
  regression benches with CI gates.

## 5. The compatibility contract

The bar: **no observable difference vs T1 except speed.**

1. **T2 never raises.** Anything that would raise side-exits first;
   T1 re-executes and raises. Error terms, stacktraces, line
   numbers byte-identical by construction. (Also why v1 needs no
   exception IR — `try`/`catch` ops are region terminators.)
2. **Stack CPs are always T1 addresses** (§4.3) — CP-based
   introspection identical without translation.
3. **`c_p->i` is a T1 address or a registered T2 resume stub**
   resolving to correct `{M,F,A}`+line (§4.5).
4. **Reductions identical**: every call costs its reduction,
   recovered loops pay at the back-edge; `process_info(_,
   reductions)` matches T1.
5. **Tracing/NIF: strict mutual exclusion + jettison-on-enable.**
   The in-flight case is pinned down: trace-triggered jettison is
   ordered before the breakpoint commit, and `trace_pattern`'s
   existing code-barrier machinery **already guarantees** that by
   the time the BIF returns, every scheduler has passed a
   scheduling boundary — verified against
   `erl_bif_trace.c`/`code_ix.c`/`erl_process.c`
   (`../verification/RESULTS.md` §1). So "every call after
   `trace_pattern` returns is traced" holds identically to T1; the
   only delta is inside the BIF's own execution window, where T1 is
   also racy. Self-enable is correct by construction in v1
   (effect-free loop bodies cannot call `trace_pattern`;
   demote-on-return already routes the rest of that invocation to
   T1). `save_calls` is expected tier-invariant via export
   indirection; the inspection matrix confirms the call-side
   accounting.
6. **GC discipline identical** at every GC site; scratch registers
   holding term pointers are dead across GC calls.
7. **The one residual observable**: sampling another process's
   `current_function` mid-inlined-leaf reports the outer function —
   the same behaviour `erlc +inline` exhibits today. Documented,
   not hidden.

Deliverable: an **inspection matrix** (companion to the trace
matrix of `05` §12.5 — every trace flag and match-spec action
classified by preservation mechanism) — every introspection surface (`process_info` items,
crash-dump fields, `process_display`, error stacktraces with line
numbers, perf/gdb) × preservation mechanism × a test, including
mid-loop-yielded processes.

## 6. Benchmarks and corpus

The optimization target is the **application corpus**: RabbitMQ,
MongooseIM, Phoenix/Ash-class services. Tracked suite, every entry
labelled by op class so wins are attributable:

- **awfy** — the 14 AWFY benchmarks in Erlang and Elixir, plus the
  `otp_benchmarks` families (estone, maps, base64, binary_match,
  ets).
- **JSON encode/decode** (new awfy family to add): stdlib `json`
  over the nativejson trio — twitter (strings), citm (structure),
  canada (numbers) — plus a ~1.5 KB API-response payload. Local
  comparators: a SIMD/NIF parser as the known-unreachable ceiling,
  Jason as the pure-BEAM peer. Goal: make stdlib `json` the
  default sensible choice, not beat handwritten SIMD. The G-bin
  results (6–10 % from three functions) make this the compound
  benchmark for the whole tier: the residual profile is dispatch
  (measured null for specialization) + map construction (G-map) +
  conversion BIFs (out of scope).
- **OTP in-tree benches** (`HOWTO/BENCHMARKS.md`).
- **Macro leg** — MongooseIM under Amoc
  (`awfy/PLAN/MONGOOSEIM_BENCH_PLAN.md`, Dockerfiles already in
  place); also the host for P0's cycle-weighted profiling.

Honest per-class v1 expectations: int/list/tuple loop benchmarks
1.5–2×; binary-scan-shaped code large (G-bin class, now in the v1
op set); floats/full maps/processes-ETS-runtime unchanged;
application macro low single digits until the map expansion lands
(the census's dialyzer arithmetic illustrates the ceiling shape:
the pure-loop bucket held 6.3 % of dynamic calls, so even 2× on
all of it is ~3 % end-to-end — binaries are what move app-level
numbers).
Elixir note: `Enum` pipelines pass funs as parameters, so v1's
literal-fun intrinsics fire for direct `lists:*` calls (pervasive
in Erlang code) but not through `Enum` wrappers — that's the
shelved cross-module case.

The census corpus structure (static + the dialyzer dynamic leg) is
in `../verification/RESULTS.md`; per §2.3 rule 1, its call-weighted
shares are structure, not cycle pools.

## 7. The binary expansion package (post-v1, green-lit)

First expansion after v1, *measured* end to end by G-bin. Contents:

- Full byte-aligned `bs_*` coverage in recovered loops beyond the
  v1 scan subset (multi-byte extraction, `bs_match_string`,
  context-threading across helper inlining).
- **Loop unrolling + the byte-lane (SWAR) recipe library.**
  Unrolling (×8 default for byte loops) enables: bounds-check
  coalescing with a scalar epilogue; adjacent-load merging;
  predicate lane-combining recipes (`== C`, `< C`, range,
  small-set — ~5 recipes cover the byte-class tests that occur)
  with OR-reduction and a scalar tail that relocates the stop byte.
  Worth ~2× on top of fused bytewise scanning [measured]; the
  package's acceptance bar is G-bin's full **≥4× isolated scan**
  (the v1 gate covers only the bytewise layer, §8 P2). The
  control→data conversion is legal under the §4.2 window rule, and
  reductions stay faithful (a single `subs FCALLS, K` per chunk is
  observably equivalent to K per-iteration decrements; yields at
  chunk granularity, as T1's own source-unrolled loops already
  have). Limits: per-byte loop-carried value dependences stay
  scalar; UTF-8 doesn't unroll. (Unrolling's original
  `test_heap`-coalescing motivation, `04` §10.6, still awaits
  corpus evidence.)
- Construction (`bs_create_bin`) batching is the follow-on, with
  the encode-side scans (`escape_binary_ascii` — same shape as the
  decode scans) as the first target.

Then the **map expansion**: shape feedback slot (runtime-recorded
keys-tuple pointer, per §2.3 rule 3) + region-level guard +
direct-offset access — 1.64× on two-field access [measured], the
guard amortising further over wider regions; flatmaps first,
hashmaps out of scope.

## 8. Roadmap

### Track A — T1/AOT captures (first; ~5–8 weeks of prototyping plus the re-baseline run, all upstreamable)

Each measured win has at least a partial cheaper home; harvest
those before paying for a tier, and let T2 face the strengthened
baseline. None of this needs profiling, speculation, or deopt:

- **A1. Scan-run superinstruction** (~2–4 weeks prototype). AOT
  recognizes byte-class scan loops (the stereotyped run-scan shape:
  count/skip a class) and emits a `bs_match`-family "scan run" op;
  T1 implements the fused, SWAR'd loop. Captures the head of the
  G-bin distribution for json/protocol parsers at compiler+emitter
  cost. Pattern-bound by design: misses state machines,
  accumulating parsers, UTF-8, refactored variants — that long
  tail is T2's case. (Degradation modes differ: an AOT pattern
  miss is a silent cliff to full T1 cost, whereas T2 degrades
  gradually via side-exits.) An AOT auto-unroll of scan clauses
  (generalizing `string_ascii`'s hand-unroll; no new instruction)
  is a cheaper sub-option capturing the middle of the win.
- **A2. Map-key inline cache in T1** (~2–3 weeks prototype). Per
  `get_map_elements` site: cache `{keys-tuple ptr → slot offsets}`;
  hit → direct loads, miss → existing generic scan + refill. No
  deopt (the miss path is today's code), works on **existing
  beams including Elixir**. This is the previously-shelved
  inline-cache idea, which G-map's 1.64× has now effectively
  priced; the earlier decision to drop it in T2's favour is
  reversed.
- **A3. AOT inlining defaults** (~1 week measurement): default-on
  local inlining and `inline_list_funcs`, capturing the
  call-elimination half of the MVP win where types/ranges are
  statically provable.

### Re-baseline checkpoint

Re-run the benchmark suite (§6) against the Track-A-improved T1.
T2 v1 proceeds if its residual case — speculation on
non-provable types/ranges, shape generality beyond recognized
patterns, the long tail of binary loops, composition of all wins in
one loop body — still clears the bar against ~24 weeks plus
permanent ownership. This is a genuine go/no-go: shrinking T2 to a
later, smaller project is an acceptable outcome, and the
verification work transfers either way.

The checkpoint also weighs the JIT against the **VM-internal
track** surfaced by the real-application profiling
(`../verification/PROFILE_RESULTS.md`): on deployed-server
workloads the dominant pools are the message/signal-delivery path,
GC (9.5–12.4 % of busy everywhere), ETS hashing+locking, send-side
copying, and allocator churn — several times the JIT-addressable
share on those corpora. Before Track B starts, the hottest of
these (the message/signal path) deserves the same treatment the
JIT got: the cheapest experiment that prices it. The next ~24
weeks should go to whichever pool prices highest.

### Track B — T2 v1 build (~24–26 weeks, ~10–11 KLOC)

Gate numbering: G1/G2/G4 below; G3 was the call-crossing gate and
already ran — both subjects null (§2.1).

| Phase | Weeks | Contents | Gate |
|---|---|---|---|
| **P0** | 4–5 | Bytecode→SSA builder + code-chunk retention; T1 PC side table (4 entry kinds, §4.5); blob range registration design; trace + inspection matrices; entry-only profile-cost measurement; cycle-weighted corpus profiling — **first round complete** (`../verification/PROFILE_RESULTS.md`: dialyzer, RabbitMQ at 489 k msg/s, Bandit at 148 k req/s — server workloads are dominated by VM-internal costs: signal/message path, GC 9.5–12.4 %, port, ETS, copy/alloc; JIT-addressable code is a ≤ 25 % long tail there; MongooseIM-under-Amoc and a Linux `perf` pass remain); Graviton re-run of the experiment kit | **G1: SSA fidelity** — reconstruct SSA for a corpus (the experiment subjects + a stdlib slice), identity-emit, compare against T1 behaviour and AOT `beam_ssa` structure. Material loss → SSA-chunk fallback. |
| **P1** | 4 | Identity transform through the full pipeline; install/jettison; range registration + `c_p->i` translation; full OTP suite under `+JT2enable` | Suite green: state preservation proven end-to-end. |
| **P2** | 6–7 | Entry speculation; flag-exit arithmetic; guard fusion; self-tail-recursion loop recovery + preheader hoisting + back-edge resume stubs; local leaf inlining; **the v1 binary scan subset** | **G2: reproduce the MVP (≥1.8×) and G-bin's *bytewise* layer (≥2.5× isolated scan — SWAR is the §7 package, gated at ≥4× there) through the pipeline**, ≤1 % tax on the application corpus. Miss → stop. |
| **P3** | 6 | `lists:*` intrinsics + helper loop recovery + constant-fun inlining; LICM-lite | **G4**: `foldl`/`map` vs the post-Track-A default configuration (with A3's inlining defaults *on* — anything else is a strawman). |
| **P4** | 4 | Eviction, watchpoints (module granularity — the index includes each blob's own module for trace lookups), observability (`erlang:t2_stats/0`, memory keys), docs, integration runs | Hard floor: ≤1 % regression on every tracked benchmark. |

Then the expansion packages of §7, in order: binary, then maps.


## 9. The shelf — deferred on negative evidence

The governing rule, unchanged since the rescope: **no
infrastructure lands ahead of a validated win that needs it.**
Reopening any row below requires an MVP-style hand-built experiment
(the §2 methodology) before its infrastructure is green-lit — the
"reopens if" column states the evidence that would justify running
one. Everything here has a finished design in 00–07.

| component | design | why shelved | reopens if |
|---|---|---|---|
| General inlining: framestates, eager-CP-push, per-region deopt stubs | `03` §9.2, `01` §6.5 | G3-2 null: call overhead is not a pool; only elimination pays | a cycle-profiled, elimination-rich call-boundary corpus shape is found (G4 is the remaining probe; G3-1 also ran null) |
| CPs into blobs: tombstone tables, lazy stack scan; plus the eager own-stack scan the trace path then needs for self-enabled `trace_pattern` | `06` §5.3–5.5, `05` §14.2 | needed only by general inlining; demote-on-return ≈ return-into-T2 for spines (inferred from G3-2's structure) | general inlining reopens — and must bring the trace-path own-stack scan with it |
| Branch-frequency counters, cold-arm pruning | `02` §7.7, `04` §10.3 | G3-1 null: T1 clause dispatch is ~4 ns; gen_server machinery outweighs dispatch 40:1 | Graviton re-run contradicts the null, or a dispatch shape with eliminable per-arm work appears |
| Monomorphic-target slots, cross-module inlining; with them, tier-up target selection (compile the dominant caller) and per-call-site deopt-skip | `02` §7.5, `04` §10.1, `07` App C M1/H10 | same evidence; Elixir `Enum` is the known motivating case | as above + the Enum-wrapper case sized by cycle profile |
| `speculate_range` + range profiling | `03` §9.3–9.4 | flag-checked overflow measured ~free; range slots were never designed | an LICM-hoistable range-guard win flag checks can't capture |
| Polymorphic PIC, speculative funs | `03` §9.6, `04` §10.2 | v2 by design | post-v1 |
| Messages/NIFs/floats coverage | `07` §17 Phase 8 | no measured pool yet | corpus profile says otherwise |
| Per-instruction T1 PC table | corrected in §4.5 | only general mid-function deopt needs it | general inlining reopens |
| Erlang JIT-server process | `05` §15.3 | a C-side MPSC queue + one dirty-CPU job suffices | scheduling policy outgrows a queue |
| SSA chunk in BEAM file | `02` §7.8 | reconstruction expected to suffice; reach argument | **G1 failure only** |

## 10. Open questions

- **`beam_ranges` extension shape** for T2 blob registration
  (separate range class vs folding into module ranges; purge
  ordering). Decide in P0 with the inspection matrix.
- **Retained-code-chunk accounting**: which `erlang:memory/0` key;
  drop retention when all eligible functions are blacklisted?
- **G1 pass criteria**, concretely: identical CFG shape, live-range
  count within ε, identity-emit passing the identity-transform
  suite (`07` §16A.1) on the corpus.
- **Fun effect classification for intrinsics**: effect-free fun →
  whole iteration is one window; BIF-effect fun → window boundary
  inside the iteration (speculation suspended between effect and
  back-edge — `foreach`'s effect-last shape loses nothing);
  Erlang-call fun → plain `call_ext` fallback. One shared effect
  table with the §4.2 validator.
- **Track A ownership**: the superinstruction and the IC are
  compiler/T1 work that can proceed independently of (and benefit)
  T2 — confirm staffing before the re-baseline gates on them.

## Appendix — the experiment kit

All experiment code is in-tree, env-gated, off by default, on this
branch:

| gate | targets | code |
|---|---|---|
| `T2_G3=a\|b` | `erl_types:are_all_limited/2` (+ `is_limited` label) | `arm/beam_asm_module.cpp` `emit_t2_are_all_limited_2` |
| `T2_GBIN=1` | `json:number/7`, `number_frac_cont/7`, `string_ascii/7` | `emit_t2_json_scan` |
| `T2_G31=1` | `t2_g31:dispatch/2`, `handle_call/3` | `emit_t2_g31_*` |
| `T2_GMAP=1` | `t2_gmap:sum_scores/2` | `emit_t2_gmap_sum_scores_2` |
| (static) | `t2_mvp:total/2`, `diff/2` | `emit_t2_total_2` (MVP) |

Harnesses and writeups in `PLAN/verification/`; benchmark corpora
documented in each outcome file's Reproduction section. The MVP
hook (`emit_i_test_yield` → `T2FunctionEntry` → specialized bodies
at `emit_int_code_end`) is experiment scaffolding — production
install is the prologue patch of §4.5.
