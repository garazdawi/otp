# T2 v1 — The Loop Tier (third-pass rescope)

> Part of the T2 design. See [`README.md`](README.md) for the full
> document index. **This file supersedes the v1 scoping in
> [`00_overview.md`](00_overview.md)–[`07_delivery.md`](07_delivery.md).**
> Those files remain the reference design for every component deferred
> here — each cut below names the section it returns to. Written after
> the MVP completed; grounded in
> [`../mvp/OUTCOME.md`](../mvp/OUTCOME.md) and a code-level audit of
> the ERTS facts the earlier files assumed.

## 1. Why this revision

The MVP answered its question: the architecture beats T1 by ~2×
(1.97× min, within 2 % of the instruction-count ceiling) on a hot
list-walking loop. What it validated, precisely:

- Prologue-side install with the intact T1 body as the side-exit
  landing zone.
- Iteration-start-stays-live → side-exits with no stub work, no
  metadata, no register reconstruction.
- Guard fusion + leaf inlining + register-resident loop state across
  a recovered self-tail-call loop (the T2-internal back-edge alone
  was worth 6 %, taking 1.85× to 1.97×).
- Flag-checked (post-hoc) overflow side-exits costing ~nothing.
- Side-exit-and-reclaim: the worst case costs roughly what pure T1
  would have paid anyway.

What it did **not** validate: cold-arm pruning on branchy dispatch
code, polymorphic call sites, map-shape specialisation,
profile-driven range speculation, cross-module monomorphic inlining.
The 00–07 v1 (~48–50 weeks, ~16 KLOC) builds infrastructure for all
of these up front: framestates and eager-CP-push, an SSA chunk in the
BEAM file, four kinds of profile site, lazy whole-stack CP scans, an
Erlang JIT-server process.

`00_overview.md` §1 itself concedes the corpus reality: branchy
state-machine code dominates production Erlang, and the wins there
are *expected* from pruning and guard elimination — expected, not
measured. Meanwhile the measured 2× lives in the loop-shaped class
and needs almost none of the heavy machinery.

The rescope principle:

> **No infrastructure lands ahead of a validated win that needs it.**
> v1 ships the validated win class with the minimum machinery. Each
> unvalidated win class gets an MVP-style hand-built experiment (per
> `../mvp/OUTCOME.md` §"Next benchmark") before its infrastructure
> is green-lit.

Every architectural invariant of 00–07 survives intact: T1 calling
convention, sync-point state preservation, deopt-to-T1, strict
trace/NIF mutual exclusion, T1 as the permanent source of truth.
Nothing here precludes growing back into the full design — the cuts
are sequencing, not abandonment.

## 2. v1 scope: what the loop tier compiles

A function is compiled when its call counter trips **and** static
structure puts it in the validated class:

1. **Self-tail-recursive functions** → loop recovery. The MVP shape,
   and the canonical Erlang loop idiom. Entry guards hoist to the
   loop preheader (run once per invocation, not per iteration);
   loop-carried values stay in CPU registers across the back-edge.
2. **Functions calling a `lists:*` higher-order helper with a
   literal fun** → intrinsic inlining (`04_optimization.md` §10.4's
   hand-ported expansions) + loop recovery of the helper, fun body
   inlined via constant propagation.
3. **Leaf functions with guard-heavy destructuring** — speculate
   entry types, fuse the guards, return. No loop needed; same
   machinery minus the back-edge.

Inside these bodies, the supported op set is Phase A as before:
integer arithmetic, comparisons, cons/tuple destructure and
construction, guard BIFs as primitive ops, and local calls to
*leaf* functions small enough to inline (≤ the §10.3 size cap, no
calls of their own).

Two rules make the boundary cheap instead of cliff-shaped:

- **An op outside the set terminates the optimized region with a
  static side-exit** — an unconditional branch to that op's T1 PC,
  the same shape as a failed speculation (`06` §4.2). The function
  still compiles; the unsupported tail just runs in T1. The old
  whole-function eligibility cliff ("one map op anywhere disqualifies
  the function") is gone.
- **Non-inlineable calls demote-on-return** (§3 S3): the call is
  emitted with the *T1 continuation* as its CP, so the callee
  returns into T1 and the rest of that invocation runs there. The
  compile filter simply avoids picking functions whose optimized
  region is punctured by such calls.

Functions with no optimizable region get no counters and no profile
slots — the zero-overhead property of `02_profiling.md` §7.1 is
preserved, just with a looser, region-based notion of "eligible".

**Loop bodies must be effect-free after leaf inlining** (no send,
no ETS, no process dictionary, no non-inlined call). Allocation is
not an effect (an abandoned partial iteration leaves garbage, not
state). This is what makes the one-shape deopt model of S2 sound,
and it matches the validated class exactly — effectful loops are
runtime-dominated anyway. `lists:foreach/2` with a side-effecting
fun is excluded; `map`/`filter`/`foldl` with pure funs are the
targets.

## 3. The five structural simplifications

### S1. Build the IR from the loaded BEAM code — no SSA chunk

**Supersedes** `02_profiling.md` §7.8 and the "BEAM SSA at runtime"
resolved decision in `07_delivery.md` §18.

v1 builds T2 IR directly from the module's loaded BEAM code: the
loader retains the raw code chunk (plus atom/literal/import tables)
for modules with eligible functions — today `beamfile_free()`
discards the decoded ops after native emission (`beam_file.c`), so
retention is a new but small loader change — and the T2 compile job
re-decodes and runs standard SSA construction (Braun-style; the CFG
is explicit in BEAM labels, the X/Y registers are the variables).
Types are seeded from the existing **`Type` chunk**, which the
compiler already emits by default and BeamAsm already consumes at
load time.

What this deletes: the new BEAM chunk format, the AOT compiler
change, the loader chunk-parsing path, the `+t2_compile_eligible`
compile option, the 30–80 % file-size cost, risk #5
(`07_delivery.md` §18), and the whole class of SSA-chunk/code-chunk
version-skew bugs.

What this buys beyond deletion: **T2 works on every `.beam` that
exists today** — applications, dependencies, Elixir output — with no
recompilation. A tier that requires rebuilding the world with a new
compile option has near-zero perf reach in its first years; a tier
that applies to deployed code has full reach on day one. This is
the single largest real-world-performance lever in the rescope.

What is lost vs. the chunk: `beam_ssa`-level value annotations.
Recovered by the pass-2 forward dataflow (which the pipeline runs
anyway) seeded by the Type chunk. The `jit_inline` annotation
carrier is not needed in v1 at all — the intrinsic set is a T2-side
manifest (`04_optimization.md` §10.4 already chose "ported, not
generated"); the `-jit_inline` module attribute (which travels in
BEAM files for free) covers library-author opt-in later.

Memory cost: ≈ the code chunk, only for modules with eligible
functions, freed on purge. Reported under a new `erlang:memory/0`
key alongside `jit_t2_code`.

**Fallback**: if the Phase-0 fidelity gate (G1, §6) shows
reconstruction loses material structure, the SSA-chunk design in
`02_profiling.md` §7.8 is the documented fallback. It is a fallback,
not a parallel track.

### S2. One deopt shape: re-call — no framestates, no eager-CP-push

**Defers** `03_compilation_and_speculation.md` §9.2 and the
framestate machinery of `01_ir_and_state.md` §5/§6.5 to the general-
inlining phase (post-G3).

Every v1 side-exit reconstructs **a valid argument vector at a call
boundary** and branches to a T1 PC. Three cases, one shape:

1. **Outer guards / loop iteration**: re-execute the iteration from
   the function entry with the iteration-start values (the MVP
   model, byte-for-byte).
2. **Inlined `lists:*` helper loop**: tail-recursive helpers have
   the property that mid-loop state *is* a valid fresh-call state —
   `foldl` suspended at element k ≡ `foldl(F, Acc_k, Rest_k)`.
   Deopt materialises `(F, Acc_k, Rest_k)` into the call's argument
   X-regs and branches to the T1 PC **of the call op itself**; T1
   performs a genuine call to the genuine helper and the walk
   finishes generically. Identical result, identical stacktraces if
   anything later raises.
3. **Inlined local leaf**: the leaf body is straight-line and
   effect-free, so the enclosing iteration's re-execution covers it
   (the MVP's `diff/2` case — `OUTCOME.md` finding 3).

The legality rule that replaces framestates is **guards-before-
effects**: within a re-execution window, every deopt-able guard
precedes the first effect. Since v1 loop bodies are effect-free
(§2), the rule is enforced trivially by the compile filter plus an
IR validation pass.

Deleted: `T2FrameState`, `parent_fs` chains, per-region deopt-stub
X/Y-restore emission, eager CP pushes, multi-level inlining
metadata. The inlining depth that remains (leaf + one intrinsic
level) keeps the deopt story one-shaped.

### S3. No CPs into T2 blobs — the lifecycle collapses

**Supersedes** the tombstone CP-patching and lazy whole-stack scan
of `05_runtime.md` §14.2 and `06_dispatch_and_sideexit.md` §5.3–5.5
for v1.

Three rules:

- **(a)** Calls T2 emits that aren't inlined are emitted as
  "load the *T1 continuation PC* into LR, then branch" (two
  instructions instead of `bl`), so the frame the callee pushes
  holds a T1 address. The callee returns into T1 mid-function; the
  sync-point invariant makes that exactly the state T1 expects at
  the post-call boundary. CP values are arbitrary unvalidated code
  addresses in BeamAsm, so this is free. A loop re-enters T2 at its
  next pass through the patched prologue.
- **(b)** Inlined regions push no CP at all (S2).
- **(c)** The only T2 addresses the runtime ever holds are
  back-edge yield-resume PCs (S5), confined to `c_p->i` — never the
  Erlang stack.

Consequences:

- **Stacktraces and `process_info(_, backtrace | current_stacktrace)`
  can never contain a T2 address.** CP-based introspection is
  byte-identical to T1 *by construction*, not by translation.
- **Uninstall** = revert the prologue patch (`06` §5.2 steps 1–2)
  + thread-progress sync + translate `c_p->i` for processes yielded
  inside the blob — a single field compared against a generation
  counter at schedule-in, no stack walk. Deleted: per-blob
  CP-to-T1-PC side tables, the lazy stack scan, continuation
  trampolines, the high-water sweep, `+JT2lazy_scan_max_depth`.
- **Eviction becomes trivially safe** — same sequence as uninstall.

Cost: the post-call remainder of an invocation runs in T1 after a
real call. On the v1 target class (§2) the optimized region
contains no such calls, so the cost is nil where it matters.

The full CP machinery returns, unchanged from `06` §5, when general
mid-function inlining lands.

### S4. Profiling: entry types + a counter; overflow by flags, not ranges

**Trims** `02_profiling.md` §7.3/§7.5/§7.6/§7.7 and **cuts**
`speculate_range` from v1
(`03_compilation_and_speculation.md` §9.3–9.4 amended below).

Kept: the per-function call counter and **function-entry per-arg
type slots**. Dropped for v1: arith-operand sites, call-return
sites, switch-arg sites (no consumer in the v1 pass list),
monomorphic-target slots (no general inlining to feed), map-shape
slots (Phase 5), branch-frequency counters (gated on G3, §6).

Interior and loop-carried types come from forward dataflow seeded
by entry speculation + the Type chunk + **proof carried by
overflow-checked arithmetic** (an `adds`/`b.vs` whose exit didn't
fire proves the result is small). The invariant that MVP findings
#4 and #5 bought us (the AND-not-OR fused check; the Net-bignum
corruption):

> Every value a speculative lowering consumes must have its type
> established by a dominating guard or proof. Loop-header phis are
> where this is easy to get wrong; the IR validator enforces it
> mechanically.

`speculate_range` is cut entirely from v1:

- It requires min/max range profiling that **no designed profile
  slot collects** — `02` §7.2's slot is a type bitmask + saturating
  count; the second-pass critique's H8 auto-selection rule
  ("observed × 1.5") assumed data that was never going to exist.
  This was a latent inconsistency in 00–07; resolving it by adding
  range fields would have doubled profile cost for one consumer.
- The MVP measured the alternative — compute into scratch with
  flag-setting instructions, `b.vs` to the side-exit, commit only
  after — at effectively zero cost (within 2 % of ceiling *with*
  the V-flag branches in the loop). `mul` uses the
  `smulh`-compare pattern T1 already uses.
- The deopt-at-sync-point constraint (`03` §9.3) is still satisfied:
  nothing is committed to X/Y before the flag check, so the exit
  re-executes the whole BEAM op in T1, which handles the bignum
  case generically. The constraint's wording changes from "deopt
  before the operation" to "deopt before the *commit*".

The one-untag trick (`03` §9.4) stays verbatim. `speculate_range`
returns in v2 only if LICM-hoistable range guards show a measured
win the flag checks can't capture.

With profiling reduced to entry-only sites, the steady-state tax
budget tightens from 3 % to **1 %**.

### S5. Loop back-edge yields resume into T2

**Supersedes** `05_runtime.md` §12.4 item 3's "yielded T2 frame
demotes to T1" for loop back-edges (function-entry yields keep the
demote model — they're pre-work and rebound on the next call).

Why this must be v1, not v2: a timeslice is ~4 000 reductions ≈
~4 000 loop iterations. Under demote-on-yield, any fold longer than
one timeslice runs its first slice in T2 and the **entire remainder
in T1** — for recovered+inlined helper loops the T1 landing is the
generic helper, which never re-enters T2 within the invocation. The
bigger the loop, the smaller the win, on exactly the workload this
tier exists for. (Self-recursive functions dodge this — T1's tail
call re-enters through the patched prologue every iteration — but
inlined-helper loops, case 2 of §2, do not.)

Mechanism: the back-edge yield saves `c_p->i` = a per-loop **T2
resume stub**. The state saved at the yield is the loop state laid
out as a fresh-call argument vector (S2), which makes the
jettison-time story trivial: translating a stale `c_p->i` means
pointing it at the **T1 function entry** — the saved X-regs already
form a valid call. The MVP implemented exactly this resume shape
(by accident of its hook placement) and it survived every test;
`06` §6 "Yield" describes it.

This is **not** OSR-entry: resumption happens at a T2-defined yield
point with fully synced state. There is no T1→T2 mid-loop state
mapping anywhere.

### Smaller cuts

- **The Erlang JIT-server process** (`05` §15.3): replaced by a
  C-side MPSC queue drained by a single dirty-CPU-scheduler job;
  install runs under the `code_ix` write lock as designed. Fewer
  moving parts, no kernel-app boot-order coupling, same
  single-writer property for the `JitAllocator`. The Erlang server
  returns if scheduling policy ever outgrows a queue.
- **Watchpoints at module granularity only** (`05` §14.1 trimmed):
  v1's only cross-module dependency is intrinsic-inlined `lists:*`,
  so the table is `Module → Vector<BlobRef>` with jettison-on-reload.
  Every blob also registers its **own module** (its inlined local
  leaves and literal funs live there — trace-enable lookups need it,
  §4). Per-function granularity returns with general inlining.
- **Tier-up target selection** (`07` App. C, M1) and per-call-site
  deopt-skip (H10): no general inlining to drive; deferred with it.

### A correction, independent of the rescope

`03_compilation_and_speculation.md` §9.1 asserts T2 deopt resolves
cold-tail addresses from "the T1 blob's per-instruction PC table
(which BeamAsm already maintains for line-number debugging)".
**That table does not exist.** BeamAsm maintains per-function line
tables only (`beam_ranges.c`), and `erts_debug:disassemble/1`
returns `false` under the JIT. The Phase-0 "T1 PC table audit"
would have discovered this; the rescope makes it moot:

v1 needs T1 PCs at exactly three kinds of point — **function
entries, call ops, and post-call continuations** (the latter for
S3's demote-on-return CPs). BeamAsm binds labels at all three
during emit; recording them into a small per-eligible-function side
table at load time is a bounded T1 change, far cheaper than the
per-instruction table the full design will eventually need (that
need re-arises with general mid-function deopt, post-G3, and should
be costed then).

## 4. Compatibility invariants — the non-negotiables

The bar: **no observable difference vs T1 except speed.** This
section is the contract; the inspection matrix below is its test.

1. **T2 never raises.** Any op that would raise side-exits first;
   T1 re-executes and raises. Error terms, stacktraces, and line
   numbers are byte-identical by construction. (This is also why v1
   needs no exception IR: `try`/`catch`/`raise` ops are simply
   outside the supported set and terminate the region per §2.)
2. **Stack CPs are always T1 addresses** (S3). `current_stacktrace`,
   `backtrace`, crash-dump stack sections, `process_display` —
   identical without translation.
3. **`c_p->i` is either a T1 address or a registered T2 resume
   stub** (S5) that resolves to the correct `{M,F,A}`+line. This
   requires registering T2 blob ranges for PC→MFA lookup:
   `beam_ranges.c` is strictly per-loaded-module today, so v1
   extends it (or adds a parallel index consulted by
   `erts_lookup_function_info`). Small, bounded ERTS change; also
   what perf/gdb metadata (`05` §12.5 item 6) needs.
4. **Reductions are identical.** Every call — inlined or not —
   costs its reduction; recovered loops pay at the back-edge
   (`05` §12.4 item 1 verbatim). `process_info(_, reductions)`
   matches T1 exactly.
5. **Tracing and NIFs: strict mutual exclusion + jettison-on-enable**,
   unchanged from `06` §2.4, plus the in-flight quiescence rule
   spelled out below. Trace semantics never observe T2.
   `save_calls` works via export indirection regardless of tier.
6. **GC discipline identical** at every GC site (`01` §6). One
   codegen consequence worth naming: scratch registers holding term
   pointers are dead across any GC call — values are reloaded from
   their X/Y homes after, since GC moves terms.
7. **The one residual observable**: sampling another process's
   `current_function` while it executes an *inlined leaf* reports
   the outer function. Precedent: `erlc +inline` does the same
   today. Documented, not hidden.

### Trace-enable against an in-flight loop (the hard case)

Setup: process P is mid-loop inside a T2 blob, and tracing is
enabled on something the blob inlined — a local leaf, a `lists:*`
helper, or the literal fun (funs are reachable via local-pattern
tracing of the compiler-generated local function). Jettison alone
("future entries go to T1") is not sufficient here, because P's
in-flight invocation executes inlined copies that pass through no
prologue. Two cases:

**Case A — another process enables the trace.** Ordering rule:

1. The trace path looks up affected blobs in the watchpoint index
   and jettisons them (prologue revert + generation bump) *before*
   the breakpoint stage/commit, under the same code-modification
   permission.
2. P keeps executing the inlined (untraced) copy only while the
   `trace_pattern` call is still in flight. The BIF's existing
   staged-breakpoint **thread-progress wait doubles as blob
   quiescence**: a scheduler only passes a progress point at a
   scheduling boundary, so by the time `trace_pattern` returns,
   every process that was inside the blob has yielded out of it —
   the back-edge yield saved `c_p->i` = resume stub, and the
   generation check at next schedule-in redirects it to the T1
   entry (a valid fresh call, S2/S5). From then on every call to
   the traced function goes through its traced prologue.

The resulting guarantee is **identical to T1's**: calls concurrent
with the `trace_pattern` call itself are racy (they are under T1
too, while breakpoints are staged); every call made after
`trace_pattern` returns is traced. The delta vs T1 is confined to
that window: a mid-loop T1 process would start emitting per-
iteration call events as soon as the breakpoints commit, a few
iterations earlier than the jettisoned T2 process. Nothing after
the BIF returns differs. P0 must verify that the existing
staged-bp thread-progress ordering actually provides the
return-implies-quiescence property (trace-matrix row), and 16A
gets a forced test: enable tracing against a process mid-T2-loop,
assert no post-return call goes unreported.

**Case B — P enables the trace itself.** Unreachable from inside a
v1 loop body (bodies are effect-free, §2; `trace_pattern` is about
as effectful as a BIF gets). The only way P can execute
`trace_pattern` is through a real, non-inlined call — and S3's
demote-on-return already pointed that call's CP at the **T1
continuation**, so by the time the BIF runs, the remainder of P's
invocation is destined for T1 regardless; the inlined copies it
would have executed are unreachable. The synchronous prologue
revert inside the BIF covers P's *next* invocation. Correct by
construction, no window at all. (Post-G3, when general inlining
puts CPs into blobs, this stops being automatic — the enabling
process could return *into* a tombstoned blob and keep executing
inlined copies after its own `trace_pattern` returned. The trace
path must then eagerly scan the calling process's own stack before
returning; recorded in §7 so it lands with that machinery.)

One index consequence: the module-granularity watchpoint table must
record the blob's **own module** as a dependency, not only `lists`
— a local trace pattern on an inlined local leaf (`{M,diff,2}`)
must find the blob for `{M,total,2}`. Coarse (any trace in module M
jettisons all of M's blobs) but correct, and trace-enable is rare;
recompile follows automatically when the pattern clears
(`06` §5.1's temporary jettison-and-recompile).

**Deliverable: an inspection matrix** alongside the Phase-0 trace
matrix (`05` §12.5) — every introspection surface (`process_info`
items, crash-dump fields, `erlang:process_display/2`, error
stacktraces with line numbers, perf/gdb integration) × its
preservation mechanism × a test that exercises it against a
T2-compiled function, including mid-loop-yielded processes.

## 5. What stays from 00–07, unchanged

- Install/uninstall mechanics: `06` §§1–3 and §5.1–5.2 (minus the
  CP-scan steps S3 removed). The prologue patch at `L_f + 4`, the
  entry-stub contract, mutual exclusion, the bridge pool.
- The sync-point state model: `01` §6 (v1 simply has fewer sync-point
  kinds in play).
- One-untag arithmetic: `03` §9.4.
- Recompile/backoff policy: `03` §9.5, with one amendment — exits
  at *static* unsupported-op side-exits that saturate trigger a
  permanent demote for the module's lifetime (recompiling cannot
  improve a static exit), while speculative-site exits follow the
  existing widening policy.
- Guard BIFs as primitive ops: `04` §10.7 (minus `length/1`,
  already removed).
- Code cache and budget: `05` §13, with eviction simplified by S3.
- Tier-up counter and thresholds: `05` §15.1–15.3.
- Observability: `07` §16 in full, plus the inspection matrix.
- Testing strategy: `07` §16A in full — identity-transform suite,
  forced-deopt harness, lifecycle BIFs, concurrency stress,
  regression benches.

## 6. Phases, effort, and decision gates

| Phase | Weeks | Contents | Gate |
|-------|-------|----------|------|
| **P0** | 4–5 | Bytecode→SSA builder + code-chunk retention; T1 PC side table (entries / call ops / continuations); trace matrix + inspection matrix; entry-only profile-cost measurement; corpus measurement (kept from old Phase 0) | **G1: SSA fidelity.** Reconstruct SSA for a corpus of OTP functions; structurally compare against AOT `beam_ssa` output and identity-emit behaviour. Material loss → fall back to the SSA chunk (`02` §7.8). |
| **P1** | 4 | Identity transform through the full pipeline; install/jettison; blob range registration + `c_p->i` translation; full OTP suite under `+JT2enable` (16A.1) | Suite green. State-preservation model proven end-to-end. |
| **P2** | 6 | Entry speculation; flag-exit arithmetic; guard fusion + strength reduction; self-tail-recursion loop recovery + preheader hoisting + back-edge resume stubs; **local leaf inlining** (≤ size cap, no calls) | **G2: reproduce the MVP through the pipeline.** The hand-written MVP hit 1.97×; the compiled pipeline must hit ≥ 1.8× on the same benchmark, with ≤ 1 % tax on the application corpus. Miss → stop and re-examine before P3. |
| **P3** | 6 | `lists:*` intrinsics (hand-ported expansions) + helper loop recovery + constant-fun body inlining; LICM-lite (preheader guard/capture hoisting); unrolling **only if** `test_heap` coalescing shows up on the corpus, else defer | **G4: intrinsics pay.** `foldl`/`map` benchmarks vs an `inline_list_funcs`-off baseline show the projected win. |
| **P4** | 4 | Polish: eviction, watchpoints-lite, `erlang:t2_stats/0`, memory keys, docs, integration runs (RabbitMQ, OTP suite, Elixir compiler) | Hard floor: ≤ 1 % regression on every tracked benchmark. |

**v1 total ≈ 24–26 weeks, ~10–11 KLOC** (vs 48–50 weeks, ~16 KLOC
in the 00–07 scoping). The cut is real but the irreducible core —
IR, type lattice, codegen, runtime integration — is unchanged; the
savings come from S1–S5, not from optimism.

**Gate G3 — the branchy-corpus experiment (1–2 weeks, anytime after
P1, MVP methodology).** Hand-write cold-arm pruning + guard
elimination + clause-dispatch specialisation for one hot
gen_server-shaped dispatch function (the way `emit_t2_total_2` was
hand-written), measure against T1. This is the experiment that
decides whether the *expected* branchy-code wins of `00` §1 are
real. Branch-frequency counters, monomorphic-target slots, general
inlining with framestates, and the full CP/stack-scan lifecycle are
green-lit **only if G3 shows the win** — that's most of the deferred
~25 weeks, spent only once it's bought evidence.

### 6.1 The benchmark corpus, and what v1 honestly does to it

The optimization target is the **application corpus**: RabbitMQ,
MongooseIM, Phoenix/Ash-style web services. The tracked suite:

- **`../awfy`** — the 14 classic AWFY benchmarks in both Erlang
  (`apps/awfy/src/awfy_*.erl`) and Elixir
  (`apps/awfy/lib/awfy/benchmarks/`), plus the OTP-benchmark
  families (`apps/otp_benchmarks/`, incl. vendored estone, maps,
  base64, binary_match, ets).
- **OTP in-tree benches** (`HOWTO/BENCHMARKS.md`, `ts:benchmarks()`)
  — `emulator_bench`, `stdlib_bench`, and the protocol-shaped
  `ssl`/`ssh`/`inets`/`megaco` specs.
- **Macro leg** — MongooseIM under Amoc load: a designed
  first-class awfy leg (`awfy/PLAN/MONGOOSEIM_BENCH_PLAN.md` —
  pinned MongooseIM broker, upstream amoc-arsenal-xmpp scenarios,
  local + AWS topologies, throughput + p99 reporting;
  `Dockerfile.mongoose`/`Dockerfile.amoc` already in the repo).
  That plan's own workload description — "scheduler under
  message-pass load, ETS contention from real session stores,
  binary handling in XML, gen_server hot-paths, TLS CPU cost" —
  is precisely the application-corpus mix T2 must be honest
  about. P0's cycle-weighted profiling (below) rides this same
  rig: perf the broker under Amoc load.

Every tracked benchmark is **labelled by dominant op class** so
wins and regressions are attributable: int/list/tuple loops |
floats | binaries | maps | processes/ETS/runtime | mixed. Honest
per-class expectations for v1 as scoped:

| Class | Benchmarks (examples) | v1 expectation |
|-------|----------------------|----------------|
| int/list/tuple loops | Bounce, List, Towers, Permute, Queens, Sieve; estone list/arith micros | The validated class — 1.5–2× plausible |
| floats | Mandelbrot, NBody | **No change** (floats are Phase E; out of v1) |
| binaries | base64, binary_match, unicode, JSON parsing; ssl/ssh/megaco | No change from T2 (BIF/`bs_*` dominated) |
| maps / polymorphic | Richards, DeltaBlue, Havlak, CD, Json model; Elixir structs | No change (maps out of v1; dispatch gated on G3) |
| processes/ETS/runtime | estone msgp, ets suite, mnesia_tpcb | No change **by design** (`07` §19) |
| application macro | MongooseIM/amoc | Low single digits from list/tuple fragments + leaf functions |

The conclusion this table forces: **for the application corpus, the
two coverage classes that matter most after v1 are binaries
(protocol parsing/construction) and maps (Elixir structs,
mongoose_acc-style accumulators)** — currently parked at Phase D
and Phase 5 of `07` §17, i.e. last. That ordering was inherited
from implementation convenience, not from corpus value. It changes:

**P0 corpus measurement becomes cycle-weighted dynamic profiling.**
Profile RabbitMQ, MongooseIM-under-amoc, and a Phoenix app under
load (perf on Linux ARM64); bucket samples into: v1-class ops |
binary ops | map ops | floats | runtime/BIF/NIF/GC (unreachable
for T2 by design). This replaces the static op-counting audit and
*decides the expansion order with data* — including the honest
ceiling: the runtime/NIF bucket bounds what any JIT tier can do
for these apps.

**Gate G-bin — binary-matching loop experiment (MVP methodology,
after G2).** Hand-write a T2-shaped specialisation of one real
protocol-parser loop (an AMQP frame decoder or XMPP tokenizer
shape: tail-recursive loop carrying a match context, `bs_get_*`
ops fused, per-op match-context dance eliminated). Binary parsing
loops are *loop-shaped* — the match context is ordinary
loop-carried state in the argument vector, so re-call deopt (S2)
and the whole loop-tier architecture apply unchanged; what's
missing is only `bs_*` op coverage. If G-bin shows the win,
Phase-D-style coverage is pulled forward ahead of general
inlining.

**Gate G-map — map-region experiment (MVP methodology, after
G2).** Hand-write region-level shape specialisation
(`02` §7.6's design) for one hot map-access chain (Elixir struct
update pipeline or mongoose_acc fold): one shape guard at region
entry, direct offset loads after. If it shows the win, Phase-5
map coverage is pulled forward.

G-bin and G-map are each 1–2 weeks, ordered against G3 by the P0
profile's bucket sizes. The expansion sequence after v1 is thus
bought with ~3–6 weeks of experiments instead of committed blind.

One Elixir-specific honesty note: `Enum.map(list, fun)` delegates
to `:lists.map/2` through a wrapper where the fun is a *parameter*,
so v1's literal-fun intrinsics do not fire through Enum pipelines
(that's the deferred cross-module/H11 case). Hand-written
recursion and Erlang-style direct `lists:*` calls with literal
funs — pervasive in RabbitMQ and MongooseIM — do fire.

## 7. The road back to the full design

| Deferred component | Designed in | Unlocked by |
|--------------------|-------------|-------------|
| Framestates + eager-CP-push (general inlining) | `03` §9.2, `01` §6.5 | G3 pass |
| Lazy stack scan, tombstone CP tables | `06` §5.3–5.5, `05` §14.2 | General inlining (CPs into blobs) |
| Eager own-stack scan in the trace path (self-enable with CPs into blobs) | §4 Case B above | General inlining (CPs into blobs) |
| Branch-frequency counters, cold-arm pruning | `02` §7.7, `04` §10.3 | G3 pass |
| Monomorphic-target slots + cross-module inlining | `02` §7.5, `04` §10.1 | G3 pass + per-function watchpoints |
| `speculate_range` + range profiling | `03` §9.3–9.4 | Measured LICM-hoistable win flag checks can't capture |
| Map-shape feedback + region shape specialisation | `02` §7.6 | **G-map** pass (§6.1); priority vs G3/G-bin set by the P0 profile |
| Polymorphic PIC, speculative funs | `03` §9.6, `04` §10.2 | Phase 6 |
| Binary (`bs_*`) coverage in recovered loops | `07` §17 Phase 7 | **G-bin** pass (§6.1); priority vs G3/G-map set by the P0 profile |
| Messages / NIFs / floats | `07` §17 Phase 8 | Phase coverage, post-v1 |
| Per-instruction T1 PC table | `03` §9.1 (corrected, §3 above) | General mid-function deopt |
| Erlang JIT-server process | `05` §15.3 | Scheduling policy outgrowing a C queue |
| SSA chunk in BEAM file | `02` §7.8 | G1 failure only |

## 8. Open questions

- **`beam_ranges` extension shape.** Separate range class for T2
  blobs vs. folding into the module ranges; interaction with purge
  ordering. Decide in P0 alongside the inspection matrix.
- **Retained-code-chunk accounting.** Which `erlang:memory/0` key;
  whether retention is dropped for modules whose eligible functions
  all got blacklisted.
- **G1 pass criteria.** "Structurally compare" needs a concrete
  definition — proposal: identical CFG shape, identical live-range
  count ±ε, and identity-emit output passing 16A.1 on the corpus.
- **Fun purity for intrinsics.** The intrinsic inliner must verify
  the literal fun's body is effect-free (§2) before recovering the
  helper loop; otherwise fall back to a plain `call_ext`. The check
  is a walk of the fun's SSA — define "effect-free" as the same set
  S2 uses.
- **Demote-on-return interaction with `save_calls`.** A T1
  continuation CP means the *return* side of `save_calls` accounting
  is unchanged; confirm in the inspection matrix that call-side
  accounting (which happens in the callee dispatch) is also
  tier-invariant. Expected yes via export indirection.
