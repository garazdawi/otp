# Deopt by re-execution vs. precise state reconstruction — survey

Written to answer a specific challenge to the v1 loop-tier design
(`../T2/08_v1_loop_tier.md` §S2, "re-call-only deopt"): pre-BeamAsm
JIT experiments at OTP found that (a) too many JIT-worthy functions
contain side effects (a `gen_server:cast` in the loop body), and
(b) the cost of occasional re-execution ate the JIT's gains. Does
any production JIT use re-execution-style deopt, and what were the
documented pros/cons?

## The short answer

Re-execution is **mainstream — but only at granularities where no
observable effect can be repeated**. Every production JIT surveyed
obeys the same legality rule T2's S2 adopts (guards fire before
effects; exits re-execute only effect-free work). Where they differ
is what they do when an operation *does* have effects:

| System | Re-executes on exit? | Granularity | Effects handled by |
|--------|---------------------|-------------|--------------------|
| HotSpot (C1/C2) | **Yes** — `Unpack_reexecute` / the reexecute bit in debug info | single bytecode | re-execution allowed only for bytecodes that haven't committed effects; otherwise resume *after* the bytecode |
| JavaScriptCore | **Yes** — OSR exit re-enters Baseline/LLInt at the current bytecode | single bytecode | checks emitted before the op's effects; "OSR check failure does not affect semantics" is the stated invariant |
| HHVM gen-1 | effectively — guard failure re-enters at tracelet entry | **tracelet (multi-op region)** | tracelets end before effects; guards hoisted to entry |
| V8 / Maglev / TurboFan | No — precise framestates | per-op | *lazy deopt*: checkpoint after each effectful op; resume after the effect |
| LuaJIT | No — snapshot restore | per-guard (sparse) | snapshots constrained by stores; precise restore |
| PyPy | No — compressed resume data | per-guard | precise reconstruction (virtualizables) |
| Graal/Truffle | No — FrameStates | per-op | new FrameState materialised after each effect |
| Transmeta CMS | **Yes — region-level, effects included** | translation region | **hardware**: gated store buffer + shadowed registers; rollback undoes effects |

Two structural observations fall out:

1. **Nobody re-executes effects in software.** The one system that
   re-executed regions *containing* stores — Transmeta's Code
   Morphing Software — needed silicon (a gated store buffer with
   commit/rollback) to make it legal. Everyone else either ends the
   speculative span at the effect (HHVM tracelets, HotSpot/JSC
   per-bytecode discipline) or pays for precise per-op resume
   metadata (V8/LuaJIT/PyPy/Graal).
2. **Re-execution per se is not the controversial part** — HotSpot
   and JSC, the two most battle-tested method JITs in existence, use
   it as the *default* exit semantics. The engineering content is
   entirely in the legality rule and in keeping exits rare.

## The systems in more detail

### HotSpot: the reexecute bit

Deopt metadata carries a per-frame "reexecute" flag;
`Unpack_reexecute` re-runs the current bytecode in the interpreter,
`Unpack_deopt` resumes after it. The compiler chooses per bytecode:
re-execution is used exactly when the bytecode's observable effects
have not yet been committed at the trap point (allocation-profiles,
uncommon traps on type checks); bytecodes whose effects already
happened resume past the bytecode instead. I.e. HotSpot implements
*both* arms and selects by effect status — the same dichotomy as
T2's "re-execute the window" vs "commit and start a new window".

### JavaScriptCore: bytecode-boundary exits, and the cost model

Pizlo's "Speculation in JavaScriptCore" is the most explicit public
treatment:

- Exit targets are bytecode instruction boundaries in the profiling
  tier: "the exit destinations can just be bytecode instruction
  boundaries."
- The legality invariant: "OSR check failure does not affect
  semantics (we exit to the same point in the same code, just with
  fewer optimizations)." Checks are placed so no effect is
  re-observed; the ToPrimitive/ToString case (two operations, one
  with effects, lowered from one bytecode) is their canonical
  example of how violating this produces a double-executed effect —
  precisely the bug class the pre-BeamAsm experiments hit.
- The cost model: an OSR exit costs ~2.5 µs from DFG and ~10 µs
  from FTL, so speculation only pays when the success probability
  is "p ~ 1". **Crucially, that cost is dominated by re-entering a
  profiling interpreter tier and shuffling state into its frame
  format.** T2 has no interpreter: a side-exit is a branch into
  native T1 with X registers already in T1 layout (sync-point
  invariant). The MVP measured the all-exits worst case at ≈ pure
  T1 cost (`../mvp/OUTCOME.md` §"Side-exit cost") — three orders of
  magnitude below JSC's exit cost, because there is no tier-format
  impedance.

### HHVM: the closest relative of re-call

Gen-1 HHVM compiled *tracelets* — multi-instruction regions with
all type guards hoisted to region entry, regions broken at points
where speculation couldn't continue. Guard failure re-entered at a
boundary and selected/compiled a different tracelet version. The
documented cons (Facebook's own retrospectives + the OOPSLA'14 /
PLDI'18 papers): *too-small regions* — repeated entry guards on
values that hadn't changed, and state shuffle between memory and
registers at every boundary. Gen-2 fixed it with profile-guided
*larger* regions, keeping the same entry-guard philosophy.

Read against T2: the tracelet failure mode is region-boundary
overhead, not re-execution legality. T2's regions are
function/loop-sized (bigger than tracelets), its boundary state is
the T1 X/Y layout that values already live in (no shuffle by
construction), and entry guards hoist to the loop preheader so they
run once per invocation, not per region entry.

### The in-house history (BEAM/C, HiPE, BEAMJIT)

"The Road to the JIT" documents why each pre-BeamAsm attempt died,
and the killers were consistent: HiPE — "a small overhead of
switching from native code to interpreted BEAM and vice versa" plus
the difficulty of choosing what to compile; BEAMJIT (tracing) —
tracing "without lowering the base speed" proved difficult, "it was
also difficult to design the mechanism for context switching
between the interpreted code and native code in a way that didn't
lower the base speed", and hot traces sometimes never re-executed.
BeamAsm's founding decision was to eliminate the mode switch
entirely by compiling everything.

This matters for weighing the old re-call experience: in a
pre-BeamAsm JIT, "occasional re-call" meant **falling back into the
interpreter** — paying the state-mapping and mode-switch cost that
those projects independently identified as their primary tax. T2's
re-call lands in BeamAsm-native T1 under an identical calling
convention and register layout; the mode-switch term that ate the
gains is structurally absent. The MVP's worst-case measurement
(every iteration exits; total cost ≈ what pure T1 pays for the same
work) is the direct evidence on this architecture.

## What this survey says about S2

1. **The legality rule is settled industry practice.** Re-execute
   only spans in which no effect has been committed; never
   re-execute an effect in software. S2's guards-before-effects is
   the same rule HotSpot and JSC enforce per bytecode, applied at
   window granularity.
2. **"Effect ⇒ don't compile" is the wrong reading of that rule.**
   The mature systems treat an effect as a *boundary*, not a
   disqualifier: HotSpot resumes after the effectful bytecode, V8
   places a lazy-deopt checkpoint after it, HHVM starts the next
   tracelet. The equivalent for T2 — effects end a re-execution
   window and a new window begins after them — requires only that
   the post-effect instruction boundary be a legal exit target
   (one more entry kind in the T1 PC side table), because effectful
   ops are already sync points with X/Y in T1 layout. This is the
   amendment adopted into `08` §S2.
3. **Exit frequency, not exit cost, is the live risk.** JSC's
   "p ~ 1" lesson transfers even though their exit-cost constant
   doesn't: a speculation that fails with non-trivial probability
   inside a loop is a net loss at any exit cost. T2's existing
   exit-counter + jettison/backoff policy (`03` §9.5, in-loop
   weighting) is the enforcement mechanism; the floor is always T1.
4. **The scope question — how much hot Erlang code has effect-free
   or BIF-effect-only loop bodies — is empirical and measurable
   without building anything.** Hence the P0 *effect-shape census*
   added to `08` §6: walk the SSA of a corpus (dialyzer, RabbitMQ,
   MongooseIM, Phoenix/Elixir deps), find self-tail-recursive loop
   bodies, bucket them: effect-free | BIF-effect-only (send, ets,
   put — window boundaries, fully compilable) | Erlang-call-bearing
   (demote-on-return in v1; converted to BIF-effect shape by
   cross-module inlining post-G3). The pre-BeamAsm objection is a
   claim about this distribution; the census replaces the argument
   with a number.

## Sources

- Pizlo, "Speculation in JavaScriptCore", webkit.org/blog/10308 —
  exit semantics, effects discipline, cost model.
- OpenJDK `src/hotspot/share/runtime/deoptimization.{hpp,cpp}` —
  `Unpack_reexecute`, `Unpack_uncommon_trap` ("redo last bytecode").
- "The HipHop Virtual Machine" (OOPSLA 2014); "HHVM JIT: a
  profile-guided, region-based compiler" (PLDI 2018); Facebook
  engineering retrospective on the gen-2 region JIT redesign.
- Drejhammar & Rasmusson, "BEAMJIT: a just-in-time compiling
  runtime for Erlang" (Erlang Workshop 2014); Drejhammar, "BEAMJIT,
  a Maze of Twisty Little Traces" (Erlang Factory).
- "The Road to the JIT", erlang.org/blog — documented abandonment
  reasons for BEAM/C, HiPE, BEAMJIT.
- Dehnert et al., "The Transmeta Code Morphing Software" (CGO 2003)
  — hardware commit/rollback for region re-execution with effects.
- `../mvp/OUTCOME.md` — measured side-exit cost on this
  architecture (worst case ≈ pure T1).
