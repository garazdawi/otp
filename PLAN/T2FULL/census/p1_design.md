# P1 — general caller-driven call-site specialization

The strategic lever from `opt_landscape.md`, now scoped. P1 replaces the
hand-coded fold recognizers (maps:fold Stage 1, lists foldl-class) with ONE
uniform mechanism: at a hot call site to a cross-module loop function with a
statically-known fun, inline the callee's loop into the caller, specialize it to
the caller's fun + profiled types, and deopt to the original call. Generalizes
Stage 1 (which is exactly this for one hand-written case). Purge/trace safety
deferred (but the dep hook is specified, since P1 is inherently cross-module).

## The settled decision (why caller, not callee)

**Specialize at the CALLER, as close to the call site as possible.** Callee-clone
keyed by the fun is ruled out as the foundation: a universally-used loop like
`lists:map/2` would accumulate one clone per distinct-fun-ever-seen (unbounded),
and its entry would become a runtime search over those clones — a megamorphic
dispatch on the hot path, centralized at one shared function. It also throws away
information: the fun is a compile-time constant at the caller, and callee-clone
discards that to reconstruct it at runtime.

Caller-inline keeps the information (fun known statically → no dispatch,
monomorphic by construction), and its duplication is benign: one clone per *hot*
call site (cold sites stay generic, inlining budget caps it), cost is
distributed compile-time code size, no runtime dispatch. All 11 survey callers
pass the fun as a literal `MakeFun` (Stage 1's `resolve_copies` already reaches it
through copies/hoisting), so the common case needs no fun guard at all.

**Escalation ladder (all caller-side):**
1. Fun literal at a hot site → monomorphic caller-inline, no fun guard. (Default; ~all real idioms.)
2. Fun is a variable / lightly-polymorphic site (e.g. `mymap(F,L)->lists:map(F,L)`)
   → a small **caller-site PIC**: guard the top 1–2 observed fun code pointers,
   inline those, bounded by *that site's* polymorphism.
3. Megamorphic site → stay generic (the un-inlined call).

The generic un-inlined call is the shared fallback at every rung — exactly Stage
1's slow edge, which already scales to N specializations with one fallback.
Callee-clone survives only as a narrow later optimization (same fold + same fun
hot across many callers), never the foundation.

## Survey evidence (why P1 at all)
`survey_results.md`: the recognizer approach is narrow and fragile even on Erlang
(tail-`lists:foldl` blind spot; maps:fold is flatmap-only), and Elixir
`Enum.reduce` / Gleam `list.fold` get nothing and REGRESS 3–20% under forced T2
(closures install and pay guard overhead with no inlining). One general primitive
covers tail and body, lists and maps and user recursion, and the polyglot idioms.

## Mechanism (grounded in existing machinery)

1. **Trigger (general, replaces the mfa allowlist).** A hot `CallExt`/`TailCallExt`
   to a cross-module `M:F/A` where (a) some argument resolves (via `resolve_copies`)
   to a statically-known fun (literal `MakeFun`, or a guarded code pointer at rung
   2), and (b) `M:F/A` is *loop-shaped* — self-recursive, recoverable by
   `t2_loop_recover`. This structural test subsumes the maps:fold/lists:foldl
   special cases.
2. **Get the callee's loop.** Cross-module retained access (the path the debug
   BIFs use: `erts_get_module(M,ix)->curr.t2_retained` → `t2_build_for_debug`)
   builds the callee's HIR; run `t2_loop_recover` on it to expose the loop with
   its back edge and loop-carried state. Requires the callee module loaded with
   retention (`T2_RETAIN=1`); if absent, abandon the site (stay generic).
3. **Inline the recovered loop into the caller** — the core new capability, a
   generalization of `t2_inline_leaf` (single-block, non-looping) to a
   multi-block *looping* callee. Clone the callee's blocks into the caller, rename
   values, bind caller args → callee params and the caller's literal fun → the
   callee's fun param, and redirect the callee's `Return` to the caller's
   continuation (the post-call join for a body site; the caller's own return for a
   tail site — same split Stage 1 does).
4. **Devirtualize the per-element call.** Wherever the inlined loop calls the fun
   param (`call_fun`/apply), splice the fun body in place (`splice_fun`, as Stage
   1 / the leaf inliner already do). This removes the per-element indirect call —
   the dominant win the survey shows is missing today.
5. **Specialize + clean up.** `t2_speculate` narrows element/acc arithmetic to
   unboxed small ops with guards; the Stage-3 `t2_opt` suite then reaps the dead
   loads, copies, and CSE the inlining exposes (the survey's E2b shows this suite
   is load-bearing: 1.02x→2.30x).

## Deopt / fallback — two models by loop bound

The shared fallback is always the **original call, verbatim** (Stage 1 pattern),
but the deopt *target* differs by whether the loop is bounded:

- **Bounded loop (flatmap ≤32, Stage 1): restart.** Side-exit re-executes the
  whole call from the entry state (`T2_OP_SPEC_CALLSITE`/`ENTRY`). Redo is ≤32
  elements; the one-time double reduction charge is the documented deviation.
- **Unbounded loop (list fold): re-dispatch, NOT restart.** Restarting a list
  fold at element k would redo 0..k and re-charge k reductions (up to 2x work).
  Instead, deopt **re-invokes the generic callee with the loop-carried state** —
  a tail call to `M:F/A(remaining_tail, acc_k, fun)`. The list-walk loop already
  carries `remaining_tail` and `acc_k` as loop-carried values, so the deopt sync
  map is exactly `{remaining_tail, acc_k, fun}` and T1 continues from k with no
  redo and no over-charge. This is a new deopt shape ("re-dispatch to generic with
  loop-carried state") and is the correct model for any unbounded inlined loop.

Type-guard misses take the deopt; **timeslice yields stay in T2** (the recovered
loop's existing back-edge yield/resume machinery resumes into the inlined loop in
the caller's blob) — so yield does not deopt.

## Reductions & yield
Bounded loops keep Stage 1's `FoldBudget` batch. Unbounded loops inherit the
recovered loop's per-iteration reduction charge + back-edge yield/resume (T2-Full
P2 machinery) when spliced in — no new accounting needed; correctness is
"reductions == the generic callee would charge," asserted per example.

## Purge/trace (deferred depth, hook specified)
Inlining a cross-module callee bakes its code into the caller's blob, so the blob
must die if the callee module is purged/reloaded. `dep_hdrs` already records such
deps (Stage 1 records the own-module dep); P1 records the **callee module's**
code header. Full purge-safety (the dependency-registry invalidation) is deferred
per the exploration scope, but the dep must be recorded from day one so the hole
is closeable, not silent.

## Staged decomposition

- **P1a — the primitive.** General caller-inline of a self-recursive list fold
  with a literal fun, with the unbounded re-dispatch deopt. Demonstrators (both
  get ~1.0x today): the **Gleam `list.fold` caller** (`gleam@list:fold/3` is a
  clean self-recursive list fold) and the **tail `lists:foldl` caller** (also
  fixes the recognizer blind spot as a side effect — the general trigger sees the
  tail call). Gate: Gleam/Erlang list-fold callers reach parity with the non-tail
  `lists:foldl` win (2.3–3.8x), byte-identical results + reductions, deopt via
  re-dispatch verified (a non-small element mid-list continues on T1 correctly).
- **P1b — caller-site PIC** for pass-through funs (rung 2): guard the top
  observed fun code pointer, inline it, deopt to generic on miss.
- **P1c — more callee shapes**: foldr, map/filter (build a list — allocation, ties
  into escape analysis later), and arbitrary user self-recursion; Elixir
  `Enum.reduce` (its list clause is self-recursive — should fall out of P1a once
  the protocol/`{:cont,acc}` wrapper is seen through).

## Risks / open questions
- **Inlining a recovered loop** is the hard new piece (vs. leaf-inline's single
  block); block-clone + value-rename + param-bind + return-redirect must leave a
  `t2_validate`/`t2_validate_windows`-legal blob (degrade to T1 on any failure).
- **Callee eligibility/retention**: the callee must be T2-eligible and retained;
  many stdlib fold fns aren't installed today (survey). P1a needs them retained.
- **Re-dispatch deopt** materializing `{remaining_tail, acc, fun}` at an arbitrary
  loop point — verify the sync map is exact and the tail call is reduction-neutral.
- **Nested/transitive inlining** (`mymap` → `lists:map`): P1a stops at one level;
  transitive is future.

## Correctness + measurement gates
Re-run the survey harness (scratchpad/survey/) after each stage: every caller
byte-identical across T1 / P1-off / P1-on, reductions identical, and the deopt
paths (non-small element, huge list that yields) verified. Headline target: the
Elixir/Gleam/tail-Erlang idioms that get 0.8–1.0x today reach the 2–3x the
hand-coded shapes get — with no recognizer for their specific module/function.
