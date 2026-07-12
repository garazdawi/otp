# P1c-1 — transitive wrapper inlining (unlock the polyglot fold idioms)

Builds on P1a (`p1_design.md`, committed `8838b13f3b`). P1a inlines a callee that
is *directly* self-recursive (Gleam `gleam@list:fold/3`). The idiomatic Erlang and
Elixir folds are NOT directly self-recursive — they are thin wrappers that
tail-call the real loop. This increment adds **transitive wrapper inlining**: one
capability that lights up both, generalizing what the old hand-coded lists
intrinsic special-cased (`{IClass::Foldl,"foldl","foldl_1",3,2}`).

## The grounded structures (confirmed in-tree)

- **Gleam** `gleam@list:fold/3` — directly self-recursive. P1a already: 1.0x→~3.1x.
- **Erlang** `lists:foldl/3` is a **depth-1 wrapper**:
  ```
  foldl(F, Accu, List) when is_function(F, 2) ->
      case List of [Hd|Tail] -> foldl_1(F, F(Hd,Accu), Tail); [] -> Accu end.
  foldl_1(F, Accu, [Hd|Tail]) -> foldl_1(F, F(Hd,Accu), Tail);   %% the loop
  foldl_1(_F, Accu, []) -> Accu.
  ```
  `foldl/3` guards `is_function`, peels the first element, then tail-calls the
  self-recursive `foldl_1/3`. Tail `lists:foldl` gets 1.0x today (P1a rejects
  `foldl/3` — not self-recursive).
- **Elixir** `Enum.reduce/3` is a **depth-2 wrapper**:
  `reduce(l,acc,fun) when is_list(l) -> :lists.foldl(fun, acc, l).` → `lists:foldl`
  → `foldl_1`. Gets ~0.97x (regresses) today.

## Mechanism: make the P1 trigger transitive

At a hot `TailCallExt` to `M:F/A` with a literal env-free `MakeFun` argument
(the P1a entry condition), build the callee HIR and classify:

1. **Self-recursive loop** → apply P1a (inline the recovered loop, devirtualize
   the per-element `call_fun`). Terminal.
2. **Thin wrapper** — small, non-loop, and every returning path ends in a
   `TailCallExt` to some `M':F'/A'` that passes the fun through (as a literal
   argument), possibly behind admissible guards (`is_function/2` on the literal
   fun = statically true; a `cons`/`[]` case; an `is_list` type test) → **inline
   the wrapper body into the caller** (its guards become caller-side
   speculation/deopt, its peeled work becomes straight-line ops), which replaces
   the original call with the wrapper's `M':F'/A'(… , fun)` tail-call now carrying
   the literal fun. **Re-run the trigger on that exposed tail-call.**
3. **Neither** (or wrapper too large / non-passthrough / unadmissible guard) →
   abandon, stay generic.

Bounded transitive depth (e.g. 3) with a visited set (no cycles). Terminates at a
loop (rung 1) or gives up. Erlang folds terminate at depth 1 (`foldl`→`foldl_1`),
Elixir at depth 2 (`Enum.reduce`→`lists:foldl`→`foldl_1`).

## Guards, deopt, reductions across the chain

- **Guards.** `is_function(F,2)` on a literal arity-2 `MakeFun` is a compile-time
  truth — no runtime guard. The `is_list`/cons/`[]` structure of the wrapper folds
  into the loop the same way P1a already handles the recovered loop's own
  cons-match; the wrapper's peel-first (`foldl/3`) is just the loop's first
  iteration. Any guard that is NOT statically dischargeable (e.g. `is_list` on a
  value that could be an improper list or non-list) becomes a speculation guard
  that deopts to the generic original call.
- **Deopt = the ORIGINAL call, unchanged.** Every fast-path side exit still
  re-dispatches to the *outermost* original `M:F/A(remaining_tail, acc_k, fun)`
  (P1a's `T2_OP_SPEC_REDISPATCH`). Sound because `Enum.reduce(remaining, acc, fun)`
  and `lists:foldl(remaining, acc, fun)` are both the correct continuation from
  element k. The re-dispatch re-charges the wrapper chain's fixed entry
  reductions (a small constant: +1 per wrapper level, like P1a's +1) — documented
  deviation, term-result-neutral.
- **Reductions.** The inlined fast path must charge what the whole generic chain
  charges (Enum.reduce entry + lists:foldl wrapper + foldl_1 per element).
  Measure the generic chain's `process_info(reductions)` at two list sizes, solve
  a+b·n, and assert identity per demonstrator (as Stage 1 did).
- **Deps.** Transitive inlining bakes EVERY module in the chain into the caller
  blob — record each (`Enum`, `lists`, own) code header in `dep_hdrs` (purge hook;
  registry invalidation still deferred).

## Reuse / new
Reuse: P1a's cross-module HIR access (`t2_build_for_debug`), recovered-loop clone,
`splice_fun`, re-dispatch deopt, `resolve_copies`, the CFG split. New: the wrapper
classifier + the transitive re-trigger loop + inlining a small **non-loop**
multi-block callee whose returning paths are a fun-passing tail-call (a cross-
module generalization of `t2_inline_leaf`, without a back edge).

## Demonstrators & gates (this increment)
- **Must-have: Erlang tail `lists:foldl`** (survey E1 `foldl_sum`, E2 `foldl_count`)
  — depth 1. Target: the 2.3–3.8x the non-tail control (E1b/E2b) already gets.
- **Stretch: Elixir `Enum.reduce`** (survey X1 sum, X2 count) — depth 2. Target:
  from ~0.97x (regress) to ~2–3x. If depth-2 proves too hard in one pass, land
  depth-1 (Erlang) and report the depth-2 blocker precisely.
- Gates (per the P1a pattern): survey harness byte-identical results + reductions
  across T1 / P1-off / P1-on; re-dispatch deopt verified (non-small element
  mid-list continues on T1, no restart); maps:fold + Gleam P1a unregressed;
  `-Werror=sign-compare` clean; install parity (no new failures); **do not commit**.

## Explicitly deferred (later P1c increments)
Body (non-tail) sites (the clone/join path P1a left wired-but-unenabled); `foldr`
(non-tail recursion, combine-on-return); `map`/`filter` (allocate a list — ties
into escape analysis); the `call_fun` eligibility tightening (P1a wart).
