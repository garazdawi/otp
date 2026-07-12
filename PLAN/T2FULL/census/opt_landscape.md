# T2 optimization landscape — what to build, and what the compiler already did

Question (exploration): beyond Stage 3's standard suite, what common JIT
optimizations should T2 implement? Answer framed against (a) what the BEAM
compiler already does statically, and (b) what production method/trace JITs
center on.

## The framing that decides everything

The BEAM compiler's SSA optimizer (`beam_ssa_opt` + friends) is already a
thorough *static* optimizer. Confirmed pass inventory in this tree:

- Classical scalar: `ssa_opt_cse`, `ssa_opt_dead`, core constant folding
  (`sys_core_fold`), copy propagation, `ssa_opt_live`, `ssa_opt_redundant_br`,
  `ssa_opt_merge_blocks`, `ssa_opt_trim_unreachable`, `ssa_opt_coalesce_phis`,
  `ssa_opt_tail_phis`.
- Type + range: `beam_ssa_type` (`ssa_opt_type_*` — type inference/narrowing,
  redundant type-test removal), `ssa_opt_ranges` (integer range analysis).
- Unboxing/specialization: `ssa_opt_float` (float → FP registers),
  `ssa_opt_element`/`get_tuple_element`/`record`/`tuple_size`, `ssa_opt_sw`,
  `ssa_opt_is_between`.
- Motion: `ssa_opt_sink` (code sinking), `ssa_opt_linearize`.
- Escape/alias: `beam_ssa_alias` + `beam_ssa_ss` — alias/sharing analysis, but
  narrowly to enable **in-place destructive updates** (`update_tuple`,
  `private_append`), NOT general allocation elimination.
- Binary/message: `ssa_opt_bsm`, `ssa_opt_bs_create_bin`, `ssa_opt_bs_ensure`,
  `beam_ssa_recv`.

**Consequence:** T2 must NOT position itself as "add the missing classical
opts." Re-running CSE/DCE/const-fold/copy-prop is justified only as
*post-inline/post-speculation cleanup* — inlining and speculation manufacture
fresh opportunities the static compiler never saw (a cross-module callee's body
meeting the caller's constants; a speculated type unlocking a guard). That is
Stage 3. Its value is real but bounded: it recovers what inlining leaves on the
floor; it does not create new headroom.

The headroom is in the opts the static compiler **cannot** do, because it lacks
(1) runtime type/value/target profiles and (2) the ability to cross a module
boundary at all (hot code loading forbids it). That is the whole reason a
profiling tier-2 exists. Invest there.

## Runtime-only opts, prioritized (the actual roadmap)

### P1 — General monomorphic call-site speculation + inline (devirtualization / inline caching)
The hand-written `maps:fold` (Stage 1) and lists intrinsics are one-off
instances of this. The general form: at a hot `call_ext` / `apply` / fun-call
site, if the profile shows one dominant target, guard on it, inline the
target's body, and deopt to the real dispatch on a miss. This is the single
most important optimization in every production method JIT (HotSpot C2 +
CHA, V8 TurboFan inline caches, Graal, SpiderMonkey). For BEAM it is uniquely
powerful because **the static compiler cannot inline across modules at all**.
Everything else compounds on top of it — you cannot LICM/CSE/unbox across a
call you did not inline. Turns "hand-code each hot library caller" into "any
hot monomorphic caller." Directly generalizes Stage 1. **Biggest lever.**

### P2 — Tag elimination / unboxing under speculation, across loop bodies
In the fold loop we optimized, the accumulator and induction variable are
tagged smalls: every iteration untags, adds, re-tags, and re-checks overflow.
The compiler unboxes floats (`ssa_opt_float`) but keeps integers tagged — it
cannot prove smallness without a guard and will not insert speculative guards.
T2 can: keep the speculated-small acc/IV as raw machine words in registers
across the whole loop, tagging only at loop exit and deopt boundaries. Removes
a tag+untag every iteration and unlocks P3. This is the LuaJIT/PyPy
"keep it unboxed in the trace" trick. High per-iteration leverage on exactly
the examples in scope.

### P3 — Redundant-guard elimination + range-based overflow-guard removal
Speculation inserts type/overflow guards; many are redundant once a value is
known-small, or once a runtime-informed range proves no overflow. Summing
n≤32 values each in a bounded range cannot overflow a 60-bit small for a long
time, so the per-iteration accumulator overflow guard can be coarsened (checked
once against a conservative bound, or once per k iterations) instead of every
add. The compiler has static range analysis (`ssa_opt_ranges`) but no
speculation to feed it tighter runtime facts. Combines with P2.

### P4 — Loop unrolling for runtime-bounded loops
The flatmap fold is bounded by `MAP_SMALL_MAP_LIMIT` (32). Small,
runtime-known-bounded loops are the classic unroll target (every trace JIT
unrolls). Unrolling amortizes loop overhead (cmp/branch/IV-increment) and —
with P2/P3 — lets several iterations share one guard and exposes cross-iteration
CSE/scheduling. Pairs with P5.

### P5 — Induction-variable strength reduction / pointer walk  *(already planned as Stage 3e)*
Replace `base + (i>>4)<<3` per load with a pointer that `+= 8`; retire the
index and its untag. Classic; on the roadmap.

## Tier 3 — bigger, harder, later

- **Escape analysis → allocation sinking / scalar replacement in loops.**
  make_fun-sink (Stage 3d) is a one-instance special case. The general form —
  do not heap-allocate a tuple/cons/map/fun that does not escape its iteration
  — is a major lever for alloc-heavy Erlang/Elixir loops (intermediate tuples
  built then immediately deconstructed). The compiler's alias analysis only
  enables destructive updates, not elision. Hard (GC-safety), high ceiling.
- **Deopt-gated exception/effect elision** — assume the hot no-throw path; ties
  into the purge/trace machinery deliberately deferred for this exploration.
- **Instruction scheduling / block layout / asm peephole** — minor on
  out-of-order aarch64; low priority.

## Explicitly NOT worth T2 doing
The compiler already nails these statically and inlining rarely manufactures
fresh instances: float unboxing, binary match/construction opts (also a T2
frontend blocker class), destructive tuple update, switch/select lowering,
general block merging beyond cleanup.

## Bottom line
Stage 3 (cleanup) is table stakes. The real program is **P1 (general
call-site speculation + inline)** as the spine, with **P2/P3 (loop
unboxing + guard coarsening)** and **P4 (unrolling)** as the per-iteration
multipliers that make each inlined hot loop approach hand-written C. That is
the ordering that matches where production JITs put their effort, and it is
precisely the set the static BEAM compiler structurally cannot provide.
