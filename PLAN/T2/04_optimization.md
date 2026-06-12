# T2 — Optimization (Inlining + Code Generation)

> **v1 scope rescoped by [`08_v1_loop_tier.md`](08_v1_loop_tier.md)**:
> v1 inlining is local-leaf + `lists:*` intrinsics with literal funs
> only; §10.1's cross-module monomorphic inlining (and §7.5's slot
> that feeds it) is gated on the G3 experiment. §10.5's loop
> recovery applies *first and foremost to the compiled function's
> own self-tail-recursion* — the MVP's validated core win — not just
> to inlined helpers. Unrolling (§10.6) is deferred unless the P3
> corpus shows `test_heap` coalescing wins; LICM-lite (preheader
> guard hoisting) stays.
>
> Part of the T2 design. See [`README.md`](README.md) for the full
> document index. This file covers §§10–11: the inlining strategy
> (what we inline in v1, the higher-order-intrinsic annotation
> evolution, loop recovery and unrolling, DOMJIT-style guard BIFs)
> and the code-generation strategy (direct asmjit emission,
> sync-point-constrained register allocation).

## 10. Inlining strategy

### 10.1 What we inline in v1

- **Local calls** (intra-module): inline if size ≤ threshold.
- **Remote calls to monomorphic targets**: the per-call-site
  monomorphic-target slot (§7.5) records the observed `Export*`
  at runtime. T2's inliner reads the slot at compile time;
  monomorphic + size ≤ threshold + audited-pure-or-semi-pure ⇒
  inline.
- **`jit_inline`-annotated higher-order helpers** when the fun
  argument is a constant-known fun at the call site. Initial set:
  the 10 BIFs in `lib/compiler/src/sys_core_fold_lists.erl`
  (`lists:all/2`, `any/2`, `foreach/2`, `map/2`, `flatmap/2`,
  `filter/2`, `foldl/3`, `foldr/3`, `mapfoldl/3`, `mapfoldr/3`).
- **`call_fun` sites whose target is provably a known fun** via SSA
  constant propagation — e.g. the literal fun threaded into an
  inlined `lists:foldl/3`. No speculation, no deopt machinery
  needed.
- **All guard BIFs as primitive ops** (DOMJIT-style — §10.7).

The `call_fun` slot from §7.5 is **collected** in v1 but
**consumed** in v2 — see §10.2.

### 10.2 What we don't inline in v1

- **Polymorphic call sites** (PIC-style switch). Profile is `POLY`.
- **Speculative fun specialisation** (§9.6).
- **`apply/3` with non-constant arguments**. Can't predict target.
- **Naive recursive expansion**. Self-recursive callees go through
  loop recovery (§10.5), not nested expansion.
- **Calls into modules whose BIFs we haven't audited as pure or
  semi-pure**.

### 10.3 Heuristics

- Size threshold: ≤24 T2 IR ops (**starting value, calibrated in
  Phase 0** against the corpus from F2; the AOT compiler's 24-op
  default is over Core Erlang, which has different node density).
- Depth cap: 3 levels.
- Total expansion cap: 5× original function's IR size.
- Skip inlining if the callee deopt'd in a previous T2 compile.
- Tail-recursive callees → loop recovery, not nested expansion.

### 10.4 Higher-order intrinsics: the annotation evolution

The AOT compiler today has `lib/compiler/src/sys_core_fold_lists.erl`
(~400 lines) with hand-written Core Erlang expansions for the 10
lists higher-order BIFs. T2 reuses the *pattern* and evolves it:

1. **AOT annotates.** A new annotation
   `jit_inline => #{fun_arg_pos => N}` on `b_function.anno` for
   every higher-order helper the AOT knows about. Initial set =
   the existing `sys_core_fold_lists.erl` set. The annotation
   targets the *wrapper* function only (e.g. `lists:foldl/3`,
   `lists:map/2`); recursive helpers don't need their own
   annotation because the inliner discovers them via tail-call
   recovery (§10.5). The annotation is keyed on a single
   parameter position; multi-fun helpers are not in v1.
2. **T2 reads the annotation** at compile time. When an
   annotated function appears in a call site with a constant
   fun in position N, T2 inlines. Funs of non-fixed arity at
   the inlined site are *not* inlined — fall back to a regular
   `call_fun`.
3. **Library authors opt in.** A new `-jit_inline` module
   attribute lets users mark their own helpers (e.g.
   `my_lib:fold_with_logging/3`).
4. **AOT auto-inference** (later). The AOT compiler can detect
   "this function calls `call_fun` on a parameter" and add the
   annotation automatically.

**Implementation: ported, not generated.** T2 hand-translates
`sys_core_fold_lists.erl`'s expansion templates into C++
IR-builder calls. The alternative — invoking `core_to_ssa` from
inside the runtime at JIT-init — adds a runtime dependency on the
Erlang AOT pipeline that we'd rather avoid. Drift between
`sys_core_fold_lists.erl` and the C++ port is caught by the
identity-transform suite (§16A.1): if the AOT inlining and T2
inlining produce different observable behaviour, tests fail.

Benchmarking note: when measuring T2 wins on higher-order-heavy
code, **disable `inline_list_funcs`** in the baseline so we don't
conflate "T2 is great" with "the AOT had this turned off".

### 10.5 Loop recovery and loop info

T2 IR uses the `LoopInfo` analysis style (LLVM/Maglev). Loop
structure is **side data**, not an IR construct:

```cpp
struct LoopInfo {
    Vector<Loop> loops;
};
struct Loop {
    BBLabel header;
    BBLabel preheader;          // synthesised if absent
    Vector<BBLabel> latches;
    Vector<BBLabel> exits;
    Vector<BBLabel> body;
    Loop* parent;
};
```

A `LoopInfoPass` builds dominators, finds back-edges, groups blocks.
Cheap, runs once, fed to LICM and unrolling.

**Loop recovery during inlining.** When the inliner expands a tail-
recursive callee (e.g. the recursive helper inside an inlined
`lists:foldl/3`), it emits a flat loop instead of nested recursive
calls:
- Detect: callee is self-recursive, recursive call is in tail
  position.
- Rename the callee's entry block as a loop header inside the
  caller.
- Replace the recursive tail call with a back-jump that updates
  the formal parameters via phi nodes at the header.

Without loop recovery, inlining `lists:foldl/3` produces N levels
of nested recursive expansion until the depth cap fires — much
worse than a single self-jumping loop. With recovery, the same
input becomes a flat loop on which LICM and unrolling can fire.

### 10.6 Loop unrolling — the primary loop optimization for Erlang

In imperative-language JITs, IV analysis is the high-value loop
optimization (array indexing → `*p++`). Erlang doesn't iterate flat
arrays — it walks lists by pointer-chasing cons cells, with no
addressing computation to strength-reduce. **IV analysis is weak
here.**

What *does* pay off: amortising per-iteration costs across multiple
iterations. The overhead is:

1. **GC test (`test_heap`).** Per-allocation in the iteration body.
   Unrolling K iterations into one body lets us emit a single
   `test_heap(K * iter_alloc_words)` for the whole batch — big saving
   for `lists:map/2`, `lists:filter/2` keeping, list comprehensions,
   binary construction.
2. **Spine traversal.** `[H1|T1] = L; [H2|T2] = T1; …` collapses to
   fewer comparisons, fewer back-edges, better instruction
   scheduling, pipelined cell loads.
3. **Term creation.** Building K cons cells in one body advances
   HTOP once (`add htop, htop, K*16`) instead of K times.
4. **Branch overhead.** One taken back-edge per K elements.

Heuristics:
- Default factor 4; up to 8 for very small bodies.
- Skip for bodies with calls (other than known-pure inlined ones)
  or variable-cost work.
- After unrolling, **re-place `test_heap`** so allocations within
  the unrolled body are covered by a single combined test at the
  unrolled-loop entry. This is the win that justifies most of the
  unrolling work.
- Watch the code-cache budget: factor-4 unrolling roughly
  quadruples body size.

**Branchy iteration bodies** (e.g. `lists:filter/2` keeping some
elements but not others — kept iterations allocate a cons cell,
dropped ones don't). The unified `test_heap` at the unrolled-loop
entry reserves the *worst-case* allocation for K iterations
(K cons cells). HTOP is only advanced when an allocation actually
fires. The reservation pays a slightly higher GC pressure (we
guarded a worst case that may not materialise) for a single
heap-test per K iterations. Acceptable; matches BeamAsm's existing
"`test_heap` covers the worst path" pattern.

LICM is also v1; it pays for hoisting type guards on parameters
that don't change per iteration, hoisting captured-environment
loads from inlined funs, and constant subexpressions the AOT
couldn't see before inlining.

IV analysis is v2.

### 10.7 Guard BIFs as primitive IR ops (DOMJIT-style)

ZJIT and Maglev "open-code" well-known BIFs as IR ops rather than
emitting calls into the runtime. This exposes the operation to type
inference, constant folding, CSE, and elimination when the result
is unused.

**Decision: T2 v1 treats every guard BIF as a primitive IR op.**
The set:

- Type predicates: `is_atom/1`, `is_binary/1`, `is_bitstring/1`,
  `is_boolean/1`, `is_float/1`, `is_function/1`, `is_function/2`,
  `is_integer/1`, `is_list/1`, `is_map/1`, `is_map_key/2`,
  `is_number/1`, `is_pid/1`, `is_port/1`, `is_record/2`,
  `is_record/3`, `is_reference/1`, `is_tuple/1`.
- Size and access: `length/1`, `map_size/1`, `tuple_size/1`,
  `byte_size/1`, `bit_size/1`, `size/1`, `element/2`,
  `binary_part/2`, `binary_part/3`, `hd/1`, `tl/1`, `map_get/2`.
- Numeric: `abs/1`, `round/1`, `trunc/1`, `floor/1`, `ceil/1`.
- Process info: `node/0`, `node/1`, `self/0`.

Each carries result-type metadata (signatures from `beam_call_types.erl`),
a "pure" attribute for CSE and DCE, and a lowering rule that emits
inline machine code (or, for complex cases like `length/1`, an
inline fast-path with a slow-path tail call to the existing BIF
implementation — preserving trap-out semantics, see §12.4).

**IR-level invariant for split fast/slow lowerings.** Ops like
`length/1` lower to a primitive that internally branches between
an inline fast-path (bounded-cost traversal) and a slow tail call
to the BIF (which handles trap-out for long lists). **The fast
and slow paths are inseparable** — neither pass may eliminate the
slow tail. DCE that "knows" the fast path always succeeds would
happily delete the slow tail and break trap-out semantics; this is
forbidden. The op is treated as an atomic two-path construct by
all subsequent passes.

## 11. Code generation strategy

### 11.1 Direct asmjit emission

T2 lowers the optimised IR straight to asmjit's API, identical in
shape to BeamAsm's per-op emitters in `arm/instr_*.cpp`. Reuse:

- **Global runtime fragments**: `t2_deopt_dispatch`, GC entry,
  exception raise, scheduler re-entry — shared with BeamAsm.
- **Calling-convention helpers**: register flush/restore around C
  calls (the `emit_enter_runtime`/`emit_leave_runtime` patterns
  from BeamAsm).
- **Veneer infrastructure**: long branches, literal pool placement.

New T2-specific emitters:
- `speculate_type` (outer): single tag-bit test + jcc to T1 PC.
- `speculate_type` (inlined): same shape, jcc to per-region deopt
  stub.
- `speculate_range`: cmp + jcc, same boundary semantics.
- `untag_int` / `tag_int` / `add_small` / `mul_raw`: the small-int
  fast path; matches BeamAsm's existing arithmetic emitters.
- `framestate`: no code emitted; records metadata indexed by PC.
- Inlined-region prologue/epilogue: parameter load from outer X
  regs at entry; result store to outer X reg at exit.

Estimated emitter LOC: ~1.5 KLOC ARM64 + ~1 KLOC shared.

### 11.2 Sync-point-constrained register allocation

Per §6, the outer function has its own register layout but must
match T1's X/Y state at every sync point (function entry, calls,
returns, GC sites, BIF calls, speculation guards, tracing-relevant
sites, receive safe points).

The allocator is a single linear-scan pass over IR live intervals
(LuaJIT style; see `research/luajit.md`) with two extensions:

1. **Sync-point constraints** — each sync point carries a "live
   X-reg map" listing which SSA value must be in which X register
   (and similarly Y). At allocation time, values pinned by an
   active sync constraint cannot be displaced; values that need
   to live across a sync point either sit in the X reg the
   constraint pins, or get spilled and reloaded.
2. **Untagged-value tracking** — scratch values from
   `untag_int`/`mul_raw`/etc. are flagged untagged. These cannot
   be placed in any X/Y register (would confuse GC) and are
   automatically excluded from sync constraints (they're never
   live across sync points by construction — `tag_int` happens
   before the next sync point).

**v1 model**: T1's X/Y assignments are required *only* at sync
points. Between sync points the allocator is free; values can live
in any CPU register, untagged where profitable. The sync-point
constraints form the complete contract with T1.

### 11.3 Inlined regions

Inside an inlined region, sync-point constraints are looser
because the region is bracketed by a single outer-function
framestate at its entry call site. Within the region, the
allocator can freely remap registers — speculation guards inside
the region deopt via the framestate at the entry point, not via
mid-region sync.

The allocator runs on the entire function (outer + inlined) as a
single SSA scope; the difference between outer and inlined
regions shows up only as different sync-constraint density —
many constraints in the outer function, very few inside an
inlined region.

### 11.4 Why not LLVM / Cranelift / a separate backend

- LLVM: licensing OK now (Apache 2.0 with exception), compile-time
  cost too high for ~1ms target, dependency footprint enormous.
- Cranelift: workable, but adds a big dependency and we don't
  benefit from its multi-language design. asmjit reuse is smaller.
- Separate backend: HiPE lesson. Don't.

