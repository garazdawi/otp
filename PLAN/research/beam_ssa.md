# BEAM SSA — The AOT Compiler's IR and Optimizations

Sources (all in this repo):
- `lib/compiler/src/compile.erl` — pipeline definition.
- `lib/compiler/src/beam_ssa.hrl` — IR record definitions.
- `lib/compiler/src/beam_types.hrl` — type lattice.
- `lib/compiler/src/beam_ssa_type.erl` — type inference.
- `lib/compiler/src/beam_ssa_opt.erl` — optimization orchestrator + many passes.
- `lib/compiler/src/beam_ssa_pre_codegen.erl` — register allocation.
- `lib/compiler/src/beam_ssa_codegen.erl` — SSA → BEAM bytecode.
- `lib/compiler/internal_doc/beam_ssa.md` — invariants and exception handling.
- `lib/compiler/internal_doc/ssa_checks.md` — `%ssa%` test syntax.
- `lib/compiler/src/cerl_inline.erl` — Core Erlang inliner.

This is the inventory of what is *already* available to be reused as a T2
optimizer. The design heavily relies on the fact that the AOT compiler
already has 15+ years of investment in SSA-level optimizations and a
mature type system that we want to extend rather than reimplement.

## 1. Compilation pipeline

From `compile.erl`:

1. **Frontend** — parse, transform, lint, expand records.
2. **Core Erlang** — `core` → `core_lint_module` → `core_old_inliner`
   (`cerl_inline`) → `sys_core_fold` → `sys_core_alias` → `sys_core_bsm`
   → core transforms.
3. **BEAM SSA** —
   - `core_to_ssa` (`beam_core_to_ssa`, ~138 KB) — Core → SSA, three
     passes: flatten Core, lift lambdas, translate with pattern-match
     compilation.
   - Pre-opt: `beam_ssa_bool`, `beam_ssa_share`, `beam_ssa_recv`,
     `beam_ssa_bsm`.
   - **Main optimization phase: `beam_ssa_opt`** (~151 KB), 44 named
     passes in 6 phases (see §4 below).
4. **Codegen** — `beam_ssa_pre_codegen` (~132 KB, register allocation
   in 19 phases) → `beam_ssa_codegen` (~105 KB, SSA → BEAM ops).
5. **Assembly** — `beam_a` → `beam_block` → `beam_jump` → `beam_clean`
   → `beam_trim` → `beam_flatten` → `beam_z` → validators → `beam_asm`.

The interesting layers for T2 are **steps 3 and 4** — BEAM SSA itself
plus its optimization orchestrator. Everything earlier is parsing-side
(file-bound, AST-shaped) and not relevant. Everything later (assembly)
is bytecode-shaped; T2 will probably bypass it.

## 2. The IR shape

From `beam_ssa.hrl`:

```erlang
-record(b_module, {
  anno=#{} :: anno(),
  name :: module(),
  exports :: [{atom(), arity()}],
  attributes :: list(),
  body :: [b_function()]}).

-record(b_function, {
  anno=#{} :: anno(),
  args :: [b_var()],
  bs :: #{label() => b_blk()},  %% block map (or list of {label, block})
  cnt :: label()                %% fresh-name counter
}).

-record(b_blk, {
  anno=#{} :: anno(),
  is :: [b_set()],              %% straight-line instructions
  last :: terminator()          %% br | switch | ret
}).

-record(b_set, {
  anno=#{} :: anno(),
  dst=none :: 'none' | b_var(),
  op :: op(),
  args=[] :: [argument()]
}).
```

Terminators:
```erlang
-record(b_ret, {anno=#{}, arg :: value()}).
-record(b_br,  {anno=#{}, bool :: value(), succ :: label(), fail :: label()}).
-record(b_switch, {anno=#{}, arg :: value(), fail :: label(),
                   list :: [{b_literal(), label()}]}).
```

Values: `#b_var{name}`, `#b_literal{val}`, `#b_remote{mod, name, arity}`,
`#b_local{name, arity}`.

True SSA: variables are assigned exactly once. There are no explicit phi
nodes — block joins handle merging through implicit phi-at-entry.
**Type information lives in `#b_set.anno` under the `result_type` key.**

`internal_doc/beam_ssa.md` documents the invariants — exception block
`?EXCEPTION_BLOCK = 1` is reserved, `succeeded:body` semantics for
calls that may fail, etc. Required reading before touching the IR.

## 3. The type lattice

`beam_types.hrl` defines:

```
any (top)
├ #t_atom{elements=any | ordset(atom())}
├ #t_number{}
│  ├ #t_float{elements=any | range}
│  └ #t_integer{elements=any | beam_bounds:range()}
├ #t_list{} → #t_cons{}, nil
├ #t_tuple{size, exact, elements=#{pos => type}}
├ #t_fun{arity, target, type}
├ #t_map{super_key, super_value}
├ #t_bitstring{size_unit, appendable}
├ #t_bs_context{}, #t_bs_matchable{}
├ pid, port, reference, identifier
└ none (bottom, unreachable)
```

Limits (constants in `beam_types.hrl`):
- `?ATOM_SET_SIZE = 5` — beyond 5 distinct atoms, collapse to `any`.
- `?TUPLE_ELEMENT_LIMIT = 12` — track element types only for indices ≤12.
- `?TUPLE_SET_LIMIT = 12` — beyond 12 distinct tuple shapes, collapse.
- `?RETURN_LIMIT = 30` — beyond 30 distinct return patterns, collapse.

Unions (`#t_union{}`) hold non-overlapping branches by category (atom,
list, number, tuple_set, other). Operations: `meet/2` (intersection,
narrowing) and `join/2` (union, widening).

This lattice is exactly what T2 wants to consume. The type information
that survives whole-module AOT inference is loaded with the BEAM file
(in OTP 25+ via `+type_information`); T2 starts from here and *narrows
further* using runtime profile data.

## 4. The 44 optimization passes

From `beam_ssa_opt`, organised by phase. Highlight: this list is the
de facto T2 optimization menu — most passes are SSA-level, in-memory,
self-contained. Anything tagged "in-memory, self-contained" is a
candidate to invoke directly from T2 with a different driver.

### Prologue (control-flow normalization)
1. `ssa_opt_split_blocks`
2. `ssa_opt_coalesce_phis`
3. `ssa_opt_tail_phis`
4. `ssa_opt_element` — `element(N, T)` simplification
5. `ssa_opt_linearize` — block map → linear form
6. `ssa_opt_tuple_size` — cache `tuple_size/1`
7. `ssa_opt_record` — fold `is_tuple/tuple_size/element` → `is_tagged_tuple`
8. `ssa_opt_update_tuple` — chain `setelement/3` → `update_tuple`
9. `ssa_opt_cse` — type-aware CSE
10. `ssa_opt_live` — liveness, dead-store kill

### Module passes
11. `ssa_opt_bc_size` — bytecode size estimation (for inlining decisions)
12. `ssa_opt_type_start` — **whole-module type signature analysis**

### Repeated fixpoint (up to `?MAX_REPETITIONS = 16` until convergence)
13. `ssa_opt_live`
14. `ssa_opt_is_between` — `is_integer(X, Min, Max)` → 3 BIFs
15. `ssa_opt_ne` — `=/=` simplification
16. `ssa_opt_bs_create_bin` — bitstring construction
17. **`ssa_opt_dead`** (~54 KB, ~1600 lines) — dead code + branch shortcut
    + type-narrowing-from-conditions. **Highest-impact pass.**
18. `ssa_opt_cse`
19. `ssa_opt_tail_phis`
20. `ssa_opt_sink` — sink instructions toward uses
21. `ssa_opt_tuple_size`
22. `ssa_opt_merge_updates` — chain `setelement` more
23. `ssa_opt_record`
24. `ssa_opt_try` — exception simplification
25. **`ssa_opt_type_continue`** — `beam_ssa_type:opt_continue` —
    incremental type-driven optimization (the second-most-impactful
    pass).

### Epilogue module passes
26. `ssa_opt_alias` (~69 KB) — alias analysis for destructive updates
27. `ssa_opt_destructive_update` (~50 KB) — in-place tuple/map updates

### Early epilogue
28. `ssa_opt_type_finish`
29. **`ssa_opt_float`** (~1200 lines) — convert mixed-type arith to
    direct float ops; emit `{float,convert}` to unbox once and reuse
30. `ssa_opt_sw` — switch optimization
31. `ssa_opt_no_reuse`
32. `ssa_opt_deoptimize_update_tuple`

### Late epilogue (final normalization)
33–46: `ssa_opt_live` (final), `ssa_opt_bsm`, `ssa_opt_bsm_shortcut`,
`ssa_opt_sink`, `ssa_opt_blockify`, `ssa_opt_redundant_br`,
`ssa_opt_merge_blocks`, `ssa_opt_bs_ensure`, `ssa_opt_try`,
`ssa_opt_get_tuple_element`, `ssa_opt_tail_literals`,
`ssa_opt_trim_unreachable`, `ssa_opt_unfold_literals`, `ssa_opt_ranges`.

### The "highest leverage" passes for T2

Listed in order of expected ROI when type information becomes more
precise (because of profile feedback):

1. **`beam_ssa_type:opt_continue`** — better input types ⇒ more
   guards/checks elided ⇒ feeds dead-code & CSE.
2. **`ssa_opt_dead`** — branch shortcut on more-precise predicates.
3. **`ssa_opt_cse`** — type-aware CSE recognises more redundancy.
4. **`ssa_opt_float`** — explicit float context once we know operand
   types.
5. **`ssa_opt_record`** + **`ssa_opt_alias`** + **`ssa_opt_destructive_update`**
   — when we know an update's tuple is unaliased, in-place rewrite.

## 5. Type inference (`beam_ssa_type`)

Algorithm (lines 116–235 of `beam_ssa_type.erl`):

1. **Module-level signature analysis (`opt_start/2`)**:
   - Exported functions start with `any` arg types; locals start with
     `none`.
   - Forward dataflow: propagate from call sites into callees.
   - When a return type narrows, schedule callers for re-analysis.
   - Fixpoint when args + returns stabilise.
2. **Per-function refinement (`opt_continue/4`)**:
   - Forward dataflow within each function.
   - Guards narrow types via `meet/2` (`is_integer(X)` → X gains
     `#t_integer{}`).
   - Phi-at-block-entry merges via `join/2`.
3. Whole-pipeline fixpoint up to 16 times in the main optimizer.

Based on Lindahl & Sagonas's "Practical Type Inference Based on Success
Typings" — same lineage as Dialyzer.

For T2: the algorithm itself is portable. The novel input is profile
data narrowing the *initial* arg types of exported (or just hot)
functions. If instead of `any` we start with the observed-types-at-
runtime, the same pipeline produces strictly more precise output.

## 6. Inlining

Currently happens at **Core Erlang level** in `cerl_inline.erl`
(~103 KB). Driven by:
- `-compile(inline)` to inline everything.
- `-compile({inline, [{F,A}, ...]})` to inline specific functions.
- `-compile({inline_size, N})` to set the size threshold.

Defaults: `default_size() -> 24.`, `default_effort() -> 150.` (the
"effort" cap protects against worst-case compile-time blowup).

There is **no SSA-level inlining**. This is precisely the gap a T2
should fill — profile-guided inlining at SSA level, after type
information has already narrowed call sites to monomorphic targets.

## 7. Codegen and register allocation

`beam_ssa_pre_codegen` does linear-scan register allocation on SSA
form, citing:
- Mössenböck & Pfeiffer (2002): Linear Scan RA in SSA form
- Wimmer & Mössenböck (2005): Optimized Interval Splitting
- Wimmer & Franz (2010): Linear Scan RA on SSA Form

Phases (selected): `find_yregs`, `reserve_yregs`, `copy_retval`,
`number_instructions`, `live_intervals`, `reserve_regs`, **`linear_scan`**,
`frame_size`, `turn_yregs`.

`beam_ssa_codegen` then translates each block to BEAM ops, using the
register annotations.

For T2 we have a choice:
- **Reuse `beam_ssa_pre_codegen`** to allocate to BEAM X/Y registers,
  then emit BEAM ops, then ingest into BeamAsm normally. (Sista-
  style.)
- **Replace `beam_ssa_pre_codegen`** with a native register
  allocator that emits machine code directly. (Maglev-style.)

The first option is dramatically simpler and reuses ~130 KB of
hardened code. See `T2.md` for the recommendation.

## 8. SSA invariants (from `internal_doc/beam_ssa.md`)

Critical reading. Highlights that the IR *requires*:

- `succeeded:body` and `succeeded:guard` ops to express "this op may
  fail and the failure is observable". Used after `bs_match`, BIFs that
  can fail in body context, etc.
- `?EXCEPTION_BLOCK = 1` — the unique fail target for all unhandled
  failures.
- Block predecessors are derived from terminators; the `bs` map can be
  reordered and re-derived because of this.
- Try/catch lowers to `new_try_tag`/`landingpad`/`kill_try_tag`
  primitives.
- Old-style `catch` lowers similarly with a different primitive.

If T2 mutates SSA, these invariants must hold afterward. The validator
(`beam_ssa_validator` if present, or the next pass tripping over a
violation) catches violations, so this is testable.

## 9. Pass sizes — what's portable vs not

| Module | Size | Portable to T2? | Notes |
|--------|------|-----------------|-------|
| `beam_ssa_type` | ~117 KB | Yes | Pure SSA; needs richer initial types from profile data. |
| `beam_ssa_opt` | ~151 KB | Mostly | Orchestrator; selectively invoke passes. |
| `beam_ssa_pre_codegen` | ~132 KB | Partial | Linear scan reusable; output adapts to either BEAM regs or native. |
| `beam_ssa_codegen` | ~105 KB | If we go bytecode-out | Re-emits BEAM ops; needs no change if T2 is bytecode-to-bytecode. |
| `beam_ssa_alias` | ~69 KB | Yes | Pure SSA. |
| `beam_ssa_bool` | ~61 KB | Yes | Pure SSA. |
| `beam_call_types` | ~56 KB | Yes | Type signatures for BIFs/builtins. Data-only. |
| `beam_ssa_dead` | ~54 KB | Yes | Pure SSA, highest-leverage pass. |
| `beam_ssa_destructive_update` | ~50 KB | Yes | Pure SSA. |
| `cerl_inline` | ~103 KB | Partial | Core-Erlang-shaped; T2 wants SSA-level inlining instead. |
| `beam_core_to_ssa` | ~138 KB | No | Front-end only; T2 reads existing SSA. |
| `beam_ssa_bsm` | ~45 KB | Yes | Pure SSA. |
| `beam_ssa_recv` | ~36 KB | Yes | Pure SSA. |

Roughly 40–45 % of the compiler is in-memory SSA passes that T2 can
either invoke directly or adapt with minimal changes. The rest is
front-end and assembly emission that T2 wouldn't need.

## 10. What T2 specifically needs that doesn't exist yet

1. **Profile-narrowed initial types.** Today `beam_ssa_type` starts
   exported functions with `any`. T2 wants to start them with the
   types observed at the actual call sites in the running system.
2. **SSA-level inlining.** The inliner is at Core level; SSA-level
   inlining doesn't exist. The infrastructure (function lookup, fresh-
   name counters in `b_function.cnt`) is there, but the inlining pass
   has to be written.
3. **Speculation & deopt support in the IR.** New SSA op kinds for
   "type guard with deopt target", and an annotation format for the
   X-register snapshot at each guard. Both add to `b_set.anno`.
4. **A non-orchestrator driver** that takes a single function (not a
   whole module), runs a curated subset of passes, and returns
   optimised SSA + a deopt-metadata table.
5. **A native code generator** *or* a bytecode-emit pipeline that
   produces something BeamAsm can ingest.

These are the work items T2.md lays out.
