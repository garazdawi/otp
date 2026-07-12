# Stage 3 — standard optimization suite across the inlined boundary

Exploration goal (continuation of Stage 1, `mapsfold_design.md`): once a hot
caller is inlined into a T2 blob (the `maps:fold` fast loop, the lists
intrinsics, the leaf inliner), run the **standard compiler optimizations** on
the resulting HIR so the inlined body is as tight as a hand-written loop.
Purge/trace safety stays out of scope. This spec is grounded in the *actual*
generated code for `fold_bench:sum_vals/1` (dumps below), not on paper.

## What the inlined loop actually looks like (measured)

`T2_INTRIN_DUMP` for `sum_vals(M) = maps:fold(fun(_K,V,A)->A+V end,0,M)`:

```
block0:
  v0@x0 = param #0
  gc_test words=2 live=1                       ; the fun's 2 heap words
  v1@x1 = make_fun #0                           ; DEAD on the fast path
  v2@x2 = copy v0@x0
  v3@x0 = copy v1@x1                            ; DEAD on the fast path
  v4@x1 = const_int 0
  v5 = is_flatmap_bounded v2@x2
  branch v5 -> block2(fast) | block1(slow)
block1(slow): tail_call_ext maps:fold/3 v3, v4, v2    ; the only user of v1/v3
block2(fast):
  v6@x8 = flatmap_size v2@x2
  fold_budget v6@x8
  v13@x3 = copy v4@x1 ; v14@x4 = const_int 0 ; v15 = const_int 1 ; jump block3
block3:
  v7@x3 = phi v13,v12 ; v8@x4 = phi v14,v16
  v9 = cmp_lt v8@x4, v6@x8 ; branch v9 -> block4 | block5
block4:
  v10@x5 = flatmap_key_at v2@x2, v8@x4          ; DEAD — sum_vals ignores K
  v11@x6 = flatmap_val_at v2@x2, v8@x4
  jump block6
block5(exit): v17@x0 = copy v7@x3 ; return v17@x0
block6:
  speculate_type v7@x3, v11@x6
  v12@x3 = add_small v7@x3, v11@x6 ; jump block7
block7:
  v16@x4 = add_small v8@x4, v15 ; jump block3
```

Emit (`T2_DUMP`) confirms the per-iteration waste in `block4`+`block6`:

- The dead `flatmap_key_at` lowers to ~6 aarch64 instrs (`lsr` untag-i,
  `and x,x,-8` untag-map, `ldr [+16]` keys term, `and -8` untag keys tuple,
  `add 8` skip arity, `ldr [base+i<<3]`) — **all dead**.
- The map untag `and x,x,-8` is recomputed in every `flatmap_*_at` and is
  loop-invariant.
- The index untag `lsr x,x,4` is recomputed per load.
- `make_fun` still allocates on the fast path and forces the `gc_test`.

## The opt suite and its concrete targets here

| Opt | Fires on this example | Kind of win |
|-----|-----------------------|-------------|
| **DCE** | `flatmap_key_at` (sum_vals); slow-only `make_fun`/`copy` once sunk | per-iter (big) + per-call |
| **make_fun sink** (partial DCE) | move `v1`/`v3` into `block1`, shrink entry `gc_test` by the fun words | per-call: kills a heap alloc + GC test on the fast path |
| **Copy propagation** | `v13=copy v4(const0)`, `v17=copy v7`, count_pos copy chains | cleanup, fewer moves |
| **Constant folding** | fold all-const ops (cmp/add of consts); `const 0` feeders | cleanup |
| **CSE / GVN-lite** | dedup identical pure ops (map untag shared by key/val; repeated consts) | per-iter |
| **LICM** | hoist loop-invariant pure ops out of `block3..7` (map-base, keys-base) | per-iter |
| **IV strength reduction** (stretch, 3e) | replace `m+24+(i>>4)<<3` with a value pointer that += 8; drop i-untag; optionally compare pointers and retire `i` | per-iter (biggest single) |

`count_pos` additionally exercises single-input phis and longer copy chains
(`v18=copy v7 ; v12=phi v18 ; v15=copy v12 ; v20=copy v15`) — copy-prop +
dead-phi elimination collapse them.

## Hard constraints (the T2 deopt/home model — do not break)

T2 HIR is not textbook SSA; opts MUST respect:

1. **Homes.** Every value has a BEAM register home (`dst_reg`) and each op
   records the home it reads each operand from (`operand_regs[i]`). `copy` ops
   are *register moves*, not no-ops. Eliminating/merging a value changes homes.
   Safe rule: after any rewrite, the full validator (`t2_validate`) re-proves
   the home/liveness/sync model — so a pass may rewrite freely but MUST leave a
   validator-legal blob or the whole compile degrades to T1 (loudly). Prefer
   conservative rewrites that keep homes consistent.
2. **Sync maps are uses.** A `T2SyncMap` (`op->sync`, `x[]`, phi feeders) holds
   the exact live BEAM state at a deopt/call boundary. A value referenced by
   any live sync map is NOT dead. Use-counting MUST scan operands, phi inputs,
   AND every sync map's `x[]`/`y[]`. CSE that merges two values must not change
   what a sync map observes at a boundary (i.e. only merge when the survivor is
   available with the same home at every boundary that referenced the victim).
3. **Deopt classes stay intact.** The callsite-class ops (`T2_OP_SPEC_CALLSITE`,
   `fold_budget`, the spliced `add_small`/`speculate_type`) carry a call-boundary
   sync map and side-exit to the erased call's T1 PC. Opts must not drop their
   sync, move them across the shape guard, or hoist them out of the region the
   window validator (`t2_validate_windows`) checks. The fast loop must remain
   effect-free/alloc-free and must not write below `sync->x_live` (==3) — so
   strength-reduction pointers must live in X≥ the spliced frontier, or in the
   dedicated untagged-home mechanism (`T2_LIR_VF_UNTAGGED`).
4. **Purity.** DCE/CSE apply only to side-effect-free, non-faulting ops. Pure
   here: `const_int`, `copy`, `cmp_lt`, the flatmap loads (`flatmap_size`,
   `flatmap_key_at`, `flatmap_val_at`, `is_flatmap_bounded` — never fault), and
   dead `add_small`/`sub_small` (their deopt guard is dead too when the result
   is). NOT pure: `make_fun` (allocates — DCE only via sinking + gc_test
   adjustment), calls, returns, branches, `fold_budget`, `gc_test`, stores.

## Pass structure, placement, levers

Add one general HIR pass module `t2_opt.cpp/.hpp` exposing
`bool t2_opt(T2Function&, const T2LoopInfo&, bool *changed, std::string *err)`
run as a **fixpoint** of the sub-opts. Wire it in `t2_compile.cpp` right after
`t2_licm_lite` (line ~479) and before the `rewritten && t2_validate` re-check
(so its output is re-validated by both `t2_validate` and `t2_validate_windows`,
exactly like the existing passes). Only run when `recovered || intrinsified`
(the blob has an inlined body to optimize).

Levers (mirror `T2_NO_SPEC`/`T2_NO_INLINE`): master `T2_NO_OPT`, plus per-opt
`T2_NO_DCE`, `T2_NO_CONSTFOLD`, `T2_NO_COPYPROP`, `T2_NO_CSE`, `T2_NO_SINK`,
`T2_NO_STRENGTH` — so each opt's contribution is measurable in isolation.
`T2_OPT_TRACE` logs what fired. `T2_OPT_DUMP` dumps HIR after the pass.

Shared infra (build once, reuse): a use-counter over the whole function
(operands + phi inputs + all sync-map `x[]`/`y[]`), a purity predicate
`t2_op_is_pure(kind)`, and a value-number key `(kind, imm_int, index,
operand-id-vector)` for CSE.

## Decomposition into small functional commits

Each lands only when it builds `-Werror` clean (incl. x86 `-Werror=sign-compare`,
which the ARM leg misses — recompile the modified sources with
`-Wsign-compare -Werror=sign-compare` before declaring done) AND the
independent harness passes with byte-identical results/order/reductions:

- **3a — DCE** (+ the shared use-counter/purity infra). Kills the dead
  `flatmap_key_at`. Smallest safe slice; proves the infra.
- **3b — constant folding + copy propagation** (home-aware).
- **3c — CSE / GVN-lite** (pure-op value numbering, sync-safe merging).
- **3d — make_fun sinking** (partial DCE: sink to the slow block, shrink the
  entry gc_test by the fun's word count; the fast path then neither allocates
  the fun nor GC-tests for it).
- **3e — IV strength reduction / pointer walk** (stretch; isel/emit-level:
  preheader computes the untagged value/key base, the loop walks a pointer that
  += 8, the index untag disappears; optionally retire `i` by comparing the
  value pointer against an end pointer).

## Correctness + measurement gates

- `mapsfold_verify:all()` (independent harness) green under: opts-on,
  `T2_NO_OPT=1`, each per-opt lever, and plain T1 — all with identical
  checksums, order, and the `10+5n` reduction identity. (The opts change the
  instruction stream, NOT the term result or the reduction charge — `fold_budget`
  is untouched; a dead-guard DCE removes no live charge.)
- `map_SUITE` / `maps_SUITE` green T2-forced.
- `fold_bench:baseline()` for `sum_vals` and `count_pos`: report ns/fold for
  T1, opts-off (Stage 1 only), and opts-on, plus a per-opt breakdown via the
  levers, so each opt's ns contribution is attributed. Target: opts-on beats
  Stage-1-only, and the gap is explained op-by-op.
- No new lists-intrinsic regression (`lists_regress`), and the identity/tax
  blobs unaffected (opts only run on recovered/intrinsified blobs).
