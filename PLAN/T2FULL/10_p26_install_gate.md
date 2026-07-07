# P2.6 Blocker B — install-quality gate (diagnosis + design)

**Status:** diagnosis COMPLETE (2026-07-07); gate implementation lands AFTER
blocker A (async compile). Measured aarch64-apple-darwin, pinned `{scheduler,1}`,
best-of-N, on a build off `943da30a9d` (P2.5 fix included).

## The unifying principle (verified both directions)

**T2 beats T1 iff it removes work.** Winners eliminate allocations, inline
calls, or fuse ops. Losers re-emit T1's ops *plus a tax* and eliminate nothing
→ slower-or-equal. The gate's job: refuse to install any blob that is a strict
re-emit of T1's ops plus guards.

## Key reframe (de-risks the P2.5 headline)

The P2.5 figures **pattern −60% / lists −52%** were measured under `+JT2enable`
(force-compile-everything → every loser installs). Under realistic
counter-triggered `T2_RETAIN=1`, the same micros are **break-even** (lists
446→432 µs, pattern 289→296 µs) because most losers never trip. So the
net-negative-blob problem only bites **when a no-win blob actually installs**.
The gate removes that case in both modes, restoring the "never slower than T1"
floor.

## Root cause — two mechanisms, neither is guards/spills/regalloc

Verified: spills = 0 (`T2_RA_DUMP`), not a register-allocation problem.

1. **Non-tail-call lowering tax (the lists / body-recursion killer).**
   T2 lowers a non-tail call as `mov x30,<T1-cont>; mov x14,<target>; br x14`
   (indirect branch that does NOT push the return-address stack, so every paired
   `ret` on the ascent mispredicts). By design — `t2_emit.cpp:1342-1350`
   materializes the T1 continuation as CP and branches (demote-on-return): the
   descent runs in T2, the **ascent (the actual `+`/cons work) runs in T1**.
   So T2 can only *add* cost to a body-recursive function. Measured flat
   **~4.6 ns/frame T2 vs ~1.1 ns/frame T1** (cache-friendly) = ~3.5 ns fixed
   tax/non-tail-call. `append/2` 3.1× slower; isolated body-recursion 4.0–4.3×.
   **NOT cheaply codegen-fixable** — it is the demote-on-return architecture;
   a `blr` fix can't apply (the continuation is deliberately a *T1* address).
   → **the gate (don't install), not a codegen patch.**

2. **Dispatch lowering + speculation tax (the pattern shape).**
   T1 lowers a 27-way `select_val` as `i_select_val_bins` (binary search,
   ~5 compares, `ccmp`/`orr`-fused). T2 lowers the same `switch` as a **linear
   scan of 27 `cmp/b.eq`** plus an always-passing `speculate_small` on the loop
   counter (3 insns/iter, pure tax). Only breaks even because the hot clause is
   coincidentally T2's *first* case — cliffs if the hot value is last.
   **IS codegen-fixable** (port T1's `i_select_val_bins` binary-search / jump
   table into T2 `switch` isel/emit) — the single highest-value codegen fix —
   but even fixed only reaches T1 *parity*, so the gate should still reject it.

## The gate (static, compile-time)

A runtime A/B timer is unsafe (effectful functions can't be re-run to benchmark).
So the gate is purely static, evaluated in `t2_compile.cpp` right before
`erts_t2_install` (~line 370), where HIR, post-isel LIR, regalloc results and
blob size are all in hand. Every signal is a byproduct of passes that already
run (`t2_eligible.c` already walks every op + computes size/arity/loop-ness).

**Signals**
- `calls_inlined` = non-tail calls in source − non-tail `call`/`call_ext` in LIR
- `calls_retained` = non-tail `call`/`call_ext` remaining (incl. non-tail self-call)
- `fused_arith` = `add_small`/`sub_small`/… with unboxed intermediates under spec
- `bs_scan_runs` = fused bs scan-run ops
- `switch_max_cases` = largest `switch` case count
- `spec_guards`, `spills`, `blob_size` (supporting)

**Eliminated-work present** iff `calls_inlined ≥ 1` OR `fused_arith ≥ 2` OR `bs_scan_runs ≥ 1`.
**Disqualifying tax** iff `calls_retained ≥ 1` OR `switch_max_cases > 4` OR (`spec_guards ≥ 1` AND `fused_arith == 0`).
**Decision:** `INSTALL iff (eliminated-work present) AND NOT (disqualifying tax)`, else keep T1.

**Verification (all measured):**

| function | inlined | fused | switch | retained | rule | measured |
|---|---|---|---|---|---|---|
| mvp `total/2` | 1 | 2 | – | 0 | INSTALL | 2.1× faster ✓ |
| scanbench | – | – | – | 0 | INSTALL (scan-run) | 2.6–2.9× ✓ |
| `tsum` tail-arith | 0 | 2 | – | 0 | INSTALL | +28% ✓ |
| `append/2` | 0 | 0 | – | 1 | REJECT | 3.1× slower ✓ |
| `reverse/2` | 0 | 0 | – | 0 | REJECT (E=0) | 15% slower ✓ |
| `blist`/`nsum` | 0 | 0 | – | 1 | REJECT | 4.0–4.3× slower ✓ |
| `pat_loop4/2` | 0 | 0 | 27 | 0 | REJECT | break-even/cliff ✓ |

`calls_retained ≥ 1` catches the whole body-recursion class; the `E == 0` arm
catches tail-cons + dispatch losers. Thresholds (`fused_arith ≥ 2`,
`switch > 4`) to be tuned against a larger corpus before the gate lands.

## Files
- Gate widening: `erts/emulator/beam/jit/t2/t2_eligible.c` (already yields op histogram/size/loop-ness)
- Install point: `erts/emulator/beam/jit/t2/t2_compile.cpp:~370`
- Call lowering: `erts/emulator/beam/jit/t2/t2_emit.cpp:1342-1384`
- Optional codegen fix: T2 `switch` isel/emit → port T1 `i_select_val_bins`
