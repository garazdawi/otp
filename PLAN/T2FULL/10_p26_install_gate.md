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

## Implementation — LANDED (2026-07-07, on top of blocker A)

Base rebased onto `lukas/erts/beamjit2` tip `7057c02a97` (A's async compile
+ P2.5). The gate is evaluated in `t2_compile_install_one`
(`t2_compile.cpp`) right after the blob is emitted and before
`erts_t2_install`. Signals are read off the post-isel + post-regalloc LIR
(op-kind counts) plus a hoisted `leaf_inlined` flag:

- `calls_retained` = #Call + #CallExt (light-BIF `CallBif` and tail calls
  excluded — only the demote-on-return non-tail call carries the tax);
- `fused_arith` = #AddSmall + #SubSmall;  `spec_guards` = #SpeculateSmall;
- `bs_scan` = #{StartMatch,BsMatch,BsGetTail,BsTestTail};
- `switch_max_cases` = widest `Switch.num_cases`;
- `calls_inlined` = `leaf_inlined` + presence of a foldl-class intrinsic
  loop (`ReductionCheckCallee`/`ChargeReds`/`DemoteCallee` erase the lists
  callee they stand in for);
- `spills` structurally 0 (the placement allocator keeps every value in an
  X/Y home) — trace only, not in the rule.

Rule exactly as designed. On REJECT: the blob is freed through the same
barrier-scheduled path a failed install uses, `RejectedGate` is returned,
and the tier worker's existing disarm-on-any-non-install makes it terminal
(never retries). Behind `T2_INSTALL_GATE` (default ON; `=0` = pre-B
behaviour). Rejections counted in a new `t2_tier_stats.rejected` field
(counter path, kept distinct from `failed`) and `t2_stats.gate_rejected`
(+JT2enable path); `t2_gate: REJECT …` trace under `T2_DEBUG`/`T2_INSTALL_TRACE`.

### Validation (aarch64-apple-darwin, pinned {scheduler,1}, best-of-N)

Winners still install + win (measured, gate ON):
| winner | mode | T1 | T2 gate-ON | speedup |
|---|---|---|---|---|
| mvp `total/2`   | +JT2 / RETAIN | 3.63 / 3.50 µs | 1.96 / 1.75 µs | 1.85–2.0× |
| scanbench plain | +JT2 / RETAIN | 1267 / 1210 µs | 445 / 484 µs | 2.5–2.8× |
| scanbench digit | +JT2 | 1124 µs | 390 µs | 2.9× |
| `tsum` tail-arith | +JT2 | 411 µs | 287 µs | 1.43× |
| foldl `sumfold/1` | +JT2 | — | installs (intrinsic) | — |

Losers rejected → recover to the T1 floor (gate ON vs OFF vs T1):
| loser | T1 | gate ON | gate OFF (installs loser) |
|---|---|---|---|
| lists micro (+JT2) | 445 µs | **443 µs** | 1103 µs (2.5× slower) |
| apponly (+JT2) | 292 µs | **292 µs** | 929 µs |
| revonly (+JT2) | 172 µs | **172 µs** | (T2 ~15% slower) |

`t2_tier_stats` (RETAIN, aggressive threshold): gate ON → 25 compiled, **2
installed** (the two winners), **20 rejected**, 3 failed (genuine compile
failures, kept separate); gate OFF → 22 installed, 0 rejected.

A+B macro: dialyzer PLT build (stdlib+kernel, aggressive tiering) — gate
OFF 6.59 s (≈13% over T1), gate ON 6.00 s (within ~3% of T1 5.81 s): the
gate recovers nearly all of the residual regression (dialyzer is almost
entirely body-recursive analysis = the loser shape, so it has few T2 wins;
the ~3% residual is counter/async overhead, not installed losers).

Behavioral smoke (T1 vs RETAIN-gate-ON vs +JT2-gate-ON/OFF): identical
`phash2` across all modes over two workloads covering spawn/link/monitor,
exits, GC, receive-timeout, exceptions, many-way dispatch, and bitstring
scanning. The gate is compile-time and strictly subtractive (installs a
subset of the pre-B set, each blob byte-identical), so it cannot introduce
new behaviour.
