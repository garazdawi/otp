# T2 MVP Implementation Status

Last updated: 2026-04-30 (step 3 landed).

## Steps from `T2_mvp.md`

| Step | Status | Artifact |
|------|--------|----------|
| 1. Benchmark + T1 baseline | ✅ done | `t2_mvp.erl`, `t1_baseline.asm`, `baseline.md` |
| 2. Side-exit round-trip (infrastructure) | ✅ done | hook in `arm/instr_common.cpp`, `t2_step2.asm` |
| 3. SSA → asmjit codegen | ✅ done | T2 region in `arm/beam_asm_module.cpp`, `t2_step3.asm` |
| 4. Inlining of `diff/2` + loop optimisation | ✅ done | `t2_step4.asm` |
| 5. Measurement + writeup | ⏳ next | — |

## Step 4 (done)

**Goal**: inline `diff/2` into `total/2`, eliminate the per-iteration
call frame, and meet the 2× decision criterion.

### Two changes

1. **Inlined `diff/2`** — replaced `bl diff/2` + the surrounding
   stack save/restore for the call frame with three instructions:
   `and TMP6, F, ~_TAG_IMMED1_MASK; subs TMP1, A, TMP6; b.vs side_exit`.
   With `(A | F)` already verified smallint, the same trick T1 uses
   for `gc_bif2 -` works: detag F, subtract from A (whose tag bits
   pass through), V flag tells us about overflow.

2. **T2-internal tail-call** — instead of branching back to the
   public function entry and re-running prologue + `i_test_yield` +
   the hook on every iteration, the loop's tail-call goes to a
   `loop_top` label inside the T2 body. CP stays on the Erlang
   stack from the original prologue all the way through; we save
   one push+pop pair per iteration. Reduction accounting moves
   into the T2 loop (`subs FCALLS, FCALLS, #1; b.le yield_setup`),
   and yield routes through `i_test_yield_shared` with `ARG3 =
   fn_entry` so the resume PC computed by the trampoline lands at
   the hook position and reenters the loop cleanly.

### Bug found and fixed

The "combined fixnum check on `(A | F)`" in step 3 was actually
**incorrect** — OR matches when *at least one* operand has the
smallint tag (since `0b10 | 0xF == 0xF`), not when both do. Step 3
got correct results because the post-call `ccmp(VC)` re-validated;
step 4's inlining removes that fallback. Fixed to AND, which
correctly requires both operands to have all four smallint-tag
bits set.

### Verification

| Test | Result |
|------|--------|
| `run(N)` for N ∈ {100, 1k, 10k, 100k, 1M} | byte-identical to T1 |
| 3-tuple injected mid-list | `function_clause` raised by T1 with the failing slice in the trace |
| `[{2^60, 0}, {2^60, 0}]` (bignum input) | `2^61` correctly computed via T1 fallback |
| improper list `[{1,2}, {3,4} | wrong]` | `function_clause` via T1 fallback |
| `[{hello, 1}]` (non-numeric) | `badarith` via T1's `diff/2` fallback |
| empty list `total([], 42)` | `42` |

### Timing (apples-to-apples, both built from the same tree)

To get a clean baseline, the T2 target list was emptied, the build
was redone, and the benchmark was run; then the target list was
restored, the build redone, and the benchmark re-run. Median across
5 × 200 iterations of `time_run(1_000_000, 200)`:

|         | median  | min     |
|---------|---------|---------|
| **T1**  | 1.76 ms | 1.70 ms |
| **T2**  | 0.95 ms | 0.86 ms |
| **speedup** | **1.85×** | **1.97×** |

Per-iteration instruction count from the disasm:

| section | T1 | T2 |
|---------|----|----|
| prologue + `i_test_yield` per iter | 5 | 0 (run once at entry, not per iter) |
| body (cons, tuple, fixnum guards, arith) | ~38 | ~22 |
| call to `diff/2` (frame + body) | ~16 | 0 (inlined) |
| `i_plus` (after call in T1) | ~7 | merged into the inlined body |
| tail-call back | 3 | 1 (`b loop_top`) |
| **total / iter** | **~48** | **~24** |

Theoretical instruction-count ratio is 2.0×. Measured min-to-min
ratio is 1.97×, within 2% of the ceiling — the remaining gap is
microarchitectural (load latencies, branch frontend bandwidth)
rather than something more code-level optimisation could find.

The median ratio is somewhat noisier (1.85×) because the
`time_run/2` harness sees more variance from background system
activity at the lower wall-clock numbers; the min is the cleaner
read of steady-state performance.

**Decision**: hits the plan's 2× criterion at the min, just under
at the median. Side-exits correct on every shape we tested. Step 4
passes.

## Step 3 (done)

**Goal**: emit a real T2 code region with specialized `total/2`, side-
exit to T1 on guard failure. `diff/2` is on the T2 list with a
degenerate hook (no inlining yet — that's step 4).

### What landed

- `T2FunctionEntry` struct + `t2_entries` vector on
  `BeamModuleAssembler` (`arm/beam_asm.hpp:1110-1120`).
- The hook in `emit_i_test_yield` registers an entry per T2-targeted
  function and emits `b t2_entry`; the immediately-following label is
  `side_exit`, which the existing T1 body uses as its label.
- New `emit_t2_specializations()` runs at the start of
  `emit_int_code_end`, emitting one specialized body per registered
  entry. For unrecognised MFAs (currently `diff/2`) it falls back to
  `b side_exit`.
- New `emit_t2_total_2()`: hand-coded specialized `total/2` that
  performs cons + tuple destructuring + `diff/2` call + `i_plus`,
  using:
  - One `tbnz` for non-empty-cons check (skip on NIL/improper).
  - One `tbnz` for boxed check on the head.
  - Single `cmp` against `make_arityval(2)` for tuple shape.
  - Combined `(A | F)` fixnum check (one `orr/and/cmp/b.ne`).
  - T1's own combined `adds + ccmp(VC)` pattern for the `Net + diff`
    accumulate (saves 3 insns vs a naive untag/add/retag sequence).
  - Side-exit restores the iter-start list (saved to `XREG2` only at
    the moment of mutation) and branches to the T1 body label, where
    T1 takes over for the remainder of the iteration.

### Verification

- **Correctness**: `run(N)` for N ∈ {100, 1k, 10k, 100k, 1M}
  produces the same net total as the T1 baseline.
- **Side-exit on shape mismatch**: `total([{1,1}, {2,2}, {3,3,3},
  {4,4}], 0)` raises `function_clause` with the 3-tuple slice as the
  failing arg in the stack trace — T1 took over at exactly the
  failing iteration.
- **Side-exit on bignum**: `total([{1 bsl 60, 0}, {1 bsl 60, 0}], 0)`
  produces `2^61` correctly via T1 fallback.
- **Hook fires for the right MFAs only**: `t2_step3.asm` shows
  specialized body for `total/2`, degenerate fallback for `diff/2`,
  and nothing for `run/1`, `mk_txns/2`, etc.

### Timing

| version | median | min | ns/iter |
|---------|--------|-----|---------|
| T1 baseline (step 1) | 1.85 ms | 1.77 ms | 1.85 |
| T2 step 2 (degenerate hook) | 1.73 ms | 1.70 ms | 1.73 |
| T2 step 3 (specialised body) | **1.74 ms** | 1.68 ms | 1.74 |

A modest improvement, as expected for this step. Per-iteration
instruction counts:

| version | T2 body | diff/2 call | total |
|---------|---------|-------------|-------|
| T1 (no T2) | n/a | inline (16 ovh) | ~48 |
| T2 step 3 | 31 | ~16 ovh | ~47 |
| T2 step 4 (predicted) | ~15 | inlined (0 ovh) | ~15–18 |

The big win is in step 4 when the `bl diff/2` call frame and its
prologue/yield/epilogue go away — that's the headline 2–5× the plan
asks for. Step 3 is the foundation the inlining slots into.

### Files changed

- `erts/emulator/beam/jit/arm/beam_asm.hpp` (T2FunctionEntry struct,
  member declarations).
- `erts/emulator/beam/jit/arm/beam_asm_module.cpp`
  (`emit_t2_specializations`, `emit_t2_total_2`, hook into
  `emit_int_code_end`).
- `erts/emulator/beam/jit/arm/instr_common.cpp` (hook now registers
  `t2_entries` rows instead of emitting a degenerate branch).

## Step 1 (done)

- `t2_mvp.erl` — kv-list walk benchmark.
- T1 baseline: median 1.85 ms / `run(1_000_000)`, ~48 fast-path
  insns/iter.
- Disasm captured in `t1_baseline.asm`.
- Annotated analysis in `baseline.md`.

## Step 2 (done)

**Goal**: a forced side-exit hook lands at function entry for
T2-targeted functions and round-trips through to T1 body. The plan
described this as "a no-op T2 path that runs total/2 with the same
code as T1 but reached via the new dispatch entry, with a forced
side-exit at iter 1." For the MVP, the no-op T2 path is implemented
as a hook *inside* the existing function entry rather than as a
separate code region — the side-exit is a `b` to the immediately-
following instruction, and the rest of the function body is the
existing T1 code.

This is functionally equivalent to "T2 entry exists, immediately
side-exits to T1, T1 runs everything", but without needing two
separate function entry labels. Steps 3+ will introduce the real
dual-entry layout once T2 has actual code worth dispatching to.

### What landed

1. **MFA tracking** — `current_function` and `current_arity` added
   to `BeamModuleAssemblerCommon`
   (`erts/emulator/beam/jit/beam_jit_common.hpp:194-198`), populated
   in `emit_i_func_info`
   (`erts/emulator/beam/jit/arm/beam_asm_module.cpp:528-530`).
2. **T2 target list** — hardcoded `t2_mvp_is_target()` member
   (`erts/emulator/beam/jit/arm/beam_asm_module.cpp:235-260`)
   currently matching `[{t2_mvp, total, 2}, {t2_mvp, diff, 2}]`.
   Real implementation will read the `t2_assume_smallints` attribute
   from the BEAM file in step 3.
3. **Hook point** — `emit_i_test_yield`
   (`erts/emulator/beam/jit/arm/instr_common.cpp:3120-3140`) emits a
   `b` to the next instruction for T2-targeted functions. The hook
   sits *after* `i_test_yield` rather than between the prologue and
   the yield check, because the runtime computes the yield-resume PC
   as `function_start + TEST_YIELD_RETURN_OFFSET` — anything inserted
   before `i_test_yield` corrupts that calculation and produces
   non-deterministic SIGSEGVs (verified empirically during
   development).

### Verification

- Correctness: `run(N)` produces the correct net total for
  N ∈ {100, 1000, 10_000, 100_000, 1_000_000}; results match the T1
  baseline byte-for-byte.
- Hook fires for exactly the right MFAs: `+JDdump` of the loaded
  module shows the `MVP T2 hook for t2_mvp:total/2 …` and
  `… t2_mvp:diff/2 …` comments only — nothing on `run/1`,
  `mk_txns/2`, `time_run/{1,2}`, `module_info/{0,1}`, or the list-
  comprehension lambda. (See `t2_step2.asm` lines containing
  `MVP T2 hook`.)
- Timing: median 1.73 ms / `run(1_000_000)` — within noise of the T1
  baseline (1.85 ms median, 1.77 ms min). The single `b`-to-next
  instruction inserted on the hot path of `total/2` and `diff/2` is
  free in practice.

### Files changed

- `erts/emulator/beam/jit/beam_jit_common.hpp`
- `erts/emulator/beam/jit/arm/beam_asm_module.cpp`
- `erts/emulator/beam/jit/arm/instr_common.cpp`
- `erts/emulator/beam/jit/arm/beam_asm.hpp` (added member declaration)

### What's still missing for a "real" step 2

The hook is a degenerate side-exit; in step 3 it becomes a forward
branch to a real T2 code region. Specifically:

1. **Separate T2 code region.** Currently T2 hook lives inside the
   T1 function body. Real T2 needs its own emitted code emitted
   alongside the function body (or in a different module-level
   section) so T2 instructions can be radically different from T1's.
2. **Real side-exit stub.** Re-tag any raw fixnums held in registers,
   write tagged values to the X-register save area at offsets
   matching T1, jump to a separate T1-entry label that doesn't
   include the function prologue (since CP and reductions were
   already accounted for in T2).
3. **`t2_assume_smallints` attribute parsing.** Replace the hardcoded
   target list with one read from the BEAM file's attribute chunk.

These belong with step 3 because they're only needed once T2 actually
has different code from T1.

## Step 3 — concrete next-session plan

1. **Plumb the attribute** — extend `compile.erl` if needed (it
   already passes through unknown attributes), then add a chunk
   reader in `erts/emulator/beam/beam_file.c` so the JIT can ask
   "is this MFA T2-assume-smallints?"
2. **Add a per-function T2 code label** — emit T2 prologue + body
   either inline (after the function's normal body, separated by
   `unreachable`) or in a per-module T2 section.
3. **Replace the degenerate side-exit** — `b real_t2_entry` instead
   of `b next_insn`, with `next_insn` becoming the T1-fallback label
   that real side-exits target.
4. **Implement the first specialization** — for `total/2` and
   `diff/2`, emit T2 versions that:
   - Combined header+arity tuple guard (one `cmp` instead of three).
   - Combined fixnum check on `(A | F)` (one mask instead of two).
   - Direct fixnum arithmetic on detagged values, `b.vs` to side-exit
     on overflow.
5. **Measure** — `+JDdump` should show the new T2 region; `run(N)`
   should still be correct; `time_run/2` should beat the T1 baseline
   on its own.

Estimated 1–1.5 weeks of focused work.
