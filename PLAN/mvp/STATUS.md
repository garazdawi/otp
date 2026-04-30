# T2 MVP Implementation Status

Last updated: 2026-04-30 (step 2 landed).

## Steps from `T2_mvp.md`

| Step | Status | Artifact |
|------|--------|----------|
| 1. Benchmark + T1 baseline | ✅ done | `t2_mvp.erl`, `t1_baseline.asm`, `baseline.md` |
| 2. Side-exit round-trip (infrastructure) | ✅ done | hook in `arm/instr_common.cpp`, `t2_step2.asm` |
| 3. SSA → asmjit codegen | ⏳ next | — |
| 4. Inlining of `diff/2` | ⏳ blocked on 3 | — |
| 5. Measurement + writeup | ⏳ blocked on 4 | — |

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
