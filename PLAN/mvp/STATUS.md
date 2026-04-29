# T2 MVP Implementation Status

Last updated: 2026-04-29 (session in which step 1 landed).

## Steps from `T2_mvp.md`

| Step | Status | Artifact |
|------|--------|----------|
| 1. Benchmark + T1 baseline | ✅ done | `t2_mvp.erl`, `t1_baseline.asm`, `baseline.md` |
| 2. Side-exit round-trip | 🔍 scoped, not started | — |
| 3. SSA → asmjit codegen | ⏳ blocked on 2 | — |
| 4. Inlining of `diff/2` | ⏳ blocked on 3 | — |
| 5. Measurement + writeup | ⏳ blocked on 4 | — |

## Step 1 (done)

- `t2_mvp.erl` — benchmark module with `total/2`, `diff/2`, `mk_txns/2`,
  and a `time_run/2` harness using `erlang:monotonic_time/1`.
- T1 baseline: median 1.85 ms / `run(1_000_000)`, ~1.85 ns / iter, no
  GC during the hot loop.
- `t1_baseline.asm` — full `+JDdump` output for the module on
  aarch64-apple-darwin25.4.0.
- `baseline.md` — annotated per-iteration disassembly (~48 fast-path
  insns), with the win sources T2 needs to capture mapped to specific
  insn ranges.

## Step 2 — scoped but not implemented

The right hook point for T2 dispatch is `emit_i_breakpoint_trampoline`
in `erts/emulator/beam/jit/arm/beam_asm_module.cpp:231`. Each function
entry calls this exactly once after the public function-entry label
has been bound, so it's where T2 prologue code can be prepended.

Key constraints discovered:

1. **`BEAM_ASM_FUNC_PROLOGUE_SIZE` is a hard 12 bytes on aarch64**
   (`erts/emulator/beam/jit/beam_asm.h:127`). The trampoline emits
   exactly 3 instructions and there's a runtime assertion at
   `beam_asm_module.cpp:253` that the prologue is exactly that size.
   Prepending T2 dispatch requires either bumping this constant
   (rippling into NIF loading at `erl_nif.c:5133`, trace patching, and
   the `BEAM_ASM_NFUNC_SIZE = PROLOGUE_SIZE + 4` constant) or threading
   a per-function "expected prologue size" through the assertion.

2. **MFA tracking does not exist in `BeamModuleAssemblerCommon`**
   (`beam_jit_common.hpp:130`). It tracks the module atom (`mod`) but
   each function's `(F, A)` is only known transiently inside
   `emit_i_func_info`. Step 2 needs a `current_mfa` field set in
   `emit_i_func_info` and consumed by `emit_i_breakpoint_trampoline`.

3. **Side-exit register layout** is straightforward in principle:
   re-tag `R(N)` and `R(net)` (or whatever raw fixnums T2 holds), write
   to the X-register save area at offsets matching T1, then `b` to T1
   entry. For step 2 specifically — a no-op T2 path that always exits
   on iter 1 — the side-exit can be the entire T2 prologue: just
   "fall through to T1 entry" with no prior state mutation.

## What step 2 actually needs to validate

Per `T2_mvp.md`:

> "A no-op T2 path that runs total/2 with the same code as T1 but
> reached via the new dispatch entry, with a forced side-exit at iter
> 1. Verifies frame-layout compatibility and round-trip correctness."

Concretely: when `bl @total/2-entry` is executed, control should land
in T2 code (a new region), which should then transition cleanly to
T1's existing function body. Output of `run(1_000_000)` must match
the T1 baseline exactly (`-12005005`-ish for the test sum, byte-equal
to T1).

## Concrete next-session plan for step 2

1. **Add MFA tracking** — `current_mfa` field in
   `BeamModuleAssemblerCommon`, populated by `emit_i_func_info`.
2. **Add hardcoded T2 target list** in `beam_asm_module.cpp` — for the
   MVP, this is `[{t2_mvp, total, 2}, {t2_mvp, diff, 2}]`. Threading a
   real `t2_assume_smallints` attribute through the compiler is
   deferred to step 3.
3. **Refactor the prologue size constant** to allow per-function
   override. Two options:
   - **A**: Extend `BEAM_ASM_FUNC_PROLOGUE_SIZE` to 16 unconditionally
     and absorb the 4-byte cost on every function. Simpler. The cost
     is one extra `nop`/skip on every function entry — measure it.
   - **B**: Replace the assertion with a per-function "expected size"
     and update NIF/trace consumers to read it back from
     `ErtsCodeInfo`. More invasive.
4. **Emit T2 prologue** for matched MFAs: a single instruction that's
   the side-exit (e.g. `b t1_continue` where `t1_continue` is bound
   immediately after). For the no-op step 2 this is literally a `b`
   that skips over zero bytes — an instruction the assembler or the
   branch predictor can elide entirely.
5. **Verify** by re-running `run(1_000_000)` and comparing the result
   byte-for-byte against the T1 baseline output, then re-dumping
   `t2_mvp.asm` and confirming the new prologue appears for `total/2`
   and `diff/2` only.

Estimated 2–3 days of focused work, dominated by getting the prologue
size refactor right. After step 2 lands, steps 3 and 4 can be much
more incremental: the codegen file (e.g. `arm/t2_codegen.cpp`) is
additive, and each new BEAM-op specialization can be tested
independently.

## Why stopping here is the right call

- Step 1 stands on its own as a measurable artifact: a benchmark, a
  baseline, and a per-instruction analysis of where T2 wins live.
- Step 2 has been scoped concretely enough that picking it up later
  has no rediscovery cost — the hook point, the constraints, and the
  decision points are documented above.
- A half-implemented step 2 that breaks the build or silently
  miscompiles is worse than no step 2.
