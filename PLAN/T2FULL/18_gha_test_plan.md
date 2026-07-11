# 18 — GHA test plan for the T2 JIT (2026-07-11)

> **Question this memo answers.** The beamjit2 branch now passes the standard
> OTP CI matrix on GHA (run 29141118524, 2026-07-11: every code leg green;
> the four remaining reds are infra — Windows configure, OpenBSD fd ulimit,
> Solaris cc1 segfault, OSSF fork permissions). But *passing* is not
> *covering*: which T2 layers does that matrix actually execute, which does
> it merely compile, and in what order do we close the gap? This memo is the
> plan of record for T2 test coverage on GHA.

## 0. TL;DR

- **Today's matrix executes almost no T2 code.** T2 is off by default
  (`T2_RETAIN=1` / `+JT2enable` opt in); the backend emits aarch64-only while
  every hosted test leg is x86_64. The only T2 execution in CI is the
  arch-independent mid-end via `t2_g1_SUITE` (reconstruction fidelity on a
  `T2_RETAIN=1` peer), confirmed actually running — not skipping — in the
  green x86 emulator leg. No selftests, no isel sweep, no installs, no
  tier-up, no native T2 blob ever runs.
- **The safety net is already built and validated — it just isn't an ancestor
  of HEAD** (retrospective 16 §5). `lukas/otp/aarch64-ci` went fully green on
  2026-07-09 with (a) an additive **ARM leg in main.yaml** (build + curated
  test subset on `ubuntu-24.04-arm`) and (b) the **T2 codegen-correctness
  leg**: debug emulator + `+JT2enable true` + `T2_INSTALL_GATE=0` turns the
  OTP suite corpus into a T2 miscompile fuzzer, with a ≥500-installs floor
  probe against vacuous greens.
- **Plan: four waves, port-not-rebuild.** W1 port the correctness fuzzer onto
  beamjit2 (hard gate). W2 port the ARM main.yaml leg (non-blocking pass 1).
  W3 bank the x86-cheap coverage (selftest boot, build+isel sweep CT case,
  x86 `T2_RETAIN` guard). W4 deepen (organic tier-up CT case, parity suites,
  asan variant, widen slices, promote ARM to blocking).
- **One known bug sits on the critical path:** `+JT2enable` trips the
  `module.c:142` `ERTS_LC_ASSERT` (compile-at-load calls `erts_get_module()`
  off a scheduler thread at boot). W1 starts by reproducing/fixing it on
  current HEAD.

## 1. Layer inventory — what "cover the T2 JIT" means

A green CI must exercise each layer below on the widest arch it can run on,
with an explicit non-vacuousness probe wherever "did nothing" would also pass.

| # | layer | exercised by | arch reach |
|---|---|---|---|
| L1 | build & link, all types (opt/debug/asan, 32-bit, cross, clang/gcc) | standard matrix | everywhere *(done — this was the 07-10/11 fix wave)* |
| L2 | mid-end selftests: HIR round-trip, ranges, loop (`T2_SELFTEST=1`) | boot-time selftest | arch-independent |
| L3 | emit structural selftest (`T2_EMIT_SELFTEST=1`) | boot-time selftest | real on aarch64, stub elsewhere |
| L4 | SSA reconstruction fidelity vs AOT (`t2_g1_SUITE`) | emulator test leg | arch-independent *(already running on x86)* |
| L5 | build+isel sweep over a corpus (`T2_RETAIN=1 T2_BUILD=1 T2_ISEL=1`), build_fail == 0 | not in CI | arch-independent |
| L6 | forced install + execute every eligible function (`+JT2enable true`, gate off) | not in CI | **aarch64 only** |
| L7 | organic tier-up: counters → queue → async worker → install → execute (`T2_RETAIN=1`) | not in CI | install aarch64 only; counters arm everywhere (§5 risk) |
| L8 | debug/lock-check invariants under T2 (`cerl -debug`, jit flavor) | L6/L7 legs on TYPE=debug | aarch64 |
| L9 | memory safety under forced T2 (TYPE=asan) | not in CI (boot-env bug, §5) | aarch64 |
| L10 | perf ratios (scanbench/G-bin/tier-up tax) | `t2-arm-bench.yaml` (armci) | aarch64; **signal, not a gate** |

## 2. What GHA covers today (HEAD `ff50c215c2`, run 29141118524)

| leg | T2 content | verdict |
|---|---|---|
| 64/32-bit, clang, cross, iOS, FreeBSD, macOS builds | compiles T2 mid-end everywhere; emitter gated to aarch64 | L1 ✔ |
| Types & Flavors (emu, jit) | debug/lock-check *boot* with T2 built in (t2_tier_queue lock order regression-covered) | L1/L8-boot ✔ |
| Test emulator (x86_64) | `t2_g1_SUITE` runs for real: peer probe passed, and under the old aarch64-only gate this suite failed loudly on x86 (see 2983b661e7), so a green leg ⇒ it ran | L4 ✔ |
| Test compiler/debug/system, docs | no T2 execution (T2 off by default) | — |
| everything else | no T2 | — |

Gaps: **L2/L3 selftests never run in any leg; L5 sweep never runs; L6/L7 —
the actual tier: compile, install, run native T2 code — never happens
anywhere on GHA.** A T2 codegen bug that assembles is invisible to today's
CI.

## 3. Validated-but-unmerged assets (port these, don't rebuild)

Validation reference: branch `lukas/otp/aarch64-ci` @ `cd347cb437`, all-green
push run 2026-07-09 20:03 (main.yaml 1h14m ✔, T2 codegen correctness 22m ✔).

- **`t2-correctness.yaml`** (f6c1e2dc8b + fixup e065636af8, plus jit support
  commit 4cc80c1038 "build + run under sanitizer/assertions"): ubuntu-24.04-arm,
  fork-gated; opt bootstrap + `TYPE=debug` emulator; install-count floor probe
  (≥500 blobs, else the run fails as vacuous); curated erts+stdlib T2-dense
  slices (`num_bif/bif/bs_match_int/bs_match_misc/bs_construct/binary`;
  `lists/binary_module/string/maps/re/base64/unicode`) under
  `ERL_AFLAGS="+JT2enable true"` + `T2_INSTALL_GATE=0`; junit + ct-log + asan
  artifacts; single result-gate step. asan variant selectable but boot-blocked
  (§5).
- **ARM main.yaml leg** (b513871140): additive `arch: [x64]` dimension +
  fork-gated arm `include` on build, build-flavors, test, system-test;
  `continue-on-error` pass 1; arch-keyed base-image/prebuilt caches (cross-arch
  poisoning fixed); arch-suffixed artifacts; de-hardcoded Dockerfiles
  (multiarch triplets). x86_64 path byte-identical.
- **`t2-arm-bench.yaml`** (armci branch): stays a bench harness —
  `continue-on-error` by design, ratios to the Apple OUTCOME numbers. Do not
  promote to a gate; L10 only.

**Do NOT port:**

- `5699450f3b` "gate T2 to aarch64 only" (`ERTS_ENABLE_JIT_T2`) — superseded
  by HEAD's opposite, now CI-validated pivot: build the tier everywhere, gate
  only the emitter/bridges (de87b4f48f + 4f3e06f5eb).
- `2983b661e7` "skip t2_g1_SUITE when tier-2 is not built in" — obsolete under
  build-everywhere; on HEAD the suite must (and does) *run* on x86. Keep its
  lesson: the peer probe must distinguish "T2 absent" loudly, never
  silent-skip on an arch where T2 is expected.
- `15141cc737` bs_scan 32-bit fix — HEAD has the wider fix (55b8190c73);
  verify overlap at port time, take HEAD's.

## 4. The waves

Each wave = small functional commits on `lukas/erts/beamjit2`, validated by a
push to `origin/lukas/erts/beamjit2-ci` (origin **only**), babysat to green
before the next wave.

**W1 — the correctness fuzzer (highest value: it executes native T2 under
assertions).**
1. Reproduce the `module.c:142` `ERTS_LC_ASSERT` under `+JT2enable true` on
   current HEAD (native aarch64, `cerl -debug -emu_flavor jit`); fix the
   off-scheduler `erts_get_module()` call (or bound it: take the module lock
   path used by the async worker). This is a real product bug in
   compile-at-load, not just a CI blocker.
2. Port `4cc80c1038` (sanitizer/assert build support) and
   `t2-correctness.yaml`; push filter = `lukas/erts/beamjit2` +
   `lukas/erts/beamjit2-ci`; keep the ≥500 install floor, `TYPE=debug`
   default, fork-only guard.
3. Gate: blocking. It was green on 2026-07-09; a red here is a real T2 bug.
   Cost ~25 min on the free arm runner.

**W2 — the ARM main.yaml leg (makes the *standard* suite set run where T2 is
real).**
1. Port `b513871140` (matrix + caches + Dockerfiles). Pass 1 stays
   `continue-on-error`, curated subset (clang build; jit flavors opt/debug;
   emulator, stdlib, kernel tests).
2. On this leg `t2_g1_SUITE` runs on aarch64 (L4 on the arch where emit is
   real), and the debug jit flavor boots the full lock-check under T2 (L8).
3. Gate: non-blocking pass 1; promote in W4.

**W3 — bank the x86-cheap layers into the standard matrix (no new runners).**
1. L2/L3: run the selftests in an existing leg — either a boot step
   (`T2_SELFTEST=1 T2_EMIT_SELFTEST=1 erl -noshell -eval halt()` must print
   pass; x86 exercises the stubs' no-op contract) or, better, a CT case in a
   new `t2_smoke_SUITE` so every platform that runs emulator tests gets it.
2. L5: sweep CT case — `T2_RETAIN=1 T2_BUILD=1 T2_ISEL=1` peer, load the
   corpus modules (boot set + stdlib slice), assert build_fail == 0 and
   lowered > 0. This banks the retrospective P0 item "committed CT suite" for
   the structural half.
3. x86 `T2_RETAIN` arch guard (retrospective §5 latent risk: counters arm,
   cost with no possible win) + a regression test that `T2_RETAIN=1` on x86
   is a clean no-op (t2_stats present, installs == 0, no crash).

**W4 — deepen, then tighten.**
1. Organic tier-up CT case (L7): hot self-recursive loop pinned to one
   scheduler trips the counter; assert `t2_tier_stats` installs ≥ 1 and the
   result stays correct. Hard gate on the ARM legs; skips (loudly) on x86.
2. Bank the parity harnesses as CT (retrospective P0: phash2 parity +
   scanbench/tsum/lex_wl) — parity hard-gates, ratios informative.
3. asan: fix the arm boot env (prim_tty re stack-check + inet ets
   system_limit at kernel start), then add/switch the correctness leg's asan
   variant (L9).
4. Widen the fuzzer slices toward full emulator+stdlib (+compiler) as cost
   allows; promote the W2 ARM leg to blocking after a green streak.

## 5. Known bugs / risks on the critical path

- **`module.c:142` `ERTS_LC_ASSERT` under `+JT2enable`** (open, flagged during
  the CI-enablement wave): blocks W1 on TYPE=debug. Note the 2026-07-09
  correctness run was green on debug + `+JT2enable` on `aarch64-ci`'s code —
  so either the assert path is newer than that branch or it triggers only in
  boots that run there and not here. Reproduce first; don't assume.
- **asan boot on `ubuntu-24.04-arm`** fails before tests start (prim_tty re
  stack-check + inet ets system_limit) — why `debug` is the W1 default; asan
  is W4.
- **x86 `T2_RETAIN` counters** arm with no possible install (retro §5) — W3
  closes it and pins it with a test.
- **Vacuousness** is the standing failure mode of "enable a flag in CI": every
  T2 leg keeps an explicit floor probe (installs ≥ N, lowered > 0, selftest
  banner present). A leg that can pass while executing nothing is not
  coverage.
- **Runner economics:** `ubuntu-24.04-arm` hosted runners are free only for
  public repos; every ARM/T2 job keeps `if: github.repository ==
  'garazdawi/otp'` so nothing runs (or bills) if the branch reaches
  erlang/otp. QEMU (the local `t2ci` container) stays a compile-check tool
  only; runtime repro happens native.

## 6. Acceptance criteria — "CI covers the T2 JIT" means

Every push to `beamjit2-ci` demonstrates, with hard-failing gates:

1. standard x86_64 matrix green, `t2_g1_SUITE` *running* (L1, L4);
2. mid-end selftests + build/isel sweep green in the standard matrix (L2, L5,
   x86 stubs of L3);
3. ARM build + selftests green with the real emitter (L1, L2, L3);
4. ≥500 forced T2 installs exercised by the curated suites under a debug
   emulator with zero assertion/asan findings (L6, L8);
5. organic tier-up installs and runs at least one function correctly (L7);
6. no T2 leg is vacuous (every one carries its floor probe).

Bench ratios (L10) remain tracked but never gate.

## 7. Rollout discipline

Wave-by-wave; each wave lands as small compile-clean commits with the
CI-wired linters run locally before push (shellcheck on workflow scripts,
license header, format checks). Fixup commits over amend/rebase while a wave
is being validated on the ci branch. Push target is `origin` (garazdawi
fork) exclusively — never upstream — and the ci branch is fast-forward from
the work branch.
