# 21 — Eligibility widening + exceptions: status and next steps

Status memo, 2026-07-16. Consolidates the "bs position, maps, exceptions"
eligibility-widening mandate (step 5 of the general-tier path, doc
[`19`](19_general_tier_path.md)) and the exceptions leg that closed it.
Supersedes the pre-implementation recon in
[`census/eligibility_wins.md`](census/eligibility_wins.md) for status.

## What this phase is (and is NOT)

This is a **coverage** phase: admit more real functions to the tier. It is
NOT a speed phase. The measurement program (memos 14/17/20) established
*eligible ≠ win* twice over — speed comes from P2/P3 + map-shape
specialization, not from admission. So the value here is: (a) functions
whose *only* blocker was a cold construct (a `try`, a record update, a
byte read) can now tier up their **hot body**; (b) the never-slower-than-T1
floor extends to them; (c) the widening exercised code paths that
surfaced — and let us fix — a **latent correctness bug in the shipping
tier** (see §Exceptions).

## Current coverage (measured 2026-07-16, forced-T2 over the base libs)

kernel + stdlib + compiler: **16 577 / 20 181 functions (82.1%) install as
T2 blobs** (forced `T2_INSTALL_GATE=0 T2_RETAIN=1 +JT2enable`). This is a
**function-count** coverage figure, not runtime own-time — the S0 census
(doc [`20`](20_census_results.md)) is the own-time-weighted view (59% json
/ 19.6% mapwl / 30.7% compiler addressable) and is the number that governs
realizable speed. The two are complementary: high function coverage means
few functions are outright rejected; the own-time census says how much of
the *hot* path that coverage reaches.

## Landed (the mandate, all three legs + supporting)

| leg | commits | effect |
|---|---|---|
| type-test guards | `cc63738f4f` | +7 guards (is_float/number/boolean/bitstring/pid/port/reference), +574 fns |
| closures | `2b6d49b002` | inline fun-literal captures (small-guarded) |
| **bs position** | `7863766baa`,`ba18ffc3ff`,`48e3cad221` (+ P-C unroll/SWAR arc `0bf5..e47f`) | byte-aligned scan → cursor-IV; multi-clause scanners; int reads 8..56 bits |
| **maps** | `da77151dda`,`6cf1733ccf` | update_record inline (+2165); single-pair put_map_assoc (+449) |
| **exceptions** | `e54bf63cf3`,`e3305d7fe1`,`d7e4b02c8b` | try/catch Strategy 2 (+421); Y-frame fix; full unlock (+~2044 eligible) |

## Exceptions — the leg that closed the mandate

**Mechanism (Strategy 2, `e54bf63cf3`):** the tier runs the `try` body
itself and reuses T1's registered catch tag. A raise in the body (or a
callee) side-exits to T1, whose `next_catch` finds the tag T2 stored in the
Y slot (`catches` bumped) and unwinds into T1's `try_case`. The handler
block is never a tier-2 CFG edge, so it is dropped as an inert
`ERR_EXIT_SHARED` island — the tier never models the post-unwind exception
triple. Catch tag is captured at load in `patchCatches`, keyed by the BEAM
handler-label ordinal (stable across the `try→catch` transform), threaded
through the retained-code bridge.

**The bug this uncovered + fixed (`e3305d7fe1`) — the real prize.** Error
exits (`badmatch`/`case_end`/`if_end`) side-exit into T1's raise PC with no
sync map, on the theory the predecessors already synced the boundary. True
for X registers, FALSE for the Y-frame: an `init_yregs`'d slot is a dead
`const_nil` in T2's view, so the sync-everything allocator never pins it to
its home, and T1's raise/GC/handler path reads a garbage stack word →
`EXC_ARM_DA_ALIGN`/SIGBUS. It is layout/GC-sensitive (hides under a
debugger). Minimal trigger `erl_error:is_op/2`. **This predates exceptions
— any function with `allocate`+`init_yregs`+early raise was exposed** — the
Strategy-2 `try` just made it deterministic. Fix = a frame-only sync on the
error exit so the allocator materializes every Y slot. This hardened the
whole tier, not just exceptions.

**Full unlock (`d7e4b02c8b`) — fail-closed.** Admits the handler-only
raising ops `raise`/`raw_raise`/`build_stacktrace` to the eligibility scan.
Soundness does NOT ride on the handler drop: the builder has no translate
case for these, so its default arm rejects any *reachable* occurrence (the
whole function stays in T1), while handler occurrences drop with their
island. Even a handler that failed to drop surfaces its raise to the same
default. `try_case_end` deliberately excluded (normal-path op, not a
handler op).

**Validation:** byte-identical T1 vs forced-T2 on a comprehensive
`erl_error` exerciser + badmatch/case_end/if_end in live frames;
`exception_SUITE` 16/16 forced-T2; force-T2 build+run of all 260
kernel/stdlib/compiler modules, no crash.

## CI

The gate that exercises T2 is `.github/workflows/t2-correctness.yaml`
(ASan + debug, forced-T2 miscompile fuzzer over a curated suite slice) —
**PASSED** on the 2026-07-16 push. `main.yaml` runs T2 OFF, so its reds
(Windows/OpenBSD/Solaris builds, clang-format-14 drift, `bif_SUITE`
error-formatting test) are the branch's chronic pre-existing set, confirmed
identical on the prior beamjit2-ci run. Judge T2 changes by t2-correctness,
not main.yaml. (See memory `project_beamjit2_ci_enablement`.)

## Remaining (ranked coverage/effort)

1. **Bare `catch`/`catch_end`** (exceptions increment 2). `catch_end` is on
   the *normal* path (inline, massages THE_NON_VALUE) — a distinct op, not
   a droppable handler. Small–medium; completes exception coverage.
2. **`call_fun` standalone install** (recon WIN 2). Builder already decodes
   it; needs isel+emit+un-build-only. Highest raw coverage (~18% json/
   compiler marginal) but memo 17 flags *eligible-but-flat* — GC/yield/deopt
   at the call is the risk surface, value uncertain. Measure before funding.
3. **general_bif guard-BIF subset** (recon WIN 3): element/hd/tl/map_get/
   is_map_key/map_size/byte_size/… via the stubbed GuardBif op. Elixir map
   idioms; medium.
4. **try-of / `try_case_end`** (normal-path exception op) — a follow-on to
   admit `try … of … catch` functions.
5. **Remaining bs**: multi-read, variable-binary, unaligned; further
   cursor-IV widening (doc [`14`](14_bs_cursor_iv.md)).
6. **Complex maps**: multi-pair `put_map`, exact match, native records.

## Recommendation

The mandate is **complete and shipped** (CI-green on the gate that
matters). The highest-value *follow-on* is not more coverage — coverage is
already ~82% by function count and the own-time ceiling is governed by
*eligible ≠ win*. It is either (a) finish exception coverage cheaply (bare
catch, item 1) for completeness, or (b) return to the strategic question
doc [`19`](19_general_tier_path.md)/[`20`](20_census_results.md) pose:
whether S1 (maps-opcode shape specialization) converts any of this
coverage into realizable speed. Do NOT fund `call_fun` (item 2) on
coverage alone — gate it on a measured win. The one unambiguous win this
phase produced is the Y-frame fix (`e3305d7fe1`): a silent-corruption bug
retired from the shipping tier.
