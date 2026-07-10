# 16 — Retrospective: what we could have done better, and what to do next

**Date:** 2026-07-10. **Status:** post-landing retrospective (companion to
[`15_scope_and_disposition.md`](15_scope_and_disposition.md), which is the
authoritative disposition). This memo is the after-action review: what T2-Full
turned out to be, where the process could have reached the same conclusion
cheaper, the latent risks still in the tree, and the prioritized closeout. It
records an assessment for the owner to act on — it does **not** itself change
scope; memo 15 already did that.

> **Provenance.** The opcode-eligibility facts in §6 and the appendix are
> verified against `erts/emulator/beam/jit/t2/t2_eligible.c` at HEAD. Line/LOC
> counts and the `bs_scan` disposition are cross-checked against the tree where
> noted; a few figures are flagged **(to confirm)** where they were estimated in
> review and are worth a `cloc`/`git` pass before acting.

---

## 1. Outcome in one paragraph

T2-Full is a profile-driven optimizing second tier for the BEAM JIT
(`lukas/erts/beamjit2`): it reconstructs SSA from loaded BEAM, runs a
BEAM-specific mid-end (HIR → LIR → isel → linear-scan-on-SSA regalloc → asmjit
emit) and installs optimized blobs behind a single prologue-word patch,
aarch64-only. It was chartered for **"≥20 % end-to-end on most applications."**
The M0/de-risk measurement program concluded that goal is **measured-dead** and
landed T2 as a **specialist tier**: 2.5–3.1× on single-clause byte
scan-and-count kernels, ~1.4–2.0× on integer/float tail loops, a
never-slower-than-T1 floor elsewhere, and **~0 % on real services** (memo 14).
P3–P7 (general inlining, classic opts, allocation elimination, x86 port) are
decided-against (memo 15).

---

## 2. Scorecard

**What went well.**

- **M0 "price the pool before funding the phase" discipline paid for itself.**
  It retired the 12–19-week rung-2/P3 build on evidence (memos 12–13) and killed
  P5 before it was built. This is the single most valuable thing the project
  did — it converted a speculative 60-week program into a measured 3-day tier
  plus a decisive negative.
- **The niche it hits, it hits hard and honestly.** scanbench 2.54×/3.10×
  (Apple), 3.71×/16.5× (server-ARM Neoverse), cross-checked, not cherry-picked.
- **Tier core is high quality.** Barrier-disciplined blob freeing, correct
  two-phase retire, a real install gate that restores a T1 floor for losers
  (dialyzer PLT regression +13 % → +3.3 %, memo 10).

**What went badly.**

- **The core value hypothesis was wrong *and* was knowable earlier.** The
  eligible-opcode set (§6) was the load-bearing decision and was not
  pressure-tested against real service code until near the end.
- **Decided-against and diagnostic code is still in the tree** (§5, §7), for a
  tier whose realized value is byte scanners and numeric loops.
- **The evidence isn't fully banked as runnable regressions** — several headline
  numbers live in prose, not in a committed benchmark suite (§5).

Fairness: this was **not** a runaway. The whole P0–P2 tier landed in three days,
the M0 gate genuinely stopped the expensive phases, and the deliverable is a
correct, shippable specialist tier plus a large reusable measurement corpus. The
lesson is about **ordering and framing**, not wasted quarters.

---

## 3. What could have been done better — technical

1. **Regalloc sophistication overshot shipped value.** P2 built a real
   Wimmer linear-scan-on-SSA allocator (`t2_regalloc.cpp`) + validators + deopt,
   but the tier that *ships* is placement-allocated single-path loops with
   effectively zero spills (`t2_compile.cpp:146`). Much of that machinery buys
   headroom for the P3/P4/P5 work that was then cancelled. The HIR/LIR seam
   design is right; the *timing* — building the expensive allocator before the
   value gate — is what to change next time.

2. **The eligible-opcode census should have been the *first* measurement, not
   the last.** `erts_t2_genop_supported` (`t2_eligible.c:48`) excludes
   `call_fun`, all map ops, and binary *construction* — which memo 14 found is
   97–100 % of every profiled service's own-time. A static census of "what
   fraction of hot service own-time uses only supported ops," run on real
   `.beam`, needs no emitter, no regalloc, none of the tier — and would have
   surfaced the service wall in about a day. See §6 for the exact set.

3. **The install gate's win-signals are path-blind histograms — a real, still-open
   soundness gap.** A non-`bs`, multi-clause integer accumulator can still
   over-accept (`t2_compile.cpp:232-239`, and
   [`15_scope_and_disposition.md`](15_scope_and_disposition.md) §8). The general
   fix is cheap and known: require the emitter's already-computed single-path bit
   for any histogram-summed win signal. It was scoped out on a winding-down tier;
   it is the one technical loose end that undermines the never-slower-than-T1
   headline (see §5 P0).

4. **Decided-against / dead weight still wired in.** The A1 byte-class scan-run
   experiment (`bs_scan`, "do not merge as-is" per
   `../../PLAN` memory note) is still declared as a specific op across
   `erts/emulator/beam/jit/arm/ops.tab:1075`, `x86/ops.tab:996`,
   `emu/ops.tab:1281`, with emitters (`arm/instr_bs.cpp:265`) and an emu
   implementation (`emu/bs_instrs.tab:1194`) — the x86 emitter is an explicit
   stub (`x86/instr_bs.cpp:271-277`). Its win already ships via T2's fused scan
   loop, so it is dead weight occupying a specific-op slot on three backends.
   *(Note: this is a JIT/emu **specific-op** slot, not a public external opcode —
   the review's "public opcode 192" framing overstates it. Still worth removing.)*
   Superseded MVP/G prototypes in `arm/beam_asm_module.cpp` are similar dead
   weight **(LOC to confirm)**.

5. **x86 gating is a stub, not a guard.** `erts_t2_enabled()` is arch-agnostic,
   so `T2_RETAIN=1` on x86 can arm counters that can never install (no x86
   emitter). Cheap footgun; add a runtime arch guard.

---

## 4. What could have been done better — process / strategy

1. **The decisive negative was predicted in April, confirmed in July.** The
   pre-T2FULL critique already said Erlang code is branchy / decision-tree
   shaped, cited HiPErJiT losing on branchy Dialyzer, and *prescribed the
   mitigating measurement* (branch-density bucketing + a numeric corpus). That
   measurement was not run first.

2. **The go/no-go MVP validated the specialist niche and was read as validation
   of the general tier.** The MVP hit ~1.97× on a synthetic integer monomorphic
   small-int accumulator — precisely the slice T2 ultimately shipped as. A green
   MVP on the one shape that always wins is weak evidence for a
   "20 %-on-most-apps" mandate; the go/no-go criterion should have required a
   branchy/service **negative control**.

3. **By end of June all three thesis pillars had thin/missing pools — the
   strategic verdict was answerable before most of the build.** Elimination
   ~7.3 % on dialyzer / 0.4 % on json; sinkable allocation ~0 % (the top
   allocator is BIF-internal C, not JIT-addressable); addressable share 24–34 %
   with "20 % is not reachable on this workload; 5–15 % is." The goal doc itself
   already conceded 20 % unreachable on services.

4. **Methodology was mostly sound** (best-of-9×2, pinned scheduler, warm-up,
   server-ARM cross-check), with two caveats worth remembering: the rung-2 value
   delta is *estimated, not measured* (it would require building the inliner),
   and the `tsum` / `lex_wl` benchmark **sources are not in the tree** (results
   in prose only), so a few headline numbers are not independently reproducible.

---

## 5. Latent risks still in the tree

- **Install-gate soundness hole** (multi-clause int accumulator over-accept):
  possible sub-T1 install, currently unmeasured. Cheap fix; see P0.
- **Decided-against `bs_scan` specific-op** occupying slots on three backends
  with a stubbed x86 emitter.
- **x86 arms cost with no possible benefit** (no `T2_RETAIN` arch guard).
- **Diagnostics not production-gated:** receive-path counters and any `galloc`
  raw-`malloc` usage outside ERTS allocation accounting must be stripped/gated
  before any merge **(scope to confirm)**.
- **Hardening lives on unmerged branches.** The aarch64 gate, sanitizer/fuzzer
  CI, and the server-ARM bench leg are **not ancestors of HEAD** — the safety
  net exists but isn't wired into the shipped branch.

---

## 6. The eligible-opcode set (verified — grounds §3.2 and answers "which ops?")

A function is T2-eligible iff **every** generic op in its body is in the
supported set (`erts_t2_genop_supported`, `t2_eligible.c:48`), with two ops
additionally argument-checked: `bs_match` (byte-aligned subset only,
`erts_t2_bs_match_check`) and `bif2` (comparison subset only,
`t2_bif2_op_supported`). One unsupported op anywhere in the body drops the whole
function to T1.

**Supported** (the switch, verbatim categories):

- *Scaffolding:* `int_func_start`, `int_func_end`, `int_code_end`, `label`,
  `line`, `executable_line`.
- *Register / stack-frame:* `move`, `swap`, `init_yregs`, `allocate`,
  `allocate_heap`, `deallocate`, `trim`, `test_heap`.
- *Fun creation:* `make_fun3` only — **`call_fun` is explicitly unsupported**
  (intrinsics consume constant funs; anything that actually *calls* a fun stays
  T1; `t2_eligible.c:71-73`).
- *Calls / returns:* `call`, `call_last`, `call_only`, `call_ext`,
  `call_ext_last`, `call_ext_only`, `return`.
- *Control flow:* `jump`, `select_val`, `select_tuple_arity`.
- *Guards (type tests / comparisons):* `is_integer`, `is_atom`, `is_nil`,
  `is_list`, `is_nonempty_list`, `is_tuple`, `test_arity`, `is_tagged_tuple`,
  `is_lt`, `is_ge`, `is_eq`, `is_ne`, `is_eq_exact`, `is_ne_exact`.
- *Tuples / lists:* `get_list`, `get_hd`, `get_tl`, `get_tuple_element`,
  `put_list`, `put_tuple2`.
- *Arithmetic (generic arith lowers to these):* `gc_bif1`, `gc_bif2`,
  `gc_bif3`.
- *Clause-failure exits:* `badmatch`, `if_end`, `case_end`.
- *Byte-aligned binary **matching** subset:* `bs_start_match3`, `bs_match`
  (byte-aligned commands only: `ensure_at_least`, byte-sized unsigned-big
  `integer`, `skip`, `get_tail`; ≤1 destination), `bs_test_tail2`,
  `bs_get_tail`.
- *Comparison `bif2` subset:* value-producing `erlang:` `>=` `<` `=<` `>` `=:=`
  `=/=` only (`t2_bif2_op_supported`, `t2_eligible.c:301`). Arith equality
  `==`/`/=` is excluded (routed through T1's generic `i_bif2` C call).

**Not supported** (everything else falls through to `return 0`) — the load-bearing
exclusions, i.e. what makes real services ineligible:

- **`call_fun` / `call_fun2`** — higher-order calls (closures invoked as
  callbacks): the single biggest service exclusion.
- **All map operations** — `get_map_elements`, `put_map_assoc`/`put_map_exact`,
  `is_map`, `has_map_fields`, `update_map_*`, `new_map`.
- **Binary *construction*** — `bs_create_bin`, `bs_init*`, `bs_put_*` (only the
  match/scan side is in; building binaries is out).
- **Non-byte-aligned / non-trivial binary matching** — bit-unaligned reads,
  multi-byte integers, floats, utf8/16/32, signed/little-endian flags, ≥2
  destinations (rejected by `erts_t2_bs_match_check`).
- **Exceptions** — `catch`, `try`, `try_case`, `raise`, `build_stacktrace`,
  `raw_raise`.
- **Receive / messaging** — `loop_rec`, `loop_rec_end`, `recv_mark`/`recv_set`,
  `wait`, `wait_timeout`, `timeout`, `send`.
- **`apply` / `apply_last`.**
- **General BIF calls** — `bif1`, `bif2` (except the comparison subset above),
  i.e. any non-GC BIF.
- **Float-register instructions** — `fload`/`fstore`/`fadd`/`fsub`/`fmul`/
  `fdiv`/`fnegate`/`fconv`/`fcheckerror` (float arithmetic that doesn't route
  through `gc_bifN`).
- **Most type tests** — `is_map`, `is_binary`, `is_bitstring`, `is_number`,
  `is_float`, `is_boolean`, `is_function`/`is_function2`, `is_pid`, `is_port`,
  `is_reference` (only the six listed above are in).
- **`on_load`** functions are detected and refused (`on_load_out`).

The shape this admits is exactly: **single-/few-clause functions doing integer/
float/comparison arithmetic, tuple/list access, direct calls, and byte-aligned
binary scanning** — i.e. numeric tail loops and byte scanners. It excludes the
map-, closure-, and binary-construction-heavy code that dominates real service
own-time — which is precisely why memo 14 measured ~0 % on services.

---

## 7. What to do next (prioritized)

**P0 — close correctness / hardening loose ends (~1 wk, high payoff, low risk).**
- Generalize the install gate to require the emitter's single-path bit for any
  histogram-summed win signal (`t2_compile.cpp:152`, reuse `admit_scan_loop`),
  and add the multi-clause-accumulator over-accept regression test.
- Merge the aarch64-gate and correctness-CI (sanitizer/fuzzer) branches into the
  working branch so the safety net is actually an ancestor of HEAD.
- Add the x86 `T2_RETAIN` runtime arch guard.
- Bank the phash2 parity harness + scanbench/tsum/lex_wl as a committed CT
  suite, so the headline numbers are reproducible.

**P1 — strip decided-against / diagnostic weight (~1 wk, low risk).**
- Remove or fully gate the A1 `bs_scan` specific op (arm/x86/emu `ops.tab`,
  emitters, `emu/bs_instrs.tab`) — its win already ships via T2's fused scan.
- Delete the superseded MVP/G prototypes in `arm/beam_asm_module.cpp`.
- Gate/strip the receive-path diagnostic counters and any raw-`malloc` `galloc`
  usage.

**P2 — decide disposition explicitly.**
- **(A) Keep as a specialist tier behind `+JT2enable` — RECOMMENDED, and already
  the owner decision (memo 15).** Cost = the P0/P1 cleanup. Real 2.5–3.1× on
  scan/count + numeric loops, guaranteed floor, opt-in, no default-path risk.
- **(B) Archive-and-extract** (~2 wk): pull the portable wins that help **T1
  too** (byte-scan fusion as a T1/compiler improvement; the measurement corpus
  as standalone assets), park the tier. Strong second choice.
- **(C) Broaden the frontend for services** — larger than the whole rung-2
  build, uncertain payoff, high risk. **Do not fund** without running the
  opcode-eligibility census (§6) on real service `.beam` first.
- **(D) Retire entirely** — only if neither the niche nor the substrate has a
  consumer; the measured 2.5–16× argues against it.

**P3 — redirect the *strategy*, not just the tier.** Every measurement points
the same way: on real services the cycles are in **VM-internal work
(message/signal path, ETS, term copying) and GC**, not JIT-addressable Erlang.
That is where the other active tracks already live (large-heap-GC,
auto-hibernation, process-sharing / inline handoff). Recognize T2 as a
**kernel/parser accelerator** and migrate the "20 % on most apps" ambition to
the VM-internal / GC program.

**Single recommendation:** take **Option A** — ~2 weeks of P0/P1 hardening +
cleanup, ship T2 opt-in with memo 15 as the honest scope statement, close the
install-gate hole, and explicitly move the broad-speedup ambition to the GC /
VM-internal track. The measurement program already did the hard, correct work of
proving what T2 *is*; the remaining job is to make the tree reflect that
conclusion and not carry decided-against weight or an unguarded floor into
whatever ships.
