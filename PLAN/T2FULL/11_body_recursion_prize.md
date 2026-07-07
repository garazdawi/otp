# The body-recursion prize — measurement + technique (2026-07-07)

Decision-grade measurement of what T2 can address on *real* Erlang code, and
how. Built off `lukas/erts/beamjit2` tip `c1785353ab` (P2.5 + P2.6 A+B, gate ON),
isolated worktree, `otp-beamjit2` untouched. Workloads: **dialyzer**
(`--build_plt erts kernel stdlib`) and a **compiler** run (23 stdlib/compiler
sources). Time via `eprof` own-time (tier-independent — the shape distribution is
a property of the Erlang code); shapes via `beam_disasm`, cross-checked against
the gate's own `calls_retained` signal.

## The prize table (time-weighted, not counted)

| bucket | dialyzer | compile | addressable? |
|---|---|---|---|
| **body_recursive** (non-tail self-call) | 25.7% | 15.4% | prize |
| **nontail_helper** (loop/straightline + non-tail call to another fn) | 33.4% | 24.4% | prize |
| pure_loop (tail loop, no non-tail call) | 5.1% | 2.3% | **loop tier already handles** |
| leaf / tail-only | 14.5% | 9.5% | no |
| **PRIZE = body + helper** | **≈59%** | **≈40%** | — |
| — cons-builder subset (comprehension/map/append; TMC-amenable) | ≈7.9% | similar | TMC only |

Tier behaviour: the gate rejects **92–93% of all hot functions** on both
workloads (dialyzer 4 installed / 403 rejected / 26 failed; compile 10 / 615 /
40); **73–74% of rejects carry a non-tail call** (`ret≥1`). Hot rejected
functions are named and real: `erl_types:are_all_limited/2` + `is_limited/2`
alone = **10.8% of the entire dialyzer run** (mutually-recursive tree walkers,
each calling a non-tail helper per iteration); `beam_ssa:rpo_1/4`,
`beam_types:verified_normal_type/1`, `sets:is_element/2` on the compiler.

**Headline:** the loop tier — everything P0–P2 built — reaches only **2–5%** of
these workloads' time; **40–59% is locked behind the non-tail-call barrier**,
and it is hot. The barrier is "*any* non-tail call," not just body recursion —
which is why even genuine tail loops (calling a helper each iteration) are
rejected, and why the prize is this large.

## The realizable-speedup nuance (do not conflate with coverage)

Coverage ≠ speedup. These functions run in **T1** today (the gate keeps them
there), so they pay **no** demote tax now. Bringing the ascent into T2 **at
parity buys ≈0**. The win is entirely the *elimination* delta on top of parity —
unboxing counters, removing tuple-tag guards, and above all **inlining the
non-tail helper into the loop** (the 33% `nontail_helper` bucket is prime
inlining fodder). These are pointer-chasing tree traversals with little
unboxable arithmetic, so per-frame elimination is moderate. **Estimate: ~0% at
bare parity → ~10–25% on dialyzer/compiler-class with full P3 elimination +
helper inlining.** So the prize justifies high priority (it is *the* thing that
makes T2 relevant on real code) but it is a **P3-scale investment, not a quick
win**, and it must be driven by inlining, not bare parity.

## Feasibility — ascent-in-T2 is rung-2, LARGE, no shortcut

Non-tail calls (incl. non-tail self-calls) lower as `mov x30,<T1-cont>; mov
tmp,<callee T1 entry>; br tmp` (`t2_emit.cpp:1351-1384`) — demote-on-return, the
`br` not pushing the return-address stack (the ~3.5 ns mispredict). Loop recovery
only rewrites a *tail* self-call (`t2_loop.cpp:341`). Keeping the ascent in T2
requires **T2 return CPs on the Erlang stack** — which the design currently
forbids ("no return addresses into T2 blobs", `t2_emit.cpp:1342`). This is **not
rung-1-expressible** (framestates are reserved-and-null, `t2_hir.hpp:545`); it is
the plan's **rung-2 = framestates + eager-CP-push (P3)**. Scope **LARGE**:
touches `t2_emit` call lowering, framestate population, `t2_ranges`/`t2_pctab`
(return-CP → MFA/T1 reverse lookup — `ERTS_T2_PC_CONT` already tabulates the
target), a new blob **tombstone + lazy-stack-scan** lifecycle, and T2-CP
awareness in **every runtime CP walker** (stacktrace, exception unwind, purge/
`check_process_code`, hibernate trim, GC). Pitfall: jettison (trace/purge/evict)
while return CPs are live on many process stacks at depth — the correctness-
critical, expensive piece, and why the plan gates P3 before it. **No cheap
parity-only shortcut**: a narrow `bl`-to-T2-entry hack takes the full CP-on-stack
risk for ≈0 reward.

## Technique survey (OCaml / GHC)

- **OCaml Tail Modulo Cons (TMC / destination-passing)** — `[@tail_mod_cons]`,
  OCaml 4.14; turns recursion-under-a-constructor (`H::f(T)`) into a **tail loop**
  by allocating each cons top-down with a hole and filling it next iteration.
  Semantically sound for Erlang's strict cons, BUT needs **runtime heap mutation**
  of the hole. The crux is BEAM's **per-process copying generational GC with
  immutable cells**: the destination is live across an allocating call that can
  move it → needs a GC-safe placeholder, interior-pointer/re-derive rooting, and
  a remembered-set write — i.e. **GC cooperation, and it collides with the
  in-flight large-heap-GC rework**. Best at the **compiler level** (`beam_ssa`) so
  it also helps **T1** (O(1) stack, less GC pressure, upstreamable) and makes the
  comprehension subset loop-tier-eligible — but still needs a "fill destination"
  primitive + GC support. Covers only the **cons-builder ~8%**; does nothing for
  fold/boolean/tree recursion (~51% of the prize).
- **GHC join points** ("Compiling without continuations", PLDI'17) — closure-free
  continuation jumps; the theory behind eager-CP-push (make the return a join
  point, not a mispredicted demote). The rung-2 direction.
- **GHC fusion/deforestation** — eliminate the intermediate list entirely; needs
  laziness + whole-program rewrite rules → not JIT-applicable. Upper-bound only.

## Recommendation

1. **Make ascent-in-T2 / rung-2 the FLAGSHIP of P3** (it is already scoped there;
   the measurement elevates it from "general inlining" to "the precondition for
   T2 mattering on real code"). Drive it through the **inliner** (fuse the loop+
   helper pairs — `are_all_limited`↔`is_limited`, `cerl_trees:fold`↔`cerl:type`),
   because bare parity buys ≈0. Expect ~10–25% on analysis/compiler-class with
   full P3. Do NOT ship a narrow parity hack.
2. **TMC = separate, lower-priority, compiler-level track** for the comprehension/
   map/filter subset. Orthogonal, helps T1, cuts stack/GC pressure — but ~8% only
   and GC-coupled, so spike it independently and sequence it against the
   large-heap-GC work. Not a substitute for rung-2.

**Net:** P2 (loop tier) reaches only 2–5% of real analysis/compiler code; rung-2
(P3) is the precondition for broad relevance — Large, high-risk (stack-walking),
payoff ~10–25% via inlining. The "20% on most apps" goal lives or dies on P3.
