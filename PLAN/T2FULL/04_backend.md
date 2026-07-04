# T2-Full — Backend Evaluation and Decision

> The mandate left the toolkit open ("asmjit, another toolkit, or
> write our own assembler"). This file is the evaluation. Decision:
> **asmjit + our own BEAM-specific mid-end**, with a hard HIR→LIR
> seam as the hedge. Evidence gathered July 2026.

## 1. The structural question that decides everything

Before comparing toolkits: **which of our required optimizations
could a toolkit's mid-end actually provide?** The list is inlining,
GVN, LICM, unrolling, escape analysis/allocation sinking, plus the
BEAM-specific set (guard elimination, type speculation on tagged
terms, `test_heap` coalescing and re-placement, match-context
registerization, loop recovery of tail recursion, the atomic
fast/slow guard-BIF lowerings of
[`../T2/04_optimization.md`](../T2/04_optimization.md) §10.7).

Walking the list:

- **Inlining** must understand the Erlang calling convention,
  reduction accounting, monomorphic-slot profiles, and feed loop
  recovery — even Cranelift's new inliner (Wasmtime 36, Nov 2025)
  delegates the *decision* to an embedder callback because policy
  is always language-specific.
- **LICM/unrolling** must respect sync points, framestates, and
  effect-free re-execution windows — hoisting across a deopt point
  is a BEAM-correctness question invisible to any generic pass.
- **GVN/CSE** pays off mainly on *our* primitive ops (tag tests,
  `element/2`, untag/tag pairs); a backward-chained CSE walk is
  ~a week of work (LuaJIT and Maglev both do exactly this).
- **Escape analysis / allocation sinking** operates on Erlang heap
  allocation and deopt-snapshot materialization — LuaJIT-style,
  inseparable from our deopt model; no toolkit has it.

The controlled experiment for this question is JSC's B3: WebKit
replaced LLVM under an existing speculative frontend and got
**parity to +11.6 % performance at 4.7× faster compilation**
([B3 launch post](https://webkit.org/blog/5852/introducing-the-b3-jit-compiler/))
— LLVM's mid-end contributed nothing net once the language-specific
tier had done its work, while costing most of the compile budget.

**Conclusion: we write the mid-end regardless.** The toolkit
question reduces to instruction selection + register allocation +
encoding + code-buffer management.

## 2. The matrix

| Dimension | asmjit + own mid-end | LLVM ORC | Cranelift | MIR |
|---|---|---|---|---|
| Code quality (int/pointer server code) | on us; B3/MIR prove small pipelines reach ~90 %+ of heavyweight backends | best (reference) | ~14 % behind LLVM ([Cranelift docs](https://github.com/bytecodealliance/wasmtime/blob/main/cranelift/docs/compare-llvm.md)) | ~91 % of GCC -O2 ([MIR README](https://github.com/vnmakarov/mir)) |
| Compile latency / function | µs–low ms (BeamAsm-like emission + one linear-scan pass) | **~100 ms typical, seconds worst** (Azul Falcon) — disqualifying vs our 1 ms median / 10 ms cap | sub-ms–few ms (~10× faster than LLVM) | ~250–340 µs small fns |
| aarch64 maturity | proven — BeamAsm ships on it | mature (JITLink default since 2024) | first-class (regalloc2, ISLE) | supported, lightly battle-tested |
| Deopt / side-exit support | we build it (framestate metadata, planned in [`../T2/04_optimization.md`](../T2/04_optimization.md) §11) | patchpoints/stackmaps perpetually "experimental"; WebKit abandoned them; Azul carries a fork | stack maps are GC-rooting only; **no deopt primitives**, no production speculative-JIT embedder | none |
| Mid-end provided (of our list) | none — by design, all ours | all generic, none BEAM-aware | egraph GVN/const-fold/LICM; **no unrolling**; inlining off-by-default, embedder-policy | inlining, GCSE, LICM, CCP; no unrolling/escape |
| License (OTP = Apache-2.0) | Zlib, already vendored | Apache-2.0-w/-exception, OK | Apache-2.0-w/-exception, OK | MIT, OK |
| Build integration | zero delta | huge dependency + annual major-version churn (PostgreSQL's routine LLVM-N build fixes; Julia's decade of latency engineering) | **Rust + cargo in the ERTS autoconf build; no official C API** ([open issue since 2019](https://github.com/bytecodealliance/cranelift/issues/1293)) | single C11 library, trivial |
| 10-year maintenance risk | on the OTP team — but the same competency BeamAsm already demands | version-churn treadmill | healthy org; Rust dependency permanently narrows OTP's platform set | **bus factor ≈ 1** (Makarov); v1.0.0 May 2024, quiet since |
| Incremental adoptability | full control | poor, all-or-nothing | good *if* the IR has a clean LIR seam (see §4) | moderate |

## 3. Per-candidate verdicts

**asmjit + own mid-end — chosen.** Calibration that the scope is
bounded: B3+Air went from prototype to shipping-at-LLVM-parity in
~4 months including a 1.3 KLOC graph-coloring allocator; ZJIT
(Shopify, several engineers, ~1 year) shipped an SSA HIR + LIR +
backend and in May 2026 adopted **linear scan on SSA form (Wimmer)**
— exactly the algorithm the reference design planned in
[`../T2/04_optimization.md`](../T2/04_optimization.md) §11.2 — for
the same reasons (method-level compilation + inlining)
([ZJIT regalloc post](https://railsatscale.com/2026-05-27-a-new-register-allocator-for-zjit/));
V8's Maglev ships meaningful wins with a single-pass allocator and
deliberately no heavyweight opts; MIR proves one engineer can build
the whole pipeline (23.4 KLOC, 91 % of GCC -O2). The regalloc
verdict across every fast mid-tier surveyed (Maglev, ZJIT, LuaJIT,
MIR, Air's cheap tier): **linear scan or simpler**; graph coloring
appears only at peak tiers. Our sync-point pinning composes
naturally with linear-scan intervals and would fight a coalescing
graph-coloring allocator. Residual risk — it's all on the OTP
team — is mitigated by it being the same skill set BeamAsm already
requires, and by the HiPE lesson: what killed HiPE was the
*parallel* backend+runtime universe, whereas T2 shares BeamAsm's
stack, calling convention, and assembler.

**LLVM ORC — rejected** on compile latency (Falcon ~100 ms/method
typical; Julia and PostgreSQL both engineered for years to *avoid*
invoking LLVM at runtime; Postgres ships JIT effectively off by
default), deopt support (patchpoints still experimental a decade
on), and version churn. Erlang prior art seals it: ErLLVM (2012)
required custom LLVM patches and never shipped.

**Cranelift — best external option, wrong first move.** Genuinely
attractive (production-hardened, first-class aarch64, ~10× faster
than LLVM at ~14 % quality cost), but: no deopt primitives and no
production speculative-JIT embedding to copy; no unrolling and
embedder-driven inlining (both ours anyway); Rust-only API with the
C API request open since 2019 — making Rust+cargo a hard ERTS build
dependency would fork the tier's platform availability. Revisited
at the x86_64 port decision (§4).

**MIR — right size, wrong fit**: bus factor ≈1, no deopt, no
sync-point register contract. Its value is as the calibration
datapoint for our own scope.

**libgccjit** (GPL + full-GCC-pipeline latency), **GNU lightning**
(beta, strictly weaker than vendored asmjit), **copy-and-patch**
(baseline-tier technique; BeamAsm already occupies that slot with
better code) — all out.

## 4. The decision, with hedges

1. **HIR→LIR hard seam** (ZJIT-shaped). Every BEAM-semantic pass —
   inlining + loop recovery, speculation, GVN, LICM, unrolling +
   `test_heap` re-placement, escape analysis/sinking — lives in HIR
   and survives any backend change. LIR's contract is exactly
   "isel + regalloc + encode + framestate metadata", and no HIR
   pass may depend on asmjit types. If x86_64 porting effort or
   regalloc quality ever justifies it, a Cranelift-based (or
   TPDE-style) LIR consumer replaces emission without touching the
   mid-end.
2. **Linear scan on SSA (Wimmer) with sync-point pin constraints**,
   per [`../T2/04_optimization.md`](../T2/04_optimization.md)
   §11.2, now backed by unanimous mid-tier precedent. If quality
   evidence later demands more, B3's 1.3 KLOC IRC shows the upgrade
   is bounded.
3. **Reuse BeamAsm's emission substrate**: global fragments,
   `emit_enter/leave_runtime`, veneers, `JitAllocator`, perf/gdb
   metadata registration — T2 is a second client of the existing
   machinery, not a second machinery.
