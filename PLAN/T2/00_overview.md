# T2 — Overview

> Part of the T2 design. See [`README.md`](README.md) for the full
> document index. This file covers §§1–4: goals, motivation,
> architecture diagram, and the prior-art-vs-departures choices.

## 1. Goals and non-goals

### Goals

- Add a *second tier* of JIT compilation, sitting **above BeamAsm**, on
  **aarch64** (Apple Silicon + Linux ARM64 first; x86_64 later).
- The compilation unit is **a function**, not a trace. Method-based.
- The IR is **CFG-based SSA**, defined and manipulated in **C++**
  alongside the existing BeamAsm code in `erts/emulator/beam/jit/`.
- Generate native code **directly via asmjit**, reusing BeamAsm's
  global runtime fragments (GC entry, exception handling, scheduler
  re-entry).
- Apply optimizations driven by **profile-narrowed type information**
  and **inlining of small functions** — including the higher-order
  helpers in `lists` when called with a literal fun.
- **The outer function matches T1's abstract machine state at every
  sync point** — function entry, calls, returns, GC sites, BIF
  boundaries, speculation guards, tracing-relevant points, receive
  safe points. *Between* sync points, the register allocator is free
  to keep SSA values in CPU registers without flushing to T1's X/Y
  layout. Speculation failures in the outer function jump directly to
  T1 code at the corresponding BEAM instruction boundary; the outer
  function carries lightweight stackmap-style metadata at each sync
  point recording the live X-reg map. (See §6.)
- **Coexist** with BeamAsm. T2 is opt-in and additive; turning it off
  affects neither correctness nor T1 performance.
- Inherit T1's calling convention, reductions accounting, GC
  discipline, and tracing primitives **verbatim**. Divergence here
  only inside well-bounded inlined regions, never at boundaries.
- Keep the implementation **small**: target ~20 KLOC of new C++ and
  Erlang combined, reusing BeamAsm and existing BIF/runtime code as
  much as possible.
- Target compilation cost ~1 ms per function.
- **Hard floor: T2 must not ship slower than T1, including the
  aggregate steady-state tax from profiling and tier-up
  infrastructure.** v1 is the foundation; the optimization wins
  accumulate over subsequent iterations (cf. ZJIT shipping at
  "slower than YJIT", `research/zjit.md` §7). For Erlang, a T2
  that's slower than T1 in v1 is dead in the water — the measurement
  bar is "comparable to T1 baseline at worst, faster on
  profile-narrowable hot paths". Aggregate workload performance
  under T2-enabled mode must not regress more than **3%** vs T1-only
  mode on any tracked benchmark — that's the budget for the
  profiling, eligibility-check, and counter overhead taken
  together. Phase 0 measurement (see `07_delivery.md` §17 audits)
  sets the production target.

  The honest framing on what kind of code T2 wins on: **today's
  Erlang corpus is dominated by branchy state-machine code**
  (pattern-matched dispatch trees, gen_server callbacks, protocol
  handlers). T2's measurable wins on that corpus come predominantly
  from cold-arm pruning and DOMJIT-style guard elimination
  (`04_optimization.md` §10.7) — not from inlining or loop
  optimization, which are real but apply to a smaller fraction of
  production code. The numeric/loop wins are an *expansion target*
  (see §2 below): if T2 closes the gap to NIF-level performance on
  numeric Erlang, code that today lives outside Erlang (NIFs, C
  ports, other languages) becomes plausible Erlang. The MVP under
  `../T2_mvp.md` already demonstrates ~2× on a numeric microbenchmark
  with hand-written T2 codegen.

### Non-goals

- A "FTL-equivalent" T3 with whole-program LLVM-quality optimization.
  T2 is the only new tier; T3 is explicitly future work.
- Tracing JIT semantics. Method-based.
- Replacing BeamAsm. T1 stays the default and the source of truth for
  every BEAM op's semantics.
- **No** polymorphic specialisation in v1. Monomorphic call sites
  only at first; PIC-style switch over multiple receivers is roadmap.
  (Note: inlining a higher-order helper like `lists:foldl/3` with a
  literal-fun argument is *not* polymorphic — the fun is statically
  known via constant propagation. That case *is* in v1; see §10.)
- No general OSR-entry mid-loop in v1. **OSR-exit is in v1** because
  tracing requires it (§9, §12.5). OSR-entry is roadmap.
- x86_64 in v1. The architecture should not preclude it; first
  target is aarch64 only.
- Cross-module deep optimization that requires invalidation broader
  than what code purge already handles. T2 dependency tracking is
  per-blob (§14).

## 2. Why T2

The current BeamAsm JIT is a template-style code generator with no
inter-instruction optimization. It pays its way (full coverage, fast
load-time compilation, ARM64 + x86_64 support, no mode-mixing
complexity), and per "The Road to the JIT" it was a deliberate choice
to not chase peak throughput in the baseline tier.

**The Erlang AOT compiler already does the standard SSA optimization
set** (`beam_ssa_type`, `beam_ssa_dead`, `beam_ssa_cse`,
`beam_ssa_alias`, `beam_ssa_destructive_update`, `beam_ssa_float`,
…). A T2 that merely re-runs these passes with runtime types would
yield modest gains. The case for T2 turns on what the AOT compiler
*cannot* do:

1. **Cross-module inlining**. The AOT operates per-module. Code like
   `lists:map(fun(X) -> X * 2 end, L)` compiles to a closure-driven
   loop inside `lists:map/2` with no visibility into the closure.
   T2 can see both at runtime and merge them, eliminating closure
   allocation, the indirect call, and exposing the loop body to
   type specialisation.
2. **Type-driven speculation beyond static proof**. The AOT must
   treat exported function parameters as `any`. A running system
   sees stable, narrow types at almost every hot call site. T2
   speculates on those types and unlocks the optimizations that
   follow.
3. **Specialised arithmetic**. Once T2 knows operands are small
   integers in a safe range, it emits the same native sequence
   BeamAsm uses for the small-int fast path — but without the
   per-op type guard, because the guard moved to a single
   `speculate_range` at the previous BEAM instruction boundary.
   Inside inlined loops with constant-known funs, this collapses
   to native loops.

The core argument: **inlining is the core value**, with type
speculation as the enabler. Anything T2 does in the outer function
*without* inlining will be modest (the AOT already did most of it).
Anything T2 does *across* an inlined boundary is novel.

### 2.1 Alternatives considered

Three simpler alternatives we considered before committing to T2:

1. **Default-on `inline_list_funcs` in the AOT compiler.** The
   compiler already has hand-written Core Erlang expansions for
   the 10 lists higher-order BIFs (`sys_core_fold_lists.erl`).
   Turning the option on by default would capture some of the
   wins T2 promises at zero runtime cost.
   *Why insufficient*: it covers only one specific module (lists)
   with a hand-curated set; doesn't extend to user-defined
   higher-order helpers; can't see runtime types so can't drive
   the unboxing/specialisation that produces the bulk of the win.
   But this *is* useful in its own right and worth shipping
   alongside or before T2 — it's strictly additive.

2. **Richer AOT type information + better static specialisation.**
   The AOT compiler could emit a "guarded specialised" version
   of a function (one for `is_integer` arg, one for `is_list`,
   with a type-dispatch entry). Ahead-of-time speculation; no
   runtime profiling required.
   *Why insufficient*: parameter types of exported functions are
   generally `any` — the AOT cannot tell which type to specialise
   on. Static dispatch over multiple specialisations doesn't
   actually solve the problem; it just multiplies the code-size
   cost across all possibilities. T2's profile-driven approach
   targets exactly the type that's hot in this deployment.

3. **Profile-guided AOT.** Erlang programs typically run a
   profiling phase, then re-deploy. A profile-guided AOT could
   compile with profile data instead of doing it at runtime.
   *Why insufficient*: the deployment workflow (profile, then
   recompile, then redeploy) is much heavier than runtime
   adaptation; doesn't handle phase changes during a single
   process's lifetime; doesn't deal with hot-code upgrade where
   the next module is unknown at compile time. A useful tool to
   add separately, but not a substitute for runtime tier-up.

We chose T2 because none of these alternatives covers the
cross-module-with-runtime-types case that produces the largest
expected wins. Several of them remain worth doing alongside T2 —
they're complementary, not competing.

## 3. Architecture in one diagram

```
                ┌──────────────────────────────────────────────────┐
                │  Erlang source                                   │
                │   ↓ AOT compiler                                 │
                │  BEAM file                                       │
                │    + type info (beam_ssa_type)                   │
                │    + SSA chunk (NEW — gated by compile option)   │
                │    + jit_inline annotations (NEW)                │
                └──────────────────────┬───────────────────────────┘
                                       │ load-time
                                       ▼
        ┌──────────────────────────────────────────────────────────┐
        │  BeamAsm (T1) — template JIT                             │
        │   - per-instruction emitters                             │
        │   - asmjit codegen                                       │
        │   - exports addressv[active_code_ix] dispatch            │
        │   - **NEW**: per-function call counter (eligible only)   │
        │   - **NEW**: per-function type feedback vector (eligible)│
        │   - eligibility check at module load — only emit         │
        │     profiling for functions whose ops are all in the     │
        │     supported phase set (§17)                            │
        └──────────────────────┬───────────────────────────────────┘
                               │ counter trips
                               ▼
        ┌──────────────────────────────────────────────────────────┐
        │  JIT server (Erlang process under kernel/code)           │
        │   - receives tier-up requests from BeamAsm               │
        │   - deduplicates, queues, applies eligibility rules      │
        │   - dispatches compile job to a dirty CPU scheduler      │
        │   - installs resulting blob, patches the function        │
        │     prologue (NIF-style) to redirect into T2             │
        └──────────────────────┬───────────────────────────────────┘
                               │ dispatch
                               ▼
        ┌──────────────────────────────────────────────────────────┐
        │  T2 compile (runs on a dirty CPU scheduler)              │
        │   - reads SSA chunk + type feedback vector               │
        │   - runs T2 optimizer (C++)                              │
        │   - returns a code blob to the server                    │
        └──────────────────────┬───────────────────────────────────┘
                               │
                               ▼
        ┌──────────────────────────────────────────────────────────┐
        │  T2 optimizer (C++, in erts/emulator/beam/jit/)          │
        │   1. Build T2 IR from BEAM SSA records                   │
        │   2. Type inference + narrowing                          │
        │   3. Insert speculate_type/speculate_range from feedback │
        │   4. Inline hot call sites (constant-fun + jit_inline)   │
        │      - inlined regions emit per-region deopt stubs from   │
        │        codegen-time framestate metadata (eager CP-push at │
        │        region entry; uniform deopt stub regardless of     │
        │        nesting depth — see §9.2)                          │
        │   5. CSE / DCE / guard reduction / const folding         │
        │   6. Loop recovery + LICM + unrolling (with test_heap    │
        │      coalescing)                                         │
        │   7. Direct lowering to asmjit                           │
        └──────────────────────┬───────────────────────────────────┘
                               │
                               ▼
        ┌──────────────────────────────────────────────────────────┐
        │  T2 blob installed                                       │
        │   - separate code cache from BeamAsm                     │
        │   - function prologue at L_f+4 patched (single 4-byte    │
        │     store, NIF-style) to redirect into the T2 entry stub │
        │   - watchpoints registered for inlined dependencies      │
        │   - T1 body left intact: it's both the side-exit landing │
        │     zone and the in-flight-caller "finish naturally"     │
        │     fallback. Uninstall = revert the patched `b`.        │
        │   - intra-module calls (direct relative branches baked   │
        │     at AOT time) hit the prologue too, so they redirect  │
        │     into T2 with no code-gen-side change. See            │
        │     `06_dispatch_and_sideexit.md` §2 for details.        │
        └──────────────────────────────────────────────────────────┘
```

The key insight: **T2 outer-function code matches T1's abstract
machine state at every sync point** — function entry, calls,
returns, GC sites, BIF boundaries, speculation guards, tracing-
relevant points, receive safe points. T2 entries can be called by
T1 callers. T2 can return into T1 callers. Speculation failures in
the outer function jump to T1 at the corresponding BEAM instruction
boundary. *Between* sync points the register allocator has freedom;
this is the central optimization opportunity (§6). No mode-switch.

**Memory-ordering note.** Cross-thread state shared between T2,
T1, the JIT server, and the schedulers (the prologue patch on
blob install, the watchpoint table, the per-call-site profile
slots, and the per-process tombstone scan) reuses the same
release/acquire and thread-progress patterns BeamAsm and the
existing code loader already employ for atomic module upgrade
(this is the same machinery NIF loading uses to patch the
prologue). There is no T2-specific memory-ordering protocol —
inheriting T1's approach is a hard architectural commitment.
Phase 0 audits the points and confirms the pattern reuse is
mechanical.

## 4. Choices

| Decision | What we choose | Reference / rationale |
|----------|---------------|---------------------|
| Compilation unit | **Method-level** | ZJIT, Maglev, JSC-DFG; Bolz-Tereick says trace JITs aren't worth it given resources. |
| IR style | **CFG-based SSA** | Maglev explicitly chose this over sea-of-nodes for cache friendliness. V8 abandoned sea-of-nodes in favour of CFG. |
| IR identity | **New T2 IR in C++** | Lives next to BeamAsm; one-to-one with BEAM SSA ops in Phase A, augmented with speculation/unboxing ops. |
| Optimizer language | **C++** | Co-located with BeamAsm; achievable ~1 ms compile target; avoids cross-language IR marshalling. |
| Backend | **Direct IR → asmjit, reusing global fragments** | Same path BeamAsm uses; sharing GC entry / exception / scheduler fragments. |
| State preservation | **Outer function = T1 X/Y layout at sync points only** | The register allocator is free between sync points (function entry, calls, returns, GC, BIF boundaries, speculation guards, tracing, receive). Speculation failure in outer = jump to T1 at the corresponding BEAM instruction; only inlined regions need framestates. (§6) |
| Tier-up trigger | **Per-function call counter** | JSC's single-counter scheme; only emitted for tier-2-eligible functions. |
| Profile data | **Type feedback vector with `BeamTypeId` bitmask per slot** | Slots at function entry, before arith, on call return, on switch arg; bitmask of seen types over saturating count. |
| Speculation model | **Speculate when monomorphic; deopt at instruction boundaries only** | `speculate_type` + `speculate_range` *before* the operation, never mid-arithmetic. |
| Deopt target | **T1 BeamAsm code at the same BEAM instruction** | Same calling convention, same X/Y layout — direct jump, no reconstruction. |
| Inlining v1 | **Monomorphic + constant-fun-target + jit_inline-annotated helpers + tail-recursion → loop recovery** | ZJIT's v1; Sista's monomorphic case. Constant-fun case handles `lists:foldl(F,...)` with literal `F`. |
| Higher-order intrinsics | **AOT-annotated `jit_inline`, initial set = the `sys_core_fold_lists.erl` 10 BIFs** | Reuses existing AOT pattern; evolves to library-author opt-in and AOT auto-inference. |
| Loop optimizations v1 | **Recovery + LICM + unrolling (factor 4–8) with `test_heap` coalescing** | IV analysis is weak for Erlang's list-walking idioms; unrolling pays off via heap-test coalescing and term-creation batching. |
| Polymorphic v2 | **PIC-style switch with N≤4 receivers** | Sista's polymorphic case; JSC's PICs. Roadmap. |
| Recompilation | **Exit-counted with exponential backoff** | JSC's `100 * 2^R` model; HotSpot does the same. |
| Watchpoints | **For module reload, fun reload, BIF rebinding** | JSC's invalidation model. Hooks in `code:purge/1`. |
| Coexistence | **Opt-in, off by default** | ZJIT's deployment model; matches "don't break BeamAsm" goal. |
| Architectures | **aarch64 first, x86_64 later** | aarch64 is where we want it most (Apple Silicon + ARM cloud); x86_64 follows once architecture-agnostic patterns are proven. |
| Code cache | **Separate region from BeamAsm, evictable** | YJIT's bounded budget + GC pattern. |
| Compilation thread | **Erlang JIT server process under `kernel/code` + dirty CPU scheduler for the actual compile work** | Co-ordinator is an Erlang process (queueing, dedup, blob install); the C++ optimizer runs as a dirty CPU job so it doesn't block normal schedulers. |
| Eligibility | **Counters and feedback emitted only for functions whose BEAM ops are all in the supported set** | Zero-overhead for code T2 can't compile anyway. |
| Compile-time target | **~1 ms per function** | Concrete budget; gates pass selection. |

