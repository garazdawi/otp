# HotSpot — C1 / C2 Tiered Compilation

Sources:
- [How Tiered Compilation works in OpenJDK — Microsoft devblog](https://devblogs.microsoft.com/java/how-tiered-compilation-works-in-openjdk/)
- [Introduction to HotSpot JVM C2, Part 1 — Emanuel Peter (2024)](https://eme64.github.io/blog/2024/12/24/Intro-to-C2-Part01.html)
- [Tiered Compilation in JVM — Baeldung](https://www.baeldung.com/jvm-tiered-compilation)
- [JVM JIT-compiler overview — Vladimir Ivanov (2015)](https://cr.openjdk.org/~vlivanov/talks/2015_JIT_Overview.pdf)
- [Understanding OSR in C1 — JITWatch wiki](https://github.com/AdoptOpenJDK/jitwatch/wiki/Understanding-the-On-Stack-Replacement-(OSR)-optimisation-in-the-HotSpot-C1-compiler)

HotSpot is the canonical tiered JIT: 25+ years of production tuning, two
hand-written compilers, an OSR mechanism, deopt, all in one VM. It is the
reference architecture for "two-compiler tiered" designs and the model JSC
and V8 trace their ancestry to.

## 1. The five tiers

| Tier | Compiler | Profiling | Notes |
|------|----------|-----------|-------|
| 0 | Interpreter | Yes (slow path) | Where everything starts. |
| 1 | C1 | None | Compiled trivial code; no instrumentation. |
| 2 | C1 | Limited (invocation/backedge counters) | Used when C2 queue is full. |
| 3 | C1 | Full (MDO type profiles) | Normal path before C2. |
| 4 | C2 | None (consumes profile) | Server-quality optimization. |

A method's progression is normally `0 → 3 → 4`, with `1` and `2` as
back-pressure relief when C2 is overloaded:

> If the C2 compile queue is too long, the method is sent to T2 instead
> of T3 — that is, run with cheaper instrumentation while waiting — to
> keep latency low.

## 2. Threshold formula

Default scaling:
```
if (Executions > Tier3InvocationThreshold * scale)
   OR (Executions > Tier3MinInvocationThreshold * scale
       AND Executions + Iterations > Tier3CompileThreshold * scale)
   then promote
```

Default raw thresholds:
- Tier3 invocation: 200
- Tier4 invocation: 5 000 (or 15 000 combined invocations + back-edges)
- Backedge counters trigger OSR with their own thresholds.

The `scale` factor adjusts with code-cache fullness and CompilerThread
load; this is the equivalent of JSC's `M / (M - U)` and HotSpot does the
same dynamic-load-balancing trick.

## 3. Profiling — the MDO

Each method has a **Method Data Object** (MDO) attached, populated by
T0 (interpreter) and T3 (instrumented C1). The MDO records:
- per-call-site **type profiles** (observed receiver classes, with a
  capped number of slots per site — typically 2–4, the rest collapsed
  into a "polymorphic" bucket).
- **branch frequency** counters per `if`.
- **null-check failure** flags.
- **array store** type observations.
- **exception** counters (for `try/catch` predicate hoisting).

C2 reads the MDO during compilation. It is the *only* source of profile
data — there is no continuous profiling once C2 has compiled the method.

## 4. C1 — the client compiler

C1 has its own pipeline: bytecode → HIR (high-level IR, SSA-ish) →
optimization passes → LIR (low-level IR, machine-code-like) → register
allocator → machine code. It does:
- linear-scan register allocation,
- inlining of trivial accessors and small leaf methods,
- some constant folding and dead code,
- intrinsics for hot library methods (e.g. `Math.sqrt`),
- *insertion of profiling code* in T2/T3 mode.

C1 is roughly 10× faster than C2 to compile and produces code roughly 30%
slower than C2. It exists primarily to (a) get something compiled fast at
startup and (b) collect profile data for C2.

## 5. C2 — the server compiler

C2's IR is **sea-of-nodes** — a graph where nodes represent operations
and edges represent both data dependencies and control dependencies (with
control edges making the graph not a true DAG). The IR is *very* flexible
but slow to traverse and cache-unfriendly. Both V8's TurboFan and Maglev
have moved away from sea-of-nodes (TurboFan retains it; Maglev rejected
it explicitly — see `v8.md`).

Pass list (rough, not exhaustive):
- parse + initial graph build
- inlining (CHA-driven, see below)
- escape analysis
- loop optimizations (unrolling, peeling, range-check elimination)
- global value numbering / CSE
- dead code elimination
- iterative GVN until fixpoint
- conditional constant propagation
- lowering to LIR
- graph-coloring register allocation (Chaitin–Briggs)
- code emission

C2 compile times are non-trivial (tens to hundreds of ms per method).

## 6. Speculation and uncommon traps

C2 emits **uncommon traps** at speculation points:
- Implicit null check: no explicit branch; if a SIGSEGV occurs at this
  PC, the signal handler reads the trap descriptor and deopts.
- Branch-never-taken: the unobserved branch becomes an uncommon trap.
- Type-never-seen-here: virtual call sites monomorphic in profile become
  direct calls + a type check that traps if the receiver class differs.
- Class-hierarchy-analysis (CHA): "interface I has a single
  implementation X" allows C2 to inline X's method directly. If a second
  implementation is loaded later, all dependent C2 blobs are invalidated
  via the dependency tracker.

When a trap fires:
1. Capture all SSA values that are live at the trap point (the C2
   compiler emits a "scope description" recording where each Java local
   and stack value lives in registers/spill slots).
2. Reconstruct an interpreter frame from the scope description.
3. Resume in the interpreter at the bytecode index the trap recorded.
4. Update the MDO with the violation so the next C2 compile knows.

Repeated traps in the same method exponentially back off recompilation
(same idea as JSC's `R` counter).

## 7. OSR

C2 compiles a method as a *standalone* OSR entry when the *backedge*
counter trips (i.e. there's a hot loop in a method that has not itself
been called enough). The OSR-compiled blob has a special entry point
that:
- accepts an interpreter frame,
- copies all live Java locals into the right registers,
- jumps into the loop body.

OSR-compiled blobs are typically *only* used for the in-flight loop;
once the function call returns and is re-invoked normally, C2 will
emit a non-OSR blob for the next call.

## 8. Code cache

A single fixed-size memory region (default 240 MB on modern HotSpot,
split into segments for non-profiled / profiled / non-method code).
Eviction is triggered when the cache fills; uncached methods fall back
to interpreter and re-compile later. Sweeping is incremental.

## 9. CompilerDirectives

User-side mechanism to override per-method compilation policy: force
compile-level, force inline, exclude, dump IR. Directives are JSON files
loaded via `-XX:CompilerDirectivesFile`. Useful in production diagnosis
when a specific method is being deopt-bouncing.

## 10. Lessons for an Erlang T2

- **Two-compiler tiering is well-validated.** BeamAsm-as-T1 + a new T2
  is the same shape as C1+C2.
- **MDO-equivalent matters.** A T2 needs structured per-method profile
  data — type profiles per call site, branch frequencies, null-check
  flags. We need to plan storage and aging.
- **Uncommon traps are the right deopt model.** Generic stub + scope
  descriptions, not per-trap hand-written code.
- **CHA-equivalent for Erlang** is non-obvious. Modules can be reloaded
  and new behaviours can implement existing ones at runtime. Code purge
  hooks already exist; we'd extend them to invalidate T2 blobs.
- **Sea-of-nodes is *not* a default**. Maglev/JSC-DFG abandoned it for
  good reasons. We should plan a CFG-based SSA — much more cache-
  friendly, easier to debug, simpler register allocator.
- **C2 compile-time cost is real.** HotSpot mitigates it with
  asynchronous compilation on dedicated threads. Erlang already has a
  scheduler with dirty CPU schedulers — the T2 compiler thread is a
  natural fit.
- **OSR matters less than you'd think.** HotSpot uses it heavily because
  Java has hot loops in `main`/test runners. Erlang's hot loops are
  almost always inside functions called millions of times — the
  "compile on call count" path covers most cases. OSR is worth doing
  but need not be in the MVP.
- **Recompilation backoff is essential.** Phase changes happen.
- **Maintenance cost is the existential threat.** HotSpot has a
  multi-decade engineering investment behind it. Anything we build
  that's bigger than ~10 KLOC in C++ needs an explicit maintenance
  story or it will rot — exactly the path that killed HiPE.
