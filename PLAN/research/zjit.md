# ZJIT — Ruby's New Optimizing Tier (and other Ruby JITs)

Sources:
- [Launch announcement: "ZJIT is now available in Ruby 4.0" — Rails at Scale (2025-12-24)](https://railsatscale.com/2025-12-24-launch-zjit/)
- [Max Bernstein — "ZJIT is now available in Ruby 4.0"](https://bernsteinbear.com/blog/launch-zjit/)
- [Ruby's JIT Journey: From MJIT to YJIT to ZJIT — codemancers](https://www.codemancers.com/blog/rubys-jit-journey)
- [Ruby Bug Tracker #21221 — Proposal to upstream ZJIT](https://bugs.ruby-lang.org/issues/21221)
- [Shopify blog — Ruby 3.2's YJIT is Production-Ready](https://shopify.engineering/ruby-yjit-is-production-ready)
- [k0kubun — RJIT: A Pure-Ruby JIT for Ruby](https://k0kubun.medium.com/rjit-a-pure-ruby-jit-for-ruby-f4084f0765)

ZJIT is **the most directly applicable prior art** for our project. It is
Shopify's Tier-2 optimizing JIT for Ruby, sitting above the existing YJIT
baseline, shipped in Ruby 4.0 (December 2025) but not yet enabled by
default. The project shape is *almost identical* to ours:

| Ours | ZJIT |
|------|------|
| Tier-2 above BeamAsm (template JIT, no IR) | Tier-2 above YJIT (LBBV, no IR) |
| SSA IR consuming BEAM SSA from compiler | New SSA HIR from YARV bytecode |
| Method-level compilation | Method-level compilation |
| Profile-guided types + small-function inlining | Profile-guided types + (limited) inlining |
| asmjit backend, share with BeamAsm | YJIT backend, shared until they outgrew it |
| Side-exit to T1 | Side-exit to interpreter (not YJIT) |

## 1. Why ZJIT exists (vs. just improving YJIT)

The Shopify team's stated rationale:

> "We want to raise the performance ceiling (bigger compilation unit size
> and SSA IR) and encourage more outside contribution (by becoming a more
> traditional method compiler)."

Concretely:
- YJIT's lazy basic block versioning is local to single basic blocks.
  Cross-block optimization is hard. Constant folding can't span method
  boundaries.
- YJIT compiles bytecode directly to machine code with no IR.
  Optimizations are mostly peephole. Adding a new pass means modifying
  the codegen — bad for contribution.
- The YJIT register allocator was a custom design that didn't scale to
  large compiled regions.

So ZJIT *isn't* replacing YJIT — it's additive. YJIT remains the default
in Ruby 4.0. ZJIT is "compiled by default but not enabled by default".
Users opt in via `--zjit`, `RUBY_ZJIT_ENABLE`, or `RubyVM::ZJIT.enable`.

## 2. Architecture

**HIR (High-level IR)** — SSA, method-as-compilation-unit.
- YARV bytecode → HIR.
- Optimization passes operate on HIR.
- HIR knows about Ruby semantics — type guards, send sites, etc.
- DOMJIT-style "open coding" of well-known C methods. `Integer#succ`
  becomes a `FixnumAdd` HIR node, not a call to the C function. This
  exposes the operation to type inference and constant folding.

**LIR (Low-level IR)** — closer to machine ops.
- HIR lowered to LIR.
- LIR fed to a register allocator and code emitter.

**Backend** — initially borrowed from YJIT, currently being rewritten:
> "We are re-writing the register allocator after reading the entire
> history of linear scan papers."

This is the same path Maglev took (greedy linear scan, single-pass).
The honesty here is useful: even with industrial resources, "borrow
the existing baseline JIT's backend" was the right starting point and
"rewrite the register allocator after the rest works" was the right
sequence.

## 3. Optimizations implemented

Per the launch posts:
- Type inference and specialization.
- Constant folding (e.g., the example `def add; one + two; end` folds
  to literal `3` while tracking guards for redefinitions of `Integer#+`,
  `one`, `two`).
- Limited inlining: constants, `self`, parameters, optional parameters
  (in progress).
- C method "inlining" (DOMJIT-style HIR rewrites for built-ins).
- Guard reduction: `GuardNotFrozen` was a C call to `rb_obj_frozen_p`,
  now a load + test + conditional branch inline.

**Not yet implemented**: general method inlining, polymorphic
specialisation (currently monomorphic-only), recompilation on phase
transition. These are explicitly future work.

## 4. Tier-up and profiling

Profiling story is *less* polished than the IR design — the launch
post explicitly defers details. What's documented:
- Compilation triggered by call counts; thresholds similar to YJIT's.
- Type info for specialisation comes from monomorphic call-site
  observation: "we only optimize monomorphic calls right now — cases
  where a method send only sees one class of receiver while being
  profiled."
- Phase-transition handling is on the roadmap. Currently if call-site
  types change after compilation, ZJIT side-exits frequently. Plan: use
  side-exits as additional profile data for recompilation.

The interesting honesty: **monomorphic-only is fine for an MVP**. They
shipped to production traffic at GitHub.com and Shopify with that
constraint. Polymorphic call sites just don't get optimised in the
first version.

## 5. Side-exit / deopt

Side-exits transfer to the **interpreter**, not to YJIT. Critical
because it means ZJIT doesn't have to maintain a frame layout
compatible with YJIT.

> "We gracefully handle the phase transition from integer to string;
> a guard instruction fails and transfers control to the interpreter."

Rationale (inferred): the interpreter is already the "ground truth"
runtime for Ruby; YJIT's frames are interpreter-compatible already;
ZJIT exits all the way back to the slowest tier and lets normal call
counters bring it back up.

For our project, the equivalent decision is: **side-exit from T2 to
BeamAsm-compiled code at a known BEAM SSA point**. We don't have a
true interpreter on aarch64 — BeamAsm *is* T1 — so the parallel is
"side-exit to BeamAsm code at a known BEAM label".

## 6. Frame management — deferred reification

> "Plans to avoid 'flush[ing] local variable state and stack state to
> the VM frame' until actually needed for exceptions or introspection."

This is exactly the Maglev "tagged-region/untagged-region split" idea
in another guise: don't materialise every X-register write to the VM-
visible frame; keep them in machine registers/spill slots, and only
make them VM-visible at deopt or when the runtime needs to see them
(GC, exception, introspection).

We should plan the same. Today, BeamAsm flushes X registers on every C
call boundary; T2 should be able to skip that for analysis-proven-
safe regions.

## 7. Status (Dec 2025)

- Compiled into Ruby 4.0 by default; not enabled by default.
- Coverage: full Ruby test suite, Shopify large-app test suite + shadow
  traffic, GitHub.com test suite, an unnamed bank's systems.
- Performance: faster than the interpreter, **not yet as fast as YJIT**.
  The team explicitly says "ZJIT has a great new foundation and now
  needs to pull out all the Ruby-specific stops to match YJIT."
- Production-ready timeline: not yet specified.

The performance datapoint is *important*: a year of Shopify engineering
gets a new SSA-based optimizer to "almost as fast as the existing LBBV
baseline". This is realistic warning that we should set expectations
accordingly. **The first version of T2 will likely be slower than
BeamAsm in many cases.** It's the foundation that matters; the wins
accumulate over subsequent iterations.

## 8. Lessons for our T2 (the most important section)

Direct lessons. ZJIT is so close to our project that these are
near-prescriptive.

1. **Method-level SSA HIR is the right shape.** Don't do LBBV; don't
   do tracing. Method as compilation unit, SSA HIR consuming BEAM SSA.
2. **Coexist with the baseline, don't replace it.** Ship T2 as
   `--enable-t2` / runtime opt-in. BeamAsm stays the default for the
   foreseeable future. Same model as ZJIT-on-top-of-YJIT.
3. **Side-exit all the way to baseline, not to a peer tier.** No
   tier-2-to-tier-2 deopt; just bail to BeamAsm at a known BEAM SSA
   point.
4. **Borrow the baseline backend first; replace later.** asmjit + the
   per-instruction emit functions in `arm/instr_*.cpp` should be the
   first T2 code generator. Custom register allocation is a v2 task.
5. **Monomorphic-only is fine for MVP.** Polymorphic call sites can
   stay non-optimised. The PIC-equivalent (export-table-mediated
   dispatch with a single observed target) covers the common case.
6. **DOMJIT-style "open code BIFs in HIR".** `length/1`, `tuple_size/1`,
   `element/2`, `is_*/1` guards — instead of emitting a BIF call,
   emit an HIR node that the optimizer can fold/specialise.
7. **Defer frame reification.** X registers stay in CPU regs across
   T2 regions; only flush at GC/exception/deopt boundaries.
8. **First-version performance will be unimpressive.** Don't promise
   benchmarks; promise the foundation. ZJIT shipped at "slower than
   the existing JIT" and that's *normal* for v1.
9. **Inlining comes later.** ZJIT's general-purpose method inliner
   is roadmap, not v1. The constant folding and type narrowing wins
   come *first*; inlining is what unlocks the next tier of wins.
10. **Plan for outside contributions early.** SSA + method-as-unit +
    pass pipeline is hugely more contributor-friendly than a custom
    LBBV codegen. Same applies to ours: writing a new SSA pass in
    Erlang (alongside the existing `beam_ssa_*` modules) is much more
    accessible than modifying `arm/instr_*.cpp`.

## 9. Other Ruby JITs (briefly)

### MJIT (deprecated)
- Original Ruby JIT, Ruby 2.6 (2018) — 3.2 (2022).
- Approach: dump generated C source to disk, invoke external `gcc`/
  `clang`, dlopen the resulting `.so`.
- Slow warmup (literally launches a compiler subprocess), large
  memory footprint, hard to deploy (needs a C compiler at runtime).
- Replaced by RJIT in 3.2.

### RJIT (deprecated, never default)
- Pure-Ruby assembler, no external compiler dependency. Maintained by
  k0kubun as a less-resourced sibling to YJIT.
- x86_64-only.
- Removed/superseded by ZJIT.

### TruffleRuby
- GraalVM-based. Different paradigm: partial evaluation of an
  AST-walking interpreter using Graal as the "self-specialising"
  compiler.
- Achieves substantial speedups (often 5-10× on numeric workloads)
  but requires the entire GraalVM stack and JVM warmup.
- *Not* a model we'd follow — requires a much deeper compiler stack
  and cooperation from the host runtime. Mentioned for completeness;
  not directly applicable.

### Why none of the historical Ruby JITs displaced YJIT

Same lesson as HiPE: **maintenance burden + niche perf wins → death**.
MJIT had a C-compiler dependency at runtime; RJIT was always a
single-maintainer side project; TruffleRuby trades JVM weight for
peak perf and has a separate ecosystem. YJIT won by being deeply
integrated, simple, and demonstrably useful in production. ZJIT will
have to clear the same bar.

The lesson for our T2 is identical to HiPE's: **it has to ship
*useful* improvements early enough to justify its existence**. A
sophisticated SSA-based optimizer that ships at "slower than the
baseline" can survive a year (ZJIT did); it cannot survive several
years.
