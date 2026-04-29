# References

Running bibliography of all primary sources consulted while writing the PLAN/
documents. Add a one-line note for each entry describing what it was used for so
we can re-find context later.

## JavaScriptCore (WebKit)

- [Speculation in JavaScriptCore — Filip Pizlo (2020)](https://webkit.org/blog/10308/speculation-in-javascriptcore/) — primary source for `jscore.md`. Tier hierarchy, value profiling, OSR exit/entry, watchpoints, recompilation strategy, the "value bet" framing.

## LuaJIT

- [LuaJIT homepage — Mike Pall](https://luajit.org/luajit.html) — high-level pitch for the trace compiler.
- [LuaJIT SSA IR (tarantool wiki copy)](https://github.com/tarantool/tarantool/wiki/LuaJIT-SSA-IR) — primary source for `luajit.md`. IR shape, snapshots, SLOAD/sinking, guards, side traces.
- [DeepWiki: JIT Compilation System](https://deepwiki.com/LuaJIT/LuaJIT/2-jit-compilation-system) — secondary overview.
- [LuaJIT/src/lj_record.c (v2.1)](https://github.com/LuaJIT/LuaJIT/blob/v2.1/src/lj_record.c) — source pointer for the trace recorder.

## YJIT (Ruby)

- [YJIT: A Basic Block Versioning JIT Compiler for CRuby — Chevalier-Boisvert et al., VMIL 2021](https://dl.acm.org/doi/10.1145/3486606.3486781) — original paper.
- [Evaluating YJIT's Performance in a Production Context — MPLR 2023](https://dl.acm.org/doi/pdf/10.1145/3617651.3622982) — production data.
- [YJIT documentation in ruby/ruby](https://github.com/ruby/ruby/blob/master/doc/jit/yjit.md) — primary source for `yjit.md`. Thresholds, code GC, observability counters.
- "Simple and Effective Type Check Removal through Lazy Basic Block Versioning" — Chevalier-Boisvert & Feeley, ECOOP 2015 — foundational LBBV paper.

## CPython

- [PEP 744 — JIT Compilation](https://peps.python.org/pep-0744/) — primary source for `python_jit.md`. Copy-and-patch, stencils, tier 2 uops, deopt boundary.
- [PEP 659 — Specializing Adaptive Interpreter](https://peps.python.org/pep-0659/) — referenced; describes the in-place specialization that the JIT consumes.
- [Python 3.13 What's New (cpython/Doc/whatsnew/3.13.rst)](https://github.com/python/cpython/blob/main/Doc/whatsnew/3.13.rst) — confirms the experimental status, build flags.
- [LWN: Improved code generation in the CPython JIT](https://lwn.net/Articles/958350/) — narrative on how the JIT evolved.

## Pyston

- [pyston/pyston — README](https://github.com/pyston/pyston) — current status, history (no longer maintained as a separate distribution; some ideas merged).
- [Contributing the Pyston JIT? — discuss.python.org thread](https://discuss.python.org/t/contributing-the-pyston-jit/24195) — discussion of why CPython did not directly adopt Pyston's design.

## PyPy

- ["Tracing the meta-level: PyPy's tracing JIT compiler" — Bolz, Cuni, Fijałkowski, Rigo (ICOOOLPS 2009)](https://dl.acm.org/doi/10.1145/1565824.1565827) — foundational meta-tracing paper.
- [PyPy Trace Optimizer documentation (rpython.readthedocs.io)](https://rpython.readthedocs.io/en/latest/jit/optimizer.html) — primary source for `pypy.md`. Optimization pass list, dispatcher pattern.
- ["Musings on Tracing in PyPy" — Bolz-Tereick (Jan 2025)](https://pypy.org/posts/2025/01/musings-tracing.html) — *the* recent reflection on tracing-vs-method-JIT trade-offs. Strongly recommends method-based JITs over tracing for well-resourced projects.
- "Allocation Removal by Partial Evaluation in a Tracing JIT" — Bolz et al. — referenced for virtuals/escape analysis.
- "Loop-Aware Optimizations in PyPy's Tracing JIT" — referenced from optimizer docs.

## ZJIT and other Ruby JITs

- [ZJIT launch announcement — Rails at Scale (2025-12-24)](https://railsatscale.com/2025-12-24-launch-zjit/) — primary source for `zjit.md`. SSA HIR, method-level compilation, monomorphic-only v1, side-exit to interpreter.
- [Max Bernstein — ZJIT launch post](https://bernsteinbear.com/blog/launch-zjit/) — engineering details, register-allocator rewrite.
- [Ruby Bug Tracker #21221 — Proposal to upstream ZJIT](https://bugs.ruby-lang.org/issues/21221) — original upstream proposal.
- [Ruby's JIT Journey: From MJIT to YJIT to ZJIT — codemancers](https://www.codemancers.com/blog/rubys-jit-journey) — historical comparison.
- [k0kubun — RJIT: A Pure-Ruby JIT for Ruby](https://k0kubun.medium.com/rjit-a-pure-ruby-jit-for-ruby-f4084f0765) — RJIT design notes.
- [Shopify Engineering — Ruby 3.2's YJIT is Production-Ready](https://shopify.engineering/ruby-yjit-is-production-ready) — YJIT production rollout context.

## HotSpot

- [Introduction to HotSpot JVM C2 JIT Compiler, Part 1 — Emanuel Peter (2024)](https://eme64.github.io/blog/2024/12/24/Intro-to-C2-Part01.html) — IR shape, deopt mechanics, observable behaviour.
- [How Tiered Compilation works in OpenJDK — Microsoft devblog](https://devblogs.microsoft.com/java/how-tiered-compilation-works-in-openjdk/) — primary source for `hotspot.md`. The 5 tiers, threshold formula, tier-up/tier-down policy.
- [Tiered Compilation in JVM — Baeldung](https://www.baeldung.com/jvm-tiered-compilation) — corroborates threshold defaults.
- [JVM JIT-compiler overview — Vladimir Ivanov](https://cr.openjdk.org/~vlivanov/talks/2015_JIT_Overview.pdf) — Oracle internals talk; deeper material if needed later.
- [Understanding OSR in C1 — JITWatch wiki](https://github.com/AdoptOpenJDK/jitwatch/wiki/Understanding-the-On-Stack-Replacement-(OSR)-optimisation-in-the-HotSpot-C1-compiler) — concrete OSR details.

## V8

- [Maglev — V8's Fastest Optimizing JIT (2023)](https://v8.dev/blog/maglev) — primary source for `v8.md`. CFG-SSA vs sea-of-nodes, feedback-driven specialization during graph build, simple linear register allocator.
- [Sparkplug intro (InfoQ summary, 2021)](https://www.infoq.com/news/2021/06/v8-sparkplug-compiler/) — context on the baseline tier.
- [Digging into the TurboFan JIT](https://v8.dev/blog/turbofan-jit) — sea-of-nodes background.
- [V8 holiday season 2023 retrospective](https://v8.dev/blog/holiday-season-2023) — confirms Maglev shipped in M117.

## HiPE (Erlang native compiler)

- [All you wanted to know about the HiPE compiler — Sagonas et al. (Erlang Workshop 2003)](https://user.it.uu.se/~kostis/Papers/erlang03.pdf) — canonical paper; IR layers, mode-mixing, calling convention. (PDF — fetch separately if needed.)
- [HiPE on AMD64 — Luna & Pettersson (Erlang Workshop 2004)](https://erlang.org/workshop/2004/amd64.pdf) — concrete x86_64 calling convention.
- [HiPE overview slides — Sagonas](https://user.it.uu.se/~kostis/Teaching/KT2-12/Slides/HiPE_overview.pdf) — IR-pass diagram, optimization summary.
- [HiPErJiT: A Profile-Driven JIT for Erlang — Kallas et al. (IFL 2018)](https://angelhof.github.io/files/papers/hiperjit-2018-ifl.pdf) — research prototype that *did* add profiling on top of HiPE; relevant prior art.
- [erlang.org — The Road to the JIT (2020)](https://www.erlang.org/blog/the-road-to-the-jit/) — primary source for HiPE's removal rationale and BeamAsm's design philosophy.
- [HiPE removal mailing list thread (June 2020)](http://erlang.org/pipermail/erlang-questions/2020-June/099645.html) — official deprecation discussion.

## Sista (Pharo / Cog)

- [Sista: Saving Optimized Code in Snapshots for Fast Start-Up — Béra (ManLang 2017)](https://rmod-files.lille.inria.fr/Team/Texts/Papers/Bera17b-ManLang-SistaArchitecture.pdf) — primary paper. (PDF — fetch separately.)
- [Clément Béra: An update on the Sista project (2016)](https://clementbera.wordpress.com/2016/06/09/an-update-on-the-sista-project/) — primary source for `sista.md`. Bytecode-to-bytecode design, FullBlockClosure deopt issue, benchmark numbers.
- [Sista open alpha release (2017)](https://clementbera.wordpress.com/2017/07/19/sista-open-alpha-release/) — status snapshot.
- [Cog Blog — About Cog](http://www.mirandabanda.org/cogblog/about-cog/) — Cog baseline JIT design (note: site uses self-signed certificate).
- [Pharo Optimising JIT Internals (slides)](https://www.slideshare.net/slideshow/pharo-optimising-jit-internals/79516042) — talk slides.

## Erlang internals (existing in this repo)

- `lib/compiler/internal_doc/beam_ssa.md` — SSA invariants, exception handling, phi semantics.
- `lib/compiler/internal_doc/ssa_checks.md` — `%ssa%` assertion testing syntax.
- `lib/compiler/src/beam_ssa.hrl` — record definitions for the SSA IR.
- `lib/compiler/src/beam_types.hrl` — type lattice records (`#t_atom{}`, `#t_integer{}`, …).
- `erts/emulator/beam/jit/beam_asm.h` — JIT C entry-point declarations.
- `erts/emulator/beam/jit/beam_jit_common.{hpp,cpp}` — shared assembler base class, codegen pipeline.
- `erts/emulator/beam/jit/arm/beam_asm.hpp` — ARM64 register assignments and ABI.
- `erts/emulator/beam/jit/arm/instr_*.cpp` — per-instruction emitters (the "templates").
- `erts/emulator/beam/jit/beam_asm_module.cpp` — `BeamModuleAssembler::emit` dispatcher; generated `beamasm_emit.h`.
