# CPython JIT (PEP 744) and Pyston

Sources:
- [PEP 744 — JIT Compilation](https://peps.python.org/pep-0744/)
- [PEP 659 — Specializing Adaptive Interpreter](https://peps.python.org/pep-0659/)
- [Python 3.13 What's New](https://github.com/python/cpython/blob/main/Doc/whatsnew/3.13.rst)
- [LWN: Improved code generation in the CPython JIT](https://lwn.net/Articles/958350/)
- [pyston/pyston README](https://github.com/pyston/pyston)

The CPython JIT is interesting as a counter-example: a deliberately *minimal*
JIT chosen for ease of build, maintainability, and platform portability rather
than peak performance. It's also the most recent production JIT shipped for a
mainstream dynamic language and has thought hard about cost-of-maintenance,
which is something that killed HiPE and that we have to take seriously.

## 1. Tier 1 vs Tier 2 in CPython

CPython since 3.12 has had a *specializing adaptive interpreter* (PEP 659):
- Bytecode instructions self-rewrite into specialised variants based on
  observed types. `LOAD_ATTR` rewrites itself into `LOAD_ATTR_SLOT`,
  `BINARY_OP` into `BINARY_OP_ADD_INT`, etc. This is "Tier 1".
- The specialised instructions form the input to the JIT.

Tier 2 is a *uops* (micro-ops) representation. A specialised Tier 1
instruction expands into multiple uops, each performing a smaller, more
uniform piece of work. Tier 2 has its own interpreter (always built),
which handles trace execution before/instead of the JIT.

The JIT is added on top: "compile the optimized uops trace into machine
code via copy-and-patch". As of writing, the JIT in CPython 3.13 is
roughly performance-parity with the uops interpreter — the win is the
infrastructure, not the steady-state speedup yet.

## 2. Trace projection

A trace is a linear sequence of uops collected at runtime. Trace start is
typically a hot loop or function entry; trace end is at a back-edge, an
unrecorded operation, or after a fixed length.

Specialisation data from PEP 659 drives selection: only paths that have
been heavily specialised are worth promoting to a Tier 2 trace, because
unspecialised uops are just generic Python semantics.

## 3. The Tier 2 optimizer

A small set of optimization passes operates on uops in trace form:
- DCE (after specialisation many side-effect-free uops become dead).
- Type narrowing (specialisations carry type info; redundant guards drop).
- Constant propagation (specialisation often turns operands into known
  constants — e.g. attribute slot offsets).

Crucially the optimizer is *itself generated* from the same DSL that
generates the interpreter — so changes to the bytecode definition
automatically propagate to the optimizer and the JIT.

## 4. Copy-and-patch

The compilation strategy. At build time:
1. A C template (`Tools/jit/template.c`) implements every uop as a tiny
   C function with `[[clang::musttail]]` annotations.
2. LLVM compiles the template into one *stencil* per uop: a small chunk
   of position-independent machine code with named "holes" for runtime
   constants (operand values, jump targets, addresses of helper
   functions).
3. The stencils are baked into the CPython binary as `jit_stencils.h`.

At runtime, JIT compilation of a trace becomes:
1. For each uop in the trace, copy its stencil into the code cache.
2. Patch each hole with the runtime value (operand byte, helper address,
   continuation address).
3. Make the resulting page executable.

There is no IR, no register allocator, no instruction selection. The
machine instructions were already chosen by LLVM at build time; runtime
just bolts stencils together.

### Trade-offs

Pros:
- Build-time LLVM cost (3–60 s extra per build) but no runtime LLVM
  dependency.
- Ports cleanly to any platform LLVM can target — currently x86_64,
  aarch64, i686 on Linux/macOS/Windows.
- Generated JIT changes automatically when the interpreter generator
  emits new uop definitions.

Cons:
- Each uop is its own block of code with no inter-uop optimization. No
  CSE across uops, no register allocation across uops (each uop reads/
  writes the Python eval stack via memory).
- Calling-convention overhead at every uop boundary.
- Deep tree of musttail calls; relies on LLVM not breaking the tail-call
  contract.
- "About as fast as the existing specializing interpreter on most
  platforms" — explicitly acknowledged by the PEP.

## 5. Deopt

The Tier 2 interpreter (and the JIT, when fall-through to interpreter is
needed) is the deopt target. Every uop knows where it would go in Tier 1
bytecode if its assumptions failed. Deopt is therefore "stop executing
this uop trace, restart in the Tier 1 interpreter at this bytecode index"
— the specialising interpreter then re-specialises (or doesn't) based on
new observations.

## 6. Pyston (separate, mostly historical)

Pyston-lite (the project that briefly distributed as a `pip`-installable
JIT for CPython 3.8–3.10):
- Used DynASM (the assembler from LuaJIT) for codegen — a tiny C-side
  assembler that emits machine code from a DSL.
- Co-optimised the interpreter to do more profiling (e.g. inline caches
  in the bytecode) so the JIT could specialise.
- Did *not* deeply optimise — the philosophy was "interpreter, but lower
  in the stack and with hot-path patches".
- No longer maintained as a separate distribution; some ideas merged
  into CPython mainline (the specialising interpreter direction).

## 7. Lessons for an Erlang T2

What to copy:
- **Generate the lowering layer from a DSL.** Erlang has BEAM
  instruction definitions in `compiler/scripts/genop.tab` and similar.
  Anything in T2 that *could* be auto-generated from those (specialised
  lowering templates, deopt frame descriptors) probably should be — both
  to keep maintenance low and to inherit changes to BEAM ops
  automatically. This is the lesson that almost certainly killed HiPE.
- **Profile-driven specialisation in the lower tier.** PEP 659's
  bytecode self-rewriting is similar in spirit to what we'd want
  BeamAsm to do: collect type observations cheaply at well-defined
  sites during T1 execution, so T2 has data to consume.
- **The Tier 2 interpreter as deopt target is genius.** It means deopt
  doesn't have to land in a *correct* but *slow* tier — it lands in a
  tier that was already designed to faithfully execute the optimised
  IR. We don't have an analogous "uops interpreter" for BEAM SSA
  today, but we *do* have BeamAsm itself, which serves the same role.
- **5% benchmark improvement is the bar to ship**, per the PEP. That
  is a useful sobering datum: the *infrastructure* matters more than
  the first-version speedup; design the infrastructure to allow many
  small wins to accumulate.

What to avoid:
- **Copy-and-patch as the primary codegen strategy.** It limits us to
  per-uop optimization, which forfeits exactly the wins the prompt is
  asking for (cross-instruction inlining, type-driven specialisation,
  alias analysis). The CPython JIT's stencil approach is great for an
  *infrastructure-first, incrementally-improving* JIT. Our T2 should
  start higher up the optimization curve because the AOT compiler has
  already done most of the work.
- **Shipping the JIT off-by-default forever.** PEP 744 is explicitly
  experimental and currently doesn't justify itself. We need a clear
  performance story before T2 ships enabled.
