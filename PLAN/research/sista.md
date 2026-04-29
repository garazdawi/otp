# Sista — Speculative Inlining Smalltalk Architecture

Sources:
- ["Sista: Saving Optimized Code in Snapshots for Fast Start-Up" — Béra, ManLang 2017](https://rmod-files.lille.inria.fr/Team/Texts/Papers/Bera17b-ManLang-SistaArchitecture.pdf) (PDF)
- [Clément Béra — "An update on the Sista project" (2016)](https://clementbera.wordpress.com/2016/06/09/an-update-on-the-sista-project/)
- [Sista: open alpha release (2017)](https://clementbera.wordpress.com/2017/07/19/sista-open-alpha-release/)
- [Cog Blog — About Cog](http://www.mirandabanda.org/cogblog/about-cog/) (self-signed cert)
- [Pharo Optimising JIT Internals (slides)](https://www.slideshare.net/slideshow/pharo-optimising-jit-internals/79516042)

Sista is the closest published analogue to the project we're planning:
**a tier-2 optimizing JIT bolted onto a pre-existing baseline JIT (Cog) for
a dynamic language**. The Cog VM is to Pharo as BeamAsm is to BEAM —
template-style baseline JIT, no IR. Sista is what an optimizer that sits
above it looks like.

## 1. Architecture: split between Cog and Scorch

The existing **Cog** VM (in C) is unchanged in role:
- Baseline JIT generating machine code from Smalltalk bytecode.
- Polymorphic inline caches (PICs) at every send site.
- Stack/register layout that the GC and runtime understand.

Sista adds:
- **Scorch** — the optimizer, written in Smalltalk *image-side* (in
  the running language itself, not in C).
- A new bytecode set (**SistaV1**) with extra opcodes.
- A small amount of Cog runtime support: counter increments, deopt
  trampolines, blob installation.

The crucial design decision: **Scorch optimizes from bytecode to
bytecode, not from bytecode to machine code.** Scorch reads regular
Smalltalk bytecode + PIC profile data, builds an SSA IR, optimizes it
(inlining, CSE, bounds check elimination, etc.), and emits *SistaV1
bytecode*. Cog then JIT-compiles the SistaV1 bytecode the way it
already JITs everything else.

> "I convert it to an SSA-like IR, do the optimization passes, then
> convert it back to bytecodes."

## 2. Why bytecode-to-bytecode

Reasons Béra documents:
- **The baseline JIT already exists and works.** Reusing it as the
  backend means Scorch never has to deal with platform-specific
  machine code, register allocation for multiple ISAs, or the GC's
  view of stack frames. All those are Cog's job, and Cog already
  solves them for the unoptimised case.
- **Snapshots can save optimised code.** A Pharo image can be saved
  to disk and restored later; if the optimised form is bytecode, it
  fits in the image just like any other code. The 2017 paper's main
  contribution is demonstrating exactly this.
- **The optimizer can be written in the language it's optimising.**
  Scorch is Smalltalk; it's introspectable and easier to evolve than
  C-level optimizer code.
- **Sista's correctness is easier to argue.** Bytecode-to-bytecode
  transformations are checked by the same loaders, decompilers, and
  validators that the rest of the system uses.

The cost: the optimizer cannot exploit machine-level facilities
(architecture-specific instructions, custom register allocation across
optimization regions). For most adaptive optimizations on a dynamic
language this is acceptable — the wins come from *removing work*
(redundant checks, indirections, calls), not from machine-code micro-
tuning.

## 3. SistaV1 bytecode extensions

The SistaV1 bytecode set adds opcodes the unoptimised set doesn't have.
The 2016 blog post enumerates broad categories; the 2017 paper details
specifics. The relevant new ones:

- **Type-check opcodes** that branch (or trap) on receiver class. The
  optimized code uses these instead of full polymorphic sends.
- **Trap opcodes** — `trapIfNotInstanceOf`, `trapIfNotSmallInteger`,
  etc. A trap is an unconditional deopt point: if the predicate
  fails, the runtime reverts the frame to baseline.
- **Inlined-block opcodes** — when a closure is inlined into its
  caller, certain control-flow opcodes (`branchIfTrue`,
  `branchIfFalse`) operate on stack values without going through the
  closure invocation protocol.
- **Counter-increment opcodes** for tier-up profiling at the baseline
  level. A counter sits at every back-edge; when it trips, Scorch is
  invoked.
- **Encoding extensions** — wider literal indices, wider jump
  displacements (the unoptimised bytecode set was limited to 256
  literals, etc.).

## 4. Tier-up policy

A counter increments at each backward branch and at function entry in
baseline code. When it crosses a threshold (Béra mentions "thousands"
in his blog, no exact number in 2016), the runtime calls into Scorch
*from Smalltalk image*, asynchronously, and Scorch:

1. Pulls the bytecode and metadata for the hot method.
2. Walks PICs at each send site to collect type feedback.
3. Walks call-graph counters to estimate hot callees.
4. Optimizes (see next section).
5. Asks Cog to JIT-compile the resulting SistaV1 bytecode.
6. Patches the method dictionary so future calls go to the new
   compiled code.

## 5. Speculative inlining

Driven entirely by PIC contents.

- **Monomorphic send site** (one receiver class observed): inline the
  callee directly, prefixed with a single `trapIfNotInstanceOf`.
- **Polymorphic send site** (2–6 classes): inline a small
  switch-on-class with one inline copy per receiver. Each branch can
  itself be further optimised. Below the cap, the call site falls
  back to the standard polymorphic dispatch.
- **Megamorphic** (above cap): no inlining; emit a normal send.

Inlined methods can themselves contain sends, which are inlined
recursively up to a depth/size cap. Béra emphasises this is where the
big wins come from — collapsing layers of accessors and small
dispatching helpers into the caller.

Other optimisations Scorch performs:
- Dead code elimination,
- Constant propagation/folding,
- Common subexpression elimination,
- Loop bounds-check elimination,
- Overflow-check elimination on small-integer arithmetic,
- Push/pop optimization (since the IR is closer to register-form than
  bytecode is).

## 6. Deoptimization

This is the hardest part of Sista, and the part that the 2016 update
focuses on. Two issues:

**(a) Multi-frame deopt.** When a trap fires, Scorch may have
inlined many call frames into one optimised frame. Deopt has to
*split* the optimised frame back into N baseline frames, each with
its own program counter, locals, and stack. The metadata carried with
each trap describes the call-stack shape at that point.

**(b) Closures.** The original Cog closure representation embedded
captured variables in the closure object in a way that interacted
badly with deopt — if Scorch had folded a closure invocation into the
caller, deopt had to re-materialise both the closure and any captured
state in a way that matched the bytecode-level semantics. The
**FullBlockClosure** redesign (separate compiled-block objects,
explicit captured state) was a prerequisite for Sista to be tractable.

> "Deoptimization sometimes required to deoptimize multiple optimize
> frames, which was very tricky to handle… FullBlockClosure
> [redesign] simplified [it] considerably."

The takeaway: **inlining and deopt are inseparable design problems**.
Any decision to inline must come with a clear deopt story. We will
hit the same wall when we inline a `fun` invocation in T2 — the deopt
has to recreate the fun frame *and* the caller frame.

## 7. Persistence — the 2017 paper's contribution

Optimised methods can be saved into the Pharo image (the
checkpointed heap) so a restored image starts up with optimised code
already in place. This trades startup time for image size. It's also
why bytecode-to-bytecode mattered: the optimised form is a
first-class Smalltalk method, not an opaque blob.

For Erlang we do not have an analogous "image save" concept (the
node restarts cold), so this contribution doesn't directly apply.
But the underlying lesson — that an optimised form which is *just
more bytecode* fits naturally into existing tooling — is worth
remembering.

## 8. Numbers

From the 2016 blog: "1.6× and 1.8× speedup" on Shoutout and integer
benchmarks at the time of writing, with the author noting they had
"spent close to no time tweaking the performance" and were focused
on correctness. Subsequent work pushed the numbers higher; the 2017
paper has updated benchmarks (worth re-reading the PDF for them).

## 9. Lessons for an Erlang T2

The most directly applicable design from any of the JITs we surveyed.

1. **Bytecode-to-bytecode is the cheapest path to a tier-2.** If T2
   produces optimised BEAM SSA (or a small extension of it) and lets
   BeamAsm code-gen as today, we never have to write a second
   asmjit-using backend. We get to focus 100% on the IR-level
   optimizer.
2. **Inlining is the dominant win**, *not* CSE or DCE individually.
   This matches the prompt directly: "speculative type information
   and inlining of small functions". Scorch's experience says these
   two together cover most of the realistic gain.
3. **Deopt is harder than the optimizer.** Plan it first, not last.
   Specifically, plan how to reconstruct N nested call frames from
   one optimised frame, including any closure captures.
4. **Profile data lives in inline caches.** BeamAsm doesn't have
   PICs today, but it does have indirect dispatch through the export
   table — the same place we'd add per-call-site type observation.
5. **Run the optimizer in the language being optimised** — for us
   that means writing Scorch's equivalent (the optimization passes)
   in *Erlang* and reusing the existing `beam_ssa_*` modules
   directly. The optimizer is then loadable, hot-upgradeable, and
   debuggable using ordinary Erlang tools.
6. **Closures-and-deopt matters for Erlang too.** Funs capture
   variables; if T2 inlines a fun, deopt has to materialise the fun
   and its captured environment. Pharo had to redesign closures to
   make this tractable. We need to audit `erl_fun.[hc]` and the
   `make_fun3` / call-fun paths for the same kinds of issues
   *before* committing to inline funs.
7. **Add new opcodes as needed for traps and type guards.** Don't
   try to express T2-specific semantics through existing BEAM
   opcodes if a small handful of new ones make the optimizer
   tractable. The unification of "compiled and optimised look like
   the same thing to the loader" comes for free if everything's
   still BEAM SSA at the joint.
