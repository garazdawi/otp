# PyPy — Meta-Tracing JIT for Python (RPython)

Sources:
- ["Tracing the meta-level: PyPy's tracing JIT compiler" — Bolz, Cuni, Fijałkowski, Rigo (ICOOOLPS 2009)](https://dl.acm.org/doi/10.1145/1565824.1565827)
- [PyPy Trace Optimizer documentation (rpython.readthedocs.io)](https://rpython.readthedocs.io/en/latest/jit/optimizer.html)
- ["Musings on Tracing in PyPy" — Carl Friedrich Bolz-Tereick (2025)](https://pypy.org/posts/2025/01/musings-tracing.html)
- ["Allocation Removal by Partial Evaluation in a Tracing JIT" — Bolz et al.](https://www.researchgate.net/publication/...)
- ["Loop-Aware Optimizations in PyPy's Tracing JIT" — referenced in optimizer docs.]

PyPy is the canonical optimizing JIT for Python and the most successful
production tracing JIT for any major dynamic language. Its **meta-tracing**
approach is unique — the JIT is not built for Python specifically; it's a
generic framework (RPython's JIT generator) that traces *the interpreter
loop* of any RPython-implemented language and produces an optimised version.

We've already covered LuaJIT (the canonical small-team tracing JIT). PyPy
matters specifically because:
1. It has 15+ years of production tracing experience.
2. The author has recently published a candid post-mortem on what tracing
   does and doesn't do well, and what she'd do differently with a clean
   slate (`musings-tracing.html`, 2025). That post is *the* authoritative
   reflection on tracing-JIT design choices for any new project.
3. Allocation removal / partial evaluation on traces is the most
   sophisticated published version of what we'd want from "let me unbox a
   short-lived tuple across a hot region".

## 1. Meta-tracing in one paragraph

PyPy's interpreter is written in RPython — a statically-typed subset of
Python translated to C. The RPython toolchain *generates* a tracing JIT for
the interpreter being implemented. At runtime, the JIT traces the
*interpreter's* hot loop (which happens to be executing Python bytecode);
it sees both the interpreter's own primitives and the guest-language
operations they carry out. Optimization on this combined trace folds away
interpretation overhead and produces specialised native code for the
guest-language workload.

Hints in the interpreter (`hint(promote=True)`, `jit.elidable`, etc.) tell
the meta-tracer what to specialise on (e.g. "this is a constant from the
JIT's POV — promote it") and what not to trace into.

## 2. Trace IR and optimization passes

PyPy's trace IR is roughly:
```
[p0,i0,i1]
label(p0, i0, i1)
i2 = getarrayitem_raw(p0, i0, descr=<Array Signed>)
i3 = int_add(i1, i2)
i4 = int_add(i0, 1)
i5 = int_le(i4, 100)
guard_true(i5)
jump(p0, i4, i3)
```

Linear, typed, with `guard_*` ops marking exits and `descr=` metadata for
type/layout info. Variables are SSA-ish boxes (`history.py`).

The optimizer runs in a fixed sequence:
```
intbounds : rewrite : virtualize : string : earlyforce : pure : heap : unroll
```

| Pass | Purpose |
|------|---------|
| `intbounds` | Integer-range analysis for bounds-check removal, narrowing. Rule-based DSL. |
| `rewrite` | Strength reduction (`x*2` → `x<<1`), algebraic identities, constant folding. |
| `virtualize` | Escape analysis. Allocations that don't escape become "virtual"; field accesses become direct value tracking. **The big win — partial evaluation on traces.** |
| `string` | String-specific rewrites and constant folding. |
| `earlyforce` | Materialise virtuals when they're about to escape, before `pure` runs. |
| `pure` | Memoization of pure ops + constant folding when args are constants. |
| `heap` | Redundant-load elimination, alias tracking across heap accesses. |
| `unroll` | Unrolls the loop once, glues the two copies, then reruns the other passes — letting cross-iteration optimizations fire. |

All passes use a `make_dispatcher_method()` pattern: an opcode-keyed
dispatcher that routes to per-opcode handlers. Falls back to
`emit_operation` for non-specialised opcodes.

## 3. Allocation removal — the "virtual" mechanism

PyPy's virtual-object analysis is what makes a trace through a Python
`for x in xs: total += x` fast. As the trace records, intermediate
PyInt objects are allocated, summed, discarded. Without virtuals, every
iteration heap-allocates an integer.

The `virtualize` pass detects allocations whose lifetime is bounded by
the trace, marks them "virtual", and replaces field accesses with direct
references to the values that would have been stored. If a virtual
object is about to *escape* (passed to a function that wasn't traced
into, stored to the heap, etc.), `earlyforce` materialises it just
before the escape point.

Key paper: "Allocation Removal by Partial Evaluation in a Tracing JIT".
This is *the* paper to read before designing any "unbox short-lived
allocations across a region" pass for our T2.

## 4. Loop linking — bridges, side traces, trace trees

A failing guard in a hot trace becomes a *bridge*: a small recorded trace
that starts at the failure point, with the assumption negated. The bridge
ends by jumping back into either the main trace or the interpreter.

Multiple bridges off a single trace form a tree (the same shape as
LuaJIT's side-trace tree). The optimizer reruns on each new branch,
re-applying virtual-object analysis to specialise the cold path.

## 5. Bolz-Tereick's 2025 reflections

The candid post-mortem after 15+ years of production. Direct quotes:

> "We tried to implement a method-based meta-JIT … weren't as good as we
> hoped. We then switched to tracing and finally started producing
> interesting performance."

So tracing was *pragmatic*, not principled.

> "Tracing tends to have big performance cliffs."

The fundamental problem: when tracing assumes wrong, performance
collapses. A method JIT degrades more gracefully.

> "Problematic edge cases: when to stop inlining, how to handle
> recursion, preventing trace explosion from nested loops … solved by
> adding heuristics, but doing so loses a lot of the simplicity of
> tracing."

Heuristic-driven trace-control is a maintenance tax.

> "Programs with extremely unpredictably branchy code like bytecode
> interpreters perform poorly [under tracing], since tracing assumes
> loops take similar control flow paths."

Erlang code (pattern matching, state machines, supervisor trees) is
*exactly* this kind of branchy code. This is a strong indicator that a
tracing JIT is wrong for our workload.

> "Given adequate resources and industrial constraints, it's very unclear
> you should use tracing. Tracing is a labor-saving device for compiler
> authors. Modern view: method-based JIT with sophisticated CFG-based
> baselines is preferable."

This is the closing argument: with industrial resources (which we have
— OTP team + ecosystem), method-based with CFG-SSA is the better choice.
**This validates our T2 design direction directly.**

## 6. Lessons for an Erlang T2

What to copy:
- **Allocation removal / virtuals.** The single best published
  optimization for short-lived heap allocations. We want this for
  Erlang tuples, conses, fun closures across optimization regions.
  Implementation hint: integrate with our SSA pass pipeline as a
  separate "escape & virtualise" pass after type narrowing.
- **`pure` op memoization.** BIFs marked elidable (`erlang:length/1`
  on a list, `erlang:tuple_size/1`, etc.) can be CSE'd more
  aggressively if T2 has an "always-pure" attribute on each op.
- **Per-opcode dispatcher pattern** (`make_dispatcher_method`). Tidy
  way to keep optimizer code organised as it grows.
- **The optimizer-is-a-pipeline pattern.** Composable passes that
  share an `Optimization` base class are easier to reason about than
  monolithic optimizers.

What to avoid:
- **Tracing as the primary compilation strategy.** The author of one
  of the most successful tracing JITs explicitly says don't, given
  resources. Erlang code is precisely the branchy-state-machine kind
  that tracing handles worst.
- **Meta-tracing.** Brilliant idea, but unjustifiable engineering
  investment for a single language. We're optimizing one language
  (Erlang); a hand-written optimizer is the right scale.
- **Unbounded inlining via tracing.** PyPy's trace-explosion problem
  is real. Method-JITs with explicit inlining heuristics control this
  cleanly.

What to *cite to skeptics*:
- The Bolz-Tereick post is the strongest single argument that the
  tracing-vs-method-JIT debate is *over* — given resources, method-
  based wins. We are well-resourced. Our T2 should be method-based
  CFG-SSA, exactly aligning with Maglev/JSC-DFG/ZJIT/Sista.
