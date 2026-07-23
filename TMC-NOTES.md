# Tail-Modulo-Cons (TMC) for Erlang/OTP

An opt-in compiler transform that turns list-building recursion into an
**O(1)-stack destination-passing loop**, plus the runtime support it needs.
Off by default (`+tmc`); with the option off the compiler output is
byte-identical. This branch is TMC-only and independent of any JIT tiering
experiment.

Modelled on OCaml's `[@tail_mod_cons]` (2022). The idea for Erlang goes back to
erlang-questions (2002); it was never done because BEAM's heap is immutable and
mutating a tenured cons to point at a young cell breaks the generational GC.
TMC sidesteps that (see *GC mechanism*).

## What it transforms

Two list-building idioms, lowered through **one** destination-passing core.

**Front-end 1 — body-recursive builders** (`[H | self(...)]`: `map`, `filter`,
`append`, list comprehensions, tree walks):

```erlang
map(F, [H|T]) -> [F(H) | map(F, T)];      %% grows the stack O(n)
map(_, [])    -> [].
```

**Front-end 2 — the tail-recursive accumulator+reverse idiom** people hand-write
to avoid that stack growth:

```erlang
squares(L) -> squares(L, []).
squares([H|T], Acc) -> squares(T, [H*H|Acc]);   %% prepend, reverse at the end
squares([], Acc)    -> lists:reverse(Acc).       %% (reverse/2 also handled)
```

Both are rewritten to build the list **forward** into a generated helper
`-tmc-F/A-'/A+2` that threads two extra arguments — `Root` (the list being
built, eventually returned) and `Dest` (the cell whose tail is the current
hole). The original function builds the first cell and tail-calls the helper;
each iteration builds the next cell and splices it on with
`set_cons_tail(Dest, New)`; the base clause seals the last hole and returns
`Root`. Same element order, same evaluation order, no accumulator, no reverse.

Front-end 2's base seals with `lists:reverse(Acc, Root)`
(`= reverse(Acc0) ++ Root`), which is the original result for **any** initial
accumulator, so it needs no interprocedural "seeded with `[]`" proof, and no
uniqueness/alias proof either (a fresh `Root` is built; `Acc` is never mutated).
It fires only when the accumulator is used **solely** as the prepend tail and
the reverse argument (so eliminating the reversed accumulator is unobservable),
rejecting escapes, observed-before-reverse, multi-reverse and aliased cases.

v1 scope: cons-only, single self-recursion. Multi-cons tails, nested
constructors, mutual recursion and filter-shape accumulators are left
unchanged (natural later extensions of the same core).

## The `set_cons_tail` instruction

A new BEAM instruction (opcode **192**), `set_cons_tail Cell NewTail`,
destructively writes the tail (CDR) of `Cell`. Emitted **only** by the TMC
transform, which guarantees `Cell` is a freshly built, unshared cons cell
reachable only from registers. Implemented in the interpreter
(`emu/instrs.tab`) and the aarch64 T1 BeamAsm JIT
(`jit/arm/instr_common.cpp`); the x86_64 emitter is a documented TODO (a faithful
mirror to be added and re-proven on an x86 host), so on x86 a `+tmc` module runs
under the interpreter flavor. The compiler side is
`beam_ssa_tmc` (recognizer + rewrite), wired into `compile:` behind the `tmc`
option, with codegen / validator support.

## GC mechanism (force-fullsweep)

The hole write can create an old->young pointer if `Cell` was tenured by a GC
that fired mid-build. BEAM keeps no remembered set, so instead of a write
barrier the instruction flags the process for a **full-sweep** GC whenever
`Cell` is not in the young generation `[high_water, HTOP)`:

```
CDR(Cell) = NewTail;
if (Cell not in [high_water, HTOP)) p->flags |= F_NEED_FULLSWEEP;
```

A full sweep scans the old heap and rebuilds a single generation, repairing the
edge. When `Cell` is young — the common case, the build has not spanned a GC —
it is a plain young->young store and nothing is flagged. This is the same
young-generation test the in-place `update_record` instruction already uses.
The mechanism has been verified to repair every edge with no corruption under
forced mid-build tenuring on debug (lock-checking + assertions) and
AddressSanitizer builds.

## Honest value

* **O(1) stack, automatically.** A body-recursive builder that grows the stack
  ~2 words/element (megabytes for a 1M list, and scanned on every GC) runs in
  constant stack — measured ~5800x less peak stack at 200k — without the
  programmer hand-writing the accumulator+reverse idiom.
* **Less memory.** Front-end 2 builds `n` cells instead of the idiom's `2n`
  (`n` prepended + `n` from `reverse`), ~2-4x less heap in practice.
* **Wall time.** Competitive-to-faster for typical list sizes (often ~2x faster
  than body-recursion in the 10k-1M range); the average real-world speedup is
  modest — this is the correct general transform, not a large %.
* **Long-build tax.** A build that spans many GCs forces one full sweep per
  tenuring interval. Heap growth is geometric so this is ~O(log n) sweeps and
  the list copy stays ~O(n), but a single-list build past ~1M elements can be a
  modest wall regression versus plain body-recursion (still less memory). Because
  the feature is opt-in, this is neutralized in practice.

Removing the long-build tax without a full sweep needs a per-process
tracked-edge remembered set scanned by the minor GC. That is a corruption-class
GC change requiring old-heap sizing coordination and careful handling of shared
element/seal edges — a dedicated GC project, deliberately out of scope here.

## Using it

    erlc +tmc mymod.erl          %% enable the transform
    erlc +tmc +tmc_report ...    %% also print which functions were rewritten

Off by default. A `beam_ssa_tmc`-rewritten module runs on the interpreter and
the aarch64 JIT; on x86 it currently runs under `-emu_flavor emu`.
