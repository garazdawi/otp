<!--
%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
-->

# Tail-Modulo-Cons (TMC)

A compiler transform that turns list-building recursion into an
**O(1)-stack destination-passing loop**, plus the runtime support it needs.
**On by default**; disable per-module with `+no_tmc` (`erlc`) or the `no_tmc`
compile option. With TMC disabled the compiler output is byte-identical to
before the feature existed. The transform only touches *eligible* functions;
every other function compiles exactly as it did.

Modelled on OCaml's `[@tail_mod_cons]` (2022). The idea for Erlang goes back to
erlang-questions (2002); it was never done because BEAM's heap is immutable and
mutating a tenured cons to point at a young cell breaks the generational GC.
TMC sidesteps that (see *GC mechanism*).

## What it transforms

Two list-building idioms, lowered through **one** destination-passing core
(`beam_ssa_tmc`).

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

The list element may be any expression, including one that allocates on the
heap (`[{K,V} | f(T)]`, `[<<...>> | f(T)]`, …): the element-building
instructions are preserved and evaluated before the cell is spliced on.

Front-end 2's base seals with `lists:reverse(Acc, Root)`
(`= reverse(Acc0) ++ Root`), which is the original result for **any** initial
accumulator, so it needs no interprocedural "seeded with `[]`" proof, and no
uniqueness/alias proof either (a fresh `Root` is built; `Acc` is never mutated).
It fires only when the accumulator is used **solely** as the prepend tail and
the reverse argument (so eliminating the reversed accumulator is unobservable),
rejecting escapes, observed-before-reverse, multi-reverse and aliased cases.

Scope: cons-only, single self-recursion. Multi-cons tails (`[A, B | self()]`),
self calls that are not directly a cons tail, mutual recursion and filter-shape
accumulators are left unchanged (natural later extensions of the same core).

## Semantic consequence: shallower stacks

TMC converts a body-recursive builder into a tail loop, so a call that used to
sit inside `n` stack frames now runs in one. This is observable:

* an exception raised while building the list carries a **shorter
  stacktrace** (the intermediate builder frames are gone), and
* `erlang:process_info(P, stack_size)` / `current_stacktrace` report the
  shallower stack.

The list *value* and evaluation order are unchanged; only the stack shape is.

## The `set_cons_tail` instruction

A new BEAM instruction (opcode **192**), `set_cons_tail Cell NewTail`,
destructively writes the tail (CDR) of `Cell`. Emitted **only** by the TMC
transform, which guarantees `Cell` is a freshly built, unshared cons cell
reachable only from registers. Implemented in the interpreter
(`emu/instrs.tab`) and in both BeamAsm T1 JIT back-ends — aarch64
(`jit/arm/instr_common.cpp`) and x86_64 (`jit/x86/instr_common.cpp`). The
compiler side is `beam_ssa_tmc` (recognizer + rewrite), run from `compile:`
unless `no_tmc` is given, with codegen (`beam_ssa_codegen`) and validator
(`beam_validator`) support.

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
  the list copy stays ~O(n); a single-list build past ~1M elements can be a
  modest wall regression versus plain body-recursion (still less memory). This
  is the one case where `+no_tmc` may be worth reaching for.

Removing the long-build tax without a full sweep needs a per-process
tracked-edge remembered set scanned by the minor GC. That is a corruption-class
GC change requiring old-heap sizing coordination and careful handling of shared
element/seal edges — a dedicated GC project, deliberately out of scope here.

## Using it

    erlc mymod.erl                 %% TMC is applied by default
    erlc +no_tmc mymod.erl         %% disable the transform for this module
    erlc +tmc_report mymod.erl     %% print which functions were rewritten

The legacy `+tmc` option is still accepted for compatibility; since the
transform now runs by default it is a no-op.
