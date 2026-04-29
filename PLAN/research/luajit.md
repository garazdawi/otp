# LuaJIT — Tracing JIT with SSA IR

Sources:
- [LuaJIT homepage](https://luajit.org/luajit.html)
- [LuaJIT SSA IR (tarantool wiki mirror)](https://github.com/tarantool/tarantool/wiki/LuaJIT-SSA-IR)
- [DeepWiki: JIT Compilation System](https://deepwiki.com/LuaJIT/LuaJIT/2-jit-compilation-system)
- `LuaJIT/src/lj_record.c` (v2.1).

LuaJIT is the canonical *tracing* JIT for a dynamic language. We are unlikely
to copy this architecture wholesale — Erlang has too many side-effecting
operations (message send, ETS, GC) and too many natural function-call
boundaries for tracing to be the right unit. But LuaJIT has the cleanest
public design notes for any small JIT, and several of its sub-components
(linear SSA, snapshot-driven deopt, allocation sinking) are individually
relevant.

## 1. The trace as the unit of compilation

A *trace* is a single linear sequence of executed bytecode, recorded across
arbitrary control flow (function calls, branches), with each conditional
becoming a **guard**. The compiler unit is the trace, not the function.

- **Hot loop trace**: started when a backward branch's counter trips.
- **Hot call trace**: started when a non-builtin function's counter trips.
- **Side trace**: started when a guard in an existing trace fails repeatedly,
  branching off the parent trace at the failure point.

Recording inlines all calls. The trace ends at a loop back-edge (compiling
a `LOOP` node), at a sufficiently long straight-line trace, or when an event
occurs that can't be recorded (a NYI primitive — "Not Yet Implemented").

## 2. The IR

Linear, pointer-free, SSA, 64-bit per instruction.

- Instructions live in a flat array. References (`IRRef`) are array indices.
- The array is grown bidirectionally: constants down, instructions up.
- All ops are 2-operand. Variadic ops (calls) chain via `CARG` extension
  instructions.
- The IR is **immutable**. Optimization happens at *emit time*: when about
  to emit `MUL(5,3)`, the FOLD engine emits the constant `15` instead.
- Every instruction is typed. ~23 types: `nil`, `false`, `true`, `string`,
  `table`, `function`, integer widths (i8..u64), float, double, pointer
  variants. Higher-level Lua types are modeled with guards.
- Guards are flagged comparisons (`LT`, `EQ`, `ABC` array bounds, `RETF`
  return frame) that exit the trace on failure.
- `SLOAD slot, type` reads an interpreter slot *and* type-checks it. There
  is no `SSTORE` — store-back to interpreter slots happens implicitly via
  snapshots at trace exits.

## 3. Snapshots — the core deopt mechanism

A **snapshot** records the IR refs that should be written back into each
interpreter stack slot if the trace exits at this point. Snapshots are
emitted *sparsely* — before each guard, at loop boundaries, and at calls
that might deopt.

On trace exit:
1. Look up the snapshot associated with the failing guard.
2. For each live slot, materialise its IR-ref-or-spill value back into the
   interpreter stack slot, converting type tags as needed.
3. Re-enter the interpreter at the bytecode PC the snapshot recorded.

This is the same idea as JSC's "stackmap-driven OSR exit" — but designed
into the IR from day one. The IR genuinely never models stores to
interpreter slots; allocation sinking falls naturally out of this.

## 4. Optimizations

The dump banner `JIT: ON CMOV SSE2 SSE3 AMD fold cse dce fwd dse narrow`
lists the passes:

- **FOLD** — constant folding at emit time. The FOLD engine is the most
  important optimization (it also rewrites algebraic identities,
  canonicalises operand order, etc.).
- **CSE** — per-opcode chained search backward, very cheap.
- **DCE** — instructions with no uses are simply not emitted.
- **FWD** — store-to-load forwarding (because the IR is linear, this is
  literally walking back to the most recent store and replacing the load
  with the stored value's IR ref).
- **DSE** — dead store elimination, made possible by the snapshot model:
  a store is "dead" if no snapshot references it.
- **NARROW** — type narrowing (e.g. double → integer when the context
  guarantees it).
- **Loop optimization** — `LOOP` instruction marks the boundary between
  pre-loop "header" and the loop body; loop-invariant code sinks before the
  `LOOP`, induction variables are recognised.
- **PHI** at the trace tail represents loop-carried dependencies (left =
  initial value, right = end-of-iteration value).
- **Allocation sinking** — separately documented; see below.

## 5. Allocation sinking

If an allocation never escapes the trace (no global store, no escape into
a non-recorded call), the allocation is *not emitted*. Instead its fields
are materialised on demand into snapshots — so if the trace exits, the
allocator runs in the deopt path; if not, no allocation ever happens.

This is the optimization that turns most trace work from "boxed-Lua" into
"unboxed-machine-words". The mechanism (objects sunk to exits) is exactly
what the snapshot/IR model enables.

## 6. Code generation

There is no separate LIR/MIR. The same SSA IR is consumed directly by the
backend. Register allocation is local: the allocator walks instructions in
reverse, holding a small set of register-slot mappings; `RENAME` instructions
record register-to-register moves the allocator inserts. Memory addressing
modes (`AREF`+`ALOAD`) are fused into single load instructions where the
target ISA supports it.

## 7. Side traces and trace trees

When a guard fails > N times, a side trace is recorded starting at the
failure. Side traces "graft onto" the parent: the `BASE` instruction
records the parent IRRef and the snapshot index. Multiple side traces from
a single root trace form a *trace tree* — together they cover the
specialisations the actual workload needs, without compiling the rest.

## 8. Limitations

- Variadic calls, certain FFI patterns, and various BIFs are NYI; tracing
  bails out and the path stays interpreted.
- The IR is immutable; you can't insert/delete instructions for a global
  rewrite. All optimization is local-at-emit-time.
- Compile time and code quality are both better than a method JIT for
  small hot loops, and worse for code where there is no obvious "hot
  trace" (e.g. branchy state machines).
- A bad first specialisation pollutes the entire trace tree until it's
  blacklisted and re-recorded.

## 9. What is *not* applicable to Erlang

- **Tracing across calls**. Erlang programs are dominated by message
  passing, ETS, GC, and inter-process work. Hot inner loops rarely span
  many functions — the language style discourages it.
- **Side-trace explosion** is bad enough in Lua; for Erlang's many-shape
  pattern matches (records, maps, binaries) it would be far worse.
- **Bailout to interpreter** doesn't fit BeamAsm's "always-JIT" model.

## 10. What *is* applicable

- **Linear SSA** is much easier to write and debug than sea-of-nodes, and
  fast to allocate (just a flat array). For a T2 IR this is a strong
  reference point.
- **Snapshots as the deopt model** are conceptually cleaner than per-exit
  hand-rolled code. They generalise to any tier transition.
- **Allocation sinking** is *very* relevant for Erlang. Tuples, conses,
  small maps — most are short-lived and could be unboxed across short
  optimization regions if the deopt path materialises them.
- **FOLD at emit time** is the right way to make a small JIT not slow:
  every other tier-2 JIT either does the same or pays for it.
- **Backward CSE walk** is what every fast SSA JIT does (Maglev included).
