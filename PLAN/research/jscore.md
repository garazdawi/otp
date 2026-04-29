# JavaScriptCore (WebKit) — Tiered Speculative JIT

Source: ["Speculation in JavaScriptCore" — Filip Pizlo (WebKit blog, 2020)](https://webkit.org/blog/10308/speculation-in-javascriptcore/).

JSC is the most heavily-documented production tiered JIT for a dynamic language
and is the closest reference architecture for what a T2 for Erlang should look
like. We are explicitly *not* aiming for the FTL (T3) tier, so the parts of
the design that matter are LLInt, Baseline, and DFG.

## 1. Tier hierarchy

| Tier | What it is | Throughput (per-bytecode ns on JetStream 2) |
|------|------------|----------------------------------------------|
| LLInt | Bytecode interpreter written in `offlineasm` | ~3.97 |
| Baseline | Per-opcode template JIT, polymorphic inline caches, diamond speculations | ~1.71 (~2.3× LLInt) |
| DFG | CFG-based SSA optimizer with OSR-based speculation | ~0.349 (~4.9× Baseline) |
| FTL | DFG + LLVM-style B3 backend, full optimizations | ~0.225 (~1.55× DFG) |

Bytecode is the persistent shared representation across all tiers. *Bytecode
indices are also the names of OSR exit targets* — that is the fundamental
unifying choice that makes tier-down work cleanly.

## 2. Tier-up control

A single execution counter per function:
- a function call adds **15 points**, a loop iteration adds **1 point**.
- LLInt → Baseline at **500 points** (scaled to 250 if a Baseline blob already exists for this function, 2000 otherwise).
- Baseline → DFG at **1 000 points**.
- DFG → FTL at **100 000 points**.

Thresholds are scaled by:
```
scale(S, R, M, U) = (0.825914 + 0.061504 * sqrt(S + 1.02406))
                  * pow(2, R)
                  * M / (M - U)
```
- `S` = function bytecode size (sqrt growth — small functions tier up sooner).
- `R` = recompilation count (exponential backoff per failed compile).
- `M` = total executable memory; `U` = estimated bytes this function will use (chokes off compilation as the code cache fills).

Before tier-up, JSC also checks **profile saturation**: at least 3/4 of profile
sites must have data, otherwise counters reset (up to 5 retries).

## 3. The "value bet" framing

The blog explicitly frames speculation as an EV calculation:
```
EV = p * benefit - (1 - p) * cost
```
With measured costs (DFG exit ≈ 2 499 ns, FTL exit ≈ 9 998 ns) and a per-
bytecode benefit of ~1.48 ns, the break-even probability for DFG is **p > 0.9994**.

**The lesson for our T2 is: only speculate where we are nearly certain.** Cheap
profiling counters that may be wrong sometimes are catastrophic if the cost of
being wrong is microseconds — the speculation only pays back over thousands of
correct executions.

## 4. Profiling infrastructure

Three complementary mechanisms.

### Value profiles
At each profiling site (e.g. each `get_by_val`), one **bucket** holds the most
recently observed JSValue. Every ~1 000 execution counter points, a slow path
runs `prediction propagation` — a static analysis that fills in the types the
abstract interpreter can't reason about (heap loads, call returns, function
arguments) using the bucket data.

`SpeculatedType` is a 40-bit set: `Int32Only`, `BoolInt32`, `NonBoolInt32`,
`Double`, `Cell`, etc. Any subset is representable, so `Int32 | Double`
("number-ish") narrows to Int32 fast paths in DFG.

### Inline caches (PICs)
`get_by_id`, `put_by_id`, `call`, etc. start as an unconditional jump to a slow
path. On first execution the slow path patches the call site with a structure
check + direct memory access. Structures (= "hidden classes") are **hash-
consed**, so structure-equality is a single pointer compare. ICs are
"negative-cost profiling" — they speed up execution *and* record per-site type
distributions that DFG reads.

### Case flags
Binary bits like `didObserveInt32Overflow`, `sawNonCell`. DFG only speculates
on the corresponding fast path if the flag is unset. Overflow handling is
binary: once an `add` has overflowed once, DFG generates double arithmetic
unconditionally, *not* an overflow check + slow path.

## 5. OSR exit (tier-down)

Each speculative DFG node carries:
- The check (e.g. `Int32:@left`).
- A **stackmap** — for every value live in the optimized frame, the IR ref
  *or* the spill slot that holds it.

On check failure:
1. Jump to the per-exit "OSR exit stub".
2. Stub reconstructs the equivalent Baseline (or LLInt) frame: copies machine-
   register values to interpreter slots, materialises any sunken allocations.
3. Jumps to the Baseline machine code address corresponding to the bytecode
   index that was being executed.

The stub's reconstruction is data-driven — there is no per-exit hand-written
code. The DFG records *what* needs to live where; a generic stub *does* the
moves.

## 6. OSR entry (tier-up at a loop header)

Hard, and JSC supports it only at loop headers. When DFG compiles a function
whose loop hasn't terminated (per the profiler), it emits an alternative entry
that:
- accepts an unoptimized frame,
- type-checks the live variables against DFG's assumptions,
- copies them into the right machine registers / spill slots,
- jumps to the loop header.

If a check fails, it just stays in Baseline. The blog explicitly says OSR
entry "pessimises optimizations at any merge points between entrypoints" — so
adding entries has a code-quality cost.

## 7. Recompilation policy

After OSR exit, DFG counts exits. Threshold to jettison:
- normal exits: `100 * pow(2, R)`.
- exits triggered from a loop: `5 * pow(2, R)` (loops are bad places to bounce).

When the threshold is hit, the DFG (or FTL) blob is **jettisoned** — all
future calls fall back to Baseline. Eventually the execution counter
re-trips and the function gets recompiled, this time with the new exit
profile available so DFG knows not to speculate on the things that just
killed it. The recompile counter `R` increments, doubling all thresholds.

The blog gives a real example of a WSL compiler function going through 8
compiles before stabilising. **The system tolerates phase changes**: a
function that has different "hot types" at different points in the program
will repeatedly recompile until both phases are tier-stable.

## 8. DFG IR

CFG-based SSA. Nodes are typed and operations are *speculatively variant*:
```
CompareEq(Int32:@a, Int32:@b)        // integer fast path
CompareEq(String:@a, String:@b)      // memcmp loop
CompareEq(Object:@a, Object:@b)      // pointer compare
CompareEq(Untyped:@a, Untyped:@b)    // generic
```
The `Int32:` prefix is itself a **type guard** that exits if violated.

DFG deliberately omits the things that make compilation slow:
- no global register allocation (linear scan only),
- no escape analysis,
- no SSA loop optimizations,
- limited constant propagation.

Those live in FTL. Inlining, prediction propagation, CSE, basic dead-code,
and lowering of guarded operations all happen in DFG.

## 9. Watchpoints

Watchpoints are the dual of speculation. Instead of emitting a check, DFG
*registers* a callback with the runtime:
- "I've folded the value of global G; invalidate me if G is ever assigned."
- "I'm assuming class C has no subclasses; invalidate me on subclassing."

When the runtime triggers the watchpoint, DFG code is jettisoned immediately.
This costs nothing at steady state — *and* it lets DFG fold values that
guards alone could never observe.

## 10. Lessons for an Erlang T2

1. **Bytecode is the OSR pivot.** BEAM bytecode (or, more precisely, BEAM SSA
   labels) plays the same role JSC's bytecode does — every speculation must
   know how to reconstruct an interpretable state at a known BEAM label.
2. **Counters per function, not per call site.** JSC's single execution
   counter is simpler than per-callsite hotness and works well in practice.
3. **Profile saturation matters.** Don't tier up before the value profiles
   are populated; you'll just speculate on noise.
4. **Exponential backoff per recompile.** Phase changes happen; pay for them
   gracefully.
5. **The "value bet" enforces discipline.** Every speculation a designer
   wants to add has to clear `p ≈ 1`. That naturally pushes us toward
   speculations the AOT compiler can already nearly prove (e.g. integer
   arithmetic where the AOT type lattice already says `#t_integer{}`).
6. **Watchpoints > guards** for things the runtime can monitor cheaply
   (module reload, fun reload, code purge) — same pattern as JSC global
   variable folding.
7. **OSR exit machinery is generic and stackmap-driven.** Don't write per-
   exit stubs.
