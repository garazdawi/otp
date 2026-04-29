# YJIT — Lazy Basic Block Versioning for Ruby

Sources:
- ["YJIT: A Basic Block Versioning JIT Compiler for CRuby" — Chevalier-Boisvert et al., VMIL 2021](https://dl.acm.org/doi/10.1145/3486606.3486781)
- ["Evaluating YJIT's Performance in a Production Context" — MPLR 2023](https://dl.acm.org/doi/pdf/10.1145/3617651.3622982)
- [`ruby/doc/jit/yjit.md`](https://github.com/ruby/ruby/blob/master/doc/jit/yjit.md)
- "Simple and Effective Type Check Removal through Lazy Basic Block Versioning" — Chevalier-Boisvert & Feeley, ECOOP 2015 (foundational LBBV paper).

YJIT is interesting prior art because it has explicitly *not* gone the
"build an SSA IR, do a few passes, lower to native" route. Instead it
specialises basic blocks lazily, generating native code directly from
YARV bytecode with no IR at all. The trade-offs are a useful contrast to
the Maglev/JSC-DFG style.

## 1. Lazy Basic Block Versioning (LBBV)

The core idea:
- Compile *one basic block at a time*, on demand, when control flow
  actually reaches it.
- Each block can have multiple **versions** specialised on a *context* —
  the inferred type of values on the operand stack and locals at that
  block's entry.
- When generating a successor block, look up an existing version that
  matches the current context; otherwise compile a new version.

So instead of "type-check, then a single generic body", you get specialised
bodies per type combination. Type checks become *implicit* in which
version was chosen.

The 2015 ECOOP paper's claim is that this makes most explicit type checks
disappear with no separate type analysis pass — the type knowledge flows
naturally as you walk basic blocks.

## 2. No IR

YJIT translates YARV bytecode (`Iseq` = "instruction sequence") *directly*
to native code. There is no SSA IR, no peephole optimizer, no register
allocator in the traditional sense. Codegen lives in `yjit/src/codegen.rs`
in the Ruby tree.

The pros: *very* small, *very* fast to compile, easy to maintain.
The cons: limited to optimizations that can be done one-instruction-at-
a-time with local context. No CSE, no inlining, no constant folding
beyond what a peephole pass would catch. Performance ceiling is
correspondingly modest.

## 3. Tier-up policy

- Default threshold: a method must be called **30 times** before YJIT
  compiles it.
- After the global ISeq compile count crosses **40 000**, the threshold
  rises to **120** (don't waste code cache on infrequently-called code in
  large applications).
- A separate "cold" cap (~200 000 calls before any compilation) is
  available to suppress compilation of init-only code.

The user can call `RubyVM::YJIT.enable` *after* boot; that lazy-enable
strategy is now the recommended deployment pattern (it skips compiling
gem and framework startup code entirely).

## 4. Side exits, not deopt

When a runtime check fails (type mismatch, IC miss, etc.) YJIT branches
to a **side exit** — a small stub that restores the interpreter state and
jumps back into the YARV interpreter. There is no on-stack replacement
of optimized frames into baseline frames; YJIT's frames *are* the
baseline frames. The interpreter just resumes from the next bytecode.

This is much simpler than JSC-style OSR exit, and it's possible because
YJIT generates code with *the same* stack/register layout as the
interpreter, only specialised at branch points.

## 5. Code memory

- Default executable region: **128 MiB** (`--yjit-mem-size=128`).
- Code GC (off by default in Ruby 3.3): when the cap is hit, throw away
  *all* compiled code and start over. Trades latency at the GC for the
  ability to run with a smaller code cache.
- Statistics: `code_region_size`, `yjit_alloc_size`, `code_gc_count`.

## 6. Inlining

Effectively none. YJIT inlines a few primitive accessors at the YARV level
(via `optimized_call`-style instructions) but not user methods. The docs
explicitly warn users to avoid wrapper methods because each indirection is
visible.

## 7. Observability

YJIT collects unusually rich runtime stats (`RubyVM::YJIT.runtime_stats`):
- `yjit_insns_count` / `vm_insns_count` — coverage of bytecode by JIT vs
  interpreter (target: ~99%).
- `side_exit_count` — and per-reason exit counters (`exit_*`).
- `compiled_iseq_count`, `code_region_size`, `code_gc_count`.
- `--yjit-trace-exits` dumps backtraces of every exit site.

This level of observability is something we should plan from day one for
T2. It's how we'll diagnose "speculation looked great in microbenchmarks
but kills the production workload".

## 8. Reported numbers

15–19% on real workloads (Shopify production, Rails apps). Notable
because YJIT does not do speculative inlining or any heavy optimization —
just monomorphic-call specialisation, type-check elimination via LBBV,
and machine-code generation in place of bytecode dispatch.

## 9. Lessons for an Erlang T2

- **LBBV is a lightweight alternative to a full SSA-with-type-inference
  optimizer.** If a T2 design feels too heavyweight, LBBV is a fallback
  worth keeping in mind: each block is specialised by the types it sees,
  no offline analysis required.
- **The lack of OSR is a major simplifier.** YJIT's side-exit-to-
  interpreter model would translate to "side-exit-to-T1 BeamAsm code".
  T1 already exists, is correct, and has stable layout — *if* T2 keeps a
  compatible frame layout, deopt is just a branch.
- **The "fall back to T1" simplicity** comes at a cost: you can't inline
  aggressively because then T1 would have to reconstruct a callee frame
  from a T2 inlined region. JSC pays this cost; YJIT skips it by not
  inlining.
- **Eager profiling is unnecessary if specialisation happens lazily on
  block entry.** This is a strong argument against putting profiling
  counters on every call site at T1 if a T2 chose LBBV.
- **Code cache GC is a real production issue.** Whatever budget T2 gets,
  it will fill. Plan eviction policy from the start.

## 10. Why we should *not* base T2 on YJIT

- The biggest perf wins (per the prompt) come from **inlining** and
  **AOT-style optimization passes** — exactly what LBBV deliberately
  avoids.
- Erlang already has a sophisticated SSA pipeline (`beam_ssa_*`). Using
  it as the T2 IR gets us aggressive optimization for free; ignoring it
  in favour of LBBV throws away years of tuning work.
- LBBV's win is type-check elimination. The Erlang AOT compiler already
  does aggressive type-driven guard elimination via `beam_ssa_type`; the
  remaining wins are largely about *speculative* type assumptions a
  whole-program AOT cannot make.

YJIT is a *useful counterexample* — the fallback if the T2 SSA approach
turns out to be too expensive. Worth keeping the design notes around.
