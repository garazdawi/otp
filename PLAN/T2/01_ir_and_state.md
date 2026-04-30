# T2 — IR and State Preservation

> Part of the T2 design. See [`README.md`](README.md) for the full
> document index. This file covers §§5–6: the T2 SSA IR (op
> categories, type lattice, where types come from, why a new IR
> rather than reusing BEAM SSA records) and the sync-point-based
> state-preservation model that makes deopt cheap.

## 5. The IR

A new C++ IR — call it **T2 IR** — defined in
`erts/emulator/beam/jit/t2/`. CFG-based SSA, designed to map cleanly
onto BEAM SSA ops with the additions needed for speculation,
unboxing, and explicit deopt support.

### 5.1 Structure

```cpp
struct T2Function {
    Vector<T2BasicBlock>  blocks;
    Vector<T2Value>       values;     // SSA values
    Vector<T2FrameState>  framestates;// only populated for inlined regions
    Vector<T2Watchpoint>  watchpoints;
    LoopInfo              loops;      // analysis result, see §10.5
};

struct T2BasicBlock {
    Vector<T2Op*>  ops;        // linear list of ops
    T2Op*          terminator; // branch / jump / switch / return
    Vector<T2Op*>  phis;       // explicit phi nodes at block entry
};

struct T2Op {
    T2OpKind  kind;
    T2Type    type;          // result type (lattice; see §5.3)
    Vector<T2Value*> operands;
    T2Value*  result;        // SSA result (NULL for side-effect-only ops)
    union {
        T2OpAttrs attrs;     // op-specific (literal value, MFA, etc.)
        T2FrameStateRef fs;  // for ops that may deopt inside an inlined region
    };
};
```

Every value carries a type from the lattice (§5.3). Every op that may
deopt and is inside an inlined region carries a `T2FrameStateRef`.

### 5.2 Op categories (Phase A — MVP)

The op kinds below cover the Phase A subset (§17). Phases B–E add
maps, exceptions, binary, message-passing, NIFs, floats.

**Constants and parameters:**
```
%v = const_int  N
%v = const_float F
%v = const_atom A
%v = const_nil
%v = const_literal idx        // literal table reference
%v = param      idx           // function parameter (tied to X register at entry)
%v = phi        [v1,B1] [v2,B2] …
```

**Type tests** (produce boolean):
```
%v = is_integer %a
%v = is_atom    %a
%v = is_tuple   %a
%v = is_list    %a
%v = is_nonempty_list %a
%v = is_nil     %a
%v = is_function %a, arity
%v = is_tagged_tuple %a, arity, atom    // record check
…
```

**Comparisons:**
```
%v = cmp_eq_exact %a, %b      // =:=
%v = cmp_lt %a, %b
%v = cmp_le %a, %b
…
```

**Generic arithmetic** (may allocate bignum, may raise):
```
%v = add  %a, %b
%v = sub  %a, %b
%v = mul  %a, %b
%v = idiv %a, %b
%v = rem  %a, %b
%v = band %a, %b
…
```

**Speculative arithmetic** (only after `speculate_type`/`speculate_range`):
```
%v = untag_int %a              // tagged → raw machine word
%v = tag_int   %a              // raw → tagged small
%v = add_small %tagged, %raw   // one-untag trick (§9.4)
%v = sub_small %tagged, %raw
%v = mul_raw   %raw_a, %raw_b  // both operands untagged
```

**Speculation:**
```
speculate_type  %a, expected_type, deopt: <fs|jump_to_T1>
speculate_range %a, min, max,      deopt: <fs|jump_to_T1>
```
Both ops act as type-narrowing assertions. In the *outer function*
the deopt is "jump to T1 at this BEAM instruction" — no framestate.
In an *inlined region* the deopt uses an attached framestate to
restore outer-function X/Y state, then jumps to T1's call instruction.
See §6, §9.

**Tuples and lists:**
```
%v = make_tuple [%e1, %e2, …]
%v = get_tuple_element %t, idx
%v = make_list  %hd, %tl
%v = get_hd     %list
%v = get_tl     %list
```

**Funs and calls:**
```
%v = call      <mfa>, [%args…]      // statically-known target
%v = call_ext  <mfa>, [%args…]      // remote (export-table dispatch)
%v = call_fun  %fun,  [%args…]      // closure
       tail_call <mfa>, …
       tail_call_ext <mfa>, …
       tail_call_fun %fun, …
%v = bif       <name>, [%args…]      // BIF (may GC; outer function has X-reg flush)
%v = guard_bif <name>, [%args…]      // guard-context BIF (failure → branch, not exception)
%v = make_fun  idx, [%env…]
```

**Control flow:**
```
branch %cond, then: B1, else: B2
jump   B
switch %v, [val: B …], default: Bd
return %v
```

**Process / runtime:**
```
gc_test          words
reduction_check  cost           // §12.4
schedule_out                    // emitted at the same points T1 yields (§12.4)
framestate       [%v→x0, %v→y2, …], ip: beam_pc, parent_fs: <ref>
```

The `framestate` op is a marker, not generated code. It records, for
a particular BEAM instruction boundary, which SSA values would be in
which X/Y slots if T1 were executing this point. Used **only** for
deopt targets inside inlined regions (§9.2). Stacks chain through
`parent_fs` for nested inlining.

### 5.3 The type lattice

A C++ port of `lib/compiler/src/beam_types.hrl`, retaining the same
shape:

```
                          any
            /      |       |      |        \
       number   atom   container  identifier  bs_*
       /  \      |     /  | \  \    /|\ \
   integer float boolean t l c m  pid port ref ...
    [min,max]    [val]   |   |
```

Refinements:
- `integer{min, max}` — bounded range.
- `atom{value}` — singleton atom (or a small set up to N).
- `tuple{arity, exact, [T0, T1, …]}` — known shape and element types.
- `list` ∋ `cons` | `nil` | `cons|nil`.
- `fun{arity, target_mfa}` — known fun identity (drives constant-fun
  inlining; see §10.2).

Reuses limits from the AOT lattice (`?ATOM_SET_SIZE = 5`,
`?TUPLE_ELEMENT_LIMIT = 12`, etc.) — same constants, ported.

`meet` (intersection / narrowing) and `join` (union / widening) are
the standard SSA dataflow operations.

### 5.4 Where types come from

Three sources, in priority order:

1. **AOT-emitted types**: the BEAM file's type chunk (already exists
   for `beam_ssa_type` output) gives us a starting point per
   BEAM-SSA value.
2. **Profile feedback**: the type feedback vector (§7) narrows
   exported parameters and call-return values when monomorphic.
3. **Forward dataflow inside T2**: a pass identical in shape to
   `beam_ssa_type:opt_continue/4`, ported to C++.

Speculation (`speculate_type`) is inserted only where (2) shows a
monomorphic profile and (1)+(3) don't already prove the type.

### 5.5 Why a new IR rather than reusing BEAM SSA records

We considered reusing `#b_set{}` records directly with extensions.
That's the natural fit *if* the optimizer is in Erlang. With a C++
optimizer, marshalling Erlang terms in/out of the optimizer at every
boundary is excessive overhead — both implementation and compile
time. A native C++ IR keeps everything in one place, achieves the
~1ms compile target, and matches the existing JIT codebase.

The new IR is *defined to be 1:1 with BEAM SSA in Phase A* (every
BEAM-SSA op kind has exactly one T2 IR op kind). The new ops are the
ones BEAM SSA can't represent: `speculate_*`, `untag_*`, `tag_*`,
`add_small`/`mul_raw`, `framestate`, the loop-recovery preheader.
This keeps the maintenance story clean — adding a new BEAM-SSA op
adds one new T2 IR op, and the IR translation stays mechanical.

## 6. The state-preservation model

**This section captures the central simplification.**

### 6.1 The principle

T2's outer-function machine state matches T1's **at every point
where T2 could exit or yield** — and only at those points. Between
exit points, T2 has its own register layout and the freedom to keep
SSA values in CPU registers across multiple BEAM ops without
flushing to X/Y.

The *exit/yield points* are the well-known set of places where T2
might transition control out of itself:

- Function entry (yield via `i_test_yield`; tracing breakpoint).
- Function call sites (caller's reduction check via
  `dispatch_return`; callee may yield/GC/raise).
- Returns to the caller.
- GC sites (any allocation, including `gc_test`, `make_*`,
  `bs_create_bin`, BIFs that may GC).
- BIF call boundaries (BIFs may yield, GC, or raise).
- Speculation guard sites (where deopt may fire).
- Tracing-relevant sites (call_trace, return_trace, line trace).
- Receive safe points.

At each such point the T2 code generator places a **state-sync
constraint**: the X/Y registers and HTOP/FCALLS must hold exactly
what T1 would hold at that point in the original BEAM code. The
register allocator honours these constraints. *Between* sync
points, the allocator is free.

In practice, X and Y registers need to be synced fairly often —
calls, GC, and yields are frequent — so the allocator's freedom is
real but bounded. Within a straight-line stretch between two sync
points (e.g. `get_tuple_element` → `is_integer` → comparison), the
intermediate values can live in any CPU register, untagged where
profitable, without ever touching the X/Y array.

### 6.2 What this buys us

1. **Cheap deopt in the outer function.** At a sync point, X/Y
   already match T1, so the deopt stub is at most a few moves to
   re-tag any untagged scratch values back into X/Y, then a jump
   to T1's PC for that BEAM instruction. In the common case
   (sync point happens to be at a call boundary where T1 also
   has nothing in scratch) the stub is a single `jmp`.
2. **Tracing, GC, scheduler re-entry continue to work.** All of
   those fire at sync points by construction — the patchable
   prologue is at function entry (a sync point), GC fires at
   `test_heap` (a sync point), trace breakpoints sit at call
   sites (sync points). T2 looks like T1 at every point any
   runtime mechanism could observe it.
3. **Real register allocator in the outer function**, but
   constrained: live ranges must end (or the value must be
   spilled to its T1-mandated X/Y slot) before each sync point.
   The allocator gets meaningful freedom on the straight-line
   sections between sync points and pays nothing at the sync
   points themselves.
4. **Real allocator freedom in v1.** Sync points come from a
   fixed list of op kinds (function entry, calls, returns, GC
   sites, BIF boundaries, speculation guards, tracing-relevant
   sites, receive safe points). Identifying them is a small
   dataflow analysis; honouring them is a register-allocation
   constraint. Between sync points the allocator is free to keep
   SSA values in CPU registers, untagged where profitable. This
   is the central source of T2 wins in the outer function — not
   a "later phase" relaxation.

### 6.3 What it constrains

The optimizer needs to *identify all sync points up front* before
the register allocator runs. The identification happens during
T2 IR construction: as we walk the original BEAM SSA, each op that
maps to one of §6.1's categories produces a T2 IR op marked as a
sync point (the marking is metadata on the IR op, not a separate
op). The set is closed and bounded — the union of "ops that can
yield/GC/raise" plus "ops where speculation may deopt" plus "ops
where the runtime may inspect state".

**Static vs data-dependent.** Some BEAM ops *conditionally*
yield/GC/raise depending on runtime values: `bs_match`, NIFs that
may trap, BIFs whose trapping is data-dependent (`length/1` on
long lists, arithmetic that overflows into bignum). For these we
take the **static** decision: treat them as sync points
unconditionally. Distinguishing the no-trap branch dynamically
adds allocator complexity for marginal gain — most of these sites
either trap rarely (no allocator pressure to recover) or the
inliner has already replaced the call with a constant (in which
case there's no call site at all).

Optimizations that move work across sync points must respect the
constraints. Concretely:

- Hoisting work above a GC site is fine (the hoisted result has
  to live somewhere; if it's in X, it must hold a valid term).
- CSE that crosses a sync point is fine if the value is in a
  register at the sync point (so it survives) or already lives
  in an X register that the sync constraint pins.
- Deferring an X-register flush across a sync point is *not*
  allowed.
- Reusing an X register for a different SSA value across a sync
  point is *not* allowed (it would change the sync state).

### 6.4 Why the sync-point model

Many BEAM op pairs have no sync point between them —
`get_tuple_element` followed by a comparison, arithmetic followed
by a tuple build, a chain of `is_*` guards. At those interior
boundaries T1's X/Y state is essentially "this value could be in
this X reg" — but nothing observes the X reg between the two ops.
Forcing T2 to maintain T1's exact mapping at those interior
boundaries pays nothing and constrains the allocator unnecessarily.

The sync-point model gives correctness with strictly more allocator
freedom, at the cost of a small dataflow pass and stackmap-style
metadata recording the live X-reg contents at each sync point.

### 6.5 Inlined regions

When T2 inlines a callee at call site C in the outer function, the
inlined region runs in a "remapped" world:
- The callee's X registers map to scratch CPU regs or fresh stack
  slots, **not** the outer function's X registers.
- The callee's parameters are pre-loaded from the outer function's
  X registers at C, then live in their remapped homes for the
  duration of the inlined region.
- A single **framestate** is emitted at C, recording the outer
  function's X/Y state at instruction C.
- All speculations within the inlined region deopt via this
  framestate: restore outer-function X/Y state at C, then jump
  to T1's code at instruction C, which performs the original
  non-inlined call.

When the inlined region completes normally:
- The callee's return value is placed in whatever X register T1
  expects after instruction C. The outer-function abstract state
  is now valid again, and execution continues in T2 outer code.

Nested inlining: each inlined region pushes its parent CP at region
entry (eager-CP — see §9.2 in `03_compilation_and_speculation.md`). Codegen-time
framestate metadata chains via `parent_fs` to record the live-X-reg
map at each outer level, but no runtime CP materialisation is needed
— the CP frames are already on the Erlang stack from the eager
pushes. Deopt restores X/Y state and branches to the outermost
call's T1 PC.

### 6.6 Worked example

Erlang:
```erlang
double_all(L) -> lists:map(fun(X) -> X * 2 end, L).
```

After T2 inlining + constant-fun propagation + tail-recursion → loop
recovery (§10.5):

```
function double_all/1:
  entry(%list):                                  // outer: state = T1
    framestate [%list→x0], ip: double_all/1+0
    jump loop_header

  loop_header:                                   // INLINED region begins
    %l   = phi [%list, entry], [%tl, body]
    %acc = phi [const_nil, entry], [%cell, body]
    %is_cons = is_nonempty_list %l
    branch %is_cons, then: body, else: done

  body:
    %hd = get_hd %l
    %tl = get_tl %l
    // closure body inlined: fun(X) -> X * 2 end
    speculate_type  %hd, small_int,    deopt: framestate
    speculate_range %hd, MIN/2, MAX/2, deopt: framestate
    %hd_raw  = untag_int %hd
    %two     = const_int 2
    %prod    = mul_raw %hd_raw, %two
    %elem    = tag_int %prod
    gc_test 2
    %cell = make_list %elem, %acc
    reduction_check 1
    jump loop_header

  done:                                          // INLINED region ends
    %result = call_ext lists:reverse/1, [%acc]   // sketch — actual lowering
                                                  //  builds forward, see §10.7
    return %result                               // %result → x0 per T1 convention
```

The single `framestate` at entry is the only deopt machinery in this
function. If any speculation fails — say a list element is a string
— the deopt path restores `%list → x0` and jumps to T1's compiled
`lists:map/2` call site. Correct, slower, no machine-state surgery.

