# T2 — Profiling, Compilation, and Speculation

> Part of the T2 design. See [`README.md`](README.md) for the full
> document index. This file covers §§7–9, the data-flow story:
> profiling collects feedback in T1 → the compilation pipeline
> consumes it in T2 → speculation, guards, and deopt close the loop.

## 7. Profiling infrastructure

T1 changes. All cheap, all *additive*, all gated on tier-2-eligibility:

### 7.1 Eligibility check at module load

When BeamAsm loads a module, it scans each function's BEAM ops. If
all ops are in the **supported phase set** (currently Phase A; grows
as coverage extends — see §17), the function is *eligible*.

Eligible functions get:
- A 32-bit call counter.
- A type feedback vector (sized by number of profile points).

Ineligible functions get **nothing** — no counter, no feedback
vector. Zero overhead, no possibility of T2 ever touching them.

### 7.2 Type feedback vector

Per-function array of type-profile slots:

```c
typedef struct {
    Uint16 seen_types;   // bitmask of BeamTypeId flags observed
    Uint16 count;        // saturating count, caps at 65535
} T2ProfileSlot;

typedef struct {
    Uint32         call_count;
    Uint16         num_slots;
    T2ProfileSlot  slots[];
} T2FunctionProfile;
```

`seen_types` is a bitmask using the existing `BeamTypeId` flags
(`TYPE_INTEGER`, `TYPE_FLOAT`, `TYPE_ATOM`, `TYPE_LIST`,
`TYPE_TUPLE`, `TYPE_BINARY`, `TYPE_MAP`, …). Monomorphic = one bit
set; polymorphic = two or three; megamorphic = four or more.

### 7.3 Where to profile

T1 emits lightweight type recording at four kinds of program point:

1. **Function entry** — one slot per parameter.
2. **Before generic arithmetic** — operand types for `add`, `sub`,
   `mul`, etc.
3. **Call-return values** — type of the value returned by each call.
4. **`switch`/`select_val` arguments** — type of the value being
   matched.

Each profiling site is a single read-or-modify-write into the
feedback vector — measured cost target ~2 ns per site on aarch64
(**actually measured in Phase 0** against the F2 corpus on Apple
Silicon and Linux ARM64; under cache miss costs can rise to
50 ns, so the realistic budget includes a cache-miss factor).

```
; aarch64: profile slot for parameter %x (slot 0)
ldr   tmp, [feedback_base, #0]      ; .seen_types
orr   tmp, tmp, #type_tag_of_x
str   tmp, [feedback_base, #0]
; saturating count (cheap if no atomic needed; this is per-process,
; per-function so racy increments are tolerable)
ldrh  cnt, [feedback_base, #2]
add   cnt, cnt, #1
strh  cnt, [feedback_base, #2]
```

**Per-scheduler shards + scheduler-1-only profiling.** To avoid
cross-scheduler false sharing on the feedback vector, the slot is
sharded per scheduler (one cache-line-aligned vector per
scheduler). Only scheduler 1 emits profile updates; the OTP
scheduler's load-compaction behaviour puts hot code on
scheduler 1 over time, so this captures the bulk of the workload
without paying the contention cost. The JIT manager reads the
union of shards at tier-up. Cost of reading at tier-up time is
amortised; cost of writing per call is the single-shard
six-instruction sequence above with no contention.

### 7.4 Call counter

Lives in the same per-function side table (§7.1). Threshold scaled
per JSC's formula (§15). Threshold trip enqueues a compile request
with the T2 manager; counter resets to a "pending compile" sentinel
to suppress duplicate enqueues.

### 7.5 Per-call-site monomorphic-target slot (Phase 3)

T2's inliner needs to know which target a call site reaches —
information the type feedback vector doesn't capture. Add one
slot per call/apply/call_fun site:

| Site kind | Slot contents |
|-----------|---------------|
| `call_ext` | `Export*` of last-seen target, or `POLY` after a second distinct target |
| `call_fun` | Fun's underlying code pointer, or `POLY` |
| `apply/3`  | `{M,F,A}` triple (interned pointer), or `POLY` |

Update sequence per call: load slot; compare; conditional-store
(if currently zero, store; if equal, no-op; if different,
CAS to `POLY`). ~2 ns per call; comparable to the type-bitmask
slots. Lives in the same per-function metadata block.

This is the profile half of an inline cache without the
in-place-patching half. T2 reads the slot at compile time:
monomorphic → inline that target with a guard; polymorphic →
leave the call alone (full PIC-style polymorphic specialisation
is v2; see §17).

### 7.6 Map-shape feedback (Phase 5)

When map operations (`get_map_element`, `map_get`, `map_update`,
`is_map_key`) come into T2's supported set in Phase 5, add a
shape-feedback slot per access site:

| Site | Slot contents |
|------|---------------|
| `get_map_element`, `map_get` | Hash-consed shape pointer of last-seen map, or `POLY` |
| `map_update`, `is_map_key`   | Same |

T2 uses this for **region-level shape specialisation**: a single
shape guard at the entry to a hot region with multiple map
accesses, after which all accesses are direct offset loads with
no per-site checking. Shape mismatch deopts to T1, which carries
the generic code unchanged.

This subsumes what idea #6 (`lukas/erts/implement-inline-caches`)
was trying to do for map keys at T1 — but replaces in-place IC
patching with simple feedback collection. The T2 specialisation
gives the optimization a much bigger payoff (compounding across
inlined regions, deopt-on-mismatch eliminating the slow path
from hot code) than IC-patching at T1 alone could deliver.

### 7.7 Branch-frequency counters (v2)

For `is_*`, `select_val`, `select_arity` sites — a 16-bit taken/
not-taken counter pair. Used to bias branch ordering and prune
dead arms under speculation. v2 because the AOT compiler's
`ssa_opt_dead` already covers most of what this would unlock; the
v1 wins are elsewhere.

### 7.8 SSA-in-BEAM-file

T2 needs the function's BEAM SSA at runtime. `+type_information`
embeds *type signatures* but not the SSA itself. We add:

1. **AOT compiler change.** A new BEAM chunk (`"SSA "`) carrying
   `#b_function{}` records (post-optimization, pre-codegen) for
   each function, gated by a new compile option.
2. **Loader change.** Read the chunk on module load; store keyed
   by `{Mod,Fun,Arity}` for the T2 manager to retrieve.
3. **C API**: `erts_beam_get_ssa(Mod,Fun,Arity)` returns the SSA
   in a form the T2 IR builder can consume directly (parsed once
   at load time into an in-memory C++ representation, *not* an
   Erlang term roundtrip per compile).
4. **Compile option** (`+t2_compile_eligible` or similar) is
   **mandatory** for any T2-eligible module. Modules without it
   are unconditionally T1, regardless of phase coverage.

Chunk size impact: estimated 30–80 % of the existing code chunk per
module. Acceptable for opt-in.

## 8. Compilation pipeline

The T2 optimizer is a pipeline of passes over the function's IR.
All in C++. Target compile time: ~1 ms median per function, with a
hard cap at ~10 ms (compile abort + leave function on T1).

### 8.1 Pass list (Phase A)

```
   T2Manager::compile({M,F,A}, profile)
     │
     ├─ 1. T2IR builder
     │       BEAM SSA records → T2 IR; tied X/Y reg layout to T1's
     │
     ├─ 2. Type inference (forward dataflow on the lattice)
     │       Identical algorithm to beam_ssa_type, ported to C++
     │
     ├─ 3. Speculation insertion
     │       For each profile-monomorphic site, emit speculate_type
     │       (and speculate_range where arithmetic lowering needs it)
     │
     ├─ 4. Inlining (§10)
     │       - constant-fun-target call_fun → inline body
     │       - jit_inline-annotated callees with constant fun → inline
     │       - tail-recursion → loop recovery
     │       - register framestates at each inlined call site
     │
     ├─ 5. Re-run type inference
     │       (now sees through inlined bodies, narrows further)
     │
     ├─ 6. Constant folding
     ├─ 7. CSE
     ├─ 8. DCE
     ├─ 9. Guard strength reduction
     │       remove is_TYPE checks proven by inferred type
     │
     ├─ 10. Loop info analysis (LoopInfo-style, see §10.5)
     ├─ 11. LICM
     ├─ 12. Loop unrolling (§10.6)
     │       re-place test_heap after unrolling for coalescing
     │
     ├─ 13. Allocation sinking
     ├─ 14. Speculative arithmetic lowering
     │       generic add/sub/mul where range proven safe →
     │       untag + add_small / mul_raw
     │
     ├─ 15. Direct lowering to asmjit
     │       - outer function: emit code that produces T1's exact
     │         X/Y state at every instruction boundary
     │       - inlined regions: free use of scratch regs, untagged
     │         temporaries
     │       - speculate_* outer = single jcc to T1 PC
     │       - speculate_* inlined = single jcc to deopt stub that
     │         consults framestate
     │
     └─ 16. Install blob
             - allocate from T2 code cache
             - register watchpoints for inlined callees / folded
               constants from other modules
             - patch Export.addressv
```

### 8.2 Pass selection rationale

Which AOT-style passes do we run, and why?

The AOT compiler already does CSE, DCE, dead-code, type narrowing,
and most of the rest. T2 mostly **redoes them after the inlining
pass** because inlining creates new opportunities the AOT couldn't
see. Pass 2 (initial type inference) confirms what the AOT proved;
the *interesting* type inference is pass 5, after inlining.

LICM and loop unrolling are Erlang-specific: the AOT can't do them
across an inlined `lists:foldl/3`, but T2 can.

### 8.3 What we deliberately don't do in v1

- Polymorphic call-site specialisation (PIC switch).
- Speculative fun specialisation from runtime PIC observation
  (Sista FullBlockClosure case; v2).
- Induction-variable analysis (Erlang doesn't iterate flat arrays;
  weak applicability — §10.6).
- Cross-module deep optimization beyond what watchpoints can
  invalidate.

### 8.4 Compile errors and abort policy

Compiles can fail in well-defined ways. Each has a defined fallback:

- **T2 IR validation assertion** — indicates a bug in the
  optimizer. **Abort the BEAM** with a runtime crash; this is not
  a recoverable condition.
- **asmjit backend rejects an emitter sequence** — bug in the
  lowering. **Abort the BEAM**.
- **Watchpoint registration races a literal-table change** — bug
  in the JIT-server's reload sync. **Abort the BEAM**.
- **OOM in the compile thread** — out of address space for the
  T2 code cache. Recoverable: free the largest evictable blob
  (§13.2) and retry once; if still failing, leave the function
  on T1 and increment the saturating `oom_compile_failures`
  counter.
- **Encountering an op outside the supported phase set** —
  expected during phase ramp-up. Blacklist the function for the
  lifetime of the loaded module (cleared on module reload); log
  to `t2_compile_failures_by_function` (§16).
- **10 ms compile-time cap exceeded** — leave on T1; increment
  saturating `compile_timeout` counter; do not blacklist (the
  function may compile fine after the cache warms).

Repeated compile failure for the same `{M,F,A}` (other than
"unsupported op" which blacklists immediately): blacklist after
N=3 retries.

## 9. Speculation, guards, and deopt

Two distinct deopt cases, both very simple in this design.

### 9.1 Outer-function deopt

**Mechanism**: at any BEAM instruction boundary, the outer function
maintains T1's exact X/Y state (§6). A `speculate_type` or
`speculate_range` failure at boundary K compiles to:

```
  ; speculate_type %a, small_int  (in outer function)
  test  reg_a, TAG_MASK
  jne   .t1_pc_for_K              ; cold tail, statically scheduled
```

`t1_pc_for_K` is the T1 BeamAsm address of the BEAM instruction
the speculate guards. No stub, no metadata table lookup, no
framestate reconstruction.

The cold-tail address is resolved at T2 codegen time by reading the
T1 blob's per-instruction PC table (which BeamAsm already maintains
for line-number debugging). This is the *only* T1↔T2 cross-reference,
and it's resolved once at codegen, not per deopt.

### 9.2 Inlined-region deopt — eager-CP-push

**Mechanism**: when the inliner expands a call at site C, it emits
an *eager CP push* at the inlined-region entry — pushing the
parent CP onto the Erlang stack as if the call had really happened.
Erlang's CP is 1 word (2 with frame pointers), so the steady-state
cost is ~1–2 instructions per inlined call.

A `framestate` at C records, *for codegen only*, the outer SSA
values mapped to outer X/Y slots at C:
`[%out_v0 → x0, %out_v1 → x1, …]`. Nested inlining chains
framestates via `parent_fs`. The framestate is consumed at codegen
to emit the deopt stub's X/Y-restore moves; it isn't a runtime
data structure (see §13.3).

A `speculate_*` failure inside the inlined region compiles to:

```
  ; speculate_type %a, small_int  (in inlined region)
  test  scratch_a, TAG_MASK
  b.ne  .deopt_stub_C
```

The deopt stub at `.deopt_stub_C`:

```
.deopt_stub_C:
  ; X/Y restore — codegen-emitted moves into T1's expected layout.
  ; On aarch64, XREG0..XREG3 live in x25..x28 (BeamAsm ABI; see
  ; arm/beam_asm.hpp); higher-numbered Xs go to the in-process X
  ; array.
  mov   x25, scratch_for_x0       ; xreg0 from scratch (re-tagged
                                  ; if the value was untagged)
  mov   x26, scratch_for_x1
  str   scratch_for_x4, [x_reg_array + 32]    ; overflow Xs
  ...
  ; HTOP and FCALLS are already in correct state — sync-point
  ; invariant (§6) makes C a sync point with consistent HTOP
  ; and FCALLS already ticked per inlined call (§12.4).
  ; CP frames are already on the Erlang stack from the eager push.
  b     .t1_pc_for_C        ; T1 re-executes the original call
```

The stub size is uniform regardless of inlining depth: roughly
5–10 X/Y-restore instructions plus the final branch. No CP
materialisation at deopt time; no chain walk.

### 9.3 The deopt-at-sync-point constraint

Deopt is only legal at *sync points* (§6.1 in `01_ir_and_state.md`):
function entry, call sites, returns, GC sites, BIF boundaries,
speculation guards, tracing-relevant points, and receive safe points.
These are the points where a valid BEAM-machine state can be
reconstructed (the live-X-reg map is recorded). Mid-arithmetic
(between `untag_int` and `add_small`) deopt is impossible because
registers contain raw machine values that don't correspond to any
valid BEAM state — but mid-arithmetic also can't be a sync point by
construction, so the constraint composes naturally.

Consequence: speculations on operand ranges must happen *before* the
operation. `speculate_range` checks that the operand is in a range
where the upcoming arithmetic provably cannot overflow. Pass it →
arithmetic is unconditionally safe, no deopt needed afterwards.
Fail it → deopt at the boundary, T1 handles the operation.

This is why `speculate_range` exists alongside `speculate_type`. It
moves the overflow check *out* of the arithmetic (where deopt would
be impossible) and *up* to the previous instruction boundary (where
deopt is legal).

### 9.4 The one-untag arithmetic trick

Erlang small-integer tags occupy the low bits. For addition:
```
  tagged + untagged = tagged_result
```
Adding a tagged operand to an untagged operand preserves the tag
through the addition (tag bits + 0 = tag bits). So:

```
  speculate_type  %a, small_int
  speculate_type  %b, small_int
  speculate_range %a, -2^58, +2^58     ; leaves headroom for sum
  speculate_range %b, -2^58, +2^58
  ; --- past speculation: arithmetic provably cannot overflow ---
  %b_raw    = untag_int %b
  %sum      = add_small %a, %b_raw     ; %sum is already tagged
```

(BEAM small-integer range on 64-bit is ±2^59 — 4 tag bits in a
64-bit word. Speculating to ±2^58 leaves one bit of headroom so
the sum of two values still fits in the small range. Tighter
ranges than ±2^58 are fine; wider would re-introduce the overflow
check.)

Multiplication can't use this trick (tag bits would scale), so
`mul_raw` requires both operands untagged, then re-tags:
```
  %a_raw    = untag_int %a
  %b_raw    = untag_int %b
  %prod_raw = mul_raw %a_raw, %b_raw
  %prod     = tag_int %prod_raw
```

This matches BeamAsm's existing small-int fast path (`arm/instr_arith.cpp`).
T2's contribution is moving the type-and-range checks to a single
guard at the previous boundary, instead of doing them per-op. After
the guard, the arithmetic is straight-line native instructions.

### 9.5 Recompilation policy

Each T2 blob has an exit counter. Threshold: `100 * 2^R` where R is
the number of times this function has been recompiled (JSC).

Threshold hit → blob jettisoned (`Export.addressv` reverts to T1).
Profile counters reset. Function continues in T1 and may re-tier-up
later, this time with knowledge of which speculation killed it.

**Exit-reason buffer** (per blob). A flat array indexed by
*speculation-site ID*, one entry per `speculate_*` op in the IR:

```c
typedef struct {
    Uint16 fail_count;          // saturating
    Uint16 last_seen_type_mask; // BeamTypeId bitmask
} T2ExitReason;
```

~4 bytes × <100 sites/blob ≈ ~400 B. On recompile, the optimizer
reads the array: any site with `fail_count > T` gets its
speculation widened to the union of `last_seen_type_mask`, or
dropped entirely.

**Loop-vs-non-loop weighting.** Each speculation site carries a
static `is_in_loop` flag (computed from the loop info, §10.5):

- Outer-function exits, non-loop: `100 * 2^R` budget.
- Outer-function exits, in loop: `25 * 2^R`.
- Inlined-region exits, non-loop: `50 * 2^R`.
- Inlined-region exits, in loop: **drop the inline at this site
  on recompile** rather than just retrying with the same
  inlining. Loop-resident inline failures are the worst case;
  the inlining decision itself was wrong.

### 9.6 Funs: only the speculative case is hard

The Sista FullBlockClosure deopt complexity arises from *speculating*
on a runtime fun's identity. If observation lies, deopt has to
materialise the original fun, reconstruct the indirect `call_fun`,
and unwind. That's the v2 polymorphic-PIC scenario.

The constant-fun inlining case (where a `call_fun` site has a
target **provably constant** via SSA constant propagation, e.g. a
literal fun threaded into an inlined `lists:foldl/3`) is in v1 —
see §10.1. It requires no speculation and no extra deopt
machinery. The hard case (speculative inlining based on observed-
but-not-provably-constant fun identity) is v2.

Audit list for safe constant-fun inlining (Phase 0):
- `erts/emulator/beam/erl_fun.h`/`erl_fun.c` — fun representation.
- `make_fun3` and `call_fun` instructions in `arm/instr_fun.cpp`.
- Closure-capture invariants — when inlining the fun's body, the
  inliner must thread captured environment values correctly. For
  funs that capture nothing (the common case), trivial. For funs
  that capture, the captures live as fresh SSA values produced at
  the `make_fun` site; the inliner threads them into the inlined
  body.

