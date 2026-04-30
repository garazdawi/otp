# T2 — Dispatch, Install/Uninstall, and Side-exits

> Part of the T2 design. See [`README.md`](README.md) for the full
> document index. This file covers the *concrete mechanics* of how a
> T2 blob is plugged into a running BEAM, how it is unplugged again,
> and exactly what happens at each side-exit. The high-level
> architecture lives in [`00_overview.md`](00_overview.md); the
> framestate/eager-CP-push design lives in
> [`03_compilation_and_speculation.md`](03_compilation_and_speculation.md);
> the runtime invariants this builds on (calling convention, GC,
> watchpoints, code cache lifecycle) live in
> [`05_runtime.md`](05_runtime.md).
>
> The MVP code referenced throughout — `t2_mvp_is_target`,
> `T2FunctionEntry`, `emit_t2_specializations`, `emit_t2_total_2` —
> lives in `erts/emulator/beam/jit/arm/beam_asm_module.cpp`.

## Why this file exists

Two things kept getting waved at in the design without being pinned
down:

1. *"T2 takes over the function."* Where exactly does the install
   happen? `Export.addressv` is the obvious answer, but it is
   wrong: BeamAsm bakes intra-module calls as direct relative
   branches that bypass the export entry entirely. Patching only
   the export means external callers go to T2 while internal
   callers stay in T1 — half the function "tiered up", half not.
   The right install site is the *function prologue itself*, the
   same patchable region NIF loading and tracing already use.
   What happens to the in-flight callers? When does the change
   become visible across schedulers? How do trace, NIF, and T2
   coexist on the same prologue?

2. *"On a guard failure we side-exit to T1."*
   Where exactly does control land? What state does T1 expect when
   it lands there? How does that state arrive? The MVP implements
   one specific side-exit shape; the full design has four
   categories that look very different from each other.

Both questions hide failure modes that won't surface until late
phases. This file pins them down up-front.

## 1. Anatomy of a T1-compiled function

T1's per-function memory layout (the parts that matter for tier
switching):

```
   ErtsCodeMFA                       <-- function metadata
   --------------------------------  <-- public entry label `L_f`
   <breakpoint trampoline>           <-- 12 bytes, patchable prologue
   <i_test_yield>                    <-- 12 bytes, fixed offset
   --------------------------------  <-- TEST_YIELD_RETURN_OFFSET (= +24)
   <function body>                   <-- T1 SSA-lowered code
   ...
   <return / tail-call>
```

### 1.1 The patchable prologue, in detail

`emit_i_breakpoint_trampoline` (`arm/beam_asm_module.cpp`) lays
down four instructions:

```
  L_f + 0:   <enter_erlang_frame>     ; push frame pointer (4 B)
  L_f + 4:   b   next                 ; ★ PATCHABLE redirect (4 B)
  L_f + 8:   bl  i_breakpoint_trampoline_shared   ; (4 B)
  L_f + 12:  next:                    ; = BEAM_ASM_FUNC_PROLOGUE_SIZE
```

In the steady state, the `b next` at `+4` jumps over the `bl` and
control falls through into `i_test_yield` at `+12`. When tracing
or NIF-load patches the byte flag at `ErtsCodeInfo`, it also
patches `b next` → `b shared_call` so the `bl` runs and dispatches
on the flag. The trampoline shared fragment at line 176–229 of
`arm/beam_asm_module.cpp` already handles three flag bits:
`ERTS_ASM_BP_FLAG_BP`, `ERTS_ASM_BP_FLAG_CALL_NIF_EARLY`, and
`ERTS_ASM_BP_FLAG_BP_NIF_CALL_NIF_EARLY` (the combination).

This `b` at `L_f + 4` is *the* install/uninstall surface. It is
reached by every caller — external (via `Export.addressv`),
intra-module (via direct relative branch baked at AOT time),
funcall (via `Export.dispatch` from `ErlFunEntry`), apply, BIF
trap-out — because they all land at the public function label.
Patching it once redirects all of them.

**Crucially, the frame is already pushed by the time the `b` at
`+4` runs.** `enter_erlang_frame` at `L_f + 0` is the first
instruction every caller executes; the patched redirect runs
*after* that. So whoever the patched `b` lands at must *not* push
a frame again — it must assume the frame pointer is already on
the Erlang stack. This applies equally to the existing trace/NIF
shared trampoline (which runs `bl` not `bl-after-frame-push`) and
to the T2 entry stub we add below.

**T2 is mutually exclusive with trace/NIF on this byte.** The
existing flag bits combine (`BP_NIF` is a documented composition);
T2 does *not* compose with them. Either a function is T2-installed
*or* it has a breakpoint/NIF active, never both. This is a hard
rule: if any flag bit is set, T2 install is rejected; if T2 is
installed, trace install jettisons T2 first.

### 1.2 Other reference points

- **`Export.dispatch.addresses[ix]`**: external caller dispatch
  entry. `ix` is the active code index (`erts_active_code_ix()`).
  Points at `L_f`. *Not* T2's primary install surface — it only
  catches external callers, and patching it leaves the prologue
  byte (and thus intra-module calls) untouched. T2 leaves
  `addresses[ix]` alone.
- **`c_p->i` for a yielded process**: points at `L_f +
  TEST_YIELD_RETURN_OFFSET` (= `L_f + 24`). The
  `i_test_yield_shared` fragment computes this from
  `current_label + PROLOGUE_SIZE + 12` (see
  `arm/instr_common.cpp:emit_i_test_yield_shared`), hardcoding the
  invariant that `i_test_yield` immediately follows the prologue.
- **CPs on the Erlang stack**: return addresses point into the
  *caller*, not into the callee blob. Caller-side CPs only matter
  for tier-switching the *caller*. This is why the lazy stack-scan
  in §5 walks every process's stack on reload, regardless of which
  function reloaded.

T2 is layered on top of this layout, not in place of it.

## 2. T2 install: prologue patching, the NIF model

The install model mirrors NIF loading: patch the function's
prologue so every entry to the public label diverts to the
alternative implementation. We do *not* touch `Export.addressv` —
that wouldn't catch intra-module calls, which BeamAsm bakes as
direct relative branches at AOT time.

The T1 blob's bytes are otherwise immutable. The only mutation is
the single instruction at `L_f + 4` (the patchable `b next`) plus
the breakpoint flag byte in `ErtsCodeInfo`. That single-instruction
mutation is the entire install surface; uninstall reverses it.

### 2.1 What's freshly emitted

A separate T2 blob containing:
- the specialized body (e.g. `emit_t2_total_2`),
- a thin **T2 entry stub** at the head of the blob (see §2.3),
- the deopt stubs for every inlined-region speculation (§4.3),
- a per-blob CP-to-T1-PC side table for the lazy stack scan
  (`05_runtime.md:13.3`),
- the watchpoint registrations that bind this blob to upstream
  module state (`05_runtime.md:14.1`).

The T1 blob is read-only after this; the only pre-existing byte
that changes is `b next` at `L_f + 4` and the breakpoint flag.

### 2.2 The prologue patch

Single mutation: patch the redirect instruction at `L_f + 4`:

```
  b   next     ──→     b   <T2 reach target>
```

Where `<T2 reach target>` is either the T2 entry stub directly
(when in branch range — see §2.7) or a near-side trampoline that
forwards to it. We do *not* set any flag bit and do *not* go
through the shared `i_breakpoint_trampoline_shared` fragment;
T2 mutual-exclusion with bp/NIF (§1.1) means there is no flag-
based dispatch needed.

```
  before T2 install:                  after T2 install:
   L_f + 0:  enter_erlang_frame        L_f + 0:  enter_erlang_frame
   L_f + 4:  b   next       ──┐        L_f + 4:  b   <reach target>  ◄ patched
   L_f + 8:  bl  shared       │        L_f + 8:  bl  shared    (unreachable)
   L_f +12:  next: ...      ◄─┘        L_f +12:  next: i_test_yield (unreached)
```

This is a single aligned 4-byte store on aarch64. Visibility: the
write happens under the `code_ix` write lock; an icache flush + a
`dmb ish` follows so any CPU that observes the new instruction
also observes the new T2 blob. The window during which a CPU sees
the old `b next` while another is already executing T2 is tolerated
the same way existing tracing-patch races are tolerated — both
behaviours (T1 or T2) are correct; only the choice differs.

**Install precondition:** the JIT server reads the flag byte at
`ErtsCodeInfo.u.metadata.breakpoint_flag` and the instruction
word at `L_f + 4`. Install proceeds only if the flag byte is zero
(no bp/NIF active) *and* the instruction at `L_f + 4` is the
unmodified `b next`. Any other state means trace or NIF claimed
the prologue first; T2 install is rejected (the function stays on
T1, no error). See §2.6 for the full check list.

### 2.3 The T2 entry stub

The patched `b` (eventually — see §2.7 for the "via trampoline"
case) lands at the **T2 entry stub** at the head of the T2 blob.
Two crucial entry conditions, inherited from the patched-prologue
design:

- **The frame is already pushed.** `enter_erlang_frame` at
  `L_f + 0` ran before the patched `b`. The stub must *not*
  push another frame.
- **FCALLS is not yet decremented.** T1's `i_test_yield` at
  `L_f + 12` is unreached. The stub does its own decrement.

Therefore the stub is **shorter than T1's prologue+i_test_yield
combined** — it's i_test_yield only:

```
L_f_t2_entry_stub:                      ; frame already pushed
   adr    ARG3, L_f                     ; same MFA-derivation contract
   subs   FCALLS, FCALLS, #1
   b.le   t2_yield_setup
   ; fall through into T2 body
t2_body:
   ...                                  ; specialized body

t2_yield_setup:
   ; ARG3 = L_f, route through i_test_yield_shared. Resume PC
   ; computed by the shared fragment is L_f + TEST_YIELD_RETURN_OFFSET
   ; = L_f + 24 = T1 body start. A yielded T2 frame thus transparently
   ; demotes to T1 for the rest of its current invocation
   ; (`05_runtime.md` §12.4 alt-3). v2 may add T2-resume stubs.
   b      i_test_yield_shared
```

Why no `enter_erlang_frame`: re-pushing would corrupt the Erlang
stack. The patched-prologue model fundamentally trades "the T2
entry stub gets to choose its own prologue" for "every caller's
frame-push happens identically across tiers". This trade is the
right one — it eliminates an entire class of stack-discipline
divergence between T2 and T1.

### 2.4 Coexistence with tracing and NIFs — strict mutual exclusion

T2 *does not coexist* with tracing or NIFs on the same function.
There is no shared dispatch, no flag-bit switching, no combined
trampoline. The rule is enforced both ways:

- **Pre-existing trace or NIF blocks T2 install.** §2.2's
  preconditions reject the install. The function stays on T1.
  When trace clears, the function becomes T2-eligible again on
  its next counter trip.
- **T2-installed function blocks new trace/NIF install — by
  jettison.** When `erlang:trace_pattern/3` or `erlang:load_nif/2`
  targets a T2-installed function, the runtime first jettisons T2
  (revert the prologue patch, §5), *then* installs the trace or
  NIF as it normally would. The user-facing behaviour is "trace
  always wins"; T2 is invisible from the API.

This is much simpler than the flag-bit composition I'd previously
sketched. T2 doesn't extend `i_breakpoint_trampoline_shared`,
doesn't add a `_T2` flag bit, doesn't need a multi-arm dispatch.
It owns the prologue or it doesn't run there at all.

The simplification is real: the existing `BP`, `NIF`, `BP_AND_NIF`
combinations remain a closed set. Adding T2 to that universe
would have meant 8 distinct arm states (`{none, BP, NIF, BP_NIF}
× {T2_off, T2_on}`); the strict-exclusion rule reduces it back
to 4 arms with one extra precondition check.

### 2.5 In-flight callers of T1

A process currently executing inside the T1 body when the patch
goes in keeps executing T1. The patch only changes what happens
on *future entries* through the prologue. Existing CP frames
point at T1 return addresses; existing tail-call branches inside
the function body don't go through the prologue; the running
function finishes its current invocation in T1.

This is the entire reason the T1 blob stays intact: we need it
both as a side-exit target (§4) and as a "still running, finish
naturally" landing zone for in-flight callers. There is no
quiescence requirement at install time, no in-flight migration,
no atomic switchover beyond the single 4-byte store.

(Uninstall is harder — see §5 — because then we want to
*encourage* in-flight T2 callers to migrate back, not just stop
new entries.)

### 2.6 Install-time preconditions

Between "the JIT server decided to compile this function" and
"the T2 blob is ready to install", milliseconds-to-tens-of-ms
elapse. During that window the world can change: a watched module
may have reloaded, a trace may have been enabled on the function,
the T1 blob may have been purged, a NIF may have been loaded.

At install, the server checks (under the `code_ix` write lock):

1. **Prologue is in the unmodified T1 form.** The instruction at
   `L_f + 4` is the original `b next`. If it's anything else
   (`b shared` indicating bp/NIF active, or `b <some other
   target>` indicating concurrent T2 install — should not happen
   under the write lock, but checked defensively), abort the
   install. The T2 blob is dropped to the cache evictor.
2. **The breakpoint flag byte at `ErtsCodeInfo` is zero.** No
   bp/NIF is active. (Strict mutual exclusion, §2.4.)
3. **None of the watched modules' generation counters have
   changed** since the T2 IR was built. If any have, abort and
   re-queue.
4. **The function is not in the trace patternset.** (Trace
   install takes the same `code_ix` lock; mutual exclusion gives
   this for free, but the explicit check guards against a missed
   wakeup.)
5. **The T2 blob's reach target is allocated and within range.**
   See §2.7 — either the T2 entry stub is within ±128MB of
   `L_f + 4`, or a near-side trampoline has been emitted that is.
6. **The T1 PC table for side-exits** (§4.2) is populated and
   matches the T1 blob currently loaded.

All six checks happen under the `code_ix` write lock. The common
case is sub-microsecond.

### 2.7 Branch range and the T2 reach target

aarch64 unconditional `b` has ±128 MB reach. The single
instruction we patch into `L_f + 4` must reach the T2 entry stub
in that range, and the T2 cache may grow large or be allocated
arbitrarily far from any given module's code. We have three
options, in order of preference:

**(a) Co-located allocation: the default.** The T2 code cache is
allocated as one or more 32 MB blocks (matching BeamAsm's
`JitAllocator` block size, `05_runtime.md:13.1`) placed
adjacent to the BeamAsm code-cache region. With both caches in
the same ~256 MB neighbourhood, every direct `b` from any T1
prologue reaches every T2 stub. The allocator takes a *proximity
hint*: when growing the T2 cache, prefer pages near the BeamAsm
cache; only fall back to far placement if the neighbourhood is
exhausted (very large process; rare).

The `JitAllocator` API supports this via `mmap(MAP_FIXED_NOREPLACE)`
or platform-specific equivalents. Phase 0 measures actual
allocation distance on Linux ARM64, macOS aarch64, and Windows
arm64. The expectation is that for ≤ 256 MB total JIT memory the
co-location strategy works without trampolines.

**(b) Per-module trampoline pool.** When co-location fails (T2
blob lands > 128 MB from the T1 module), the JIT server allocates
a small **trampoline slot** within ±128 MB of the T1 module and
populates it with an absolute jump:

```
trampoline_for_L_f:                     ; near the T1 module
   ldr   x16, =L_f_t2_entry_stub        ; literal pool — 4 bytes for
                                        ; the load + 8-byte aligned
                                        ; absolute target nearby
   br    x16                            ; absolute indirect branch
```

The patched `b` at `L_f + 4` targets `trampoline_for_L_f`
(in-range) and the trampoline forwards to the T2 stub
(unrestricted range). This is the same veneer pattern BeamAsm
already uses for cross-blob branches in `arm/beam_asm_module.cpp`
(`bind_veneer_target`, `_veneers`, `flush_pending_stubs`); we
reuse that mechanism rather than reinventing it.

Trampoline overhead: 8 bytes of code + 8 bytes of literal per
out-of-range T2-installed function. In practice (option (a)
working) this stays at zero; in degenerate cases a few hundred
bytes per T2-installed module.

**(c) Indirect via x16 directly from the prologue (rejected).** We
considered patching `L_f + 4..+11` as a two-instruction sequence
(`ldr x16, [pc,#…]; br x16`), using both the patchable slot and
the now-unreached `bl shared` slot. Two reasons we don't:
- It's *two* writes, not one; the in-between observation window
  shows a CPU executing a malformed prologue.
- It permanently kills the reuse of `bl shared` for any future
  trace/NIF install. Since T2 is jettisoned on trace-enable
  anyway, and the post-uninstall prologue must restore the exact
  original two instructions, mutating both is brittle.

Option (a) carries the design; option (b) is the documented
fallback. Option (c) is captured here as a non-choice so it
doesn't get re-proposed during implementation.

**Where the trampoline lives.** In option (b), the trampoline is
allocated from a small **bridge pool** per module. The pool is
allocated at module-load time (sized by the number of T2-eligible
functions the loader sees, with a safety factor) so that no
runtime allocation is needed during T2 install. If the pool is
exhausted, T2 install for further functions in that module is
rejected — they stay on T1. This is rare but bounded.

## 3. The dispatch hand-off in detail

### 3.1 Production model (prologue patch, walked)

Trace through any caller — external, intra-module, fun, apply —
for a T2-installed function:

```
  Caller (any kind):                       ; lands at L_f via direct
    ... bl L_f / b L_f ...                 ; branch, dispatch table,
                                           ; or apply trampoline.

  L_f + 0:   enter_erlang_frame            ; pushes frame pointer
                                           ; (RUNS for every caller,
                                           ; T1 or T2 — frame discipline
                                           ; is identical across tiers)
  L_f + 4:   b   <reach target>            ; ★ patched at install ★
                                           ; reach target = entry stub
                                           ; directly (case (a), §2.7),
                                           ; or near-side trampoline
                                           ; (case (b))
  L_f + 8:   bl  shared    (unreachable)   ; left in place; uninstall
                                           ; reverts the patched b
                                           ; and this becomes reachable
                                           ; again
  L_f + 12:  i_test_yield        (unreached while T2 is installed)
  L_f + 24:  <T1 body>           ◄────────── side-exit landing zone
             ...                            (§4)
```

Inside the T2 blob:

```
  L_f_t2_entry_stub:                       ; ★ patched-to target ★
                                           ; ENTRY CONDITIONS:
                                           ;  - frame pushed by L_f+0
                                           ;  - FCALLS not yet ticked
                                           ;  - XREGs hold args
                                           ;  - x30 (LR) = caller's RA
    adr    ARG3, L_f                       ; MFA-derivation contract
    subs   FCALLS, FCALLS, #1              ; do i_test_yield's work
    b.le   t2_yield_setup
    ; fall through into T2 body. NO enter_erlang_frame — the frame
    ; is already on the Erlang stack from L_f+0.

  t2_body:
    ; specialized body. See §4 for guard / side-exit shape.

  t2_yield_setup:
    ; ARG3 = L_f, route through i_test_yield_shared. Resume PC =
    ; L_f + TEST_YIELD_RETURN_OFFSET = L_f + 24 = T1 body start.
    ; Yielded T2 frames thus transparently demote to T1 for the rest
    ; of their current invocation (per `05_runtime.md` §12.4 alt-3
    ; "yielded T2 frame demotes to T1"). v2 may emit a T2-resume stub.
    b      i_test_yield_shared
```

Key properties of the production model:

1. **One install surface.** The `b` at `L_f + 4`. Patching it
   redirects every caller — external (`Export.dispatch`),
   intra-module (direct relative branch baked at AOT time),
   funcall, apply — because they all converge on `L_f` and run
   `enter_erlang_frame` followed by the patched `b`.
2. **The patched `b` runs after frame push, not before.** The T2
   entry stub assumes the frame is already on the stack and does
   not re-push. This is the rule that lets the patched-prologue
   model unify T1 and T2 stack discipline — every caller, every
   tier, identical frame-push placement.
3. **T1 body is unchanged and reachable.** Side-exits jump
   directly into it at `L_f + 24` (or any other T1 PC, per §4.2).
   Uninstall reverts the patched `b` and execution silently flows
   through T1 again.
4. **MFA contract preserved.** `ARG3 = L_f` in the entry stub
   means `i_test_yield_shared`'s `ARG2 = ARG3 - sizeof(MFA)`
   computation still finds the function's `ErtsCodeMFA`, exactly
   as it does for a T1 function.
5. **Yield resume PC = T1 body start.** A yielded T2 frame
   resumes into T1 — same v1 design choice as `05_runtime.md`
   §12.4 alt-3.
6. **Reach is bounded by `b`'s ±128 MB range.** Co-located
   allocation handles this in the common case; out-of-range
   functions go through a near-side trampoline (§2.7).

### 3.2 MVP model (static codegen-time hook, walked)

The MVP doesn't have a dynamic patch: there's no JIT server, no
post-load mutation, no flag-byte mechanism. Instead, the T1
codegen itself emits the redirect at compile time, after
`i_test_yield`:

```
  L_f_mvp + 0:    enter_erlang_frame
  L_f_mvp + 4:    b   next                 ; unpatched
  L_f_mvp + 8:    bl  shared    (unused)
  L_f_mvp + 12:   i_test_yield
  L_f_mvp + 24:   b   L_f_t2_entry         ; ★ MVP static hook ★
  side_exit:                               ; bound HERE
    <T1 body>                              ;   T1 fallback,
    ...                                    ;   reachable only via
                                           ;   b side_exit from T2
```

The MVP hook runs *after* `i_test_yield`, not before, because the
MVP's T2 body wants to inherit T1's FCALLS decrement and yield
bookkeeping for free. This is the reverse of the production
model: in production, T2's entry stub does its own FCALLS work
and the redirect happens before `i_test_yield`. In MVP, T1 does
the FCALLS work and T2 enters with FCALLS already ticked.

Why the MVP couldn't put the hook in the prologue itself: the
runtime's yield-resume PC computation is hardcoded as
`current_label + PROLOGUE_SIZE + 12` (i.e. just past
`i_test_yield`). Anything emitted between the prologue and
`i_test_yield` would shift the offset and break resume. The
MVP comment in `arm/beam_asm_module.cpp` `emit_i_breakpoint_trampoline`
records this; the production model fixes it by routing
yield-resume through the T2 entry stub and re-deriving the
contract there.

The two models share §4 (side-exit semantics) verbatim — they
only differ in *where* the entry happens and *who* drives the
yield bookkeeping.

## 4. Side-exits: four categories, one shape

Every side-exit is "set up T1's expected machine state, branch to a
T1 PC". Categories differ in *how much setup* is needed before the
branch.

### 4.1 The shared invariant: iteration-start-stays-live

Across all categories, the rule is:

> Whatever XREG/YREG values were live at the most recent
> sync-point boundary preceding the current speculation must
> remain in their canonical T1 locations, *or be cheaply
> re-materialisable from registers that survived through the
> guard*.

In the MVP, this is achieved by *not mutating XREG0/XREG1 until
after every guard has fired*. The loop body computes
`new_net = Net + (A - F)` and `tail = cdr(list)` into scratch
registers; only after the last guard succeeds does it commit
`mov XREG1, TMP1; mov XREG0, TMP3`. A guard failure mid-iteration
finds XREG0 still pointing at the iteration's *original* list
head and XREG1 still holding the *iteration's entry* `Net`. T1
re-executes the iteration from scratch; it has all the inputs.

The same invariant generalises to deeper bodies: a code generator
emits speculation guards before the corresponding XREG mutations,
or it pessimises them by re-materialising the pre-mutation values
into the deopt stub (the framestate-driven path in §4.3). For
straight-line untyped IR this is always achievable. For loops,
the loop header is the natural sync point and codegen chooses
register assignments such that the pre-iteration X-state is
preserved through the iteration body.

### 4.2 Speculation guard failure (outer function)

The simplest case. Compiled IR:

```
  speculate_type %a, small_int      ; outer function, sync point
```

Outer-function compile lays down the operand check followed by a
branch to the *T1 PC for this BEAM instruction*:

```
  ; speculate_type %a, small_int
  and  TMP, reg_a, #TAG_MASK
  cmp  TMP, #_TAG_IMMED1_SMALL
  b.ne .t1_pc_for_K
```

`.t1_pc_for_K` is resolved at T2 codegen time from the T1 blob's
per-instruction PC table (BeamAsm already maintains it for line
debugging). The cold tail is *not* a stub — it's a direct branch
to T1.

State at the branch:
- All XREG/YREGs in T1's expected layout (the "outer function
  preserves T1 state at every boundary" invariant from §6 of
  `01_ir_and_state.md`).
- HTOP, FCALLS at T1's expected values (sync-point invariant).
- CP frames on the Erlang stack as T1 left them.

T1 picks up at `.t1_pc_for_K` and re-executes the BEAM
instruction. No stub work, no metadata lookup.

The MVP demonstrates an aggressive subcase of this: every guard in
`emit_t2_total_2` is `b.ne entry.side_exit` / `b.vs
entry.side_exit`, where `entry.side_exit` is the start of the T1
body. The "T1 PC for this instruction" happens to be the start of
the body in MVP because the entire T2 body covers a single BEAM
instruction window (the recursive tail-call to `total`). In a
production T2, each speculation site picks its own T1 PC.

### 4.3 Speculation guard failure (inlined region)

The harder case. Compiled IR:

```
  call_inlined diff(%a, %f)         ; inlined call site C
    speculate_type %a, small_int    ; inside diff's body
```

At the inlined call site, codegen emitted (per `9.2`):
1. an *eager CP push* — pushing the parent CP onto the Erlang
   stack as if the call had really happened (1 instruction with
   frame pointers off, 2 with on);
2. a `framestate` *codegen-only* metadata record mapping outer
   SSA values to outer X/Y slots at C.

A guard failure compiles to:

```
  ; speculate_type %a, small_int  (in inlined region)
  and  TMP, scratch_a, #TAG_MASK
  cmp  TMP, #_TAG_IMMED1_SMALL
  b.ne .deopt_stub_C
```

`.deopt_stub_C` is emitted in the same blob, cold-laid:

```
.deopt_stub_C:
  ; X/Y restore from framestate at C: codegen-emitted moves into
  ; T1's expected layout. On aarch64, XREG0..XREG3 live in
  ; x25..x28; higher-numbered Xs go to the X array.
  mov   x25, scratch_for_x0    ; re-tag if previously untagged
  mov   x26, scratch_for_x1
  str   scratch_for_x4, [x_reg_array, #32]
  ...
  ; HTOP / FCALLS already correct (sync-point invariant + FCALLS
  ; tick per inlined call from §12.4).
  ; CP frames already on the Erlang stack from the eager push.
  b     .t1_pc_for_C           ; T1 re-executes the original call
```

Stub size: 5–10 X/Y-restore moves plus the final branch,
*regardless of inlining depth*. The framestate captures the outer
state directly; we don't walk a chain of nested framestates at
runtime.

State at the branch into T1:
- XREG/YREGs reconstructed from the framestate.
- CP frames already on the stack from the eager push, exactly as
  T1 expects after the call site (T1 sees this as "I'm about to
  execute the call instruction, my CPs look fine, my X args look
  fine").
- HTOP / FCALLS already correct.

T1 re-executes `call diff/2` from scratch. The eager CP push means
T1's Erlang stack discipline is undisturbed — there's no extra CP
to pop, no missing CP to push. (This is the win that motivated the
eager-CP-push design over Sista's lazy frame materialisation.)

### 4.4 Range-check failure

A `speculate_range %a, -2^58, +2^58` is a special case of §4.2 or
§4.3 — same machinery. The reason it gets its own subsection is
that range checks are emitted *just before* the arithmetic they
guard, but deopt has to land at the *previous* sync point. The
codegen ordering is:

```
  ; instruction boundary K  (sync point)
  speculate_range %a, -2^58, +2^58   ; deopt target = T1 PC for K
  speculate_range %b, -2^58, +2^58   ; deopt target = T1 PC for K
  %b_raw = untag_int %b
  %sum   = add_small %a, %b_raw
  ; instruction boundary K+1
```

Both speculate_ops point their cold tails at K's T1 PC, even
though they're emitted between K and K+1 in IR order. The
arithmetic that follows them has no fallback path — it's
unconditional native add — because the speculations made it
provably safe. (See §9.3 for why mid-arithmetic deopt can't be a
sync point.)

In the MVP this shape is visible without an explicit
`speculate_range`: the inlined `diff` and `+` use ARM's `subs` and
`adds` and branch on the V flag (`b.vs entry.side_exit`). That's
the runtime equivalent of speculate_range — we *check* the range
post-hoc and bail. T2 v1 promotes that check to a pre-arithmetic
speculation in cases where profile data shows the range is stable;
the post-hoc V-flag check remains the fallback for ranges T2
hasn't proven.

### 4.5 GC inside an inlined region

A potential GC site inside an inlined region is a sync point
(see §6.1 of `01_ir_and_state.md`). At every such site, the
inliner:

1. Flushes scratch-resident X values to the X array.
2. Re-tags any untagged values held in scratch regs.
3. Materialises HTOP at its sync-point-correct value.

After this flush, the GC machinery sees the inlined region's state
as a perfectly normal post-call BEAM state. GC walks the X array
and the Erlang stack the same as for a T1 frame.

If the GC happens to *enter* in a way that requires deopt
afterwards (unusual — most paths just return to the post-GC
continuation), the framestate at the GC site provides the X/Y
restore information. In practice this path is the same as §4.3
with a different trigger.

### 4.6 Recompilation-driven jettison (not really a side-exit)

When a blob's exit counter saturates (`9.5`), the JIT server
reverts the prologue patch and tombstones the blob. This is
*not* an in-flight side-exit — it's a future-call redirect. A
process currently executing the tombstoned blob continues
executing it; its CPs get patched lazily on next schedule-in.

We list it here only to avoid confusion: "T2 jettison" and
"T2 side-exit" are different things. Side-exits happen *inside* a
T2 blob's execution; jettison happens *between* invocations.

## 5. T2 uninstall: prologue revert, tombstones, lazy scan

Uninstall is the inverse of install: revert the prologue patch so
that future entries fall through to the T1 body, then deal with
in-flight processes whose CPs point into the T2 blob. The T1 blob
is unchanged throughout — that's the architectural reason we
preserved it.

### 5.1 The four uninstall triggers

1. **Watchpoint fired** (module reload, BIF rebound, etc. —
   `05_runtime.md:14`).
2. **Trace enabled** on this function or on an inlined callee
   (the inlined callee's prologue isn't reachable from the
   inlined copy, so we can't patch *its* prologue —
   `05_runtime.md:12.5`).
3. **Cache eviction** (T2 blob pressure exceeded budget —
   `05_runtime.md:13.2`).
4. **Validation failure on recompile** (exit counter saturated —
   `03_compilation_and_speculation.md:9.5`).

All four use the same machinery.

### 5.2 The uninstall sequence

Under the `code_ix` write lock:

1. **Revert the prologue patch** at `L_f + 4`:
   `b <reach target>`  →  `b next`. Single 4-byte store + icache
   flush + `dmb ish`. After this, every new entry to `L_f` falls
   through into the T1 body exactly as it did before T2 was
   installed. (No flag bit to clear — the strict-mutual-exclusion
   model in §2.4 means there's no `_T2` flag in the breakpoint
   byte to begin with.)
2. **Free the trampoline slot if one was used** (case (b) of
   §2.7). Returns the bridge-pool slot for reuse by future T2
   installs in the same module.
3. **Move the T2 blob into the tombstone set** (a per-cache list
   of "logically freed but physically retained" blobs). Memory
   stays mapped — in-flight processes may still be executing
   inside it (between an entry and a yield/return), and CPs from
   inlined regions on those processes' Erlang stacks may point
   into it.
4. **Bump `t2_global_gen`** (a single 32-bit counter incremented
   on every uninstall — see `05_runtime.md:14.2`).

Steps 1–4 are O(1) and complete inside the lock. New callers go
to T1 immediately. In-flight T2 callers continue executing and
exit naturally on the next yield, return, or side-exit.

Note: there is no equivalent of "revert `Export.addressv`" — we
never patched it. External callers continue to read
`Export.dispatch.addresses[ix]` and land at `L_f`; that hasn't
changed. What changed is what happens *at* `L_f`, and the prologue
revert handles that for *all* caller kinds simultaneously.

### 5.3 Why CPs need patching

A T2 blob containing inlined regions emits per-call-site CP
pushes. If a process is mid-execution in a T2-inlined region when
the blob is tombstoned, its Erlang stack contains a CP pointing
*into the tombstoned blob* — specifically, at the post-call PC
inside the inlined region. When the inlined call eventually
returns and pops that CP, control would land at a now-stale
address.

The fix is to patch each such CP to its *T1 equivalent*. Every T2
blob carries a CP-to-T1-PC side table:

```
  cp_metadata: { t2_post_call_pc → equivalent_t1_post_call_pc }
```

populated at codegen time from the inlining map. The lazy stack
scan uses it to translate stale CPs in place when a process is
next scheduled in. After patching, the CP points into T1 and
return semantics match T1's exactly.

### 5.4 The lazy stack scan

```
schedule_in(p):
    if p->seen_t2 && p->t2_scan_gen != t2_global_gen:
        for cp_slot in walk_erlang_stack(p):
            if cp_slot.value in tombstone_set:
                blob = blob_for(cp_slot.value)
                cp_slot.value = blob->cp_metadata.lookup(cp_slot.value)
        p->t2_scan_gen = t2_global_gen
```

99%+ of schedule-ins are a single integer compare and a branch
(`t2_scan_gen == t2_global_gen` is the common case). Only when a
generation mismatch is observed does the scan run.

### 5.5 Freeing the tombstoned blob

A tombstoned blob is freed when:

1. *Thread-progress sync* (existing `erts_thr_progress` API)
   confirms no scheduler is currently executing inside the blob.
2. *Every process* has either been scheduled in since the
   tombstone (so its CPs are patched) or has never had `seen_t2`
   set. Tracked via a `last_scheduled_gen` per process and a
   watermark.

Long-tail processes (suspended for hours in `receive`) hold the
blob alive until they're scheduled. Acceptable: a few KB of
retained code memory per stuck process.

A high-water sweep proactively walks the process table when the
tombstone set crosses a threshold (`05_runtime.md:14.2`).

### 5.6 What about CPs on the *scheduler stack*?

Native return addresses on the C call stack of a scheduler
thread point into BeamAsm's dispatch fragments, not into T2 blobs.
T2 blobs don't push native return addresses — they use the same
calling convention as T1, which keeps return addresses on the
Erlang stack and uses `ret` to bounce through them. So the
scheduler's C stack doesn't need scanning.

The single exception is when a scheduler is *currently inside*
the T2 blob (between the patched `b L_f_t2_entry_stub` and the
next yield/return). That's handled by the thread-progress sync
in §5.5 step 1 above — we don't free the blob until thread
progress confirms no scheduler is mid-execution inside it.

## 6. The MVP as a worked example

Putting it all together for `t2_mvp:total/2`:

**Install (compile-time, per module load).** The MVP doesn't have
a JIT server, doesn't patch the prologue dynamically, and doesn't
allocate a separate blob. The hook in `emit_i_test_yield`
registers a `T2FunctionEntry` for the hardcoded list
`{t2_mvp, total, 2}`; `emit_int_code_end` then calls
`emit_t2_specializations` which emits `emit_t2_total_2` into the
T1 module's own blob. The static `b L_f_t2_entry` is laid down at
`L_f + 24` at codegen time. Production install replaces this with
dynamic prologue patching at `L_f + 4` (§2.2).

**Dispatch (every call).** Caller (any kind) lands at `L_f`, the
public function entry. Prologue runs, `b next` falls through (no
patch, no flag), `i_test_yield` decrements FCALLS, the static hook
at `+TEST_YIELD_RETURN_OFFSET` branches to `L_f_t2_entry`. In
production, the patched `b` at `L_f + 4` would redirect to the T2
entry stub *before* `i_test_yield`, and the stub would do its own
FCALLS work.

**Loop body (per iteration).** All guards fire before XREG
mutation (the iteration-start-stays-live invariant of §4.1). On
guard success, XREG0/XREG1 commit and the loop tail-calls itself
via `b loop_top` (no CP touch — the original prologue's CP push
spans every iteration).

**Side-exit (per guard failure).** Direct branch to `side_exit`,
the start of the T1 body. XREG0 still holds the iteration's
original list, XREG1 still holds the iteration's entry Net. T1
re-executes from the body start. (This is §4.2 — outer-function
deopt — because the MVP doesn't inline a callee whose body extends
past a sync point. `diff` is inlined but its entire body is
straight-line arithmetic with no sync point, so the MVP collapses
§4.2 and §4.3 into the same shape.)

**Yield (per FCALLS exhaustion).** The T2 loop top's
`subs FCALLS, FCALLS, #1; b.le yield_setup` triggers
`yield_setup`, which sets `ARG3 = fn_entry` and branches to
`i_test_yield_shared`. The shared fragment computes resume PC =
`fn_entry + TEST_YIELD_RETURN_OFFSET` = the hook position, so
on resume control re-enters the T2 loop. (In production with the
prologue-patch model, resume PC = `fn_entry + TEST_YIELD_RETURN_OFFSET`
= start of T1 body, so a yielded T2 frame demotes to T1 for the
rest of its current invocation. v2 may add T2-resume stubs.)

**Uninstall (production only — MVP doesn't implement it).** The
MVP hook is hardcoded into T1 codegen, not patched in post-load,
so "uninstall" would mean "regenerate T1 without the hook" — i.e.
unload the module. Every reference to the T2 region disappears
with the module. Production triggers (§5.1) and the lazy scan
(§5.4) don't apply to the MVP.

## 7. Open issues for production

- **Memory ordering of the prologue patch on aarch64.** The
  patch at `L_f + 4` needs an icache flush + `dmb ish` so a CPU
  that observes the new instruction also observes the new T2
  blob. BeamAsm already does this for module install
  (`erts_seal_module`) and trace patching; we route T2 prologue
  patching through the same path.
- **A CPU racing the patch.** A CPU executing the prologue
  exactly as the patch lands may observe either the old `b next`
  or the new `b <reach target>`. Both behaviours (T1 or T2) are
  correct; only the choice differs. This is the same race
  tracing already accepts.
- **NIF/BIF dispatch caching is a non-issue.** A BIF that caches
  a dispatch pointer across a potentially-yielding call always
  sees `L_f` — T2 doesn't patch `Export.addressv`. Even after
  T2 uninstall, dispatch keeps working; only the prologue's
  redirect changes. **This is the main reason to prefer
  prologue-patch over `Export.addressv`-patch: no caller-side
  cache invalidation needed.** Audit list shrinks from "every
  BIF that caches export pointers" to "nothing".
- **Trace/NIF strict mutual exclusion.** §2.4 captures the rule:
  T2 doesn't compose with bp/NIF on the same prologue byte. The
  install path checks the flag byte under the `code_ix` write
  lock; the jettison-on-trace-enable path is symmetric. A
  comprehensive Phase-0 test matrix exercises every transition:
  `(none → T2)`, `(T2 → trace)`, `(trace → T2-rejected)`,
  `(T2 → NIF-load)`, etc. The failure-mode budget is "no
  silent corruption"; behaviour-preserving rejections are
  acceptable.
- **Multiple code indices.** `ErtsCodeInfo` and the prologue
  bytes are part of the loaded module's code, not part of
  `Export.dispatch.addresses[ix]`. They live exactly once per
  loaded module, not once per code index. T2 install thus
  doesn't need to fan out across `ix` — there's only one
  prologue to patch. Module reloads create a new module instance
  with its own prologue; T2 blobs depending on the old instance
  are jettisoned via watchpoint (`05_runtime.md:14`).
- **Branch-range failures and the bridge pool (§2.7).** Phase 0
  measures actual T2-cache distance from BeamAsm caches across
  Linux ARM64, macOS aarch64, and Windows arm64. Sized so that
  case (b) (trampoline) fires for ≤ 1% of installs in
  representative workloads; if the rate is higher, the
  proximity-hint allocator gets tightened. The bridge pool's
  per-module allocation is sized at module-load time as
  `count(T2-eligible functions) × 1.5`; pool exhaustion is a
  bounded degradation (further T2 installs in that module
  rejected, function continues on T1).
- **NIFs replacing T2-installed functions at runtime.** Rare
  (post-load `erlang:load_nif/2` after T2 has installed). Same
  composition rule as trace: jettison T2 first via the
  `code_ix`-locked path, then NIF install proceeds. Detection:
  `erlang:load_nif/2` notifies the JIT server of impending
  replacements before patching the prologue itself.
