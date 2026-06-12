# T2 — Runtime Integration

> **v1 scope rescoped by [`08_v1_loop_tier.md`](08_v1_loop_tier.md)**:
> in v1, loop back-edge yields *resume into T2* via per-loop resume
> stubs (supersedes §12.4 item 3's demote-for-the-invocation model
> for back-edges; entry yields keep it), T2 blobs contain no CPs
> (08 §4.3) so §14.2's lazy stack scan and tombstone CP tables
> collapse to a single `c_p->i` translation, the Erlang JIT-server
> of §15.3 is replaced by a C-side queue + dirty-scheduler job, and
> watchpoints run at module granularity. All deferred machinery
> returns with general inlining (08 §9).
>
> Part of the T2 design. See [`README.md`](README.md) for the full
> document index. This file covers §§12–15: how T2 lives inside the
> running BEAM. Calling convention, GC and context switching,
> tracing, the code cache and its lifecycle, module reload and
> watchpoint-driven invalidation, and tier-up triggers.

## 12. Calling convention, context switching, and tracing

This section codifies the **identical-to-T1** principle for all
runtime-facing concerns: the calling convention, GC discipline,
context switching/reductions, and tracing primitives. Anywhere T2
diverges from T1 here is a maintenance burden and a potential bug
surface. We diverge only inside well-bounded inlined regions, never
at boundary points like calls, yields, GC safe points, or trace
stubs.

### 12.1 Same calling convention as BeamAsm

T2 code uses:
- `x19` = scheduler_registers
- `x20` = E (Erlang stack pointer)
- `x21` = c_p
- `x22` = FCALLS
- `x23` = HTOP
- `x25–x28` = XREG0..XREG3
- `x15–x17` = XREG4..XREG5

A T2 function is **callable by T1 code as if it were T1 code** —
same calling convention, same register layout at entry/return.

### 12.2 What changes inside a T2 inlined region

Inside an inlined region (and only there), T2 may:
- Use untagged values in scratch CPU registers.
- Defer X-register flushes until the region exits or a C call.
- Reuse scratch registers for different values.

But at every **deopt point** (which is at instruction boundaries
in the original outer function), the framestate captures whatever
mapping is needed to reconstruct outer-function X/Y state.

### 12.3 GC

Same as BeamAsm. GC walks the X-register array via
`c_p->scheduler_registers->x_reg_array` and the Erlang stack from
`c_p->stop` to `c_p->stack_end`. T2 outer code's X/Y layout is
identical to T1's, so GC works without changes.

T2 inlined regions *must* flush X registers before any GC point
(allocation, function call that may GC). The inliner enforces this
by emitting flush sequences at each potential GC site within an
inlined region.

T2 v1 keeps untagged values out of the X register array — they'd
confuse the GC scanner. Untagged values live only in scratch CPU
regs, never spilled to X slots. v2 may add untagged-spill metadata.

### 12.4 Context switching and reductions — identical to T1

**Principle: T2 yields to the scheduler at exactly the same
boundaries, using exactly the same mechanism, as T1.** No new yield
points, no new reduction-counting scheme, no new context-save
format. This is a hard architectural commitment — divergence here
is the HiPE failure mode.

Mechanisms inherited verbatim from BeamAsm:

1. **`FCALLS` (x22) is the reduction counter.** T2 decrements it on
   every function call, the same way `emit_dispatch_return` does in
   `arm/instr_call.cpp`. **When inlining, *each inlined call still
   costs a reduction*.** We do not amortise reductions across
   inlined boundaries — that would change observable scheduling.
   For loop-recovered inlined regions (§10.5), the FCALLS
   decrement is emitted at the loop back-edge so each iteration
   pays one reduction. Unrolling K iterations into one body pays
   K decrements' worth — a single `subs FCALLS, K` at the chunk
   boundary is observably equivalent (identical counts; yields at
   chunk granularity, which is the granularity T1's own
   source-unrolled loops like json's `string_ascii` already have).
   This is an IR-level invariant; no pass may eliminate or reduce
   the total FCALLS charge.
2. **Function-entry yield (`i_test_yield`).** Every T2-compiled
   function emits the same `i_test_yield` sequence as T1. The
   yield jumps to the shared `i_test_yield_shared` fragment which
   saves PC to `c_p->i` and jumps to `context_switch_simplified`.
   For loop-recovered inlined regions, an `i_test_yield`-equivalent
   is emitted at the loop back-edge (a sync point — §6.1) so a
   long-running inlined loop can yield without waiting for the
   outer call boundary. The yield path uses the loop header's
   sync metadata to materialise an outer-function-equivalent state
   and lands in T1 the same way an outer-function yield does.
3. **`c_p->i` save format is unchanged.** A yielded T2 frame saves
   the *T1 BeamAsm PC* for the current BEAM instruction (the same
   mechanism §9.1 uses for outer-function deopt). On resume the
   scheduler re-enters T1 code; the function will tier-up to T2
   again on its next ordinary call.
   - This means **a yielded T2 frame transparently demotes to T1**
     for the rest of its current invocation. Acceptable: yields
     are rare on hot paths; the loss is bounded.
   - Alternative (v2): emit T2 entry stubs at every yield point
     so resumption stays in T2. Not v1.
4. **Trap-out from BIFs.** BIFs that decide to trap call back into
   the scheduler the same way they do under T1. T2 emits the same
   setup (live X-registers in the array, save state on `c_p`).
5. **Heap GC trigger.** T2 emits `test_heap` at the same allocation
   boundaries T1 does — modulo unrolling-driven coalescing
   (§10.6), which is a *local* re-batching, not a change in
   *when* GC can run.
6. **Major collection / message reception.** Both go through the
   normal scheduler entry path; no T2-specific code.
7. **Dirty schedulers and trapping NIFs/BIFs.** T2 calls dirty
   BIFs/NIFs through the same `Export.addressv` indirection
   BeamAsm uses. No bypass.

What this *forbids*:
- Inlining `length/1` so its trap-out path disappears. Even after
  open-coding it as a primitive op (§10.7), the lowering must
  preserve the trap-out semantics — fast path inline, slow path
  through the existing BIF that handles the trap.
- Reductions amortisation across inlined calls.
- Custom yield points inside the T2 body that aren't also valid
  BEAM instruction boundaries.

### 12.5 Tracing — preserve T1's mechanisms; jettison only as fallback

**T2 supports each tracing primitive the same way T1 does, where
possible. Jettison-and-fall-back-to-T1 is the safety net, not the
default.**

T1 has a small set of trace mechanisms; T2 mirrors each:

1. **Patchable function prologue.** T1's prologue contains a 3-
   instruction patchable region that can become a redirect to a
   tracing fragment. **T2 emits the same patchable region in the
   same place.** Trace-enable on a T2 function patches the T2
   prologue identically; the redirect lands in the same
   `generic_bp_global` fragment T1 uses.
   - Exception: if T2 inlined other callees, those callees'
     prologues aren't in this blob. Tracing on an inlined
     callee → jettison the blob. (See below.)
2. **Per-call-site `call_trace` / `return_trace`.** Same stubs as
   T1, in the same SSA-level points; trace fragments are global.
3. **`save_calls`.** Routed through
   `Export.addressv[ERTS_SAVE_CALLS_CODE_IX]`. **T2 inherits this
   for free** — any call through an export entry hits the
   save-calls redirect when active, regardless of caller tier.
4. **Match-spec body tracing.** T1 evaluates match-spec bodies via
   the trace fragment branching into the match-spec interpreter.
   T2 reuses the same path; the trace fragment is tier-agnostic.
5. **Process-event traces** (`garbage_collection`, `running`,
   `send`, `receive`). Fired from the runtime, not from generated
   code. T2 emits no special code.
6. **perf/gdb metadata.** T1 supports perf integration via
   `beamasm_register_metadata`. T2 registers blobs through the
   same path, so perf, gdb, and frame-pointer-based tooling work
   identically across tiers.

What forces jettison:

- **Tracing on an inlined callee.** The inlined callee's prologue
  isn't reachable; jettison the blob holding the inlined copy.
  The watchpoint table (§14) records `t2_inlined_from` mappings
  for exactly this lookup.
- **Match-spec actions on a call site eliminated by inlining.**
  Same fix: jettison.
- **`return_to_trace` for an inlined return target.** Same fix.

**Phase-0 trace matrix (deliverable).** A table covering every
trace primitive, classified into four columns: *inline-preserved*
(T2 emits the same patchable code as T1), *Export-indirection-
preserved* (works via `Export.addressv` regardless of tier),
*requires jettison* (T2 must drop the blob on enable), *requires
temporary jettison-and-recompile* (drop and let it tier-up again
under the new constraint).

Coverage:

- Every `erlang:trace/3` flag: `call`, `return_to`,
  `running_procs`, `procs`, `garbage_collection`, `send`,
  `receive`, `arity`, `set_on_first_link`, `set_on_link`,
  `set_on_first_spawn`, `set_on_spawn`, `silent`, `timestamp`,
  `cpu_timestamp`, `monotonic_timestamp`,
  `strict_monotonic_timestamp`.
- Every match-spec action: `message`, `set_seq_token`,
  `enable_trace`, `disable_trace`, `display`, `caller`,
  `silent`, `trace`, `process_dump`, `exception_trace`,
  `return_trace`.
- Every `erlang:system_monitor` event: `long_gc`, `large_heap`,
  `long_schedule`, `long_message_queue`, `busy_dist_port`,
  `busy_port`. T2's deferred-X-flush behaviour between sync
  points must not break the runtime's view of process state at
  the moment a system_monitor sample lands — sync points
  guarantee consistent state at every observation point.
- `erlang:trace_pattern(_, true, [global])` (wholesale enable on
  every export of a module): jettison every T2 blob that called
  into that module. Recompile is automatic if the trace clears
  later — the jettison is a temporary jettison-and-recompile.
- `save_calls`: Export-indirection-preserved (free).

The matrix lands in this section at the end of Phase 0.

### 12.6 Stack interaction between tiers

Both tiers use the same native stack (`$rsp`/SP), the same Erlang
stack (Y registers), and the same calling convention. A T2
function can call a T1 function and vice versa without any stack
switching — they share identical abstract machine state. This is
the architectural commitment that prevents the HiPE mode-mixing
overhead.

## 13. Code cache and lifecycle

### 13.1 Memory budget

- Default: 64 MB for T2 code (configurable via `+JT2cache <n>`).
- Allocator: separate `JitAllocator` instance from BeamAsm's, with
  its own RX/RW dual-mapping pool. Single-threaded — only the
  dirty CPU compile worker writes here, so no mutex needed.
- Block size: 32 MB (matches BeamAsm).
- Reverse watchpoint index (§14.1): hashmap
  `Module → Vector<BlobRef>` for fast invalidation lookup.
  Bounded by number of T2 blobs × average watched modules; ~few MB
  worst case.
- Tombstone set (§14.2): jettisoned-but-not-yet-freed blobs, kept
  alive until the lazy stack scan has drained references.

### 13.2 Eviction

When the budget is full and a new T2 compile is requested:

1. Find the T2 blob with the lowest "useful work / bytes" score
   (active execution counter from §13.3 / blob size).
2. Revert the function's prologue patch (`b <reach target>` →
   `b next`) so future entries fall through to T1, and free the
   trampoline slot if one was used. See
   `06_dispatch_and_sideexit.md` §5.
3. Move the blob into the tombstone set; do not free yet.
4. Lazy stack scan (§14.2) drains references over time.
5. Compile the new function.

### 13.3 Per-blob metadata

Each T2 blob has:
- The CP-to-T1-PC side table (used by the lazy stack scan, §14):
  `{t2_return_addr → equivalent_t1_pc}` for every CP-pushing site
  in the blob. ~16 B per site.
- The active execution counter (incremented in the T2 prologue;
  used by §13.2 eviction). Decayed exponentially with a half-life
  tied to the GC interval.
- The exit count and exit-reason buffer (per-site array — §9.5).
- The watchpoint list (§14).
- The T1 PC table reference for outer-function deopt resolution
  (§9.1).

(Framestates are *codegen-only* metadata, consumed during deopt-
stub emission — they don't live in the runtime blob.)

## 14. Module reload and code purge

This is where HiPE lost. `code:purge/1` and atomic reload happen at
runtime; any T2 optimization that depended on assumptions about
*some other module* must invalidate when that module reloads.

### 14.1 Watchpoint registration

When T2 compiles a function and folds in (e.g.) a constant from
another module, an inlined call to another module's function, or a
BIF whose binding could change, it registers a watchpoint:

```
watchpoints[t2_blob] = {
    [{module_loaded,  ModName} → invalidate]
    [{function_loaded, ModName, FunName, Arity} → invalidate]
    [{bif_rebound,     ModName, FunName, Arity} → invalidate]
}
```

A reverse index keyed by `Module` (`Module → Vector<BlobRef>`,
in §13.1) makes "all blobs depending on `lists`" an O(1) lookup
instead of an O(N_blobs) walk. Granularity is per-function — a
reload of `lists` invalidates all T2 blobs that inlined any
`lists` function, not just one.

### 14.2 Invalidation: jettison + lazy stack scan

The naive "walk every process's stack at jettison time" is
catastrophically expensive at scale (millions of processes). T2
uses a *lazy* model: jettison sets a flag; processes patch their
own CPs the next time they're scheduled in.

**Sync with the code loader.** Before a code reload commits, the
code loader notifies the JIT server. The server:

1. Blacklists in-flight T2 compiles for the affected modules
   (so we don't install a blob using stale SSA after the reload
   commits — sub-race (a) from the critique B3).
2. Identifies all T2 blobs depending on the affected modules
   via the reverse index (§14.1).
3. For each such blob: revert the function's prologue patch so
   future entries fall through to T1, free any trampoline slot,
   move the blob into the tombstone set (§13.1), bump
   `t2_global_gen`. Memory stays mapped. Full sequence in
   `06_dispatch_and_sideexit.md` §5.

**Generation counter.** Two new per-process fields:

- `p->t2_scan_gen` (Uint32): the value of `t2_global_gen` we
  last scanned this process's stack at.
- `p->seen_t2` (bool): set the first time control enters any T2
  blob (cheap T2 prologue check). Pure-T1 processes never set
  it and skip the scan entirely.

**Schedule-in scan.** When a process is scheduled in:

```
if p->seen_t2 && p->t2_scan_gen != t2_global_gen:
    for cp_slot in walk_erlang_stack(p):
        if cp_slot.value in tombstone_set:
            blob = blob_for(cp_slot.value)
            cp_slot.value = blob->cp_metadata.lookup(cp_slot.value)
    p->t2_scan_gen = t2_global_gen
```

CP patching is in-place. Patched CPs become T1 PCs; when the
corresponding `return` fires, control lands in T1 normally — no
deopt-time work, no re-execution.

99%+ of schedule-ins are a single integer compare and a branch.

**Freeing tombstoned blobs.**

- *Phase A*: thread-progress sync (existing infrastructure)
  ensures no scheduler is currently inside the blob.
- *Phase B*: free the blob when an epoch ticks past "every
  currently-existing process has been scheduled at least once
  since jettison". Track via a `last_scheduled_gen` per process
  and a watermark.
- *Long-tail processes* (suspended, hibernating, stuck in
  `receive` for hours): blob memory waits on the process's
  lifetime. Acceptable.

**Tombstone-set high-water sweep.** When the tombstone set
crosses a threshold (in entries or retained code memory), trigger
a proactive sweep:

- Iterate the process table; find processes whose
  `p->t2_scan_gen` lags significantly.
- Run the same scan procedure under the standard process-suspend
  lock — no need to wait for natural schedule-in.
- Once swept, generations advance and Phase B can free the
  affected blobs.

The threshold is tunable (`+JT2tombstone_high_water`).

### 14.3 Hot code upgrade — worked example

```erlang
example(L) ->
    lists:foldl(fun ?MODULE:sum/2, 0, L).

sum(_, A) -> A + 1.
```

Suppose T2 has inlined `lists:foldl/3` *and* `sum/2` into
`example/1` (the latter via constant-fun-target inlining, §10.1).
The module is upgraded to:

```erlang
example(L) ->
    lists:foldl(fun ?MODULE:sum/2, 0, L).

sum(_, A) -> A + 2.
```

What happens:

1. Loader notifies JIT server of upcoming reload of this module.
2. JIT server reverse-indexes blobs depending on this module —
   `example/1`'s T2 blob is one of them.
3. JIT server reverts `example/1`'s prologue patch (so future
   entries flow through to the existing T1 body), frees the
   trampoline slot if one was used, moves the T2 blob into the
   tombstone set, bumps `t2_global_gen`.
4. New module commits. Future `example/1` calls go to T1, which
   calls the *new* `sum/2`.
5. Existing processes mid-execution in `example/1`'s T2 blob:
   - If they're inside the inlined `sum/2`, they finish that
     iteration with the old code. Acceptable (in-flight calls
     during reloads always see the version they entered with —
     this matches BEAM's existing semantics).
   - Their CPs pointing into the now-tombstoned blob get
     patched to T1 equivalents on next schedule-in.
6. Eventually the tombstoned blob's references drain; Phase B
   frees the memory.

### 14.4 Tracing as a watchpoint event

The trace-enable case in §12.5 is a generalisation of this: any
runtime event invalidating a T2 assumption triggers blob jettison.
Tracing is one such event. Implementation is unified — one
watchpoint table, one jettison routine, the same lazy-scan
machinery.

## 15. Tier-up triggers and heuristics

### 15.1 Threshold

Per-function call counter, threshold scaled by:

```
threshold = base * sqrt(size + 1) * 2^recompile_count
            * (M / (M - U))
```

- `base` = 1000 (tunable; matches JSC's Baseline→DFG threshold).
- `size` = number of IR ops the function compiles to.
- `recompile_count` = prior T2 compiles for this function.
- `M`, `U` = total / used bytes of T2 code cache.

The `M/(M-U)` shape is borrowed from JSC as a starting point; it
may need re-tuning for Erlang's blob-size and cache-pressure
profile. Phase 4 includes a measurement task to re-evaluate the
formula against representative workloads.

### 15.2 Profile saturation

Before tier-up, check ≥75% of profile slots have observations and
the function's argument-type buckets are *stable*. "Stable" =
each tracked slot's `seen_types` bitmask hasn't changed across the
last N call-counter ticks (JSC's stable-bucket-for-N-iterations
rule; v1 uses `N = 64`). If not stable, reset counter, give it
more time, max 5 retries.

### 15.3 Compile queue

The JIT server process holds the queue. The server is single-
threaded by virtue of being an Erlang process; compile requests
serialise through it (queueing, dedup, eligibility filtering).
Compile work itself runs on a dirty CPU scheduler so it doesn't
block normal schedulers. **Only one compile worker runs at a
time** — the compile work is CPU-bound but rare; serialising the
worker means asmjit's `JitAllocator` doesn't need a mutex and
the watchpoint table doesn't need cross-compile concurrency
control.

If the queue exceeds a high-water mark, further requests are
dropped (function continues in T1; counter retrips later).

### 15.4 OSR — partial v1

**OSR-exit (in v1).** Mid-execution transition from T2 down to T1.
Required for tracing (§12.5), watchpoint invalidation (§14), and
recompilation backoff (§9.5). In the outer function the mechanism
is trivial: jump to T1's compiled code at the same instruction. In
inlined regions, deopt via the per-region deopt stub emitted from
codegen-time framestate metadata (§9.2 in
`03_compilation_and_speculation.md`).

**OSR-entry (deferred).** Mid-execution transition from T1 up to
T2. Hot loops that don't span a tier-up-triggering function call
won't get T2'd until the next call-return cycle. Acceptable for
v1 given Erlang's idiom of expressing loops as recursive calls.

