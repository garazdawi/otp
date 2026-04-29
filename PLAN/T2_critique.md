# T2 Plan — Critique

A thorough review of `PLAN/T2.md` (1942 lines, commit `14c12f8f4d`),
intended for inline response. Each item is a checkbox so the response
can be marked status (`fix`, `wontfix`, `defer`, `disputed`, etc.).

Organised into six categories:

- **A. Internal contradictions.** Two parts of the plan say different
  things about the same decision.
- **B. Architectural under-specification.** Decisions taken but not
  worked out to the level needed to start writing code.
- **C. Wrong numbers and engineering bugs.** Specific values, formulae,
  or claims that look incorrect.
- **D. Missing content.** Topics the plan doesn't address that a v1
  implementation will need to.
- **E. Editorial.** Structural and cross-reference cleanup.
- **F. Strategic concerns.** Higher-level questions about feasibility
  and direction.

---

## A. Internal contradictions

### A1. Strict-vs-relaxed state-preservation model
- [ ] §1 goals (line 54–57): "outer function preserves T1's abstract
      machine state **at every BEAM instruction boundary**".
- [ ] §3 architecture summary (~line 193): "interchangeable with T1
      code at every BEAM instruction boundary".
- [ ] §4 Choices table (line 208): "Outer function = identical T1 X/Y
      layout at instruction boundaries".
- [ ] §6.1 (line 442–446) and §6.4 (line 525–540) reverse this to
      "matches T1's **at every point where T2 could exit or yield** —
      and only at those points".
- [ ] §6.2 item 4 (line 495–501) calls the strict version "the v1
      default", relaxed for later phases.
- [ ] §11.2 (line 1217–1221) reiterates the v1-strict / relax-later
      framing.
- [ ] §17 Phase 1 (line 1595–1596) says "output matches T1 at every
      BEAM instruction boundary" — strict.
- [ ] §17 Decisions Resolved (line 1761–1762) restates the strict
      version: "Outer function = identical T1 layout; framestates
      only in inlined regions."
- [ ] Three readings are tenable, all undermined by other parts of
      the document:
      1. **Strict in v1, relaxed later** → §1/§3/§4 are correct; §6
         is roadmap. But §6 reads as if the relaxation *is* the design.
      2. **Relaxed is the design; strict is a v1 implementation
         simplification** → §1/§3/§4 are wrong as stated.
      3. **Relaxed at sync points = strict at boundaries because most
         boundaries are sync points anyway** → maybe true but never
         argued.
- [ ] **Concrete fix**: pick one and propagate. If reading 1 is the
      real policy, add it explicitly to §1, §4, §17, and the Phase
      1/Phase 4 deliverables in §17 (today they're disjoint).

We don't need them at every boundary, only at places where we might
exit to T1 or be pre-empted. There will be a lot of places where that
can happen, but lets give the register allocator as much space as we can.

Update the plan to be consistent with this in mind.

### A2. §6.4 duplicate heading
- [ ] Two `### 6.4` headings:
      `6.4 Why the relaxation matters` (line 525) and
      `6.4 Inlined regions` (line 542). The second should be `6.5`,
      and the existing `6.5 Worked example` (line 568) shifted to
      `6.6`.

Fix.

### A3. §17 Decisions Resolved row 1 contradicts §4 / §17 Still-Open
- [ ] §17 row 1 (line 1755–1757) says: "T2 manager supervision tree.
      Background thread, not a process under `kernel/code` (changed
      from earlier draft — matches HotSpot CompilerThread)."
- [ ] But §4 Choices (line 222) says "**Erlang JIT server process
      under `kernel/code`** + dirty CPU scheduler", and §17 Still-Open
      (line 1790–1797) tags the same item "**Decided: dirty CPU
      scheduler + JIT server process** ... A JIT server Erlang process
      under `kernel/code` co-ordinates compile requests".
- [ ] So row 1 is stale text from the earlier "background thread"
      revision. The current decision is the *opposite* of what row 1
      says. Delete or rewrite row 1.

We willo use a server running in erts that handles it. Similar to the
literal purger.

### A4. §17 "Decisions resolved" / "Still open" sections have merged
- [ ] Most entries under "Still open" now begin with "**Decided:**"
      inline (line 1784, 1790, 1798, 1803, 1806, 1813, 1819, 1825).
      The two-section split is broken — every "Still open" item has
      already been resolved.
- [ ] Consolidate: every item with a decision moves to "Decisions
      resolved"; the "Still open" header keeps only items genuinely
      lacking a decision (currently: "Trace audit specifics",
      "Watchpoint granularity").

fix

### A5. §15.4 cross-reference to §9
- [ ] §15.4 (line 1518–1520) cites "tracing (§12.5), watchpoint
      invalidation (§14), and recompilation backoff" as users of
      OSR-exit. The recompilation backoff machinery is described in
      §9.5, not anywhere in §14 or §15. Add the §9.5 cross-reference
      or move §9.5 under §15.

fix

### A6. "Inlined-region" wording in §1 vs §6
- [ ] §1 says deopt in the outer function uses "no framestate
      machinery". §6.1 says the outer function still emits sync-point
      "live X-reg maps" — which *is* a framestate, just narrower than
      the JSC kind. Either rename the §6 metadata to something other
      than "framestate" (it currently shares the name with the inlined-
      region full framestate), or acknowledge in §1 that the outer
      function carries lightweight stackmap-style metadata.

acknowledge in §1 that the outer function carries lightweight stackmap-style metadata.

---

## B. Architectural under-specification

### B1. Multi-frame deopt is hand-waved
- [ ] §9.2 (line 897–899) says nested inlining "walks the framestate
      chain and materialises the parent CP frames on the Erlang stack
      before jumping." This is the hardest part of any deopt
      implementation (Sista's whole FullBlockClosure problem; JSC has
      a dedicated deopt thunk pipeline) and gets one sentence.
- [ ] Required: a worked example showing two-deep nested inlining
      (`f` inlined into `g` inlined into `h`) hitting a guard inside
      `f`. What does the stub look like in machine code? Where do the
      saved CPs come from? Are they materialised from `framestate`
      metadata, or threaded through chained framestates? How big does
      the stub get for, say, 4-deep inlining?
- [ ] A bound on the size of the deopt stubs across realistic inlining
      depth (call it 3–4 deep) needs to be in §13.1's memory budget.
- [ ] §9.5 ("100 deopts before jettison") presumes deopts are *cheap*,
      so a healthy steady-state can absorb dozens of them per blob.
      But the inlined-region deopt is much more expensive than the
      outer-function "single jcc" path: §10.3 permits inlining depth
      3, so a single deopt may reconstruct 3 CP frames =
      ~20–40 instructions of frame plumbing per fire. The "single
      jcc" mental model the plan promotes (§6.2 item 1, §9.1) and the
      "100 deopts is fine" recompile policy are talking about
      different deopt paths and need to be reconciled.

**Resolution: eager-CP-push.** At inlined-region entry, push the
parent CP onto the Erlang stack as if the call had really happened.
Erlang's CP is 1 word (2 with frame pointers), so the steady-state
cost is ~1–2 instructions per inlined call — in the noise.

What deopt at site C must reconstruct:

1. **Outer X/Y state at C** — recorded in framestate metadata at
   codegen, consumed at codegen to emit X/Y-restore moves in the
   stub.
2. **HTOP / FCALLS at C** — comes for free from the sync-point
   invariant (§6): C is a sync point, HTOP is consistent at every
   sync point, FCALLS ticks per inlined call (§12.4).
3. **CP frames** — already on the Erlang stack from eager push at
   inlined entry; nothing for the stub to do.
4. **Jump target** — T1 PC for C, resolved at codegen from the T1
   PC table (§9.1).

Resulting stub at any nesting depth:

```
.deopt_stub_C:
  ; X/Y restore (codegen-emitted moves, ~5–10 insns)
  mov  x25, scratch_for_x0
  mov  x26, scratch_for_x1
  str  scratch_for_x4, [x_reg_array+32]
  ...
  b    .t1_pc_for_C        ; T1 re-executes original call_ext
```

Framestate becomes a **codegen-only data structure** — flat array
of `{t1_pc, live_entries[], parent_fs_id}` consumed during deopt-
stub emission, then thrown away (or kept as a debug record). It
doesn't compete for runtime blob memory; ~no cost in §13.1.

§9.5 stays as-is, with one refinement worth recording: distinguish
in-loop-inlined exits from in-loop-outer exits in the recompile
heuristic — inlined-in-loop exits should drop the inlining for
that site on recompile, not just widen the speculation. Captured
via the per-site exit-reason buffer (B7).

Plan edits required:
- §9.2: rewrite to describe eager-CP; remove "walks the framestate
  chain and materialises the parent CP frames" wording.
- §9.5: add the "in-loop-inlined → drop the inline on recompile"
  refinement.
- §13.1: clarify framestate is codegen-only, no runtime memory cost.

### B2. Sync-point identification underspecified
- [ ] §6.1 (line 451–460) lists eight categories of sync point
      ("function entry", "calls", etc.). §6.3 (line 504–510) says the
      compiler must "identify all sync points up front" via dataflow.
- [ ] But: the list is at the BEAM-op level. T2 IR has its own ops
      (§5.2). Some BEAM ops lower to multiple T2 ops (e.g. an
      arithmetic op may decompose into `speculate_type`,
      `speculate_range`, `untag_int`, `add_small`). Which of those T2
      ops carries the sync constraint? Only the ones that map to the
      original BEAM-op boundary? Then the constraint moves with
      lowering — that's a real design constraint on the lowering pipeline.
- [ ] Several BEAM op kinds *conditionally* yield/GC/raise depending on
      runtime values: `bs_match`, NIFs that may trap, BIFs whose
      trapping is data-dependent (e.g. `length/1` on long lists,
      arithmetic that overflows into bignum). §6.1's list categorises
      these under "function call sites" / "BIF call boundaries", which
      papers over the conditional case. Is the constraint static
      ("always treat them as sync points") or dynamic ("only on the
      branch that traps")? Static is simpler but pays a sync at sites
      that almost never trap. Pick.

For most of these functions it is not possible to know beforehand if
they will yield or not, so it should be a static decision. Unless of course
their arguments are literals (because of inlining), but then they are not called
at all.

- [ ] Required: a definition of "sync point" expressed in T2 IR terms,
      not BEAM-op terms. Specifically: which T2 ops are sync points,
      and which are guaranteed to be inserted at *original* BEAM-op
      boundaries vs. injected by lowering.

When creating the T2 ops, we walk the original beam SSA for a functions and
from there we can emit specific T2 ops that are syncpoints. Or we embed
it as metadata to instructions.

### B3. Inlining vs hot-code-upgrade race
- [ ] §14.3 "Hot code upgrade" (line 1470–1476) says the watchpoint
      table revokes blobs that inlined the now-old code. But three
      distinct races aren't addressed:

      **(a) In-flight T2 compile started before reload.** Compile
      reads the now-old SSA chunk for `lists:foldl/3`, optimisation
      pass runs, blob installation tries to commit *after* the
      watchpoint already fired (because the watchpoint walks the
      table of installed blobs, not the queue of in-flight compiles).
      Required: a generation counter checked at install time, or
      re-validation of all watched dependencies before installing.

      **(b) Process executing inside an inlined-but-now-stale
      region.** P is mid-execution in T2 blob B which inlined
      `lists:foldl/3`. `code:purge(lists)` fires. P is *inside* the
      inlined region. Possibilities: deopt at the next sync point
      (latency unbounded if we're in a tight unrolled loop); patch
      the blob to deopt now (requires concurrent code modification on
      a running scheduler); keep the old blob alive until P leaves it
      (ref-counted; needs design). Pick one.

      **(c) Jettison-while-Export-already-swung.** When a watchpoint
      jettisons a T2 blob *because* of a reload, the new module's
      `Export.addressv` has already been pointed at the new code. The
      P-in-blob-B case from (b) ends up returning to T1 — but T1
      reaches the new code, not the version P entered with. This is
      the same hazard `code:purge/1` already manages with literal-area
      refcounting; T2 jettison needs the same care. Spell out the
      ordering.

We need to sync the JIT and code loader. That is when starting to initiate
a code reload we should first tell the JIT server what we are about to do
so that it can blacklist any compilations of that module and also invalidate
the T2 code fragments that are about to be updated.

For example if we have this code:

```erlang
example(L) ->
   lists:foldl(fun ?MODULE:sum/2, 0, L).

sum(_, A) -> A + 1.
```

and it gets upgraded to

```erlang
example(L) ->
   lists:foldl(fun ?MODULE:sum/2, 0, L).

sum(_, A) -> A + 2.
```

just as we are running that code and all of it is T2 compiled with inliner.
Then the T2 code needs to recognize that sum/2 has changed and exit back to T1
when called.

### B4. Reductions through inlined calls
- [ ] §12.4 item 1 (line 1308–1311) says "*each inlined call still
      costs a reduction*" and "we do not amortise reductions across
      inlined boundaries". Good principle. But:
- [ ] How is this enforced mechanically? If `lists:foldl/3` is
      inlined and recovers as a flat loop (§10.5), is each iteration
      still costing a reduction? At T1 it does (each iteration has a
      `call_only` decrement). The straightforward T2 lowering of a
      flat loop *would* skip this, because there's no longer a "call".
- [ ] Related: the inlined callee's prologue had `i_test_yield`
      (§12.4 item 2). Do we replicate that at the inlined entry? At
      every loop-recovered back-edge? If we replicate it, every
      inlined call site in a hot loop has its own yield site;
      if we collapse them, we change observable scheduling. Pick.
- [ ] Either the lowering needs to inject FCALLS decrements at loop
      back-edges or per-K-iterations after unrolling, or this rule
      needs an exception for inlined recursive helpers. Resolve
      explicitly. §5.2 has `reduction_check cost` as an IR op but
      never says where the inliner inserts it.

Yes, it still needs to cost, and it should be possible to yield in the loop.
This probably means that when yielding in such a loop we need to have some
extra code to restore the frame state.

### B5. Watchpoint indexing for invalidation
- [ ] §14.1 (line 1441–1458) registers watchpoints with `{Mod,Fun,Arity}`,
      `{literal_table_idx}`, `{export_entry}`. §14.2 says invalidation
      walks the watchpoint list on `code:purge/1`.
- [ ] How big is "the watchpoint list" in production? If a server
      has 10K T2 blobs and each blob has 5–20 watchpoints, this is
      50K–200K entries. Linear walk on every code load is expensive.
      A reverse index keyed by Module is required; specify the
      structure (hashmap of Module → Vector<BlobRef>), and put the
      memory cost into §13.1.

fix

### B6. No quiescence/thread-progress story
- [ ] §13.2 item 3 (line 1422–1423) mentions "a quiescence wait (in
      case another scheduler was mid-execution — same wait pattern as
      code purge)". Same in §14. But "the same wait pattern" isn't
      itself written down anywhere in the plan.
- [ ] BeamAsm's existing pattern: `code:purge/1` has a multi-stage
      release using literal-area refcounting and a "thread progress"
      sync. T2 blob eviction needs the same model — and may have *new*
      requirements because deopt stubs reference the T1 blob's PC
      table (§9.1 line 866–871). If the T1 blob is purged out from
      under a T2 blob, the T2 blob's deopt stubs are dangling.
- [ ] Required: explicit ordering for "purge T1 blob holding inlined
      `lists:foldl`" — does T2 jettison first, or is T1 quiesced past
      all T2 deopt-stub references? What's the guarantee?

When purging code we need to kill processes lingering in T1 and T2 PCs
for that generation of code. When deleting a T2 code fragment because of
too many exits, we need to do a OSR on each process changing the return
pointer to the T1 address. We need to be very careful here as there can
be millions of processes and walking all their stacks will be very expensive.

**Resolution: lazy stack scan on schedule-in, with bounded tombstone
set.**

Five pieces:

1. **Generation counter.** `t2_global_gen` in the runtime, bumped on
   every blob jettison. Per process: `p->t2_scan_gen`, last value
   scanned at. Schedule-in compares; on equal, skip. 99%+ of
   schedule-ins are one int-compare plus a branch.

2. **Pre-filter.** `p->seen_t2` flag, set the first time control
   enters a T2 blob (cheap T2 prologue check). Pure-T1 processes
   pay nothing — the flag never clears.

3. **Tombstone set.** Jettisoned-but-not-yet-freed blobs in an
   address-range-keyed structure. Binary search per CP at scan time.

4. **CP-to-T1-PC metadata per blob.** At T2 codegen, every CP-
   pushing site records `{t2_return_addr → equivalent_t1_pc}` into
   a side table. ~16 B per site; negligible.

5. **Scan procedure.** On schedule-in, if pre-filter passes and
   generations differ:

   ```
   for cp_slot in walk_erlang_stack(p):
       if cp_slot.value in tombstone_set:
           cp_slot.value = blob.cp_metadata.lookup(cp_slot.value)
   p->t2_scan_gen = t2_global_gen
   ```

   In-place CP patching. CPs become T1 PCs; when the corresponding
   `return` fires, control lands in T1 normally. No deopt at scan.

**Freeing tombstoned blobs.**

- **Phase A**: thread-progress sync; no new entries to the blob.
- **Phase B**: free the blob when an epoch ticks past "every
  currently-existing process has been scheduled at least once since
  jettison" — tracked via a `last_scheduled_gen` watermark.
- **Long-tail process** (suspended, hibernating, stuck in `receive`
  for hours): blob memory waits on the process's lifetime.

**Tombstone-set high-water sweep.** When the tombstone set reaches
a threshold size (in entries or in retained code memory), trigger a
proactive sweep:

- Iterate the process table, find processes whose
  `p->t2_scan_gen` lags the global gen significantly.
- For each, run the same scan procedure — but from the sweep
  context, not from a scheduler-in path. The process need not be
  scheduled in for this; its stack is walked under the standard
  process-suspend lock.
- Once swept, processes' generations advance and the corresponding
  blobs become eligible for freeing under Phase B's rule.

This bounds the tombstone-set's worst case: instead of waiting on
every long-suspended process to eventually run, we proactively
clear the long tail when memory pressure justifies the walk cost.
The threshold is tunable (`+JT2tombstone_high_water`).

Plan edits required:
- §13.2: replace "quiescence wait" with the lazy-scan + tombstone
  protocol; add the high-water sweep.
- §14.2: same; clarify that watchpoint invalidation triggers the
  jettison-and-tombstone flow, not an immediate stack walk.
- §14.3: hot-code-upgrade reload reuses the same machinery.
- New per-process fields documented (`t2_scan_gen`, `seen_t2`,
  `last_scheduled_gen`).

### B7. Speculation-failure → exit-reason buffer
- [ ] §9.5 (line 962–963) says deopt records "which speculation
      killed it (stored in a per-function exit-reason buffer)". §13.3
      lists "exit count and exit-reason buffer" as per-blob metadata.
- [ ] Format unspecified. Per-speculation-site index? Reason code +
      observed runtime type? Bounded ring buffer or unbounded?
      Required for §9.5's "this time with knowledge of which
      speculation killed it" to be actionable on recompile.

**Resolution.** Per-blob array indexed by *speculation-site ID* (one
entry per `speculate_*` op in the IR), each entry being
`{Uint16 fail_count, Uint16 last_seen_type_mask}`. ~4 bytes ×
<100 speculation sites/blob ≈ ~400 B/blob. On recompile, the
optimizer reads the array: any site with `fail_count > T` gets its
speculation widened to the union of `last_seen_type_mask`, or
dropped entirely. Cheap, fixed-size, indexable, no ring buffer
needed. The §9.5 "weighted per loop-vs-non-loop" rule falls out of
indexing site_id into a static metadata table that records
is-in-loop per site.

### B8. Profile saturation criterion
- [ ] §15.2 says "≥75% of profile slots have observations and the
      function has been running long enough that its argument-type
      buckets have converged". "Converged" is undefined. JSC uses a
      stable-bucket-for-N-iterations rule; pick a concrete one.

Follow JSC.

---

## C. Wrong numbers and engineering bugs

Fix all of the below.

### C1. SmallInt range is wrong
- [ ] §9.4 (line 934–935) writes
      `speculate_range %a, -2^60, +2^60`.
      On 64-bit BEAM, the small-integer tagged range is approximately
      ±2^59 (4 tag bits in a 64-bit word). ±2^60 cannot be represented
      as a tagged small at all, so the speculation is unsatisfiable.
      The illustrative range that "leaves room" should be a value
      strictly less than 2^59 — typical choice ±2^58 (so that the
      sum of two such values still fits in 2^59 and stays small).

### C2. 24-IR-op size threshold borrowed from Core Erlang
- [ ] §10.3 (line 1026) says "Size threshold: ≤24 IR ops (matches
      AOT inliner default)". The AOT inliner's threshold is over
      *Core Erlang* terms, not over T2 IR ops. The two IRs have
      different node densities — a Core Erlang `case` is one node;
      its T2 IR equivalent is multiple `branch`/`is_*`/`phi` ops.
- [ ] Either re-derive a T2-IR threshold empirically (with a Phase 0
      experiment on representative functions) or document the chosen
      number as a guess to be calibrated.

### C3. "Active execution count" for eviction is not actually defined
- [ ] §13.2 step 1 (line 1419–1420) selects the blob with lowest
      "useful work / bytes" using "recent execution count / blob size".
- [ ] But the only counter the plan defines is the **tier-up call
      counter** in §7.4, which gets reset to a "pending compile"
      sentinel after T2 install (line 685). After install, the slot
      isn't being incremented per T2 invocation — so the eviction
      policy is reading stale data, or zero, or the sentinel.
- [ ] Required: a separate per-T2-blob execution counter,
      incremented inside the T2 prologue (cheap), with a documented
      decay model (JSC uses exponential decay with a half-life tied
      to GC interval). Cost goes into §13.3's per-blob metadata.

### C4. `binary_part/2,3` listed as Phase A guard BIFs but binary
      is Phase D
- [ ] §10.7 (line 1157) lists `binary_part/2`, `binary_part/3` among
      Phase A guard BIFs. But Phase 7 (line 1682–1685) is when
      "Phase D instructions (binary)" arrive in T2.
- [ ] Inconsistency: either `binary_part/2,3` need a Phase A
      lowering even without bitstring matching support (plausible —
      they take an existing binary and extract a sub-binary; no
      construction context required), or they should move out of
      §10.7 into Phase D / Phase 7. Decide.

### C5. `length/1` fast-path / slow-path invariant not stated
- [ ] §10.7 (line 1162–1165) calls out `length/1` as having "an
      inline fast-path with a slow-path tail call to the existing
      BIF implementation — preserving trap-out semantics".
- [ ] §12.4 line 1340–1343 also discusses this. But the actual
      invariant — "fast-path is only taken when the input list is
      short enough that traversal cost is bounded by the reduction
      budget" — isn't written. What threshold does T2 use? T1's
      `length/1` traps after a fixed number of cells; what's that
      number, and does T2 honour the same one?
- [ ] Stronger: the IR-level invariant **"the fast and slow paths
      are inseparable; neither pass may eliminate the slow tail
      call"** is what makes the open-coding safe across optimisation.
      DCE that "knows" the fast path always succeeds would happily
      delete the slow tail and break trap-out semantics. Write this
      down as a constraint on every T2 pass, not a property of the
      lowering.

### C6. Deopt stub example understates aarch64 register handling
- [ ] §9.2 (line 890–894) shows the stub as moves into the X-reg
      array. On aarch64 BeamAsm, the first few X registers live in
      physical CPU registers (XREG0..XREG3 → x25–x28 in the existing
      ABI; see `arm/beam_asm.hpp`), and only overflow Xs hit the
      in-process X-array.
- [ ] The real stub has to mix `mov x25, scratch_reg` for
      register-resident X slots with `mov [x_reg_array+offset],
      scratch_reg` for overflow slots, plus retag for any untagged
      scratch values still pending. The two-line example does only
      the array-flush case.
- [ ] Concretely: deopt stub at C must (a) flush untagged scratch
      back to tagged form, (b) ensure ABI-register X regs (x25–x28)
      hold the values §6's sync constraint says they should, (c)
      flush overflow Xs (≥ x11 say) into the array.

### C7. Phase effort breakdown skewed
- [ ] Appendix B (line 1917–1925) gives 8 weeks for Phase 0, 4 weeks
      for Phase 1, 6 for Phase 2, **10 for Phase 3**.
- [ ] Phase 0 (plumbing) at 8 weeks for ~3 KLOC C++ + 1 KLOC Erlang
      seems thin for: AOT compiler change for SSA chunk, loader,
      C API, T2 manager process, eligibility checks, profiling
      infrastructure, build-system integration for the dirty
      scheduler. Likely 12+ weeks.
- [ ] Alternate framing: Phase 0 already delivers the IR builder, so
      Phase 1 (4w) is just integrating it. Phase 1 then looks too
      *small* relative to whatever residual Phase 0 work overflowed
      — i.e., the boundary between Phase 0 and Phase 1 is fuzzy
      and the 8w/4w split may not match where the work actually
      lives.
- [ ] Phase 3 (inlining MVP + loop recovery + intrinsics) at 10
      weeks for 4 KLOC C++ + 0.5 KLOC Erlang seems light given that
      the entire inlined-region framestate / deopt machinery is
      implemented here, plus per-region deopt stubs, plus the
      linear-scan allocator inside inlined regions, plus loop
      recovery, plus annotation-driven intrinsics reuse from
      `sys_core_fold_lists.erl`. Seven+ items in one phase. Likely
      14–16 weeks.
- [ ] Phase 4 (production polish) at 8 weeks for 1.5 KLOC + docs
      seems heavy in lines/weeks but may be light in actual work
      (rollout, regression hunting, incident response don't fit a
      LOC/week framing).

### C8. ~2 ns per profile site claim is optimistic
- [ ] §7.3 (line 666) and §7.5 (line 701) both estimate "~2 ns per
      site" for the type-bitmask and per-call-site profile updates.
- [ ] The aarch64 sequence in §7.3 (lines 668–678) is six
      instructions including two loads and two stores. Hot in cache
      it's ~3 cycles, ~1 ns at 3 GHz. Under cache miss a single
      profile point can hit ~50 ns. With store-buffer effects on
      real workloads the observed cost is typically 4–6 ns. With 4
      profile sites per function entry × 10⁹ function calls/s =
      40 ns/call extra = measurable.
- [ ] **Cross-scheduler cache-line contention is not analysed.** The
      profile feedback vector lives per-function (§7.1), so multiple
      schedulers running the same function concurrently all write to
      the same cache line. Even with relaxed atomics, false sharing
      between adjacent slots will move that line through the L1s
      repeatedly. On a 32-core box this can dominate the cost.

We need to have per scheduler counters here, we cannot update them
from all schedulers. In another JIT experiment I did, we made it
so that only scheduler 1 did profiling, this because load is normally
compacted to schedulers 1 and given enough time all the hot code will
run on that scheduler.

- [ ] Required: actually measure on representative workloads in
      Phase 0; budget the realistic number. The cheap claim makes
      "T1 stays the default; profiling is cheap" look stronger than
      it really is.

**Resolution: per-scheduler counters + scheduler-1-only profiling
+ measure in Phase 0.** Profile slots are sharded per scheduler
to eliminate cross-scheduler false sharing. Only scheduler 1 emits
profile updates by default, relying on the OTP scheduler's load-
compaction behaviour to put hot code on scheduler 1 over time.
Phase 0 deliverable: micro-benchmark the profile-update cost on
aarch64 (Apple Silicon + Linux ARM64) on representative workloads
(F2 corpus); budget the realistic number into §7.3 and the
tier-up overhead estimate.

### C9. M/(M-U) formula is borrowed without justification
- [ ] §15.1 (line 1492) uses JSC's threshold scaling
      `* (M / (M - U))` where `M` is total cache, `U` is used.
- [ ] JSC's formula is calibrated against its specific workload mix
      (lots of small JS contexts). Erlang has very different blob
      sizes (functions are typically small) and different cache
      pressure (one big code cache for a long-running VM). The
      formula may be wrong for our shape. State why we're keeping it
      or pick a different scaling.

We use this scaling as a starting point.

---

## D. Missing content

### D1. No test strategy
- [ ] The plan does not describe how T2 is *tested*. JSC has the
      JIT-test harness with deterministic deopt injection. We have
      EUnit, Common Test, the existing test-suite runners — but
      *what* gets tested?
- [ ] Required: a §X "Testing strategy" section covering:
      - Identity-transform correctness suite (Phase 1: T2 must
        produce identical observable behaviour to T1 on all of
        OTP's existing tests).
      - Deopt stress tests (every `speculate_*` in v1 should have a
        forced-deopt path tested).
      - Watchpoint invalidation tests (purge at every meaningful
        moment).
      - Concurrency stress (T2 compile while target function is
        executing in T1 on another scheduler).

We need to write specific testcases that stress these points in the
code with special BIFs that allows us to reliable trigger them.

### D2. No compile-error path
- [ ] §8 (line 763–767) says compiles have "a hard cap at ~10 ms
      (compile abort + leave function on T1)". §8.3 mentions
      "deliberately don't do" passes. What about *failures*?
- [ ] Possible failures: assertion in T2 IR validation, asmjit
      backend rejects some sequence, watchpoint registration fails
      because the literal table changed mid-compile, OOM in compile
      thread, encountering a Phase A op that's actually unsupported.
      Each needs a defined fallback (assume "leave on T1" everywhere
      — but say so, and define what's logged).

Any of these that indicate a bug in the JIT should abort the beam. The
only that is a valid abort is encountering an unknown op which should result
in a direct blacklist of that function.

- [ ] On repeated compile failure for the same `{M,F,A}`: does the
      function get permanently blacklisted, or does the call counter
      keep retripping forever? Is the failure logged in
      `t2_stats` (§16 has `t2_compile_failures` but no per-function
      reason)? Spell out.

**Resolution.** Compile failure → blacklist that `{M,F,A}` for
the lifetime of the loaded module; cleared on module reload. §16
extends with a per-function failure log:

```
t2_compile_failures_by_function => #{
    {M,F,A} => #{ reason => atom(),
                  count  => N,
                  last_failed_at => Timestamp }
}
```

Plan edit: extend §16's `erlang:t2_stats/0` schema; add
`erlang:t2_compile_failures/0` and per-`{M,F,A}` query.

### D3. JIT server concurrency model
- [ ] §17 "Decisions resolved" (line 1790–1797) decides on "JIT
      server process under `kernel/code` + dirty CPU scheduler".
      That's the queueing layer. But:
- [ ] **An Erlang process is single-threaded by construction.**
      Multiple compile requests come in concurrently — does the
      server serialise them all through itself before dispatching?
      That's a bottleneck. Or does it dispatch directly to dirty
      schedulers and only co-ordinate completion (which avoids the
      bottleneck but makes dedup, eligibility, and queue-length
      tracking harder)? Pick one.

We don't do multiple parallel compilations, so it should serialize.

- [ ] Multiple compiles in flight on different dirty schedulers —
      can they touch the same shared state (T2 metadata, watchpoint
      table)? The plan doesn't say.
- [ ] Is there a single shared T2 IR allocator/arena, or one per
      compile? Memory accounting?

For the T2 IR (that is thrown away after compilation to assembly),
it just allocates the memory and then removes it.

- [ ] Worker threads writing into the JitAllocator (§13.1) —
      asmjit's JitAllocator isn't documented as thread-safe; do we
      need a mutex, or one allocator per scheduler?

There is only one worker thread, so no mutex needed.

### D4. Multi-scheduler compile thread safety
- [ ] Even with the JIT server serialising requests, the compile
      *output* (the T2 blob) lands in the same memory the schedulers
      read from. The atomic export-table flip (§3) is mentioned but
      not specified for the multi-scheduler case.
- [ ] Required: explicit memory ordering when installing a T2 blob.
      Is `Export.addressv` updated under a barrier? Do callers see a
      consistent view? aarch64's weak ordering means this matters.
- [ ] If compiles can run in parallel on different dirty CPU
      schedulers, the T2 code-cache allocator needs to be
      thread-safe. §13.1 says "separate JitAllocator instance from
      BeamAsm's" but doesn't say *thread-safe*. **Per-scheduler
      arenas** are the standard fix (asmjit-friendly, no contention
      at allocation time). Decide and document.

### D5. ARM64 memory ordering not addressed
- [ ] On weakly-ordered aarch64, every cross-thread write needs
      explicit synchronisation. The plan mentions atomic export-table
      flips, profile-counter writes, watchpoint registration —
      collectively a lot of inter-thread state.
- [ ] §7.3 line 674 says "racy increments are tolerable". This
      conflates two very different races:

      **Semantically-tolerable races** (lost OR-bits in the type
      bitmask, an under-counted call counter). These are fine — the
      worst case is a slightly delayed tier-up.

      **Observation races** (the JIT manager reads a stale slot at
      compile time and makes a wrong inlining decision based on it,
      or sees a partially-updated `Export*` for the per-call-site
      monomorphic-target slot — §7.5). These are *not* tolerable;
      they produce wrong specialisation and unnecessary deopts.

The same mechanisms that we use today with T1 and code upgrades should
suffice here.

- [ ] Required: a §X subsection enumerating the cross-thread
      writes and their barrier requirements (acquire/release, full
      `dmb`, etc.). For the observation race, the JIT manager needs
      acquire-load when reading profile slots and release-store at
      the update site.

### D6. Coverage methodology for percentages
- [ ] Multiple places quote coverage:
      - "raises tier-2-eligibility from ~60–70% of functions to
        ~85–90%" (§17 Phase 5, line 1672–1673),
      - "Coverage ~98–100%. T2 can compile essentially all Erlang
        code" (Phase 8, line 1693).
- [ ] These numbers don't have a methodology. What's the corpus?
      Number of functions where every BEAM op is in the supported
      phase set? Weighted by call count? A pre-Phase-0 measurement
      (counting BEAM ops in OTP + a representative app code) would
      replace these placeholders with real numbers.

I think these numbers come from the instructions in the Erlang/OTP
code base.

**Resolution: re-measure in Phase 0.** Add a Phase 0 deliverable
to the §17 task list: count BEAM ops in OTP + a representative
application corpus (RabbitMQ / dialyzer / elixir compiler — same
benchmarks as F2) and bucket by phase coverage (A / B / C / D /
E). Replace the placeholders in §17 Phase 5 / Phase 7 / Phase 8
with the measured numbers. Keep the methodology in the plan so
later updates can re-run it.

### D7. Branchy allocation under loop unrolling
- [ ] §10.6 (line 1110–1129) shows unrolling with combined
      `test_heap` for a fixed K-iteration batch. Works when every
      iteration allocates the same amount.
- [ ] But the body of `lists:filter/2` — kept iterations allocate
      a cons cell, dropped ones don't. Naive unrolling × K = up to
      K cells, but the actual allocation depends on data.
- [ ] Two paths: (a) reserve K cells, free unused — wastes heap,
      makes GC accounting harder; (b) per-iteration `test_heap`
      survives, defeating the point. Pick one and document.

What we can do is that we make a single test heap to make sure we
have enough space for the worse case scenario, but only update
the heap pointer when we actually use the heap.

### D8. T1 PC table existence assertion needs verification
- [ ] §9.1 (line 866–871) relies on "the T1 blob's per-instruction
      PC table (which BeamAsm already maintains for line-number
      debugging)". I haven't verified this; it sounds right, but
      Phase 0 should validate that it's complete and stable enough
      to use as a deopt target index.
- [ ] If the table is only maintained for DWARF/line-info and gets
      sparse compilation (some BEAM ops don't emit a PC entry), T2's
      deopt model breaks. Add an explicit Phase 0 audit task.

fix

### D9. Tracing / system-event matrix beyond §12.5
- [ ] §12.5 enumerates trace primitives at a high level. But
      `erlang:trace/3` flags are richer: `arity`, `running_procs`,
      `procs`, `set_on_first_link`, `garbage_collection`, `send`,
      `receive`, `call`, `return_to`, plus all the match-spec
      actions (`message`, `set_seq_token`, `enable_trace`, …).
- [ ] Equally relevant: `erlang:system_monitor` events (`long_gc`,
      `large_heap`, `long_schedule`) — these fire from the runtime
      based on observed process state. T2's deferred-X-flush /
      register-allocator divergence may break the runtime's view of
      "process state" at the moment a system_monitor sample lands.
- [ ] And: `erlang:trace_pattern(_, true, [global])` — enabling
      tracing on every export of a module wholesale. Does T2 have to
      jettison every blob that called into that module?

Yes, most likely it will have to. We can make the jettison temporary
as most likely the trace will dissapear after a while.

- [ ] §17 Phase 0 (line 1567–1577) mentions "trace audit" as a
      deliverable. Move (or copy) the full matrix into §12.5 once
      the audit completes — but at minimum the plan should commit to
      producing one in Phase 0 and to using it as an input for v1
      scope. Currently §12.5 has only "examples", and §17 mentions
      it once.

**Resolution: ship the matrix in §12.5.** Phase 0 deliverable
becomes a complete table covering:

- Every `erlang:trace/3` flag (`call`, `return_to`,
  `running_procs`, `procs`, `garbage_collection`, `send`,
  `receive`, `arity`, `set_on_first_link`, `set_on_link`,
  `set_on_first_spawn`, `set_on_spawn`, `silent`, `timestamp`,
  `cpu_timestamp`, `monotonic_timestamp`,
  `strict_monotonic_timestamp`).
- Every match-spec action (`message`, `set_seq_token`,
  `enable_trace`, `disable_trace`, `display`, `caller`, `silent`,
  `trace`, `process_dump`, `exception_trace`, `return_trace`).
- Every `erlang:system_monitor` event (`long_gc`, `large_heap`,
  `long_schedule`, `long_message_queue`, `busy_dist_port`,
  `busy_port`).
- `erlang:trace_pattern(_, true, [global])` wholesale enable.
- `save_calls`.

For each, classify: **inline-preserved** / **Export-indirection-
preserved** / **requires jettison** / **requires temporary
jettison-and-recompile**. The matrix lands as a table in §12.5.

### D10. Compile-pipeline observability format
- [ ] §16 (line 1529–1558) lists ETS counters. What about logs?
      JSC has a JIT logger (`-d` flag) that prints per-compile
      decisions: which functions were compiled, why a speculation
      was inserted, why a recompile happened. Erlang's analogue
      would be `logger:debug` integration or a dedicated trace event.

Yes, we need debug tools like this. There is +Jdump today that should
enable T2 dumping as well.

- [ ] Include in §16: a per-compile event log (configurable,
      default off, costs nothing when off). Same for deopts.

---

## E. Editorial cleanup

fix all these.

### E1. Duplicate `### 6.4` heading
- [ ] See A2.

### E2. §17 "Decisions resolved" mixed with "Still open"
- [ ] See A4.

### E3. §15.4 cross-reference
- [ ] See A5.

### E4. "T1 blob backpointer" misdescribed
- [ ] §13.3 (line 1433) says "Backpointer to the T1 blob (for fast
      revert)". But T1 blobs aren't separately allocated for
      tier-2-eligible functions — they're part of the module's
      BeamAsm code allocation. "Revert to T1" is just an
      `Export.addressv` store; the T1 address is always known via
      the export table. The "backpointer" is implicit, not a new
      field.
- [ ] What *is* worth recording per-blob is the **T1 PC table
      reference** (used for outer-function deopt resolution per
      §9.1). Rename the §13.3 entry to that, or delete it.

### E5. §9.6 vs §10.2 cross-reference confusing
- [ ] §9.6 (line 968–978) discusses speculative-fun deopt, then
      hands off to §10 ("Covered in v1 — §10."). §10.2 line 1018
      has "Speculative fun specialisation (§9.6)" as a v1 non-goal.
      The reader has to round-trip §9.6 ↔ §10.2 to figure out the
      actual policy: *constant-fun* inlining is in v1, *speculative-fun*
      inlining is not.
- [ ] Restructure: state the rule once (probably in §10.1) and
      remove the back-reference in §9.6.

### E6. §10.4 "annotation evolution" understates implementation
      difficulty
- [ ] §10.4 step 1 says "AOT annotates ... A new annotation
      `jit_inline => #{fun_arg_pos => N}` on `b_function.anno`". This
      glosses over: (a) what counts as "the function the annotation
      applies to" (the wrapper, the recursive helper, both), (b) how
      the inliner decides whether to apply it when `fun_arg_pos` is
      a non-first parameter, (c) how it handles funs of non-fixed
      arity at the inlined site.
- [ ] Phase 3's effort estimate likely doesn't reflect this either
      (see C7).

### E7. §10.4 "ported vs generated" is two very different impls
- [ ] §10.4 (line 1052–1055) says the SSA expansion shapes for the
      lists BIFs "should be derived from `sys_core_fold_lists.erl`
      directly — either ported to T2 IR construction at JIT-init
      time, or generated through the `core_to_ssa` pipeline once and
      cached". These are *very* different implementations:

      - **Ported.** T2 hand-translates the Core Erlang expansion
        templates into C++ IR-builder calls. Static, fast, no
        Erlang runtime dependency at JIT-init.
      - **Generated.** T2 invokes `core_to_ssa` at JIT-init to
        compile the templates into SSA, then caches the result.
        Dynamic, depends on the Erlang AOT pipeline being usable
        from inside the runtime, but stays in sync with
        `sys_core_fold_lists.erl` automatically.
- [ ] The "should" hides a real implementation choice. Pick one and
      remove the alternative, or flag the decision as open.

### E8. Three "v1" definitions
- [ ] §1 defines v1 as "first version covering the goal set". Phase
      breakdown in §17 calls Phases 0–4 "v1 total". Some sub-sections
      refer to "v1 default" (e.g. §6.2 item 4) for the strict-state-
      preservation choice. These are all the same v1, but the reader
      has to triangulate. Make Section 1 say
      "v1 = Phases 0 through 4 (≈40 weeks)" once explicitly.

### E9. Cross-references to non-existent sections
- [ ] §6.5 (line 568) → "Worked example" is currently labelled 6.5,
      but if 6.4 dedups (see A2), all subsequent §6.x references
      shift. Verify all references to §6.5 throughout the document.

---

## F. Strategic concerns

### F1. 40-week / 18-month calendar feasibility + branch hazard
- [ ] Appendix B says 40 weeks single-engineer with "Multiply ~2×
      for total calendar time" — so ~80 weeks calendar = ~18 months
      from start to v1. For a feature this central, that's a long
      hangout in mainline before users see value. JSC's Maglev
      shipped its first version in something like 9 months (V8 had
      more engineers; LinearScan rewrite was deferred — ZJIT's
      pattern).
- [ ] **Branch-vs-master integration hazard.** §17 says "each phase
      ships something measurable" but doesn't say which phases land
      in master vs which stay on a branch. Without intermediate
      merges, an 18-month branch is its own project-management
      hazard — the merge surface area accumulates the entire delta
      against a fast-moving runtime.
- [ ] **The AOT-side change has to land first.** The BEAM SSA
      emission in the AOT compiler (Phase 0) needs to land in master
      well before T2 v1 ships, otherwise every master-merge is a
      giant integration. Plan should commit explicitly to "Phase 0
      AOT changes ship to master before Phase 1 starts" as a
      sequencing rule.
- [ ] Possible mitigations: parallelise more (e.g. AOT changes in
      Phase 0 can run concurrently with IR-builder work), or descope
      v1 to skip Phase 3.5 (LICM + unrolling) and ship the inlining
      MVP without the loop optimization layer. The latter risks v1
      being underwhelming on the canonical `lists:foldl` case (which
      is the headline win — see F2).

1. We will have to have some small prototype quickly that shows that
   the concepts will deliver real value, but if they do, then 18 months
   is ok.
2. All the phases are on a branch until we are done. If the early
   prototype shows promise, we will have to co-ordinate with upstream.
3. It will not ship separately, we'll co-ordinate with master to keep
   merge conflicts at a minimum.

### F2. Inlining thesis not sized against real workloads
- [ ] §2 asserts "inlining is the core value" — but the plan never
      quantifies the upside on representative code. How much of an
      Erlang server's hot path is `lists:*` higher-order BIF calls
      vs. local function calls vs. cross-module monomorphic calls?
      Without that, we can't tell whether v1 captures the majority
      of the inlining benefit or only the tail.
- [ ] Required (Phase 0): profile a representative workload (cowboy
      response handler? Ranch acceptor loop? RabbitMQ message dispatch?
      a real Shopify Ruby/Erlang equivalent?) and bucket inlining
      candidates by category. If 60% of hot inline candidates are
      polymorphic remote calls, v1 (monomorphic-only) misses most
      of the wins.

Yeah, we need some good benchmarks. RabbitMQ have been good in the
past. Also dialyzer + the elixir compiler are good candidates.

### F3. No comparison with simpler alternatives
- [ ] Plausible alternatives the plan doesn't discuss:
      - **Make `inline_list_funcs` default-on in the AOT compiler.**
        Probably captures 30–60% of the inlining wins T2 promises,
        at zero runtime cost. Why is the JIT route preferred? Cost
        in code size? Cost in compile time? Worth a one-paragraph
        comparison.
      - **Richer AOT type information + better static specialisation.**
        The AOT compiler could emit a "guarded specialised" version
        of a function (one for `is_integer` arg, one for `is_list`,
        with a dispatch by type). That's ahead-of-time speculation,
        no runtime profiling needed. Not as flexible as T2, but much
        cheaper to ship.
      - **Profile-guided AOT.** Erlang programs typically run a
        profiling phase, then re-deploy. Why not compile with profile
        data instead of doing it at runtime?
- [ ] None of these are necessarily better. But the plan reads like
      "T2 is obviously the right answer", and a brief
      "alternatives-considered" section would strengthen the case
      for choosing T2 specifically.

Ok, add such sections.

### F4. HiPE lesson incompletely applied
- [ ] §14 invokes the HiPE lesson ("This is where HiPE lost") for
      mode-mixing. But HiPE's failure modes were broader: code-size
      bloat, divergent dispatch (own calling convention), trace
      semantics drift, and "too much code, too few maintainers"
      (Appendix B explicitly mentions this last one).
- [ ] The plan addresses calling convention (§12.1) and tracing
      (§12.5) well. It addresses code size loosely (§13.1 budget).
      It does not address **maintainership** at all — who owns T2
      after v1 ships? A 16-KLOC C++ optimizer ≠ HiPE-sized, but it's
      also not zero-effort to keep working as the AOT compiler and
      runtime evolve. If the maintainership story is "OTP team owns
      it", that's a real ongoing commitment that should be named.

It is owned by OTP team.

### F5. Asymmetric tier sequencing
- [ ] Most reviewed JITs (JSC, V8, ZJIT, HotSpot) are "warm tiers
      below T2" — Sparkplug/Baseline, YJIT, C1. Erlang's T1 is
      already a fully optimising baseline (BeamAsm with template
      JITting). The plan implicitly assumes "T2 above T1 is the
      same shape as T2-above-baseline elsewhere" — but the gap from
      T1 to T2 is smaller (BeamAsm is faster than V8 Sparkplug,
      JSC Baseline) so the *upside* may also be smaller.
- [ ] Concretely: ZJIT shipped at "slower than YJIT" in v1 (per
      `zjit.md` §7). Our v1 might ship at "comparable to, or even
      slightly slower than, BeamAsm" on functions where the AOT
      already won most of the wins. This is mentioned obliquely in
      §17 risks; should be a headline expectation in §1 (Goals) so
      stakeholders aren't surprised.

We need to start by analyzing the code of benchmark projects and
see which patterns a T2 could help with there. The first T2 implementation
cannot be slower than T1 as then it is dead in the water.

---

## How to use this document

1. Walk top to bottom.
2. For each `[ ]`, reply inline (this file is meant to be edited
   directly):
   - `[x] fix` + description of the fix you'll apply (or commit hash).
   - `[x] wontfix` + 1-line reason.
   - `[x] disputed` + 1-line counter.
   - `[x] defer` + which phase / which open question it goes into.
3. When all are addressed, the surviving fixes feed back into
   `T2.md` and this critique file gets archived (or deleted —
   it's superseded by the plan it improved).

---

# Second-pass review

After applying the resolutions for sections A–F, several new issues
surface — partly because the resolutions changed enough text that
old text is now stale, partly because the resolutions exposed
architectural questions that weren't visible before. Sections
G–L below.

## G. New contradictions introduced by recent edits

### G1. Phase 3 effort: §17 says 10 weeks, Appendix B says 14–16
- [ ] §17 line 2128: "Phase 3 — Inlining MVP + loop recovery +
      intrinsics (≈10 weeks)". Appendix B was updated to
      14–16 weeks (C7 resolution); §17's title wasn't. Sync the two.

### G2. Phase 0–2 calendar arithmetic stale
- [ ] §17 "Sequencing rationale" (line 2218): "Phases 0–2
      (~18 weeks) validate or invalidate the entire approach".
      With Phase 0 grown to 12w and Phase 1 4w, Phase 2 6w, the
      total is **22 weeks**, not 18. Update the calendar claim.

### G3. Phase 3 task list still lists "Multi-frame deopt dispatch"
      as the hard part
- [ ] §17 Phase 3 (line 2141): "Multi-frame deopt dispatch (the
      hard part)". With B1's eager-CP-push resolution, deopt is
      uniform regardless of nesting depth — there *is* no
      "multi-frame deopt dispatch" task anymore. Replace with
      "Inlined-region deopt stub emission" or remove.

### G4. §18 Risk #1 fallback (a) names a model we rejected
- [ ] §18 risk #1 (line 2229): "(a) the strictest 'identical at
      every BEAM instruction boundary' rule from earlier drafts —
      correct but constrains the allocator unnecessarily". A1
      resolved the relaxed sync-point model as **the design**, not
      a roadmap. The risk should be re-phrased: the failure mode
      is not "we didn't relax fast enough" but "the sync-point
      identification or the constrained allocator turned out to be
      complex enough to threaten the compile-time budget".
- [ ] Same risk's mitigation: "Phase 1's identity transform ships
      option (a) by default; relaxation to the sync-point model
      happens when measurements show it's worth the engineering
      cost." But Phase 1 was rewritten to ship the relaxed model
      directly. Mitigation text is stale.

### G5. §18 Risk #2 says "measure in Phase 1"
- [ ] §18 risk #2: "Compile-time overhead too high. ~1 ms target
      may slip, especially with inlining. **Mitigation**: ...
      measure in Phase 1 and tune." But Phase 1 explicitly runs
      no optimization passes (it's the identity transform). The
      compile time in Phase 1 is the absolute floor and isn't
      representative of v1 cost. Move the measurement target to
      Phase 2 (when speculation+type narrowing land) and Phase 3
      (when inlining lands).

### G6. §3 architecture diagram: stale "BeamAsm blob retained for
      jettison fallback"
- [ ] §3 line 244: "BeamAsm blob retained for jettison fallback".
      Per E4 resolution, T1 blobs aren't separately allocated —
      they're part of the module's BeamAsm code. "Revert to T1"
      is just an `Export.addressv` store. The diagram still
      implies a separate "T1 blob" object retained alongside.
      Reword: "Export.addressv flips between T1 and T2 entries;
      no separate retention".

### G7. §3 architecture diagram: "inlined regions register
      framestates" misleading
- [ ] §3 line 231: "inlined regions register framestates". With
      B1's resolution, framestates are **codegen-only metadata**
      consumed at stub emission. They aren't "registered" against
      anything at runtime. The diagram still implies runtime
      framestate state. Reword: "inlined regions emit deopt stubs
      using codegen-time framestate metadata".

### G8. §6.5 still describes lazy CP materialisation
- [ ] §6.5 line 644: "Nested inlining: each inlined region adds
      one more frame to the framestate chain. Deopt unwinds
      through `parent_fs` references to **materialise N CP frames
      on the Erlang stack** and resume in T1 at the outermost
      call." This is the lazy-CP wording B1 explicitly replaced.
      With eager-CP, CPs are pushed at inlined-region *entry*,
      not materialised at deopt. §6.5 needs rewriting to match
      §9.2.

### G9. §6.6 worked example: `framestate` shown as IR op
- [ ] §6.6 example shows
      `framestate [%list→x0], ip: double_all/1+0` as an IR op in
      the function body. With B1's resolution, framestates are
      compile-time metadata, not IR ops. The example reads like
      `framestate` is generated code. Either prefix the line with
      a comment ("`// codegen-only marker`") or drop it from the
      example body and show the deopt stub instead.
- [ ] §6.6 example also uses `%result = lists_reverse(%acc)` as
      if `lists_reverse` were an IR op or known intrinsic. Should
      be `%result = call_ext lists:reverse/1, [%acc]` (or marked
      `// sketch`).

### G10. §9.3 says deopt is "only legal at BEAM instruction
       boundaries" — contradicts §6
- [ ] §9.3 line 1046: "Deopt is only legal at points where a
      valid BEAM-machine state can be reconstructed — which means
      at **BEAM instruction boundaries** in the original code."
- [ ] §6 (the resolved A1 model) says deopt is legal at **sync
      points only** — function entry, calls, returns, GC, BIF
      boundaries, speculation guards, tracing-relevant sites,
      receive safe points. That's a strict subset of "every BEAM
      instruction boundary". The two phrasings disagree.
- [ ] Reword §9.3: deopt is legal at sync points (where the live-
      X-reg map is recorded). The mid-arithmetic-unsafe case
      (between `untag_int` and `add_small`) follows from this:
      those are between sync points by construction.

### G11. §15.4 still talks about "deopt via framestate"
- [ ] §15.4 line 1888: "In inlined regions, deopt via framestate
      (§9.2)." With B1, the framestate is consumed at codegen;
      the runtime mechanism is the deopt stub. Reword: "In
      inlined regions, deopt via the per-region deopt stub,
      whose moves were emitted from codegen-time framestate
      metadata".

---

## H. Architectural under-specification (new)

### H1. Tagged values across GC inside inlined regions
- [ ] §12.3 says "T2 v1 keeps untagged values out of the X
      register array — they'd confuse the GC scanner. Untagged
      values live only in scratch CPU regs, never spilled to X
      slots." But what about **tagged** SSA values inside an
      inlined region that are live across a GC point (e.g. a
      tagged accumulator threaded through a `lists:map` body that
      allocates)? They have to be reachable from a GC root —
      either spilled to a Y slot (which then needs to be marked
      live in the BEAM stack frame), or referenced via a stackmap
      the GC walker consults.
- [ ] The plan's flush rule ("inliner emits flush sequences at
      each potential GC site within an inlined region") doesn't
      say *where* the flush goes. To Y? Which Y slots? Tracked
      how? Required: an explicit invariant for "live tagged values
      at GC points inside inlined regions go to Y slots N..M; the
      flush is part of the lowering of any op that may GC".

### H2. Inlined-region register pressure on the T1-pinned regs
- [ ] §12.1: x25–x28 = XREG0..XREG3, x15–x17 = XREG4..XREG5.
      These are the outer function's pinned X regs at sync points.
      Inside an inlined region, the allocator can use whatever
      scratch registers are available — but x25–x28 still hold
      the *outer* X0..X3 at the inlined-region's entry sync
      point. If the inlined region needs more registers than the
      ABI scratch set provides (x0–x14), can it use x25–x28?
- [ ] If yes: the outer X0..X3 are clobbered. They must be re-
      established at the next sync point (which is fine, but the
      moves cost something).
- [ ] If no: the inlined region has fewer registers than the
      outer function. Inlining a complex callee may not have
      enough working registers to avoid spills.
- [ ] Pick a discipline. The eager-CP model already pushes the
      parent CP at inlined entry, so a single sync point is
      formally entered there; the allocator policy can clobber
      x25–x28 within the region as long as it restores at sync
      points. But this is unstated.

### H3. Sync-constraint conflict: same SSA value, different X
       regs at different sync points
- [ ] §11.2 says "values pinned by an active sync constraint
      cannot be displaced; values that need to live across a sync
      point either sit in the X reg the constraint pins, or get
      spilled and reloaded."
- [ ] But what if SSA value V must live across two sync points
      S1 and S2, and the T1-mandated X-reg for V at S1 is x0
      while at S2 it's x2 (because T1's register allocator put
      it differently)? V can't be in two places. The allocator
      either spills to Y between S1 and S2 then reloads to x2 at
      S2, or moves V from x0 to x2 between sync points. **Both
      are extra code that the "free between sync points" model
      doesn't visibly account for.**
- [ ] Required: either (a) demonstrate this conflict is rare in
      practice, (b) document the spill-and-reload strategy
      explicitly, or (c) constrain the allocator's freedom to
      avoid creating multi-X-reg-bound live ranges.

### H4. Active execution counter atomicity
- [ ] §13.3: "active execution counter (incremented in the T2
      prologue ...)". Multiple schedulers can call the same T2
      blob concurrently. Per-scheduler shards (matching the
      profile counters from C8) is the standard fix; without
      sharding, the counter is either non-atomic (lost
      increments, biased eviction policy) or atomic (per-call
      cost in the prologue is now an `ldaxr/stxr` loop).
- [ ] Required: state explicitly that active execution counters
      are per-scheduler-sharded, with the eviction code reading
      the union.

### H5. §15.3 queue-drop policy: counter reset on drop
- [ ] §15.3: "If the queue exceeds a high-water mark, further
      requests are dropped (function continues in T1; counter
      retrips later)."
- [ ] But §7.4 says "counter resets to a 'pending compile'
      sentinel to suppress duplicate enqueues". On drop, the
      counter is at sentinel; if we don't reset it, the counter
      never retrips — the function never compiles. We need to
      explicitly reset the counter back to a non-sentinel value
      on drop.

### H6. Stable speculation-site IDs across recompiles
- [ ] §9.5 / B7 resolution: per-blob exit-reason buffer indexed
      by *speculation-site ID*. The site ID is presumably a
      sequential integer assigned at codegen.
- [ ] On recompile, the IR may be different — different
      speculations may be inserted, dropped, reordered. A
      sequential index from compile N doesn't correspond to a
      site in compile N+1. The recompile heuristic ("any site
      with `fail_count > T` gets its speculation widened") needs
      a *stable* site identifier — e.g. `{source BEAM PC,
      speculation kind, narrowed type}` triple — not a sequential
      index.
- [ ] Required: spell out the stable-ID scheme.

### H7. Profile-feedback conflict resolution
- [ ] §5.4 lists three sources of types: AOT-emitted, profile
      feedback, forward dataflow. Priority order is given but
      conflict semantics aren't. Cases:

      **Profile narrows AOT.** AOT says `integer`, profile says
      `[1, 2, 3]` (set of small atoms). Profile narrows, fine.

      **Profile contradicts AOT.** AOT says `integer`, profile
      says `atom`. Either AOT is wrong (compiler bug?) or profile
      saw a transient anomaly (a process that called the function
      before tier-2-eligibility was checked, with a corrupted
      register?). What's the policy?
- [ ] Required: explicit conflict-resolution policy. Probable
      answer: AOT is ground truth; profile is filtered against
      AOT — observations outside the AOT type are dropped.

### H8. Speculation-range auto-selection
- [ ] §9.4 picks ±2^58 as the example range. But how does the
      optimizer *choose* the range automatically?
- [ ] If the profile saw integers in [-1000, 1000], speculating
      to ±2^58 is fine — wider than needed but safe, no overflow
      check needed. If the profile saw integers in [-1, 2^60],
      speculating to ±2^58 fails on most observations. The
      narrower the speculation, the more deopts; the wider, the
      more we have to fall back on overflow checking.
- [ ] Required: an algorithm. Probable answer: take the
      observed range from the profile, snap to the nearest
      "leaves-headroom-for-add" range, with a cap.

### H9. Audit list for "audited-pure-or-semi-pure" callees
- [ ] §10.1: "monomorphic + size ≤ threshold + audited-pure-or-
      semi-pure ⇒ inline". What's the audit list?
- [ ] If it's a fixed manifest of OTP-stdlib functions, that's
      one thing. If it includes arbitrary user-module functions
      based on per-function attributes, that's another.
- [ ] Required: define the manifest. Initial set probably =
      `sys_core_fold_lists.erl` set + a curated small set of
      common pure functions (lists:reverse/1, lists:append/1,2,
      maps:get/2, maps:put/3, etc.). User-module functions opt
      in via a `-jit_inline` attribute or annotation.

### H10. Per-callee deopt-skip rule too coarse
- [ ] §10.3: "Skip inlining if the callee deopt'd in a previous
      T2 compile". This skips the callee globally, even if the
      deopt was at a specific site that may not exist in this
      caller's context.
- [ ] Better rule: skip inlining at a *specific call site* if a
      prior compile saw a deopt traced to inlining the callee at
      that site. Per-site, not per-callee.
- [ ] Combined with H6: this needs a stable identifier for "this
      call site, this callee" that survives recompiles.

### H11. `lists:foldl(MyFun, ...)` where MyFun is non-literal
- [ ] §10.5: loop recovery during inlining of `lists:foldl/3`
      requires the fun argument to be a constant-known fun (per
      §10.1). What about
      `lists:foldl(MyFun, 0, L)` where `MyFun` is bound from
      elsewhere — passed in as a parameter, returned from a
      function call, looked up in a record?
- [ ] In v1, this case **falls back to a regular `call_ext` to
      `lists:foldl/3`** — the higher-order helper isn't inlined
      because the constant-fun precondition fails. That means
      most user code patterns
      (`fun_handler(Items, F) -> lists:map(F, Items)`) get no T2
      win at all. Worth stating explicitly so expectations are
      set.

---

## I. Wrong numbers / engineering bugs (new)

### I1. Phase 1 emitter LOC short of §11.1's estimate
- [ ] Appendix B Phase 1 = "C++ ~2 KLOC". §11.1 line 1397:
      "Estimated emitter LOC: ~1.5 KLOC ARM64 + ~1 KLOC shared"
      = ~2.5 KLOC just for emitters. Plus the IR builder, sync-
      point pass, jettison plumbing — which are also Phase 1.
      Likely 3–4 KLOC for Phase 1 in reality.

### I2. `length/1` fast-path threshold not specified
- [ ] §10.7 / C5: `length/1` lowers to "an inline fast-path with
      a slow-path tail call to the existing BIF implementation".
      But the BIF traps after ~4000 cells (CONTEXT_REDS-bounded).
      An inline fast-path traversing 4000 cells is *massive* —
      4000 cons-cell loads + branches inline = thousands of
      instructions per call site. That's not a fast path.
- [ ] Alternative: the inline fast-path traverses up to a small
      bound (say, 16 cells); if the list is longer, tail-call
      the BIF unconditionally. Loses some inline win on lists
      length 16–4000 but keeps code-size bounded.
- [ ] Pick a bound and document it.

### I3. Profile sharding not reflected in §7.2 struct
- [ ] §7.2 (line 717–728) shows `T2FunctionProfile` with a
      single `slots[]` array. C8 resolution introduced per-
      scheduler sharding. The struct should be:

      ```c
      typedef struct {
          Uint32         call_count;       // global
          Uint16         num_slots;
          T2ProfileSlot  slots[NUM_SCHEDULERS][num_slots];
      } T2FunctionProfile;
      ```

      Or equivalent. Update §7.2 accordingly.

### I4. §7.5 "CAS to POLY" mislabelled
- [ ] §7.5 line 794: "Update sequence per call: load slot;
      compare; conditional-store ... CAS to POLY". A load + cmp +
      conditional-store is not a CAS; it's three separate
      operations with no atomicity. With scheduler-1-only
      profiling (C8), atomicity isn't needed because only one
      scheduler writes — but the wording confuses the reader.
      Reword: "single-writer non-atomic load-compare-store
      sequence".

### I5. §15.2 saturation budget too short
- [ ] §15.2: "max 5 retries" with N=64 ticks per retry =
      5×64 = 320 ticks before giving up. JSC's actual rule
      involves much larger windows (thousands of ticks). For
      Erlang code with sparse profile observations (e.g. a
      gen_server that takes one type for the first 100 calls then
      stabilises), 320 ticks may not be enough.
- [ ] Either bump to N=256 with 8 retries (= 2048 ticks), or
      mark the value as "starting point, calibrated in Phase 0"
      like the size threshold.

### I6. §8.1 pass-list ordering: speculative-arith lowering after
       unrolling
- [ ] §8.1 has unrolling at step 12, speculative arithmetic
      lowering at step 14. After unrolling K iterations, the
      generic add/sub/mul ops are duplicated K times. Lowering
      to `add_small`/`mul_raw` at step 14 then generates K copies
      of the speculative form. If lowering happened *before*
      unrolling (between steps 9 and 10), unrolling would
      duplicate the already-lowered code, which is a smaller IR
      to walk in passes 11–13.
- [ ] Probably negligible perf-wise; matters mainly for compile
      time. Worth thinking about whether step 14 can move to
      step 9.5.

### I7. Allocation-sinking (step 13) before lowering (step 14)
- [ ] Allocation sinking moves allocations across IR. Many
      allocations in T2 are inside inlined regions (cons-cell
      construction during inlined `lists:map`). Sinking an
      allocation across a sync point would violate sync
      constraints — but the sync point is a property the lowering
      pass marks. If sinking runs *before* lowering, the marker
      isn't set yet; the sinker has to re-derive the sync points
      itself.
- [ ] Required: state which passes care about sync-point markers
      and where in the pipeline the markers become authoritative.

---

## J. Missing content (new)

### J1. No regression detection / CI gating
- [ ] §16A.5 mentions a "regression bench suite" but doesn't say
      *who runs it* or *when*. Required: a CI gate that runs
      the benchmark suite on every PR touching `erts/emulator/
      beam/jit/t2/`, with a hard-fail threshold (T2 must not
      regress against T1 by more than ε). Without this, the
      hard-floor commitment from §1 is aspirational.

### J2. No "profile data quality" metric
- [ ] If profile observations get stuck on the wrong type (e.g.
      due to scheduler-1-only profiling missing types that hit
      other schedulers), T2 speculates wrong and constantly
      deopts. The exit-reason buffer (B7) records *what* failed,
      not *why* the profile was wrong.
- [ ] Required: a `t2_profile_quality` counter — ratio of "T2
      speculations that fired correctly" / "T2 speculations
      total". Sustained low values indicate profile-collection
      bugs, not workload pathology.

### J3. No "stack-scan latency on schedule-in" risk
- [ ] B6's lazy stack scan does an O(stack-depth) walk on the
      first schedule-in after a jettison. For a process with a
      deep stack (telecom-style state-machine processes can have
      stacks thousands of frames deep), this is non-trivial work
      under the schedule-in hot path. Latency-sensitive
      applications may not tolerate the spike.
- [ ] Add to §18 risks and to §16 observability: a metric for
      "longest schedule-in scan" (max walk depth).

### J4. No discussion of dist (cross-arch nodes)
- [ ] In a cluster of mixed aarch64 + x86_64 nodes, the same
      module is loaded on both. The SSA chunk and `jit_inline`
      annotations are present in both BEAM files. On x86_64
      nodes (which don't have T2 in v1), the SSA chunk is wasted
      space.
- [ ] Bigger concern: hot-code upgrade in a distributed system.
      Node A (aarch64, T2 active) calls Node B (x86_64, T1
      only). The export-table indirection abstracts this — fine.
      But module reload on Node B affects only Node B's T1
      blobs; Node A's T2 blobs depend on local state, so cross-
      node dependencies are out of scope. **Worth stating
      explicitly that T2 reasons only about local state.**

### J5. No discussion of profile emission overhead on cold code
- [ ] Eligibility check (§7.1) emits profile-recording for
      every tier-2-eligible function. If 60% of functions are
      eligible (per §17 Phase 5 estimates) but only ~5% actually
      tier up, the other 55% pay the profile overhead for
      nothing. With ~2 ns per slot × 4 slots/entry × N calls/sec
      across all eligible-but-cold functions, this is a real tax
      on T1 baseline performance.
- [ ] Required: Phase 0 measurement task quantifying the
      "profile overhead on never-tier-up code" cost; compare to
      baseline T1 performance.

### J6. No "T2 disabled globally on first abort" mode
- [ ] §8.4 says compile bugs (IR validation, asmjit reject,
      watchpoint race) **abort the BEAM**. In production this is
      catastrophic — users running OTP in 24/7 systems can't
      tolerate a JIT bug crashing the VM.
- [ ] Required: a graceful-degradation mode (`+JT2safe_mode true`)
      where the same conditions instead disable T2 globally and
      log an error. Default: abort (development-friendly);
      production deployments opt into safe-mode.

### J7. Active execution counter not in §13.3 metadata struct
- [ ] §13.3 lists per-blob metadata including "active execution
      counter ... decayed exponentially". But §7 / §13.3 don't
      say *where* the decay happens or *how often*. Half-life
      "tied to the GC interval" — but GCs are per-process, not
      global. Does the runtime decay all blob counters every N
      GCs? Every N seconds via a system timer? Unspecified.
- [ ] Required: concrete decay implementation (probably a
      periodic timer in the JIT server that visits all blobs
      every K seconds and applies the decay).

### J8. No discussion of `apply/3` and dynamic dispatch
- [ ] §10.2 says "`apply/3` with non-constant arguments. Can't
      predict target." But Erlang code commonly uses
      `apply(M, F, Args)` with M/F coming from configuration or
      runtime data. Such call sites get the per-call-site
      monomorphic-target slot (§7.5) — even if the args are
      dynamic, the *target* the slot observes may be monomorphic
      in practice. Can T2 inline based on the slot, with a guard
      checking `M=ObservedM, F=ObservedF`?
- [ ] Probably v2 (it's "speculative-target" inlining, similar
      to speculative-fun). But §10.2 should be explicit: even
      monomorphic-by-observation `apply/3` is v2, not v1.

---

## K. Editorial cleanup (new)

### K1. §3 diagram references "framestates"
- [ ] See G7. Reword for codegen-only.

### K2. §6.5 lazy-CP wording
- [ ] See G8. Rewrite for eager-CP.

### K3. §6.6 example uses non-existent `lists_reverse` op
- [ ] See G9. Use `call_ext` form or mark as sketch.

### K4. §9.3 BEAM-instruction-boundary wording
- [ ] See G10. Realign with §6 sync points.

### K5. §10.4 references "the AOT compiler today has
       sys_core_fold_lists.erl (~400 lines)"
- [ ] The line count is incidental and will rot. Drop it or
      pin it to a commit.

### K6. §15.4 "deopt via framestate"
- [ ] See G11. Reword.

### K7. §17 Phase 4 doesn't mention the M/(M-U) re-tuning task
- [ ] §15.1 says "Phase 4 includes a measurement task to re-
      evaluate the formula against representative workloads",
      but Phase 4's task list doesn't include it. Add to §17.

### K8. §19 "Optimizations targeting message passing, ETS, GC,
       or NIFs" — overbroad
- [ ] §10.6's `test_heap` re-coalescing is technically a "GC
      optimization" (changes when GC fires). The blanket
      "T2 will never make these optimizations" is wrong as
      stated. Reword: "T2 doesn't optimize the *implementation*
      of message passing, ETS, GC, or NIFs — those wins live in
      the runtime. T2 may re-batch, hoist, or eliminate the
      *triggers* for them (e.g. coalesce `test_heap` calls)
      where the SSA structure permits."

### K9. Appendix A: `instr_t2.cpp` location vs §11.1 layout
- [ ] Appendix A places ARM64 T2 emitters at
      `arm/instr_t2.cpp`. §11.1 talks about "emitters" without
      specifying the directory. Reconcile: the ARM-specific
      emitters live in `arm/`; the architecture-agnostic glue
      lives in `t2/t2_codegen.cpp`. Make Appendix A reflect the
      split.

---

## L. Strategic concerns (new)

### L1. Cumulative steady-state tax on T1 baseline
- [ ] T1 baseline was previously zero-overhead. With T2 active,
      every tier-2-eligible function pays:
      - profile-emit cost (~2–50 ns × 4 slots/entry — per C8 bound
        to scheduler 1, so 1/Nschedulers of the tax overall)
      - per-call-site monomorphic-target slot writes (§7.5)
      - patchable-prologue overhead from the eligibility check
      - call counter decrement
- [ ] Per-call cost may add up to 5–10 ns extra per function
      entry. On a 10⁹-calls/sec workload that's 5–10% slowdown of
      *T1 code* even when no T2 compilation has happened.
- [ ] Required: §1's hard floor needs an explicit qualifier —
      "T2 enabled with v1 must not slow down *aggregate workload
      performance* by more than X%". Phase 0's profile-cost
      micro-benchmark and J5's tier-up-eligible-but-never-tiered
      measurement together quantify this.

### L2. Lazy stack-scan is incompatible with hard real-time SLAs
- [ ] B6's lazy stack scan does an O(stack-depth) walk on first
      schedule-in after a jettison. The high-water sweep
      (§14.2) compounds it: under memory pressure, every
      process gets walked even if it's not currently scheduled.
      For Erlang systems with hard latency requirements (Erlang
      Solutions's telecom customers, financial systems), this
      may be unacceptable.
- [ ] Mitigation strategies that preserve lazy semantics:
      bound the scan length (give up after N stack frames; force
      a synchronous deopt instead); rate-limit the high-water
      sweep; skip the sweep on processes with an "RT-bounded"
      flag.
- [ ] Required: explicit acknowledgement in §18 risks; a
      runtime knob to disable jettison (force always-recompile)
      for RT-critical processes.

### L3. T2 carries an irreducible stake in BeamAsm internals
- [ ] T2 reads BeamAsm's per-instruction PC table (§9.1), uses
      BeamAsm's calling convention (§12.1), reuses BeamAsm's
      global runtime fragments (§11.1), inherits BeamAsm's
      memory-ordering patterns (§3 memory note). Every change
      to BeamAsm is a potential source of T2 bugs.
- [ ] Maintenance reality check: an OTP team member modifying
      `arm/instr_*.cpp` can't tell from the diff whether T2
      depends on the structure they're changing.
- [ ] Required: a documented *contract* between T2 and BeamAsm
      — what BeamAsm-internal interfaces T2 depends on, with
      assertions in the BeamAsm code that fail if the contract
      breaks. Otherwise T2 silently rots.

### L4. The "1 KLOC Erlang" budget is aspirational
- [ ] Appendix B says Phase 0 has "Erlang ~1 KLOC" and Phase 3
      has "Erlang ~0.5 KLOC". Total Erlang code = 1.5 KLOC for
      v1.
- [ ] What goes into that?
      - JIT server process (§3) — gen_server: ~300 LOC.
      - SSA chunk emit in `beam_asm.erl` — ~200 LOC.
      - `jit_inline` annotation propagation in
        `sys_core_fold_lists.erl` — ~100 LOC.
      - `code_server.erl` invalidation hook — ~150 LOC.
      - Test helpers (§16A force-deopt BIFs, lifecycle drivers)
        — ~300 LOC.
      - Observability (`erlang:t2_stats/0`,
        `erlang:t2_info/3`) — ~200 LOC.
      Total: ~1.25 KLOC. Tight but feasible.
- [ ] What's *not* in that count: changes to the AOT compiler
      type-information chunk emission, integration of profile
      data into compile decisions, anything that touches
      `lib/compiler/`. If the AOT side balloons, the Erlang
      budget breaks. Worth flagging.

### L5. The HiPE failure mode wasn't just maintainership
- [ ] §17 maintainership commitment ("OTP team owns it") and
      Appendix B's "comfortably under HiPE's footprint"
      argument both lean on the size argument. But HiPE also
      lost because:
      - The optimization wins were narrow (numeric workloads
        only; nothing for typical Erlang services).
      - The integration cost (mode-switching overhead, calling-
        convention divergence) ate the wins on mixed workloads.
- [ ] T2 addresses (b) by sharing calling convention with T1.
      It *doesn't* yet have evidence that (a) doesn't apply —
      the inlining-thesis sizing (Phase 0 / F2 corpus) is the
      gate. If inlining wins are also narrow (only the small set
      of `lists:foldl`-shaped patterns benefit), T2 could end up
      with the HiPE problem of "real but small wins, not
      enough to justify the maintenance".
- [ ] §18 risks should add: "Phase 0 measurement shows
      inlining-driven wins are smaller than projected".
      Mitigation: descope to ship just the type-narrowing
      speculation (Phase 2 wins) without the inlining tier.
