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
