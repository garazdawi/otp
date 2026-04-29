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
- [ ] §1 (line 54–57) commits to "**The outer function preserves T1's
      abstract machine state at every BEAM instruction boundary**".
      §6.1 (line 442–446) and §6.4 (line 525–540) reverse this to
      "matches T1's **at every point where T2 could exit or yield** —
      and only at those points". §3 (the architecture diagram block,
      ~line 130–170) and §4 (Choices, line 199–225) inherit the strict
      framing.
- [ ] §17 "Decisions resolved during planning" (line 1761–1762) restates
      the strict version: "Outer function = identical T1 layout;
      framestates only in inlined regions." — contradicting §6.4.
- [ ] §6.2 item 4 (line 495–501) tries to reconcile by calling the
      strict version "the v1 default" and the relaxed one "later
      phases", but that policy isn't reflected anywhere else in the
      plan (the goals, choices, phases, and risks).
- [ ] **Concrete fix**: pick one and propagate. If the v1-strict /
      relaxed-later phasing is the real policy, add it explicitly to
      §1, §4, §17, and the Phase 1/Phase 4 deliverables in §17.

### A2. §6.4 duplicate heading
- [ ] Two `### 6.4` headings:
      `6.4 Why the relaxation matters` (line 525) and
      `6.4 Inlined regions` (line 542). The second should be `6.5`,
      and the existing `6.5 Worked example` (line 568) shifted to
      `6.6`.

### A3. §17 "Decisions resolved" no longer matches the plan body
- [ ] Row 4 (state-preservation model, line 1761–1762) — see A1.
- [ ] The "Decisions resolved" and "Still open" sections have drifted
      into mixed status (e.g. several "Still open" items are tagged
      "**Decided:**" inline). Consolidate: every item that has a
      decision should live under "Decisions resolved"; items lacking
      one stay under "Still open".

### A4. §15.4 cross-reference to §9
- [ ] §15.4 (line 1518–1520) cites "tracing (§12.5), watchpoint
      invalidation (§14), and recompilation backoff" as users of
      OSR-exit. The recompilation backoff machinery is described in
      §9.5, not anywhere in §14 or §15. Add the §9.5 cross-reference
      or move §9.5 under §15.

### A5. "Inlined-region" wording in §1 vs §6
- [ ] §1 says deopt in the outer function uses "no framestate
      machinery". §6.1 says the outer function still emits sync-point
      "live X-reg maps" — which *is* a framestate, just narrower than
      the JSC kind. Either rename the §6 metadata to something other
      than "framestate" (it currently shares the name with the inlined-
      region full framestate), or acknowledge in §1 that the outer
      function carries lightweight stackmap-style metadata.

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
- [ ] Required: a definition of "sync point" expressed in T2 IR terms,
      not BEAM-op terms. Specifically: which T2 ops are sync points,
      and which are guaranteed to be inserted at *original* BEAM-op
      boundaries vs. injected by lowering.

### B3. Inlining vs hot-code-upgrade race
- [ ] §14.3 "Hot code upgrade" (line 1470–1476) says the watchpoint
      table revokes blobs that inlined the now-old code. But the race
      window — between when a process running inside an inlined region
      starts and when the upgrade lands — needs explicit handling.
- [ ] Suppose process P is mid-execution in T2 blob B that inlined
      `lists:foldl/3`. `code:purge(lists)` fires. Two outcomes:
      (a) P finishes the inlined region — fine, the next call hits
      the revoked address. (b) P is *inside* the inlined region when
      the watchpoint fires.
- [ ] What's the policy for (b)? Possibilities: deopt at the next
      sync point (latency unbounded if we're in a tight loop); patch
      the blob to deopt now (requires concurrent code modification);
      keep the old blob alive until P leaves it (ref-counted; needs
      design). Pick one.

### B4. Reductions through inlined calls
- [ ] §12.4 item 1 (line 1308–1311) says "*each inlined call still
      costs a reduction*" and "we do not amortise reductions across
      inlined boundaries". Good principle. But:
- [ ] How is this enforced mechanically? If `lists:foldl/3` is
      inlined and recovers as a flat loop (§10.5), is each iteration
      still costing a reduction? At T1 it does (each iteration has a
      `call_only` decrement). The straightforward T2 lowering of a
      flat loop *would* skip this, because there's no longer a "call".
- [ ] Either the lowering needs to inject FCALLS decrements at loop
      back-edges or per-K-iterations after unrolling, or this rule
      needs an exception for inlined recursive helpers. Resolve
      explicitly.

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

### B7. Speculation-failure → exit-reason buffer
- [ ] §9.5 (line 962–963) says deopt records "which speculation
      killed it (stored in a per-function exit-reason buffer)". §13.3
      lists "exit count and exit-reason buffer" as per-blob metadata.
- [ ] Format unspecified. Per-speculation-site index? Reason code +
      observed runtime type? Bounded ring buffer or unbounded?
      Required for §9.5's "this time with knowledge of which
      speculation killed it" to be actionable on recompile.

### B8. Profile saturation criterion
- [ ] §15.2 says "≥75% of profile slots have observations and the
      function has been running long enough that its argument-type
      buckets have converged". "Converged" is undefined. JSC uses a
      stable-bucket-for-N-iterations rule; pick a concrete one.

---

## C. Wrong numbers and engineering bugs

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

### C3. "Active execution count" for eviction not specified
- [ ] §13.2 step 1 (line 1419–1420) selects the blob with lowest
      "useful work / bytes" using "recent execution count / blob size".
- [ ] How is "recent execution count" measured? Sampled? Decayed
      counter? Window? An absolute counter biases against
      newly-promoted blobs unfairly. Pick a decay model (JSC uses
      exponential decay with a half-life tied to GC interval).

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

### C6. Deopt stub example understates aarch64 register handling
- [ ] §9.2 (line 890–894) shows the stub as moves into the X-reg
      array. On aarch64 (current target), X registers in BeamAsm
      live in physical registers (x0–x10 at least), with overflow on
      the in-process-X-array. The stub needs to handle both: spilling
      from physical regs that hold *current scratch values* into
      either the physical x0–x10 ABI registers or into the in-process
      array, depending on which X slots are live.
- [ ] Concretely: deopt stub at C must (a) flush untagged scratch
      back to tagged form, (b) ensure ABI-register X regs hold the
      values §6's sync constraint says they should, (c) flush
      overflow Xs (≥ x11 say) into the array. The two-line example
      does only (c).

### C7. Phase effort breakdown skewed
- [ ] Appendix B (line 1917–1925) gives 8 weeks for Phase 0, 4 weeks
      for Phase 1, 6 for Phase 2, **10 for Phase 3**.
- [ ] Phase 0 (plumbing) at 8 weeks for ~3 KLOC C++ + 1 KLOC Erlang
      seems thin for: AOT compiler change for SSA chunk, loader,
      C API, T2 manager process, eligibility checks, profiling
      infrastructure, build-system integration for the dirty
      scheduler. Likely 12+ weeks.
- [ ] Phase 3 (inlining MVP + loop recovery + intrinsics) at 10
      weeks for 4 KLOC C++ + 0.5 KLOC Erlang seems light given that
      the entire deopt machinery for inlined regions is implemented
      here, plus loop recovery, plus annotation-driven intrinsics
      reuse from `sys_core_fold_lists.erl`. Likely 14–16 weeks.
- [ ] Phase 4 (production polish) at 8 weeks for 1.5 KLOC + docs
      seems heavy in lines/weeks but may be light in actual work
      (rollout, regression hunting, incident response don't fit a
      LOC/week framing).

### C8. ~2 ns per profile site claim is optimistic
- [ ] §7.3 (line 666) and §7.5 (line 701) both estimate "~2 ns per
      site" for the type-bitmask and per-call-site profile updates.
- [ ] On Apple Silicon, a load + or + store is ~3 cycles minimum
      (memory store latency + dependency chain), or ~1 ns at 3 GHz.
      But these are emitted in the hot-path inline; cache pressure
      and store-buffer effects on real workloads typically push the
      observed cost to 4–6 ns per site. With 4 profile sites per
      function entry × 10⁹ function calls/s = 40 ns/call extra =
      measurable.
- [ ] Required: actually measure on representative workloads in
      Phase 0; budget the realistic number. The cheap claim makes
      "T1 stays the default; profiling is cheap" look stronger than
      it really is.

### C9. M/(M-U) formula is borrowed without justification
- [ ] §15.1 (line 1492) uses JSC's threshold scaling
      `* (M / (M - U))` where `M` is total cache, `U` is used.
- [ ] JSC's formula is calibrated against its specific workload mix
      (lots of small JS contexts). Erlang has very different blob
      sizes (functions are typically small) and different cache
      pressure (one big code cache for a long-running VM). The
      formula may be wrong for our shape. State why we're keeping it
      or pick a different scaling.

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

### D2. No compile-error path
- [ ] §8 (line 763–767) says compiles have "a hard cap at ~10 ms
      (compile abort + leave function on T1)". §8.3 mentions
      "deliberately don't do" passes. What about *failures*?
- [ ] Possible failures: assertion in T2 IR validation, asmjit
      backend rejects some sequence, watchpoint registration fails
      because the literal table changed mid-compile, OOM in compile
      thread. Each needs a defined fallback (assume "leave on T1"
      everywhere — but say so, and define what's logged).

### D3. JIT server concurrency model
- [ ] §17 "Decisions resolved" (line 1790–1797) decides on "JIT
      server process under `kernel/code` + dirty CPU scheduler".
      That's the queueing layer. But:
- [ ] Multiple compiles in flight on different dirty schedulers —
      can they touch the same shared state (T2 metadata, watchpoint
      table)? The plan doesn't say.
- [ ] Is there a single shared T2 IR allocator/arena, or one per
      compile? Memory accounting?
- [ ] Worker threads writing into the JitAllocator (§13.1) —
      asmjit's JitAllocator isn't documented as thread-safe; do we
      need a mutex, or one allocator per scheduler?

### D4. Multi-scheduler compile thread safety
- [ ] Even with the JIT server serialising requests, the compile
      *output* (the T2 blob) lands in the same memory the schedulers
      read from. The atomic export-table flip (§3) is mentioned but
      not specified for the multi-scheduler case.
- [ ] Required: explicit memory ordering when installing a T2 blob.
      Is `Export.addressv` updated under a barrier? Do callers see a
      consistent view? aarch64's weak ordering means this matters.

### D5. ARM64 memory ordering not addressed
- [ ] On weakly-ordered aarch64, every cross-thread write needs
      explicit synchronisation. The plan mentions atomic export-table
      flips, profile-counter writes, watchpoint registration —
      collectively a lot of inter-thread state.
- [ ] Required: a §X subsection enumerating the cross-thread
      writes and their barrier requirements (acquire/release, full
      `dmb`, etc.). Concrete: profile counter writes — do they need
      release semantics so the T2 manager observes consistent counts?
      My guess is yes. Spell it out.

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

### D8. T1 PC table existence assertion needs verification
- [ ] §9.1 (line 866–871) relies on "the T1 blob's per-instruction
      PC table (which BeamAsm already maintains for line-number
      debugging)". I haven't verified this; it sounds right, but
      Phase 0 should validate that it's complete and stable enough
      to use as a deopt target index.
- [ ] If the table is only maintained for DWARF/line-info and gets
      sparse compilation (some BEAM ops don't emit a PC entry), T2's
      deopt model breaks. Add an explicit Phase 0 audit task.

### D9. Tracing event matrix beyond §12.5
- [ ] §12.5 enumerates trace primitives at a high level. But
      `erlang:trace/3` flags are richer: `arity`, `running_procs`,
      `procs`, `set_on_first_link`, `garbage_collection`, `send`,
      `receive`, `call`, `return_to`, plus all the match-spec
      actions (`message`, `set_seq_token`, `enable_trace`, …).
- [ ] §17 Phase 0 (line 1567–1577) mentions "trace audit" as a
      deliverable. Move (or copy) the full matrix into §12.5 once
      the audit completes — but at minimum the plan should commit to
      producing one in Phase 0 and to using it as an input for v1
      scope. Currently §12.5 has only "examples", and §17 mentions
      it once.

### D10. Compile-pipeline observability format
- [ ] §16 (line 1529–1558) lists ETS counters. What about logs?
      JSC has a JIT logger (`-d` flag) that prints per-compile
      decisions: which functions were compiled, why a speculation
      was inserted, why a recompile happened. Erlang's analogue
      would be `logger:debug` integration or a dedicated trace event.
- [ ] Include in §16: a per-compile event log (configurable,
      default off, costs nothing when off). Same for deopts.

---

## E. Editorial cleanup

### E1. Duplicate `### 6.4` heading
- [ ] See A2.

### E2. §17 "Decisions resolved" mixed with "Still open"
- [ ] See A3.

### E3. §15.4 cross-reference
- [ ] See A4.

### E4. "T1 blob backpointer" misdescribed
- [ ] §13.3 (line 1433) says "Backpointer to the T1 blob (for fast
      revert)". Revert to T1 doesn't actually need a backpointer —
      it just resets `Export.addressv[active_code_ix]` to the T1
      address (always known via the export table). The metadata that
      *is* useful is the T1 PC table reference (used for outer-
      function deopt — §9.1). Rename for accuracy.

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

### E7. Three "v1" definitions
- [ ] §1 defines v1 as "first version covering the goal set". Phase
      breakdown in §17 calls Phases 0–4 "v1 total". Some sub-sections
      refer to "v1 default" (e.g. §6.2 item 4) for the strict-state-
      preservation choice. These are all the same v1, but the reader
      has to triangulate. Make Section 1 say
      "v1 = Phases 0 through 4 (≈40 weeks)" once explicitly.

### E8. Cross-references to non-existent sections
- [ ] §6.5 (line 568) → "Worked example" is currently labelled 6.5,
      but if 6.4 dedups (see A2), all subsequent §6.x references
      shift. Verify all references to §6.5 throughout the document.

---

## F. Strategic concerns

### F1. 40-week / 18-month calendar feasibility
- [ ] Appendix B says 40 weeks single-engineer with "Multiply ~2×
      for total calendar time" — so ~80 weeks calendar = ~18 months
      from start to v1. For a feature this central, that's a long
      hangout in mainline before users see value. JSC's Maglev
      shipped its first version in something like 9 months (V8 had
      more engineers; LinearScan rewrite was deferred — ZJIT's
      pattern).
- [ ] Possible mitigations: parallelise more (e.g. AOT changes in
      Phase 0 can run concurrently with IR-builder work), or descope
      v1 to skip Phase 3.5 (LICM + unrolling) and ship the inlining
      MVP without the loop optimization layer. The latter risks v1
      being underwhelming on the canonical `lists:foldl` case (which
      is the headline win — see F2).

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
