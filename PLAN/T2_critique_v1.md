Read the whole plan. Substantial number of weaknesses — some structural, some sloppy. Sorted by severity.

A. Internal contradictions (the central one)
The strict-vs-relaxed state-preservation model is genuinely inconsistent.

§1 goals (line 54): "outer function preserves T1's abstract machine state at every BEAM instruction boundary".
§3 architecture summary (line 193): "interchangeable with T1 code at every BEAM instruction boundary".
§4 choices table (line 208): "Outer function = identical T1 X/Y layout at instruction boundaries".
§6 then introduces the relaxation: matches "only at sync points".
§6.2 point 4 says "v1 default: keep T1's X/Y assignments everywhere" — i.e., v1 ships the strict model.
§11.2 says the same.
§17 Phase 1 says "output matches T1 at every BEAM instruction boundary" — strict.
So which is it? Three readings are tenable, all undermined by other parts of the document:

"Strict in v1, relaxed later" → §1/§3/§4 are correct; §6 is roadmap. But then §6 reads as if the relaxation is the design.
"Relaxed is the design; strict is a v1 implementation simplification" → §1/§3/§4 are wrong as stated.
"Relaxed at sync points = strict at boundaries because most boundaries are sync points anyway" → maybe true but never argued.
Pick one and rewrite the rest accordingly.

§6.4 has a duplicate heading. "6.4 Why the relaxation matters" and "6.4 Inlined regions" are both numbered 6.4. Numbering broken from there.

§17 Decisions resolved row 1 contradicts the choices table. Says "T2 manager supervision tree. Background thread, not a process under kernel/code (changed from earlier draft — matches HotSpot CompilerThread)". But §3, §4, and §17 Still-open entry all say it is an Erlang process under kernel/code. Stale text from a previous revision.

§17 "Still open" and "Decisions resolved" sections have merged. Most entries under "Still open" begin with "Decided: …". The two-section split is broken.

§15.4 wrong cross-ref. Cites §14.4 for OSR; §14.4 is "tracing as a watchpoint event", not OSR.

B. Architectural under-specification
Multi-frame deopt is hand-waved. §9.1 claims outer-function deopt is "single jcc, no metadata table lookup". §9.2 says inlined-region deopt walks a framestate chain and materialises N CP frames on the Erlang stack. For deeply nested inlining this is non-trivial work — but no bound on nesting depth is set, and the deopt-cost analysis in §9.5 ("100 deopts before jettison") presumes deopts are cheap. With nested inlining 3 levels deep (which the plan permits — §10.3 depth cap = 3), each deopt does ~20–40 instructions of frame reconstruction. That's an order of magnitude more than the "single jcc" mental model the plan promotes.

Sync-point identification is asserted, not specified. §6.3 says "small dataflow analysis: walk the IR, mark every op kind from the list in §6.1 as a sync point". But several BEAM op kinds in real code conditionally yield/GC/raise depending on values — binary:match, NIFs that may trap, BIFs whose trapping is data-dependent. Are these always sync points? The list in §6.1 is "function call sites", which papers over the question.

Inlining vs hot code upgrade race. §14.2 says "after a new module is installed but before exposed: walk the watchpoint table". But:

An in-flight T2 compile started before the reload, that consumes the old SSA, will install a stale T2 blob after invalidation has run. Need a generation counter on the watchpoint trigger (or re-check before installing).
A process executing inside an inlined-but-now-stale region after reload: when does the deopt actually fire? The plan says trace-enable signals each scheduler; the same machinery has to apply to module reload mid-execution. Not stated.
If the new module's exported function is inlined in some T2 blob, and that blob is jettisoned, the still-running process at that moment goes back to T1 — but T1's Export.addressv has already been swung to the new code. The semantics here need the same care code:purge/1 already takes.
Reduction accounting through inlined calls is stated as a constraint, not specified mechanically. §12.4 says "each inlined call still costs a reduction" and §5.2 has reduction_check cost as an IR op, but we never say where the inliner inserts these. If the inlined callee's prologue had i_test_yield, do we replicate that? If we replicate it, every inlined call site in a hot loop has its own yield site; if we collapse them, we change observable scheduling.

Watchpoint granularity per-function is harder than implied. §14.1 says "per-function: a reload of lists invalidates all T2 blobs that inlined any lists function". But the table is per-blob → set-of-watched-things; the inverse query (given a reloaded function, find all watching blobs) needs an index on the table. For a system with thousands of T2 blobs each watching a few functions, the index isn't free.

No quiescence / thread-progress story. §13.2 says "Quiescence wait, then free". Existing code-purge uses erts_thr_progress_* infrastructure. T2 jettison would need to integrate with the same — and §14.2/§13.2 don't say how.

C. Wrong numbers and engineering bugs
SmallInt range bounds in §9.4 are wrong. Plan uses ±2^60. Erlang's 64-bit SmallInt is 60-bit signed (4 tag bits). For a + b to provably fit in a SmallInt without overflow, both operands must be in ±2^59, so the sum fits in ±2^60. The example would emit speculation that allows operands in ±2^60, which can overflow into bignum range. Off by 2×.

The 24-IR-op size threshold for inlining (§10.3) is borrowed from the AOT inliner's Core Erlang. Core Erlang nodes are coarser than T2 IR ops — a single Core Erlang call expands to several T2 IR ops (param loads, stack adjustment, the call itself, return value placement, possibly a framestate). The 24 cap will be hit much earlier in T2 than in Core. Probably needs to be 50–80 in T2 IR.

T2 active execution count for eviction (§13.2) isn't actually specified to exist. The "useful work / bytes" score uses "recent execution count" — but the call counter we do specify is the tier-up trigger, which gets reset to a sentinel after T2 install. So the eviction policy reads stale data. Need a separate per-T2-blob execution counter.

binary_part/2,3 is double-classified. §10.7 lists it as a guard BIF (Phase A primitive op). Phase D (line 1683) covers binary. Either it's Phase A or Phase D — pick one.

length/1 open-coding has unstated lowering constraint. §10.7 says it gets "an inline fast-path with a slow-path tail call to the existing BIF". §12.4 says don't inline length/1 so its trap-out path disappears. These are consistent in principle but the IR-level invariant ("the fast and slow paths are inseparable; neither pass may eliminate the slow tail") is never stated.

The deopt stub example in §9.2 only writes to the X-reg array. On aarch64, XREG0..XREG3 live in CPU registers (x25-x28). Real stub has to mix mov x25, scratch_reg with mov [x_reg_array+offset], scratch_reg. The example understates the work.

§17 effort breakdown. Phase 0 (8w) does the IR builder; Phase 1 (4w) just integrates it. Phase 1 looks too small relative to Phase 0's residual work. Conversely, Phase 3 (10w, with inlining + framestates + linear-scan + AOT change + loop recovery + LoopInfo + multi-frame deopt) packs 7+ items into one phase. Probably underestimated.

Profile-update cost (~2 ns) is optimistic. The aarch64 sequence in §7.3 is six instructions including two memory accesses; under cache miss, a single profile point can hit ~50ns. On a hot inner loop with cross-scheduler contention on the shared profile cache line, can be much worse. No analysis of cache-line contention across schedulers.

Profile saturation thresholds aren't given. §15.2 says "≥75% of slots have observations" and "argument-type buckets have converged" — but never specifies a minimum count to consider a slot monomorphic. With Uint16 count, a slot with count=1 and one type is "monomorphic" by the bitmask test but uselessly thin.

JSC's M / (M - U) threshold formula is borrowed without justification for our setup. JS heap dynamics are very different from a JIT code cache we control entirely. Whether this formula is the right shape is asserted, not argued.

D. Missing content
Test strategy is absent. What's the v1 plan? Differential testing (T1 result == T2 result for same input)? Fuzzing? OTP test suite under T2? Stress testing forced deopt at every sync point? Phase 4 says "integration testing" but nothing on the test architecture itself.

Compile-error path. If the optimizer crashes mid-compile (assertion fail, OOM, encounters a Phase A op that's actually unsupported, hits the 10ms cap), what happens? "Leave function on T1" is implicit. Does the function get permanently blacklisted? Is the failure logged in t2_stats? Phase 4 mentions observability but not error semantics.

Concurrency model for the JIT server. §3 says "JIT server (Erlang process under kernel/code)". An Erlang process is single-threaded by construction. Multiple compile requests come in concurrently — does the server serialise them all through itself? That'd be a bottleneck. Or does it dispatch directly to dirty schedulers and only co-ordinate completion? Not stated.

Multi-scheduler dirty-CPU compile. §17 says compiles run on "a dirty CPU scheduler" (singular?). Erlang has multiple dirty CPU schedulers; if compiles can run in parallel on different ones, the T2 code cache allocator needs to be thread-safe (§13.1 says "separate JitAllocator instance" but doesn't say thread-safe). Per-scheduler arenas would be the standard fix; not discussed.

ARM64 memory ordering. Across schedulers running on different cores, profile updates need at least some ordering. Acquire-load when the JIT server reads the slot; release-store at the update site. Plan says "racy is tolerable" without distinguishing semantically-tolerable races (lost OR-bits) from observation races (compiler reads stale slot at compile time, makes wrong inlining decision).

Coverage methodology. "60-70%, 85-90%, 90-95%, 98%" appear without sourcing. Are these from analysing OTP's stdlib? RabbitMQ? Made up? With these numbers driving phase prioritisation, they should at least be marked "tentative, to be measured in Phase 0".

Branchy allocation under loop unrolling. §10.6's test_heap coalescing is described for the simple case where every iteration allocates a fixed amount. With conditional allocations (lists:filter keeps some elements), coalescing over-allocates — correct but wasteful. For tight memory situations, this could trigger more frequent GC or evict from the heap unnecessarily. No analysis.

T1 PC table existence. §9.1 asserts BeamAsm maintains "a per-instruction PC table" that T2 reads to resolve cold-tail addresses. The research/beamasm.md doc mentions line-info but not BEAM-instruction-index ↔ T1 PC. Phase 0 should verify this exists; the plan asserts it as a fact.

erlang:trace interactions beyond §12.5. What about erlang:system_monitor (long_gc, large_heap)? erlang:trace_pattern(_, true, [global]) enabling tracing on every export of a module? The Phase 0 audit is mentioned but the plan never enumerates the events.

Compile-pipeline observability. +JT2trace_compiles exists but format is unspecified. For debugging a regression we'd want per-compile records ("function X tier-up at T=10s, compile 1.2ms, IR size 47, blob size 4KB, jettisoned at T=20s due to RangeFail at SSA point #12").

E. Editorial cleanup
In addition to A's contradictions:

§6.4 duplicate heading.
§17 stale text in Decisions Resolved row 1.
§17 Still-Open / Resolved sections merged.
§15.4 wrong cross-ref.
§10.4 "should be derived from sys_core_fold_lists.erl directly — either ported to T2 IR construction at JIT-init time, or generated through the core_to_ssa pipeline once and cached" — these are very different implementations, not interchangeable. The "should" hides real implementation choice.
§9.6 talks about the safe constant-fun case; §10.2 says "Speculative fun specialisation (§9.6)" which is the unsafe case that §9.6 mentions only briefly. Confusing cross-reference.
§13.3 says "Backpointer to the T1 blob (for fast revert)" — but T1 blobs aren't separately allocated for tier-2-eligible functions; they're part of the module's BeamAsm code. "Revert" is just an Export.addressv store. The "backpointer" is implicit, not a new field.
F. Strategic concerns
40 weeks for v1 (§17), 18 months calendar. That's a long road for an opt-in feature. The plan says "each phase ships something measurable" but doesn't say which phases land in master vs which stay on a branch. Without intermediate merges, an 18-month branch is its own project-management hazard — especially when the BEAM SSA emission in the AOT compiler (Phase 0) needs to land in master well before T2 v1 ships, or every master-merge is a giant integration.

The "T2 wins by inlining; without inlining T2 is modest" thesis (§2) is the backbone but isn't sized. No estimate of "what fraction of OTP / RabbitMQ runtime is in functions where inlining unlocks meaningful wins". If it's <20%, a 16-KLOC + 18-month investment for a single-digit-percent ratio_in_t2 win is a hard sell. The "Open question" labelled "Coverage methodology" should really be the prerequisite for committing to the plan.

No comparison with simpler alternatives. Given the choice between (a) this T2 plan, (b) just merging more of inline_list_funcs into AOT default, (c) rich type info from AOT into BeamAsm so it specialises arithmetic without a new tier — the plan never compares ROI. (b) and (c) are smaller projects with possibly comparable wins on the workloads where T2 helps most.

If you want, I can walk through these in priority order and propose targeted fixes to land in the plan, or you can pick the ones that bother you most and we focus there.
