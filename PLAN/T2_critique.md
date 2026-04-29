# T2 Plan — Critique (second-pass)

A second-pass review of `PLAN/T2.md` after the first round of resolutions
landed. The first round (sections A–F, 45 items) is in git history at
commits `ed325311f6` (initial), `7808eee6bb` (sharper points),
`b8087cc60c` (close-out), and the corresponding T2.md commits
`a06ac5e381` (A-series), `66174466c2` (B-series), `43556cb9d6`
(C-series), `29c6cb5916` (D-series), `473c8e7e99` (E-series),
`d10f1a55f3` (F-series).

This pass found 51 new items, organised into:

- **G. New contradictions introduced by recent edits.** Stale text the
  resolutions left behind.
- **H. Architectural under-specification.** Decisions the resolutions
  exposed but didn't pin down.
- **I. Wrong numbers / engineering bugs.** Specific values that look
  off.
- **J. Missing content.** Topics not yet addressed.
- **K. Editorial cleanup.** Small fixes; many cross-reference G/H/I.
- **L. Strategic concerns.** Higher-level questions raised by the
  current state.

Each item has a **Suggested resolution** that's a starting point — edit
or replace if you disagree.

---

## G. New contradictions introduced by recent edits

### G1. Phase 3 effort: §17 says 10 weeks, Appendix B says 14–16
- [ ] §17 line 2128: "Phase 3 — Inlining MVP + loop recovery +
      intrinsics (≈10 weeks)". Appendix B was updated to
      14–16 weeks (C7 resolution); §17's title wasn't.

**Suggested resolution.** Update §17 Phase 3 title to
"(≈14–16 weeks)" matching Appendix B.

### G2. Phase 0–2 calendar arithmetic stale
- [ ] §17 "Sequencing rationale" (line 2218): "Phases 0–2
      (~18 weeks) validate or invalidate the entire approach".
      With Phase 0 grown to 12w + Phase 1 4w + Phase 2 6w, the
      total is **22 weeks**.

**Suggested resolution.** Change "(~18 weeks)" to "(~22 weeks)" in
the sequencing rationale.

### G3. Phase 3 task list still lists "Multi-frame deopt dispatch"
      as the hard part
- [ ] §17 Phase 3 (line 2141): "Multi-frame deopt dispatch (the
      hard part)". With B1's eager-CP-push resolution, deopt is
      uniform regardless of nesting depth — there *is* no
      "multi-frame deopt dispatch" task anymore.

**Suggested resolution.** Replace the bullet with "Inlined-region
deopt-stub emission (uniform shape per §9.2's eager-CP-push)".

### G4. §18 Risk #1 fallback (a) names a model we rejected
- [ ] §18 risk #1 (line 2229): "(a) the strictest 'identical at
      every BEAM instruction boundary' rule from earlier drafts —
      correct but constrains the allocator unnecessarily". A1
      resolved the relaxed sync-point model as **the design**.
- [ ] Same risk's mitigation: "Phase 1's identity transform ships
      option (a) by default; relaxation to the sync-point model
      happens when measurements show it's worth the engineering
      cost." Phase 1 was rewritten to ship the relaxed model
      directly. Mitigation text is stale.

**Suggested resolution.** Rewrite risk #1:

> **State-preservation model is impractical.** If sync-point
> identification (§6.3) or the constrained allocator (§11.2) turn
> out to add more compile-time complexity than budgeted, or if the
> sync-constraint conflict frequency (§H3) exceeds expectations,
> the v1 wins evaporate.
> **Mitigation**: Phase 1's identity transform validates the model
> end-to-end on the OTP test suite before any optimisation pass
> lands. If complexity dominates, fall back to a stricter model
> (T1 X/Y layout at every BEAM instruction boundary) that's
> simpler but loses some allocator freedom.

### G5. §18 Risk #2 says "measure in Phase 1"
- [ ] §18 risk #2 mitigation: "measure in Phase 1 and tune". Phase 1
      runs no optimization passes (identity transform), so its
      compile time isn't representative.

**Suggested resolution.** Change "measure in Phase 1 and tune" to
"measure in Phases 2 and 3 (when speculation, then inlining,
land)".

### G6. §3 architecture diagram: stale "BeamAsm blob retained for
      jettison fallback"
- [ ] §3 line 244: "BeamAsm blob retained for jettison fallback".
      Per E4 resolution, T1 blobs aren't separately allocated —
      they're part of the module's BeamAsm code.

**Suggested resolution.** Replace the diagram bullet with
"`Export.addressv` flips between T1 and T2 entries (T1 code lives
in the module's BeamAsm allocation; no separate retention)".

### G7. §3 architecture diagram: "inlined regions register
      framestates" misleading
- [ ] §3 line 231: "inlined regions register framestates". Per
      B1, framestates are codegen-only metadata.

**Suggested resolution.** Replace the bullet with "inlined regions
emit per-region deopt stubs from codegen-time framestate metadata
(eager CP-push at region entry; uniform deopt stub regardless of
nesting depth — see §9.2)".

### G8. §6.5 still describes lazy CP materialisation
- [ ] §6.5 line 644: "Nested inlining: each inlined region adds
      one more frame to the framestate chain. Deopt unwinds
      through `parent_fs` references to **materialise N CP frames
      on the Erlang stack**".

**Suggested resolution.** Rewrite §6.5's nested-inlining paragraph:

> Nested inlining: each inlined region pushes its parent CP at
> region entry (eager-CP — §9.2). Codegen-time framestate metadata
> chains via `parent_fs` to record the live-X-reg map at each
> outer level, but no runtime CP materialisation is needed — the
> CP frames are already on the Erlang stack from the eager pushes.
> Deopt restores X/Y state and branches to the outermost call's
> T1 PC.

### G9. §6.6 worked example: `framestate` shown as IR op
- [ ] §6.6 example shows `framestate [%list→x0], ip: …` as an IR
      op in the function body. Framestates are codegen-only.
- [ ] Example also uses `%result = lists_reverse(%acc)` — not a
      real IR op.

**Suggested resolution.** Two edits:

1. Replace the literal `framestate` line with a comment:
   ```
   // codegen-only framestate at entry: [%list→x0]
   // (consumed when emitting the deopt stub for the region;
   // no IR op generated here)
   ```
2. Replace `%result = lists_reverse(%acc)` with
   `%result = call_ext lists:reverse/1, [%acc]` (or mark the
   block `// sketch — actual lowering would handle reverse/1
   per §10.7 manifest`).

### G10. §9.3 says deopt is "only legal at BEAM instruction
       boundaries" — contradicts §6
- [ ] §9.3 line 1046: "Deopt is only legal at points where a
      valid BEAM-machine state can be reconstructed — which means
      at **BEAM instruction boundaries** in the original code."
      §6 (per A1) restricts this further to **sync points only**.

**Suggested resolution.** Rewrite §9.3 first paragraph:

> Deopt is only legal at *sync points* (§6.1) — function entry,
> call sites, returns, GC sites, BIF boundaries, speculation
> guards, tracing-relevant points, and receive safe points. These
> are the points where a valid BEAM-machine state can be
> reconstructed (the live-X-reg map is recorded). Mid-arithmetic
> (between `untag_int` and `add_small`) deopt is impossible because
> registers contain raw machine values that don't correspond to any
> valid BEAM state — but mid-arithmetic also can't be a sync point
> by construction, so the constraint composes naturally.

### G11. §15.4 still talks about "deopt via framestate"
- [ ] §15.4 line 1888: "In inlined regions, deopt via framestate
      (§9.2)." With B1, framestate is consumed at codegen; the
      runtime mechanism is the deopt stub.

**Suggested resolution.** Reword: "In inlined regions, deopt via
the per-region deopt stub emitted from codegen-time framestate
metadata (§9.2)".

---

## H. Architectural under-specification

### H1. Tagged values across GC inside inlined regions
- [ ] §12.3 says "T2 v1 keeps untagged values out of the X
      register array". But what about **tagged** SSA values inside
      an inlined region that are live across a GC point (e.g. an
      accumulator threaded through an inlined `lists:map` body
      that allocates)? They have to be reachable from a GC root.
- [ ] The plan's flush rule ("inliner emits flush sequences at
      each potential GC site") doesn't say *where* the flush goes.

**Suggested resolution.** Add to §12.3:

> **Tagged values across GC inside inlined regions.** Live tagged
> SSA values inside an inlined region that span a GC site are
> spilled to fresh **scratch Y slots** reserved at codegen for the
> region's lifetime. The reserved Y slots are allocated above the
> outer function's BEAM-mandated Y slots and are part of the
> inlined region's frame layout. The standard GC walker picks
> them up via the existing Y-register scan (`c_p->stop` to
> `c_p->stack_end`); the GC sees them as ordinary tagged terms.
> The inliner's lowering pass annotates each potential-GC op with
> the spill list and emits the spill before / reload after the
> GC-capable site.

### H2. Inlined-region register pressure on T1-pinned regs
- [ ] §12.1: x25–x28 hold XREG0..XREG3, x15–x17 hold XREG4..5.
      Inside an inlined region, can the allocator use them as
      scratch? Plan doesn't say.

**Suggested resolution.** Add to §11.3:

> **Use of T1-pinned X-regs inside inlined regions.** The
> allocator may freely use x25–x28 / x15–x17 as scratch *inside*
> an inlined region. The outer X0..X5 values they held at the
> region's entry sync point are spilled to scratch Y slots
> alongside H1's mechanism, then restored at the next sync point
> (typically the region exit). This trades a couple of stores
> for full architectural-register-file access inside the region —
> worthwhile for any non-trivial inlined body.

### H3. Sync-constraint conflict: same SSA value, different X
       regs at different sync points
- [ ] §11.2 says values pinned by an active sync constraint
      cannot be displaced; values that need to live across a sync
      point either sit in the X reg or get spilled.
- [ ] Conflict case: V live across S1 (T1-mandated x0) and S2
      (T1-mandated x2). V can't be in two places.

**Suggested resolution.** Add to §11.2:

> **Sync-constraint conflict policy.** When SSA value V is live
> across two sync points S1 and S2 with different T1-mandated
> X-reg constraints, the allocator emits a move (or
> spill+reload) between S1 and S2:
> 1. If V is in the constraint-pinned X-reg at S1, the allocator
>    emits a move to the new constraint-pinned X-reg before S2.
> 2. If a move conflicts with another live value, the allocator
>    spills V to the scratch Y area between S1 and S2 and
>    reloads at S2.
>
> In practice T1's per-BEAM-op X-reg layout is sticky (the same
> SSA value tends to occupy the same X-reg across consecutive
> BEAM ops), so conflicts are rare. Phase 1's identity-transform
> suite measures the conflict frequency on real workloads; if
> conflicts dominate, fall back to keeping V in its T1 reg
> throughout (losing some allocator freedom but always correct).

### H4. Active execution counter atomicity
- [ ] §13.3: "active execution counter (incremented in the T2
      prologue ...)". Multiple schedulers can call concurrently.

**Suggested resolution.** Add to §13.3:

> The active execution counter is **per-scheduler-sharded**
> (matching the C8 profile counters). Each call increments only
> the local scheduler's shard via a single non-atomic store. The
> eviction code (§13.2) reads the union of shards. Per-shard cost
> is one cache-line-resident store per call; no contention.

### H5. §15.3 queue-drop policy: counter reset on drop
- [ ] §15.3: "If the queue exceeds a high-water mark, further
      requests are dropped". §7.4 says "counter resets to a
      'pending compile' sentinel". On drop the counter is at
      sentinel; it never retrips.

**Suggested resolution.** Add to §15.3:

> On queue drop, the function's call counter is reset to
> `threshold − probe_value` (default `probe_value = threshold/4`)
> rather than left at the sentinel. The function will retrip
> after `probe_value` more calls — small enough that we don't
> lose tracking, large enough that we don't immediately
> re-enqueue while the queue is still draining.

### H6. Stable speculation-site IDs across recompiles
- [ ] §9.5 / B7's per-blob exit-reason buffer is indexed by
      speculation-site ID. On recompile, the IR may differ;
      sequential indices don't correspond.

**Suggested resolution.** Add to §9.5:

> **Stable site identification.** Speculation-site IDs are not
> sequential indices but a hash of
> `{source_BEAM_PC, speculation_kind, narrowed_type}`. The hash
> is stable across recompiles as long as the underlying BEAM SSA
> hasn't changed. On module reload the hashes reset (consistent
> with the existing "profile data resets on module reload"
> policy, §17 Decisions Resolved). The exit-reason buffer is keyed
> by hash, not index.

### H7. Profile-feedback conflict resolution
- [ ] §5.4 lists three sources of types in priority order but
      doesn't define conflict semantics.

**Suggested resolution.** Add to §5.4 after the three sources:

> **Conflict resolution: AOT is ground truth.** Profile
> observations are filtered against the AOT type — observations
> outside the AOT-proven type are *dropped* (treated as transient
> anomalies, e.g. a memory corruption or a window before
> tier-2-eligibility was checked). Profile *narrows* AOT but
> never *contradicts* it. If the profile sees only `[1, 2, 3]`
> when AOT proved `integer`, profile narrows to a small set;
> if profile sees `atom` when AOT proved `integer`, profile is
> ignored. Forward dataflow inside T2 then refines further but
> only within the AOT-proven type lattice.

### H8. Speculation-range auto-selection
- [ ] §9.4 picks ±2^58 as an example but doesn't say how the
      optimizer chooses ranges automatically.

**Suggested resolution.** Add to §9.4:

> **Range-selection algorithm.** The optimizer chooses the
> speculation range as
> `clamp(observed_range × headroom_factor, ±2^58)`.
> - `observed_range` comes from the profile feedback (the
>   `[min, max]` of integer slot observations, or the union of
>   small-int values in the bitmask).
> - `headroom_factor` = 1.5 (default) — we speculate to 1.5× the
>   observed range so we don't deopt at the edge of the observed
>   distribution.
> - The cap at ±2^58 leaves room for one addition without
>   overflow check (see §9.4 paragraph above).
>
> If the observed range exceeds ±2^58, no `speculate_range` is
> emitted; the operation falls back to generic arithmetic with
> overflow checking. Phase 0 calibration may tune
> `headroom_factor`.

### H9. Audit list for "audited-pure-or-semi-pure" callees
- [ ] §10.1: "monomorphic + size ≤ threshold + audited-pure-or-
      semi-pure ⇒ inline". Manifest unspecified.

**Suggested resolution.** Define the manifest. Add to §10.1:

> **Pure-or-semi-pure inlining manifest** (v1):
> - The `sys_core_fold_lists.erl` set: `lists:all/2`, `any/2`,
>   `foreach/2`, `map/2`, `flatmap/2`, `filter/2`, `foldl/3`,
>   `foldr/3`, `mapfoldl/3`, `mapfoldr/3`.
> - Curated additions: `lists:reverse/1`, `lists:append/1`,
>   `lists:append/2`, `maps:get/2`, `maps:get/3`, `maps:put/3`,
>   `maps:remove/2`, `maps:size/1`, `maps:keys/1`, `maps:values/1`,
>   `tuple_to_list/1`, `list_to_tuple/1`.
> - All §10.7 guard BIFs (already primitive ops).
>
> User modules opt in via the `-jit_inline` attribute or per-
> function annotation (§10.4). Modules without explicit opt-in
> are not inlined.

### H10. Per-callee deopt-skip rule too coarse
- [ ] §10.3: "Skip inlining if the callee deopt'd in a previous
      T2 compile" — too coarse; should be per call site.

**Suggested resolution.** Replace the bullet in §10.3:

> - **Skip inlining at a *specific call site*** if a prior
>   compile saw a deopt traced to inlining the callee at that
>   site. The skip is keyed by `{caller_BEAM_PC, callee_MFA}`
>   and recorded in the exit-reason buffer (§9.5) using the
>   stable hash from H6. Other call sites of the same callee
>   are still eligible.

### H11. `lists:foldl(MyFun, ...)` where MyFun is non-literal
- [ ] §10.5 loop recovery requires a constant-known fun arg.
      Real user code commonly passes `MyFun` from elsewhere.
      What does v1 do?

**Suggested resolution.** Add to §10.2 (what we don't inline in v1):

> - **Higher-order helpers called with a non-literal fun.**
>   `lists:foldl(MyFun, 0, L)` where `MyFun` is a parameter, a
>   function-call result, a record field, etc. v1 falls back to
>   a regular `call_ext` to the helper — the helper is not
>   inlined because the constant-fun precondition (§10.1) fails.
>   This means user code patterns like
>   `fun_handler(Items, F) -> lists:map(F, Items)` get no T2
>   inlining win in v1; the win arrives in v2 with speculative-
>   fun inlining (§9.6, Phase 6).
>
> Phase 0's inlining-thesis sizing measurement (§17 audits)
> quantifies how much of the corpus this affects.

---

## I. Wrong numbers / engineering bugs

### I1. Phase 1 emitter LOC short of §11.1's estimate
- [ ] Appendix B Phase 1 = ~2 KLOC C++. §11.1 line 1397 estimates
      ~2.5 KLOC just for emitters; plus IR builder, sync-point
      pass, jettison plumbing. Likely 3–4 KLOC total.

**Suggested resolution.** Two options:

1. **Re-scope the boundary.** Move the architecture-agnostic
   emit framework + IR builder (~1 KLOC) into Phase 0; Phase 1
   then ships only the architecture-specific emitters (~1.5 KLOC
   ARM64) plus the sync-point pass and jettison plumbing (~0.5
   KLOC). Phase 1 stays at 4 weeks.
2. **Accept the underestimate.** Phase 1 grows to ~3 KLOC and
   5 weeks; Appendix B updated accordingly.

Recommended: option 1 — Phase 0 is already gated on audits
completing; bundling the emit framework with the audits is
natural.

### I2. `length/1` fast-path threshold not specified
- [ ] §10.7 says inline fast-path with slow-path tail call to the
      BIF. BIF traps after ~4000 cells; inlining 4000 cells per
      site is massive.

**Suggested resolution.** Add to §10.7:

> The `length/1` inline fast-path traverses up to **16 cons
> cells**. If the list is longer (cell #17 reached without
> hitting nil), the inline path tail-calls into the existing
> BIF, which handles trap-out for long lists. The 16-cell bound
> keeps the inline code under ~80 instructions per site; lists
> length 1–16 pay no BIF call; lists length 17+ pay one tail
> call into the existing BIF. Phase 0 may revisit the bound if
> measurements show a different sweet spot.

### I3. Profile sharding not reflected in §7.2 struct
- [ ] §7.2 struct shows a single `slots[]` array; C8 introduced
      per-scheduler sharding.

**Suggested resolution.** Update §7.2:

```c
typedef struct {
    Uint16 seen_types;   // bitmask of BeamTypeId flags observed
    Uint16 count;        // saturating count, caps at 65535
} T2ProfileSlot;

typedef struct {
    Uint32         call_count;       // global; only sched-1 increments
    Uint16         num_slots;
    Uint16         num_schedulers;   // = erts_no_schedulers
    // Layout: slots[scheduler_id * num_slots + slot_idx],
    // cache-line-aligned per scheduler to avoid false sharing.
    T2ProfileSlot  slots[/* num_schedulers * num_slots */];
} T2FunctionProfile;
```

### I4. §7.5 "CAS to POLY" mislabelled
- [ ] §7.5 line 794: load + cmp + conditional-store called "CAS
      to POLY" — not actually a CAS. Confusing wording.

**Suggested resolution.** Replace "CAS to POLY" with:

> single-writer non-atomic load-compare-store (only scheduler 1
> writes per C8; no inter-scheduler atomicity needed).

### I5. §15.2 saturation budget too short
- [ ] N=64 ticks × 5 retries = 320 ticks total before giving up.
      JSC uses much larger windows.

**Suggested resolution.** Bump to `N = 256, max_retries = 8`
(2048 ticks). Mark as "starting point, calibrated in Phase 0
against the F2 corpus" — same calibration treatment as the
size threshold (§10.3).

### I6. §8.1 pass-list ordering: speculative-arith lowering after
       unrolling
- [ ] Unrolling at step 12 duplicates generic add/sub/mul K
      times; lowering to `add_small`/`mul_raw` happens at step 14.
      Lowering K times costs more than lowering once before
      unrolling.

**Suggested resolution.** Move speculative-arithmetic lowering
from step 14 to step **9.5** (between guard strength reduction
and loop info analysis). Updated order:

```
9. Guard strength reduction
9.5. Speculative arithmetic lowering   (NEW position)
10. Loop info analysis
11. LICM
12. Loop unrolling
13. Allocation sinking
14. (was: spec-arith lowering — moved up)
15. Direct lowering to asmjit
```

This way unrolling duplicates the already-lowered (smaller)
form.

### I7. Allocation-sinking (step 13) before lowering (step 14)
- [ ] Sinking moves allocations across IR; sync-point markers are
      a property the lowering pass marks. If sinking runs before
      lowering, markers aren't authoritative.

**Suggested resolution.** Move sync-point marking earlier in the
pipeline (already proposed in I6 — set during T2 IR construction
in step 1, since §6.3 says sync points are derived from BEAM SSA
ops). Then sinking (now step 13 with I6's reorder, step 12
without) operates on IR with authoritative markers throughout.
Document in §6.3:

> Sync points are marked on T2 IR ops at IR construction time
> (step 1 of §8.1). All subsequent passes treat the markers as
> authoritative and may not move work across a sync-point op
> in a way that violates the sync constraints.

---

## J. Missing content

### J1. No regression detection / CI gating
- [ ] §16A.5 mentions a regression bench suite but doesn't say
      who runs it or when.

**Suggested resolution.** Add to §16A as a new subsection:

> ### 16A.6 CI gate (Phase 4 onwards)
>
> Every PR touching `erts/emulator/beam/jit/t2/`, the AOT-side
> SSA-chunk emission, or the T2 server in
> `lib/kernel/src/code_server.erl` runs the §16A.5 regression
> bench suite via OTP's existing CI infrastructure. Hard-fail
> threshold: T2-enabled vs T2-disabled performance must not
> regress more than 2% on any tracked benchmark (ε margin on
> top of measurement noise).
>
> Gate runs on the same pre-merge CI as existing OTP perf
> tracking; failures block merge until a maintainer overrides
> with a justification.

### J2. No "profile data quality" metric
- [ ] If profile observations get stuck on the wrong type, T2
      speculates wrong and constantly deopts. No metric surfaces
      this distinct from generic deopt rate.

**Suggested resolution.** Add to §16's `erlang:t2_stats/0`:

```erlang
t2_speculations_succeeded => N,
t2_speculations_deopted   => N,
t2_profile_quality        => Float  % succeeded / (succeeded + deopted)
```

Add to §16 narrative: "Sustained `t2_profile_quality` below 0.8
indicates a profile-collection bug or a workload pathology and
should trigger an alert in production observability."

### J3. No "stack-scan latency on schedule-in" risk
- [ ] B6's lazy stack scan does an O(stack-depth) walk on first
      schedule-in after a jettison. Deep stacks → latency spike.

**Suggested resolution.** Add to §18 risks:

> **Stack-scan latency on schedule-in.** The lazy stack scan
> (§14.2) does an O(stack-depth) walk on first schedule-in after
> a jettison. For deep stacks (telecom-style state machines,
> thousands of frames) this is a non-trivial latency spike at
> the schedule-in hot path.
> **Mitigation**: bounded scan via `+JT2lazy_scan_max_depth N`
> (default 1024). If the walk exceeds N frames, the scan
> truncates and instead forces a synchronous deopt of the
> process at its next sync point. Track `max_lazy_scan_depth`
> and `truncated_scan_count` in `erlang:t2_stats/0` for
> production tuning.

### J4. No discussion of dist (cross-arch nodes)
- [ ] Mixed aarch64+x86_64 cluster: SSA chunk wasted on x86; how
      do cross-node code-upgrades interact?

**Suggested resolution.** Add to §19 (out of scope):

> **Distributed Erlang.** T2 reasons only about *local* code
> state. Cross-node calls go through `Export.addressv`
> indirection and are not specialised by T2; remote-node module
> reloads do not affect local T2 blobs (T2 watchpoints track
> local module state only).
>
> The SSA chunk and `jit_inline` annotations are present in
> BEAM files regardless of the architecture loading them; on
> x86_64 nodes (where T2 is not enabled in v1), the chunk is
> wasted disk and load-time bytes. This is acceptable for v1;
> when x86_64 T2 lands (Phase 10), the chunk is paid for on
> both archs.

### J5. No discussion of profile emission overhead on cold code
- [ ] Eligible-but-never-tier-up functions still pay profile
      overhead. With 60% eligible × 5% tier-up rate, 55% of
      eligible functions pay for nothing.

**Suggested resolution.** Add to Phase 0 audits (§17):

> **Profile-overhead-on-cold-code micro-benchmark.** Run the F2
> corpus under three configurations:
> 1. T1 only, no profiling.
> 2. T1 with profile-emission active for tier-2-eligible
>    functions but T2 compilation disabled (so no functions
>    actually tier up).
> 3. T1 + T2 fully enabled.
>
> Quantify the overhead added by configuration (2) vs (1) and
> by configuration (3) vs (1). Target: configuration (2) within
> 1% of (1) on aggregate workload performance. If exceeded,
> revisit the eligibility criterion (§7.1) — perhaps narrow
> profile-emission to functions that already passed a "warm-up"
> threshold rather than every eligible function from module
> load.

### J6. No "T2 disabled globally on first abort" mode
- [ ] §8.4 aborts the BEAM on certain bug conditions —
      catastrophic in production.

**Suggested resolution.** Add to §8.4:

> **Production safe-mode** (`+JT2safe_mode true`, default
> `false`). When enabled, the bug conditions that would normally
> abort the BEAM (T2 IR validation assertion, asmjit backend
> reject, watchpoint-registration race) instead:
> 1. Disable T2 globally for the lifetime of the VM.
> 2. Revert all active T2 blobs to T1 via the existing jettison
>    machinery.
> 3. Log an error via `logger:error/2` with the failing
>    `{Mod,Fun,Arity}` and reason.
> 4. Increment a `t2_safe_mode_aborts` counter in
>    `erlang:t2_stats/0`.
>
> Default-false makes development bugs maximally visible; long-
> running production deployments opt into safe-mode via emulator
> flag or `ERL_FLAGS`.

### J7. Active execution counter decay implementation
- [ ] Decay "tied to the GC interval" — but GCs are per-process,
      not global. Implementation unspecified.

**Suggested resolution.** Add to §13.3:

> **Decay implementation.** A periodic timer in the JIT server
> visits all blob counters every `K` seconds (default `K = 60`)
> and applies `counter := counter / 2` (half-life = K seconds).
> The walk is cheap because the JIT server already keeps the
> blob list as a flat structure; the timer is `erlang:send_after/2`
> and runs on the JIT-server process, not blocking schedulers.
> Tunable via `+JT2decay_interval`.

### J8. No discussion of `apply/3` and dynamic dispatch
- [ ] `apply(M, F, Args)` with monomorphic-by-observation target
      isn't covered.

**Suggested resolution.** Add to §10.2 (what we don't inline in
v1):

> - **`apply/3` with a monomorphic-by-observation target.** Even
>   when the per-call-site slot (§7.5) has converged on a single
>   `{M,F,A}`, T2 in v1 does not inline based on this — it would
>   require speculative-target inlining and the same deopt
>   machinery as speculative-fun inlining (§9.6). The slot is
>   collected in v1 (drives general profile metrics) but
>   consumed by Phase 6's polymorphic + speculative-fun work.
>   Static `apply(M, F, Args)` where `M` and `F` are
>   provably-constant atoms is treated as `call_ext` and follows
>   the §10.1 inlining rules normally.

---

## K. Editorial cleanup

### K1. §3 diagram references "framestates"
- [ ] See G7.

**Suggested resolution.** Apply G7's resolution.

### K2. §6.5 lazy-CP wording
- [ ] See G8.

**Suggested resolution.** Apply G8's resolution.

### K3. §6.6 example uses non-existent `lists_reverse` op
- [ ] See G9.

**Suggested resolution.** Apply G9's resolution.

### K4. §9.3 BEAM-instruction-boundary wording
- [ ] See G10.

**Suggested resolution.** Apply G10's resolution.

### K5. §10.4 references "the AOT compiler today has
       sys_core_fold_lists.erl (~400 lines)"
- [ ] The "(~400 lines)" parenthetical rots over time.

**Suggested resolution.** Drop the parenthetical. The reference
to the file is enough; line counts in plans rot fast.

### K6. §15.4 "deopt via framestate"
- [ ] See G11.

**Suggested resolution.** Apply G11's resolution.

### K7. §17 Phase 4 doesn't mention M/(M-U) re-tuning
- [ ] §15.1 says "Phase 4 includes a measurement task to
      re-evaluate the formula", but Phase 4's task list omits it.

**Suggested resolution.** Add to Phase 4 in §17:

> - **Tier-up threshold re-tuning.** Re-evaluate the `M/(M-U)`
>   formula in §15.1 against the regression bench suite; adjust
>   the formula or constants if the JSC borrowing produces poor
>   blob-eviction behaviour for Erlang's blob-size profile.

### K8. §19 overbroad blanket optimization claim
- [ ] §10.6's `test_heap` re-coalescing technically *is* a "GC
      optimization". The blanket rejection is wrong.

**Suggested resolution.** Reword the §19 entry:

> **Optimizations targeting the implementation of message
> passing, ETS, GC, or NIFs.** Those wins live in the runtime
> implementations of those subsystems, not in the JIT. T2 may
> re-batch, hoist, or eliminate the *triggers* for these
> subsystems where the SSA structure permits (e.g. coalesce
> `test_heap` calls under loop unrolling, §10.6; eliminate
> redundant guard BIF calls via CSE, §10.7). The runtime
> services themselves are out of scope.

### K9. Appendix A: `instr_t2.cpp` location vs §11.1 layout
- [ ] Appendix A places ARM64 emitters at `arm/instr_t2.cpp`
      but §11.1 is silent on location.

**Suggested resolution.** Update Appendix A to make the split
explicit:

```
erts/emulator/beam/jit/
├── arm/
│   ├── instr_t2.cpp          (NEW) ARM64-specific T2 emitters
│   └── beam_asm_global.cpp   (extend) add t2_deopt_dispatch
├── t2/                       (NEW) architecture-agnostic T2 compiler
│   ├── t2_codegen.cpp        IR → asmjit dispatch (calls into arm/)
│   └── ... (other files unchanged) ...
```

Add to §11.1 a paragraph noting: "Architecture-specific
emitters live in `arm/instr_t2.cpp` (and later `x86_64/`);
architecture-agnostic IR-to-asmjit dispatch lives in
`t2/t2_codegen.cpp`."

---

## L. Strategic concerns

### L1. Cumulative steady-state tax on T1 baseline
- [ ] Profile emission, per-call-site slots, prologue, call
      counter — together a 5–10% per-call tax on T1 code, even
      when nothing has tier-upped.

**Suggested resolution.** Update §1's hard floor and add a Phase
0 measurement:

§1 hard floor — add qualifier:

> **Hard floor: T2 must not ship slower than T1, including the
> aggregate steady-state tax from profiling and tier-up
> infrastructure.** Aggregate workload performance under T2-
> enabled mode must not regress more than **3%** vs T1-only
> mode on any tracked benchmark — this is the budget for the
> profiling, eligibility-check, and counter overhead taken
> together. Phase 0 measurement (§17 audits) sets the
> production target.

Phase 0 audits — add bullet:

> - **Aggregate-tax measurement.** Quantify the steady-state
>   tax on T1 baseline performance from profile-emission and
>   call-counter overhead (J5 measurement) plus the patchable-
>   prologue cost. Target: ≤ 3% regression vs T1-only baseline
>   on the F2 corpus.

### L2. Lazy stack-scan is incompatible with hard real-time SLAs
- [ ] B6's lazy scan plus the high-water sweep can spike
      schedule-in latency unpredictably.

**Suggested resolution.** Two-part resolution:

1. **Acknowledge in §18 risks** (extend J3's resolution):

   > Erlang systems with hard latency requirements (telecom
   > switches, financial systems, hard-RT control planes) may
   > not tolerate the lazy-scan latency spikes even with
   > bounded scan depth. v1 ships the bounded-scan mitigation
   > (J3) and the metric (`max_lazy_scan_depth`); operators
   > with strict SLAs must measure and decide whether T2 is
   > viable for their workload.

2. **Add a per-process opt-out** (roadmap, not v1):

   > **Roadmap (post-v1):** add a process flag
   > `process_flag(t2_jit, false)` that prevents the process
   > from ever entering T2 code (and thus from ever needing a
   > stack scan). Latency-sensitive processes can opt out at
   > spawn time. Out of scope for v1; revisited if production
   > deployments report SLA violations.

### L3. T2 carries an irreducible stake in BeamAsm internals
- [ ] T2 depends on the T1 PC table, calling convention, global
      runtime fragments, patchable prologue. No documented
      contract makes these invariants visible to BeamAsm
      maintainers.

**Suggested resolution.** Add a new section between §11 and §12,
or integrate into Appendix A:

> ## 11A. T2 ⇄ BeamAsm contract
>
> T2 is built on top of BeamAsm and depends on a small set of
> BeamAsm-internal invariants. Changes to BeamAsm that violate
> these break T2 silently. The contract:
>
> 1. **Per-instruction PC table.** BeamAsm emits a table
>    mapping each `(Module, Function, BEAM-op-index)` to the
>    native PC of that instruction. T2 reads this at codegen
>    to resolve outer-function deopt targets (§9.1).
>    *Invariant*: the table is dense (every BEAM op has an
>    entry) and stable across module loads.
> 2. **Calling-convention register assignments.**
>    `x19=scheduler_registers, x20=E, x21=c_p, x22=FCALLS,
>    x23=HTOP, x25–x28=XREG0..3, x15–x17=XREG4..5`.
>    *Invariant*: T2 can rely on these mappings being
>    BeamAsm's globally.
> 3. **Global runtime fragments.** `i_test_yield_shared`,
>    `context_switch_simplified`, GC entry, exception raise,
>    save-calls dispatch. T2 calls these.
>    *Invariant*: their calling conventions and side effects
>    don't change without T2-side coordination.
> 4. **Patchable function prologue.** T1's 3-instruction
>    patchable region for tracing breakpoints. T2 emits a
>    compatible region.
>    *Invariant*: the patch format and `generic_bp_global`
>    target convention don't change.
>
> **Enforcement.** `beam_jit_t2.h` declares each invariant via
> `static_assert` checks (register numbers, function-pointer
> signatures). CI fails the build if a BeamAsm change drifts
> from the contract. Contract-breaking BeamAsm work requires
> a coordinated T2 update in the same change.

### L4. The "1 KLOC Erlang" budget is aspirational
- [ ] Appendix B's "Erlang ~1 KLOC" + "~0.5 KLOC" undersells
      the actual Erlang surface area.

**Suggested resolution.** Replace Appendix B's bare "Erlang ~1
KLOC" with an enumerated breakdown:

> **Erlang code (≤1.5 KLOC v1, broken down):**
> - JIT server (`gen_server` under `kernel/code`): ~300 LOC
> - SSA-chunk emit in `beam_asm.erl`: ~200 LOC
> - `jit_inline` annotation propagation in
>   `sys_core_fold_lists.erl` and friends: ~100 LOC
> - `code_server.erl` invalidation hook: ~150 LOC
> - Test-only BIFs and lifecycle drivers (§16A.3): ~300 LOC
> - Observability (`erlang:t2_stats/0`,
>   `erlang:t2_info/3`): ~200 LOC
> - AOT-side type-information chunk additions (modifications,
>   not net new file): ~250 LOC
> - **Total: ~1500 LOC.** If AOT-side work balloons beyond
>   ~250 LOC, the budget breaks; revisit and re-scope.

### L5. The HiPE failure mode wasn't just maintainership
- [ ] HiPE's other failure mode was narrow optimization wins
      (numeric workloads only). T2 hasn't proven inlining wins
      are broad.

**Suggested resolution.** Add to §18 risks:

> **Inlining wins are smaller than projected.** HiPE shipped
> real but narrow optimizations and lost on the breadth-of-
> applicability test. T2's thesis is that inlining-driven wins
> are broad across Erlang workloads, but this is unproven until
> Phase 0's inlining-thesis sizing measurement (§17 audits) and
> the prototype (§17 sequencing).
>
> **Mitigation**: if the corpus measurement shows inlining wins
> covering less than (say) **30%** of hot-path BEAM ops on the
> F2 corpus, descope to ship just the type-narrowing speculation
> tier (Phase 2 wins) without the inlining MVP (Phase 3). The
> descoped product delivers Phase 2's ~10–20% wins on type-
> narrowable hot paths and avoids Phase 3's largest maintenance
> cost. The threshold is set in Phase 0 based on the prototype
> measurement; ship-vs-descope is a decision point at the end
> of Phase 0.

---

## How to use this document

1. Walk top to bottom.
2. For each `[ ]`, edit the **Suggested resolution** if you disagree
   (or accept it as written), then mark the item:
   - `[x] fix` + commit hash applying the resolution.
   - `[x] wontfix` + 1-line reason.
   - `[x] disputed` + 1-line counter.
   - `[x] defer` + which phase / which open question it goes into.
3. When all are addressed, the surviving fixes feed back into
   `T2.md` and this critique file gets archived (or deleted —
   it's superseded by the plan it improved).
