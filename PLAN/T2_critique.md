# T2 Plan — Critique (second-pass)

A second-pass review of `PLAN/T2.md` after the first round of resolutions
landed. The first round (sections A–F, 45 items) is in git history at
commits `ed325311f6` (initial), `7808eee6bb` (sharper points),
`b8087cc60c` (close-out), and the corresponding T2.md commits
`a06ac5e381` (A-series), `66174466c2` (B-series), `43556cb9d6`
(C-series), `29c6cb5916` (D-series), `473c8e7e99` (E-series),
`d10f1a55f3` (F-series).

This pass found 51 items in G–L, plus 6 follow-ups in M after
the user's inline review:

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
- **M. Architectural concerns surfaced in second-pass review.** Larger
  items the inline comments on G–L exposed (tier-up target selection,
  GC + pre-emption inside inlined regions, hibernation interaction,
  `erlang:memory()` reporting, call-frequency signal extensions,
  in-flight compile generation check).

Each item has a **Suggested resolution** that's a starting point — edit
or replace if you disagree. Where the user already wrote an inline
reply, the file shows the original suggestion, the user comment, and
a **Revised suggested resolution** that incorporates the comment.

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

**Original suggested resolution.** Spill tagged values to fresh
scratch Y slots reserved for the inlined region's lifetime; the
GC walker picks them up via the existing Y-register scan.

If a GC needs to be done, should we not restore the frame state?
The GC could mean that we are pre-empted, so it may have to be done
anyway.

**Revised suggested resolution: eager deopt on GC inside inlined
regions.** Agreed with the user comment. Treat any GC site inside
an inlined region as a synchronous deopt:

> **GC inside an inlined region triggers eager deopt.** GC sites
> inside an inlined region behave as deopt points. The deopt stub
> at the inlined call site C runs first (X/Y restore from
> codegen-time framestate metadata, §9.2); the runtime then enters
> GC at the outer-function state at C; if GC schedules the process
> out, it yields from outer-function state — which has a T1 PC and
> resumes naturally in T1 (§12.4 item 3). After GC, we're in T1;
> the function may re-tier-up later via the normal call-counter
> path.
>
> This trades the per-GC inlining win for architectural
> simplicity: no need to spill tagged values to scratch Y slots,
> no GC stackmap for inlined regions, no in-inlined yield path.
> GC inside inlined loops is the slow path; it pays an extra
> deopt-and-recompile cost. Phase 1 measurement quantifies how
> often GC fires mid-inlined-region in practice. If frequent
> enough to matter, v2 can introduce stackmap-based GC for
> inlined regions.

The original spill-to-Y-slots resolution remains as a v2 fallback
if eager-deopt-on-GC turns out to throw away too much performance.

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

**Original suggested resolution.** Per-scheduler-sharded counter,
non-atomic store, eviction reads union.

If we have many schedulers, we might have to make some schedulers share
counters in order to not explode in memory.

**Revised suggested resolution: bounded shard count.** The shard
count is `min(erts_no_schedulers, MAX_SHARDS)` where
`MAX_SHARDS` is a tunable upper bound (default 8). Schedulers map
to shards by `scheduler_id mod num_shards`. With 64 schedulers
and `MAX_SHARDS=8`, eight schedulers share each shard — non-atomic
stores from those eight schedulers may lose increments under
contention, but the loss is bounded and the eviction policy is
robust to small undercounts. This caps memory at
`MAX_SHARDS × num_slots × sizeof(slot)` per blob regardless of
machine size. Apply the same bound to the C8 profile-counter
shards.

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

Can we make the AOT compiler automatically mark things as inlineable?
So that we don't have to do it manually? We of course need to do it
manually for BIFs, as there we would have to have a T2 ops implementation
that we can inline, but for Erlang code it would be better if the AOT
compiler can do that analysis.

**Revised suggested resolution: AOT auto-inferred inlineability
for Erlang code.** Yes — replace the user-opt-in attribute with
an AOT analysis pass:

> **Manifest sources, in v1**:
>
> 1. **BIF allow-list** (manual): the §10.7 guard BIFs plus the
>    `sys_core_fold_lists.erl` set, plus the curated additions
>    listed above. These need T2-side IR-op implementations and
>    can't be auto-inferred.
>
> 2. **AOT-inferred Erlang code**: a new compiler pass walks
>    each module's SSA and marks every local function whose body
>    is "T2-inlineable" with `jit_inline => #{...}` on
>    `b_function.anno`. Eligibility:
>    - All BEAM ops in the function are in the supported phase
>      set (§17 Phase A in v1).
>    - No process-dictionary access, no message ops, no traps.
>    - Bounded recursion depth (or self-tail-recursion → loop
>      recovery, §10.5).
>    - Body size below the §10.3 threshold.
>
>    The AOT pass is purely additive — modules compiled without
>    the new pass still load fine; their functions just don't
>    auto-inline.
>
> User-module opt-in via `-jit_inline` attribute remains for the
> rare case where the auto-inference is too conservative.

Phase 0's inlining-thesis sizing (F2 corpus) measures how often
auto-inference catches user code; if the hit rate is high, we
can drop the manual-opt-in path entirely.

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

In the example `fun_handler(Items, F) -> lists:map(F, Items)`,
I would have expected the call to `fun_handler/2` to be inlined
into its caller, which could have F as a literal and then
the inlining should work.

This brings up an important point, how do we make sure that the
best place to inline is chosen as the starting point to JIT?
Most likely things such as `lists:map/2` will trigger the JIT
counter very early, but there is very little gain in compiling
that, we want to compile its caller, or quite possible its callers
caller.

**Revised suggested resolution.** Two pieces:

1. **The fun_handler example** does work in v1 *if* the caller
   tier-ups before fun_handler does — in which case T2 inlines
   `fun_handler/2` (chained with `lists:map/2`) into the caller,
   sees `F` as a literal at the caller's site, and the
   constant-fun precondition holds. The text in the original H11
   resolution was too pessimistic; it assumed `fun_handler/2`
   tier-ups standalone. Reword §10.2's bullet:

   > **Higher-order helpers called with a non-literal fun at
   > *this* compile unit's boundaries.** If T2 compiles
   > `fun_handler/2` standalone (no caller inlining), the inner
   > `lists:map(F, Items)` has `F` as a parameter — non-literal
   > from this compile unit's perspective — and is not inlined.
   > **The fix is in M1's tier-up target selection**: don't
   > tier-up annotated higher-order wrappers standalone; let the
   > caller's tier-up pull them in. With the caller as the
   > compile unit, `F` is often a literal, and the inlining
   > chain `caller → fun_handler → lists:map → user_fun` works.

2. **Tier-up target selection** is a new architectural concern
   raised by the user comment — broken out into **M1** below.

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

**Original suggested resolution.** Inline a 16-cell fast-path;
tail-call BIF for longer lists.

Why do we even bother with length? Can't we just use the BIF as T1 does?

**Revised suggested resolution: drop length/1 from the manifest
of inlined guard BIFs.** Agreed. T2 lowers `length/1` as a
regular `call_ext` to the existing BIF, exactly as T1 does. No
inline fast path; no special trap-out machinery; no
fast-and-slow-paths-inseparable invariant (C5 can drop
length/1 from the example).

Update §10.7 manifest: remove `length/1` from the list. Add a
note: "`length/1` is treated as an opaque BIF call in v1 — the
existing T1 BIF implementation handles trap-out and is fast
enough that inline lowering doesn't pay the complexity. v2 may
revisit if profile data shows length/1 dominates a hot path."

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

When developing T1, we created a SUITE that was specifically
made to emit all instructions. We should do something similar here,
that is create testcases that excerize the specific parts so
that we get a shorter feedback loop.

**Additional resolution: per-feature exerciser suite (§16A.7).**
Modeled on BeamAsm's per-instruction exerciser:

> ### 16A.7 T2 feature exerciser suite (Phase 1 onwards)
>
> A targeted Common Test suite (`t2_SUITE`) with a separate
> testcase per T2 IR op kind, per speculation kind, per inlining
> shape, per loop-recovery shape, etc. Each testcase constructs
> the smallest possible Erlang fragment that hits the feature,
> compiles it under T2, and checks observable behaviour and the
> emitted IR.
>
> Examples:
> - `speculate_type_smallint_outer/0`: a function with one
>   integer parameter, one speculate_type, one add, one return.
>   Verify: T2 emits the one-untag form, deopt jumps to T1 PC.
> - `speculate_type_smallint_inlined/0`: same, but inside an
>   inlined `lists:foldl/3` body.
> - `inline_constant_fun_lists_map/0`: `lists:map(fun(X) -> X*2 end, L)`.
>   Verify: closure is inlined, no `make_fun` allocation, loop
>   recovery fires.
> - `unroll_lists_filter/0`: factor-4 unrolling on
>   `lists:filter/2` body. Verify: combined `test_heap`, single
>   back-edge per 4 iterations.
>
> Targeted suite gives a sub-second feedback loop per feature.
> Failures point at a single op or pass, not a whole-application
> regression.

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

Can we do it like this, if we find such a process, then we scan
first 1024 slots and then replace a return address with one
to some special code that resumes the checking. So that if we
get to that point while the process is running we continue scanning
1024 more.

We would of course need to store the original data somewhere,
but that could be done in the scheduler_data structs and then it is restored
when the process is scheduled out.

As an optimization we could store in the process how far it has scanned
so that at next schedule in, it knows where to continue. This would
cost a work in the process struct though... unless we store it
on the stack somewhere?

**Revised suggested resolution: continuation-trampoline scan.**
Adopt the user's proposal:

> **Bounded incremental scan with continuation trampolines.**
> The schedule-in scan walks at most `+JT2lazy_scan_max_depth N`
> stack frames (default 1024) per visit. If unscanned frames
> remain below the bound:
>
> 1. The scan finds the deepest in-bound CP that pointed into
>    a tombstoned blob and patches it normally (CP-to-T1-PC
>    metadata lookup).
> 2. The scan replaces the *next* CP (the first one that would
>    have been scanned but exceeded the bound) with the address
>    of a per-process **scan-continuation trampoline**.
> 3. The original CP value is preserved in the slot immediately
>    above the patched one — the trampoline reads it on entry.
>    No new per-process heap fields needed; we use the stack we
>    were already walking.
> 4. The process resumes. When unwinding eventually pops back
>    to the patched return address, the trampoline runs:
>    - Restore the original CP from its preserved location.
>    - Walk the next 1024 frames using the same procedure
>      (patch tombstone hits in place; install another
>      trampoline if necessary).
>    - Branch to the (now-patched) original CP.
>
> Cost: O(N) work per stack-unwind to depth N; deferred until
> the process actually unwinds that far. Pays nothing for
> processes that never unwind past their current depth.
>
> Tombstoned blobs can be freed only when *no* trampoline still
> references them. Add a per-blob "trampoline reference count"
> incremented when a trampoline is installed pointing at one of
> the blob's CPs and decremented when the trampoline runs. Free
> the blob (Phase B per §14.2) when both the per-process
> generation watermark advances *and* the trampoline refcount
> hits zero.

This replaces the truncate-and-deopt fallback. Track
`scan_trampoline_count` and `max_unwind_completion_lag` in
`erlang:t2_stats/0`.

### J4. No discussion of dist (cross-arch nodes)
- [ ] Mixed aarch64+x86_64 cluster: SSA chunk wasted on x86; how
      do cross-node code-upgrades interact?

**Original suggested resolution.** Distributed-Erlang
considerations + SSA-chunk wasted on x86 nodes.

Uhm, I think you have missunderstood something. code is always local,
there is nothing to take into consideration for cross-node things.

**Revised suggested resolution.** Agreed — code is always local.
The only real concern is the wasted bytes on x86_64 nodes that
load BEAM files compiled with the SSA chunk (in v1, before x86_64
T2 lands). Add to §19 (out of scope):

> **SSA chunk on non-T2 architectures (v1 only).** BEAM files
> compiled with the SSA chunk and `jit_inline` annotations carry
> those bytes even when loaded on x86_64 nodes (where T2 is not
> enabled in v1). This is acceptable for v1; when x86_64 T2 lands
> (Phase 10), the chunk is paid for on both arches. Phase 0 chunk-
> size measurement (§17 audits) sets the absolute size; if it's
> unacceptable, the SSA chunk can be made arch-conditional at
> load time (skip loading on architectures without T2).

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

No, just abort. No need to make things this complicated. There
should not be any such bugs at all. Possibly we can add something
to deal with T2 IR validation assertion, but the others are
not things that should happen.

**Revised suggested resolution.** Drop the production safe-mode.
§8.4 stays as-is for asmjit reject, watchpoint race, and other
invariant violations: they abort the BEAM (those genuinely
shouldn't happen, and silent degradation hides the bug).

The one possible exception is **T2 IR validation assertion** —
which is closer to "we don't yet know all the patterns the AOT
SSA can produce" than to "memory corruption". Add to §8.4:

> **T2 IR validation failure** (the C++ assertion in the IR
> builder, e.g. an unexpected SSA shape, a malformed phi, an
> unbound value). Disable T2 globally for the lifetime of this
> module (per-`{Mod,Fun,Arity}` blacklist applied to the whole
> module on first failure); log via `logger:error/2` with the
> failing case; increment `t2_ir_validation_failures` in
> `erlang:t2_stats/0`. Module reload clears the per-module
> blacklist. **All other failure modes still abort the BEAM.**

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

2. ~~Add a per-process opt-out~~ rejected by user — every-call
   check is too expensive.

No, this would mean that we have to check a flag everytime a
T2 call is made, and we don't want to have that overhead for
a call. We can allow the user to trade memory for latency if
they want to.

**Revised suggested resolution: memory-for-latency knobs.**
Drop the per-process flag. Provide cache-sizing knobs that let
operators trade memory for predictable latency by reducing
jettison frequency:

> **Latency-tuning knobs** for hard-RT operators. Increasing
> these spends memory to reduce stack-scan frequency:
>
> - `+JT2cache N` — total T2 code-cache budget (§13.1, default
>   64 MB). Larger cache → fewer evictions → fewer tombstones
>   → fewer scans.
> - `+JT2tombstone_grace_seconds N` — minimum time a tombstoned
>   blob remains alive before Phase B frees it (default 0; pure
>   schedule-in-driven). Larger value → more memory held but
>   less proactive sweeping.
> - `+JT2decay_interval N` — active-execution-counter decay
>   period (J7). Larger value → eviction policy biases against
>   evicting any blob → less churn.
> - `+JT2lazy_scan_max_depth N` — bounded scan; combined with
>   the J3 trampoline mechanism, lets operators cap per-
>   schedule-in latency at a known cost.
>
> Operators with strict SLAs run with a large `+JT2cache` and
> tuned scan depth; in practice T2 jettisons are rare in
> long-running steady state, so the latency exposure is small.
> Acknowledge in §18 risks that hard-RT viability needs measurement.

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

**Original suggested resolution.** Add an inlining-wins risk
with a 30%-coverage descope condition.

IIRC there was a HiPE paper that did run-time profiling of types
using Erlang tracing and then used that profiling to guide
HiPE optimizations. Can you find that paper and see what conclusions
where made there?

**HiPErJiT (Kallas & Sagonas, IFL 2018).** Found and read the
paper in full. The relevant takeaways for T2:

**What HiPErJiT did:**
- Profile-driven JIT on top of HiPE. Used ERTS's existing
  `erlang:trace/3` infrastructure for profiling — call/return_to
  /sched_in/sched_out events into a profiler process.
- Profiled three things: (a) per-function execution time, (b)
  call frequencies *between function pairs*, (c) argument types
  via depth-k type abstraction.
- For massive concurrency, used "genealogy-based statistical
  profiling": maintained a process tree by spawn site, profiled
  only a 10% sample of leaf processes per spawn-MFA group.
- Two profile-driven optimizations on top of standard HiPE:
  **optimistic type compilation** (duplicate function into
  `f$opt` and `f$std`; header function dispatches by type test;
  `f$opt` is type-specialised by HiPE's existing type-analysis
  pass given the profiled types) and **profile-driven inlining**
  (greedy: inline the most-called callee into each function up
  to a 1.6× code-size cap).

**Performance results (paper's Tables 1, 2, 3):**
- Average ~2× speedup over BEAM, similar to HiPE.
- Beat HiPE on 5 small benchmarks (nrev, qsort, fib, smith, tak)
  via profile-driven inlining decisions HiPE's static heuristic
  missed. The smith case is canonical: HiPErJiT inlined
  `alpha_beta_penalty/2` and `max/2` into `match_entry/5` based
  on call frequency; HiPE's static heuristic skipped these.
- **Lost to HiPE on Dialyzer (real workload, ~30 KLOC):**
  HiPE 1.78× vs HiPErJiT 1.46× on PLT building, 1.73× vs 1.42×
  on analysis. The paper attributes this to (1) profiling
  overhead, (2) not all modules JIT-compiled from start.
- **Lost to BEAM on the ring benchmark** (heavy message-passing,
  ~1.5 M messages/sec). Paper: "the majority of the execution
  time is spent on message passing, which is handled by BEAM's
  run-time system. Finally, there are a lot of process spawns
  and exits ... which leads to considerable profiling overhead".

**Profiling overhead measurements (Section 5.1):**
- Sequential benchmarks: 5–19% overhead.
- Concurrent benchmarks: 19% average; up to 40% on heavily-
  concurrent programs.
- Genealogy-based sampling helped (without it: 13% additional
  overhead on concurrent benchmarks).

**Implications for T2:**

1. **Profile-driven inlining beats static inlining on the
   right workloads** — validates our inlining thesis in the
   right direction. The smith / nrev / qsort wins are exactly
   the cross-procedural-context-needs-profile pattern T2 targets.

2. **But profile-driven approaches lose on real workloads**
   (Dialyzer 1.46× vs 1.78× HiPE) primarily because of
   *profiling overhead*. This is the L1 concern made concrete:
   even when the optimizer is otherwise good, the profiling
   tax can swamp the win on programs that don't have enough
   pure compute to amortise it.

3. **Heavy-message-passing workloads are hostile to
   profile-driven JITs.** HiPErJiT lost to BEAM on the ring
   benchmark. Erlang services that are predominantly
   message-passing (most gen_servers, supervisors, distributed
   systems) won't benefit unless the profiling overhead is
   pushed below the per-message-cycle cost.

4. **HiPErJiT used full erlang:trace/3 for profiling.** That's
   the heavyweight option. T2 uses lightweight per-function
   counters embedded in BeamAsm code (§7). Our approach should
   be much cheaper than HiPErJiT's, but L1 / J5 / phase-0-audit
   still need to verify this on similar message-heavy workloads.

5. **HiPErJiT's call-frequency-driven inlining is the key
   profile signal** — not just types. Their main inlining
   heuristic is "F_a calls F_b more than F_a calls anyone else
   ⇒ inline F_b into F_a". Our plan (§7.5 per-call-site
   monomorphic-target slot) collects this data but only uses it
   for *target identification* (to inline the right monomorphic
   callee). We should also use the call-frequency for **tier-up
   target selection** (M1) and **inlining priority**.

6. **The "f$opt / f$std with type-test header" pattern** is
   essentially what BeamAsm already does for arithmetic fast
   paths. T2's `speculate_type` + jump-to-T1-on-fail is a
   slightly different shape (one optimised version, deopt to
   the original) but achieves the same effect with less code
   duplication.

**Revised suggested resolution.** Add to §18 risks:

> **Profile-driven JIT on Erlang historically loses on
> message-heavy real workloads** (HiPErJiT, IFL 2018: HiPE
> 1.78× vs HiPErJiT 1.46× on Dialyzer; HiPErJiT slower than
> BEAM on the ring benchmark, primarily because of profiling
> overhead). The historical evidence is that profile-driven
> *static-style optimization* over Erlang traces struggles to
> beat AOT on real applications even when the optimizer is
> sound; the bottleneck is the profile-collection cost.
>
> **What's different for T2:** profile collection is
> per-function (eligible-only), per-scheduler-sharded (C8),
> single-writer scheduler-1-only by default. HiPErJiT used full
> `erlang:trace/3`; T2 uses inline counters cheaper by an order
> of magnitude.
>
> **What's the same risk:** if the profile overhead exceeds the
> compilation benefit on real Erlang workloads (which are
> typically message-heavy), v1 ships at "comparable to BEAM"
> rather than "faster than BEAM" on the workloads users care
> about.
>
> **Mitigation**: Phase 0 measurement against the F2 corpus
> (RabbitMQ, Dialyzer, Elixir compiler) verifies the per-call
> profile cost is within the L1 3% steady-state budget.
> HiPErJiT's ring benchmark is a useful failure-mode test —
> include a similar message-passing-dominated benchmark in the
> corpus. **If profile overhead exceeds budget on real
> workloads, descope** to:
>
> - Trigger profiling on a function only after its call counter
>   crosses a "warm" threshold (drop profile-emission for
>   lukewarm functions).
> - Or: ship just the type-narrowing speculation tier (Phase 2)
>   without the inlining MVP (Phase 3) if inlining wins are too
>   narrow to justify the profile tax.
>
> The decision point is end of Phase 0 based on the prototype
> measurement.

The HiPErJiT lesson also reinforces M1 (tier-up target
selection): their call-frequency-driven inlining was the win on
the small benchmarks where they beat HiPE. Static call counters
don't capture this; we should use the per-call-site target slots
(§7.5) as a call-frequency signal too, not just for monomorphic-
target identification.

---

## M. Architectural concerns surfaced in second-pass review

These items came out of the user's inline comments on H-L; large
enough to deserve their own entries.

### M1. Tier-up target selection — compile the *right* function
- [ ] The current tier-up trigger is a per-function call counter
      (§7.4). Hot library functions (`lists:map/2`, `lists:foldl/3`,
      `maps:get/2`, …) trip first because they're called from
      everywhere. But the *inlining win* lives in their callers —
      compiling `lists:map/2` standalone doesn't see the
      `fun(X) -> X * 2 end` literal at the caller's site, so the
      higher-order helper inlines into nothing meaningful.
- [ ] HiPErJiT's data (L5) is the warning: their best wins came
      from inlining the *right* callee into a *specific* caller
      (smith: `alpha_beta_penalty/2` into `match_entry/5`),
      driven by call-frequency observations. Static call-count-
      only tier-up doesn't encode this.

**Suggested resolution.** Three pieces:

1. **Annotated higher-order helpers don't tier up standalone.**
   Functions carrying `jit_inline => #{fun_arg_pos => N}` (§10.4)
   are *not* compiled in isolation; they're inlined from a hot
   caller's compile unit. The annotation doubles as a
   "tier-up-suppress" flag for the standalone form.

2. **Compile up the call chain, not down.** When a function F
   trips its call counter, the JIT server consults the per-
   call-site target slots (§7.5) for F to discover *who calls
   F*. If F's dominant caller (>50% of incoming calls per
   profile) is itself tier-2-eligible, compile the *caller*
   instead of F (with F as an inlining candidate). One step up
   per tier-up trip; if the caller has a dominant caller of its
   own, the *next* tier-up trip rolls one more step up.

   This requires reverse call-frequency tracking. The §7.5
   slots already record outgoing target frequency; we need the
   incoming side. Cheap addition: a `Uint32 inbound_call_count`
   per function, incremented at each call entry alongside the
   §7.4 call counter.

3. **Heuristic cap.** Don't roll up forever — bound at 2 levels
   (caller-of-caller). Above that, just compile the function
   that tripped. The bound prevents pathological cases (the
   whole call chain is mutually recursive; no stable
   "dominant caller").

**Test**: smith-style benchmark must show T2 inlining a
non-trivial helper into the caller (not just `lists:map`-style
patterns). Phase 3's Phase-3-bench includes this.

### M2. GC + pre-emption inside inlined regions
- [ ] If GC fires inside an inlined region, the GC may decide to
      pre-empt (yield) the process. On yield, `c_p->i` saves a
      *T1* PC for resume; the in-inlined-region state has no T1
      equivalent.
- [ ] Without explicit handling, this is a correctness hole:
      yield resumes in T1 from the wrong PC, or the inlined-
      region's spilled-tagged-values (H1) become unreachable
      because the resume side doesn't know where they live.

**Suggested resolution: eager deopt on GC inside inlined
regions** (also in H1's revised resolution; broken out here for
visibility):

> When T2 emits an inlined region containing one or more
> potential GC sites, the inliner emits an *eager deopt* at
> each GC site:
>
> 1. Run the per-region deopt stub (X/Y restore from codegen-
>    time framestate metadata, §9.2).
> 2. Branch to T1's PC for the inlined call site C.
> 3. T1 then performs the GC at outer-function state (where it
>    expects to be); if GC pre-empts, the yield happens from
>    valid T1 state.
>
> After GC the process is in T1 mid-call. The function may
> re-tier-up later via the normal call-counter path.
>
> Cost: every GC inside an inlined loop pays the deopt cost
> (~5–10 X/Y-restore moves + branch). Inlining is throttled at
> GC events. Acceptable for v1; v2 may add stackmap-based GC
> for inlined regions if Phase 1 measurement shows GC events
> dominate inlined-loop performance.

This subsumes H1 — there's no need for the "spill tagged values
to scratch Y slots" mechanism if GC always deopts. Simplifies
§12.3 (inlined regions don't need GC-aware spill metadata) and
the inliner (no flush sequences at GC sites — the deopt stub
already restores the correct state).

### M3. Hibernation interaction with per-process T2 fields
- [ ] B6 added `p->t2_scan_gen`, `p->seen_t2`. Hibernation
      compacts the process's heap and stack but the per-process
      flags must survive. They're plain integer / bool fields
      on the process struct, so survival is trivial — but
      worth verifying in the implementation.
- [ ] Stronger concern: a hibernated process's stack is
      typically empty (only a continuation MFA), so the
      tombstone scan finds nothing to patch. But `seen_t2` is
      set; a future call back into T2 from the hibernation
      resumption is fine. The interaction is benign — just
      worth confirming no per-process T2 metadata lives on the
      heap (which gets compacted).

**Suggested resolution.** Add to the implementation checklist
(§17 Phase 0 audits):

> **Hibernation audit.** Verify that all per-process T2 metadata
> (`t2_scan_gen`, `seen_t2`, `last_scheduled_gen`, any future
> additions) live on the process struct directly, not on the
> heap. Hibernation compacts the heap; metadata on the heap
> would be lost or corrupted.

### M4. T2 code cache in `erlang:memory()` reporting
- [ ] T2 code lives in a separate `JitAllocator` (§13.1), 64 MB
      default. Operators using `erlang:memory()` to track VM
      memory consumption don't see it accounted for.

**Suggested resolution.** Add to §16:

> Extend `erlang:memory/0,1` with a new key `jit_t2_code` that
> reports the bytes currently allocated to T2 blobs (active +
> tombstoned). Existing `code` key continues to report
> BeamAsm-allocated T1 code unchanged. Tools that introspect
> `erlang:memory()` need updating to know about the new key;
> the addition is backwards-compatible (no removed keys).

### M5. Call-frequency signal beyond monomorphic-target identification
- [ ] §7.5 per-call-site monomorphic-target slot records *which*
      target a call site reaches. It doesn't record *how often*.
      HiPErJiT (L5) shows that call-frequency between specific
      function pairs is the strongest profile-driven inlining
      signal — stronger than monomorphic-target alone.

**Suggested resolution.** Extend §7.5:

> **Per-call-site monomorphic-target slot, with frequency.**
> Each slot records target identity *and* call count from this
> site:
>
> ```c
> typedef struct {
>     Export*  target;       // or POLY
>     Uint32   call_count;   // incoming calls observed at this site
> } T2CallSiteSlot;
> ```
>
> The inliner consumes both: monomorphic + high-frequency at
> this site → strong inline candidate (top priority); monomorphic
> + low-frequency → low priority (skip if size budget tight);
> polymorphic → not inlined in v1.
>
> The frequency signal also feeds tier-up target selection (M1):
> when a function trips its call counter, the JIT server
> aggregates the inbound-call-frequency from the per-site slots
> of all known callers to pick the right *compile unit*.

### M6. Hot module reload during compile (in-flight blob installation)
- [ ] B3 partially addresses this (sub-race (a)): in-flight T2
      compile reads stale SSA, installs blob *after* the
      watchpoint already fired. The fix mentioned a "generation
      counter checked at install time" but the plan doesn't
      describe how it's implemented.

**Suggested resolution.** Make the generation-counter mechanism
concrete in §14.2:

> **In-flight compile generation check.** When the JIT server
> dispatches a compile job for `{M,F,A}`, it captures the
> current `module_load_gen[M]` and stores it with the job. At
> blob-install time, the server re-reads `module_load_gen[M]`;
> if it has changed, the compile is discarded (the blob is
> never installed; counters are reset; the function may
> retrip later under the new module). The generation counter
> is incremented atomically by the code loader before the
> module reload commits.

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
