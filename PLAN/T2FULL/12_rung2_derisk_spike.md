# Rung-2 de-risk spike — persistent T2 return-CPs on live stacks (2026-07-07)

> **Question the spike answers:** are persistent T2 return-CPs on live Erlang
> stacks *tractable* — correct under every stack-walker, and safe to jettison —
> so that the full rung-2 + inliner build (P3) is worth committing? This is the
> highest-risk runtime work in the plan: it touches GC stack scanning, exception
> unwinding, stacktrace building, and code purge.
>
> Built off `lukas/erts/beamjit2` tip `c50815b06d`, isolated worktree,
> `otp-beamjit2` untouched, own aarch64/macOS build (RA frame layout).
>
> **This file is the design memo (Step 1). Stress-test results (Step 3), parity
> (Step 4) and the GO/NO-GO verdict live in §7–§9, filled from the flagged
> `T2_RUNG2_SPIKE` implementation.**

## 0. TL;DR

The dangerous-sounding part — "every CP walker must learn about T2 blobs" — is
**already 80% done**, because the P2 back-edge-yield work had to teach
`erts_lookup_function_info` about blobs so a yielded `c_p->i` resolves to an MFA.
The read-only introspection family (stacktrace, `current_function`,
`current_stacktrace`, tracing, crash dump) inherits that and resolves a T2
return-CP to the **correct MFA for free**; exception unwinding and the GC/DEBUG
stack validators are **structurally CP-agnostic** and need nothing. On aarch64
there is **no frame-pointer chain** (RA layout only), which removes an entire
class of walker complexity the plan budgeted for.

The **one genuine correctness hazard** is `check_process_code` (the purge
proof-of-no-reference): its stack scan matches CPs only against the *module's*
code range, so a T2-blob return-CP on the stack is **silently missed** → the
purger frees code a pending return still points at → use-after-free. This is
exactly the hazard the **tombstone + lazy-stack-scan** lifecycle exists to
cover, and it generalizes the *already-shipping* two-phase retire that today
translates only `c_p->i`.

Crucially, a **self-recursive, non-inlined** spike keeps each T2 frame **1:1**
with its Erlang frame, so it needs **no framestates** — side exits use the
existing EFFECT/ERROR/CONT pctab entries unchanged. That cleanly isolates the
"persistent CP on stack" risk (this spike) from the "framestate/inliner"
complexity (the rest of rung-2).

## 1. The invariant being broken (what makes this risky)

The shipping T2 tier is built on one load-bearing rule, stated in the source:
**a T2 blob never leaves a return address on an Erlang stack.** A blob PC is held
in exactly one place — a yielded process's `c_p->i` parked at a back-edge resume
stub — and even that is transient and translated on jettison.

- `t2_emit.cpp:1342-1350` — non-tail calls "never `bl`", materialize the **T1**
  continuation as the CP; "no return addresses into T2 blobs (PLAN/T2/08 §4.3)".
- `t2_install.c:108-111` — "By construction no live `c_p->i` ever points into a
  P1 blob".
- `t2_install.c:159-163` — "P1 blobs hold no CPs/resume PCs, so in-flight
  invocations leave on their next call/return/side-exit".
- `t2_ranges.h:29-36` — blobs are kept out of `beam_ranges` precisely because
  `erts_lookup_function_info` would cast `rp->start` to a `BeamCodeHeader*` and
  misread a blob; a separate registry (`erts_t2_find_blob`) exists instead.

Rung-2 deliberately violates this: the ascent stays in T2 by pushing a **T2
return CP** (an in-blob landing pad) so the callee's `ret` lands back in the
blob. Every mechanism below is judged against that stronger requirement.

### 1.1 The mechanism on aarch64 (RA layout, verified)

- Frame layout on aarch64 is **always `ERTS_FRAME_LAYOUT_RA`**, `CP_SIZE == 1`.
  `ERLANG_FRAME_POINTERS` is undefined on all ARM builds (`erts/configure.ac`
  forces `enable_native_stack=no` for ARM; the FP_RA two-word push is x86-only,
  `x86/beam_asm.hpp:671`). ARM Erlang-frame emitters never reference
  `erts_frame_layout` (0 hits). **The design targets RA only.**
- The CP is **one word**, pushed by the *callee's* prologue
  (`emit_enter_erlang_frame`, `arm/beam_asm.hpp:429`: `str x30,[E,#-8]!`) and it
  lands at the **top of the callee frame** (`[E + N*8]` = `getYRef(N)` after
  `allocate N`). It is not explicitly tagged; `is_CP(x)` is just low-2-bits-zero
  (`erl_term.h:1379`), true for any ≥4-byte-aligned code address.
- Return is `ldr x30,[E],#8; ret x30` (`arm/instr_call.cpp:54`).
- **Therefore a persistent T2 return-CP requires no callee change at all**: the
  caller alone chooses the CP by the value it leaves in `x30` before branching
  to the callee entry. The current demote sequence
  (`mov x30,<t1_cont>; br <callee>`, `t2_emit.cpp:1368/1378-1381`) becomes
  `bl <callee>` (or `mov x30,<in-blob landing pad>; br <callee>`), with the
  continuation ops bound as the landing pad.
- The post-call continuation ops are **already physically emitted into the blob**
  today (the LIR keeps `Call` as a non-terminator, `t2_isel.cpp:906-951`;
  `mark_unreachable()` only records an offset, `arm/beam_asm.hpp:882`, it does
  not suppress emission). Today they are dead code reached only in T1; rung-2
  binds a label there and makes the callee return into them. **The ascent code
  already exists in the blob** — this is why the spike's emit change is small.

## 2. CP-walker inventory (grounded, per-walker verdict)

Legend: **NONE** = works unchanged; **FREE** = already blob-aware via the P2
`erts_lookup_function_info` fallback; **LINE** = correct MFA already, line number
needs a blob-PC→T1-PC reverse map; **HAZARD** = must change for correctness.

| # | Walker | file:line | How it finds CPs | Resolves CP→MFA? | Verdict |
|---|---|---|---|---|---|
| 1 | `erts_lookup_function_info` | `beam_ranges.c:248` | (resolver) | yes | **FREE** — `:261-273` already returns `blob->mfa` for a blob PC; only `loc` (line) is absent |
| 2 | GC rootset scan / stack copy | `erl_gc.c` (rootset), `1584-1602` (FP copy) | tag (`is_CP`) | no | **NONE** — a CP is primary-tag HEADER (low bits 0); never dereferenced as a heap term. FP-chain adjust is x86-only |
| 3 | `erts_validate_stack` (DEBUG) | `erl_gc.c:3845` | RA: skip; FP: chain | no | **NONE** — no-op in RA layout; never calls lookup |
| 4 | `gather_stacktrace` / `build_stacktrace` / `erts_build_stacktrace` | `beam_common.c:782,1205`; `erl_bif_info.c:2425` | scan + `erts_inspect_frame` | deferred → lookup | **FREE** for MFA; **LINE** for line fidelity |
| 5 | `process_info(current_function/current_stacktrace)` | `erl_bif_info.c:2357,2413` | `rp->i` + `erts_printable_return_address` | yes | **FREE** — same fallback (this is the path that already legitimately sees a blob `c_p->i`) |
| 6 | Exception unwind `next_catch` / `handle_error` | `beam_common.c:449,580` | scan for catch markers + trace sentinels | **no** | **NONE** — pops to the catch marker; never resolves a CP |
| 7 | `i_raise` / `raise_1` | `beam_common.c` | restores saved trace | no | **NONE** |
| 8 | Hibernate stack trim | `bif.c:1390-1408` | discards whole stack (`stop = hend - CP_SIZE`) | n/a | **NONE** — the stack (and every return-CP) is thrown away; wakes via fresh apply |
| 9 | Match-spec caller / bp / debugger / process-dump | `erl_db_util.c:2843`; `beam_bp.c:1875`; `erl_debugger.c`; `erl_process_dump.c` | `erts_inspect_frame` / top-of-stack | via lookup | **FREE** — inherit the fallback; dump degrades to "unknown function", never crashes |
| 10 | **`check_process_code`** (purge) | **`beam_bif_load.c:1200-1204`** | scan, `ErtsInArea` vs **module range** | no (range test) | **HAZARD** — a T2 stack CP is not in the module range → missed → UAF |
| 11 | `erts_check_copy_literals_gc_need` (literal purge) | `beam_bif_load.c:1007,1034` | boxed/list heap ptrs only | n/a | **NONE** — CPs are code ptrs, not heap refs; ignored |

Detail on the two that need work:

**#4 line fidelity (LINE).** `gather_stacktrace` records the raw return address;
`build_stacktrace` later calls `erts_lookup_function_info(_, pc, 1)`. For a blob
PC that returns the MFA but `loc == LINE_INVALID_LOCATION` (no T2 line table), so
the stack item is built without a line. To recover the line, the blob PC must be
mapped to its equivalent **T1** PC and that PC's line taken. The CONT pctab
(`ERTS_T2_PC_CONT`) already stores the T1 continuation PC per call site; the gap
is that `erts_t2_pc_lookup*` is keyed by `(fn_index, beam_idx)`, not by an
arbitrary blob address, so the full build needs a **blob-PC → {fn_index,
beam_idx}** side index (equivalently, a per-blob sorted table of landing-pad
offset → CONT T1 PC — see §3). Not a correctness issue; a fidelity gap.

**#10 the hazard (HAZARD).** `beam_bif_load.c:1200-1204`:
```c
for (sp = rp->stop; sp < STACK_START(rp); sp++) {
    if (is_CP(*sp) && ErtsInArea(cp_val(*sp), mod_start, mod_size)) {
        return am_true;   /* process references the old module */
    }
}
```
`mod_start/mod_size` are the module's `old.code_hdr`/`old.code_length`. A T2 blob
lives in a **separate** JIT mapping, so a blob CP fails `ErtsInArea` and the loop
reports **no reference** even though the process will return into that blob. The
single-IP T2 hook right above it (`:1181-1187`,
`erts_t2_find_blob(rp->i) && blob->code_hdr == old.code_hdr`) covers only `rp->i`,
not the deeper stack. This is the proof the purger trusts before freeing code at
`beam_bif_load.c:2260-2267` (`erts_t2_jettison_instance` then
`beamasm_purge_module` then `erts_t2_release`). Miss ⇒ free ⇒ UAF on return.

## 3. The jettison lifecycle (tombstone + lazy stack scan)

The fix generalizes machinery that **already ships** for `c_p->i`. Today
(`t2_install.c:170-270`) a blob with resume stubs is retired in two phases behind
code barriers: phase 1 walks the process table and translates any `c_p->i` inside
the blob span to its `t1_demote` target (per-entry, via the `resume_tab`); phase
2 asserts the span is clear, deregisters and frees. The generalization:

1. **Per-blob return-CP table (generalize `resume_tab`).** At emit, alongside
   each landing pad, record `{landing_pad_offset (blob-relative), t1_cont}` where
   `t1_cont` is the call site's `ERTS_T2_PC_CONT` T1 PC (already computed and
   pushed as the demote CP today, `t2_emit.cpp:1367/1377`). This is the exact
   analogue of `ErtsT2ResumeEntry {offset, t1_demote}` (`t2_ranges.h:68-77`) but
   for stack return-CPs rather than `c_p->i` resume PCs. It doubles as the
   blob-PC→T1-PC reverse map that closes the LINE gap in §2 #4.

2. **`check_process_code` extension (closes the HAZARD).** In the stack loop, for
   each `is_CP(*sp)`, also `blob = erts_t2_find_blob(cp_val(*sp))` and treat
   `blob && blob->code_hdr == modp->old.code_hdr` (or any blob depending on the
   module) as a reference — mirroring the `rp->i` hook, extended across the whole
   stack. This makes the purger's proof sound again.

3. **Tombstone + lazy stack scan (frees safely while CPs are live).** On
   jettison: revert the prologue (no new entries), **tombstone** the blob (keep
   it mapped and registered), bump a global generation. A process that is next
   scheduled in, or is walked by a high-water sweep, has its **whole Erlang
   stack** scanned: every `is_CP(*sp)` whose value falls in a tombstoned blob
   span is rewritten in place to the CP table's `t1_cont` for that offset (an
   aligned word store — the same store class the retire pass already does to
   `c_p->i`). After translation the CP points into T1 and return semantics match
   T1 exactly (`PLAN/T2/06 §5.3-5.5`). The blob is freed only once thread
   progress confirms no scheduler is inside it **and** every process has either
   been scanned since the tombstone or never had `seen_t2` set.

4. **Safety-assert broadening.** `t2_assert_no_resume_pc_in` (`t2_install.c:112`)
   and the phase-2 assert must scan **stacks**, not just `c_p->i`, before free.

### 3.1 Why the lazy scan is tractable here (RA layout dividend)

On aarch64 (RA layout) the stack has **no frame-pointer chain**; a full scan is a
linear sweep `for (sp = stop; sp < STACK_START; sp++)` testing `is_CP`. There is
a theoretical ambiguity — a boxed/immediate term whose bit pattern has low bits 0
would look like a CP — but this is the *identical* discrimination the existing
`check_process_code` stack loop, `next_catch`, and `gather_stacktrace` already
rely on, so the spike inherits, and does not worsen, that property. (The proper
frame walk via `erts_inspect_frame` is available if a bit-exact walk is wanted;
the existing purge scan chooses the linear `is_CP` sweep, and we match it.)

### 3.2 The interleaving that must be proven safe (Step 3 hazard #3/#4)

The nasty case: return CPs into a blob are live at depth on a **suspended**
process's stack when the blob is jettisoned. The retire order already handles the
`c_p->i` analogue; for stack CPs the ordering is: tombstone (sync, code-mod
permission) → barrier → **stack-translate every live process** → barrier →
assert-clear + free. A process resuming mid-flight either sees a translated T1 CP
(returns to T1) or, if scheduled in before its scan, is scanned at schedule-in
before it can execute the stale CP. Yield-mid-descent (hazard #4) combines both:
`c_p->i` (already handled) **and** stack CPs (new) coexist; both must translate.

## 4. The narrow spike — what is implemented (Step 2)

Flag: **`T2_RUNG2_SPIKE`** (env, default off; recognized in `t2_emit.cpp` and the
runtime paths). When off, behaviour is byte-identical to today (demote-on-return).

Scope, deliberately minimal to isolate the CP-on-stack risk:

- **One shape:** self-recursive non-tail call (`bfac`, `build`, `lsum` — synthetic,
  T2-eligible, forced in with `+JT2enable true T2_INSTALL_GATE=0`). **No inlining**
  ⇒ each T2 frame is 1:1 with its Erlang frame ⇒ **no framestates** ⇒ side exits
  keep using the existing EFFECT/ERROR/CONT pctab entries.
- **Emit change (`emit_lir_call`, non-tail, self-target):** instead of
  `mov x30,<t1_cont>; br <callee>`, bind an in-blob landing pad and push it as the
  CP so the callee returns into the blob (ascent stays in T2). Invalidate the
  register cache at the landing pad (callee clobbered caller-saved regs; the
  continuation reloads X regs from memory — the canonical state contract holds
  exactly as after a BIF call).
- **Return-CP table:** per-blob `{landing_pad_offset → t1_cont}`, registered with
  the blob range (generalizes `resume_tab`).
- **`check_process_code`:** stack loop also consults `erts_t2_find_blob`.
- **Jettison:** tombstone + lazy stack scan translating live stack CPs to
  `t1_cont` before free.

Everything else (rung-1 side exits, the loop tier, the install gate) is unchanged
and default-off.

<!-- §5-§9 (implementation notes, stress results, parity, verdict, estimate)
     filled from the flagged build. -->

## 5. Implementation notes

Milestone 1 — emit only (`t2_emit.cpp`, `emit_lir_call`, local non-tail call):
```
Label cont = a.new_label();
a.adr(a64::x30, cont);       /* x30 = &landing pad (in blob), adr reaches */
mov_imm(SUPER_TMP, op.target);
a.br(SUPER_TMP);             /* descent; callee prologue stores x30 as CP */
a.bind(cont);                /* callee's `ret x30` lands here             */
reg_cache.invalidate();      /* continuation reloads X/Y homes; result X0 */
/* record {cont, op.t1_pc_cont} in spike_cps for the return-CP table      */
```
Confirmed by disasm (`T2_DUMP=1`): the spike path emits `adr x30, L<cont>;
mov x14,<callee>; br x14` with the continuation ops bound at `L<cont>` and
reachable. `spike_cps` holds `{landing pad, CONT-pctab T1 PC}` per call.

Milestone 2 (hazards #3/#4) adds a per-blob **return-CP table** (a parallel to
`resume_tab`: `{landing_pad_offset → t1_cont}`), teaches `check_process_code`
to consult `erts_t2_find_blob` per stack CP, and adds the tombstone + stack
scan to jettison. See §7.3/§7.4.

## 6. Stress-test method

Synthetic, self-recursive, T2-eligible shapes. Two install drivers, both with
`T2_INSTALL_GATE=0` (these shapes are bare-parity, so the quality gate rejects
them — §0 of the prize memo): `+JT2enable true` (compile-at-load, opt) and the
`erts_debug:get_internal_state({t2_install,M,F,A})` force-install BIF (opt +
DEBUG). Each run compares spike **ON** (`T2_RUNG2_SPIKE=1`) vs **OFF**
(byte-identical demote-on-return baseline). Install and "genuinely spiked" are
confirmed per function via `{t2_installed,M,F,A}` (returns `{addr,size,mfa}`).

The shapes (all genuinely non-tail — the self-call feeds `*`/`|`/`+`, so it
cannot be DCE'd to a tail call; a `_ = N+0` shape *was* silently TCO'd and a
`keep(N)` shape inlined-to-tail — both discarded):
- `build(N) -> [N | build(N-1)]` (cons per ascent frame — GC pressure)
- `raise_at(N) -> N + raise_at(N-1)`; `raise_at(0) -> boom()` where a leaf
  `boom()` raises (the ascent frames are spiked; the raise is at the bottom)
- `m1:rec(N) -> [N | rec(N-1)]`; base tail-calls `blk:block/1` (receive in a
  **separate** module, so `rec` stays eligible and m1's only stack reference is
  the T2 CPs)
- `descend(N,A) -> 1 + descend(N-1,A)` (minimal per-frame work)

Eligibility caveats found the hard way: a `receive`, and a bottom that calls
`error/1` directly, make the function ineligible; the blocking receive and the
raise must be pushed into leaf helpers so the recursive frame stays in the
supported-op set. Depth verified real: `1 + deep(N-1)` grows the stack linearly
(depth 100000 → 100083 words), and spike ON == OFF stack size (the landing-pad
CP occupies the same single RA slot as the demote CP — no stack-layout change).

Validated on **both** the opt JIT emulator and the **DEBUG JIT** emulator
(`FLAVOR=jit TYPE=debug`), the latter with all runtime sanity assertions active
(`erts_validate_stack`, `ErtsGcQuickSanityCheck`, freed-JIT poisoning, and the
spike's own phase-2 "no stack CP remains in the freed span" assert). Booting the
DEBUG JIT build first required registering the pre-existing `t2_tier_queue` lock
in `erl_lock_check.c` (a latent gap in the branch, unrelated to the spike).

## 7. Stress-test results (4 hazards)

All four hazards **PASS** on both the opt and the DEBUG JIT build, every tested
function confirmed `{t2_installed}` and spiked. Summary: `H1=200000`
`H2={deep_boom, spike:boom}` `H3={ok,5000}` `H4={ok,200000}`, with the DEBUG
sanity assertions active and no abort.

### 7.1 Hazard 1 — GC mid-descent/ascent — **PASS**

`build(200000)` (spiked; a cons per ascent frame → minor+major GC during the
ascent with 200 000 live T2 return-CPs on the stack) → **200000**, correct, no
crash, on the DEBUG build with `ErtsGcQuickSanityCheck` active. Empirically
confirms the core GC finding: a T2 return-CP has primary tag HEADER (low bits
0), so the rootset scan treats it as an opaque non-term and never dereferences
it — handled exactly like a T1 CP. **GC needs no changes.**

### 7.2 Hazard 2 — stacktrace / exception deep in the recursion — **PASS**

`catch_deep(2000)` on the spiked `raise_at` (verified `{t2_installed}=true`):
the leaf `boom()` raises `error(deep_boom)` at the bottom, and the exception
unwinds correctly through **2000 persistent T2 frames** to the enclosing
`try/catch` → `{caught, error, deep_boom, {spike,boom,0,[...line 41]}}`. The
innermost MFA is correct. (An earlier attempt used `error/1` *inside* the
recursive function, which is ineligible → ran in T1 and did **not** test T2
frames; the fix pushes the raise into a leaf so the ascent frames are genuinely
spiked.) Confirms exception unwinding is structurally CP-agnostic (pops to the
catch marker, `beam_common.c:580`) across T2 frames, and stacktrace building
resolves T2 CPs to the correct MFA via the blob fallback (`beam_ranges.c:267`).

Note on frames/lines: a self-recursion's frames share one return address and
`gather_stacktrace` records **non-duplicates only** (`beam_common.c:817`), so a
deep recursion contributes a single frame regardless of tier. A blob CP
resolves to MFA with **no line** (`beam_ranges.c:258/270`); line fidelity for
*cross-function* T2 CPs needs a blob-PC→T1-PC reverse map over the CONT pctab
(§2 #4) — a fidelity gap, not a correctness issue.

### 7.3 Hazard 3 — jettison while CPs are live — **PASS**

`m1:rec(4000)` (spiked) blocks in the leaf `blk:block/1` at depth 4000, so
**m1's only reference on the process stack is its 4000 T2 return-CPs** (the
process's `rp->i` and block frame are in `blk`, not m1). Two triggers exercised:

1. **Trace-enable jettison** (`h3(5000)` via `trace_pattern({_,susp_build,2},
   true, [local])`, which jettisons the blob — trace always wins): the two-phase
   CP-retire translates the process's live stack CPs to their T1 continuations
   before the blob is freed; the process resumes, its ascent runs in T1, →
   `{ok,5000}`, no UAF, no assertion failure (the DEBUG phase-2 "span clear"
   assert holds).
2. **Purge path** (the actual UAF hazard): after `code:load_file(m1)` (m1
   current→old; its own blob survives — not dep-jettisoned at reload),
   `check_process_code(P, m1)` returns **true** — detecting the reference *only*
   through the T2 stack CPs (the existing `ErtsInArea` module-range test misses
   them; this is the `beam_bif_load.c:1200` extension firing). `soft_purge`
   correctly returns **false**; after the process finishes and leaves the old
   code, `code:purge` frees it safely. Without the extension this returns false
   → the purger frees code the pending return points at → UAF. **This is the
   spike's single most important positive result: the one genuine hazard is
   closed and the fix is load-bearing.**

### 7.4 Hazard 4 — yield mid-descent then jettison — **PASS**

`h4(200000)`: `descend` (spiked) is spawned, `suspend_process`'d (caught at a
scheduler yield **mid-descent**), the blob is trace-jettisoned, then
`resume_process`'d → **200000**, correct. For a body-recursive shape a
mid-descent yield demotes `c_p->i` to the **T1** entry body (the install entry
stub routes entry-yields through `i_test_yield_shared` with the T1 resume PC),
so `c_p->i` needs no translation and only the stack CPs do — which the CP-retire
handles. The task's "both `c_p->i` **and** stack CPs are blob addresses" case
only arises when a loop-tier back-edge (which parks a blob `c_p->i`) coexists
with a rung-2 non-tail call in the *same* function; the spike does not build
that (body recursion has no recovered loop), so it is called out as an
untested-in-spike combination, not a failure — both translation mechanisms
exist and are proven independently (resume-tab for `c_p->i`, cp-tab for stacks).

## 8. Parity

Call-dominated micro (`count(N) -> 1 + count(N-1)`, N=500000, min of 9 trials,
representative of 3 runs), ns per non-tail call+return:

| | ns/call | vs T1 |
|---|---|---|
| **T1** (not installed) | ~4.15 | — |
| **T2 demote-on-return** (spike OFF) | ~4.28 | +3% |
| **T2 spike** (ascent in T2) | ~4.52 | **+9%** |

Two honest findings, both important:

1. **The demote "mispredict tax" is ≈0 for self-recursion.** T2-demote is within
   ~3% of T1, not the ~3.5 ns the prize memo cites. Every level of a
   self-recursion returns to the *same* continuation address, so the return is
   monomorphic and the indirect-branch predictor (BTB) hits it even without a
   RAS push (the descent uses `br`, not `bl`, so the RAS is unused either way —
   confirmed with the calling-convention analysis). The demote tax the prize
   cites is therefore a **polymorphic-return** (helper / mutual-recursion)
   phenomenon, which a *self*-recursive spike cannot exercise.
2. **The naive spike is a small regression, not a win** (~9% slower than T1).
   Keeping the ascent in T2 at *identity-backend* quality plus the landing-pad
   `reg_cache.invalidate()` reload buys **≤0** — exactly what the prize memo
   predicted ("bare parity buys ≈0"). Disasm confirms the mechanism is real (the
   spike emits `adr x30, L<cont>` and binds the continuation in-blob, vs demote's
   `mov x30, <t1_cont>; br`). The value of rung-2 is **not** parity: it is that
   the ascent becomes *optimizable in T2* (unboxing, guard removal, and above
   all inlining the helper) — the elimination delta the prize is about.

## 9. VERDICT + scoped estimate for the full rung-2 build

### GO — persistent T2 return-CPs on live stacks are tractable.

The spike proved the highest-risk unknown safe on a narrow but genuine case,
under DEBUG assertions, across all four hazards:

- **The scary part is mostly already done.** The read-only CP-walker family
  (stacktrace, `current_function`/`current_stacktrace`, tracing, crash dump,
  match-spec) resolves a T2 return-CP to the correct MFA **for free** via the P2
  `erts_lookup_function_info` blob fallback. Exception unwind and the GC/DEBUG
  stack validators are **structurally CP-agnostic** and needed nothing (verified
  on DEBUG). aarch64 is **RA-only** (no frame-pointer chain), removing a whole
  class of walker complexity.
- **The one genuine hazard — `check_process_code`/purge — is closed** with a
  ~15-line change mirroring the existing `rp->i` hook, and it is **proven
  load-bearing** (§7.3.2).
- **Jettison-while-live is safe** via a tombstone + two-phase stack-scan retire
  that generalizes the shipping `c_p->i` resume-retire, proven on suspended /
  blocked processes at depth with no UAF.

### What was easy vs hard

| Piece | Difficulty | Status |
|---|---|---|
| CP walkers (stacktrace/exception/GC/tracing) | **EASY** — already blob-aware or structural | done, verified |
| `check_process_code` stack-CP recognition | **EASY** (~15 lines) | done, proven load-bearing |
| Return-CP table + emit landing pad | **EASY** (mirror resume-tab) | done |
| Jettison stack-scan (eager, stop-gap) | **MEDIUM** | done for suspended procs; race for *running* procs deferred |
| Production lazy per-schedule-in scan | **MEDIUM** | not built — the eager scan races a running process's own stack write; needs Process scan-gen + schedule-in hook + tombstone set + high-water sweep |
| Line-fidelity blob-PC→T1-PC reverse map | **LOW/MEDIUM**, optional | not built (fidelity only) |
| Framestates + eager-CP-push for **inlined** regions | **HARD** — the real bulk | out of spike scope (self-recursion is 1:1, needs no framestates) |
| Cross-module inliner + elimination scoring | **HARD** — where the *win* is | out of spike scope |

### Remaining risks

1. **The lazy-scan race at scale.** The spike's eager stack scan is correct only
   because the test process is suspended/blocked; a *running* process mutating
   its own stack concurrently with a cross-process scan is a real race. The
   production answer (per-process scan at schedule-in) is well-understood
   (§3, PLAN/T2/06 §5.4) but unbuilt; it is the main lifecycle risk to retire.
2. **Framestate/deopt correctness for inlined regions** — this is rung-2's
   actual complexity and the spike deliberately excludes it (no inlining ⇒
   frames are 1:1 ⇒ existing EFFECT/ERROR/CONT pctab entries suffice). The
   CP-on-stack risk this spike retired is *orthogonal* to and smaller than the
   framestate risk.
3. **Polymorphic-return CPs.** The spike only did self-calls (monomorphic
   returns). Helper inlining puts *distinct* blob CPs on the stack; the walkers
   handle them (MFA-correct), but the mispredict economics and the line-fidelity
   gap only bite there — to be measured when the inliner exists.
4. **No win at parity.** Rung-2 must be driven by the inliner/elimination; a
   parity-only build is a ~9% regression (§8). This is a scoping risk, not a
   correctness one, and matches the prize memo.

### Scoped estimate for the full rung-2 + inliner build

Ranges assume one engineer familiar with the tier; the spike removes the
scariest unknown (is CP-on-stack even safe? **yes**) from the front of it.

- Productize the CP-on-stack lifecycle (lazy per-schedule-in scan, high-water
  sweep, `check_process_code` + line reverse-map, identity-suite coverage):
  **~2–3 weeks.**
- Framestates + eager-CP-push for inlined regions (the `parent_fs` chains,
  uniform deopt stubs, deopt validators): **~4–6 weeks.**
- Cross-module inliner + elimination scoring + loop-recovery-after-inlining:
  **~4–6 weeks.**
- Integration, the full identity-transform regression suite, and measurement on
  dialyzer/compiler-class: **~2–4 weeks.**

**Total rung-2 + inliner ≈ 12–19 weeks (3–5 months)** — consistent with the
plan's "P3-scale, multi-week, no shortcut" framing. The de-risk verdict is that
this investment is **not gated on an unsolved runtime-safety problem**: the
CP-on-stack lifecycle is tractable and largely prototyped here; the remaining
cost is the inliner and framestate machinery that deliver the actual win.

## Appendix — what was implemented (the flag)

Behind `T2_RUNG2_SPIKE` (env, default off; when off, byte-identical to the
shipping demote-on-return):

- `t2_emit.cpp` `emit_lir_call`: non-tail **local** call pushes an in-blob
  landing pad as the return CP (`adr x30, cont; br callee; bind cont;
  reg_cache.invalidate()`) instead of `mov x30, <t1_cont>; br`. Records
  `{landing pad, CONT-pctab T1 PC}` in `spike_cps`.
- `T2EmitResult::cp_points` → `t2_compile.cpp` → `erts_t2_install`
  (`cp_points`/`cp_count`) → `ErtsT2Blob.cp_tab` (`t2_ranges.{h,c}`:
  `ErtsT2CpTab`, `erts_t2_blob_set_cptab`, `erts_t2_cp_translate`).
- `beam_bif_load.c` `check_process_code`: per-stack-CP `erts_t2_find_blob` check.
- `t2_install.c`: a cp_tab blob keeps its range registered and jettisons through
  a two-phase barrier retire (`t2_cp_retire_phase`) that rewrites every in-span
  stack CP to its `t1_cont` before free; phase-2 asserts the span is clear.
- `erl_lock_check.c`: register `t2_tier_queue` (debug-build boot fix).
