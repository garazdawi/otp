# T2 — Profiling Infrastructure

> **v1 scope rescoped by [`08_v1_loop_tier.md`](08_v1_loop_tier.md)
> §4.1/§4.4**: v1 keeps only the call counter and the function-entry
> type slots (§7.1–7.4); arith/call-return/switch sites (§7.3 items
> 2–4), the monomorphic-target slot (§7.5), map-shape feedback
> (§7.6) and branch counters (§7.7) are deferred behind their
> consumers' gates, and §7.8's SSA chunk is replaced by building IR
> from the loaded BEAM code (§7.8 stays as the G1 fallback design).
>
> Part of the T2 design. See [`README.md`](README.md) for the full
> document index. This file covers §7: the T1 profiling changes
> that produce the data T2 consumes — eligibility check, type
> feedback vector, where to profile, call counter, per-call-site
> monomorphic-target slot, map-shape feedback, branch-frequency
> counters, and the SSA-in-BEAM-file emit. Compilation and
> speculation that consume this data live in
> [`03_compilation_and_speculation.md`](03_compilation_and_speculation.md).

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

> **[Decision reversed.]** The G-map experiment priced this win
> (1.64× on struct-shaped access) and the T1 inline-cache route is
> now **Track A item A2** in `08_v1_loop_tier.md` §8 — a T1-only
> change that works on existing beams, with the T2 region
> specialisation as the later compounding layer. The paragraph
> above records the original (pre-measurement) reasoning.

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

