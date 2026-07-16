# Monomorphic map-shape specialization in T2 (S1) — plan

Planning doc, 2026-07-16. The one lever the sampling evidence (doc
[`../22_where_time_goes_sampling.md`](../22_where_time_goes_sampling.md))
supports: turn the generic O(k) flatmap key scan behind
`get_map_element` / `put_map` into an **O(1) shape-guarded indexed
access**, baked as a T2 speculation and deopting to T1 on a shape miss.
This is doc [`../19`](../19_general_tier_path.md) S1, informed by the
`../otp-ideas` inline-cache prototype (IDEA 06).

## 0. The prior on this (read before building)

The measured pool is real: map access + `erts_maps_put` + key-comparison
`eq` are the top hotspots (doc 22 §3 — mapwl `erts_maps_put` 27.6%,
`i_get_map_element_shared` 5.4%; compiler `eq` 12% + map reads ~8%).
**But two independent lines of evidence say the win is uncertain and must
be measured before the machinery is built:**

1. **The inline-cache prototype (`lukas/erts/implement-inline-caches`,
   IDEA 06) proved hit-*rate*, never throughput.** ~90% hit on Elixir
   struct access, ~35% on compiler (polymorphic build-up). Its own
   `CONCLUSIONS.md`: a per-access **C-helper** IC in the interpreter is
   not obviously faster than the scan it replaces; the win, *if any*, only
   comes from moving the shape guard into **JIT-emitted code**. That step
   was never done, and flatmaps are tiny (≤32 keys, often ≤5), so even the
   inlined guard may not beat a 5-compare scan.
2. **T2's own "eligible ≠ win" record** (memos 14/17/20) and doc 22 §5
   (`sets:is_element` regressed 2.3× under forced T2) say admission of map
   code does not imply speedup.

**Discipline (M0): S1a is a hand-specialized micro-measurement, not a
build.** If a hand-baked monomorphic `get_map_element` does not clear a
throughput bar on a realistic (small) flatmap, we stop — the honest
outcome is "reads not worth it." No profiler, no productization until the
win is a number.

## 1. Why T2 is the right home (vs the T1/interpreter IC)

The IC prototype's make-or-break step (JIT-inline the guard) is *native*
to T2, and T2 improves on it in three ways:

- **Bake the shape as a speculation, not a runtime side-table.** The IC
  used a per-scheduler set-associative table indexed by the site pointer,
  looked up on every access. T2 bakes the profiled shape token + resolved
  index as **blob constants** and emits a single compare — no table, no
  hash, no per-access lookup. This is strictly less work than the IC's own
  fast path.
- **Deopt instead of a disable state machine.** A shape miss **side-exits
  to T1** (re-execute the erased `get_map_element`), reusing the exact
  mechanism `maps:fold` already uses (`T2_OP_SPEC_CALLSITE`,
  `spec_deopt_pc`, re-execute-the-erased-op). No `DISABLED`/recache
  bookkeeping in the fast path.
- **Sidesteps the prototype's unsolved leak.** The IC's global shape-intern
  table leaks for VM life (nothing reclaims it). T2's first cut guards on a
  **module-literal** keys-tuple pointer (structs/records/`%{k=>v}` literals
  share one), which is long-lived by construction — no interning, no
  reclamation problem. General interning is deferred to S1d and only if
  measured to add hits.

## 2. What already exists (reuse, do not rebuild)

From the `maps:fold` Stage-1 work (`census/mapsfold_design.md`), T2
already has the flatmap machinery:

- **HIR/LIR ops:** `IsFlatmapBounded` (boxed ∧ subtag==FLATMAP ∧ size≤32
  guard), `FlatmapSize`, `FlatmapKeyAt`, `FlatmapValAt` — with emitters
  (`emit_lir_flatmap_at`, `t2_emit.cpp:943`) and the flatmap byte layout
  (values inline at `+24+8*i`, keys tuple at `+16`, size at `+8`).
- **The deopt model:** `T2_OP_SPEC_CALLSITE` + `spec_deopt_pc` →
  re-execute the erased op at its T1 PC with the boundary sync map. Sound
  because the fast path is effect-free until commit.
- **`get_map_element` isel/emit** (`t2_emit.cpp:856`), today: flatmap/HAMT
  inline for immediate keys, generic C call for boxed keys — i.e. the
  **scan**, not a shape-guarded jump.

The gap is exactly two things: a **shape token + resolved index** to guard
on, and the **profiler** to supply them.

## 3. Design — the read path (S1a/S1b)

**Shape token.** For a flatmap, `mp->keys` is a boxed pointer to the sorted
key tuple. Two maps have the same key-set iff their key tuples have equal
content; for maps built from the **same source literal** the pointer is
*identical*. So the S1 shape token is the **keys-tuple pointer**, and the
guard is a pointer compare — valid without interning **for literal-shaped
sites** (Elixir `%Struct{}`, Erlang record-as-map, `#{a=>_,b=>_}` literals
threaded and read). That is exactly IDEA 06's ~90%-hit class.

**Profiling (S1b).** Extend the T1 profile (the entry-type profiling seam,
tasks #1a/#1b) with a per-`get_map_element`-site slot: `{keys_ptr, index,
arity, hits, misses}`, captured by the existing shared helper on the T1
path. Monomorphic if one `keys_ptr` dominates (≥ threshold, e.g. 90% over
N samples) **after warm-up** (IDEA 06 step 4 — do not sample the transient
shapes seen while a map is being built).

**Codegen.** At a monomorphic site, bake `shape_ptr` (the dominant
keys-tuple pointer, a module literal → held live by the blob's literal
refs) and `index`; emit:
```
IsFlatmapBounded(Map)            -> b_fast | b_deopt      ; existing guard
b_fast: cmp Map.keys, #shape_ptr -> eq ? | b_deopt        ; one pointer compare
        v = FlatmapValAt(Map, #index)                     ; existing op, constant i
        ... commit v ...
b_deopt: side-exit to T1 get_map_element PC (re-execute)  ; T2_OP_SPEC_CALLSITE
```
No key scan, no `eq`, no C call on the fast path. The miss path is the
unmodified T1 instruction — non-flatmap, wrong-shape, and boxed-key maps
all simply run at T1 speed minus one guard.

**Why this also drains `eq`.** doc 22 §3: the compiler's 12% `eq` is
mostly map-key comparison *inside* `get_map_element`. The shape-guarded
path performs zero key comparisons on a hit, so a hit reclaims the
`FlatmapValAt`-equivalent read **and** its share of `eq`.

## 4. Design — the write path (S1c, gated separately)

`erts_maps_put` is the bigger raw pool (mapwl 27.6%) but splits into two
cases that behave oppositely, and **we must measure which dominates before
touching it**:

- **Shape-preserving update** (`Map#{existing_key := V}` / record update):
  copy the map, overwrite one value slot — no scan, no rehash, shape token
  unchanged. This is monomorphic and high-value. Partly landed already
  (`update_record`, `da77151dda`); the S1c generalization is the same
  emit under a profiled shape guard for `put_map_exact` / single-key
  `put_map_assoc` on an existing key.
- **Map building** (`Map#{new_key => V}`, `maps:put` adding keys): every
  put changes the shape; there is **no monomorphic shape** to guard, and
  IDEA 06 measured exactly this as the low-hit (~35%) compiler case. Not
  an IC target. If mapwl's 27.6% is dominated by building (likely, given
  `rec_to_map`/`decode` construct maps), the write-side win is small — a
  key S1c go/no-go input.

So S1c is **conditional on a census split of `erts_maps_put` into
update-vs-build** (a cheap addition to the perf/annotate work already set
up), then only the update half is specialized.

## 5. Staging + gates

| stage | what | gate / kill |
|---|---|---|
| **S1a** | Hand-bake a monomorphic `get_map_element` (fixed shape+index) in the blob for a struct-field microbench; `timer:tc` vs generic T1, flatmap sizes {3,5,8,16,32}. **No profiler.** | **Throughput win ≥ ~1.3× at size ≤ 8** (the realistic struct case), and never-slower. Miss → **STOP; reads not worth it** (keep the negative, like doc 22 §5). |
| **S1b** | If S1a wins: the shape profiler (entry-type seam extension) + monomorphic-site detection + real codegen; validate byte-identical vs T1 across hit/miss/non-flatmap/boxed-key. | Real Elixir/struct workload shows the microbench win survives; `maps_SUITE` green; deopt-storm counter flat on polymorphic sites. |
| **S1c** | Census-split `erts_maps_put`; if the update half is material, specialize shape-preserving single-key updates. | Update-half share ≥ threshold **and** measured write win; else skip. |
| **S1d** | Only if S1b hits are capped by *non-literal* same-shape maps: general shape interning (IDEA 06's intern table) **with** a solved reclamation model (its open leak). High cost — fund only against a measured hit-rate ceiling. | A measured hit-rate gain that converts to throughput. |

## 6. Risks (from IDEA 06 + doc 22)

- **Small maps → no win.** The dominant risk; S1a exists precisely to kill
  the project cheaply if the ≤5-key scan is already as fast as a guarded
  load. This is the prototype's unresolved question, moved to the front.
- **Polymorphic sites → deopt storms.** Monomorphic-only + deopt (no PIC —
  IDEA 06 measured PIC at 5–11%, dropped; compiler sites see 20–130+
  shapes). Needs a per-site deopt counter and self-disarm (precedent: the
  P2 counter self-disarm, `project_t2full_p2_waveD`).
- **Shape lifetime / purge.** S1 guards on module-literal pointers (blob
  holds the literal ref; purge of that module deopts/evicts the blob via
  the existing dep mechanism). The general (interned) case reopens IDEA
  06's leak — deferred to S1d, not on the S1a/b path.
- **Warm-up.** Sample shapes only after fill, or the profiler caches the
  transient build-up shapes and picks a wrong monomorphic token.

## S1a result (measured 2026-07-16) — QUALIFIED GO

`census/mapscan.erl`: `#{K := V} = M` (compiles to `get_map_element`) with
K at scan-position 0 (found immediately — the shape-guard ceiling) vs the
far end of the scan, integer keys 1..N so term order == position, T1,
`timer:tc` min-of-3, ns/access.

| flatmap size | near (spec ceiling) | far (worst pos) | per-key scan | far-key win | uniform-mean win |
|---|---|---|---|---|---|
| 3 | 1.35 | 1.45 | 0.049 | 6.8% | 3.5% (1.04×) |
| 5 | 1.35 | 1.78 | 0.109 | 24.5% | 14.0% (1.16×) |
| 8 | 1.37 | 2.71 | 0.190 | 49.3% | 32.7% (**1.49×**) |
| 16 | 1.38 | 5.04 | 0.244 | 72.7% | 57.1% (**2.33×**) |
| 32 | 1.33 | 8.68 | 0.237 | 84.7% | 73.4% (**3.76×**) |

**Reading.** The scan is genuinely linear (~0.2 ns/key) and the
found-immediately cost is flat ~1.35 ns at every size — so a
position-independent shape-guarded load floors *every* access at ~1.35 ns.
The saving is `(cost at the key's scan position) − 1.35 ns`.

- **Clears the ≥1.3× gate for flatmaps ≥ 8 keys** at uniform-mean access
  (1.49× at 8, 2.33× at 16, 3.76× at 32); worst-position (a hot field
  scanned late) reaches 1.9×/6.5× at 8/32.
- **Marginal for ≤ 5 keys** (1.04–1.16×) — IDEA 06's "often ≤5" caveat
  bites here; do not expect a win on tiny maps.
- forced-T2 today pays the same scan (near-identical numbers), so the
  specialization improves the T2 path equally.

**Caveats (S1a is a necessary, not sufficient, condition).** It measures
the read scan cost in isolation and takes the near-end cost as the spec
ceiling (the guarded path does the same boxed/flatmap checks + one
pointer compare + one indexed load, so this is a fair — slightly
conservative — ceiling). It assumes uniform key-position access and does
not prove the T2 codegen hits the floor (guard/deopt overhead). Those are
S1b's job. But the necessary condition — *there is real scan cost worth
removing* — is **met for medium+ flatmaps**.

**Disposition: proceed to S1b**, scoped to flatmaps and effectively
size-gated (the profiler should not specialize sites whose maps are
mostly ≤5 keys — no headroom). Reads first; S1c (writes) still gated on
the `erts_maps_put` update-vs-build split.

## S1b.1 result (measured 2026-07-16) — codegen ceiling PROVEN

Does the *emitted* shape-guarded indexed load actually beat the scan (S1a
measured the scan cost in isolation; S1b.1 measures real T2 codegen)? A
throwaway env-gated emitter hack (`T2_MAP_SPEC_IDX=<i>` in
`t2_emit.cpp:emit_lir_get_map_element`) replaces the scan fragment with
`IsFlatmapBounded` + a fixed `values[i]` load; `census/mapspec.erl` reads
key 1 (flatmap index 0) so `T2_MAP_SPEC_IDX=0` exercises it. Installed on
one MFA via `erts_debug:get_internal_state({t2_install,mapspec,l,3})`
under `T2_RETAIN=1` (system stays T1 — the hack is globally unsafe, it
reads index 0 for *every* map). ns/access, `timer:tc` min-of-3:

| flatmap size | T1 scan | T2 scan frag | **T2 spec load** | spec vs T1 |
|---|---|---|---|---|
| 3  | 1.34 | 1.55 | **0.68** | 2.0× |
| 5  | 1.77 | 1.92 | **0.67** | 2.6× |
| 8  | 2.42 | 2.79 | **0.67** | 3.6× |
| 16 | 4.87 | 5.13 | **0.67** | 7.3× |
| 32 | 9.13 | 8.58 | **0.67** | 13.6× |

**Reading.** The specialized load is **flat at ~0.67 ns at every size** —
the O(1) signature — while the scan grows linearly. Result column matched
baseline (correct value returned). It clears the 1.3× gate at *every*
size, including tiny maps (2.0× at size 3), and beats S1a's own ~1.35 ns
ceiling because the inline load skips the fragment *call* that S1a's
"near" column still paid.

**This is the floor.** The probe omits the shape guard (the `ldr keys;
cmp #shape; b.ne deopt` pointer check) to isolate the load. The real
specialization adds ~1 compare (~0.2–0.4 ns) → true ceiling ~0.9–1.0 ns,
still flat, still crushing the scan from size 5 up. The **necessary**
condition (codegen headroom exists) is settled; the **sufficient**
condition (real-site hit rate) is S1b.2.

## Recon corrections to §§1,3 (before building S1b.2)

Three facts from the S1b implementation recon override the sketch above:

1. **Deopt class is `T2_OP_SPEC_BOUNDARY` → `ERTS_T2_PC_EFFECT`, NOT
   `T2_OP_SPEC_CALLSITE`.** §§1,3 say CALLSITE (copied from `maps:fold`);
   that is for erased *calls*. `get_map_element` is an effect op —
   re-execute *this* PC (`ERTS_T2_PC_EFFECT`), not a call PC. Every
   CALLSITE reference in this doc should read BOUNDARY/EFFECT.
2. **`get_map_element(s)` has NO pctab entry today — a blocking gap.** The
   deopt needs `t2_pc_classify` (emit side, `beam_asm_module.cpp:733`) +
   a decode-side mirror in `t2_pctab.c` to register an EFFECT PC for the
   op; the specializer must refuse a site if
   `erts_t2_pc_lookup_kind(...ERTS_T2_PC_EFFECT)==0`. Build this first.
3. **The existing profiler is per-function-entry, not per-site.**
   `ErtsT2Profile` (tasks #1a/#1b, X0–X3 sampling at entry) cannot carry a
   per-`get_map_element` shape. S1b.2 needs a *new* per-site block keyed by
   `beam_idx`, a T1 capture hook in `emit_i_get_map_elements`
   (`instr_map.cpp:436`, right after `mp->keys` is loaded), and a
   `beam_idx`-keyed read at compile via a `T2FactSource` extension.

## Key de-duplication — decision (no, by design)

The guard is a single-word compare of `map->keys` against a baked token,
so it only **hits** when same-shape maps *share* the keys-tuple pointer.
Global key de-dup (interning keys tuples so structurally-equal key sets
become pointer-equal) is the lever that converts structural monomorphism
into the pointer monomorphism the cheap guard exploits. **We are not
building it**, because pointer identity already captures the dominant
map-as-struct lifecycle:

- **Literal-built maps** share one keys tuple within a module (literal
  pool); a fixed constructor hands out the same pointer every call.
- **Update-derived maps** — shape-preserving update (`M#{k := V}`, key
  present) **reuses** the keys tuple; only a key-*adding* put builds a new
  one. "Build once, update/read many" keeps a stable shape pointer.

Independently-constructed same-shape maps (separate `maps:from_list`
sites, cross-module) get different pointers → guard misses → deopt to the
T1 scan (correct, unaccelerated). De-dup is deliberately avoided because
it *is* IDEA 06's global intern table — reintroducing its shape-
reclamation leak plus a hash+probe cost on every map construction, paid
even by non-struct maps; and no cheaper token exists (comparing contents
or hashing is O(k), i.e. the scan). **Honest risk:** IDEA 06's ~90%/~35%
hit rates were measured *with* interning, so the pointer-identity hit rate
is unmeasured — that is exactly the S1b.2 gate. De-dup stays a fallback
lever (see S1d), gated on a measured hit-rate shortfall, and if built at
all would be scoped (e.g. compiler-literal shapes) not a global table.

## S1b.2 result (measured 2026-07-16) — pointer-identity thesis VALIDATED

The de-dup decision rests on: *shape-preserving updates of a literal base
share the base's keys pointer; independent builds don't.* Tested directly
with a new `{map_keys_ptr, Map}` debug probe (flatmap keys-tuple pointer
as integer; `erl_bif_info.c`) via `census/mapshape_probe.erl`:

| idiom | shares base keys ptr? |
|---|---|
| `B#{a:=1}`, `B#{b:=2}`, chained updates | **yes** |
| `maps:update` / `maps:put` on an existing key | **yes** |
| constructor fn `(base())#{a:=A,b:=B,c:=C}` (any values) | **yes** |
| the *same literal* at another call site / another fn | **yes** |
| `maps:from_list/1` (×2, even identical content) | no (distinct) |
| put a *new* key `B#{d=>4}` (reshape) | no (distinct) |

Two findings **stronger** than the thesis:

1. **The compiler dedups identical map literals across the module's whole
   literal pool** — `#{a=>0,b=>0,c=>0}` at any site in a module shares one
   keys tuple. So the Elixir struct pattern (`%Foo{}` = `__struct__`
   literal + shape-preserving updates) is pointer-monomorphic with **zero
   interning**. (Correctness-safe regardless: a distinct pointer just
   deopts.)
2. **The only distinct case is dynamic construction** (`from_list`), which
   is IDEA 06's low-hit compiler case — low-hit because those sites see
   *20–130+ genuinely different shapes* (IDEA 06 §), which **interning
   cannot rescue either**.

**De-dup is unnecessary from both ends:** the high-value struct pattern
already shares pointers; the low-value polymorphic sites are polymorphic
in *shape*, not merely in pointer. This closes the S1d question: de-dup is
not just deferred, it is **measured-unnecessary** for the case that has a
win. What remains uncertain is purely *workload mix* — struct-heavy code
(Elixir apps) hits; dynamic-map-heavy code (the OTP compiler) does not —
exactly IDEA 06's 90%/35% split, now explained as a map-usage-style
property, not an interning artifact.

## Staging update (2026-07-16)

- **S1a** — DONE, qualified GO (scan cost real, ≥1.3× at size ≥ 8).
- **S1b.1** — DONE, codegen ceiling PROVEN (flat ~0.67 ns, 2.0–13.6×).
- **S1b.2** — DONE, pointer-identity thesis VALIDATED; **de-dup dropped**
  (measured-unnecessary, not merely deferred). S1d struck.
- **S1b.3 (productization) — FUNDED, in progress.** Sub-pieces:
  - **3a — pctab EFFECT entry for `get_map_element` — DONE.** Emit side
    classifies the three `i_get_map_element*` specific ops
    (`beam_asm_module.cpp`); decode side classifies raw
    `genop_get_map_elements_3` (`pctab_is_effect`, `t2_pctab.c`). Verified:
    live reads zip 1:1 (resolved `beam_idx`); a dead read (raw decode
    counts it, DCE drops it from emit) trips the count-inexact fail-safe
    (BEAM_IDX_UNKNOWN → not specialized), same as the pre-existing gc_bif
    mismatches. emit ⊆ decode always, so no wrong-order-matching-count
    hazard.
  - **3b — per-site shape profiler** (`beam_idx`-keyed, T1 capture hook at
    `instr_map.cpp:436`) — next.
  - **3c — shape-guarded codegen** wired to 3b's facts + gated on 3a's
    pctab; miss → `T2_OP_SPEC_BOUNDARY`/`ERTS_T2_PC_EFFECT` deopt. Replaces
    the throwaway `T2_MAP_SPEC_IDX` probe.
  - **3d — validation** (byte-identical hit/miss/non-flatmap/boxed-key;
    `maps_SUITE`; deopt-storm self-disarm) + measure vs the S1b.1 ceiling.

  Payoff is concentrated on Elixir-struct workloads (T2 is a landed
  specialist tier; the OTP corpus is dynamic-map-heavy).
- **S1c** — unchanged (writes gated on `erts_maps_put` update-vs-build
  split). **S1d (de-dup)** — STRUCK (measured-unnecessary above).

## 7. First action

S1a only: a throwaway hand-baked specialization + microbench, to get the
one number the whole line hinges on. No enum changes shipped, no profiler,
no productization until that number clears the bar. If it doesn't, this
doc's outcome is "measured no on reads," consistent with the project's
measure-before-build discipline.
