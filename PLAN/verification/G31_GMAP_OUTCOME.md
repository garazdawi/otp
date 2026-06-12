# G3 subject 1 (branchy dispatch) + G-map — outcomes

The two remaining gates from
[`../T2/08_v1_loop_tier.md`](../T2/08_v1_loop_tier.md) §6.1, run
2026-06-12 with the established methodology: env-gated hand-written
emitters (`T2_G31`, `T2_GMAP`) on the MVP scaffolding, single build,
correctness hashes compared across modes, engagement proven (via
`+JDdump` for the null result; via the reproducible effect for the
positive one).

## G3 subject 1 — branchy dispatch: NULL

Subject: a 12-clause gen_server-shaped dispatcher
(`t2_g31:dispatch/2` — tuple-shaped messages with integer guards,
85 % `{get,I}` / 10 % `{put,I,V}` / 5 % `{incr,I}` mix) and the
same shape inside a real `gen_server` callback
(`t2_g31:handle_call/3`). The T2 bodies implement cold-arm pruning
exactly as `04` §10.3 designs it: only the hot `{get,I}` arm,
guards fused into one straight-line sequence (boxed → arity-2 →
atom `get` → smallint → bounds → 10-tuple state → direct element
load), all eleven cold arms side-exiting to the full T1 dispatch.
`handle_call/3` additionally allocates the `{reply,V,State}` tuple
(GC-test first).

| metric | T1 | T2 | ratio |
|---|---|---|---|
| isolated dispatch, 100 k mixed msgs (in-module loop) | 394 µs | 385 µs | 1.02× |
| `gen_server:call` roundtrips, 50 k mixed | 29 073 µs | 28 757 µs | 1.01× |
| pure-get / pure-put probes (external-call harness) | — | — | ±1–4 % (noise) |

Engagement verified: the `+JDdump` output contains both T2 bodies
and the hook branches; the specialized code runs and simply isn't
faster. Two reasons, both structural:

1. **T1's clause dispatch is already a few cycles.** 100 k mixed
   dispatches in 394 µs ≈ 4 ns ≈ 14 cycles for the *entire*
   12-clause select (`is_tuple` → `select_tuple_arity` →
   `select_val` jump table → guards → body). The hand-fused arm is
   the same handful of cycles. As with G3 subject 2's calls,
   BeamAsm's control-flow machinery leaves no per-op pool for an
   OoO core to recover.
2. **The gen_server wrapper outweighs dispatch 40:1.** A roundtrip
   is ~580 ns of message passing, scheduling, and reply plumbing —
   `07` §19's "the wins live in the runtime" holds for callback
   dispatch.

**Gate disposition: G3 is now closed negative on both subjects.**
Branch-frequency counters, cold-arm pruning, monomorphic-target
slots, and the general-inlining infrastructure (framestates,
eager-CP, CP/stack-scan lifecycle) stay deferred with two
independent negative results. `00` §1's framing — that today's
corpus wins would come *predominantly* from cold-arm pruning and
guard elimination on branchy code — is empirically refuted; the
measured wins live in data-access fusion instead (G-bin, G-map,
the MVP).

## G-map — flatmap shape specialization: PASS, 1.64×

Subject: `t2_gmap:sum_scores/2` — a recovered self-recursive fold
over 100 k struct-like 5-key flatmaps (instances created by
updating a literal template, sharing its keys tuple — the Elixir
struct shape), reading two fields per map. The T2 body: MVP-style
spine; per map a **shape guard** (flatmap header, size 5, two
key-slot compares) then **direct offset loads** of both values; the
usual fused boolean/smallint/overflow handling; side-exit to T1's
generic `get_map_elements` for any other shape (bigger maps,
hashmaps, missing keys, non-maps — all in the correctness corpus).

| metric | T1 | T2 | ratio |
|---|---|---|---|
| 10 × 100 k-map fold (alternating runs, min) | 1751–1822 µs | 1070–1170 µs | **1.64×** |

Correctness hash identical across modes (incl. hashmap/badmatch/
improper-list/bignum side-exit paths).

Two findings beyond the headline:

1. **Flatmap key order is atom-index-dependent — a trap worth its
   own record.** The first emitter version guarded key positions
   probed in a *different VM* of the same build; the loaded
   literal's order differed (`score` at index 3, not 4), every
   element failed the shape guard, and the workload ran **1.45×
   slower** than T1 (guards + full T1 re-execution per element).
   Production consequence, now explicit in the emitter comment and
   here: shape guards must test the **keys-tuple pointer recorded
   by runtime feedback** (exactly the hash-consed shape pointer of
   `02` §7.6), never key positions assumed at codegen time. The
   episode also incidentally measured the mis-speculation regime:
   bounded, correct, ~1.45× — which a production exit counter
   (`03` §9.5) would jettison promptly.
2. **The win compounds with region size.** Two fields cost one
   shape guard here; an Elixir-struct pipeline reading five fields
   across a region amortises the same guard further — 1.64× is the
   two-field floor, not the ceiling.

**Gate disposition: G-map PASSED.** Phase-5-style map coverage
(map-shape feedback slots + region-level shape specialisation,
`02` §7.6) joins `bs_*` coverage in the green-lit expansion set,
behind G-bin in priority (1.64× on access-heavy folds vs 5.6× on
scans; both real).

## The completed gate picture

| experiment | mechanism tested | result |
|---|---|---|
| MVP | loop recovery + elimination-rich leaf inlining | **2.0×** |
| G-bin | binary scan fusion (match-context dance) | **5.6×** isolated, 6–10 % e2e json |
| G-map | flatmap shape guard + direct loads | **1.64×** |
| G3-2 | call-crossing / overhead-only inlining | null (0 ± 1 % CPU) |
| G3-1 | branchy dispatch / cold-arm pruning | null (1.01–1.02×) |

The synthesis the plan should carry forward: **T2's wins come from
fusing data access inside loops — binaries, maps, tuples/lists —
not from optimizing control flow or calls, which BeamAsm already
handles at near-OoO-floor cost.** The v1 loop tier plus the G-bin
and G-map expansions is the evidence-backed scope; everything
gated on G3 stays shelved.

## Reproduction

```bash
make -j$(sysctl -n hw.ncpu)
./bin/erlc -o /tmp/g3 PLAN/verification/t2_g31.erl PLAN/verification/t2_gmap.erl
for v in "" "T2_G31=1 T2_GMAP=1"; do
  env $v ./bin/erl -noshell -pa /tmp/g3 \
      -eval 't2_g31:check(), t2_gmap:check(), t2_g31:bench(11), t2_gmap:bench(11), halt().'
done
# engagement: T2_G31=1 erl +JDdump true ... ; grep "T2 body" t2_g31.asm
```
