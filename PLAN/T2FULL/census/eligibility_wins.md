# T2 eligibility-widening — cheap high-coverage frontend wins

> **STATUS (2026-07-16): this is the pre-implementation recon. For what
> actually landed and current coverage, see
> [`../21_eligibility_exceptions_status.md`](../21_eligibility_exceptions_status.md).**
> WIN 1 (maps is_map/get_map_elements) + WIN 4 (badrecord) landed; the
> mandate's bs-position/maps/exceptions legs are done. WIN 2 (call_fun
> standalone) and WIN 3 (guard-BIF subset) remain; bs_position was NOT
> deferred after all (byte-aligned scan + cursor-IV landed).

Recon (opus, read-only) against the S0 census + `t2_eligible.c`/`t2_hir_builder.cpp`/
`t2_isel.cpp`/`t2_emit.cpp`. Goal (the mandate's step 5): the lowest-effort changes
that make T2 ELIGIBLE on the most additional real Erlang/Elixir functions. Widening
eligibility ≠ speedup (memos 14/17/20, re-confirmed by the survey's forced-T2
regressions) — speed comes from P2/P3 + map-shape specialization. This phase buys
coverage; frame the speed as separate.

## Three gates (all in t2_eligible.c)
1. `erts_t2_genop_supported` — buildable set (every body op must be in it).
2. `erts_t2_genop_build_only` — {call_fun_1, call_fun2_3, is_function2} build but have
   NO isel lowering → stripped from the install bitmap (P1 chain-callee only).
3. isel must lower every emitted HIR op (`t2_isel.cpp` default = fail the compile).
A real win per op = builder case + isel lowering + emit (reuse a T1 fragment) +
eligibility edit. NOTE: P1c-2 gives ZERO reuse for map ops — it degrades them to
`Opaque` (classification-only), it does not decode them.

## Census marginal (sole-blocker own-time), from RESULTS.txt
| class | json | mapwl | compiler |
|---|---|---|---|
| bs_position | 31.1% | 0 | 0 |
| call_fun | 18.0% | 0 | 18.1% |
| maps | 0 | 19.6% | 12.4% |
| general_bif | 0 | 0 | 1.4% (gross 24.8% json / 8.5% compiler — entanglement) |
badmatch/case_end/if_end, comparison bif2, and gc_bif1/2/3 arithmetic are ALREADY
supported.

## Prioritized plan (ranked coverage/effort)

### WIN 1 — map matching: `is_map` + `get_map_elements`  (TOP PICK)
Biggest real-app function-count lever: Elixir structs are maps → every struct/`%{k:v}=arg`
head is `is_map`+`get_map_elements`; config/decoder/router code. Read-only, no alloc,
no trap → NO GC sync map; fails by branching to a clause-select fail label (same shape
as existing gc_bif / bs_start_match3). Census maps-marginal 19.6% mapwl / 12.4% compiler.
- **`is_map` (SMALL):** builder — clone `is_tuple` (`translate_type_test(T2OpKind::IsMap,…)`,
  hir_builder ~1583); isel — ALREADY wired (`guard_kind` IsMap→T2LirKind::IsMap,
  isel:111); emit — add IsMap case to `emit_lir_guard` (t2_emit ~1170-1244) calling T1
  `emit_is_map` (arm/instr_common.cpp:1394); eligibility — add `genop_is_map_2`, drop
  from `ERTS_T2_BLK_MAPS` (t2_eligible:551).
- **`get_map_elements` (MEDIUM):** builder — new case decoding `genop_get_map_elements_3`
  (Fail, Src, {Key,Dst}-list; command-list decode like bs_match ~1879; Succeeded/Branch
  shape like bs_start_match3 ~1842), one dst/key; isel — new lowering + T2LirKind (no
  GC/alloc, only the {f} fail edge); emit — reuse T1 `emit_i_get_map_element`/
  `emit_i_get_map_elements` (arm/instr_map.cpp:362/391; C `beam_jit_get_map_elements`,
  `erts_maps_get` erl_map.c:186); eligibility — add op, drop from BLK_MAPS.
  CAVEAT: generic helper = eligible-but-flat (same lookup T1 does); the SPEED needs
  monomorphic-shape→fixed-offset (memo 20 §4; flatmap Stage 1 isel/emit at isel:1328-1362
  / emit:1618-1690 is a partial template). Land generic eligibility first.

### WIN 2 — `call_fun` standalone install  (highest raw coverage, builder done)
18.0% json + 18.1% compiler marginal. Builder ALREADY decodes call_fun→CallFun; only
missing isel+emit+un-build-only.
- isel — new CallFun case (currently default-fails isel:1401) → generic closure call,
  reuse T1 `emit_i_apply_fun`/`emit_call_fun` (arm/instr_fun.cpp:253/330); needs call
  sync/GC map + deopt PC + reduction charge (CallExt convention exists, isel:973).
- eligibility — remove call_fun_1/call_fun2_3 from `erts_t2_genop_build_only` (:169-170).
- CAVEAT: memo 17 §2.2 — T2 does NOT win from call-overhead removal → eligible-but-flat;
  GC/yield/deopt at the call is the risk surface. High eligibility, uncertain realizable.

### WIN 3 — general_bif guard-BIF subset  (MEDIUM; Elixir map idioms + entanglement)
`element`/`hd`/`tl`/`map_get`/`is_map_key`/`map_size`/`byte_size`/`bit_size`/`node`. GuardBif
HIR op already stubbed (hir.hpp:242, bif_num reserved). T1 fragments exist
(arm/instr_guard_bifs.cpp: emit_bif_element:579, _hd:773, _tl:1266, _map_get:854,
_is_map_key:799, _map_size:947, …).
- builder — decode non-comparison bif1_4/bif2_5 → GuardBif (fail-edge like comparison bif2
  ~1774), MFA allowlist mirroring `t2_bif2_op_supported`; isel/emit — lower GuardBif →
  matching T1 fragment per MFA; eligibility — new guard-BIF predicate (t2_eligible:356),
  drop covered ops from `ERTS_T2_BLK_GENERAL_BIF` (:613).
- BONUS: also routes the currently-unlowered non-arith `gc_bif` results (map_size/
  tuple_size/length/byte_size/abs → T2OpKind::Bif with NO isel case, a latent
  installable-but-fails-isel gap) through the same path.

### WIN 4 — `badrecord` error-exit  (near-free)
`genop_badrecord_1` → existing error-exit modeling (translate_error_exit). Tiny; unblocks
record-heavy code. Fold in with the above.

### DEFER — bs_position (bit matching)
Highest json marginal (31.1%) but LARGE (unaligned matching + position save/restore
backtracking) and json is memo-14's twice-measured eligible-but-slower class. Not now.

## Suggested implementation order for step 5
1. `is_map` + `badrecord` (small, near-free) — one small commit.
2. `get_map_elements` (medium, the real map-matching win) — one commit; verify a struct/
   map-head function now installs + is correct across map/non-map inputs.
3. guard-BIF subset (medium) — one commit; verify Elixir `map[k]`/tuple-access functions.
4. (Optional) `call_fun` standalone — one commit, but expect eligible-but-flat; measure.
Gate each: census delta (blocker share drops), a real map/struct function now T2-installs
and is byte-identical to T1 across input types, no regression to the P1 stack, sign-compare.
