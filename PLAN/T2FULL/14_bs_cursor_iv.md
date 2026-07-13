# T2-Full — Cursor-IV binary matching (unroll/SWAR-ready HIR)

Scope: replace the opaque, stateful binary-match HIR with a **cursor-as-
induction-variable** SSA form so that byte-scan loops become eligible
*and* a later pass can loop-unroll and SWAR-vectorize them. This is
chunk 1 of the `bs` widening (`13`-era scoping: 455 position ops + 89
`ensure_exactly` gate every scanning state out today), redesigned to take
**no shortcuts** — the cursor is a first-class raw IV, not a byte
peephole. aarch64 only. Paths under `/Users/lukas/code/otp-beamjit2/`.
Read-before-build surprises flagged ⚠.

Governing invariant unchanged (`08`/`09` §4): T2 matches T1's
X/Y/HTOP/FCALLS/stack at every **sync point**; between sync points
registers are free. The cursor design *uses* that freedom — the bit
cursor lives raw in a register across the loop and is reconciled to
`ErlSubBits.start` only at sync points.

---

## 1. Why the current instruction cannot unroll

T1's `bs_match` (and T2's mirror `BsMatch`, `t2_hir.hpp:230`) carries an
opaque command list and **mutates `ErlSubBits.start` in the heap on every
command**. Three consequences, each fatal to unrolling:

1. **The cursor is a memory-resident loop-carried dependency.** `start`
   (byte offset 16 of the boxed 5-word `ErlSubBits`, `erl_bits.h:60-82`)
   is read and written each command. A loop optimizer sees a
   read-modify-write through the heap — no induction variable, no
   strength reduction.
2. **Bounds-check and extraction are welded.** `ensure_at_least` +
   `integer` inside one `bs_match` is atomic to the optimizer; it cannot
   hoist the check out of, or widen it across, iterations.
3. **Position save/restore round-trips the heap.** `bs_get_position` /
   `bs_set_position` read/write `.start`; backtracking is invisible
   memory traffic instead of SSA dataflow.

The existing fused `ScanLoop` (`t2_emit.cpp:240-2925`) *does* registerize
the cursor — but it is a **bespoke emit-time LIR→asm peephole** that
bypasses the optimizer entirely and **explicitly rejects raw-mode ops**
(`t2_emit.cpp:2415-2418`). It is a hand-rolled proof that the cursor
*can* live in a register; this design promotes that structure into SSA
where LICM / unroll / SWAR can see and transform it. The new HIR path
**replaces** the ScanLoop (parity tracked by the `scan_runs` counter,
`t2_emit.hpp:92`).

---

## 2. The instruction set

Model a match context as a loop-invariant `(base, limit)` plus a **raw
bit-cursor induction variable**, decoupled from the boxed `ErlSubBits`
except at sync points. `StartMatch` is unchanged — it still
creates/reuses the boxed context term (the GC-visible root). Everything
downstream projects from it.

### Field projections (from the context term)

| op | signature | purity / GC | lowering |
|---|---|---|---|
| `BsBase` | `ctx → base` | pure read, **GC-clobbered** (rematerializable) | `ldr base,[ctx+8]; and base,~3` |
| `BsLimit` | `ctx → limit` | pure, **fully invariant** (a bit count, GC-inert) | `ldur limit,[ctx+24]` (`.end`) |
| `BsCursor` | `ctx → cursor₀` | pure read, initial IV value | `ldur c,[ctx+16]` (`.start`) |

Three one-result field readers sidestep the "one SSA result per `T2Op`"
rule (`t2_hir.hpp:648-668`) — no multi-result op needed. `BsBase` returns
a **raw machine pointer** into the (movable) binary data, so it is
classified GC-clobbered in `clobbers_reg` (`t2_opt.cpp:594`): any
allocating op invalidates it and it re-materializes from `ctx` — exactly
the ScanLoop's `SCAN_BASE` reload-after-alloc discipline
(`t2_emit.cpp:2870-2876`). `BsLimit`/`BsCursor` are pure loads;
`BsLimit`'s result is loop-invariant and hoists to the preheader via
LICM-lite (`t2_intrinsics.cpp:6279`).

### The cursor IV, and the two hot-path ops

| op | signature | purity / GC | role |
|---|---|---|---|
| *(advance)* | `cursor' = AddSmall(cursor, size)` | pure, `T2_OP_RAW_MODE` | the induction variable (raw) |
| `BsEnsure` | `(cursor, limit, need, mode) → guard` | pure, **separable** | bounds check, `mode ∈ {at_least, exactly}` |
| `BsRead` | `(base, cursor, size, flags) → value` | **pure**, no mutation | extract at `base+cursor` (non-allocating: integer ≤ word, or skip) |
| `BsSync` | `(ctx, cursor)` | effect, memory writeback | reconcile cursor → `ErlSubBits.start` at every sync/exit/deopt |

Notes that make or break correctness:

- **`BsEnsure` is the unroll lever.** It fails to the decoded fail label
  (wired via `Succeeded`+`guard_branch`, the existing pattern at
  `t2_hir_builder.cpp:2203-2206`). `mode=at_least` → `limit-cursor >=
  need` (`b_lo` fail); `mode=exactly` → `== need` (`b_ne` fail),
  subsuming `ensure_exactly` and `bs_test_tail2`. It reads only SSA
  values (`cursor`, `limit`) — never the heap — so it is hoistable and
  **strengthenable** (§4).
- **`BsRead` is pure only for non-allocating extractions** — a byte /
  fixed-width integer that fits a machine word, reading `size` bits at
  `base + (cursor>>3)`. Binary/`get_tail` extraction allocates a
  sub-bitstring and stays the existing `BsGetTail` (a sync op). A `skip`
  command produces no value — it is *only* a cursor advance. So the hot
  scan body is `{BsEnsure, BsRead, classify, advance}`, all pure but the
  ensure-guard; `BsGetTail` appears once at the tail.
- **`BsSync` is `emit_scan_writeback` promoted to a first-class op**
  (`t2_emit.cpp:2731-2742`): `ErlSubBits.start := cursor`. The raw
  cursor is an *optimization-internal* value; its ground truth is
  `.start`. `BsSync` restores that truth wherever T1 may re-observe the
  context — loop exits, deopt trampolines, and immediately before
  `BsGetTail`/side-exits. ⚠ The sync map at a `BsSync`-covered point must
  **not** name the cursor as a live X value (T1 reads `.start`, never a
  slot) — so the cursor needs **no `raw_mask` retag** (contrast the
  maps:fold IV, whose value *is* slot-observed). `BsSync` is the retag
  analog, but to memory.

---

## 3. The cursor is a raw induction variable (reuse P2)

The cursor is a `Uint` bit-offset — i.e. a small — carried raw across the
back edge. It reuses the P2 **RAW-IN-HOME** machinery verbatim:

- Loop recovery homes each loop-carried value to an X-slot header phi
  (`t2_loop.cpp:479-487`, `set_phi_inputs` at `591-602`). Add one cursor
  phi, inputs `{BsCursor(ctx) [preheader], cursor_next [latch]}`, exactly
  the maps:fold index-IV shape `{i0, i_next}` at
  `t2_intrinsics.cpp:2692-2715`.
- Flag the phi and its `AddSmall` advance `T2_OP_RAW_MODE`
  (`t2_hir.hpp:616`; precedent `i_phi->flags |= T2_OP_RAW_MODE`,
  `t2_intrinsics.cpp:2805`). Raw form = the tagged small with tag bits
  cleared (`value` unshifted vs `<<4`); overflow check is bit-identical,
  so the P3 no-overflow prover (`T2_OP_NO_OVF`, `t2_opt.cpp:1549`) elides
  the advance's overflow guard once it proves `cursor < limit ≤
  bit_size`.
- `BsEnsure`'s `limit` and `BsRead`'s `base` are raw consumers of the
  cursor (like `CmpLt`/`FlatmapValAt`, the existing raw-consumer set).
- At deopt, the cursor is **not** in the sync map as a live term — it is
  reconciled by `BsSync`, so the `raw_mask`/`fixup_raw_homes` retag path
  (`t2_emit.cpp:566-582`) is *not* needed for it. This is the maps:fold
  "callsite-class deopts never name the loop value" property
  (`t2_intrinsics.cpp:2764-2772`), specialized to a memory-homed value.

`base` (raw pointer) and `limit` (immutable bit count) complete the
triple: `base` GC-clobbered/reloaded, `limit` invariant.

---

## 4. The payoff: unroll + SWAR (a new pass)

Because the cursor is an IV and `BsEnsure`/`BsRead` are separate and
pure, unrolling is a standard IV transform, not a pattern match. A new
`t2_unroll` pass (runs after `t2_loop_info`, before `t2_opt`; the
pipeline has **no** unroll today — `t2_compile.cpp:397-496`) does:

1. **Recognize** a cursor IV: a raw header phi advanced by a constant
   stride `S` bits, whose loop guard is `BsEnsure(cursor, limit, S,
   at_least)`.
2. **Pick a factor** `N` (adaptive — see open questions): default `N =
   word_bits / S` so byte scans (`S=8`) unroll ×8 into one 64-bit word.
3. **Strengthen the guard**: emit a preheader `BsEnsure(cursor, limit,
   N·S, at_least)`; the per-iteration `BsEnsure(S)` is now dominated →
   DCE removes it. This is the "one wide check per N iterations" win —
   impossible with the welded `bs_match`.
4. **Clone the body** `N` times with `cursor + k·S` (`k=0..N-1`); the
   advance becomes `cursor + N·S`.
5. **SWAR-fuse the reads**: `N` pure `BsRead(base, cursor+8k, 8)` from
   consecutive byte offsets collapse to one aligned `ldr` + `N`
   extractions; a downstream byte *classification* (range test — "is
   digit", "is delimiter") SWAR-fuses into parallel masked compares. This
   is the A1 prototype's win (`PLAN/T2/09_trackA_a1_scan_run.md`,
   design-only, unmerged) — now *enabled by the IR* instead of a bespoke
   `emit_bs_scan`.
6. **Remainder**: the original 1-wide body handles the `< N·S`-bit tail
   (standard unroll-with-remainder; the exit `BsEnsure(exactly, 0)` /
   `BsGetTail` land in the remainder).

The unroller never touches the heap context — it manipulates SSA cursor
arithmetic and pure reads. `BsSync` on the loop exits keeps `.start`
coherent regardless of factor.

---

## 5. Lowering (builder → isel → emit)

**Builder** (`t2_hir_builder.cpp`): decode the raw generic ops (T2 reads
pre-`ops.tab`, so it sees `bs_start_match4`/`bs_get_position`/
`bs_set_position`/`ensure_exactly`):
- `bs_start_match4 {no_fail|f}` → `StartMatch` (reuse `:2076`; arg order
  `{Fail|no_fail, Live, Src, Dst}`). `{resume}` → `Move`.
- `bs_match` → emit `BsBase`/`BsCursor` projections (once per context,
  CSE-shared), then per command: `ensure_at_least`/`ensure_exactly` →
  `BsEnsure`; `integer` (≤word, empty flags) → `BsRead` + advance;
  `skip` → advance; `get_tail` → `BsSync` then `BsGetTail`.
- `bs_get_position` → `make_small(cursor)` (pure, the current cursor SSA
  value tagged — **no heap read**). `bs_set_position` → untag → a cursor
  SSA value feeding the subsequent phi (**no heap write**); backtracking
  becomes a φ-merge.
- `BsLimit` emitted once, hoisted.

**isel** (`t2_isel.cpp:1212+`): new LIR kinds for `BsBase/BsLimit/
BsCursor/BsEnsure/BsRead/BsSync`; `BsRead`/`BsEnsure` fold their fail
edge like `BsMatch` (`:1300-1306`). **emit** (`t2_emit.cpp:1475` main
dispatch): `BsBase/BsLimit/BsCursor` → the 1–2-instr field loads;
`BsEnsure` → `sub;cmp;b_lo|b_ne`; `BsRead` → `ldrb`/`ldr` at
`base+cursor>>3` + tag; `BsSync` → the `emit_scan_writeback` store. The
unrolled SWAR body emits a wide load + parallel classify.

---

## 6. GC / deopt / fidelity

- **Context term** stays whole in its X/Y slot, GC-visible, throughout —
  we never remove it; GC fixes its `base_flags` via `orig`
  (`erl_bits.h:521-538`) if the backing `ErlHeapBits` moves.
- **`base`** is a raw pointer → GC-clobbered; reload from `ctx` after any
  allocating op (`clobbers_reg`, `t2_op_dirties_window`
  `t2_loop.cpp:615-650`). In pure scan bodies (no alloc) it hoists
  invariant. ⚠ Never carry `base` across a GC — reload, per
  `t2_emit.cpp:2870-2876`.
- **`limit`** is a bit count (immutable, GC-inert) → unconditionally
  invariant.
- **`cursor`** is raw in an X home during the loop; `BsSync` writes it to
  `.start` before any T1-observable boundary (exit, deopt trampoline,
  `BsGetTail`, side-exit). Deopt is re-call class; on re-entry T1 reads
  the reconciled `.start`. Because `BsSync` covers it, the cursor needs
  no `raw_mask` entry.
- **Fidelity**: forced-mode byte-identical vs T1 on the parity kernels
  (`t2_parity_SUITE` `byte_scan`/`word_lex`, currently ineligible), the
  CSV SCC scanning states (`14`-era `scc_fidelity`), and a
  `binary`/`base64`/`json` corpus via `t2_g1_SUITE`. Unrolled output must
  hash-match the 1-wide output (same suite, factor-swept).

---

## 6b. Stack layout — unchanged; raw values never on the term stack

The design keeps T1's frame layout exactly, and should. The rung-1 deopt
model is re-call (`07`/`08`): at any deopt T2 discards transient state
and T1 re-executes from the reconciled frame, whose Y slots the stack
walker scans as terms. Every value the design adds respects this:

- the **cursor** lives raw in a register across the loop and is
  reconciled to the heap `ErlSubBits.start` by `BsSync` at every
  sync/exit/deopt — never to a stack slot;
- **base** reloads from the context (GC-clobbered), **limit** is
  invariant, the **context term** stays in its GC-visible slot.

So nothing new lands on the stack, and changing the layout would ripple
into deopt, the walker, and every sync point across the whole tier.

⚠ **Raw values must never be spilled to a Y slot.** Y slots are
term-scanned, so a raw spill there must be retagged to stay walker-safe
— paying back exactly the tag/untag tax `T2_OP_RAW_MODE` exists to
remove. Raw-on-(term)-stack is self-defeating: it taxes the values we
are de-taxing.

**Register budget.** A byte-scan ×8 SWAR body's raw working set —
cursor, base, limit, the 64-bit word, an accumulator, 2–4 classification
masks (~10 live) — fits the ~15–18 GP registers free after the JIT ABI
reserves `c_p`/`HTOP`/`E`/`FCALLS`/`TMP*`/`ARG*`. Heavier factors are
memory-bandwidth-bound, so more lanes buy little. Measure pressure
before assuming it.

**If raw spills are ever needed** (very heavy factor, complex
classification): the home is the per-scheduler **aux-memory scratch**
(already used — e.g. `TMP_MEM` for the profile trip,
`arm/instr_common.cpp`), not the Erlang stack. Raw loop scratch is dead
at every sync point — re-call deopt re-derives it, and at a yield the
cursor is already in `.start` and tagged homes are retagged by
`raw_mask` — so it never needs to survive deopt/GC/yield or be
walker-visible. That keeps it outside the GC-scanned frame with no
retag and no layout change: a contained addition gated on measured
pressure, not a stack redesign.

## 7. Phased build

Eligibility is conjunctive → nothing flips eligible until the whole op
set lands, so phases are independently green:

- **P-A — cursor-IV scaffolding (no eligibility change).** Add
  `BsBase/BsLimit/BsCursor/BsEnsure/BsRead/BsSync` HIR+LIR+isel+emit and
  every switch (§8). Re-express the *existing* supported byte-aligned
  `bs_match` subset through them (StartMatch → projections → ensure/read/
  advance → sync). Gate: existing bs tests byte-identical, `scan_runs`
  parity held or the ScanLoop still handles its cases.
- **P-B — widen eligibility.** Add `bs_start_match4`,
  `bs_get_position`/`bs_set_position` (SSA cursor tag/untag),
  `ensure_exactly` (`BsEnsure exactly`) to the scan (`t2_eligible.c:158`,
  `erts_t2_bs_match_check:194`). Multi-clause scanners become eligible;
  parity kernels + CSV SCC states flip and route through T2.
- **P-C — the unroll/SWAR pass.** Add `t2_unroll` (§4). Sweep factor,
  hash-match fidelity, benchmark against the 1-wide and the retiring
  ScanLoop.
- **P-D — retire the ScanLoop.** Once the HIR path reaches `scan_runs`
  parity + wins on the byte-scan benchmark, delete the emit peephole
  (`t2_emit.cpp:240-2925`) and its raw-mode rejection.

## 8. Integration checklist (every switch a new op touches)

HIR: `T2OpKind` (`t2_hir.hpp:98`), `t2_op_kind_name` (`t2_hir.cpp:50`),
`t2_op_is_terminator`/`t2_op_produces_value` (`t2_hir.cpp:242,259`),
`t2_op_is_pure` (`t2_opt.cpp:2100` — `BsRead/BsEnsure/BsBase/BsLimit/
BsCursor` yes, `BsSync` no), `clobbers_reg` (`t2_opt.cpp:594` — `BsBase`
GC-clobbered, `BsSync` writes memory), `op_is_effect`/
`t2_op_dirties_window` (`t2_loop.cpp:305,615`), validator + `run_raw_
checks` (`t2_hir.cpp:1957-2160`, cursor raw legality) + dumper. LIR:
`T2LirKind` (`t2_lir.hpp:79`), name/terminator/dump (`t2_lir.cpp`), isel
(`t2_isel.cpp:1212`), emit dispatch (`t2_emit.cpp:1475`). Builder
`snapshot_sync` for the trapping ops (`BsEnsure`, `BsGetTail`).

## 9. Open questions

- **Adaptive vs fixed unroll factor.** Default adaptive `N =
  word_bits/S` (byte→×8 SWAR; `S=32`→×2 for latency, not SWAR). Fixed-8
  is simpler but wastes wider strides. Recommend adaptive, `N` parameter
  on the `t2_unroll` pass, remainder always the 1-wide body.
- **`dsts>1` fusion** (single-clause `integer`+`get_tail` in one
  `bs_match`, `t2_eligible.c:301`) — orthogonal; the projection model
  handles it naturally (two reads off one context) but the scan gate
  must stop rejecting it.
- **Bit-unaligned** reads (sub-byte `integer`, 19 occurrences in the
  corpus) — `BsRead` with `size<8`/non-byte cursor; SWAR needs byte
  alignment, so unaligned stays 1-wide. Later chunk.
- **SWAR classification coverage** — the fuse in step 5 needs the
  consumer to be a recognizable byte range test. Start with the common
  `is_digit`/`is_space`/delimiter-set shapes; fall back to N scalar
  extractions when the classifier isn't SWAR-shaped (still unrolled, just
  not vectorized).
