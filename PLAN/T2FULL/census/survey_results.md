# Polyglot fold survey — what real Erlang/Elixir/Gleam callers gain from T2 today

Measured on the committed build (Stage 1 + Stage 3) with forced T2
(`T2_INSTALL_GATE=0 ERL_AFLAGS="+JT2enable true"`), 11 mirrored callers over a
1000-element list / 1000-key map, ns per whole-caller op, min-of-3. Corpus +
raw data in scratchpad/survey/. **All numbers are under FORCED T2** — production's
install gate would reject most no-benefit blobs; forcing exposes the ceiling and
the overheads.

## Headline
The hand-coded recognizer approach is narrow AND fragile, and the polyglot fold
idioms get nothing-or-worse:

| shape | result |
|---|---|
| non-tail `lists:foldl` (E1b/E2b) | **2.3-3.8x WIN** (E2b needs the Stage-3 suite: T2off 1.02x → T2on 2.30x) |
| tail `lists:foldl` (E1/E2, the idiom) | **1.00x — recognizer blind spot** |
| `maps:fold` on a 1000-key map (E3/E4) | flat-to-−5% — hashmap, shape guard always misses |
| `maps:fold` on a flatmap ≤32 (Stage 1/3 verified) | 2.5x — the actual design envelope |
| Elixir `Enum.reduce`/`Enum.sum` (X1/X2/X4) | 0.80–1.04x, X4 map **−20%** |
| Gleam `list.fold` (G1/G2/G3) | 0.89–1.00x, G3 **−11%** |

## The two structural findings

### 1. lists:foldl has a tail-call blind spot (confirmed in code)
`t2_intrinsics.cpp` `run()`: the `CallExt` scan (line ~2422) recognizes lists AND
maps sites, but the `TailCallExt` terminator scan (line ~2450) is **maps-only**
(Stage 1 added it for maps; the pre-existing lists path was never extended).
Idiomatic `f(L) -> lists:foldl(F,0,L).` compiles to `call_ext_only` (a tail call)
→ `scan: 0 lists site(s)` → no specialization. Forcing non-tail (E1b/E2b) makes it
fire → 2.3-3.8x. **Fix is tactical** (port the maps tail-site handling to lists),
but see the verdict — it is one more whack-a-mole the general mechanism subsumes.

### 2. maps:fold specialization is flatmap-only (by design)
The `IsFlatmapBounded` guard caps at `MAP_SMALL_MAP_LIMIT` (32). A 1000-key map is
a CHAMP hashmap, so the guard always misses → the slow edge (verbatim maps:fold)
runs, plus the guard cost → −5% (E3). This is NOT a regression of the flatmap
path (which still wins 2.5x at n≤32, independently verified); it is the guard-miss
tax on out-of-envelope inputs. Large maps would need a hashmap walk (unbuilt).

### 3. Elixir/Gleam: nothing, and net-negative under forced T2 (the P1 gap)
Neither `Enum.reduce` nor `gleam@list:fold` matches a recognizer; the inner
work-function (`'Elixir.Enum':reduce/3`, `gleam@list:fold/3`) does not even
T2-install. The per-element closures DO install and pay small-int
speculation-guard + prologue cost, but cannot be inlined into the generic reduce
→ pure overhead, hence the 3–20% regressions (X4 −20%, G3 −11%). This is the
direct empirical case for **P1 (general call-site specialization + inline)**.

## Verdict
- "Elixir/Gleam win little/nothing" — **confirmed, stronger** (they regress under
  forced T2).
- "Erlang lists:foldl/maps:fold win big" — **only in narrow shapes**: non-tail
  lists:foldl, and flatmap maps:fold. The idiomatic tail lists:foldl and large
  maps get nothing. The hand-coded recognizers cover two stdlib functions and are
  fragile even there.

The survey is the motivation for replacing recognizer whack-a-mole with a uniform
mechanism: profile the arriving fun (env-free → stable code pointer) + element/acc
types at the hot fold function, clone-and-inline per monomorphic target, guard at
entry, deopt to the shared generic fallback. That one primitive covers tail and
non-tail, lists and maps and user recursion, and Elixir/Gleam — everything this
survey found uncovered. See opt_landscape.md P1 and the (pending) P1 design doc.
