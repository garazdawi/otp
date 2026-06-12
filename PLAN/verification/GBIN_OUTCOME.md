# G-bin experiment — outcome

The binary-matching-loop gate from
[`../T2/08_v1_loop_tier.md`](../T2/08_v1_loop_tier.md) §6.1: do
T1's per-byte binary-matching overheads (the match-context dance)
constitute a real pool that loop-tier fusion can harvest? Primary
subject: stdlib `json:decode/1`'s scan loops. Run 2026-06-12, MVP
methodology, single build, `T2_GBIN=1` env gate.

## Verdict

**PASS — the mechanism delivers.** Isolated string scanning speeds
up **5.6×** (2.0 → 11.8 GB/s); full decodes of real documents gain
6–10 % from specializing just three functions, bounded by scan
share (Amdahl), with the remainder sitting in exactly the shapes
the other gates cover (branchy value dispatch, map/object
construction, conversion BIFs). Binary `bs_*` coverage in recovered
loops is green-lit for the post-v1 expansion — pulled ahead of
general inlining as `08` §6.1 proposed.

Contrast with G3 (0 ± 1 %): per-byte binary-match overhead is a
real per-op cost pool in T1; tiny-call overhead is not. The two
experiments together locate where the tier's wins actually live.

## What was built

`emit_t2_json_scan` (`arm/beam_asm_module.cpp`) hooked for three
MFAs, two bodies:

- **Digit scan** — `json:number/7`, `json:number_frac_cont/7`:
  byte loop consuming `0-9`, ~8 instructions/byte.
- **Plain-ASCII scan** — `json:string_ascii/7`: SWAR over 8-byte
  words (stop on `< 0x20`, `"`, `\`, `>= 0x80`; ~20 instructions
  per 8 bytes), bounded bytewise tail.

Shared structure: guard that `XREG0` is a byte-aligned `ErlSubBits`
match context and Len a smallint (side-exit untouched otherwise);
hoist base/position/end into registers; **every** exit — stop byte,
end of input, yield — syncs (write the bit position back into the
context, `X6 = Len + consumed`, overflow-checked before any commit)
and side-exits to the T1 entry, which re-matches the first
unconsumed byte and takes whatever clause applies. The T2 body
knows nothing about the clause structure; T1 keeps all the
decisions. Match-context position mutation is idempotent state
sync, so re-execution semantics are preserved. Reductions: 1/byte
(digits) and 1/chunk (SWAR), matching T1's per-iteration charges.

What this eliminates per byte vs T1: function re-entry
(prologue + `i_test_yield`), and the per-`bs_match` position
load/store + base load/mask (`instr_bs.cpp` reloads and writes back
`ErlSubBits.start` around every extract).

## Correctness

Decode-result hash identical off/on (`1865024115`) over: the
nativejson trio, a small API payload, bignums, escape sequences,
direct UTF-8, deep nesting, a 10 000-element array, and five
malformed inputs (error terms compared too).

## Measurements (Apple Silicon, min of 15 × N decodes)

End-to-end `json:decode/1`:

| corpus | T1 | T2 | speedup |
|---|---|---|---|
| twitter.json (strings/escapes) | 22 831 µs | 20 677 µs | **1.10×** |
| citm_catalog.json (structure) | 37 050 µs | 34 999 µs | 1.06× |
| canada.json (numbers) | 105 515 µs | 96 956 µs | **1.09×** |
| small.json ×2000 (1.5 KB API shape) | 7 059 µs | 6 912 µs | 1.02× |

Isolated scan loops (min of 9 × 20 decodes):

| workload | T1 | T2 | speedup |
|---|---|---|---|
| 1 MB single string | 9 856 µs | 1 772 µs | **5.56×** (2.0 → 11.8 GB/s) |
| 20 000 × 22-char floats | 37 449 µs | 32 402 µs | 1.16× |

The numbers workload is capped by `binary_to_float` per token and
list/array construction — conversion BIFs and term building, not
scanning. The 5.6× reads directly on the gate question: when the
loop *is* the scan, fusion takes BEAM from bytecode-shaped speed to
memchr-class speed.

## Findings

1. **The match-context dance is the real pool.** T1 spends
   ~10–14 cycles per byte-class iteration on re-entry + context
   field traffic even after json.erl's own hand-unrolling; the
   fused loop runs at ~0.3 cycles/byte (SWAR) — a per-op overhead
   ratio no OoO core can hide, unlike G3's call overhead.
2. **json.erl's Erlang-level 8-byte unroll (`string_ascii`) is a
   hand-written workaround for exactly this** — and the T2 loop
   beats it 5.6× while making the workaround unnecessary. Library
   code is *already* paying complexity to dodge T1's per-op cost;
   the tier refunds it.
3. **End-to-end JSON wins are a compound of all the gate shapes.**
   Scan fusion alone: 6–10 % on real documents. The remaining
   profile is branchy dispatch (`value`/`object_key`/`continue` —
   G3 subject 1), object/map building (G-map), and conversion BIFs
   (runtime, out of tier scope). The plan's "push the built-in
   parser toward NIF-class" goal needs the gates *composed*, and
   json:decode is the natural compound benchmark for that.
4. **The fast-forward + side-exit-to-entry shape is extremely
   cheap to build**: the T2 body needs zero knowledge of the
   function's clause structure (T1 keeps every decision), no
   allocation, no calls, no framestates — it is the loop-tier
   architecture at its simplest, and it composes with the
   demote-on-return world untouched.
5. Reduction fidelity and yield held trivially (sync-then-trampoline;
   resume re-enters the scan via the hook).

Caveats: scan-class loops only (UTF-8 validation in `string_utf8`
untouched — a harder, table-driven loop); encode side
(`escape_binary_ascii`, same shape) not yet done; one machine;
hand-written codegen approximates what `bs_match` fusion in a
production T2 would emit from the IR (the production version
fuses `<<Byte, Rest/bits>>` + guard + self-tail-call patterns —
exactly what the loop-recovery + `bs_*` op coverage produces
mechanically).

## Gate disposition

- **G-bin: PASSED.** Phase-D-style `bs_*` coverage in recovered
  loops moves ahead of general inlining in the expansion order, as
  `08` §6.1 conditionally scheduled. The production form: loop
  recovery over `bs_match`-headed self-recursive functions, match
  context as loop-carried state, position/base/end register
  residency between sync points, byte-class guard fusion (SWAR
  where the class test is byte-parallel).
- Remaining to compose for the json target: G3 subject 1 (branchy
  dispatch) and G-map (object construction) — each now sized
  against a measured residual rather than a guess.

## Reproduction

```bash
make -j$(sysctl -n hw.ncpu)
# corpus: twitter/citm_catalog/canada.json from nativejson-benchmark in /tmp/gbin
./bin/erlc -o /tmp/gbin PLAN/verification/t2_gbin.erl
./bin/erl  -noshell -pa /tmp/gbin -eval 't2_gbin:check("/tmp/gbin"), t2_gbin:bench("/tmp/gbin",15), halt().'
T2_GBIN=1 ./bin/erl -noshell -pa /tmp/gbin -eval 't2_gbin:check("/tmp/gbin"), t2_gbin:bench("/tmp/gbin",15), halt().'
```
