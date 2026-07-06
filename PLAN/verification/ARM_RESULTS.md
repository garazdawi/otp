# T2 ARM bench — Neoverse (ubuntu-24.04-arm)

Commit `e9fd08370e6b66a6a6f13be9589a75d05c433e57`. JSON corpus is synthetic (nativejson-shaped);
G-bin absolute us differ from the Apple OUTCOME files — the **ratio** is the rule-5 signal.

## Infrastructure checks (structural — should hold identically to Apple)

| check | result |
|---|---|
| T2_SELFTEST (HIR+ranges+emit) | PASS |
| T2_BUILD+T2_ISEL sweep | modules=115 lowered=3068 unsupported=2871 build_failures=0 |
| g1 fidelity (dssaopt) | modules=12 functions=295 reconstructed=175 |

## Measurement kit (rule-5: do the Apple nulls/wins hold on server ARM?)

| experiment | Apple (OUTCOME) | arm-runner | ratio |
|---|---|---|---|
| MVP total/2 (static, no toggle) | 1.97x min vs T1 | min=2438244 median=2738090 ns_per_iter=2.73809 | n/a (static) |
| G3-2 stage a (call-cross, leafy) | ~1.0x (null) | off=1351 a=1561 us | 0.87 |
| G3-2 stage b (inline header, leafy) | 2.25x | off=1351 b=576 us | 2.35 |
| G3-1 branchy dispatch | 1.02x (null) | base=1146 treat=1112 us | 1.03 |
| G-map flatmap shape | 1.64x | base=4542 treat=2704 us | 1.68 |
| G-bin json e2e (twitter) | 1.10x | base=84961 treat=66620 us (hash-ok) | 1.28 |
| A1 scanbench plain (scan_loop) | 2.93x | stock=0.35 scan=1.30 GB/s | 3.71 |
| A1 scanbench digit (scan_loop) | 4.08x | stock=0.42 scan=6.94 GB/s | 16.52 |
| A1 json_decode_bench twitter | 1.03-1.04x | 1.048x | 1.048x |
| recv classify (arch-invariant) | W1 49.98% / ring 99.38% / flood 99.75% | W1 49.99% / ring 99.37% / flood 99.81% | — |

Ratios >1.0 mean the arm-runner speedup; null experiments should stay ~1.0.

## Coordinator verdict (M0.5, rule 5)

**All Apple-Silicon nulls hold on server ARM** (G3-2 call-crossing
0.87, G3-1 dispatch 1.03) — the shelving decisions stand on
Neoverse. **All positives hold and the binary-scan class
strengthens substantially** (plain scan 3.71× vs 2.93× Apple,
digit scan 16.52× vs 4.08×, json e2e 1.28× vs 1.10×): the
weaker core pays more per-byte T1 overhead, so fusion pays more —
exactly the "positives can only strengthen on smaller cores"
prediction of 08 §2.3 rule 5. Pillar 1's case is stronger on
server hardware than on the development machine. The receive
classification is arch-invariant (M0.R conclusions portable).

Snapshot: pre-P1-close armci branch (workflow + kit); the armci
branch is re-pushed at each phase gate so the standing CI tracks
the tier. Run: garazdawi/otp actions run 28769427938
(ubuntu-24.04-arm).
