# Real-service VALUE — does T2-Full pay off on production service workloads? (2026-07-08)

> **Question this memo answers:** memo 13 measured T2's realizable speedup on
> dialyzer + the OTP compiler (symbolic tree/graph traversal — T2's worst case)
> and found ~3–6%, reclassifying T2 as a *specialist* tier. But those are not
> production **services**. Services live in JSON/HTTP/codec/binary-parsing hot
> paths — the "remove work" shapes T2's model (unbox, fuse arith, fuse bs-scan)
> is built for. **This memo measures T2-Full's whole-workload wall-clock on
> canonical service workloads and classifies the result** against the bar:
> ≥10% whole-workload on a canonical service ⇒ broadly valuable; single-digit ⇒
> specialist-only.
>
> Built + measured on `lukas/erts/beamjit2` tip `d95b7d1570` (last CODE change
> `c1785353ab` = P2.5 profiling + P2.6 A async-compile + B install-gate, gate
> default ON). Apple Silicon (aarch64-apple-darwin), pinned `{scheduler,1}`,
> best-of-9 ×2 (min), warm-up before every measurement. All artifacts +
> workload sources + raw numbers under `realsvc/`.

## 0. TL;DR — the NUMBER and the VERDICT

**Whole-workload wall-clock, T1 baseline vs T2 counter-triggered gate-ON (the
honest deployment number), best-of-N:**

| workload (what a service actually does) | T1 | T2 RETAIN (gate ON) | speedup | hot-path T2 installs |
|---|---|---|---|---|
| **JSON decode+encode** (5 KB API doc, `json` stdlib) | 227.9 ms | 226.6 ms | **1.01× (+0.6%)** | **0** |
| **binary frame codec** (len-prefixed parse+transform+re-emit) | 259.3 ms | 256.9 ms | **1.01× (+0.9%)** | **0** |
| **HTTP/1.1 request parse** (req line + 10 headers) | 145.0 ms | 147.5 ms | **0.98× (−1.7%)** | **0** |
| **base64 encode+decode** (1.5 KB, pure-Erlang stdlib) | 90.0 ms | 89.8 ms | **1.00× (+0.2%)** | **0** |
| **estone** (heterogeneous mixed synthetic) | 442.1 ms | 458.7 ms | **0.96× (−3.8%)** | ~17 sys, 0 hot |

All four canonical service deltas are **within ±2% measurement noise = 0% real
change**. Nothing on any service hot path tiers. estone is slightly *negative*
(counter/async overhead on code that doesn't tier).

**VERDICT: SPECIALIST (confirmed, and the niche is narrower than memo 13
implied). Measured realizable whole-workload speedup on canonical services =
~0%.** Not because the work is irreducible (memo 13's dialyzer story), but
because **97–100% of every service's own-time sits in functions T2 cannot even
compile** — they use binary *construction*, map ops, `call_fun` continuations,
and bs position-ops, none of which are in T2's eligible-opcode set. This is an
**opcode-eligibility** barrier at the frontend, **orthogonal to rung-2** (which
only removes the non-tail-call demote tax). Rung-2 would not make one JSON/HTTP/
codec function eligible, so — per the decision rule — this is **not**
"inconclusive, go build rung-2": the measurement is decisive that rung-2 is
*irrelevant* to the service hot path. T2's real wins are confined to
single-clause byte *scanners* that produce no output (scanbench 2.5–3.1×,
measured) and integer/float tail loops — a genuine but narrow specialist niche
that canonical service hot paths do not touch.

## 1. Method

Three run-modes per workload, discovered from source (`t2_retain.c`,
`t2_compile.cpp`, `erl_init.c`):

1. **T1 baseline** — no env, no flag (`erts_t2_enabled()==0`). Denominator.
2. **T2 RETAIN, gate ON** — `T2_RETAIN=1` (counter-triggered tier-up, threshold
   `1000·√(size+1)` counted on scheduler 1; gate default ON). **The realistic
   deployment number.** Warm-up runs long enough to cross threshold; verified
   installs via `erts_debug:get_internal_state({t2_installed,M,F,A})`.
3. **T2 +JT2enable, gate ON** — `+JT2enable true` (force-compile every eligible
   function at load). Sensitivity / upper bound; also the complete
   per-function accept/reject census (`T2_INSTALL_TRACE=1`).
   *(+ `T2_INSTALL_GATE=0` variant on estone to isolate the gate.)*

Bench process pinned `{scheduler,1}, {priority,high}`; per measurement, best of
9 timed batches, whole run repeated ×2 and the min kept (cross-run OS-noise
guard). Coverage via `eprof` own-time (tier-independent — the shape is a
property of the Erlang code). Gate decisions read from `T2_INSTALL_TRACE`
(`t2_install: <n> M:F/A` = installed; `t2_gate: REJECT M:F/A … (inl=… ret=…
fus=… bs=… sw=… spec=…)` = compiled-then-rejected; **absent = ineligible**,
never compiled).

**Build note (reproducibility):** the checked-in `beam.smp` was stale (Jun-16,
pre-T2 — no `+JT2enable`, no gate). Rebuilt the emulator at tip `d95b7d1570`
after regenerating the erts Makefile from the committed `config.status`
(`Makefile.in` had gained the t2 sources in `cb9df37339`); `config.h` unchanged
⇒ no reconfigure. `make emulator` only — no source modified. New `beam.smp`
verified: `+JT2enable` accepted, gate live, `scanbench` winners install.

## 2. The headline — JSON (canonical web-service hot path)

`json:decode/1` then `json:encode/1` on a realistic 4995-byte API response
(25 nested user objects: strings, ints, floats, bools, nested addr maps,
tag arrays). 5000 round-trips/batch.

**Whole-workload: T1 227.9 ms → RETAIN 226.6 ms (+0.6%, noise) → +JT2 234.3 ms
(−2.8%, one-time load-compile of all modules). Hot-path installs: 0.**

**Gate census (`+JT2enable`, complete):** of the `json` module's 92 functions,
only **16 are eligible; all 16 are gate-REJECTED; 0 install.** The entire hot
**decode** path is **ineligible** — it never appears in the trace. Cause,
confirmed from the eligible-opcode whitelist (`t2_eligible.c` +
`erts_t2_genop_supported`): the whitelist has **no `call_fun`, no map ops
(`get_map_elements`/`put_map_*`), no binary-construction ops (`bs_create_bin`
/`bs_init*`/`bs_put*`), and no `bs_get_position`/`bs_set_position`.** json's
decoder is continuation-passing over funs (`call_fun`) building maps; its
encoder reads maps and *builds* output binaries. Every hot function trips at
least one unsupported op.

**Coverage (eprof own-time) — the nuance that turns this decisive:**

| json function | own-time | why not addressable |
|---|---|---|
| `string/7` (string decode) | **18.6%** | bs position-ops + `call_fun` continuation → ineligible |
| `escape_binary/5` (string escape) | **18.0%** | bs-scan **+ builds output binary** → ineligible |
| `-do_encode_map/2-lc-` | 9.3% | map + fun → ineligible |
| `do_encode/2` | 5.9% | fun dispatch → ineligible |
| `continue/7` (decode cont) | 5.2% | `call_fun` → ineligible |
| `string_ascii/7`, `escape_binary_ascii/5` | 4.3% + 4.2% | bs-scan **+ build** → ineligible |
| … number/object/array/key helpers, `maps:from_list` … | rest | ineligible / BIF |
| **T2-installable own-time** | **0.0%** | — |

The cruel irony: **~44% of JSON's time is in binary string scan/escape code —
nominally T2's wheelhouse — and 100% of it is ineligible** because it either
*constructs* an output binary or uses the position-save ops the compiler emits
for guarded/backtracking matches.

## 3. The other services — same 0%, same reason

| workload | dominant own-time (eprof) | status |
|---|---|---|
| **HTTP parse** | `until_crlf/2` 55.2%, `until_colon/2` 20.0%, `headers/2` 8.3%, `until_sp/2` 7.7%, `skip_sp/1` 6.5% | scanners that build `<<Acc/binary,C>>` → **all ineligible** (bs construction). 0 installs. |
| **frame codec** | `upcase/2` **92.8%** (byte-transform loop), `parse/2` 3.7% | build `<<Acc/binary,B>>` / re-emit → **ineligible**. 0 installs. |
| **base64** (stdlib) | `decode_binary/4` **64.0%**, `encode_binary/4` **33.6%** | the real codec loops; bs-match **+ build** → **ineligible**. 0 installs. `base64:encode/decode` wrappers eligible but gate-REJECT (no work). |

Across all four services: **~97–100% of own-time is in functions T2 refuses to
compile**, and the thin eligible remainder (thin wrappers) is gate-rejected as
"no work eliminated." Whole-workload effect: nil.

## 4. The positive control — what T2 *can* win, and how narrow it is

To prove the machinery works and find the eligible-shape ceiling:

- **scanbench** (`plain/2`, `digit/2` — single-byte forward scan, one guarded
  clause + catch-all fallthrough, **integer** accumulator, **no output
  binary**): **installs, and wins big — plain 1186 → 467 µs = 2.54×; digit
  1036 → 335 µs = 3.10×** (1 MB buffers). This is the shape T2 is built for: a
  fused bs scan-run over bytes producing a count. It is a **validator/lexer**,
  not a codec.

- **But eligible ≠ faster.** A *realistic* multi-way byte classifier
  (`lex_wl:classify/4` — count spaces/newlines/digits over an access-log
  buffer: 2 literal-byte clauses + 1 guarded + 1 catch-all, 3 accumulators)
  **installs** (gate sees `bs≥1`) **yet runs 38% SLOWER: T1 193 µs → T2 268 µs**
  (isolated, confirmed `installed=true`). The scan-run fusion only pays for the
  single-guard counter shape; a multi-clause classifier falls to a slower
  per-byte T2 path. **This is a gate false-accept — the `bs≥1`
  "eliminated-work" signal over-accepts, and the memo-10 "never slower than T1"
  floor has a hole for multi-clause scanners.**

So the eligible-AND-winning niche is narrower than "binary": it is
*single-clause byte scan-and-count*. Real service byte handling — multi-way
classification, and above all *producing output bytes* — sits outside it.

## 5. estone — the mixed cross-check and the gate story

Whole-run estone (port_io excluded), best-of-9 ×2:

| mode | time | vs T1 |
|---|---|---|
| T1 | 442.1 ms | — |
| **+JT2enable, gate ON** | 450.4 ms | **−1.9%** |
| +JT2enable, gate **OFF** | 465.9 ms | −5.4% |
| RETAIN, gate ON | 458.7 ms | −3.8% |

Reproduces the P2.5/P2.6 story: **gate OFF installs losers and regresses −5.4%;
the gate recovers ~3.5 points (−5.4% → −1.9%)**, landing near T1. But there is
**no win** — estone's hot code doesn't tier, so the residual is pure
counter/async/compile overhead. Force-compiling the **entire running system**
(~5000 functions) installs only **17 blobs (0.34%)** — all in boot modules
(`erl_scan`, `inet_parse`, `rand`, `lists`), **none** on any service hot path,
identical across all four service runs.

## 6. What this means for T2's scope

**The service case does not rescue the "broadly valuable" thesis; it buries it,
and for a *different* reason than dialyzer.** Memo 13's analysis code was
*eligible but irreducible* (pointer-chasing, ~3–6%). Service code is worse:
**ineligible outright.** The union of T2's blind spots — `call_fun`, maps,
binary construction, bs position-ops — is exactly the vocabulary of JSON, HTTP,
and codec hot paths. A JSON round-trip, an HTTP parse, a base64 codec, a frame
transformer: each spends 97–100% of its time in code T2 will not compile.

**Decisive on rung-2 (no estimation needed).** The decision rule asked: is the
single-digit result *inconclusive because the hot code carries the inlinable
non-tail-call/loop-helper shape rung-2 targets*? **Measured answer: no.** The
service hot functions are rejected at **eligibility** (unsupported opcodes),
not at the gate's `calls_retained` non-tail-call tax. Rung-2 = framestates +
CP-on-stack to keep the T2 *ascent* alive across a non-tail call; it adds **zero
opcode coverage**. It cannot make `json:string/7` (position-ops + `call_fun`) or
`base64:encode_binary/4` (binary construction) eligible. So building rung-2
would move these service numbers by **0%** — the measurement settles it; no
"build to find out" is warranted for the service case. (Rung-2's only live value
case remains the dialyzer/compiler `are_all_limited`↔`is_limited` class of memos
11–13, which those memos already measured as marginal ~3–6%.)

**If one wanted T2 to matter for services** it would take a *different, larger
frontend* project than rung-2: teach the eligibility scan + isel + emit to
handle **binary construction, maps, and `call_fun`** — and even then the
`lex_wl` regression warns that eligible bs code is not reliably faster, so the
gate would need real per-shape cost modeling, not `bs≥1`. That is a much bigger
bet than rung-2 and this measurement gives it no support.

### Recommendation
- **Confirm T2 as a SPECIALIST tier** (memo 13's classification holds and
  tightens): 2–3× on single-clause byte scan-and-count kernels and
  integer/float tail loops; **0% on canonical services**; never-slower floor
  **with a known hole** (multi-clause bs scanners — fix or tighten the gate's
  bs signal). Do not ship/harden T2 as a general service tier on this evidence.
- **Do not build rung-2 for the service case** — measured irrelevant. Rung-2
  stays a (marginal) dialyzer/compiler play only.
- **File the gate false-accept** (`lex_wl:classify/4`, +38%): the
  eliminated-work rule `bs≥1` must not accept a multi-clause bs scanner that
  T2 lowers to a slow per-byte loop.

## 7. Reproducibility — exact commands

```
ERL=/Users/lukas/code/otp-beamjit2/bin/erl        # tip d95b7d1570, rebuilt (see §1)
cd realsvc && $ERL/../erlc *.erl                    # workloads + rsvc harness

# whole-workload wall-clock (pinned sched 1, best-of-9 warmed), per workload:
$ERL                 -noshell -pa . -eval 'rsvc:run(json_wl,8000,5000,9),halt().'   # T1
T2_RETAIN=1 $ERL     -noshell -pa . -eval 'rsvc:run(json_wl,8000,5000,9),halt().'   # RETAIN gate-ON
$ERL +JT2enable true -noshell -pa . -eval 'rsvc:run(json_wl,8000,5000,9),halt().'   # +JT2 gate-ON
#   (frame_wl 4000/3000, http_wl 6000/50000, b64_wl 6000/20000, lex_wl 6000/20000)

# per-function gate census (complete accept/reject list):
T2_INSTALL_TRACE=1 $ERL +JT2enable true -noshell -pa . -eval 'json_wl:work(json_wl:setup()),halt().'
#   -> 0 'json:' installs, 16 'REJECT json:' (encode side), decode path ABSENT (ineligible)

# install verification under RETAIN:
#   erts_debug:get_internal_state({t2_installed, lex_wl, classify, 4})  -> {Addr,Sz,MFA} (installed)
#   erts_debug:get_internal_state({t2_installed, json,   string,   7})  -> undefined     (never)

# positive control + gate false-accept:
$ERL                 -noshell -pa . -run scanbench go   # T1:   plain 1186us  digit 1036us
$ERL +JT2enable true -noshell -pa . -run scanbench go   # T2:   plain  467us  digit  335us  (2.5-3.1x)
#   lex_wl:classify/4 isolated: T1 193us -> T2 268us (+38%, installed=true)

# estone whole-run + gate isolation:
./run_estone.sh   # T1 442 / +JT2gateON 450 / +JT2gateOFF 466 / RETAIN 459 ms
```

Raw CSV: `realsvc/svc_results.csv`; estone: `realsvc/estone_results.txt`;
eprof dumps: `realsvc/*_eprof.txt`; +JT2 gate traces: `/tmp/*_jt2.txt`.
