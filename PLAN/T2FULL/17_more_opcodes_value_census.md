# 17 — Should we implement more opcodes to find the service value? (2026-07-10)

> **Question this memo answers.** Memo 14 measured **0 % whole-workload on
> canonical services** and attributed it to the eligible-opcode frontier:
> 97–100 % of service own-time is in functions T2 refuses to compile
> (`call_fun`, maps, binary *construction*, bs position-ops). The natural
> follow-up: *is that a real ceiling, or an artefact of the opcodes we chose not
> to implement? Should we implement more opcodes to find out what the value is?*
> This memo answers **no — don't build to find out** — states precisely what is
> measured vs. argued, and specifies the **cheap frontend-only census** that
> prices the "more opcodes" pool the way M0 priced every other pool: before
> funding the phase that would exploit it.

## 0. TL;DR

- **Measured (memo 14):** with the *current* eligible set, service hot own-time
  in T2 = ~0 %; system-wide install rate = 17 / ~5000 functions (0.34 %).
- **Argued, not yet measured:** that *adding* opcode classes wouldn't help
  either. Three strong arguments (§2), but it is an inference, not a number —
  the one honest soft spot in "0 % on services."
- **Building the opcodes to find out is the worst possible experiment here**
  (§2): the classes are entangled (non-stageable), eligible ≠ win (measured
  twice), and the only class with a real win story (maps) *also* needs a
  type-feedback profiler that was never built.
- **Do this instead (§3):** a frontend-only **addressable-share census** —
  days, zero codegen — that turns memo 14's argument into a per-class number.
- **Corpus gap (§4):** memo 14's corpus is all byte-slinging (JSON/HTTP/codec/
  base64), T2's worst case for this question. A **map/term-heavy** service class
  was never tested; that is where a surprise, if any, lives.

## 1. What is measured vs. what is argued

Memo 14 is decisive on one thing and inferential on another; keep them separate.

**Measured (hard):** for the eligible-opcode set *as it exists at HEAD*
(`erts_t2_genop_supported`, memo 16 §6), the service hot path spends ~0 % of its
time in T2. This is a per-function census (`T2_INSTALL_TRACE`), not an estimate:
JSON = 16/92 functions eligible, all 16 gate-rejected, 0 installed; the decode
path never even appears (ineligible); HTTP/frame/base64 = 0 installs; estone
force-compile = 17/~5000 installed, none on any hot path.

**Argued (soft):** that *implementing the missing classes* would still yield
~0 %. Memo 14 §6 makes this case well ("a different, larger frontend … and even
then the `lex_wl` regression warns eligible bs code is not reliably faster"),
but it is reasoning from three facts, **not** a measurement of the counterfactual.
The "more opcodes" pool has not been priced. That is exactly the kind of gap M0
exists to close — so close it (§3), don't leave it as an argument and don't fund
past it blindly.

## 2. Why "implement the opcodes to find out" is the wrong experiment

Three independent reasons, strongest first.

**2.1 The classes are entangled — the experiment is non-stageable.** The hot
service functions each trip *multiple* unsupported opcode classes at once, so no
single opcode addition produces a measurable delta:

| hot function (own-time) | unsupported classes it needs | count |
|---|---|---|
| `json:string/7` (18.6 %) | `call_fun` **and** bs position-ops | 2 |
| `json:escape_binary/5` (18.0 %) | bs-construction | 1 (+scan) |
| `-do_encode_map/2-lc-` (9.3 %) | maps **and** `call_fun` | 2 |
| `json:do_encode/2` (5.9 %) | `call_fun` | 1 |
| `json:continue/7` (5.2 %) | `call_fun` | 1 |

To make the *bulk* of hot own-time eligible you must build the **entire union**
{`call_fun`, maps, bs-construction, bs-position} before a single high-value
function compiles — all-or-nothing spend before the first measurement. The only
own-time that lights up with a *single* added class (`do_encode` + `continue`,
~11 %, need only `call_fun`) is dispatch glue — see §2.2.

**2.2 Eligible ≠ win — already measured, twice.** Making service code *compile*
does not make it *faster*. `lex_wl:classify/4` installs and runs **+38 %
slower** (memo 14 §4); dialyzer is eligible-but-irreducible at ~3–6 % (memo 13).
T2 wins *only by removing work* (`t2_compile.cpp:111`), and service hot paths are
memory-bound (byte copy, allocation) or dispatch glue — not the dispatch-heavy
arithmetic loops T2 removes work from. The ~11 % that only needs `call_fun`
(`do_encode`/`continue`) is call-overhead code, and G3-2 already established T2
does not win from call-overhead removal.

**2.3 The one class with a win story also needs a profiler that was never
built.** Maps are the sole class with a plausible *work-removal* story: turn a
`get_map_elements` hash lookup into a fixed-offset load via a **monomorphic map
shape**. But that win depends on map-shape type feedback, and the T1 profiler
carries *only* a small-int argument bitmask (`ErtsT2Profile`, `t2_retain.h:287`:
`count`, `nonsmall`, stability) — **no map-shape, no call-target, no branch
counters**. The full type-feedback design in `02_profiling.md` was never
implemented. So "implement map opcodes" is really "implement map opcodes +
isel/emit/deopt for them + the map-shape profiler the win depends on." The bill
is far larger than the opcode list implies.

**Net:** building to find out costs the full frontend (memo 14 §6's "much
bigger bet than rung-2") *before* it yields a single data point, and the
evidence already bounds the payoff low. That is the inverse of the M0 discipline.

## 3. The cheap experiment that prices the pool: an addressable-share census

A **frontend-only** measurement — no isel, no emit, no regalloc, no profiler —
that converts §1's argument into a per-class number. Estimated **a few days**.

**Mechanism.** `erts_t2_eligibility_scan` (`t2_eligible.c:319`) already walks
every generic op of every function. Today it drops a function to ineligible on
the first unsupported op. Change the *census build* (guarded, measurement-only)
to instead:

1. For each function, record the **set of unsupported opcode classes** its body
   contains (bucket each rejecting op into `call_fun` / maps / bs-construction /
   bs-position / exceptions / receive / float-reg / general-BIF / other), rather
   than stopping at the first.
2. Join that against **eprof own-time** for the workload (already collected in
   `realsvc/*_eprof.txt`), attributing each function's own-time to its blocking
   class-set.
3. Report, per workload and aggregated:
   - **Marginal coverage:** own-time that becomes eligible if class C is added
     *alone* (functions whose *only* blocker is C).
   - **Union coverage:** own-time eligible if the whole set {maps, `call_fun`,
     bs-construction, bs-position} is added.
   - The **entanglement matrix:** for the top-N functions, which classes co-occur
     (reproduces §2.1 quantitatively, over the full corpus not a hand table).

**What it settles.** The *addressable* ceiling per class and for the union — the
absolute upper bound on service value from more opcodes, assuming (generously)
that every eligible function then *wins*. If the union ceiling is small, the
"build the frontend" question is closed with a number. If it is large, proceed
to the realizable check below — still no full build.

**Realizable check (only for a class that clears the census bar).** One
hand-written prototype of the top function in that class (memo-13 method): does
T2 *remove work* there, or is it eligible-but-irreducible / eligible-but-slower
(the `lex_wl` outcome)? For maps specifically the prototype must also assume the
map-shape profiler (§2.3) — so cost it in. This gates the frontend build on
realizable, not addressable, value.

**Decision rule.** Build the service frontend only if **both** (a) union
addressable coverage is materially above the specialist floor (propose ≥ 15 %
whole-workload on at least one canonical service) **and** (b) the realizable
prototype shows work actually removed on that class's hot function. Either
misses ⇒ do not build; record "0 % on services" as *measured*, not argued.

## 4. The corpus gap — where a surprise, if any, lives

Memo 14's corpus (JSON, HTTP/1.1, binary frame codec, base64) is **entirely
byte-slinging** — construct/transform output bytes — which is T2's *worst* case
for this question (everything hits bs-construction). It says nothing about a
**map/term-heavy** service class: config/routing layers, in-memory KV, ETS-record
munging, protobuf/term→map decoders. Those spend their time in `get_map_elements`
+ comparison + arithmetic, *not* binary construction, and are exactly where map
shape specialization (§2.3) could remove real work. **Add one such workload to
the census corpus** (e.g. a map-lookup-dominated router or an ETS-record
transform) before concluding. This is the one place I would not pre-judge.

## 5. Recommendation

- **Do not implement more opcodes to find out.** It inverts the M0 discipline,
  is non-stageable, and the evidence already bounds the payoff low.
- **Run the addressable-share census (§3)** — frontend-only, days — to replace
  memo 14's argument with a per-class number, and **broaden the corpus (§4)**
  with one map/term-heavy service.
- **Prototype only map-shape specialization**, and only if the census surprises
  on the map class — costing in the map-shape profiler the win depends on.
- This is consistent with memo 16's **Option A + P3**: keep the specialist tier,
  and put broad-speedup effort on the VM-internal / GC track where the service
  cycles actually live — now backed by a measurement instead of an inference.

## 6. Reproducibility sketch

```
# census build (measurement-only guard around the eligibility scan):
#   T2_ELIG_CENSUS=1 -> per-function unsupported-class set, no compile
#   dump: fn_index, module:fun/arity, {classes}, size
T2_ELIG_CENSUS=1 $ERL +JT2enable true -noshell -pa realsvc \
    -eval 'json_wl:work(json_wl:setup()),halt().'   # (+ http/frame/b64/router)

# join with existing eprof own-time to get addressable share per class:
#   realsvc/*_eprof.txt  x  census dump  ->  marginal + union + entanglement
#   (offline join script; no VM change beyond the guarded census emit)
```

Artifacts to land under `realsvc/census/`: per-workload class dumps, the
own-time join, and the marginal/union/entanglement tables that make the
build/no-build call on §3's decision rule.
