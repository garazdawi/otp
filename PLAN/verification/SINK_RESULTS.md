# M0.2 — The sinkable-allocation pool

> Measurement M0.2 of [`PLAN/T2FULL/06_phases.md`](../T2FULL/06_phases.md):
> a static sinkability classifier over every heap-allocation site in hot BEAM
> code, weighted by the measured per-function allocation volumes of
> [`GALLOC_RESULTS.md`](GALLOC_RESULTS.md), sizing the pool that funds phase
> **P5** (escape analysis / allocation sinking,
> [`03_optimizer.md`](../T2FULL/03_optimizer.md) §6). Sibling of M0.1's
> elimscan; same SSA extraction and cross-module resolution machinery.
>
> Tools: `sinkscan.erl` (classifier), `sinkdrv.erl` (aggregation/census),
> `run_sink.escript` (.erl corpus driver), `run_beam_sink.escript`
> (Elixir/.beam leg driver via `debug_info` → abstract forms →
> `compile:forms` → SSA), `banditleg.escript` / `jasonleg.escript` /
> `rabbitleg.escript` (GALLOC-weighted legs).

## Headline

**The classic region-local sinkable pool does not fund P5 at the plan's
expected yield.** On the GALLOC-measured workloads, allocation volume from
sites that escape analysis could sink (T1-local) or that narrow P3 inlining
would make sinkable (T2-narrow-inline) is:

- **thin-Bandit JSON API: ≈ 0 %** of heap volume (89.2 % of the measured
  volume covered; t1+t2 = 0.0 % of covered — 46 % is BIF-internal
  (`maps:from_list`), ~31 % is Jason decode/encode whose hot-path
  allocations all escape into the result document or the decoder's
  continuation stack).
- **RabbitMQ MQTT broker: ~3 %** of heap volume (43.5 % covered at
  t1 = 3.2 %, t2 = 0.0 % within covered; extrapolated to the diffuse tail).
- **Structural ceiling (whole 312-module OTP corpus): 7.9 %** of exact
  allocation words (t1 0.6 % + t2 7.3 %) — an upper bound that assumes
  every construct/deconstruct shape is realized by inlining.

Through the plan's own Amdahl chain (03 §6.4: sinkable-volume-share ×
GC-share ≈ 12 %), the end-to-end yield is **≈ 0 % on the thin-JSON class,
~0.4 % on the broker class, < 1 % even at the structural ceiling** — far
below the 2–6 %-on-services expectation. §7 gives the reading: the pool the
plan hoped for is not absent because escape analysis is weak, but because
(a) the AOT compiler already harvests trivially-local pairs, (b) BEAM's
functional style makes most allocation *the result* (it genuinely escapes),
(c) the biggest measured allocators are C-BIF-internal, and (d) the
remaining temporaries thread through tail-recursive accumulators, reachable
only by whole-loop fusion (P5b + deep inlining), not by P5a.

---

## 1. What is measured, and how

For every **allocation site** in optimized SSA (post-`beam_ssa_opt`, the
representation the loaded-BEAM Type chunk serializes — same pipeline and
justification as M0.1), the classifier follows the SSA uses of the
allocated value and assigns a tier:

| tier | meaning | criterion |
|---|---|---|
| **t1** (T1-local) | sinkable by region-local escape analysis today | every use is local consumption: `get_tuple_element`/`get_hd`/`get_tl`/`get_map_element`, type tests, comparisons, `match_fail`, branch/switch dispatch; or flows into another constructor that is itself t1 (followed **one** level) |
| **t2** (T2-narrow-inline) | becomes t1 under the plan's narrow P3 inlining | escapes **only** as (a) a fresh return value whose resolvable callers all immediately destructure it (the `{ok,X}` pair), or (b) an argument whose resolvable callee only destructures that parameter |
| **esc** (escaped) | not sinkable at region scope | everything else: sent, stored, thrown, returned to unknown callers, closure-captured, passed to non-destructuring/opaque callees, deep-flowing (> 1 constructor level), loop-carried phi |

Allocation ops and word accounting (64-bit words):

| op | category | words |
|---|---|---|
| `put_tuple` | tuple | arity + 1 |
| `put_list` | cons | 2 |
| `update_record` | record | size + 1 |
| `put_map` | map | approx (N pairs → ~N+2; flatmap keys shared) — **excluded from exact-word totals**, shown separately |
| `make_fun` | fun | env + fixed ≈ 6+env — approx, shown separately |
| `bs_create_bin` | bin | **unknown** (run-time sized) — reported by site count |

"Exact words" below = tuple + cons + record only. Cross-module resolution:
callee/caller signatures index the whole compiled corpus per leg (M0.1
machinery); anything unresolvable classifies `esc`.

## 2. Approximations (every one, per the honesty rule)

1. **Static substitution for idea-#50.** The plan's M0.2 row names dynamic
   term-lifetime instrumentation. This measurement substitutes a **static
   region-locality classifier × measured dynamic volume weights (GALLOC)**.
   Justification: GC_RESULTS already established the garbage dies young
   (survival 0.18–0.39) — "dies young" is abundant and is *not* the scarce
   resource. What decides P5 sinkability is **region-locality of the
   value's uses**, which is exactly what this classifies; GALLOC supplies
   the dynamic volume weighting per function.
2. **Within-function weighting is static.** GALLOC weights are
   per-function; inside a function, sites on error paths count the same as
   hot-path sites. The top functions of every leg were hand-spot-checked
   (§3–§5): on each, the hot-path allocations are `esc` (accumulator/stack
   threading, result building), so the conclusion is not an artifact of
   error-path noise.
3. **t1 is a floor; t2 is a soft ceiling of the narrow-inline shape.**
   Depth-1 constructor following sends deep flows to `esc` (10 954 sites,
   ~19 % of esc — conservative). Conversely `t2 narrow_ret` requires ≥ 1
   in-corpus caller, all destructuring — but cannot see out-of-corpus
   callers of exported functions (optimistic). M0.1's *realized*
   construct/deconstruct rate at call sites was 0.4–1.0 %, consistent with
   most of t2 not being realized by Maglev-budget inlining.
4. **BIF-internal allocation is invisible to SSA and unsinkable by P5**
   (`maps:from_list`, ETS, port I/O buffers, `put_map`'s hashmap-side
   internals). It is counted via GALLOC weights in dedicated
   `BIF-internal`/`runtime-internal` buckets, never dropped silently.
5. **Elixir legs.** Jason compiled from github master with Elixir 1.19.5 on
   OTP 29 (GALLOC ran the Mix-installed release on 1.19.2/OTP 29) —
   labelled version drift, decoder structure unchanged. Elixir stdlib
   classified from the installed 1.19.5 beams via `debug_info`.
   `String.Unicode`/`String` classify the real modules (no substitution).
   The planned json.erl-substitution fallback was **not** needed.
6. **RabbitMQ leg compiled standalone from source** (v4.3.1 tag + headers,
   `amqp10_framing.hrl` from the hex `amqp10_common` 4.2.1 package) inside
   the OTP corpus for stdlib resolution; calls into other rabbit modules
   are opaque → esc (conservative). All 6 hot modules compiled; **no
   erl_syntax downgrade was needed**. `parse_remaining_len` is /3 in
   v4.3.1 source (GALLOC's JIT frame said /5) — substitution labelled.
7. **`maps:from_list` 46 % is itself partly benchmark-thinness** (GALLOC's
   own caveat: 1.2 % under Ash, absent from RabbitMQ). Both diffuse
   workloads are covered as their own legs.
8. **Phi uses**: phi arguments are `{Value, Block}` pairs; the classifier
   unwraps them (a first version did not — that inflated t1 to 3.2 % of
   sites via phantom "dead" allocations; fixed, all legs rescanned. The
   synthetic validation suite catches both directions.)
9. **beam_ssa_opt already harvests the trivially-local pool.** Verified
   directly: a tuple built and destructured in one function body compiles
   to *no allocation at all* (scalar-replaced by the AOT compiler). What
   this tool counts is the residual available to a JIT — the mirror of
   M0.1's "intra-module subsumption is already gone" finding.

## 3. Leg 1 — stdlib, GALLOC-volume-weighted (thin-Bandit; highest fidelity)

Every GALLOC row ≥ 0.9 % of the thin-Bandit heap volume, mapped to its
classified function(s) (`banditleg.escript`):

| GALLOC row | weight % | t1 % | t2 % | esc % | note |
|---|---|---|---|---|---|
| `maps:from_list/1` | 46.05 | — | — | — | **BIF-internal**, no SSA sites; not P5-addressable |
| `Jason.Decoder:object/6` | 9.98 | 0 | 0 | 100 | stack/acc conses + error tuples |
| `Jason.Decoder:key/6` | 6.16 | 0 | 0 | 100 | |
| `Jason.Decoder:array/6` | 5.03 | 0 | 0 | 100 | |
| `String.Unicode:upcase/3` | 4.10 | 0 | 0 | 100 | 126 cons + 60 bin sites, result building |
| `prim_inet:send/4` | 3.61 | — | — | — | **runtime-internal** (port path), 0 SSA sites |
| `Jason.Decoder:key/5` | 3.01 | 0 | 0 | 100 | |
| `Jason.Decoder:value/5` | 2.33 | 0 | 0 | 100 | |
| `Jason.Encode:encode_string/2` | 1.62 | 0 | 0 | 100 | returned iodata |
| `lists:reverse/1` | 1.48 | 0 | 0 | 100 | (+BIF `reverse/2` internals) |
| `Jason.Encode:list_loop/3` | 1.41 | 0 | 0 | 100 | |
| `Jason.Encode:escape_json_chunk/5` | 1.25 | 0 | 2.8 | 97.2 | |
| `erts_internal:garbage_collect/1` | 1.13 | — | — | — | forced-GC artifact (GALLOC caveat) |
| `String:downcase_ascii/1` | 1.11 | 0 | 0 | 100 | |
| `Enum.map` (= `lists:map/2`) | 0.93 | 0 | 0 | 100 | |

**Covered: 89.2 % of the thin-Bandit heap volume. Volume-weighted sinkable
share within covered: t1 = 0.0 %, t2 = 0.0 % (0.035 % of total volume, one
t2 site).** The thin-API class — the most JIT-friendly, most
allocation-concentrated workload measured — has **no P5-classic pool**: its
volume is a C BIF plus result construction that genuinely escapes.

Structural per-module context (exact words; `sinkdrv` census):
`maps.erl` 0/0/100, `json.erl` 4.0/0.7/95.3, `lists.erl` 0/9.2/90.8
(t2 = the `{ok,X}`-shaped returns of `keyfind`-class functions),
`string.erl` 1.2/1.2/97.6, `base64.erl`/`unicode.erl` 0/0/100.

## 4. Leg 2 — Jason (real Elixir modules; GALLOC weights)

All 27 Jason beams (github master, elixirc 1.19.5, debug_info → erlang_v1
forms → optimized SSA; 2 018 sites, zero failures). GALLOC-named hot
functions (30.8 % of thin-Bandit volume): **t1+t2 = 0.1 % within covered**
(table above; `jasonleg.escript`).

Whole-module structural: `Jason.Decoder` 1 058 sites, 2 136 exact words —
**0.0 / 0.0 / 100.0**; `Jason.Encode` 512 sites, 1 031 words —
**0.0 / 3.4 / 96.6**.

The shape, verified by hand on `object/6`, `key/6`, `array/6`, `value/5`:
every hot-path allocation is either (a) part of the decoded document
(escapes by definition — it *is* the result), or (b) a cons/tuple pushed
onto the decoder's **continuation stack, threaded through the mutually
recursive state functions** (`esc call_arg`). (b) dies young dynamically
but is invisible to region-local escape analysis: the callee does not just
destructure it, it threads it onward. Sinking it requires the *whole
decode loop* fused into one region with the stack kept virtual across
iterations — the PyPy-virtuals shape (P5b + loop recovery + inlining
depth beyond Maglev budgets), not P5a. Static site counts within the
module are dominated by `escapeu/6` (987/1 058 sites — the generated
`\uXXXX` escape table, rarely hot), which is why the GALLOC-named
function weighting, not module site counts, is the number that matters.

## 5. Leg 3 — RabbitMQ (best-effort, achieved at full fidelity)

The 6 GALLOC-hot modules (v4.3.1: `rabbit_db_topic_exchange`, `mc`,
`mc_mqtt`, `rabbit_mqtt_packet`, `rabbit_mqtt_qos0_queue`,
`rabbit_queue_type`) compiled standalone into the OTP corpus — 487 sites.
GALLOC-weighted (43.5 % of broker volume covered; `rabbitleg.escript`):

| GALLOC row | weight % | t1 % | t2 % | esc % |
|---|---|---|---|---|
| `trie_match_try/9` | 9.38 | 0 | 0 | 100 |
| `rabbit_mqtt_packet:parse_packet/4` | 6.40 | 8.8 | 0 | 91.2 |
| `mc:set_annotation/3` | 5.62 | 0 | 0 | 100 |
| `rabbit_mqtt_qos0_queue:deliver/3` | 4.48 | 12.5 | 0 | 87.5 |
| `rabbit_mqtt_packet:parse/2` | 4.42 | 0 | 0 | 100 |
| `rabbit_queue_type:-deliver0/4-fun-4-/3` | 3.44 | 0 | 0 | 100 |
| `parse_remaining_len/3` (see §2.6) | 3.13 | 0 | 0 | 100 |
| `trie_match/7` | 2.46 | 0 | 0 | 100 (4 make_fun closures) |
| `match/3` | 2.17 | 0 | 0 | 100 |
| `mc_mqtt:protocol_state/2 + init/1` | 2.03 | 13.6 | 0 | 86.4 |

**Volume-weighted within covered: t1 = 3.2 %, t2 = 0.0 %.** The t1 hits
are the genuine sinkable classic: 3-word tuples used as multi-value
merges/local matches inside parse and delivery functions. The escapes are
the broker's essence — message containers, annotations maps and trie-walk
state that outlive any region. Whole-module structural t2 exists
(`topic_exchange` 11.3 %, `queue_type` 7.2 % of words) in the `{ok,X}`
return shape. Extrapolating covered→total at the same rate: **~3 % of
broker allocation volume is P5a+narrow-P3 sinkable.**

## 6. Whole-corpus structural census (312 OTP modules, ceiling view)

58 359 allocation sites; exact words 177 219 (tuple 26 198 / cons 24 438 /
record 2 721 sites; map 1 497, fun 2 861, bin 644 sites separate):

| view | t1 | t2 | esc |
|---|---|---|---|
| exact WORDS (tuple+cons+record) | **0.6 %** | **7.3 %** | 92.1 % |
| sites | 0.6 % | 6.4 % | 93.0 % |
| all-approx words (+map+fun) | 0.5 % | 6.6 % | 92.9 % |
| stdlib words | 0.5 % | 7.1 % | 92.4 % |
| kernel words | 0.4 % | 5.2 % | 94.4 % |

Escape reasons (sites): call-argument 22 805, returned-to-unknown 20 143,
deep-flow 10 954 (conservative depth-1 cap), dynamic call 168, closure
capture 79. t2 composition: narrow-return 1 869, via-constructor 1 201,
narrow-argument 659. Category tiers: **bins 638/644 esc, funs 2 831/2 861
esc, maps 1 424/1 497 esc** — closures, binaries, and maps have essentially
no local pool.

**M0.4 overlap — float boxing:** `{float,put}` (the SSA float-box op)
appears **91 times in the whole corpus** — 42 in `rand`, 0 in every hot leg
(json, maps, lists, Jason, all rabbit modules). Bignum arithmetic
allocates inside BIFs (invisible/unsinkable). **The float-unboxing pool on
the measured service/compute corpora is nil**; it remains a
numeric-kernel-only shape and should not be funded from service
expectations (M0.4 can close on cycle profiles alone).

## 7. Reading for P5 — does the pool support 2–6 % on services?

**No — not through the classic P5a chain.** 03 §6.4's arithmetic,
(sinkable volume share) × (GC share ≈ 12 %), evaluated on the measured
legs:

| workload class | sinkable volume (t1+t2) | × GC 12 % → e2e |
|---|---|---|
| thin JSON API (Bandit, measured) | ≈ 0.0 % | **≈ 0 %** |
| broker (RabbitMQ MQTT, measured) | ~3 % | **~0.4 %** |
| structural ceiling (all t2 realized) | 7.9 % | **< 1 %** |

Direct allocation-instruction savings (the removed `put_*`/`test_heap`
work itself) add fractions of these, not multiples. Even granting the
optimistic end everywhere, P5a lands **an order of magnitude below the
2–6 % band**. The Graal/PyPy calibrations do not transfer because the
BEAM baseline differs structurally:

1. **The AOT compiler already sank the easy pool** (§2.9) — Java/Python
   baselines carry it into the JIT; `beam_ssa_opt` does not leave it.
2. **Functional style makes allocation the product.** The dominant escapes
   are the result being built (`ret_unknown` + accumulator `call_arg`) —
   no analysis can sink the return value of `lists:reverse` or a decoded
   JSON document.
3. **The single biggest measured allocator is a BIF** (`maps:from_list`,
   46 % thin-API) — outside SSA entirely.
4. **The dies-young temporaries that do exist thread through
   tail-recursive state machines** (Jason's continuation stack, M0.3's
   per-stage Enum intermediates). They become region-local only inside
   fused loops with values kept virtual across iterations — the
   **P5b-under-loop-fusion** shape, whose enabling cost (deep inlining +
   loop recovery + framestate virtuals) the plan priced separately.

**Funding recommendation (G-M0 input):**

- **Do not fund P5 as a standalone 8–12-week phase on this evidence.**
  Fund **P5a-lite** inside P3/P4: construct/deconstruct fusion at inlined
  boundaries (the t2 shape — it is the same machinery the inliner's
  scoring already finds) plus deopt-dead sinking of the small t1 pool.
  Expected yield: the < 1 % ceiling above; cheap because P3 builds the
  scaffolding anyway.
- **Attack the measured pools where they actually are:** (a) the
  **G-map/`maps:from_list` shape** — a consuming intrinsic that builds the
  map directly from the pair-list (or fuses with a decoder producing
  pairs) attacks the 46 % thin-API pool that P5 cannot see; this is a
  VM/intrinsic track, not escape analysis. (b) **binary construction**
  (`bs_create_bin` esc-dominated; Jason.Encode/upcase iodata) routes
  through the loop tier's binary package, not P5.
- **P5b (framestate virtuals) stays unfunded** until a fused-loop
  prototype exists; then **re-run this classifier on fused-region IR**
  (the tool takes any SSA) to re-price the accumulator-threading pool —
  that is the one place a PyPy-shaped win could still hide.
- The service-class ≥ 10 % target must therefore lean on pillars 1
  (fusion) and 2's narrow form — consistent with M0.1's verdict and
  M0.3's Enum finding; the three-pillar compounding thesis loses its
  third pillar as originally sized.

## Reproduction

```
cd PLAN/verification/sinkscan
erlc -I $(erl -noshell -eval 'io:format("~s",[code:lib_dir(compiler)])' )/src sinkscan.erl sinkdrv.erl
# leg 1 + census (OTP source checkout):
escript run_sink.escript <otp_root> sink_corpus.eterm
erl -noshell -pa . -eval 'sinkdrv:report("sink_corpus.eterm"), halt().'
# leg 2 (Jason at github master, elixirc with debug_info):
elixirc -o jason_ebin <jason/lib/*.ex>
escript run_beam_sink.escript <elixir_ebin> sink_jason.eterm jason_ebin
# leg 3 (rabbit v4.3.1 hot modules + headers fetched per SINK_RESULTS §5):
escript run_sink.escript <otp_root> sink_rabbit.eterm rabbit=<src> i1=<incs...>
# weighted legs:
escript banditleg.escript sink_corpus.eterm sink_jason.eterm sink_exstdlib.eterm
escript jasonleg.escript sink_jason.eterm
escript rabbitleg.escript sink_rabbit.eterm
```
