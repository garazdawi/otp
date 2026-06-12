# T2 v1 verification — first results

Verification work for [`../T2/08_v1_loop_tier.md`](../T2/08_v1_loop_tier.md),
run 2026-06-12. Two items completed: the **trace-quiescence ordering
check** (08 §5 item 5's load-bearing assumption) and the first round
of the **effect-shape census** (08 §3/§4.2, the re-call scope risk), including
a dynamic call-weighted leg for dialyzer.

## 1. Trace-quiescence ordering — CONFIRMED

Claim (08 §5 item 5): `erlang:trace_pattern/3`'s return implies every
scheduler has passed a scheduling boundary after breakpoints commit,
so a process mid-execution in a T2 blob has yielded out before the
BIF returns.

Verified against the code: the BIF **suspends its caller**
(`erl_bif_trace.c:523–526`), runs the multi-stage breakpoint finisher
through `erts_schedule_code_barrier` (`code_ix.c:452–480`), where
each stage's "later op" only fires after **all schedulers** execute an
instruction barrier and thread progress advances — and thread
progress is updated **only at scheduling boundaries**
(`erl_process.c:9856`), never mid-timeslice. The caller is resumed
(`erl_bif_trace.c:590`) only after the final stage. So the ordering
T2 needs exists already; the jettison just has to be inserted in the
staging phase. The 16A forced test (enable trace against a mid-T2-loop
process) remains as the regression guard.

## 2. Effect-shape census — static results

Tool: [`effect_census.erl`](effect_census.erl) — disassembles `.beam`
code chunks (no debug_info needed), finds self-tail-recursive "loop
functions", buckets by the worst blocker present anywhere in the
function (conservative: whole-function, not loop-body-only):

- **A** — pure v1 op set (compiles under the original effect-free rule)
- **B** — + effectful BIF calls (compiles under the §4.2 window model)
- **I** — + `lists:*` higher-order call sites (P3 intrinsics)
- **C** — + coverage-blocked ops: `bs_*`/maps/floats/try/receive
  (blocked on op coverage, **not** on the deopt model)
- **D** — non-tail Erlang calls that aren't leaf-inlineable
  (incl. body recursion, `call_fun`, `apply`)

Corpora: OTP apps from this tree; RabbitMQ 4.3.1 release beams
(server + deps); Elixir 1.19.5 stdlib; a Phoenix 1.7 + LiveView +
Ecto + Ash 3.4 + Bandit/Cowboy dep tree compiled fresh.

| corpus | funcs | loops | A | B | I | C | D | A+B+I share |
|---|---|---|---|---|---|---|---|---|
| rabbitmq | 36086 | 5177 | 213 | 73 | 2 | 1711 | 3178 | 5.6 % |
| phoenix_ash | 48783 | 1201 | 113 | 17 | 0 | 280 | 791 | 10.8 % |
| elixir_std | 14329 | 820 | 123 | 10 | 0 | 91 | 596 | 16.2 % |
| dialyzer | 2683 | 455 | 38 | 6 | 2 | 5 | 404 | 10.1 % |
| stdlib | 10107 | 1858 | 415 | 46 | 7 | 118 | 1272 | 25.2 % |
| compiler | 5347 | 1061 | 137 | 14 | 5 | 56 | 849 | 14.7 % |
| kernel | 5222 | 690 | 90 | 35 | 3 | 66 | 496 | 18.6 % |
| ssl | 2768 | 230 | 26 | 8 | 2 | 26 | 168 | 15.7 % |
| mnesia | 2313 | 362 | 35 | 27 | 0 | 11 | 289 | 17.1 % |

Sub-reason highlights:

- **RabbitMQ's C bucket is binaries**: 2377 of 5177 loop functions
  (46 %) touch `bs_*` ops — the G-bin thesis, confirmed at corpus
  scale. Maps flags: 661.
- **Elixir corpora are loop-function-poor** (2.5 % of functions vs
  ~18 % for OTP stdlib) and D-heavy with `d_remote` dominant —
  iteration goes through `Enum`/protocols, i.e. the cross-module
  inlining story (H11/G3+).
- **D dominates everywhere statically** (60–89 % of loop functions),
  split roughly evenly between local calls, remote calls, and body
  recursion.

## 3. Dynamic leg: dialyzer under cprof — the decisive number

A real workload (PLT build over the compiler app's beams) run under
`cprof` — call counts weight tail-recursive loops by *iterations*,
since every back-edge re-enters through the function entry.
2 423 378 803 calls counted, joined against the census buckets:

| where calls landed | share of all calls |
|---|---|
| loop functions, bucket **D** | **47.0 %** |
| loop functions, bucket **A** | 6.3 % |
| loop functions, bucket B / I / C | 0.25 % combined |
| non-loop functions | 46.5 % |

Top hot loop functions are all D: `erl_types:are_all_limited/2`
(281 M calls), `is_limited/2` (264 M), `t_has_var/1`+`t_has_var_list/1`
(126 M) — **mutually recursive structural walkers**: a tail-recursive
list spine that body-calls a helper per element, where the helper
recurses into term structure. Leaf inlining doesn't capture these
(the helpers recurse); demote-on-return covers only the trivial
spine prefix. The hot non-loop functions tell the same story
(`erl_types:oc_mark/3` 124 M, `t_inf/2`, `t_sup/2`), plus a real but
modest showing for v1's leaf class (`cerl:type/1` + `cerl:get_ann/1`
≈ 42 M calls ≈ 1.7 %).

**Honest v1 ceiling for dialyzer-shaped code**: the A bucket is
6.3 % of dynamic calls; even a 2× speedup on all of it is ~3 %
end-to-end. Consistent with 08 §6's "low single digits on the
application corpus" prediction — now with data.

### Caveats

- cprof counts calls, not cycles: a 5-op leaf and a 500-op body
  count equally. Cycle (perf) profiling — the P0 task — can shift
  shares; it will not change the D-dominance qualitatively, since
  the D functions are also the cycle-heavy ones here.
- One workload of one app. RabbitMQ/MongooseIM under load (the
  awfy macro leg) is the other half and will weight binaries and
  runtime services far more heavily.
- Whole-function bucketing is conservative for A/B (loop bodies may
  be cleaner than the function), and the census can't see
  literal-fun-ness for the I bucket.

## 4. What this changes

1. **The S2/window-model soundness question is settled in S2's
   favour, but it was the wrong question.** Effects-in-loops (B) are
   a rounding error both statically and dynamically. The pre-BeamAsm
   objection materialises as the *call* problem: hot Erlang loops
   call things (D), and `gen_server:cast` is a call before it is an
   effect.
2. **G3 broadens and gains priority.** The branchy-dispatch
   experiment is now paired with a **mutual-recursion experiment**
   drawn straight from the census: hand-write a T2 specialisation of
   `erl_types:are_all_limited/2` + `is_limited/2` (inline one level
   of the mutual recursion, fuse the spine guards, measure on the
   PLT-build workload). If call-crossing optimization can't move
   that shape, dialyzer-class code is out of reach regardless of
   infrastructure, and the loop tier's expansion budget should go to
   G-bin/G-map instead.
   *Update: the mutual-recursion experiment ran — no end-to-end
   win (0 ± 1 % CPU); see [`G3_OUTCOME.md`](G3_OUTCOME.md). The
   call-count weighting used in §3 above is now known to over-weight
   tiny-body functions ~10×; treat this section's dynamic shares as
   structure, not cycle pools.*
3. **G-bin's corpus-scale weight is confirmed** for RabbitMQ-class
   code: half its loop functions touch binaries.
4. **The v1 loop tier remains the right beachhead** — it's the
   smallest thing that exercises install/deopt/lifecycle end-to-end
   on real wins (stdlib A bucket: 415 functions; awfy loop class) —
   but the census kills any expectation that it moves application
   corpora by itself. The expansion gates are where the application
   wins live, in this order of evidence: binaries (RabbitMQ),
   call-crossing (dialyzer, Elixir), maps (Elixir/Phoenix).

## Reproduction

```bash
# static census (repo toolchain; use asdf escript for Elixir-built beams)
./bin/escript PLAN/verification/effect_census.erl \
    rabbitmq:/path/to/rabbitmq_server-4.3.1/plugins \
    stdlib:lib/stdlib/ebin dialyzer:lib/dialyzer/ebin ...

# dynamic leg
# (PLT build over compiler beams under cprof; ~2 min)
./bin/escript /tmp/census_dyn.erl   # see RESULTS history / 08 §6
# join: awk over /tmp/effect_census_detail.tsv + /tmp/census_dialyzer_counts.tsv
```
