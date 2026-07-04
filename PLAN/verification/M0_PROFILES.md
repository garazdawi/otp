# M0.3 + M0.7 — perf DSO/family split of the compute-application corpus

Second-round follow-ups to
[`PROFILE_RESULTS.md`](../verification/PROFILE_RESULTS.md), run 2026-07-04.
Same instrument as that doc's "perf in colima" section (Linux `perf`,
`cpu-clock`, `+JPperf map` resolving JIT frames by name), applied to the two
measurements the prior round left open:

- **M0.7** — split dialyzer's 75.8 % emulator share into JIT-compiled Erlang
  code vs C (BIFs/GC/runtime). Plus the Elixir compiler as a second
  compute-application exemplar.
- **M0.3** — the Elixir `Enum`/`Stream`/protocol/`lists` wrapper weight, on the
  Elixir compiler (the assigned workload) and on a maximally-Enum idiomatic
  pipeline (an upper bound).

## Method

Environment: colima aarch64 (macOS Virtualization.framework), 4 vCPU, 15 GB,
Ubuntu VM, kernel 6.8, `kernel.perf_event_paranoid=-1`. Everything runs in
`--privileged` `erlang:29` containers (OTP 29.0), scripts piped via stdin (the
macOS `/tmp` is not shared into the VM). `perf` = the Debian `linux-perf`
package's `/usr/bin/perf` (v6.12.94) — it works for `cpu-clock` software
sampling; no hardware PMU in the VM.

Sampling: `perf record -F 499 -e cpu-clock`. Two attach modes, chosen per leg:

- **per-pid** (`-p <beam.smp>`) — used for the compute legs. `cpu-clock -p`
  accrues only while the beam's scheduler threads are *on-CPU*, so idle time is
  excluded and the split is the true on-CPU composition of the workload. This is
  the correct mode here (an early system-wide `-a` run mis-read as "50 % kernel"
  what was actually `cpuidle_idle_call` — the parallel compiler leaves cores
  idle at its serialization points).
- JIT frames resolve via `/tmp/perf-<pid>.map` written by `+JPperf map`. Verified
  frames appear as `$module:function/arity` (Erlang) and `$'Elixir.Mod':fun/arity`
  (Elixir — note the single-quoted module atom), never hex.

Flag delivery: dialyzer is an escript → `ERL_FLAGS="+JPperf map"`. Elixir/mix →
`ELIXIR_ERL_OPTIONS="+JPperf map +fnu"` (`+fnu` because the precompiled Elixir
1.19.0 `elixir-otp-28` build runs on OTP 29).

Workloads:

| leg | workload | driver | wall / samples |
|---|---|---|---|
| M0.7-dia | `dialyzer --build_plt` (19 OTP apps: erts…os_mon) | self-driving, +S4 | 27 s / ~34 k |
| M0.7-dia' | `dialyzer --build_plt --apps erts kernel stdlib` (spec cross-check) | self-driving | 9 s / ~10 k |
| M0.7/M0.3-exc | `mix deps.compile` of a Phoenix/Ecto/Absinthe/LiveView tree (~554 `.ex`) | Elixir 1.19.0 on OTP 29 | 8 s / ~8 k |
| M0.3-enum | idiomatic `Enum`/`Stream`/protocol pipeline over 30 k structs, 18 s loop | compute-bound, empty msgq | 14 s / ~8 k |

Reading the split: `perf`'s `beam.smp` bucket is **all C** and *includes GC*
(`do_minor`, `sweep_new_heap`, …), which msacc scores as its own state. So
`beam.smp` ≠ msacc "emulator". The JIT bucket is pure compiled Erlang/Elixir —
the only surface a second tier can re-JIT. GC symbols are broken out below where
the emulator-vs-JIT reconciliation needs them.

---

## Leg 1 — M0.7: dialyzer DSO split

### DSO split (% of on-CPU samples)

| layer | dialyzer (19 apps, 27 s) | dialyzer (erts+kernel+stdlib, 9 s) |
|---|---|---|
| `beam.smp` (C: BIFs + GC + term plumbing + ETS) | **53.1 %** | 54.9 % |
| `[JIT]` (all compiled Erlang code) | **33.6 %** | 36.5 % |
| `[kernel]` (beam-file I/O, page faults) | 9.9 % | 5.2 % |
| libc | 2.6 % | 2.6 % |

The JIT (addressable) share is stable at **34–36 %** across build sizes. The
larger app set carries more kernel (reading ~200 more `.beam` files + page-clear
for a bigger PLT) — that share is I/O, not compute.

### The two big buckets, top symbols (% of all samples)

**`[JIT]` = compiled Erlang code, aggregated by module family** (top-200 symbols
captured; the true JIT total is 33.6 %, so each family is a *floor*):

| JIT family | share | note |
|---|---|---|
| `erl_types` (type-lattice traversal) | **≥13.2 %** | `is_limited`, `t_inf_aux`, `oc_mark`, `are_all_limited`, `t_has_var` — the **G3-2 family** |
| `lists`/`maps`/`sets`/`ordsets` (Erlang containers) | 3.2 % | |
| `dialyzer_*` engine (`typesig`, `codeserver`, …) | 2.9 % | |
| `cerl`/`cerl_trees` (Core Erlang AST) | 1.9 % | |
| `$global::` JIT runtime glue (`call_light_bif_shared`, map-element, arith) | 4.6 % | BIF dispatch stubs, not app logic |

**`beam.smp` = C runtime, top symbols:** `sweep_new_heap` 6.9, `do_minor` 6.8,
`eq` 6.8, `full_sweep_heaps` 3.9, `erts_cmp_compound` 3.0, `copy_shallow_x` 2.7,
`db_get_element_hash` 2.5 (ETS), `dec_term` 2.4 + `enc_term_int`/`decoded_size`
~1.7 (PLT term_to_binary serialization), `make_internal_hash` 1.1,
`erts_maps_put` 1.3, `copy_struct_x`/`size_object_x` ~0.8. **GC symbols sum to
≈17.8 %**; the rest is term compare/hash/copy, ETS, and PLT serialization.

### Reconciliation with the plan's guess

`00_goal_and_thesis.md` §1.1 guesses the 75.8 % emulator is "*~40–55 % JIT code
(rest is C BIFs + GC)*". Removing GC (17.8 %), kernel (9.9 %) and libc (2.6 %)
from the total leaves an emulator-compute band of ≈68.9 %, of which JIT is
33.6 % → **JIT is ≈49 % of the non-GC emulator — squarely mid-band. The guess
holds for dialyzer.** As a fraction of *total* CPU the addressable surface is
**≈34 %**.

### Implications for T2FULL (M0.7)

The §1.1 compute-application arithmetic **holds, at the pessimistic end**:

- Addressable = 34 % of total CPU. End-to-end from a uniform speedup `s` on all
  JIT code, at 100 % eligibility: `1/((1−0.34)+0.34/s)`. **`s = 2.0×` → 1.20×
  (20.5 %)**; `s = 1.6×` → 14.6 %; `s = 1.3×` → 8.5 %. So 20 % on dialyzer
  requires a **~2× on all Erlang code at ~100 % eligibility** — the plan's
  "stretch, requires all three pillars compounding *and* high eligibility" is
  exactly right, now quantified. At a realistic 70 % eligibility even `s = 2×`
  yields ~13.5 %, i.e. the honest **5–15 %** primary target is what the number
  actually supports.
- **Caveat the arithmetic hides:** ~13–17 % of total CPU (≈40–50 % of the whole
  JIT pool) *is* `erl_types` — the pool the G3-2 specialization experiment
  already harvested **0 ± 1 %** from (containers dominate, leaf gains net against
  container losses per `G3_OUTCOME.md`). The addressable pool is real but its
  largest coherent piece has a measured null against a prior optimizer. The
  general inliner's case rests on the *elimination-rich* boundaries M0.1 prices,
  not on re-attacking `erl_types` leaves.
- **NIF boundary (open-Q #4):** no NIF pool in dialyzer — crypto/asn1 NIFs never
  surface; nothing to region-split for here.
- The one *coherent* non-JIT pool is GC (~18 %) + term serialization for the PLT
  (`dec_term`/`enc_term_int` ~4 %) — Pillar-3 (allocation) reaches the first;
  the second is intrinsic dialyzer I/O.

---

## Leg 2 — M0.3: Elixir compiler + Enum weight

### 2a. The Elixir compiler (`mix deps.compile`, per-pid)

DSO split (stable across two runs):

| layer | share |
|---|---|
| `beam.smp` (C) | **62.7 %** |
| `[JIT]` (compiled Erlang/Elixir) | **23.8 %** |
| `[kernel]` | 8.1 % |
| libc | 3.5 % |

**JIT bucket, by family (% of all samples):**

| JIT family | share |
|---|---|
| **Erlang backend compiler** (`beam_ssa*`, `beam_types`, `beam_validator`, `v3_core`, `cerl*`, `beam_asm`, `compile`) | **12.4 %** |
| `$global::` JIT runtime glue | 5.3 % |
| `sets`/`ordsets`/`gb_sets` (Erlang) | 1.5 % |
| `lists` (Erlang) | 1.3 % |
| `elixir_*` frontend (Erlang-written: tokenizer, expand) | 0.9 % |
| `Enum` / `Stream` / `Protocol` / `Elixir.Kernel` / `Macro` / `Module` | **≈ 0 %** |

**`beam.smp` C bucket:** `eq` 9.0, GC (`do_minor`+`sweep_new_heap`+`full_sweep`+
`any_heap_refs`) ≈15.7, `erts_cmp_compound` 5.2, map ops (`erts_maps_put`,
`erts_hashmap_*`, `get_map_element`) ≈12.6, `erts_ptab_processes_next` +
scheduler ≈5.9 (parallel-compiler coordination), `make_internal_hash` 1.1,
`copy_struct_x`/`copy_shallow_x` (inter-process AST copy) ~1.

**Headline: compiling Elixir is not an `Enum`/protocol workload.** A grep of the
full symbol table finds exactly one Elixir-stdlib frame — `$elixir_tokenizer`
(0.08 %). The cost is the **shared Erlang backend compiler** (`beam_ssa`
optimization + validation + codegen — the same code that compiles Erlang) plus
VM-internal term compare / map / GC. Elixir's frontend translates AST to Erlang
Abstract Format and hands it to `:compile`; that backend dominates, and the
Elixir-written stdlib (`Enum`, `Kernel` macros) is negligible in it.

As a compute-application the Elixir compiler is **harder than dialyzer**:
addressable JIT is only **23.8 % of total** (33.6 % of the non-GC emulator —
*below* the plan's 40–55 % band). End-to-end at `s = 2×`, 100 % eligibility →
only **13.5 %**; even an *infinitely* fast JIT caps at `1/0.762 = 1.31×`. 20 %
is not reachable on this workload; 5–15 % is, and only with the backend compiler
(`beam_ssa`) itself as the target — which is Erlang, so any win here helps
`erlc` too.

### 2b. Idiomatic Enum/Stream/protocol pipeline (the pool's upper bound)

To actually price the pool M0.3 names (the compiler gives ~0), a compute-bound
pipeline that does *nothing but* `Enum`/`Stream`/protocol over 30 k structs:
`filter → map → group_by → flat_map(sort) → Stream.map → Stream.filter →
reduce → reduce`, with a user protocol `Scorable` dispatched per element.

DSO split: `beam.smp` **62.8 %**, `[JIT]` **18.3 %**, kernel 9.5 %, libc 8.5 %.

**JIT bucket, by family (% of all samples):**

| family | share |
|---|---|
| `Enum.*` | 3.9 % |
| `lists:*` (Enum's list-path delegate) | 2.7 % |
| `Protocol.*` dispatch (`impl_for`, `ensure_prefix`) | 2.1 % |
| `Scorable.*` (user protocol impl) | 1.9 % |
| user anon funs (pipeline bodies) | 1.1 % |
| `Stream.*` | 0.5 % |
| `Enumerable` protocol | 0.3 % |
| `$global::` glue | 4.2 % |

`Enum` + `Stream` + `Enumerable` + `Protocol` + user-protocol =
**≈8.7 % of total CPU** even when the code is *nothing but* pipelines.

**`beam.smp` C bucket — where the pipeline's cost actually goes:**
- **Allocator churn ≈15 %**: `erts_alcu_free_thr_pref` 4.3, `mbc_free` 3.4,
  `aoff_unlink_free_block` 2.1, `rbt_delete` 2.0, `rbt_insert` 1.8,
  `dealloc_block` 1.7 — *every `Enum` stage materializes a fresh intermediate
  list.* Plus GC 5.7.
- **Atom-table / protocol dispatch ≈10 %**: `atom_hash` 4.4, `hash_get` 1.9,
  `binary_to_atom` 1.5, `atom_to_binary` 1.5 — **unconsolidated** protocol
  dispatch builds the impl-module atom (`Elixir.Scorable.Item`) via atom↔binary
  on every call. **This is a `elixir script.exs` / dev-mode artifact; a
  `mix release` consolidates protocols to a direct call and this ~10 % vanishes.**
  So the *production* protocol-dispatch pool is far smaller than 2.1 %+1.9 %
  here; do not size the pool from this number.

### Implications for T2FULL (M0.3)

- **The `Enum`/protocol pool does not justify a high weight in the P3 scoring
  table on the strength of the compiler corpus** — there it is ~0 %. It scores
  only on genuinely pipeline-heavy Elixir (data processing / Broadway / view
  rendering), and even at the *maximum* (a pure pipeline) the `Enum`+`Stream`
  +protocol machinery is **~9 % of cycles**, of which the production-real,
  inline-addressable part (`Enum`/`Stream` wrapper + literal-fun bodies, protocol
  *consolidated* out) is ~5–7 %. Keep the pool in the table, weighted **low**,
  and gate its P3 credit on a *running-app* corpus profile — not the compiler.
- **The bigger Elixir signal is Pillar 3, not Pillar 2.** The pure pipeline's
  dominant cost is **allocation of intermediate lists between `Enum` stages
  (~15 % allocator + ~6 % GC)** — exactly the construct/deconstruct + escape
  pool. `Enum` inlining's real payoff (§3.2) is that it *enables* that sinking:
  once `filter |> map |> reduce` is one region with the literal funs propagated,
  the intermediate lists never materialize. So the Enum pool's value is as the
  **enabler for allocation elimination**, which is precisely how §3.2/§3.3 frame
  it — this data confirms the framing and says the credit accrues to Pillar 3's
  gate (G7), not to a standalone "Enum inlining" line.
- Protocol width (open-Q #2): the measurable protocol cost here is *consolidation
  overhead*, not polymorphic dispatch. Confirms that PIC width beyond monomorphic
  rarely pays — most Elixir protocol sites are consolidated to a direct call
  before T2 ever sees them.

---

## Raw reports

`dialyzer_perf_report.txt`, `elixir_compiler_perf_report.txt`,
`elixir_compiler_family_agg.txt`, `elixir_enum_perf_report.txt` — full
`perf report --stdio` output (DSO split + top symbols) alongside this file.
Driver scripts: `leg1_dialyzer.sh`, `leg2_agg.sh`, `leg2b_enum.sh`;
image recipe `Dockerfile.elixir` (Elixir 1.19.0 `elixir-otp-28` on OTP 29).
