<!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
-->

# BeamAsm Tier 2 (T2-Full), the optimizing JIT

BeamAsm Tier 2 ("T2-Full") is a profile-driven *optimizing* second tier for
the BEAM JIT. Where the tier-1 JIT ([BeamAsm](BeamAsm.md)) translates each
BEAM instruction to native code one at a time with hardly any cross-instruction
optimization, Tier 2 reconstructs an SSA intermediate representation from the
already-loaded BEAM code, runs a small BEAM-specific optimizing mid-end over it
(loop recovery, speculation, inlining of a few higher-order patterns, the
classic cleanup passes, byte-loop unrolling), and installs the optimized machine
code *over* the T1 function. T1 always stays intact underneath as the fallback:
any side exit, guard miss or unhandled event deoptimizes back into the T1 body.

Tier 2 is **aarch64-only** and **opt-in** — it is off by default and there is
no plan to turn it on by default. The optimizing mid-end (HIR/LIR/isel/regalloc)
is architecture-independent and is compiled on every JIT target, but the code
emitter (`t2_emit.cpp`) is aarch64 assembler, so on any other target the whole
backend is compiled out to linkable stubs and no real T2 blob is ever installed.

## Scope and status

Tier 2 landed as a **specialist** tier, not a general one. The measurement
program that drove its development
([`PLAN/T2FULL/15_scope_and_disposition.md`](../../../PLAN/T2FULL/15_scope_and_disposition.md),
[`16_retrospective.md`](../../../PLAN/T2FULL/16_retrospective.md)) concluded that
the original "≥20 % end-to-end on most applications" goal is not reachable by a
JIT on real services, because their hot code is either intrinsically irreducible
(analysis/compiler code) or uses opcodes the tier does not compile (map/binary
*construction*, higher-order dispatch). What Tier 2 *does* deliver is measured
and bounded:

| workload shape | speedup vs T1 |
|---|---|
| single-clause byte scan-and-count kernels (validators, lexers) | 2.5–3.1× (Apple Silicon), up to 16× digit-scan on server ARM |
| integer/float tail loops (arithmetic accumulation, `foldl`-class) | 1.4–2.0× |
| everything else | never slower than T1 (the install gate keeps non-winning blobs from installing) |

The tier is *safe by construction*: it never changes observable behaviour,
never raises where T1 would not, counts reductions identically, keeps stack
introspection byte-identical, and — via a static install-quality gate — declines
to install a blob unless it can show it removes work relative to T1.

The general-tier phases (rung-2 framestate deopt / cross-module inlining, escape
analysis, x86 port) were measured and decided against; they are retained only in
the `PLAN/T2FULL/` design record. **This document describes what is in the tree,
which in several places is broader than the mid-July disposition memos — the code
is the source of truth.**

## Relationship to Tier 1

Tier 2 does not replace T1; it sits on top of it and depends on it in three ways:

1. **T1 is the deopt floor.** The T1 body of every function is left completely
   intact. Every side exit from a T2 blob re-establishes the exact register
   state T1 expects and branches to a T1 machine-code address (a "PC") inside
   that body. T2 never raises exceptions and never lands `c_p->i` on a blob
   address that T1 cannot resume from.

2. **T2 installs behind the T1 prologue.** Installation is a single-word patch
   of the T1 function prologue (the same trampoline word that trace and NIF
   loading use), so every caller kind — remote call, intra-module direct branch,
   fun, apply — is caught without touching export entries. See
   [Install and jettison](#install-and-jettison).

3. **T2 reuses T1's emitters.** The Tier-2 assembler subclasses the aarch64
   `BeamModuleAssembler` and calls the same per-instruction emitters T1 uses,
   with the fail labels redirected to deopt trampolines. This keeps the two
   tiers bit-compatible on everything T2 does not deliberately optimize.

## Enabling the tier

Tier 2 carries zero cost unless explicitly enabled — a default build retains no
extra tables and arms no counters. There are two ways in, and two entry paths
into the compiler:

* `+JT2enable true` (`erts_jit_t2_force`) — the **synchronous compile-at-load**
  path. At the end of loading each module, `erts_t2_compile_module` builds,
  lowers, emits and installs *every* eligible function of the module while the
  loader still holds the module unsealed. No counters, no profiling — it is the
  "compile everything eligible" mode, used for development and testing.

* `T2_RETAIN=1` (environment) — enables per-module *retention* of the decoded
  code tables plus **counter-triggered tier-up**: functions are compiled lazily
  when their call counters trip a threshold (see [Profiling and
  tier-up](#profiling-and-tier-up)).

`erts_t2_enabled()` is true when either is set; `erts_t2_tier_enabled()` (the
counter path) is additionally gated to aarch64 and disabled under `+JT2enable`.

## Which functions are eligible

A function is Tier-2 eligible **iff every generic BEAM op in its body is in the
supported set** (`erts_t2_genop_supported`, `t2_eligible.c`). One unsupported op
anywhere drops the whole function to T1 — there is no partial compilation. Two
ops are additionally argument-checked: `bs_match` (byte-aligned command subset
only, `erts_t2_bs_match_check`) and `bif2` (a small comparison subset only,
`t2_bif2_op_supported`). The eligibility table is the single source of truth
shared with the SSA builder, so builder coverage cannot drift from it.

The supported set (verified against `t2_eligible.c` at HEAD) admits:

* **Plumbing** — `move`, `swap`, `init_yregs`, `allocate`/`allocate_heap`,
  `deallocate`, `trim`, `test_heap`, plus the function-scaffolding and line ops.
* **Calls and returns** — `call`/`call_last`/`call_only`,
  `call_ext`/`call_ext_last`/`call_ext_only`, `return`. `call_ext` to a *light*
  BIF lowers to an in-blob BIF call; heavy BIFs and loader-transformed calls stay
  T1 (isel reports them unsupported).
* **Control flow** — `jump`, `select_val`, `select_tuple_arity`.
* **Guards** — the type tests `is_integer`/`is_atom`/`is_nil`/`is_list`/
  `is_nonempty_list`/`is_tuple`/`is_binary`/`is_float`/`is_number`/`is_boolean`/
  `is_bitstring`/`is_pid`/`is_port`/`is_reference`, `test_arity`,
  `is_tagged_tuple`, and the comparisons `is_lt`/`is_ge`/`is_eq`/`is_ne`/
  `is_eq_exact`/`is_ne_exact`.
* **Data access** — `get_list`, `get_hd`, `get_tl`, `get_tuple_element`,
  `put_list`, `put_tuple2`.
* **Map *matching*** — `is_map` and `get_map_elements` (argument-checked;
  decomposed to one read-only lookup per key). Map *construction* is not
  supported.
* **Arithmetic** — `gc_bif1`/`gc_bif2`/`gc_bif3` (generic arithmetic lowers to
  these).
* **Clause-failure exits** — `badmatch`, `if_end`, `case_end`, `badrecord`,
  modelled as error-exit blocks that tail-call into T1.
* **Byte-aligned binary matching** — `bs_start_match3`/`bs_start_match4`,
  `bs_match` (byte-aligned commands only), `bs_test_tail2`, `bs_get_tail`,
  `bs_get_position`/`bs_set_position`, and the plain (no-flags) `bs_get_utf8`/
  `bs_skip_utf8` fastpath.
* **Fun creation** — `make_fun3`.
* **Exceptions (try/catch, "Strategy 2")** — `try`/`try_end`/`try_case` and the
  handler-only raising ops `raise`/`raw_raise`/`build_stacktrace`. The try body
  runs in T2 and reuses T1's registered catch tag; a thrown exception unwinds
  into T1's handler. The handler block itself is dropped as an unreachable island
  before translation, and any *reachable* raising op fail-closes the whole build.

Two supported ops are **build-only** (`erts_t2_genop_build_only`): `call_fun`/
`call_fun2` and the immediate-arity `is_function2`. They are decoded so that a
higher-order fold *callee* can be built as a link in a P1 inlining chain and its
per-element call devirtualized when it is inlined at a caller with a statically
known fun — but they have no standalone instruction selection, so a function
that still contains one after optimization is never installed on its own.

Deliberately **not** supported (the load-bearing exclusions): higher-order
dispatch (`call_fun` standalone), map/record *construction* (`put_map_*`,
`update_record`), binary *construction* (`bs_create_bin`, `bs_init*`, `bs_put_*`),
non-byte-aligned or multi-byte binary matching, general (non-GC, non-comparison)
BIFs, float-register instructions, `receive`/messaging, `send`, `apply`, and
`on_load` functions. This shape admits numeric tail loops, byte scanners, and
tuple/list/map-match access code, and excludes the map-, closure-, and
binary-construction-heavy code that dominates real service own-time.

## The compilation pipeline

One function flows through the pipeline below (driven by
`t2_compile_install_one` in `t2_compile.cpp`). Any failure at any stage degrades
that function to T1 — never an error, never an aborted load.

```
  loaded BEAM module
        │
        │  eligibility scan (t2_eligible.c) + code retention (t2_retain.c)
        ▼
  ┌──────────────────────────────────────────────────────────────────┐
  │ SSA build     BEAM generic ops → CFG-SSA HIR (Braun on-the-fly)   │  t2_hir_builder.cpp
  │ loop recover  self-tail-recursion → back edges + loop header      │  t2_loop.cpp
  │ intrinsics    lists foldl/foreach/all/any + literal fun → flat    │  t2_intrinsics.cpp
  │               loop; LICM-lite                                     │
  │ unroll        cursor-IV byte scanners → xN latch / SWAR read-sum   │  t2_intrinsics.cpp
  │ inline        local leaf callee spliced into a recovered loop      │  t2_inline.cpp
  │ speculate     generic add/sub → flag-checked AddSmall/SubSmall;    │  t2_spec.cpp
  │               monomorphic flatmap shape guard on get_map_elements  │
  │ opt suite     DCE, const-fold, copy-prop, CSE/GVN-lite, make_fun   │  t2_opt.cpp
  │               sinking (fixpoint)                                   │
  │ validate      HIR register-state walk + re-execution-window check  │  t2_hir.cpp
  └──────────────────────────────────────────────────────────────────┘
        │
        ▼
  ┌──────────────────────────────────────────────────────────────────┐
  │ isel          HIR → LIR, concrete X/Y homes, cross-tier addresses  │  t2_isel.cpp
  │ regalloc      Wimmer linear-scan-on-SSA + placement verifiers      │  t2_regalloc.cpp
  │ emit          LIR → asmjit (subclass of BeamModuleAssembler)       │  t2_emit.cpp
  │ gate          install-quality gate: does the blob beat T1?         │  t2_compile.cpp
  │ install       single-word prologue patch at L_f+4                  │  t2_install.c
  └──────────────────────────────────────────────────────────────────┘
        │
        ▼  side exits / guard misses / jettison ─────────▶ back to T1 body
```

### Eligibility scan and retention (`t2_eligible.c`, `t2_retain.c`)

Everything the loader parses out of a `.beam` file is freed right after T1
emission, and the raw code-chunk bytes were never owned by the loader at all.
So when the tier is enabled and the eligibility scan finds at least one eligible
function, retention **copies** what the SSA builder will need into one
module-lifetime allocation: the raw code-chunk bytes (re-decoded later), the atom
and import tables, the Type-chunk table, the literal-index map, the per-function
eligibility bitmap, and — for the counter path — the per-function profiling
records.

### SSA construction (`t2_hir_builder.cpp`, `t2_hir.hpp`)

The builder re-decodes the retained code chunk with `beamfile_get_code` against
a transient `BeamFile` view, yielding **generic, pre-transform** ops — the same
level `beam_ssa_codegen` emitted, which is exactly the level SSA reconstruction
wants (the loader's transform engine is never applied to the chunk). SSA is
constructed with the Braun et al. on-the-fly algorithm: local value numbering per
block, operandless phis on unsealed blocks, and a seal pass once the CFG is known.

The result is a block-structured SSA IR (HIR) with explicit phi nodes, arena
allocated, backend-neutral (no asmjit types). Two pieces of metadata make the
whole tier correct by construction:

* **Canonical homes.** Every op records the BEAM register its result was decoded
  into (`dst_reg`) and the register each operand was read from
  (`operand_regs`); phis record the variable they merge as their home. So every
  SSA value has a canonical X or Y register home *by construction*.

* **Sync maps (`T2SyncMap`).** Every op at which T2 can exit, trap, GC or yield
  — function entry, calls, returns, GC tests, frame allocation, `gc_bif`
  arithmetic, decoded error exits — carries a snapshot of the exact BEAM
  register state at that instruction boundary: the value in each live X and Y
  slot plus the current frame size. The boundary convention is pinned as "the
  state T1 would observe when about to execute the op".

Allocate/Deallocate/Trim are first-class ops so the frame layout is derivable at
any program point. A validator cross-checks every sync map against a per-block
register-state walk: a value appearing in a map without being materialized in
that register is a hard error, never silent.

### Loop recovery (`t2_loop.cpp`, `t2_loop.hpp`)

Erlang has no intra-function loops at the BEAM level — every loop is a
(tail-)call through a function entry. `t2_loop_recover` rewrites a function's
self-recursive tail calls into back-jumps to a synthesized loop header, which is
what *creates* the CFG loops the later analyses (LICM, the window validator, the
speculation and unroll passes) operate on. Loop structure is kept as side data
(dominators, back edges, natural loops merged per header) in the `LoopInfo` style
— never as an IR construct.

### Intrinsics and unrolling (`t2_intrinsics.cpp`)

Two families of pattern rewrites run here:

* **`lists` foldl-class intrinsics.** A monomorphic non-tail `call_ext` to
  `lists:foldl/3`, `lists:foreach/2`, `lists:all/2` or `lists:any/2` whose fun
  argument is an SSA-constant, environment-free `make_fun` of the same module is
  replaced with a hand-ported expansion of the wrapper and its recursive helper,
  with the literal fun's body spliced in by constant propagation, so the whole
  thing becomes a flat loop the recovery/speculation passes can optimize. Only
  effect-free fun bodies are admitted. Body-recursive shapes (`lists:map`,
  `foldr`) are *not* expressible under rung-1 deopt and stay plain calls.

* **Cursor-IV unrolling / SWAR.** A byte scanner whose latch is a simple
  cursor-advance + accumulate is unrolled xN. Two fused variants exist: a
  roll-back skip-count form that collapses N adds into one checked
  `acc + N*C` placed *before* the advance (so an overflow deopt finds the cursor
  un-advanced), and a SWAR read-and-sum form that collapses N byte reads into one
  64-bit wide load plus a horizontal byte-sum fold. Both keep reduction counts and
  deopt behaviour byte-identical to the 1-wide loop.

`t2_licm_lite` hoists loop-invariant, pure, never-faulting ops (and
window-shaped guards) out of the loop header into the preheader.

### Inlining (`t2_inline.cpp`)

`t2_inline_leaf` splices a small, call-free, frame-free, single-block *local*
callee into its call site inside a recovered loop (the MVP `diff/2` fused into
`total/2` class). Removing the call removes the loop's only effect boundary, so
the whole iteration becomes a single re-execution window; the shape-up half then
restores the flat-loop shape (copy propagation, frame elision, preserving the
re-call argument vector). Admission is strict — one call in the loop, one balanced
frame pair, whitelisted callee ops — so a spliced function always converts; the
result is re-proven in full by the validators. This is rung-1 inlining: no
framestates, no CPs; every fallible inlined op must become a window-shaped
speculative op or the function stays on T1.

### Speculation and specialization (`t2_spec.cpp`)

The speculation pass replaces generic `Add`/`Sub` `gc_bif` arithmetic inside
recovered loops with **flag-checked** `AddSmall`/`SubSmall`: compute with
flag-setting instructions, branch on overflow to a side exit, commit after —
"deopt before the commit", which is T1's own small-int fast path with the type
checks hoisted out. The one-untag trick is folded into the emitters exactly as in
T1, so no untagged machine word ever exists as an SSA value. Guards are inserted
as `SpeculateType` tag-bit tests; guard fusion ANDs the guards needed at one
deopt anchor into a single multi-operand test.

Two deopt shapes are chosen per op:

* **window** — side exit to the function's T1 entry body; T1 re-executes the
  whole iteration from the fresh-call vector. Legal only with a clean prefix (no
  effect, no frame op, no write of `X0..arity-1` before the op), enforced by
  `t2_validate_windows`.
* **boundary** — side exit to the op's own T1 EFFECT PC; T1 re-executes just that
  op from its sync-map state. Used after effects, where the window shape is
  illegal.

Facts come from a `T2FactSource`; the profile-less default speculates "observed
small" on entry arguments feeding loop-carried arithmetic phis, unless the Type
chunk excludes small integers. The same pass carries the **map-shape
specialization**: for a `get_map_elements` whose map operand is a function
parameter with a monomorphic flatmap shape fact, it attaches the observed keys
tuple as a shape hint; isel validates it and lowers a shape guard plus an O(1)
offset load instead of a key scan, dropping back to the scan if any of that fails.
A wrong shape simply deopts, so the shape is a hint, never a correctness input.

### Optimization suite (`t2_opt.cpp`, `t2_opt.hpp`)

Once a body has been inlined/expanded, the classic cleanup passes run as a
fixpoint: **DCE** (dead pure-value and dead-phi elimination), **constant folding
+ copy propagation** (home-aware operand forwarding through register copies,
single-input-phi collapse), **CSE / GVN-lite** (value-number pure ops, merging
only when the survivor is provably available in its canonical home at every
rewritten use and the victim is invisible to every sync map), and **`make_fun`
sinking** (partial DCE that sinks a fun allocation into the slow block so the fast
path neither allocates nor GC-tests for it). Every rewrite respects the home/deopt
model — homes are real register moves, sync maps are uses — and the output must
re-validate under both `t2_validate` and `t2_validate_windows`.

### Instruction selection (`t2_isel.cpp`, `t2_isel.hpp`)

Isel walks the HIR to LIR with *concrete* canonical slots taken from the decoded
homes. Because the identity lowering keeps every value in the register T1 would
keep it in at every boundary, the sync-everything policy is satisfied by
construction. Isel is also where **cross-tier addresses** are resolved against the
loaded module, all read out of the pctab (below):

* non-tail calls get their post-call continuation CP from the pctab CONT entry —
  never a T2 address;
* `gc_bif` arithmetic with a `{f,0}` fail gets its side-exit PC from the pctab
  EFFECT entry (T1 re-executes the op and raises; T2 never raises);
* decoded error exits get theirs from the pctab ERROR entry; the shared
  function-clause exit branches to the function's `func_info`;
* local call targets resolve by MFA against the code header's function table;
  external ones through the export entry;
* `call_ext` to a light BIF lowers to an in-blob BIF call whose yield/resume and
  trap CP come from the pctab, so no blob address ever reaches `c_p->i`.

Any missing pctab entry, heavy-BIF or loader-transformed call target, or shape
outside the identity table is a clean "unsupported" — the function stays T1.

### Register allocation (`t2_regalloc.cpp`)

A Wimmer-style linear scan on SSA over the value-annotated LIR. SSA liveness is
computed, live intervals are built with per-use position and location records,
and every op that carries a sync map contributes *fixed-slot* constraints — the
sync map **is** the allocator's pin-constraint set. The shipping tier reproduces
the identity placement (every interval lives in its canonical BEAM slot at every
use, effectively zero spills), and the allocator *verifies* that placement:

1. SSA liveness closes (no use without a reaching def);
2. placement soundness walked forward in slot space per block (a slot read must
   find the value the annotation names there; `Trim` renumbers Y slots,
   `Allocate`/`Deallocate` invalidate the frame, calls clobber the X file);
3. clobber liveness (a value live across a call must hold a Y home there);
4. untagged discipline (an untagged interval must never have an X/Y-slot use and
   must never be named by a sync map).

### Emission (`t2_emit.cpp`, `t2_emit.hpp`)

`BeamT2ModuleAssembler` subclasses the aarch64 `BeamModuleAssembler`, inheriting
its register defs, `mov_arg`, `emit_enter/leave_runtime`, `emit_gc_test`, the
veneer machinery, and the `protected` per-op T1 emitters. For an identity op it
synthesizes the loader `ArgVal` family from the LIR slot and calls the reused T1
emitter, redirecting the emitter's `Fail` label to a small in-blob trampoline that
branches to the op's T1 PC (`Fail`→T1-PC). A fresh assembler is created per blob,
so no per-module loader state leaks in. The blob is a full mini-function:
enter-frame prologue, body, `emit_return`, and the deopt trampolines, emitted into
the process-wide Tier-2 `JitAllocator`.

### The install-quality gate (`t2_compile.cpp`)

Before installing, a static gate decides whether the blob would actually beat T1.
Diagnosis found that T2 beats T1 *only when it removes work* — inlines a call,
fuses unboxed arithmetic, or fuses a per-byte `bs` loop into a scan run; a blob
that merely re-emits T1's ops plus speculation guards can only tie or lose. The
gate reads signals the emitter computed (a fused scan run was admitted, a leaf
call was inlined, a roll-back-pinned cursor unroll fired) and installs only when
one of them is present. It is deliberately conservative so that non-winning blobs
stay on the T1 floor. (The gate's win-signals are histogram-summed and
path-blind; a documented residual is that a non-`bs` multi-clause integer
accumulator could over-accept — see
[`15_scope_and_disposition.md`](../../../PLAN/T2FULL/15_scope_and_disposition.md).)

## The deopt and home model

The central correctness invariant of Tier 2 is: **at every program point where
control can leave the blob, the live BEAM values are sitting in the exact X/Y
registers T1 expects, and there is a valid T1 machine-code address to resume at.**
Everything else — the sync-everything register policy, canonical homes, the
window/boundary validators, the pctab — exists to guarantee this.

Deopt is **rung-1, re-call only**: a side exit always reconstructs a valid
argument vector at a re-execution boundary and branches to a T1 address. There
are no framestates, no continuation pointers into T2 blobs, and no stack scans.
The `framestate` HIR op and the per-op framestate reference exist in the IR but
are **never populated** — the rung-2 framestate machinery (eager-CP-push,
tombstone lifecycle) and rung-3 virtual-object rematerialization were designed but
decided against, so they are not in the code.

The deopt shapes actually emitted:

* **window** — re-execute the whole current iteration from the function's T1
  entry body (used for loop-body speculation whose prefix is clean).
* **boundary** — re-execute one op from its own T1 EFFECT PC (used after effects).
* **callee-demote** — for an inlined `lists` fold, the deopt/yield state is always
  the *callee's* fresh-call vector (`foldl` at element k *is*
  `foldl(F, Acc_k, Rest_k)`), and every exit re-enters real `lists` code at the
  intrinsic's call-site T1 PC.

Because rung-1 blobs contain no CPs and no resume PCs on the stack, an in-flight
invocation leaves the blob on its next call/return/side-exit and can never
re-enter once the prologue is reverted — which is what makes O(1) jettison sound.

The one long-residency case is a process that *yields* mid-loop inside a recovered
loop. That is handled by back-edge resume stubs: the yield saves the state as a
fresh-call argument vector and stores an in-blob resume PC into `c_p->i`; on
resume the process re-enters the blob. These resume PCs are registered in
`t2_ranges` (below) and guarded by an in-blob tombstone word, so a jettison that
happens while a process is yielded translates the saved `c_p->i` back to a T1
demote target instead of re-entering a freed blob.

## Install and jettison

Installation follows the NIF/trace model (`t2_install.c`, header
`t2_install.h`). The T1 prologue at `L_f+4` holds a single patchable branch
instruction — on aarch64 the word `0x14000002` (`b next`), which normally skips
the breakpoint trampoline. Installing a blob rewrites *that one 4-byte word* to
branch to the blob's entry stub. Every caller kind — external via
`Export.dispatch`, intra-module direct branch, fun, apply — converges on `L_f`
and runs `enter_erlang_frame` followed by the patched branch, so one store
redirects them all. **`Export.addressv` is never touched.**

**Reach policy** (correctness first): a direct `b` when the blob entry is within
±128 MB of `L_f+4`; otherwise a near-side bridge veneer
(`ldr x14, .+8; br x14; .quad entry`) allocated from the JIT allocator *if* that
lands in range; otherwise the install is **rejected** and the function stays T1.

**Strict trace/NIF mutual exclusion.** Install only proceeds on a pristine
prologue (breakpoint flag zero, no staged `GenericBp`, the `+4` word bit-exactly
`0x14000002`). Conversely the breakpoint/NIF installers call
`erts_t2_jettison_function` *before* setting the breakpoint flag — **trace always
wins.**

**Jettison** reverts the `+4` word, deregisters the blob from `t2_ranges`, unlinks
it from the owning module instance's install list, and schedules the span release
behind a **code barrier** (`erts_schedule_code_barrier` — thread progress plus
instruction barriers on all schedulers), which guarantees no scheduler is still
executing inside the blob before its memory is freed.

**Cross-module dependency tracking.** A blob may bake in T1 addresses of *other*
module instances (the `lists` helper an intrinsic demotes to) or of its own
instance when it inlined a fun body. Those `BeamCodeHeader`s are recorded in the
install record's `dep_hdrs`; when such an instance is deleted, overwritten,
traced or NIF-patched, `erts_t2_jettison_deps` kills every dependent blob so no
stale T1 address stays reachable. Hot-code loading and tracing therefore always
*force deoptimization* — no attempt is made to keep optimized code alive across
either event.

Everything here runs under **code modification permission** (not load
permission): the patch mutates an existing module in place and needs no `code_ix`
staging.

## Profiling and tier-up

Under `T2_RETAIN=1`, tier-up is counter-triggered (`t2_tier.c`, `t2_retain.c`):

* The T1 profiling sequence (`emit_t2_profile_sequence` in
  `arm/instr_common.cpp`) bumps an eligible loop function's call counter and,
  when it crosses the function's threshold **on scheduler 1 only**, trips into
  `erts_t2_profile_trip`. Every other scheduler stores into a shared throwaway
  record whose threshold never trips, so the counters stay uncontended.

* The per-function threshold is `base · √(size+1)`, where `size` is the
  function's generic-op count and `base` defaults to `1000` (overridable with the
  `T2_TIER_THRESHOLD` env var). Bigger functions need proportionally more trips.

* The trip applies a profile-stability rule, marks the record pending, pushes a
  compile job onto a small ring, and kicks a single worker. The compile runs
  **off the hot scheduler**: it is scheduled as misc-aux work on a normal
  scheduler other than scheduler 1, which re-acquires code-modification permission
  and only then compiles, installs and disarms. Only one worker ever runs (the
  permission is exclusive), so asmjit's allocator needs no extra locking.

The profile also feeds the speculation inserter: observed entry type-classes
narrow (never contradict) the AOT Type chunk, and an observed monomorphic flatmap
shape is what enables the map-shape specialization.

### The PC table (`t2_pctab.c`, `t2_pctab.h`)

For every eligible function, the pctab records the T1 machine-code offsets of
every "re-entry" kind a deopt needs: the function entry, each call site, each
light-BIF call site, each post-call continuation, each post-BIF/effect boundary,
and each error-exit op site. Each entry is tagged with the BEAM op's *generic
decode ordinal* (`beam_idx`). The subtlety is that codegen runs on the loader's
post-transform *specific* op stream, whose ordinals do not line up with the SSA
builder's *generic* pre-transform ordinals; so the ordinals are recovered by
re-decoding the retained chunk (the same decode the SSA builder uses) and zipping
per-kind onto the codegen-collected offsets. This table is what lets isel map an
SSA op back to a resume PC without ever putting a T2 address on the stack.

### Blob ranges (`t2_ranges.c`, `t2_ranges.h`)

Installed blobs are not part of any `BeamCodeHeader`, so `beam_ranges` cannot host
them (its lookup would misinterpret a blob's start as a code header). Tier 2 keeps
a separate, parallel sorted-interval class keyed on blob `{start,end}` whose lookup
returns a T2 blob descriptor directly. It maps a PC inside a blob back to its MFA
and, for blobs with recovered loops, owns the **resume table** (per-back-edge
resume offsets, tombstone-word distances, and per-entry T1 demote targets) used to
translate a yielded `c_p->i` on jettison.

## Speculation and specialization in the tree

The specializations that actually exist, each verified in the code:

* **Small-int arithmetic speculation** (`t2_spec.cpp`) — flag-checked
  `AddSmall`/`SubSmall` with overflow deopt, the core loop win.
* **Monomorphic flatmap shape guard** (`t2_spec.cpp`, `specialize_map_shapes`) —
  an O(1) shape-guarded offset load for `get_map_elements` on a parameter with a
  profiled flatmap shape, replacing the key scan.
* **Local leaf inlining** (`t2_inline.cpp`) — a call-free local callee spliced into
  a recovered loop, dissolving the loop's only effect boundary.
* **`lists` foldl-class intrinsics** (`t2_intrinsics.cpp`) — `foldl`/`foreach`/
  `all`/`any` with a literal fun expanded to a flat loop.
* **Cursor-IV loop unrolling + SWAR** (`t2_intrinsics.cpp`, `t2_unroll`) — verbatim
  xN unroll, roll-back-fused skip loops, and a 64-bit SWAR byte-sum for aligned
  read-and-sum scanners.
* **Loop recovery** (`t2_loop.cpp`) — self-tail-recursion turned into an actual CFG
  loop so the passes above have something to optimize.

## Debugging

### Command-line flags

* `+JT2enable true|false` — compile and install Tier-2 code for every eligible
  function at module load (the synchronous, counter-free path).
* `+JT2dump ...` — dump Tier-2 compilation. The flag builds up a bitmask and can
  be repeated to combine a **sink** with **facets**:
  * sinks: `true` (per-module `<Module>.t2.asm` file), `stderr`;
  * facets: `hir`, `lir`, `asm`, `stages` (intermediate HIR after
    intrinsics/unroll/opt), `ra` (register allocation), `all`.

  A bare `+JT2dump true`/`stderr` defaults to the `hir+lir+asm` facets. It is
  independent of `+JDdump`.
* `+JDdump true|false` — dump the *T1* (BeamAsm) assembly for each module loaded
  (`erts_jit_asm_dump`); useful for comparing a T2 blob against the T1 body it
  installs over.

### Introspection via `erts_debug:get_internal_state/1`

The tier exposes a number of tuples (backed by `t2_compile.cpp`, `t2_install.c`,
`t2_tier.c`, `t2_debug.cpp`; wired in `erl_bif_info.c`):

* `t2_stats` → `{Modules, FunctionsBuilt, Installed, IselUnsupported, EmitFailed,
  InstallRejected, BuildFailed, CompileMicros}` — cumulative driver statistics.
* `t2_opt_stats` → `{P1SitesInlined, P1LoopsRecovered, P2AccUnboxed, P2IvUnboxed,
  P3GuardsRemoved, P3IvOvfRemoved}` — one bump per committed transform.
* `t2_yield_stats` → `{BackEdgeYields, BackEdgeResumes}`.
* `{t2_install, M, F, A}` / `{t2_jettison, M, F, A}` /
  `{t2_installed, M, F, A}` — force an install, force a jettison, or query whether
  a function has a blob (returning its base/size and the MFA the ranges lookup
  resolves for an interior PC). Used by the install-wave tests.
* `{t2_build_ssa, M, F, A}` — run the SSA builder and serialize the resulting
  `T2Function` as a structured Erlang term for machine comparison (the fidelity
  gate); see `t2_debug.cpp`.
* `{t2_in_blob, Pid}` — whether `Pid`'s saved `c_p->i` currently lies inside a
  registered blob (i.e. it is yielded at a recovered loop's back edge).
* `t2_profile_census` — bucketed counter statistics across all armed functions.

### Developer environment toggles

A family of `T2_*` environment variables are compiled-in bisection levers, not
supported configuration. The most useful:

* `T2_RETAIN=1` — enable retention + counter-triggered tier-up (see above).
* `T2_TIER_THRESHOLD=N` — override the tier-up base threshold; `T2_TIER_DISARM`
  disarms counters.
* `T2_NO_OPT` (master), and the per-pass switches `T2_NO_DCE`, `T2_NO_CONSTFOLD`,
  `T2_NO_COPYPROP`, `T2_NO_CSE`, `T2_NO_SINK`, `T2_NO_SPEC`, `T2_NO_INTRIN`,
  `T2_NO_MAPS_INTRIN`, `T2_NO_INLINE`, `T2_NO_UNROLL`, `T2_NO_FUSE`,
  `T2_NO_SCAN`, and the phase gates `T2_NO_P1`/`T2_NO_P2`/`T2_NO_P3` — turn off
  individual transforms to bisect a miscompile or a lost win.
* `T2_INSTALL_GATE=0` — bypass the install-quality gate; `T2_INSTALL_LIMIT=N` caps
  installs (bisecting a bad blob); `T2_INSTALL_TRACE`/`T2_OPT_TRACE`/
  `T2_SPEC_TRACE`/`T2_P1_TRACE`/`T2_UNROLL_TRACE`/`T2_INTRIN_TRACE` log accepts
  and bail reasons.
* `T2_UNROLL_N=N` — override the unroll factor; `T2_SELFTEST`/`T2_EMIT_SELFTEST`
  run the ranges/emitter self-tests.

## Description of each file

The Tier-2 implementation resides in `$ERL_TOP/erts/emulator/beam/jit/t2`.
The files are:

* `t2_eligible.c`
    * Per-function eligibility scan; the single source of truth for the supported
      op set (`erts_t2_genop_supported`, `erts_t2_genop_build_only`,
      `erts_t2_bs_match_check`).
* `t2_retain.c` / `t2_retain.h`
    * Per-module retention of the decoded code tables the builder needs after
      load; the profiling records and the `base·√(size+1)` tier-up threshold.
* `t2_types.hpp`
    * The type lattice — a C++ port of `beam_types.hrl`, reusing the AOT
      type-union bit constants so the two cannot drift.
* `t2_hir.hpp` / `t2_hir.cpp`
    * The high-level SSA IR (HIR): op kinds, arena, sync maps, canonical homes,
      and the register-state / window validators.
* `t2_hir_builder.cpp`
    * BEAM generic-op → SSA construction (Braun on-the-fly), plus sync-map and
      home recording.
* `t2_loop.hpp` / `t2_loop.cpp`
    * Loop analysis (dominators, back edges, natural loops) and self-tail-recursion
      loop recovery.
* `t2_intrinsics.hpp` / `t2_intrinsics.cpp`
    * `lists` foldl-class intrinsics, LICM-lite, and cursor-IV unrolling / SWAR.
* `t2_inline.hpp` / `t2_inline.cpp`
    * Local leaf inlining and loop shape-up.
* `t2_spec.hpp` / `t2_spec.cpp`
    * The speculation-insertion pass: flag-checked small-int arithmetic and the
      monomorphic flatmap shape specialization.
* `t2_opt.hpp` / `t2_opt.cpp`
    * The standard optimization suite over inlined HIR (DCE, const-fold,
      copy-prop, CSE/GVN-lite, `make_fun` sinking) as a fixpoint.
* `t2_isel.hpp` / `t2_isel.cpp`
    * Instruction selection (HIR → LIR) with concrete homes and cross-tier
      address resolution.
* `t2_lir.hpp` / `t2_lir.cpp`
    * The low-level IR (LIR): asmjit-free, names canonical BEAM slots and abstract
      physical registers.
* `t2_regalloc.cpp`
    * Wimmer-style linear-scan-on-SSA register allocation and the placement
      verifiers.
* `t2_emit.hpp` / `t2_emit.cpp`
    * LIR → asmjit emission; `BeamT2ModuleAssembler` subclasses the aarch64
      `BeamModuleAssembler` and reuses the T1 per-op emitters. Compiled out to
      stubs off aarch64.
* `t2_compile.cpp`
    * The compile-pipeline driver; the install-quality gate; the `+JT2enable`
      compile-at-load driver; the `+JT2dump` routing; and the debug-BIF backing.
* `t2_install.h` / `t2_install.c`
    * Dynamic install / jettison via the single-word prologue patch, the reach
      policy, trace/NIF mutual exclusion, code-barrier span release, and
      cross-module dependency tracking.
* `t2_ranges.h` / `t2_ranges.c`
    * The blob range registration class (PC → MFA + resume table) parallel to
      `beam_ranges`.
* `t2_pctab.h` / `t2_pctab.c`
    * The T1 PC side table mapping SSA op ordinals to T1 resume PCs for deopt.
* `t2_tier.c`
    * Profile-driven tier-up: the counter trip, the compile queue, and the
      off-hot-scheduler async worker.
* `t2_debug.cpp`
    * The SSA-reconstruction debug serializer and the `t2_build_ssa` BIF backing.

## FAQ

### How do I know a function got a Tier-2 blob?

Start with `T2_RETAIN=1` (or `+JT2enable true`) and query
`erts_debug:get_internal_state({t2_installed, M, F, A})`, or look at the
aggregate `erts_debug:get_internal_state(t2_stats)`. Under `+JPperf true`,
samples inside a blob resolve to `$T2:Module:Function/Arity`.

### Why did my hot function *not* speed up?

Most likely it is ineligible (one unsupported op drops the whole function — check
the [eligible set](#which-functions-are-eligible)), or it is eligible but the
install-quality gate declined it because the blob does not *remove* work relative
to T1 (it only ties). Tier 2 is a specialist accelerator for numeric tail loops
and byte scanners; on branchy symbolic code and on services dominated by map or
binary construction it is designed to fall back cleanly to the T1 floor rather
than slow anything down.

### Does Tier 2 change any observable behaviour?

No. It never raises where T1 would not, counts reductions identically at the same
boundaries, and keeps stack introspection byte-identical. Hot-code loading, tracing
and NIF loading all force the affected blobs to be jettisoned back to T1.

### Is Tier 2 available on x86-64?

No. The optimizing mid-end builds everywhere, but the code emitter is aarch64-only,
so no blob is installed off aarch64. An x86 backend was decided against on the
measured evidence.
