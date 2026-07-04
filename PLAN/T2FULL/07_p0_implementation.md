# T2-Full P0 — Source-Grounded Implementation Map

Scope: code-chunk retention + BEAM→SSA builder, T1 PC side table, blob
range-registration design, G1 fidelity-gate harness (trace/inspection matrices
out of scope). Paths under `/Users/lukas/code/otp-beamjit2/`. aarch64
(`jit/arm/`) first; x86 deferred to P7. New tier-2 code in `beam/jit/t2/`.

**Read-before-build surprises (source contradicts / sharpens the plan) —
flagged inline as ⚠.**

---

## 1. Retention: where decoded BEAM code dies, and the retention hook

**Findings.** A load builds a `BeamFile` (`beam_file.h:185-220`) inside the
`LoaderState` (`stp->beam`). `beamfile_read` (`beam_file.c:1407`) parses every
chunk into heap tables; `beamfile_free` (`beam_file.c:1659-1724`) releases them
(`ERTS_ALC_T_PREPARED_CODE`): atoms `:1660`, imports `:1665`, exports/locals,
lambdas, lines, **types `:1697`**, debug, records, static+dynamic literals.
`beamfile_free` runs via `beam_load_prepared_dtor` (`asm_load.c:221-227`) which
`beam_load_prepared_free` (`asm_load.c:215`) calls at the end of both the error
path and the success path (`beam_load.c:233,329`). So all parsed tables die
right after T1 emission — the plan is correct.

⚠ **The raw code bytes are never owned by `BeamFile`.** `parse_code_chunk`
sets `beam->code.data`/`.size` as a *pointer into the module binary* passed to
`beamfile_read` (`beam_file.c:1341-1342`, tracing back through `iff_init`'s
`data` arg, `:1848-1862`). `beamfile_free` does **not** free it because it was
never copied. Retention must therefore **copy the code chunk bytes**, not merely
"keep" them — the input binary is transient.

⚠ **Literals already survive; the type table does not.** Static+dynamic
literals are destructively moved into the module's literal area by
`beamfile_move_literals` (`beam_file.c:1838-1846`) during finalize, so retained
SSA can reference the loaded module's literal area — retention need only keep
the *index→term* map, not the terms. The `Type` table is the opposite: consumed
*during* emission (`getTypeEntry`, `beam_jit_common.hpp:208-213`, reads
`beam->types.entries[...]`) and then freed at `beamfile_free:1697`. **Nothing
type-related survives load today** — retention must copy the type table.

Atoms are globally interned (the freed `beam->atoms.entries` is only an
index→`Eterm` map); imports (`beam->imports.entries`, `beam_file.h:86-95`) are
freed too.

**Change spec.** New retention struct attached per module instance:

```c
/* module.h: struct erl_module_instance, after `void *metadata;` (:29-43) */
struct ErtsT2RetainedCode *t2_retained;   /* NULL unless module has eligible fns */
```
```c
/* jit/t2/t2_retain.h (new) */
typedef struct ErtsT2RetainedCode {
    byte   *code;  Sint code_size;         /* copied raw code chunk bytes   */
    Eterm  *atoms; Sint32 atom_count;      /* copied index→atom map         */
    BeamFile_ImportEntry *imports; Sint32 import_count;
    BeamType *types; Sint32 type_count;    /* copied type table             */
    Sint   *literal_map; Sint32 literal_count; /* idx→module-literal-area   */
    Uint32 *eligible_bitmap;               /* per-function, from §7          */
    Uint    bytes;                         /* for accounting                */
} ErtsT2RetainedCode;
```

Hooks: (a) the eligibility scan (§7) runs during prepare; if any function is
eligible, `erts_t2_retain(stp, inst_p)` copies the four tables into
`ERTS_ALC_T_T2_CODE` on `inst_p`. Call site: end of `beam_load_finalize_code` /
`erts_finish_loading` (`beam_load.c:312`), after `beamfile_move_literals` so
`literal_map` resolves. (b) **Free at purge:** `erts_internal_purge_module_2`
(`beam_bif_load.c:2200-2236`), alongside `beamasm_purge_module:2231`. (c)
**erlang:memory:** add `erts_t2_retained_sz()` (an `erts_atomic_t`, mirroring
`mem_used`/`erts_ranges_sz`, `beam_ranges.c:65,230-234`) folded into `size.code`
at `erl_alloc.c:2503` — lowest-friction; a distinct `am_t2_code` key is possible
(open question, 08 §10).

**Alloc type.** `erl_alloc.types` format is `type NAME LIFETIME CLASS desc`
(`:125-144`). Add: `type T2_CODE LONG_LIVED CODE t2_code` (module-lifetime, CODE
class, matching `FUN_TABLE STANDARD CODE`, `:144`).

**LOC** ~250. **Risks:** literal reindexing across `beamfile_move_literals`
(dynamic negative indices, `beam_file.c:1781`); code-load permission /
staging-vs-active `code_ix` interaction on the free path.

---

## 2. The BEAM code reader — decoding the retained chunk

**Findings.** `beamfile_get_code` (`beam_file.c:1921`) + `beamcodereader_next`
(`beam_file.c:2288`) iterate the code chunk into `BeamOp` **generic** ops
(`beam_file.h:254-268`, opcodes from `gen_opc[]`). `beamcodereader_next`
synthesizes `int_func_start/5` and `int_func_end/2` at function boundaries
(`:2269-2408`), giving clean per-function segmentation for free. The loader's
`load_code` (`beam_load.c:414`) pulls generic ops from this same reader `:432`,
then runs `erts_transform_engine` `:463` (genop→genop rewrites) and specific-op
selection `:495-639` **before** `beam_load_emit_op:641`.

**Key question answered:** re-decoding the retained raw chunk yields **generic,
pre-transform ops** — the transform pipeline is applied only to the loader's own
stream, never to the chunk. This is the correct SSA-construction level (T2/08
§4.1): generic ops are post-`beam_ssa_codegen` BEAM assembly (`get_tuple_element`,
`is_integer`, `bif`, `call_ext`, `bs_match`…), one abstraction below `beam_ssa`
— exactly what a decompiler-style SSA reconstructor wants. **The builder must
NOT call `erts_transform_engine`**; it uses `beamfile_get_code` /
`beamcodereader_next` / `beamcodereader_close` (`:2415`) directly against the
retained chunk with its own `BeamOpAllocator` (`beam_file.h:275-333`).

Register operands already carry the type-slot index packed in (§3); atoms are
resolved to `Eterm`s and NIL/labels normalized during decode (`:2118-2135`).

**Change spec (`t2_hir_builder.cpp`).** `t2_build_function(ErtsT2RetainedCode*,
Uint fn_idx) -> T2Function*`: wrap retained tables in a transient `BeamFile`
view, `beamfile_get_code`, decode `int_func_start`→`int_func_end`, run
Braun-style on-the-fly SSA (sealed blocks + incomplete phis), mapping each BEAM
op 1:1 to a `T2Op` (T2/01 §5.2, §5.5). Block boundaries: labels
(`genop_label_1`) and branch/`select` terminators. X/Y registers → SSA values via
`readVariable`/`writeVariable`; Y-slot `init`/`trim` is the local environment.

**LOC** ~900 (builder) + ~600 (HIR core `t2_hir.hpp/cpp`). **Risks:** `select_val`
/`select_arity` jump-table decoding; exception edges (`try`/`catch` → region
terminators in P0, so no exception CFG); `bs_match` sub-command decoding.

---

## 3. The Type chunk — what the SSA builder seeds from

**Findings.** Parsed by `parse_type_chunk`/`parse_type_chunk_data`
(`beam_file.c:658,622`) into `beam->types` (`BeamFile_TypeTable`,
`beam_file.h:147-153`) — an array of `BeamType{type_union, metadata_flags, min,
max, size_unit}` (`beam_types.h:71-90`); entry 0 is always "any"; a stripped/
mismatched chunk yields a 1-entry fallback table (`init_fallback_type_table:604`,
`fallback=1`). Instructions reference a slot via the **register operand itself**:
the decoder packs the type index into bits `>>10` of the reg value
(`beam_file.c:2223-2245`, `raw_arg.word_value |= index << 10`), read back by
`ArgRegister::typeIndex()` (`beam_jit_args.hpp:254`). Beamasm reads it through
`getTypeEntry` → `BeamArgType` wrapper (`beam_jit_common.hpp:208-213`,
`beam_jit_types.hpp:116-147`, exposing `type()`, `hasLowerBound/min/max`, `unit`).
`BEAM_TYPES_VERSION=3` (`beam_types.h:42`); type-union bit constants at `:44-59`.

**Change spec (`t2_types.hpp`).** C++ port of the `beam_types.hrl` lattice
(T2/01 §5.3): a `T2Type` with `meet`/`join`, ported constants (`?ATOM_SET_SIZE`,
`?TUPLE_ELEMENT_LIMIT`). Seeding: for each `param`/register-def op, read
`ArgRegister::typeIndex()`, index the **retained** type table (§1), convert
`BeamType{type_union,min,max,unit}` → `T2Type`. Reuse `beam_types.h` bit macros
directly (do not re-encode). The builder's forward-dataflow pass (T2/01 §5.4
source 3) refines from these seeds.

**LOC** ~400. **Risks:** the reconstructed builder must reproduce the same
`typeIndex()` unpack; bignum-literal type entries; keeping the C++ lattice
constants in sync with `beam_types.hrl` (add a static-assert bridge).

---

## 4. T1 PC side table — four entry kinds

**Findings.** ⚠ Confirmed: **no per-instruction PC table exists** — only
per-function line tables, resolved by `beam_ranges` `lookup_loc`
(`beam_ranges.c:330-368`) off `hdr->line_table`. The four entry kinds each have
a concrete arm emitter to hook:

1. **Function entry:** `emit_i_test_yield` (`arm/instr_common.cpp:3123`), the
   first op after the prologue; `current_label` is the function's public label
   (`a.adr(ARG3, current_label)` `:3129`); MFA stashed by `emit_i_func_info`
   (`arm/beam_asm_module.cpp:613-631`). This is where the MVP already registers
   T2 entries (`t2_entries.push_back`, `:3164-3168`).
2. **Call instructions:** `emit_i_call` (`arm/instr_call.cpp:73`),
   `emit_i_call_ext`/`_only`/`_last` (`:150-169`), and the `move_call*` variants.
3. **Post-call continuation labels:** the return landing is
   `emit_dispatch_return`/`emit_return` (`arm/instr_call.cpp:30-70`); the
   continuation PC is the address right after each call emitter above.
4. **Post-BIF/effect boundaries:** `emit_call_light_bif` (`arm/instr_bif.cpp:592`),
   `emit_i_bif1/2/3` (`:110-151`), `emit_nif_start` (`:624`).

Per-module JIT metadata today: `functions.push_back(Label)`
(`beam_asm_module.cpp:622`) builds `hdr->functions`; gdb/perf blobs via
`beamasm_metadata_insert` (`beam_jit_metadata.cpp:577`) stored on
`inst->metadata` (`module.h:41`).

**Change spec.** Record `{t1_pc_offset, beam_idx, kind}` at each of the four
emitters, keyed by function. Because the emitter streams, collect into a
per-function `std::vector<T2PcEntry>` on `BeamModuleAssembler` (like
`t2_entries`, `arm/beam_asm.hpp:1118-1125`), gated on the §7 eligibility bitmap
so ineligible functions pay nothing. At `emit_int_code_end`
(`beam_asm_module.cpp:702`) serialize into a compact sorted `{u32 offset, u32
beam_idx, u8 kind}` array appended to the retained struct (§1). Offsets are
`a.offset()`-relative, rebased to absolute at finalize. Lookup API in
`t2_pctab.c`: `erts_t2_pc_lookup(ErtsT2RetainedCode*, fn, beam_idx) ->
ErtsCodePtr` (binary search), the deopt re-call target resolver (T2/08 §4.2).

**LOC** ~300 (emitter hooks span arm files; x86 stubbed). **Risks:** offset
rebasing across veneer/stub emission (`flush_pending_stubs`,
`beam_asm_module.cpp:720`); keeping `beam_idx` aligned with the builder's op
numbering (share one decode pass).

---

## 5. beam_ranges — blob registration shape (recommendation: separate class)

**Findings.** `Range{start,end}` per module (`beam_ranges.c:33-36`), sorted array
per code_ix (`:58-64`), `find_range` binary search (`:307-328`). ⚠ **Folding T2
blobs in is blocked:** after `find_range`, `erts_lookup_function_info` casts
`hdr = (BeamCodeHeader*) rp->start` (`:260`) and walks `hdr->functions` /
`hdr->num_functions` / `hdr->line_table` (`:262-279`, `:334`). A T2 blob has no
`BeamCodeHeader`, so every existing lookup would misinterpret it. Purge/staging
is also module-shaped: `erts_start/end_staging_ranges` (`:117-191`) rebuild from
the module `code_ix` staging cycle and `erts_remove_from_ranges` sets `end=start`
(`:223-228`) at module purge (`beam_bif_load.c:2215`).

**Recommendation (with evidence): a separate T2 range class** — a parallel
sorted interval array keyed on blob `{start,end}`, with its own
`erts_t2_find_blob(pc) -> ErtsT2Blob*` returning `{MFA, resume-info}` directly,
never a `BeamCodeHeader`. Grounded in the code: (a) the `:260` cast makes folding
unsafe without a per-lookup discriminator; (b) T2 blob lifetime follows
**per-blob jettison** (trace enable, watchpoint, eviction — T2/08 §4.5/§4.6),
*decoupled* from the module `code_ix` staging that drives
`erts_start_staging_ranges`; folding would tie eviction to code-load
transactions. Needed only for `c_p->i` resume-PC translation + resume-stub
MFA/line (CPs stay T1, §4.3), so the surface is small.

**Change spec (`t2_ranges.c`).** `erts_t2_register_blob(start, end, mfa,
resume_tab)`, `erts_t2_deregister_blob(start)`, `erts_t2_find_blob(pc)`; an
`erts_t2_blobs_sz()` for accounting. P0 delivers the **design + stub API +
lookup**; population waits for P1 install.

**LOC** ~350. **Risks:** concurrency vs `find_range`'s lockless `mid` cache
(`:313,322`) — reuse the thread-progress discipline; interaction with crash-dump
literal-area sizing (`:183-189`).

---

## 6. erts_debug BIF plumbing — `t2_build_ssa/3`

**Findings.** `erts_debug:get_internal_state/1` dispatches on atom args
(`erl_bif_info.c:4271-4533`) and tuple args via `switch(arityval(tp[0]))`
(`:4534-4536`). The branch has **`case 2:` (`:4537`) and `case 3:` (`:4833`)
only — no `case 4:`.** ⚠ An `{t2_build_ssa,M,F,A}` (arity-4) tuple needs a new
`case 4:`, or use `{t2_build_ssa,{M,F,A}}` (arity-2). Direct templates on this
branch: the galloc profiler (`alloc_profile_sites` → `erts_galloc_sites_term`,
`:4287-4289`) and `recv_stats` → `erts_recv_stats_term` (`:4291-4295`); the
term-builders live in `erl_process.c:268,412` with prototypes in
`erl_process.h:126,138`, and reset hooks in `set_internal_state`
(`erl_bif_info.c:5025-5027`). `available_internal_state` gating at `:4277`.

**Change spec.** Add `case 4:` in `get_internal_state_1`:
`ERTS_IS_ATOM_STR("t2_build_ssa", tp[1])` → extract `M=tp[2],F=tp[3],A=tp[4]`,
call `erts_t2_debug_build_ssa(BIF_P, M, F, A)` (new, in `jit/t2/t2_debug.cpp`
declared in a bridge header). It looks up the module's `ErtsT2RetainedCode`, runs
`t2_build_function` (§2), and serializes the `T2Function` into a structured Erlang
term: `{ok, #{cfg => [{BlockId,[SuccId]}], ops => [{BlockId,[{Kind,Type,...}]}],
live_ranges => N}}` (or `{error,not_eligible|no_retained}`). Mirror the
`erts_recv_stats_term` build style (heap sizing then `TUPLE`/`CONS`). No
`set_internal_state` counterpart needed for P0.

**LOC** ~500 (mostly the term serializer). **Risks:** building large terms on the
BIF process heap — size then `HAlloc`, or use a magic-binary handle; must run
off the module's active code under the right code-load lock.

---

## 7. Eligibility scan — per-function op-set classification

**Findings.** The MVP makes its targeting decision **inline during emission** at
`emit_i_test_yield` via `t2_mvp_is_target()` (`arm/instr_common.cpp:3154-3176`),
driven by `current_function`/`current_arity` (`beam_asm_module.cpp:630-631`). ⚠
That hook fires at **function entry, before the body is emitted**, so it cannot
see the function's full op set — it only knows the MFA. A real
supported-op-set eligibility check (T2/02 §7.1: "scan each function's BEAM ops")
therefore **cannot be folded into the emitter**; it must be a **separate pre-pass
over the re-decoded generic ops** (§2 reader), which already segments functions
via `int_func_start`/`int_func_end` (`beam_file.c:2269-2408`).

**Change spec (`t2_eligible.c`).** During prepare (after `beamfile_read`), run
`erts_t2_eligibility_scan(BeamFile*) -> Uint32* bitmap`: iterate
`beamfile_get_code`; per function, test every generic opcode against a static
"supported phase-A set" table; set the bit iff all ops are supported. The bitmap
(a) decides retention (§1 — none set → `beamfile_free` proceeds as today, zero
overhead), (b) gates profiling-site + PC-table emission (§4, T2/02 §7.1:
eligible functions get a counter + entry-type slots), (c) is stored in the
retained struct. The emitter consults it by MFA/`current_label` exactly where
`t2_mvp_is_target()` sits today (`instr_common.cpp:3160`) — swap the hardcoded
target list for a bitmap lookup.

**LOC** ~250 (scan + supported-op table). **Risks:** op-set table drift vs the
builder's actual coverage (single source of truth: derive both from one list);
generic vs specific op naming (scan operates on generic `gen_opc` opcodes).

---

## New files, build integration, LOC

| file | role | LOC |
|---|---|---|
| `jit/t2/t2_hir.hpp/.cpp` | HIR core (arena SSA, ops, `framestate` reserved) | 600 |
| `jit/t2/t2_types.hpp` | lattice (port of `beam_types.hrl`) | 400 |
| `jit/t2/t2_hir_builder.cpp` | BEAM generic-op → SSA (Braun) | 900 |
| `jit/t2/t2_compile.cpp` | driver (lookup, orchestration) | 200 |
| `jit/t2/t2_debug.cpp` | SSA serializer + BIF hook | 500 |
| `jit/t2/t2_retain.{h,c}` | retention struct + copy/free/accounting | 250 |
| `jit/t2/t2_eligible.c` | eligibility scan | 250 |
| `jit/t2/t2_pctab.c` | PC side table build + lookup | 300 |
| `jit/t2/t2_ranges.c` | blob range class (design+API) | 350 |
| emitter hooks (`arm/*`, `beam_asm.hpp`) | PC-table + eligibility gating | 300 |
| ERTS integration (`module.h`, `beam_load.c`, `beam_bif_load.c`, `erl_alloc*`, `erl_bif_info.c`) | | 300 |

⚠ **Build system:** `Makefile.in` compile rule `$(OBJDIR)/%.o: beam/jit/%.cpp`
(`:1000`) resolves `beam/jit/t2/*.cpp` → `$(OBJDIR)/t2/*.o`, but the objects must
be added to `JIT_OBJS` (`:1086-1094`) and an `$(OBJDIR)/t2` mkdir added; and
`BEAM_CPP_SRC` (`:1393`) globs only `beam/jit/*.cpp` + `beam/jit/$(JIT_ARCH)/*.cpp`
— it must be extended with `$(wildcard beam/jit/t2/*.cpp)` or dep-generation and
compile-db will miss the new dir. Add `-Ibeam/jit/t2` near `:858`.

---

## Work order — PR-sized commits (each compiles + is testable)

1. **Skeleton + build.** Empty `jit/t2/` (`t2_hir.hpp` stub, one `.cpp`),
   Makefile/`-I`/`BEAM_CPP_SRC` wiring, `ERTS_ALC_T_T2_CODE`. *Test:* clean
   `make`; `erlang:memory(code)` unchanged.
2. **HIR core + lattice** (`t2_hir.*`, `t2_types.hpp`) with an IR
   validator + unit self-test (build a tiny function by hand, validate).
   *Test:* a debug entry point asserts round-trip.
3. **Eligibility scan** (`t2_eligible.c`) + retention copy/free
   (`t2_retain.*`), wired at finalize + purge; accounting into `size.code`.
   *Test:* load a module; `erlang:memory(code)` reflects retained bytes; purge
   frees; ineligible-only module retains nothing.
4. **BEAM→SSA builder** (`t2_hir_builder.cpp`) over the retained chunk +
   type seeding (§3). *Test:* build succeeds on the corpus without crashing;
   validator green.
5. **`t2_build_ssa/3` debug BIF** (`case 4:` + `t2_debug.cpp` serializer).
   *Test:* `erts_debug:get_internal_state({t2_build_ssa,M,F,A})` returns a dump
   for corpus functions.
6. **PC side table** (`t2_pctab.c` + emitter hooks, eligibility-gated).
   *Test:* dump the four entry kinds for a corpus function; offsets resolve to
   valid code addresses.
7. **Blob range class** (`t2_ranges.c`, design + stub API + lookup + accounting).
   *Test:* register/lookup/deregister a synthetic blob; no interference with
   `erts_lookup_function_info`.
8. **G1 gate harness** (Erlang comparator, below). *Test:* G1 runs green on the
   corpus (or emits a diff report), making the gate runnable.

---

## G1 corpus and fidelity comparator

**Comparator (Erlang, `t2_g1_SUITE` / escript).** For each corpus module: (a)
compile source with `compile:file(Src, [dssaopt,...])` to dump AOT `beam_ssa`
(`compile.erl:1788,1810` — `dssa`→`ssa`, `dssaopt`→`ssaopt` listings exist); (b)
call `erts_debug:get_internal_state({t2_build_ssa,M,F,A})` for the reconstructed
SSA; (c) compare: identical CFG shape (block count + edge set modulo block
renumber), live-range counts within ε, per-block op-sequence correspondence.
⚠ **Caveat:** the reconstruction is from *post-`beam_ssa_codegen`* BEAM asm, one
lowering below `dssaopt`; compare structure modulo codegen expansions
(tuple/list build, guard-BIF inlining) — not byte-identity. Material divergence →
SSA-chunk fallback (T2/02 §7.8).

**Corpus = experiment subjects + stdlib slice:**
- Experiment subjects (in-tree, `PLAN/`): `t2_mvp` (`PLAN/mvp/t2_mvp.erl`:
  `total/2`,`diff/2`), `t2_gbin`/`json` (`number/7`,`number_frac_cont/7`,
  `string_ascii/7`), `t2_gmap` (`sum_scores/2`), `t2_g31`
  (`dispatch/2`,`handle_call/3`), `t2_g3`/`erl_types` (`are_all_limited/2`,
  `is_limited/2`) — from `PLAN/verification/*.erl` and the kit table (08 Appendix).
- Stdlib slice (loop/binary/tuple/list shapes the tier targets): `lists`, `maps`,
  `binary`, `string`, `base64`, `json`, `unicode` — exercising self-tail
  recursion, `bs_match` scan loops, `get_map_elements`, guard-heavy leaves.

---

## Consolidated surprises (flag list)

1. Raw code bytes are **not owned** by `BeamFile` (pointer into transient input,
   `beam_file.c:1341`) — retention must **copy**, not keep.
2. Literals **already survive** load (`beamfile_move_literals:1838`); the **Type
   table does not** (`beamfile_free:1697`) — copy types, index-map literals.
3. `beam_ranges` folding is **blocked** by the `BeamCodeHeader*` cast
   (`beam_ranges.c:260`) → separate blob range class.
4. Eligibility **cannot** ride the MVP's `emit_i_test_yield` hook (fires before
   the body) — needs a **separate pre-pass** over decoded generic ops.
5. `get_internal_state` tuple switch has **no `case 4:`**.
6. Retained-chunk re-decode yields **generic pre-transform ops** — builder must
   **bypass** `erts_transform_engine`.
7. `BEAM_CPP_SRC` (`Makefile.in:1393`) omits `beam/jit/t2/` — extend or dep-gen
   breaks.
