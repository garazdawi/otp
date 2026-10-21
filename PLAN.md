# Reusable BeamAsm code plan

## Outcome

Add an opt-in, versioned native-code sidecar for a `.beam` file. A producer
running inside a real VM emits the sidecar. A later VM validates it, rebuilds
VM-local state, installs its native code without running BeamAsm, and otherwise
falls back to the ordinary loader.

Start on x86-64. Prove the design with one module, then the preloaded set, then
Kernel and STDLIB. Reuse the same code image and typed fixup model for ELF
objects only after the sidecar path works.

This ordering proves the hard part first: whether BeamAsm output can survive a
new process, ASLR, and different atom indices. Starting with ELF would add an
object writer, linker integration, section discovery, static-code ownership,
and text-patching policy before answering that question.

## Checkout and prior work

Planning baseline:

- Erlang/OTP commit: `38f9af4e49bdf3a61be894578b863d3bdcec190e`
- OTP version: `30.0-rc0`
- Bundled AsmJit version: 1.21.0
- Bundled AsmJit source revision:
  `5fe1940275d04432da841896bac0a66cc2375551`
- Prototype branch: `lukas/erts/jit-cache-prototype`
- Prototype base: `d493cc2ba4a7561164a2ca152c7509b12f18f1fe`
- Prototype tip: `a841e85b9c3dcc1f3475136c7fe342d565c8ffe1`

The prototype is useful evidence, but should not be merged wholesale.

### What the prototype proved

- A semantic relocation list can identify enough VM-local values to make all
  22 preloaded module artifacts deterministic across separate producer
  processes on its AArch64 macOS build.
- Canonicalizing relocation slots and comparing outputs from two processes is
  an effective way to find missed pointers under ASLR.
- `.jc` files reached the boot loader for all 22 preloaded modules.
- `prim_eval` alone loaded and executed overlaid cached bytes. Its
  `module_info` and receive paths worked.
- Useful concepts exist for relocation classification, startup measurement,
  sidecar embedding, parser/validator structure, and fallback.

### What the prototype did not prove

- It did not avoid JIT work. The cache-hit path still called
  `erts_prepare_loading()`, including transformations and full BeamAsm
  emission, before overlaying cached bytes.
- It did not directly load general modules. Every preloaded module except
  `prim_eval` fell back to normal loading.
- It did not demonstrate literals, strings, closures, exceptions, `on_load`,
  native records, changed atom order, or an installation-wide cache.
- It did not demonstrate startup improvement.
- It primarily implemented AArch64. Its x86 relocation recording remained
  incomplete, and its build script hard-coded one macOS target.

### What to retain and what to replace

Retain or reimplement in small commits:

- phase timing and `scripts/startup-bench` ideas;
- semantic relocation taxonomy;
- canonical relocation slots;
- two-process determinism validation;
- loader fallback and preload attachment points.

Replace:

- The standalone producer with roughly 1,500 lines of ERTS/BIF stubs. First
  producer must run inside the real VM so it cannot drift from loader state.
- `dladdr`/`dlsym` names and hand-maintained local-symbol tables. Use a closed,
  versioned enum of ERTS symbol and fragment IDs.
- Native C structs and unchecked offsets in the file format. Use an explicit,
  endian-defined schema and validate every range before allocation.
- Overlay loading. Reconstruct metadata and install copied code directly so a
  hit never invokes BeamAsm.
- Unconditional general-width immediate emission in the normal JIT. Any
  cache-specific encoding mode must be gated and measured.

## Design boundary

Do not serialize a live `asmjit::CodeHolder`, labels, expression pointers,
callbacks, `Eterm` values, or final `BeamCodeHeader` memory. Serialize the
result of emission as flat sections plus OTP-owned metadata:

1. immutable code and read-only data bytes;
2. section layout and alignment;
3. internal/base relocations expressed as offsets and addends;
4. typed symbolic runtime fixups;
5. function, export, local, line, debug, coverage, and `on_load` descriptors;
6. recipes needed to reconstruct literals, catches, fun entries, strings, and
   other VM-owned objects.

AsmJit remains unmodified. OTP extracts required information through its own
BeamAsm wrapper before `relocate_to_base()` mutates the image.

## Dependency audit

Maintain a checked inventory in the implementation. Serialization must fail if
an AsmJit relocation or OTP patch site cannot be translated to an approved
kind.

### Standard relocations

These depend on the loaded section base, but not VM identity:

- intra-module labels and cross-section references;
- function and `on_load` pointers in reconstructed headers;
- line/debug tables containing code addresses;
- pointers to embedded strings, attributes, compile info, and MD5 data;
- module calls or veneers targeting named global BeamAsm fragments.

Represent each as `{section, offset, width, target_section_or_symbol, addend}`.
Use ordinary ELF relocations for these in the later ELF container.

### Runtime fixups

These need values allocated or resolved in the consumer VM:

- atoms, represented by UTF-8 name, never by atom index or tagged `Eterm`;
- imports and exports, represented by MFA and dispatch kind;
- BIFs and ERTS helpers, represented by stable resolver IDs;
- VM globals such as `the_active_code_index`, represented by stable IDs;
- global BeamAsm fragments, represented by stable fragment IDs;
- static and dynamic literals, including internal pointers and map hashes;
- BEAM string-table addresses;
- catches and their newly allocated catch indices;
- lambdas, `ErlFunEntry` objects, trampolines, and literal fun repair;
- line/debug/coverage allocations and literal-bearing metadata;
- native-record definitions and runtime record entries.

Convert existing `BeamModuleAssembler` patch maps for imports, lambdas,
literals, catches, and strings to stable section offsets after flattening.
Audit all generic `imm(pointer)`, `mov_imm(pointer)`, constant-pool, dispatch
table, and runtime-call paths for missing categories.

### Structural dependencies

These can change emitted control flow or instruction layout, so a late value
patch is insufficient:

- `select_val` sorting and binary-search trees based on atom values;
- folded comparisons whose choice depends on relationships between atom values;
- native-record field ordering and baked field positions;
- target-specific immediate specialization, especially AArch64 atom rebasing;
- any optimization chosen from literal hashes or runtime addresses.

Phase 1 rejects modules that hit these cases. Phase 2 adds a cacheable emission
mode whose decisions use canonical symbolic ordering, or stores a safe
high-level recipe when canonical emission would hurt normal JIT code. The
design must not require a globally fixed atom table.

### Compatibility restrictions

Version 1 requires an exact match for:

- architecture, OS ABI, word size, and endianness;
- ERTS/JIT ABI and code-generator fingerprint;
- relevant configure and build switches, including native Erlang stack,
  frame pointers, debug/hard-debug, coverage, and tagging layout;
- global fragment ABI;
- BEAM opcode/loader format;
- source BEAM digest;
- required CPU feature mask.

The first implementation may use an exact build ID and exact CPU feature mask.
Compatibility can be widened only with tests showing which differences are
safe.

## Sidecar format version 1

Use fixed-width little-endian integers and a section directory. Never cast file
bytes to a native struct.

Header fields:

- magic, format version, header size, and total size;
- module name and SHA-256 of the complete source BEAM;
- ERTS/JIT ABI fingerprint and code-generator fingerprint;
- target triple, pointer width, endianness, and required CPU features;
- build/configuration flags that affect emitted code;
- directory count and payload digest.

Sections:

- native `.text`, read-only data, and writable template data;
- relocations and typed runtime fixups;
- atom and string tables;
- function, export, local, catch, lambda, literal, line, debug, coverage,
  native-record, and `on_load` descriptors;
- optional diagnostic names, excluded from correctness.

Parser rules:

- check header and payload digests before trusting metadata;
- use checked addition and multiplication for every offset/count calculation;
- reject overlapping, misaligned, duplicate, out-of-range, or unknown sections;
- reject unknown mandatory fixup kinds and invalid widths or patch ranges;
- reject duplicate patch sites and patches outside declared writable regions;
- resolve all compatibility conditions before creating atoms or allocating
  executable memory;
- cap counts and total artifact size relative to source BEAM size;
- leave no partially published module after failure.

Treat an externally supplied sidecar as executable input. Automatic adjacent
sidecar discovery should require trusted installation ownership or an explicit
enablement policy. Embedded build artifacts inherit the release's trust model.

## Implementation sequence

Each step should be a focused commit that leaves ordinary BEAM loading working.

### 0. Establish measurements and inventory

1. Add opt-in phase counters around BEAM parsing, transforms, specific-op
   selection, BeamAsm emission, AsmJit flatten/resolve/relocate, metadata
   construction, runtime fixups, and publication.
2. Record emitted code/data size and module counts.
3. Extend the startup benchmark to compare normal, valid-cache, and
   rejected-cache runs with warmups plus median and tail percentiles.
4. Add an x86-64 relocation audit mode. Generate the same module in two fresh
   processes, canonicalize known patch slots, and fail on unexplained byte or
   layout differences.
5. Produce the dependency inventory above with concrete emitting call sites.

Exit criteria:

- baseline numbers are reproducible;
- every observed cross-process difference maps to a named relocation/fixup or
  a documented structural rejection;
- audit mode fails closed on unknown AsmJit relocations.

### 1. Add an in-VM producer primitive

Add an experimental internal primitive that accepts a BEAM binary and output
path, runs the real loader and x86-64 BeamAsm pipeline, and atomically writes a
sidecar. Keep it under `erts_internal` or an emulator test interface until the
format stabilizes.

The producer must:

- emit from the same `BeamFile` and `BeamModuleAssembler` path as normal load;
- stop at the stable boundary after layout/fixup discovery and before final
  base relocation;
- translate AsmJit relocations immediately into OTP-owned records;
- translate all VM identities to symbolic records;
- reject unsupported structural dependencies with
  `{error,{unsupported,Reason}}`;
- write to a temporary file, `fsync` as appropriate, then rename;
- produce byte-identical output in two producer VMs with ASLR.

Initial scope: one x86-64 module with local calls and simple atom constants.
No standalone host tool, bundle format, or cross-compilation yet.

### 2. Split loader preparation from emission

Refactor current flow into three explicit stages:

1. parse and validate BEAM; build source-derived loader metadata;
2. transform/select/emit with BeamAsm;
3. allocate VM-owned objects, patch, and publish through common finalization.

Normal loading runs all three. Cached loading validates the sidecar, runs the
minimum source/metadata work from stage 1, replaces stage 2 with native image
installation, then uses the same stage 3.

Do not optimize away BEAM parsing in this step. First prove that BeamAsm emit
count is zero on a cache hit. Metadata parsing can be cached later if timings
show it matters.

### 3. Implement direct cache installation

On a valid hit:

1. allocate through the JIT allocator as writable, non-executable memory;
2. copy declared sections and rebuild `BeamCodeHeader` and function pointers;
3. recreate and move literals, recomputing derived hashes;
4. resolve atoms, imports, exports, BIFs, helpers, globals, and fragments;
5. allocate and patch catches and fun entries;
6. build strings, line/debug/coverage tables, and `on_load` state;
7. apply base relocations and typed runtime fixups with range checks;
8. flush the instruction cache where required;
9. change memory to executable/read-only using existing W^X machinery;
10. enter common publication/finalization.

On any miss, mismatch, unsupported feature, or resolution failure, release all
temporary state and use normal loading. An explicitly requested corrupt cache
returns `badfile`; opportunistic cache lookup falls back and increments a
reason-specific counter.

### 4. Prove cross-process correctness

Create one sidecar in VM A. In VM B:

- enable ASLR normally;
- create many padding atoms in shuffled order before loading;
- assert that the cached path invokes BeamAsm zero times;
- compare behavior with ordinary loading;
- purge, reload, trace, and exercise both active code indices.

Grow a fixture ladder:

1. local calls and simple atom constants;
2. remote imports, exports, and BIF calls;
3. integers, binaries, tuples, lists, maps, static and dynamic literals;
4. strings and bit syntax using the BEAM string table;
5. closures with and without free variables;
6. catches, raises, stack traces, and line information;
7. `on_load` success and failure;
8. atom `select_val` with deliberately changed atom order;
9. debugger, coverage, tracing, and NIF prologue patching;
10. native records.

Structural cases remain explicit rejections until cacheable emission handles
them. Do not count fallback as fixture success.

Add negative cases for changed BEAM content, wrong format/build/architecture,
missing CPU features, incompatible flags, truncation, integer overflow,
overlapping sections, unknown fixups, invalid targets, and unresolved symbols.

Reuse relevant coverage from `erts/emulator/test/code_SUITE.erl`,
`fun_SUITE.erl`, `beam_literals_SUITE.erl`, `code_parallel_load_SUITE.erl`,
`jit_SUITE.erl`, and Kernel's code/coverage suites.

### 5. Add preloaded-module integration

Use the just-built target VM to generate caches for preloaded modules during a
native build. Embed one indexed bundle or install a bundle beside the runtime;
do not embed a complete per-module bundle around every module.

Requirements:

- missing or rejected cache preserves current boot behavior;
- one bad module does not invalidate unrelated entries;
- every cache hit skips BeamAsm;
- all preloaded modules pass the semantic and negative test matrix;
- builds can disable generation explicitly;
- cross builds without a runnable target VM keep normal behavior.

Only after this works should a narrow producer library or standalone producer
be considered for cross-compilation. Do not recreate the prototype's broad VM
stub surface.

### 6. Cover Kernel, STDLIB, and release artifacts

Generate sidecars from final installed `.beam` files, after all transforms and
stripping. Add an indexed application or release bundle to avoid thousands of
filesystem probes. Keep `.beam` authoritative and permit mixed hit/miss loads.

Expose low-level primitives first, for example an internal equivalent of:

- prepare a native sidecar from a BEAM binary;
- load a BEAM binary with an explicitly supplied sidecar;
- inspect cache compatibility and rejection reason.

Add an automatic code-path policy only after format and trust rules settle.
Never write beside system BEAM files during ordinary module loading.

### 7. Measure and set acceptance gates

For preloaded, Kernel, STDLIB, and a representative release, report:

- transform/select/emit/relocate time avoided;
- remaining parse, fixup, metadata, and publication time;
- end-to-end startup median and p95 for warm and cold filesystem caches;
- producer time and release-build impact;
- artifact bytes per module and total installed growth;
- RSS and private/shared executable pages;
- cache hit, miss, unsupported, incompatible, and corrupt counts;
- steady-state code size and runtime performance versus normal JIT output.

Acceptance requires zero BeamAsm emissions on hits, no semantic regressions,
clean fallback, and a measurable startup win larger than artifact and install
costs for the selected target set.

## ELF object path

Begin only after the sidecar loader passes phase 4 and preloaded integration
shows a worthwhile gain. ELF becomes another representation of the same
logical artifact:

- native code/data in `.text`, `.rodata`, and writable template sections;
- standard ELF relocations for section bases, local labels, exported C helper
  symbols, and stable shared fragments;
- custom `.beam.module` descriptors;
- custom `.beam.fixups` records for atoms, exports/funs, literals, catches,
  records, and other runtime allocations;
- a startup registry that enumerates linked descriptors and calls the same
  cache installer/finalizer.

Keep external Erlang calls through export/fun dispatch so code replacement,
tracing, breakpoints, and active-code-index semantics remain correct.

Before claiming a static executable, define:

- ownership, purge, and replacement rules for linked module code;
- writable prologues, indirection cells, or trampolines for tracing/NIF patches;
- how read-only linked text becomes patchable without violating W^X;
- whether linked code executes in place or is copied into JIT memory;
- relocation behavior if it is copied;
- module discovery, startup order, `on_load`, and failure rollback;
- compatibility between linked objects and the exact ERTS binary.

First ELF acceptance test links one module with ERTS and passes ASLR plus
changed-atom-order tests. Next link the preload set. Only then evaluate a full
release executable.

## Go/no-go decisions

Stop and reassess after dependency audit if atom values or runtime addresses
control many unrecordable code-shape decisions. Fix cacheable emission first;
do not cover structural differences with increasingly fragile byte patches.

Stop before broad packaging if direct cache loading does not save meaningful
time after BEAM parsing and runtime initialization. In that case, retain the
loader split and measurements, but do not add an installation-wide artifact
system.

Proceed to ELF only if sidecars prove both cross-process correctness and useful
startup savings. Otherwise ELF adds packaging complexity without solving the
underlying portability problem.
