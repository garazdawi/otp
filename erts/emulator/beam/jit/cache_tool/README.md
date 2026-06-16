# beam_jit_compile — standalone JIT cache producer

Build-time tool for IDEAS/07 #1: takes `.beam` files as input,
produces `.jc` (JIT cache) blobs by running the same loader +
asmjit emit pipeline the live emulator uses, but with the runtime
hooks replaced by stubs that record *symbolic* references rather
than resolving them to live VM addresses.

The same cache-loader code path consumes both these files at
runtime and any cache written by the runtime itself (under `+JC on`).
The tool is essentially the runtime's cache-producer code lifted
into a host-runnable binary.

## Why a separate tool

The alternative is a two-stage `beam.jit` build (build → run to
dump cache → relink). That works but:

  - Doubles `beam.jit` link time per build
  - Breaks cross-compilation (target binary can't run on host)
  - Duplicates code paths for "produce a cache" between build-time
    and runtime
  - Hard to test the cache subsystem in isolation

The standalone tool addresses all four. Cost: ~2 weeks to build
the first time, free thereafter.

See `IDEAS/07-startup-time-optimization.md` §1a-c for the full
design discussion.

## Directory layout

    cache_tool/
      README.md                ← this file
      main.c                   ← entry point, CLI parsing
      cache_tool_stubs.c       ← runtime hook stubs (atom_put,
                                  bif_table, alloc, etc.)
      cache_writer.c           ← serialise asmjit CodeHolder +
                                  reloc lists + literal pool to
                                  the .jc file format
      cache_tool.h             ← shared types and prototypes
      Makefile.in              ← build rules for the tool itself

## Object surface

The tool LINKS against the same source files as `beam.jit`:

  Loader path:
    erts/emulator/beam/beam_load.c
    erts/emulator/beam/beam_transform_helpers.c
    erts/emulator/beam/beamfile.c
    erts/emulator/beam/jit/asm_load.c

  asmjit emitters (per arch — built for the *target* arch):
    erts/emulator/beam/jit/<arch>/beam_asm_module.cpp
    erts/emulator/beam/jit/<arch>/beam_asm_global.cpp
    erts/emulator/beam/jit/<arch>/instr_*.cpp

  Common JIT:
    erts/emulator/beam/jit/beam_jit_common.cpp
    erts/emulator/beam/jit/beam_jit_bs.cpp
    erts/emulator/beam/jit/beam_jit_metadata.cpp

  asmjit library: as bundled with OTP

Everything else (scheduler, memory management, BIFs, I/O,
distribution, ETS, atom table, …) is REPLACED by stubs in
`cache_tool_stubs.c`. Each stub either:

  (a) provides a minimal implementation sufficient for the
      compile path (e.g. malloc-based `erts_alloc`), or
  (b) records a symbolic reference for the cache writer to
      serialise (e.g. atom names, BIF names, MFAs).

## What the stubs replace

    Runtime hook                  | Stub strategy
    ------------------------------|-----------------------------
    erts_atom_put                 | sequential index alloc +
                                  | string table; recorded as
                                  | atom_relocs by emitters
    bif_table[]                   | empty array; the emitters
                                  | now register a symbolic
                                  | reference via emit_mov_bif
    erts_export_put / lookup      | recording stub: writes
                                  | symbolic (M,F,A) per emit
    erts_alloc / erts_free        | malloc / free
    Process *, scheduler state    | unused on compile path —
                                  | stub returns NULL; emitter
                                  | code paths that dereference
                                  | are flagged build-time errors
    erts_dispatch_table /         | recorded by name; resolved at
       global fragments           | runtime by cache loader
    Executable memory alloc       | regular mmap PROT_READ|WRITE,
                                  | sealed PROT_EXEC after writes

## Output file format

See beam_jit_cache.h (already in the tree). Per-module:

    header (magic "EJC1", erts_vsn, jit_vsn, arch, cflags_hash)
    module_name_str_idx
    beam_sha256
    asmjit_code_blob
    asmjit_label_table         (POD vector from CodeHolder)
    asmjit_reloc_list          (POD vector from CodeHolder)
    our_symbolic_relocs        (atom_relocs, bif_relocs,
                                mfa_relocs, vm_static_relocs)
    literal_pool_blob          (heap-format, with atom + ptr
                                relocs alongside)
    string_table               (atom names, MFA strings)
    func_table, exports, imports
    on_load_chunk (optional)
    line_table (optional)

The runtime cache loader (separate code, in
beam_load.c next to the existing load_code()) reads this format,
remaps the atoms, copies literals, patches code via the reloc
lists, and hands off to beam_load_finalize_code() — same as the
normal loader's final step.

## Usage

    # Single module
    beam_jit_compile --arch aarch64 \
                     --out kernel.beam.jc \
                     lib/kernel/ebin/kernel.beam

    # Bundle the boot set
    beam_jit_compile --arch aarch64 \
                     --bundle \
                     --out bin/boot.jitcache \
                     erts/preloaded/ebin/*.beam \
                     lib/kernel/ebin/{kernel,application,...}.beam \
                     lib/stdlib/ebin/{lists,gen_server,...}.beam

    # Runtime use also: triggered by +JC on at runtime, but invokes
    # the same library code (just inline) — no fork/exec.

## Build integration

The tool is built once per host (it's a host artifact, not target).
It can target any architecture supported by asmjit.

In the normal OTP build:

    make all
      → builds beam.emu, beam.jit, beam.smp (with empty JIT cache)
      → builds beam_jit_compile (host tool)
      → runs beam_jit_compile to produce bin/boot.jitcache
      → bin/boot.jitcache is shipped as part of the OTP install

No relink of beam.jit. The cache is a separate file at
$ROOT/bin/boot.jitcache. Runtime mmaps it; if missing, falls
through to today's loader path.

For cross-compilation:

    make CROSS=aarch64-linux-gnu-
      → beam.jit is aarch64 (won't run on host)
      → beam_jit_compile is HOST binary that targets aarch64
        (asmjit's emitter doesn't care what host it runs on)
      → tool runs on host, produces aarch64 cache
      → cache shipped in the aarch64 OTP install

No QEMU dance.

For `NO_JIT_CACHE=1`:

    make NO_JIT_CACHE=1
      → builds beam.* as today
      → skips beam_jit_compile build and cache generation
      → identical artifact to today
      → useful for: fast iterative dev, bootstrap stage, debug
        the cache-loading path

## Stub drift CI test

A `make test-jit-cache-equivalence` target builds both the tool
and a runtime PROFILE_JIT_DUMP variant of beam.jit, runs each
over a fixed set of .beam files, and byte-compares the outputs
after canonicalising the relocations to a common base. Catches
drift between the tool's stubs and the runtime's real hooks
before it ships.

## Next steps after scaffolding

This directory currently contains README + stub headers showing
the intended shape. Filling in the bodies is the bulk of the work:

  1. Build the host-tool Makefile target (small, the existing
     `bootstrap/` directory and `utils/make_preload` have similar
     patterns).
  2. Stub `cache_tool_stubs.c` — start with the loader's first-
     pass requirements (atom_put, alloc) and let "undefined
     symbol" errors guide the rest.
  3. Wire in the existing `beam_jit_cache.h` reloc list to the
     asmjit emit calls (the `emit_mov_*` migration sketched in
     `beam_jit_cache_emit.hpp`).
  4. Implement `cache_writer.c` — serialise the asmjit CodeHolder
     parts + our reloc lists.
  5. Implement the runtime reader counterpart in `beam_load.c`.
  6. CI equivalence test.

The runtime side (#5) is what makes this user-visible. Until that's
in, the tool produces files nobody reads.
