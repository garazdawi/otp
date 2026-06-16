#!/bin/bash
# Standalone build script for beam_jit_compile.
#
# Demonstrates the concrete compile + link pathway. The full
# Makefile integration is in Makefile.in (sketched, not wired in
# yet); this script is the manual equivalent that someone can run
# during development to see what's needed.
#
# Status (committed):
#   - Compiles all loader source files (beam_load.c, beam_file.c,
#     beam_transform_helpers.c, jit/asm_load.c)
#   - Compiles cache_tool source (main, writer, reader, stubs)
#   - Compiles common JIT C++ helpers (beam_jit_common.cpp, etc.)
#   - Compiles asmjit emitter sources (beam_asm_module.cpp, etc.)
#   - Reports the categorised list of unresolved runtime symbols
#
# Open work (~ 50 stubs):
#   - 47 erts_* runtime hooks (atom/export tables, allocator
#     wrappers, magic refs, MD5, dsprintf, etc.)
#   - 3 loader helpers (catches_init, export_trampoline,
#     make_current_old)
#   - 2 ERTS_GLOBAL_LIT_* constants
#   - A handful of misc (gen_opc, opc, tag_to_letter, bif_table)

set -e

cd "$(dirname "$0")"

ROOT=$(git rev-parse --show-toplevel)
ARCH="aarch64-apple-darwin25.4.0"   # TODO: detect host arch

INC=(
    "-I$ROOT/erts/emulator/$ARCH/opt/jit"
    "-I$ROOT/erts/emulator/beam"
    "-I$ROOT/erts/emulator/sys/unix"
    "-I$ROOT/erts/emulator/sys/common"
    "-I$ROOT/erts/emulator/$ARCH"
    "-I$ROOT/erts/emulator/zstd"
    "-I$ROOT/erts/emulator/ryu"
    "-I$ROOT/erts/emulator/pcre"
    "-I$ROOT/erts/include"
    "-I$ROOT/erts/include/$ARCH"
    "-I$ROOT/erts/include/internal"
    "-I$ROOT/erts/include/internal/$ARCH"
    "-I$ROOT/erts/emulator/beam/jit"
    "-I$ROOT/erts/emulator/beam/jit/arm"
    "-I$ROOT/erts/$ARCH"
    "-I$ROOT/erts/emulator/asmjit"
)

CDEF=(
    -DHAVE_CONFIG_H
    -DBEAMASM=1
    -DUSE_THREADS
    -D_THREAD_SAFE
    -D_REENTRANT
    -DPOSIX_THREADS
    -DASMJIT_STATIC
    -DCACHE_TOOL_BUILD=1
)

BUILD=/tmp/beam_jit_compile_build
mkdir -p "$BUILD"

cc=${CC:-gcc}
cxx=${CXX:-g++}

# C files: cache_tool's own + the loader sources
C_SRCS=(
    "main.c"
    "cache_writer.c"
    "cache_tool_stubs.c"
    "cache_tool_reader.c"
    "$ROOT/erts/emulator/beam/beam_load.c"
    "$ROOT/erts/emulator/beam/beam_file.c"
    "$ROOT/erts/emulator/beam/beam_transform_helpers.c"
    "$ROOT/erts/emulator/beam/jit/asm_load.c"
)

# asmjit library — compile in directly (62 source files).
# Wildcard expansion matches the runtime's Makefile.in pattern.
ASMJIT_SRCS=(
    $ROOT/erts/emulator/asmjit/core/*.cpp
    $ROOT/erts/emulator/asmjit/support/*.cpp
    $ROOT/erts/emulator/asmjit/arm/*.cpp
)

# C++ files: shared JIT helpers + the per-arch emitters
CXX_SRCS=(
    "$ROOT/erts/emulator/beam/jit/beam_jit_common.cpp"
    "$ROOT/erts/emulator/beam/jit/beam_jit_bs.cpp"
    "$ROOT/erts/emulator/beam/jit/beam_jit_metadata.cpp"
    "$ROOT/erts/emulator/beam/jit/beam_jit_main.cpp"
    "$ROOT/erts/emulator/beam/jit/arm/beam_asm_module.cpp"
    "$ROOT/erts/emulator/beam/jit/arm/beam_asm_global.cpp"
    "$ROOT/erts/emulator/beam/jit/arm/instr_arith.cpp"
    "$ROOT/erts/emulator/beam/jit/arm/instr_bif.cpp"
    "$ROOT/erts/emulator/beam/jit/arm/instr_bs.cpp"
    "$ROOT/erts/emulator/beam/jit/arm/instr_call.cpp"
    "$ROOT/erts/emulator/beam/jit/arm/instr_common.cpp"
    "$ROOT/erts/emulator/beam/jit/arm/instr_float.cpp"
    "$ROOT/erts/emulator/beam/jit/arm/instr_fun.cpp"
    "$ROOT/erts/emulator/beam/jit/arm/instr_guard_bifs.cpp"
    "$ROOT/erts/emulator/beam/jit/arm/instr_map.cpp"
    "$ROOT/erts/emulator/beam/jit/arm/instr_msg.cpp"
    "$ROOT/erts/emulator/beam/jit/arm/instr_records.cpp"
    "$ROOT/erts/emulator/beam/jit/arm/instr_select.cpp"
    "$ROOT/erts/emulator/beam/jit/arm/instr_trace.cpp"
    "$ROOT/erts/emulator/beam/jit/arm/process_main.cpp"
)

# Additional runtime sources we link in directly (instead of
# stubbing). Each one is "the real runtime code", brought in
# because the loader's happy path actually calls into it.
EXTRA_C_SRCS=(
    "$ROOT/erts/emulator/$ARCH/opt/jit/beam_opcodes.c" # gen_opc, opc, tag_to_letter, erts_transform_engine
    "$ROOT/erts/emulator/beam/external.c"             # erts_decode_ext*
    "$ROOT/erts/emulator/beam/erl_map.c"              # erts_validate_and_sort_flatmap
    "$ROOT/erts/emulator/beam/erl_message.c"          # erts_factory_*
    "$ROOT/erts/emulator/beam/erl_bits.c"             # erts_wrap_refc_bitstring, erts_bs_*
    "$ROOT/erts/emulator/beam/copy.c"                 # size_object_x, eq, copy_struct_x
    "$ROOT/erts/emulator/beam/utils.c"                # make_hash2
)
C_SRCS+=("${EXTRA_C_SRCS[@]}")

echo "=== compiling C files ==="
OBJS=()
for src in "${C_SRCS[@]}"; do
    base=$(basename "$src" .c)
    obj="$BUILD/$base.o"
    OBJS+=("$obj")
    [ "$obj" -nt "$src" ] && { echo "  $base.o up-to-date"; continue; }
    echo "  $base.c"
    $cc -O0 -g "${INC[@]}" "${CDEF[@]}" -c "$src" -o "$obj" 2>&1 | sed 's/^/    /'
done

echo
echo "=== compiling asmjit (${#ASMJIT_SRCS[@]} files) ==="
for src in "${ASMJIT_SRCS[@]}"; do
    base=$(basename "$src" .cpp)
    obj="$BUILD/asmjit_$base.o"
    OBJS+=("$obj")
    [ "$obj" -nt "$src" ] && continue
    $cxx -std=c++17 -O0 -g "${INC[@]}" "${CDEF[@]}" -c "$src" -o "$obj" 2>&1 | sed 's/^/    /' | head -3
done
echo "  done"

echo
echo "=== compiling C++ files ==="
for src in "${CXX_SRCS[@]}"; do
    base=$(basename "$src" .cpp)
    obj="$BUILD/$base.o"
    OBJS+=("$obj")
    [ "$obj" -nt "$src" ] && { echo "  $base.o up-to-date"; continue; }
    echo "  $base.cpp"
    $cxx -std=c++17 -O0 -g "${INC[@]}" "${CDEF[@]}" -c "$src" -o "$obj" 2>&1 | sed 's/^/    /' | head -3
done

echo
echo "=== linking ==="
# Use -undefined dynamic_lookup on macOS so the link succeeds even
# with unresolved symbols (they'll error at runtime IF called).
# This lets the tool drive itself — only the codepath that's
# actually exercised needs stubs filled in. Linux equivalent is
# `-Wl,--unresolved-symbols=ignore-all`.
LDFLAGS=()
case "$(uname -s)" in
    Darwin) LDFLAGS=(-Wl,-undefined,dynamic_lookup) ;;
    Linux)  LDFLAGS=(-Wl,--unresolved-symbols=ignore-all) ;;
esac

$cxx -O0 -g "${OBJS[@]}" "${LDFLAGS[@]}" -o "$BUILD/beam_jit_compile" 2>&1 | tee "$BUILD/link.log" | tail -3

echo
echo "=== unresolved symbols (counted) ==="
grep -oE '"_[a-zA-Z_0-9]+"' "$BUILD/link.log" 2>/dev/null | sort -u > "$BUILD/missing.txt"
echo "  $(wc -l < $BUILD/missing.txt) distinct symbols"
echo
echo "=== first 20 ==="
head -20 "$BUILD/missing.txt"
