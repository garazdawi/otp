#!/bin/sh
#
# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 2026. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# %CopyrightEnd%
#
# Convert the clang source-based coverage *.profraw files dumped by
# erts_debug:coverage/1 (via cth_coverage, into <dir>/native/) into a
# single LCOV tracefile, using llvm-profdata + llvm-cov against the
# clangcov emulator binary.
#
# Usage: native_cov_to_lcov.sh <native-dir> <emulator-binary> <out.info>
#
# Best-effort: if the llvm tools, the .profraw files, or the emulator
# binary are missing it prints a note and exits 0 (so a report step that
# calls it never fails a build that simply was not a clangcov run).

set -eu

NATIVE_DIR=${1:?usage: native_cov_to_lcov.sh <native-dir> <emu-binary> <out.info>}
EMU=${2:?missing emulator binary argument}
OUT=${3:?missing output file argument}

skip() { echo "native_cov_to_lcov: $1; skipping" >&2; exit 0; }

# Locate the llvm tools: on PATH (typical Linux) or via xcrun (macOS).
if command -v llvm-profdata >/dev/null 2>&1 && command -v llvm-cov >/dev/null 2>&1; then
    PROFDATA="llvm-profdata"; COV="llvm-cov"
elif command -v xcrun >/dev/null 2>&1 && xcrun --find llvm-profdata >/dev/null 2>&1; then
    PROFDATA="xcrun llvm-profdata"; COV="xcrun llvm-cov"
else
    skip "llvm-profdata/llvm-cov not found"
fi

# If the exact binary is absent, fall back to any beam.clangcov.* sibling
# (the flavor suffix is .jit on JIT targets, .smp elsewhere).
if [ ! -f "$EMU" ]; then
    found=""
    for cand in "$(dirname "$EMU")"/beam.clangcov.*; do
        [ -f "$cand" ] && found="$cand" && break
    done
    [ -n "$found" ] || skip "clangcov emulator binary not found"
    EMU="$found"
fi

# shellcheck disable=SC2086
set -- "$NATIVE_DIR"/*.profraw
if [ ! -e "$1" ]; then
    skip "no .profraw files in $NATIVE_DIR"
fi

PROFDATA_FILE="$NATIVE_DIR/merged.profdata"
$PROFDATA merge -sparse "$@" -o "$PROFDATA_FILE"
$COV export -format=lcov -instr-profile="$PROFDATA_FILE" "$EMU" > "$OUT"

echo "native_cov_to_lcov: wrote $OUT ($(grep -c '^SF:' "$OUT" 2>/dev/null || echo 0) files)"
