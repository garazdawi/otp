#!/bin/bash
# Benchmark map IC hit rates using ex_doc compilation
#
# Usage: ./benchmark_ic.sh
#
# Requires: Elixir in PATH, OTP built with FLAVOR=emu

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
OTP_BIN="$SCRIPT_DIR/bin"
BENCH_DIR="$SCRIPT_DIR/benchmark_ex_doc"

# Prepend our OTP bin, keep rest of PATH intact
export PATH="$OTP_BIN:$PATH"
export ERL_FLAGS="+mic true -emu_flavor emu"

echo "=== Map IC Benchmark: ex_doc compile ==="
echo "OTP: $(erl -noshell -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().')"
echo "Elixir: $(elixir --version | tail -1)"
echo "ERL_FLAGS: $ERL_FLAGS"
echo ""

cd "$BENCH_DIR"

# Clean previous compilation
rm -rf _build/dev/lib/ex_doc

echo "--- Compiling ex_doc (clean build) ---"

mix run --no-start -e '
  Mix.Task.run("compile", ["--force"])

  IO.puts("\n=== Global IC Counters ===")
  counters = :erlang.system_info(:map_ic_counters)
  for {k, v} <- counters do
    IO.puts("  #{String.pad_trailing(to_string(k), 12)} #{v}")
  end
  attempts = counters[:attempts]
  hits = counters[:hits]
  hit_rate = if attempts > 0, do: Float.round(hits / attempts * 100, 1), else: 0.0
  IO.puts("  hit_rate:     #{hit_rate}%")
  IO.puts("")

  entries = :map_ic.info()
  IO.puts("Total IC entries: #{length(entries)}")

  active = Enum.count(entries, fn {_, _, state, _, _, _, _} -> state == :active end)
  disabled = Enum.count(entries, fn {_, _, state, _, _, _, _} -> state == :disabled end)
  IO.puts("  Active:   #{active}")
  IO.puts("  Disabled: #{disabled}")
  IO.puts("")

  summary = :map_ic.summary(entries)
  top = Enum.take(summary, 40)

  IO.puts("=== Top 40 IC Sites (by hits) ===")
  header = String.pad_trailing("Site", 55) <>
           String.pad_trailing("Key", 16) <>
           String.pad_trailing("State", 10) <>
           String.pad_leading("Hits", 10) <>
           String.pad_leading("Misses", 10) <>
           String.pad_leading("Arity", 7) <>
           String.pad_leading("Scheds", 7)
  IO.puts(header)
  IO.puts(String.duplicate("-", String.length(header)))

  for entry <- top do
    site = case entry.site do
      {m, f, a} -> "#{m}:#{f}/#{a}"
      other -> inspect(other)
    end
    IO.puts(
      String.pad_trailing(String.slice(site, 0, 54), 55) <>
      String.pad_trailing(String.slice(inspect(entry.key), 0, 15), 16) <>
      String.pad_trailing(to_string(entry.state), 10) <>
      String.pad_leading(to_string(entry.hits), 10) <>
      String.pad_leading(to_string(entry.misses), 10) <>
      String.pad_leading(to_string(entry.shape_arity), 7) <>
      String.pad_leading(to_string(entry.schedulers), 7)
    )
  end
'
