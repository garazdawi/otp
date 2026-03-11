#!/bin/bash
# Benchmark map IC hit rates using stdlib doc generation with ex_doc
#
# Usage: ./benchmark_ic_stdlib_docs.sh

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
OTP_BIN="$SCRIPT_DIR/bin"
BENCH_DIR="$SCRIPT_DIR/benchmark_ex_doc"
STDLIB_EBIN="$SCRIPT_DIR/lib/stdlib/ebin"
STDLIB_DOC_DIR="$SCRIPT_DIR/lib/stdlib/doc"

export PATH="$OTP_BIN:/usr/bin:/bin:/usr/sbin:/sbin:$PATH"
export ERL_FLAGS="+mic true -emu_flavor emu"

echo "=== Map IC Benchmark: stdlib doc generation ==="
echo "OTP: $(erl -noshell -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().')"
echo "Elixir: $(elixir --version | tail -1)"
echo "ERL_FLAGS: $ERL_FLAGS"
echo ""

cd "$BENCH_DIR"

# Clean previous output
rm -rf /tmp/stdlib_docs_out

echo "--- Generating stdlib docs ---"

mix run -e '
  stdlib_ebin = System.get_env("STDLIB_EBIN", "'"$STDLIB_EBIN"'")
  stdlib_doc_dir = System.get_env("STDLIB_DOC_DIR", "'"$STDLIB_DOC_DIR"'")

  # Read docs.exs config
  config_file = Path.join(stdlib_doc_dir, "docs.exs")
  config = if File.exists?(config_file) do
    {opts, _} = Code.eval_file(config_file)
    opts
  else
    []
  end

  # Get extras with full paths
  extras = Enum.map(config[:extras] || [], fn extra ->
    Path.join(stdlib_doc_dir, extra)
  end) |> Enum.filter(&File.exists?/1)

  groups_for_modules = config[:groups_for_modules] || []

  IO.puts("  ebin: #{stdlib_ebin}")
  IO.puts("  extras: #{length(extras)} files")
  IO.puts("  module groups: #{length(groups_for_modules)}")
  IO.puts("")

  ExDoc.generate(
    "stdlib",
    "6.2",
    [stdlib_ebin],
    [
      output: "/tmp/stdlib_docs_out",
      extras: extras,
      groups_for_modules: groups_for_modules,
      formatters: ["html"],
      main: "stdlib_app",
      proglang: :erlang
    ]
  )

  IO.puts("\n=== Global IC Counters ===")
  counters = :erlang.system_info(:map_ic_counters)
  for {k, v} <- counters do
    IO.puts("  #{String.pad_trailing(to_string(k), 24)} #{v}")
  end
  attempts = counters[:attempts]
  hits = counters[:hits]
  hit_rate = if attempts > 0, do: Float.round(hits / attempts * 100, 1), else: 0.0
  IO.puts("  hit_rate:                 #{hit_rate}%")
  IO.puts("")

  entries = :map_ic.info()
  IO.puts("Total IC entries: #{length(entries)}")

  active = Enum.count(entries, fn {_, _, state, _, _, _, _} -> state == :active end)
  disabled = Enum.count(entries, fn {_, _, state, _, _, _, _} -> state == :disabled end)
  not_found = Enum.count(entries, fn {_, _, state, _, _, _, _} -> state == :not_found end)
  IO.puts("  Active:    #{active}")
  IO.puts("  Disabled:  #{disabled}")
  IO.puts("  Not found: #{not_found}")
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
