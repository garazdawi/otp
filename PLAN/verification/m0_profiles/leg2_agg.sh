export ELIXIR_ERL_OPTIONS="+JPperf map +fnu"
PERF=/usr/bin/perf
mkdir -p /work; cd /app
rm -rf _build/dev/lib/phoenix _build/dev/lib/ecto _build/dev/lib/ecto_sql _build/dev/lib/absinthe \
  _build/dev/lib/phoenix_live_view _build/dev/lib/plug _build/dev/lib/jason _build/dev/lib/gettext \
  _build/dev/lib/phoenix_html _build/dev/lib/phoenix_pubsub _build/dev/lib/decimal _build/dev/lib/nimble_parsec \
  _build/dev/lib/phoenix_template _build/dev/lib/mime _build/dev/lib/plug_crypto _build/dev/lib/expo \
  _build/dev/lib/db_connection _build/dev/lib/telemetry _build/dev/lib/castore _build/dev/lib/hpax \
  _build/dev/lib/websock _build/dev/lib/plug_cowboy 2>/dev/null
mix deps.compile >/work/compile.log 2>&1 &
MP=$!
BEAM=""; for i in $(seq 1 50); do BEAM=$(pgrep -x beam.smp | head -1); [ -n "$BEAM" ] && break; sleep 0.1; done
$PERF record -F 499 -e cpu-clock -p "$BEAM" -o /work/mix.perf.data >/work/perf.log 2>&1 &
PERFPID=$!
wait $MP; DRC=$?
sleep 0.2; kill -INT $PERFPID 2>/dev/null; wait $PERFPID 2>/dev/null
echo "=== compile rc=$DRC ; $($PERF report -i /work/mix.perf.data --stdio 2>/dev/null | grep -iE '# samples') ==="
# Full report, sorted, aggregated by family (% of ALL samples)
$PERF report -i /work/mix.perf.data --stdio --sort dso,symbol 2>/dev/null | grep -vE '^#|^$' > /work/full.txt
echo "@@@DSO@@@"; $PERF report -i /work/mix.perf.data --stdio --sort dso 2>/dev/null | grep -vE '^#|^$' | head -10
echo "@@@FAMILY@@@"
awk '
{
  pct=$1; gsub(/%/,"",pct); pct=pct+0;
  line=$0;
  # find dso and symbol
  isjit = (index(line,"[JIT]")>0);
  isbeam = (line ~ /beam\.smp/);
  # symbol after [.] or [k]
  sym="";
  for(i=1;i<=NF;i++){ if($i=="[.]"||$i=="[k]"){sym=$(i+1);break} }
  if(isjit){
    jit_total+=pct;
    if(sym ~ /^\$Elixir\.Enum:/ || sym ~ /^\$Elixir\.Enum\./) f["Enum"]+=pct;
    else if(sym ~ /^\$Elixir\.Stream:/) f["Stream"]+=pct;
    else if(sym ~ /^\$Elixir\.(Enumerable|Collectable|String\.Chars|List\.Chars|Inspect|Protocol)/) f["Protocol/impl"]+=pct;
    else if(sym ~ /^\$lists:/ || sym ~ /^\$lists::/) f["lists(erl)"]+=pct;
    else if(sym ~ /^\$maps:/ || sym ~ /^\$maps::/) f["maps(erl)"]+=pct;
    else if(sym ~ /^\$(sets|ordsets|gb_sets|gb_trees|dict|orddict):/) f["erl-sets/dicts"]+=pct;
    else if(sym ~ /^\$(beam_|v3_core|core_|sys_core|cerl|erl_lint|erl_expand|erl_bits|compile|beam_asm|beam_val)/) f["erl-compiler-backend"]+=pct;
    else if(sym ~ /^\$elixir/) f["elixir_* (frontend,erl)"]+=pct;
    else if(sym ~ /^\$Elixir\.(Kernel|Macro|Module|Code|Keyword|Map|MapSet|String|Access)/) f["Elixir.Kernel/Macro/Module/..."]+=pct;
    else if(sym ~ /^\$global::/) f["JIT runtime glue (\$global::)"]+=pct;
    else f["other-JIT"]+=pct;
  } else if(isbeam){
    beam_total+=pct;
    if(sym ~ /do_minor|sweep_new_heap|full_sweep|garbage_collect|offset_heap|do_major|sweep_off|collect_live|remove_message_buffers|setup_rootset|any_heap_refs/) c["GC"]+=pct;
    else if(sym ~ /^eq$|erts_cmp_compound|cmp_/) c["term eq/compare"]+=pct;
    else if(sym ~ /maps_|hashmap|get_map_element|make_map|flatmap/) c["map ops (C)"]+=pct;
    else if(sym ~ /make_internal_hash|hxnodecmp|db_hash/) c["hashing (C)"]+=pct;
    else if(sym ~ /copy_struct|copy_shallow|size_object/) c["term copy (C)"]+=pct;
    else if(sym ~ /erts_ptab|erts_schedule|scheduler_wait|process_main/) c["sched/proc-table (C)"]+=pct;
    else if(sym ~ /qsort/) c["sort (C)"]+=pct;
    else if(sym ~ /call_bif|light_bif|call_light/) c["bif dispatch (C)"]+=pct;
    else c["other-C"]+=pct;
  }
}
END{
  printf "-- JIT families (%% of ALL samples; JIT bucket total=%.1f) --\n", jit_total;
  for(k in f) printf "  %-34s %5.2f%%\n", k, f[k];
  printf "-- beam.smp C categories (bucket total=%.1f) --\n", beam_total;
  for(k in c) printf "  %-34s %5.2f%%\n", k, c[k];
}' /work/full.txt | sort -t% -k2 -rn
echo "@@@TOPJIT@@@"; grep '\[JIT\]' /work/full.txt | sed -E 's/^ *([0-9.]+%).*\[.\] (.*[^ ]) +- +- *$/\1 \2/' | head -50
