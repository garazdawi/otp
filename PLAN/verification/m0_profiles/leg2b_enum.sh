export ELIXIR_ERL_OPTIONS="+JPperf map +fnu"
PERF=/usr/bin/perf
mkdir -p /work; cd /tmp
cat > enum_bench.exs <<'EX'
defprotocol Scorable do
  def score(t)
end
defmodule Item do
  defstruct [:id, :cat, :base, :mult]
end
defimpl Scorable, for: Item do
  def score(%Item{base: b, mult: m}), do: b * m + 3
end
defmodule Runner do
  def data do
    for i <- 1..30_000, do: %Item{id: i, cat: rem(i,16), base: rem(i*31,100), mult: rem(i,7)+1}
  end
  def work(data) do
    data
    |> Enum.filter(fn it -> Scorable.score(it) > 40 end)
    |> Enum.map(fn it -> {it.cat, Scorable.score(it)} end)
    |> Enum.group_by(fn {c,_} -> c end, fn {_,s} -> s end)
    |> Enum.flat_map(fn {c, ss} -> Enum.sort(ss) |> Enum.map(&{c,&1}) end)
    |> Stream.map(fn {c,s} -> c*1000+s end)
    |> Stream.filter(fn v -> rem(v,2)==0 end)
    |> Enum.reduce(%{}, fn v, acc -> Map.update(acc, rem(v,64), 1, &(&1+1)) end)
    |> Enum.reduce(0, fn {_,c}, a -> a+c end)
  end
  def loop(d,n,t0), do: (_=work(d); if System.monotonic_time(:millisecond)-t0<18000, do: loop(d,n+1,t0), else: n)
  def run, do: IO.puts("iters=#{loop(data(),0,System.monotonic_time(:millisecond))}")
end
Runner.run()
EX
elixir enum_bench.exs >/work/e.log 2>&1 &
MP=$!
BEAM=""; for i in $(seq 1 50); do BEAM=$(pgrep -x beam.smp | head -1); [ -n "$BEAM" ] && break; sleep 0.1; done
sleep 2
$PERF record -F 499 -e cpu-clock -p "$BEAM" -o /work/e.perf.data -- sleep 14 >/work/perf.log 2>&1
wait $MP 2>/dev/null
echo "=== $(cat /work/e.log) ; $($PERF report -i /work/e.perf.data --stdio 2>/dev/null | grep -iE '# samples') ==="
$PERF report -i /work/e.perf.data --stdio --sort dso,symbol 2>/dev/null | grep -vE '^#|^$' > /work/efull.txt
echo "@@@DSO@@@"; $PERF report -i /work/e.perf.data --stdio --sort dso 2>/dev/null | grep -vE '^#|^$' | head -8
echo "@@@FAMILY@@@"
awk '{pct=$1;gsub(/%/,"",pct);pct=pct+0;line=$0;isjit=(index(line,"[JIT]")>0);isbeam=(line~/beam\.smp/);sym="";for(i=1;i<=NF;i++){if($i=="[.]"||$i=="[k]"){sym=$(i+1);break}}
 gsub(/'"'"'/,"",sym);  # strip single quotes from symbol for matching
 if(isjit){jt+=pct;
   if(sym~/^\$Elixir\.Enum[:.]/)f["Enum.*"]+=pct;
   else if(sym~/^\$Elixir\.Stream/)f["Stream.*"]+=pct;
   else if(sym~/^\$Elixir\.Enumerable/)f["Enumerable protocol"]+=pct;
   else if(sym~/^\$Scorable/||sym~/^\$Elixir\.Scorable/)f["Scorable protocol (user)"]+=pct;
   else if(sym~/^\$Elixir\.Protocol/)f["Protocol.* (dispatch)"]+=pct;
   else if(sym~/^\$Elixir\.(String\.Chars|Inspect|Collectable|List\.Chars)/)f["other protocols"]+=pct;
   else if(sym~/^\$Elixir\.String/)f["String.*"]+=pct;
   else if(sym~/^\$Elixir\.(Map|Keyword|MapSet)/)f["Map/Keyword"]+=pct;
   else if(sym~/^\$Elixir\.Runner/&&sym~/-fun-/)f["user anon funs"]+=pct;
   else if(sym~/^\$lists[:.]/)f["lists:* (erl)"]+=pct;
   else if(sym~/^\$maps[:.]/)f["maps:* (erl)"]+=pct;
   else if(sym~/^\$global::/)f["JIT runtime glue"]+=pct;
   else f["other-JIT"]+=pct;
 } else if(isbeam){bt+=pct;
   if(sym~/do_minor|sweep_new|full_sweep|garbage_collect|offset_heap|any_heap_refs/)c["GC"]+=pct;
   else if(sym~/maps_|hashmap|get_map_element|make_map|erts_internal_map/)c["map ops (C)"]+=pct;
   else if(sym~/^eq$|erts_cmp_compound/)c["term eq/cmp (C)"]+=pct;
   else if(sym~/make_internal_hash|hxnode/)c["hashing (C)"]+=pct;
   else if(sym~/copy_struct|copy_shallow|size_object/)c["copy (C)"]+=pct;
   else if(sym~/call.*bif|light_bif|apply|dispatch|beam_jit|call_fun/)c["call/dispatch (C)"]+=pct;
   else c["other-C"]+=pct;}}
 END{printf "JIT bucket=%.1f\n",jt;for(k in f)printf "  %-30s %5.2f%%\n",k,f[k];printf "beam.smp C bucket=%.1f\n",bt;for(k in c)printf "  %-30s %5.2f%%\n",k,c[k];}' /work/efull.txt
echo "@@@TOPJIT@@@"; grep '\[JIT\]' /work/efull.txt | sed -E 's/^ *([0-9.]+%).*\[.\] (.*[^ ]) +- +- *$/\1 \2/' | head -30
echo "@@@TOPBEAM@@@"; grep 'beam.smp' /work/efull.txt | sed -E 's/^ *([0-9.]+%).*\[.\] (.*[^ ]) +- +- *$/\1 \2/' | head -20
