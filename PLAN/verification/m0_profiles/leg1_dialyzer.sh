apt-get update -qq >/dev/null 2>&1
apt-get install -y -qq linux-perf >/dev/null 2>&1
PERF=/usr/bin/perf
export ERL_FLAGS="+JPperf map"
mkdir -p /work
APPS="erts kernel stdlib compiler crypto syntax_tools runtime_tools tools ssl public_key asn1 inets mnesia xmerl eldap ftp tftp sasl os_mon"
echo "=== OTP $(erl -noshell -eval 'io:format("~s",[erlang:system_info(otp_release)]),halt().') ; apps: $APPS ==="
T0=$(date +%s)
dialyzer --build_plt --apps $APPS --output_plt /work/base.plt > /work/dia.log 2>&1 &
DPID=$!
BEAM=""
for i in $(seq 1 60); do BEAM=$(pgrep -x beam.smp | head -1); [ -n "$BEAM" ] && break; sleep 0.3; done
echo "BEAM pid=$BEAM (attach t=$(( $(date +%s) - T0 ))s)"
$PERF record -F 499 -e cpu-clock -p "$BEAM" -o /work/dia.perf.data >/work/perf.log 2>&1 &
PERFPID=$!
wait $DPID; DRC=$?
sleep 0.3
kill -INT $PERFPID 2>/dev/null
wait $PERFPID 2>/dev/null
T1=$(date +%s)
echo "=== dialyzer rc=$DRC  build wall=$(( T1 - T0 ))s ==="
tail -3 /work/dia.log
echo "@@@DSO@@@"
$PERF report -i /work/dia.perf.data --stdio --sort dso 2>/dev/null | grep -vE '^#|^$'
echo "@@@SYMBOLS_DSO@@@"
$PERF report -i /work/dia.perf.data --stdio --sort dso,symbol 2>/dev/null | grep -vE '^#|^$' | head -220
echo "@@@SAMPLECOUNT@@@"
$PERF report -i /work/dia.perf.data --stdio 2>/dev/null | grep -E 'Samples:' | head -1
