#!/bin/bash
# T2FULL M0.R -- run a Bandit JSON API on the local custom OTP build and
# drive it with the pipelined HTTP load generator (http_pipe.erl) while the
# server samples the receive-path classification counters.
set -e
cd "$(dirname "$0")/../.."
export ERL_TOP="$(pwd)"
export PATH="$ERL_TOP/bin:$PATH"
export ELIXIR_ERL_OPTIONS="+fnu"
export ASDF_ELIXIR_VERSION="${ASDF_ELIXIR_VERSION:-1.19.5}"
V=PLAN/verification
SRV=/tmp/bandit_recv_srv.out

echo "OTP: $(erl -noshell -eval 'io:format("~s",[erlang:system_info(otp_release)]),halt().')"
erlc -o "$V" "$V/http_pipe.erl"

rm -f "$SRV"
elixir "$V/bandit_recv.exs" > "$SRV" 2>&1 &
SRV_PID=$!
for i in $(seq 1 120); do grep -q "server up" "$SRV" 2>/dev/null && break; sleep 1; done
grep -q "server up" "$SRV" || { echo "SERVER FAILED"; tail -40 "$SRV"; exit 42; }
echo "server up; starting load"

# Drive for 15s so load spans the server's 4s warmup + 12s measured window
# and finishes while the server is still up (server outlives the driver).
erl -noshell -pa "$V" -eval '(catch http_pipe:run(32, 15000, x)), halt().' 2>&1 \
  | grep -oE "responses: .*" | head -1

for i in $(seq 1 60); do grep -q "RECV_STATS" "$SRV" 2>/dev/null && break; sleep 1; done
echo "=== server-side receive stats (Bandit connection handlers) ==="
grep -E "RECV_STATS" "$SRV" || { echo "NO STATS"; tail -20 "$SRV"; }
wait $SRV_PID 2>/dev/null || true
