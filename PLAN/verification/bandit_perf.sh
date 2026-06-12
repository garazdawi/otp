set -e
apt-get update -qq >/dev/null 2>&1
apt-get install -y -qq --no-install-recommends linux-perf procps curl unzip ca-certificates >/dev/null 2>&1
PERF=$(ls /usr/bin/perf_* 2>/dev/null | head -1); PERF=${PERF:-perf}
echo "container OTP: $(erl -noshell -eval 'io:format("~s",[erlang:system_info(otp_release)]),halt().')"
curl -sL -o /ex.zip https://github.com/elixir-lang/elixir/releases/download/v1.19.5/elixir-otp-28.zip
mkdir -p /elixir && unzip -qo /ex.zip -d /elixir
export PATH=/elixir/bin:$PATH
cat > /server.exs <<'EXS'
Mix.install([{:bandit, "~> 1.5"}, {:jason, "~> 1.4"}])
Logger.configure(level: :error)
defmodule Api do
  import Plug.Conn
  def init(o), do: o
  def call(conn, _) do
    {:ok, body, conn} = read_body(conn)
    %{"items" => items, "user" => user} = Jason.decode!(body)
    total = Enum.reduce(items, 0, fn %{"qty" => q, "price" => p}, acc -> acc + q * p end)
    names = Enum.map(items, fn %{"name" => n} -> String.upcase(n) end)
    resp = %{"user" => Map.put(user, "active", true), "total" => total,
             "names" => names, "count" => length(items)}
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Jason.encode!(resp))
  end
end
{:ok, _} = Application.ensure_all_started(:bandit)
{:ok, _} = Bandit.start_link(plug: Api, port: 4001, ip: {127, 0, 0, 1})
IO.puts("server up")
:timer.sleep(120_000)
EXS
cat > /tmp/http_pipe.erl <<'ERL'
-module(http_pipe).
-export([run/3]).
run(N, DurMs, _) ->
    Req = req(),
    Batch = iolist_to_binary(lists:duplicate(8, Req)),
    Parent = self(),
    Ws = [spawn_link(fun() -> w(Batch, 0, Parent) end) || _ <- lists:seq(1, N)],
    timer:sleep(DurMs),
    [W ! stop || W <- Ws],
    T = lists:sum([receive {done, C} -> C end || _ <- Ws]),
    io:format("responses: ~b (~.1f k/s)~n", [T, T * 1000 / DurMs / 1000]).
w(Batch, C, Parent) ->
    {ok, S} = gen_tcp:connect("127.0.0.1", 4001,
                              [binary, {active, false}, {nodelay, true}]),
    w(S, Batch, C, Parent).
w(S, Batch, C, Parent) ->
    receive stop -> Parent ! {done, C}, gen_tcp:close(S)
    after 0 ->
        ok = gen_tcp:send(S, Batch),
        N = drain(S, 8, 0),
        w(S, Batch, C + N, Parent)
    end.
drain(_S, 0, Acc) -> Acc;
drain(S, Left, Acc) ->
    {ok, D} = gen_tcp:recv(S, 0, 10000),
    M = length(binary:matches(D, <<"HTTP/1.1 200">>)),
    drain(S, Left - M, Acc + M).
req() ->
    Items = lists:join($,, [io_lib:format(
        "{\"name\":\"item~b\",\"qty\":~b,\"price\":~b,\"tags\":[\"a\",\"b\"]}",
        [I, I rem 7 + 1, I * 3]) || I <- lists:seq(1, 20)]),
    Body = iolist_to_binary(["{\"user\":{\"id\":42,\"name\":\"lukas\",\"role\":\"admin\"},",
                             "\"items\":[", Items, "]}"]),
    iolist_to_binary(["POST /api HTTP/1.1\r\nhost: localhost\r\n",
                      "content-type: application/json\r\ncontent-length: ",
                      integer_to_list(byte_size(Body)), "\r\n\r\n", Body]).
ERL
erlc -o /tmp /tmp/http_pipe.erl
ELIXIR_ERL_OPTIONS="+JPperf map" elixir /server.exs > /srv.out 2>&1 &
ok=""
for i in $(seq 1 90); do grep -q "server up" /srv.out 2>/dev/null && ok=1 && break; sleep 2; done
[ -n "$ok" ] || { echo "SERVER FAILED"; tail -20 /srv.out; exit 42; }
SPID=$(pgrep -f beam.smp | head -1)
echo "server beam pid $SPID; perf map: $(ls /tmp/perf-$SPID.map 2>/dev/null | wc -l)"
erl -noshell -pa /tmp -eval 'http_pipe:run(12, 45000, x), halt().' > /load.out 2>&1 &
LOAD=$!
sleep 12
$PERF record -o /perf.data -F 499 -e cpu-clock -p $SPID -- sleep 15 2>&1 | tail -1
wait $LOAD; cat /load.out
echo "=== by dso ==="
$PERF report -i /perf.data --stdio --sort dso 2>/dev/null | grep -v '^#' | grep -v '^$' | head -8
echo "=== by symbol (top 45) ==="
$PERF report -i /perf.data --stdio --sort symbol 2>/dev/null | grep -v '^#' | grep -v '^$' | head -45
