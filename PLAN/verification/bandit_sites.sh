set -e
apt-get update -qq >/dev/null 2>&1
apt-get install -y -qq --no-install-recommends procps curl unzip ca-certificates >/dev/null 2>&1
curl -sL -o /ex.zip https://github.com/elixir-lang/elixir/releases/download/v1.19.5/elixir-otp-28.zip
mkdir -p /elixir && unzip -qo /ex.zip -d /elixir
export PATH=/elixir/bin:$PATH
cat > /tmp/gc_sites.erl <<'GCS'
%% gc_sites — attribute live tagged allocations (binaries, heap, ETS) to
%% the Erlang code that created them, via +M<S>atags code + instrument.
%% Names the module:fun/arity sites producing the binary garbage that
%% drives the bin_vheap major-GC storm (PLAN/verification/GC_RESULTS.md).
-module(gc_sites).
-export([run/1]).

run(DurMs) ->
    %% Sample a few times under load and aggregate per {MFA,Type} bytes.
    N = max(1, DurMs div 1500),
    Acc = lists:foldl(fun(_, A) -> timer:sleep(1500), sample(A) end, #{}, lists:seq(1, N)),
    Ranked = lists:reverse(lists:keysort(2, [{K, B} || {K, B} <- maps:to_list(Acc)])),
    io:format("~n=== live tagged allocations by origin (avg bytes over ~b samples) ===~n", [N]),
    io:format("~-58s ~-10s ~s~n", ["origin (MFA / pid)", "type", "bytes"]),
    [io:format("~-58s ~-10s ~14b~n", [fmt(O), T, B div N])
     || {{O, T}, B} <- lists:sublist(Ranked, 30)],
    Ranked.

sample(Acc) ->
    case instrument:allocations(#{flags => [per_mfa]}) of
        {ok, {Start, _Unscanned, Map}} ->
            maps:fold(
              fun(Origin, ByType, A1) ->
                      maps:fold(
                        fun(Type, Hist, A2) ->
                                B = hist_bytes(Hist, Start),
                                maps:update_with({Origin, Type},
                                                 fun(X) -> X + B end, B, A2)
                        end, A1, ByType)
              end, Acc, Map);
        Err ->
            io:format("instrument error: ~p~n", [Err]), Acc
    end.

%% Histogram is a tuple of counts; bucket i upper bound ~ Start*2^i.
hist_bytes(Hist, Start) when is_tuple(Hist) ->
    lists:sum([element(I, Hist) * (Start bsl (I - 1))
               || I <- lists:seq(1, tuple_size(Hist))]);
hist_bytes(_, _) -> 0.

fmt({M, F, A}) -> lists:flatten(io_lib:format("~p:~p/~p", [M, F, A]));
fmt(Other) -> lists:flatten(io_lib:format("~p", [Other])).
GCS
cat > /tmp/http_pipe.erl <<'HPP'
-module(http_pipe).
-export([run/3]).
%% Raw HTTP/1.1 keepalive + pipelining (depth 8) load generator.
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
HPP
erlc -o /tmp /tmp/gc_sites.erl /tmp/http_pipe.erl 2>/dev/null
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
    conn |> put_resp_content_type("application/json") |> send_resp(200, Jason.encode!(resp))
  end
end
{:ok, _} = Application.ensure_all_started(:bandit)
{:ok, _} = Bandit.start_link(plug: Api, port: 4001, ip: {127, 0, 0, 1})
IO.puts("server up")
:timer.sleep(9000)
:code.add_patha(~c"/tmp")
_ = :code.ensure_loaded(:gc_sites)
:gc_sites.run(9000)
System.halt(0)
EXS
ELIXIR_ERL_OPTIONS="+MBatags code +MHatags code +MEatags code" elixir /server.exs > /srv.out 2>&1 &
for i in $(seq 1 120); do grep -q "server up" /srv.out 2>/dev/null && break; sleep 2; done
grep -q "server up" /srv.out || { echo "SERVER FAILED"; tail -15 /srv.out; exit 42; }
erl -noshell -pa /tmp -eval '(catch http_pipe:run(12, 35000, x)), halt().' > /load.out 2>&1 &
for i in $(seq 1 50); do grep -q "by origin" /srv.out 2>/dev/null && break; sleep 1; done
pkill -f http_pipe 2>/dev/null
echo "=== load ==="; grep -oE "responses: .*" /load.out 2>/dev/null | head -1
echo "=== gc_sites (Bandit) ==="; grep -vE "warning:|^$" /srv.out | sed -n '/by origin/,$p' | head -34
