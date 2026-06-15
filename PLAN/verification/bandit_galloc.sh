set -e
export DEBIAN_FRONTEND=noninteractive
apt-get update -qq >/dev/null 2>&1
apt-get install -y -qq curl unzip ca-certificates libssl3 libncurses6 >/dev/null 2>&1
export ERL_TOP=/otp PATH=/otp/bin:/elixir/bin:$PATH
echo "built OTP: $(/otp/bin/erl -noshell -eval 'io:format("~s",[erlang:system_info(otp_release)]),halt().')"
curl -sL -o /ex.zip https://github.com/elixir-lang/elixir/releases/download/v1.19.5/elixir-otp-28.zip
mkdir -p /elixir && unzip -qo /ex.zip -d /elixir
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
/otp/bin/erlc -o /tmp /tmp/http_pipe.erl 2>/dev/null
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
:erts_debug.set_internal_state(:available_internal_state, true)
:timer.sleep(8000)                                              # warm under load
:erts_debug.set_internal_state(:alloc_profile_all, true)        # GLOBAL on
:erts_debug.set_internal_state(:alloc_profile_sites, true)      # reset
:timer.sleep(12000)                                             # measure window
:erts_debug.set_internal_state(:alloc_profile_all, false)
sites = :erts_debug.get_internal_state(:alloc_profile_sites)
total = Enum.reduce(sites, 0, fn {_, w}, a -> a + w end)
top = sites |> Enum.sort_by(fn {_, w} -> -w end) |> Enum.take(25)
IO.puts("GALLOC_TOTAL #{total} words (#{Float.round(total*8/1.0e6,1)} MB)")
for {mfa, w} <- top, do: IO.puts("GALLOC #{Float.round(100*w/max(1,total),2)}% #{inspect(mfa)} #{w}")
System.halt(0)
EXS
ERL_ALLOC_PROFILE=1 elixir /server.exs > /srv.out 2>&1 &
for i in $(seq 1 120); do grep -q "server up" /srv.out 2>/dev/null && break; sleep 2; done
grep -q "server up" /srv.out || { echo "SERVER FAILED"; tail -20 /srv.out; exit 42; }
ERL_ALLOC_PROFILE=1 /otp/bin/erl -noshell -pa /tmp -eval '(catch http_pipe:run(8, 28000, x)), halt().' > /load.out 2>&1 &
for i in $(seq 1 60); do grep -q "GALLOC_TOTAL" /srv.out 2>/dev/null && break; sleep 1; done
echo "=== load ==="; grep -oE "responses: .*" /load.out | head -1
echo "=== allocation worklist (Bandit JSON, built instrumented OTP) ==="
grep -E "GALLOC" /srv.out
