set -e
apt-get update -qq >/dev/null 2>&1
apt-get install -y -qq --no-install-recommends procps curl xz-utils ca-certificates >/dev/null 2>&1
PERF=$(ls /usr/bin/perf_* 2>/dev/null | head -1); PERF=${PERF:-perf}
echo "container OTP: $(erl -noshell -eval 'io:format("~s",[erlang:system_info(otp_release)]),halt().')"
curl -sL -o /rmq.tar.xz https://github.com/rabbitmq/rabbitmq-server/releases/download/v4.3.1/rabbitmq-server-generic-unix-4.3.1.tar.xz
tar xf /rmq.tar.xz -C /
R=/rabbitmq_server-4.3.1
export RABBITMQ_SERVER_ADDITIONAL_ERL_ARGS="+MBatags code +MHatags code +MEatags code"
$R/sbin/rabbitmq-plugins --offline enable rabbitmq_mqtt >/dev/null 2>&1
$R/sbin/rabbitmq-server > /broker.out 2>&1 &
ok=""
for i in $(seq 1 45); do
  $R/sbin/rabbitmqctl status >/dev/null 2>&1 && ok=1 && break
  sleep 2
done
[ -n "$ok" ] || { echo "BROKER FAILED TO START"; tail -15 /broker.out; exit 42; }
$R/sbin/rabbitmqctl status 2>/dev/null | grep -E "RabbitMQ version|Erlang/OTP" | head -2
cat > /tmp/mqtt_load.erl <<'ERL'
-module(mqtt_load).
-export([run/6]).
run(Host, Port, NPubs, NSubs, Bytes, DurMs) ->
    Parent = self(),
    Subs = [spawn_link(fun() -> sub(Host, Port, I, Parent) end)
            || I <- lists:seq(0, NSubs - 1)],
    [receive {sub_ready, _} -> ok end || _ <- Subs],
    Pubs = [spawn_link(fun() -> pub(Host, Port, I rem NSubs, Bytes, Parent) end)
            || I <- lists:seq(0, NPubs - 1)],
    timer:sleep(DurMs),
    [P ! stop || P <- Pubs],
    Sent = lists:sum([receive {sent, C} -> C end || _ <- Pubs]),
    [S ! stop || S <- Subs],
    _ = [receive {rcvd, B} -> B end || _ <- Subs],
    io:format("sent ~b msgs (~.1f k/s)~n", [Sent, Sent * 1000 / DurMs / 1000]).
connect(Host, Port, ClientId) ->
    {ok, S} = gen_tcp:connect(Host, Port,
                              [binary, {active, false}, {nodelay, true},
                               {sndbuf, 1 bsl 20}, {recbuf, 1 bsl 20}]),
    Id = list_to_binary(ClientId),
    ok = gen_tcp:send(S, packet(1, <<0,4,"MQTT",4,2,0,60, (byte_size(Id)):16, Id/binary>>)),
    {ok, <<32,2,_,0>>} = gen_tcp:recv(S, 4, 5000),
    S.
packet(Type, Body) -> [<<Type:4, 0:4>>, remlen(byte_size(Body)), Body].
packet(Type, F, Body) -> [<<Type:4, F:4>>, remlen(byte_size(Body)), Body].
remlen(N) when N < 128 -> <<N>>;
remlen(N) -> <<(N rem 128 + 128), (remlen(N div 128))/binary>>.
pub(Host, Port, Topic, Bytes, Parent) ->
    S = connect(Host, Port, "p" ++ integer_to_list(erlang:unique_integer([positive]))),
    T = list_to_binary("t/" ++ integer_to_list(Topic)),
    Pkt = iolist_to_binary(packet(3, 0, <<(byte_size(T)):16, T/binary,
                                          (binary:copy(<<$x>>, Bytes))/binary>>)),
    pub_loop(S, binary:copy(Pkt, 50), 0, Parent).
pub_loop(S, B, N, Parent) ->
    receive stop -> Parent ! {sent, N * 50}, gen_tcp:close(S)
    after 0 -> ok = gen_tcp:send(S, B), timer:sleep(3), pub_loop(S, B, N + 1, Parent)
    end.
sub(Host, Port, I, Parent) ->
    S = connect(Host, Port, "s" ++ integer_to_list(I)),
    T = list_to_binary("t/" ++ integer_to_list(I)),
    ok = gen_tcp:send(S, packet(8, 2, <<1:16, (byte_size(T)):16, T/binary, 0>>)),
    {ok, <<144, _/binary>>} = gen_tcp:recv(S, 0, 5000),
    Parent ! {sub_ready, ok},
    inet:setopts(S, [{active, true}]),
    sub_loop(S, 0, Parent).
sub_loop(S, B, Parent) ->
    receive
        stop -> Parent ! {rcvd, B}, gen_tcp:close(S);
        {tcp, S, D} -> sub_loop(S, B + byte_size(D), Parent);
        {tcp_closed, S} -> Parent ! {rcvd, B}
    end.
ERL
erlc -o /tmp /tmp/mqtt_load.erl
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
erlc -o /tmp /tmp/gc_sites.erl 2>/dev/null
erl -noshell -pa /tmp -eval '(catch mqtt_load:run("127.0.0.1",1883,6,6,256,40000)), halt().' > /load.out 2>&1 &
LOAD=$!
sleep 12
$R/sbin/rabbitmqctl --timeout 60 eval 'code:add_patha("/tmp"), code:ensure_loaded(gc_sites), gc_sites:run(9000).' 2>&1 | tail -40
wait $LOAD; cat /load.out
