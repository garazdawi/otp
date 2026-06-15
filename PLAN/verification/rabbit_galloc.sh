set +e
export DEBIAN_FRONTEND=noninteractive
apt-get update -qq >/dev/null 2>&1
apt-get install -y -qq curl xz-utils ca-certificates procps libncurses6 libssl3 >/dev/null 2>&1
export ERLANG_HOME=/otp28/rel PATH=/otp28/rel/bin:/otp28/rel/erts-16.4.0.2/bin:$PATH ERL_ALLOC_PROFILE=1
/otp28/rel/erts-16.4.0.2/bin/epmd -daemon 2>/dev/null &
echo "built OTP: $(/otp28/rel/bin/erl -noshell -eval 'io:format("~s",[erlang:system_info(otp_release)]),halt().')"
curl -sL -o /rmq.tar.xz https://github.com/rabbitmq/rabbitmq-server/releases/download/v4.3.1/rabbitmq-server-generic-unix-4.3.1.tar.xz
tar xf /rmq.tar.xz -C /
R=/rabbitmq_server-4.3.1
$R/sbin/rabbitmq-plugins --offline enable rabbitmq_mqtt >/dev/null 2>&1
$R/sbin/rabbitmq-server > /broker.out 2>&1 &
ok=""
for i in $(seq 1 60); do $R/sbin/rabbitmqctl status >/dev/null 2>&1 && ok=1 && break; sleep 2; done
[ -n "$ok" ] || { echo "BROKER FAILED"; tail -25 /broker.out; exit 42; }
$R/sbin/rabbitmqctl status 2>/dev/null | grep -E "RabbitMQ version|Erlang"| head -2
cat > /tmp/mqtt_load.erl <<'MQL'
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
MQL
/otp28/rel/bin/erlc -o /tmp /tmp/mqtt_load.erl 2>/dev/null
# throttle publishers so subscriber queues stay bounded
sed -i 's/after 0 -> ok = gen_tcp:send(S, B), pub_loop/after 0 -> ok = gen_tcp:send(S, B), timer:sleep(3), pub_loop/' /tmp/mqtt_load.erl
/otp28/rel/bin/erlc -o /tmp /tmp/mqtt_load.erl 2>/dev/null
/otp28/rel/bin/erl -noshell -pa /tmp -eval '(catch mqtt_load:run("127.0.0.1",1883,6,6,256,45000)), halt().' > /load.out 2>&1 &
sleep 12
$R/sbin/rabbitmqctl --timeout 60 eval '
  erts_debug:set_internal_state(available_internal_state, true),
  erts_debug:set_internal_state(alloc_profile_all, true),
  erts_debug:set_internal_state(alloc_profile_sites, true),
  timer:sleep(12000),
  erts_debug:set_internal_state(alloc_profile_all, false),
  Sites = erts_debug:get_internal_state(alloc_profile_sites),
  Total = lists:sum([W || {_,W} <- Sites]),
  Top = lists:sublist(lists:reverse(lists:keysort(2, Sites)), 28),
  io:format("GALLOC_TOTAL ~p words (~.1f MB)~n", [Total, Total*8/1.0e6]),
  [io:format("GALLOC ~.2f% ~p ~p~n", [100*W/max(1,Total), MFA, W]) || {MFA,W} <- Top],
  ok.' 2>&1 | grep -E "GALLOC"
echo "=== load ==="; grep -oE "sent .*" /load.out | head -1
