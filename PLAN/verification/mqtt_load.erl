%% Minimal MQTT 3.1.1 load generator for the RabbitMQ profiling leg
%% (no deps; QoS0 publish firehose + consuming subscribers).
%%
%%   mqtt_load:run(Host, Port, NPubs, NSubs, PayloadBytes, DurMs)
-module(mqtt_load).
-export([run/6]).

run(Host, Port, NPubs, NSubs, Bytes, DurMs) ->
    Parent = self(),
    Subs = [spawn_link(fun() -> sub(Host, Port, I, Parent) end)
            || I <- lists:seq(0, NSubs - 1)],
    receive_n(NSubs, sub_ready),
    Pubs = [spawn_link(fun() -> pub(Host, Port, I rem NSubs, Bytes, Parent) end)
            || I <- lists:seq(0, NPubs - 1)],
    timer:sleep(DurMs),
    [P ! stop || P <- Pubs],
    Sent = lists:sum(receive_n(NPubs, sent)),
    [S ! stop || S <- Subs],
    Rcvd = lists:sum(receive_n(NSubs, rcvd)),
    io:format("sent ~b msgs (~.1f k/s), subscriber bytes-in ~b~n",
              [Sent, Sent * 1000 / DurMs / 1000, Rcvd]),
    ok.

receive_n(N, Tag) ->
    [receive {Tag, V} -> V end || _ <- lists:seq(1, N)].

connect(Host, Port, ClientId) ->
    {ok, S} = gen_tcp:connect(Host, Port,
                              [binary, {active, false}, {nodelay, true},
                               {sndbuf, 1 bsl 20}, {recbuf, 1 bsl 20}]),
    Id = list_to_binary(ClientId),
    VarHdr = <<0, 4, "MQTT", 4, 2, 0, 60>>,
    Payload = <<(byte_size(Id)):16, Id/binary>>,
    ok = gen_tcp:send(S, packet(1, <<VarHdr/binary, Payload/binary>>)),
    {ok, <<32, 2, _, 0>>} = gen_tcp:recv(S, 4, 5000),
    S.

packet(Type, Body) ->
    [<<Type:4, 0:4>>, remlen(byte_size(Body)), Body].

packet(Type, Flags, Body) ->
    [<<Type:4, Flags:4>>, remlen(byte_size(Body)), Body].

remlen(N) when N < 128 -> <<N>>;
remlen(N) -> <<(N rem 128 + 128), (remlen(N div 128))/binary>>.

%% --- publisher: unthrottled QoS0 firehose in batches of 50 ---

pub(Host, Port, Topic, Bytes, Parent) ->
    S = connect(Host, Port, "pub" ++ integer_to_list(erlang:unique_integer([positive]))),
    T = list_to_binary("t/" ++ integer_to_list(Topic)),
    Body = <<(byte_size(T)):16, T/binary,
             (binary:copy(<<$x>>, Bytes))/binary>>,
    Pkt = iolist_to_binary(packet(3, 0, Body)),
    Batch = binary:copy(Pkt, 50),
    pub_loop(S, Batch, 0, Parent).

pub_loop(S, Batch, N, Parent) ->
    receive
        stop ->
            Parent ! {sent, N * 50},
            gen_tcp:close(S)
    after 0 ->
            ok = gen_tcp:send(S, Batch),
            pub_loop(S, Batch, N + 1, Parent)
    end.

%% --- subscriber: consume and discard ---

sub(Host, Port, I, Parent) ->
    S = connect(Host, Port, "sub" ++ integer_to_list(I)),
    T = list_to_binary("t/" ++ integer_to_list(I)),
    Body = <<1:16, (byte_size(T)):16, T/binary, 0>>,
    ok = gen_tcp:send(S, packet(8, 2, Body)),
    {ok, <<144, _/binary>>} = gen_tcp:recv(S, 0, 5000),
    Parent ! {sub_ready, ok},
    inet:setopts(S, [{active, true}]),
    sub_loop(S, 0, Parent).

sub_loop(S, Bytes, Parent) ->
    receive
        stop ->
            Parent ! {rcvd, Bytes},
            gen_tcp:close(S);
        {tcp, S, Data} ->
            sub_loop(S, Bytes + byte_size(Data), Parent);
        {tcp_closed, S} ->
            Parent ! {rcvd, Bytes}
    end.
