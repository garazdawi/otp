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
