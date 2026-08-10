%% Replay a captured idle process population (see capture.erl) on the feature
%% VM and measure how much process memory hibernation (shrink) and compression
%% reclaim on the app's *real* data. Also reports the real ETS compression
%% savings captured in-node.
%%
%% Faithfulness note: real idle processes typically carry a large *allocated*
%% heap with little *live* data (heap slack left over from past work). We
%% reproduce that by rebuilding each process' captured live data and then
%% growing its heap to the captured allocated size without GC'ing it, so that
%% hibernate(shrink) has the same slack to reclaim as on the real node.
%%
%%   $ERL_TOP/bin/erl -noshell -pa bench/auto_hibernate \
%%       -eval 'app_replay:run("bench/auto_hibernate/cap_rabbitmq.bin", "RabbitMQ")' \
%%       -s init stop
-module(app_replay).
-compile(nowarn_deprecated_catch).
-export([run/2]).

run(File, Name) ->
    {ok, Bin} = file:read_file(File),
    {Data, EtsSave, Mem, PCount, WS} = binary_to_term(Bin),
    RealProcMem = proplists:get_value(processes, Mem),
    RealEts = proplists:get_value(ets, Mem),
    RealTotal = proplists:get_value(total, Mem),
    {EtsU, EtsC, EtsCnt, _EtsSk} = EtsSave,
    EtsUB = EtsU * WS, EtsCB = EtsC * WS,

    AllocWords = lists:sum([Hsz  || {Hsz, _T, _M, _P} <- Data]),
    LiveWords  = lists:sum([THsz || {_H, THsz, _M, _P} <- Data]),

    Base0 = erlang:memory(processes),
    Self = self(),
    Pids = [spawn(fun() -> replay_proc(Self, Hsz, Pl) end)
            || {Hsz, _THsz, _Mq, Pl} <- Data],
    [receive ready -> ok end || _ <- Pids],
    timer:sleep(300),
    Base1 = erlang:memory(processes),       %% reconstructed pop (with slack)

    [erlang:hibernate(P) || P <- Pids],
    timer:sleep(300),
    Shrink = erlang:memory(processes),

    [erlang:hibernate(P, [compressed]) || P <- Pids],
    timer:sleep(300),
    Comp = erlang:memory(processes),

    Pop  = Base1 - Base0,                   %% reconstructed population, allocated
    PopS = Shrink - Base0,                  %% after shrink
    PopC = Comp - Base0,                    %% after compress

    io:format("~n==================== ~s ====================~n", [Name]),
    io:format("REAL idle node (captured): ~p processes; processes ~s, ETS ~s, total ~s~n",
              [PCount, mb(RealProcMem), mb(RealEts), mb(RealTotal)]),
    io:format("  captured heap: allocated ~s vs live ~s  (slack ~s)~n",
              [mb(AllocWords * WS), mb(LiveWords * WS), mb((AllocWords - LiveWords) * WS)]),
    io:format("  ETS compression (real, in-node clones of ~p tables): "
              "~s -> ~s  (saved ~s, ~.1f%)~n",
              [EtsCnt, mb(EtsUB), mb(EtsCB), mb(EtsUB - EtsCB), pct(EtsUB - EtsCB, EtsUB)]),
    io:format("Reconstructed population on feature VM (~p procs, isolated delta):~n",
              [length(Data)]),
    io:format("  allocated (with slack) : ~s~n", [mb(Pop)]),
    io:format("  after hibernate(shrink): ~s  (saved ~s, ~.1f%)~n",
              [mb(PopS), mb(Pop - PopS), pct(Pop - PopS, Pop)]),
    io:format("  after compress         : ~s  (saved ~s, ~.1f%)~n",
              [mb(PopC), mb(Pop - PopC), pct(Pop - PopC, Pop)]),
    io:format("TOTAL reclaimable on this idle node (process compress + ETS):~n"),
    io:format("  processes ~s + ETS ~s = ~s  (~.1f% of ~s total)~n",
              [mb(Pop - PopC), mb(EtsUB - EtsCB), mb((Pop - PopC) + (EtsUB - EtsCB)),
               pct((Pop - PopC) + (EtsUB - EtsCB), RealTotal), mb(RealTotal)]),
    [exit(P, kill) || P <- Pids],
    ok.

replay_proc(Parent, Hsz, Payload) ->
    Keep =
        try
            {Dict, Msgs, State} = binary_to_term(Payload),
            [(catch put(K, V)) || {K, V} <- Dict],
            {State, Msgs}
        catch _:_ -> undefined
        end,
    grow_slack(Hsz),
    Parent ! ready,
    rloop(Keep).

%% Grow the allocated heap to ~Hsz words by building then discarding garbage,
%% reproducing the heap slack a real idle process carries. No GC afterwards.
%% Disabled when the SLACK environment variable is set to "0", to measure the
%% compression of the real *live* data only.
grow_slack(Hsz) when Hsz > 256 ->
    case os:getenv("SLACK") of
        "0" -> ok;
        _ -> _Garbage = lists:duplicate(Hsz - 200, $x), ok
    end;
grow_slack(_) ->
    ok.

rloop(Keep) ->
    receive _ -> rloop(Keep) end.

mb(Bytes) -> io_lib:format("~.2f MB", [Bytes / 1048576]).
pct(_, 0) -> 0.0;
pct(P, W) -> 100.0 * P / W.
