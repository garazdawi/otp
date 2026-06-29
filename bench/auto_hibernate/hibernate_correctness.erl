%% Correctness tests for idea #1 (automatic / compressed hibernation).
%%
%% Run on a normal build:
%%   $ERL_TOP/bin/erl -noshell -pa bench/auto_hibernate \
%%       -eval 'hibernate_correctness:go(), halt().'
%%
%% Or, for the strongest check, on a debug build (this exercises
%% ErtsGcQuickSanityCheck and all assertions after every decompress):
%%   $ERL_TOP/bin/cerl -debug -noshell -pa bench/auto_hibernate \
%%       -eval 'hibernate_correctness:go(), halt().'
-module(hibernate_correctness).
-export([go/0, h3_resume/1]).

go() ->
    ok = t_hibernate3(),
    ok = t_gc(),
    ok = t_pinfo_normal(),
    ok = t_dict_no_wake(),
    ok = t_stress(),
    ok = t_links_monitors(),
    io:format("ALL_CORRECTNESS_PASSED~n").

%% original hibernate/3 must still work
t_hibernate3() ->
    P = spawn(fun() -> receive go -> erlang:hibernate(?MODULE, h3_resume, [self()]) end end),
    P ! go, timer:sleep(50),
    P ! {ping, self()},
    receive {h3, ok} -> io:format("hibernate/3 ok~n"), ok after 2000 -> error(h3_timeout) end.
h3_resume(_Orig) ->
    receive {ping, From} -> From ! {h3, ok} end.

%% garbage_collect/1,2 of another process must still work
t_gc() ->
    P = spawn(fun() -> _ = lists:seq(1,1000), receive M -> M end end),
    true = erlang:garbage_collect(P),
    true = erlang:garbage_collect(P, []),
    exit(P, kill),
    io:format("garbage_collect/1,2 ok~n"), ok.

%% process_info on a NORMAL (uncompressed) process is unchanged
t_pinfo_normal() ->
    P = spawn(fun() -> put(a,1), put(b,[x,y,z]), receive _ -> ok end end),
    timer:sleep(20),
    {dictionary, D} = erlang:process_info(P, dictionary),
    true = lists:sort(D) =:= [{a,1},{b,[x,y,z]}],
    {memory, M} = erlang:process_info(P, memory), true = M > 0,
    L = erlang:process_info(P, [dictionary, memory, heap_size, current_function]),
    true = is_list(L),
    {{dictionary,a}, 1} = erlang:process_info(P, {dictionary, a}),
    exit(P, kill),
    io:format("process_info (normal process) ok~n"), ok.

%% process_info(Pid, dictionary) on a compressed process must return the right
%% data without decompressing it (verified by global process memory staying low)
t_dict_no_wake() ->
    Self = self(),
    Pairs = [{spawn(fun() -> put(k, lists:seq(1, V)),
                             S = {lists:seq(1, 1000 + V), make_ref()},
                             Self ! ready, dloop(S) end), V}
             || V <- lists:seq(1, 1000)],
    [receive ready -> ok end || _ <- Pairs],
    [erlang:hibernate(P, [compressed]) || {P, _} <- Pairs],
    timer:sleep(50),
    Before = erlang:memory(processes),
    %% Verify each dictionary inline (do not accumulate them, so the reader's
    %% own heap does not grow and confound the measurement).
    lists:foreach(
      fun({P, V}) ->
              case erlang:process_info(P, dictionary) of
                  {dictionary, [{k, Seq}]} when Seq =:= [] orelse hd(Seq) =:= 1 ->
                      case Seq =:= lists:seq(1, V) of
                          true -> ok;
                          false -> error({dict_mismatch, V})
                      end;
                  Other -> error({dict_unexpected, V, Other})
              end
      end, Pairs),
    erlang:garbage_collect(),   %% drop the reader's transient dict copies
    After = erlang:memory(processes),
    %% If reading had decompressed any process its heap would have re-expanded;
    %% the compressed processes must stay compressed (allow a little slack).
    true = (After - Before) < 2000000,
    [exit(P, kill) || {P, _} <- Pairs],
    io:format("process_info(dictionary) without decompress ok "
              "(processes mem ~p -> ~p)~n", [Before, After]), ok.
dloop(S) -> receive _ -> dloop(S) end.

%% many compress/wake rounds with end-to-end data verification
t_stress() ->
    N = 1000,
    Self = self(),
    Pids = [spawn(fun() -> S = {lists:seq(1,V), V, make_ref(), <<V:64>>},
                          Self ! ready, sloop(S) end)
            || V <- lists:seq(1, N)],
    [receive ready -> ok end || _ <- Pids],
    Expect = lists:sort([lists:sum(lists:seq(1,V)) || V <- lists:seq(1,N)]),
    [begin
         [erlang:hibernate(P, [compressed]) || P <- Pids],
         timer:sleep(20),
         [P ! {sum, self()} || P <- Pids],
         Sums = [receive {sum, S} -> S after 3000 -> timeout end || _ <- Pids],
         case lists:sort(Sums) =:= Expect of
             true -> ok;
             false -> error(stress_mismatch)
         end
     end || _ <- [1,2,3]],
    [exit(P, kill) || P <- Pids],
    io:format("stress 3x1000 compress/wake with verification ok~n"), ok.
sloop(S) ->
    receive {sum, From} -> From ! {sum, lists:sum(element(1,S))}, sloop(S);
            _ -> sloop(S) end.

%% compressed processes with monitors; exit/kill must propagate cleanly
t_links_monitors() ->
    Self = self(),
    Ps = [spawn(fun() -> Self ! ready, receive _ -> ok end end) || _ <- lists:seq(1,200)],
    [receive ready -> ok end || _ <- Ps],
    Refs = [erlang:monitor(process, P) || P <- Ps],
    [erlang:hibernate(P, [compressed]) || P <- Ps],
    timer:sleep(50),
    [exit(P, kill) || P <- Ps],
    Downs = [receive {'DOWN', R, process, _, killed} -> ok after 3000 -> timeout end || R <- Refs],
    true = lists:all(fun(X) -> X =:= ok end, Downs),
    io:format("monitors + exit-while-compressed ok~n"), ok.
