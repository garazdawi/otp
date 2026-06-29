%% Benchmark for idea #1: how much RAM can be saved on an idle system when all
%% processes and ETS tables are compressed.
%%
%% Usage (from a build of this branch):
%%
%%   $ERL_TOP/bin/erl -noshell -pa bench/auto_hibernate \
%%       -eval 'hibernate_bench:run()' -s init stop
%%
%%   %% also add N synthetic idle gen_server-like workers (a realistic
%%   %% long-running system, which is what idea #1 targets):
%%   ... -eval 'hibernate_bench:run(#{workers => 5000})' ...
%%
%% The benchmark:
%%   1. starts (almost) every OTP application, for a realistic idle system,
%%   2. optionally spawns N idle worker processes holding typical state,
%%   3. measures the baseline memory (erlang:memory/0 and OS RSS),
%%   4. hibernates every process (shrink) and then compresses every process,
%%      measuring the realized process-memory savings of each step,
%%   5. measures the *potential* ETS savings by cloning each table with the
%%      `compressed` option (there is no in-place table conversion),
%%   6. prints a breakdown.
-module(hibernate_bench).
-compile(nowarn_deprecated_catch).

-export([run/0, run/1]).

-define(SETTLE_MS, 3000).

run() ->
    run(#{}).

run(Opts) ->
    Workers = maps:get(workers, Opts, 0),
    StateWords = maps:get(state_words, Opts, 1000),
    io:format("~n==== idle-system compression benchmark (idea #1) ====~n"),
    Started = start_all_apps(),
    io:format("Started ~p OTP applications.~n", [length(Started)]),
    WPids = spawn_workers(Workers, StateWords),
    case Workers of
        0 -> ok;
        _ -> io:format("Spawned ~p idle workers (~p-word state each).~n",
                       [Workers, StateWords])
    end,
    settle(),

    Base = snapshot(),
    print_snapshot("BASELINE (idle)", Base),

    %% ---- ETS: potential savings from compressing every table ----
    Ets = measure_ets_savings(),
    io:format("~n-- ETS tables --~n"),
    io:format("  tables measured : ~p (skipped ~p private/unreadable)~n",
              [maps:get(measured, Ets), maps:get(skipped, Ets)]),
    io:format("  uncompressed    : ~s~n", [mb(maps:get(uncompressed, Ets))]),
    io:format("  compressed      : ~s~n", [mb(maps:get(compressed, Ets))]),
    io:format("  potential saving: ~s (~.1f%)~n",
              [mb(maps:get(saving, Ets)),
               pct(maps:get(saving, Ets), maps:get(uncompressed, Ets))]),

    %% ---- Processes: shrink, then compress, measuring each ----
    ProcBase = maps:get(processes, Base),
    {NProc, NShrink} = hibernate_all([]),
    ProcShrink = quick_processes_mem(),
    {_, NComp} = hibernate_all([compressed]),
    ProcComp = quick_processes_mem(),

    io:format("~n-- Processes --~n"),
    io:format("  processes              : ~p~n", [NProc]),
    io:format("  baseline               : ~s~n", [mb(ProcBase)]),
    io:format("  after hibernate(shrink): ~s  (saved ~s, ~.1f%) [~p hibernated]~n",
              [mb(ProcShrink), mb(ProcBase - ProcShrink),
               pct(ProcBase - ProcShrink, ProcBase), NShrink]),
    io:format("  after compress         : ~s  (saved ~s, ~.1f%) [~p compressed]~n",
              [mb(ProcComp), mb(ProcBase - ProcComp),
               pct(ProcBase - ProcComp, ProcBase), NComp]),

    After = snapshot(),
    print_snapshot("AFTER compressing all processes", After),

    %% ---- Totals ----
    ProcSave = ProcBase - ProcComp,
    EtsSave = maps:get(saving, Ets),
    Total = ProcSave + EtsSave,
    io:format("~n==== TOTAL RAM that can be saved on this idle system ====~n"),
    io:format("  process compression (realized) : ~s~n", [mb(ProcSave)]),
    io:format("  ETS compression     (potential): ~s~n", [mb(EtsSave)]),
    io:format("  combined                       : ~s  (~.1f% of erlang:memory(total))~n",
              [mb(Total), pct(Total, maps:get(total, Base))]),
    io:format("  baseline erlang:memory(total)  : ~s (RSS ~s)~n~n",
              [mb(maps:get(total, Base)), mb(maps:get(rss, Base))]),

    [exit(P, kill) || P <- WPids],
    #{baseline => Base, after_compress => After, ets => Ets,
      proc_baseline => ProcBase, proc_shrink => ProcShrink, proc_compressed => ProcComp,
      process_saving => ProcSave, ets_saving => EtsSave, total_saving => Total}.

%%% --------------------------------------------------------------------------

settle() ->
    timer:sleep(?SETTLE_MS),
    _ = [(catch erlang:garbage_collect(P)) || P <- erlang:processes()],
    timer:sleep(500),
    ok.

start_all_apps() ->
    Apps = available_apps(),
    lists:foldl(
      fun(App, Acc) ->
              case catch application:ensure_all_started(App) of
                  {ok, These} -> lists:usort(These ++ Acc);
                  _ -> Acc
              end
      end, [], Apps).

available_apps() ->
    Pattern = filename:join([code:root_dir(), "lib", "*", "ebin", "*.app"]),
    Names = [list_to_atom(filename:basename(F, ".app"))
             || F <- filelib:wildcard(Pattern)],
    Skip = [hipe, dialyzer, erl_interface, jinterface, megaco, diameter,
            wx, debugger, observer, et, ssh, eldap],
    lists:usort(Names) -- Skip.

%%% --- synthetic idle workers ------------------------------------------------

spawn_workers(0, _) -> [];
spawn_workers(N, StateWords) ->
    Parent = self(),
    Pids = [spawn(fun() -> worker(Parent, StateWords) end) || _ <- lists:seq(1, N)],
    [receive {ready, P} -> ok end || P <- Pids],
    Pids.

worker(Parent, StateWords) ->
    %% typical gen_server-ish baggage: a dictionary and some live state
    put('$ancestors', [Parent]),
    put('$initial_call', {hibernate_bench, worker, 2}),
    State = {state, lists:seq(1, StateWords), make_ref(),
             list_to_tuple(lists:seq(1, 20))},
    Parent ! {ready, self()},
    worker_loop(State).

worker_loop(State) ->
    receive
        {ping, From} -> From ! {pong, self()}, worker_loop(State);
        stop -> ok;
        _ -> worker_loop(State)
    end.

%%% --- processes ------------------------------------------------------------

hibernate_all(Opts) ->
    Self = self(),
    Pids = [P || P <- erlang:processes(), P =/= Self],
    N = lists:foldl(
          fun(P, Acc) ->
                  case catch erlang:hibernate(P, Opts) of
                      true -> Acc + 1;
                      _ -> Acc
                  end
          end, 0, Pids),
    {length(Pids), N}.

%% Read processes memory with as little intervening activity as possible
%% (erlang:memory/1 does not touch individual processes, so it won't wake the
%% compressed ones).
quick_processes_mem() ->
    erlang:memory(processes).

%%% --- ETS ------------------------------------------------------------------

measure_ets_savings() ->
    lists:foldl(fun measure_one_table/2,
                #{measured => 0, skipped => 0,
                  uncompressed => 0, compressed => 0, saving => 0},
                ets:all()).

measure_one_table(T, Acc) ->
    try
        Info = ets:info(T),
        false = Info =:= undefined,
        Type = proplists:get_value(type, Info),
        KeyPos = proplists:get_value(keypos, Info),
        Prot = proplists:get_value(protection, Info),
        Compressed = proplists:get_value(compressed, Info),
        UncWords = proplists:get_value(memory, Info),
        case Prot =:= private orelse Compressed =:= true of
            true ->
                bump_skipped(Acc);
            false ->
                Objs = ets:tab2list(T),
                Clone = ets:new(bench_clone,
                                [Type, {keypos, KeyPos}, compressed, public]),
                try
                    ets:insert(Clone, Objs),
                    W = erlang:system_info(wordsize),
                    Unc = UncWords * W,
                    Comp = ets:info(Clone, memory) * W,
                    Acc#{measured := maps:get(measured, Acc) + 1,
                         uncompressed := maps:get(uncompressed, Acc) + Unc,
                         compressed := maps:get(compressed, Acc) + Comp,
                         saving := maps:get(saving, Acc) + max(0, Unc - Comp)}
                after
                    ets:delete(Clone)
                end
        end
    catch
        _:_ -> bump_skipped(Acc)
    end.

bump_skipped(Acc) ->
    Acc#{skipped := maps:get(skipped, Acc) + 1}.

%%% --- measurement ----------------------------------------------------------

snapshot() ->
    M = maps:from_list(erlang:memory()),
    #{total      => maps:get(total, M),
      processes  => maps:get(processes, M),
      ets        => maps:get(ets, M),
      binary     => maps:get(binary, M),
      atom       => maps:get(atom, M),
      code       => maps:get(code, M),
      system     => maps:get(system, M),
      procs      => erlang:system_info(process_count),
      tables     => length(ets:all()),
      rss        => rss_bytes()}.

print_snapshot(Title, S) ->
    io:format("~n-- ~s --~n", [Title]),
    io:format("  processes : ~s~n", [mb(maps:get(processes, S))]),
    io:format("  ets       : ~s~n", [mb(maps:get(ets, S))]),
    io:format("  binary    : ~s~n", [mb(maps:get(binary, S))]),
    io:format("  code      : ~s~n", [mb(maps:get(code, S))]),
    io:format("  atom      : ~s~n", [mb(maps:get(atom, S))]),
    io:format("  system    : ~s~n", [mb(maps:get(system, S))]),
    io:format("  total     : ~s~n", [mb(maps:get(total, S))]),
    io:format("  OS RSS    : ~s~n", [mb(maps:get(rss, S))]),
    io:format("  proc count: ~p   ets tables: ~p~n",
              [maps:get(procs, S), maps:get(tables, S)]).

rss_bytes() ->
    Pid = os:getpid(),
    Out = os:cmd("ps -o rss= -p " ++ Pid),
    case string:to_integer(string:trim(Out)) of
        {KiB, _} when is_integer(KiB) -> KiB * 1024;
        _ -> 0
    end.

mb(Bytes) ->
    io_lib:format("~.2f MB", [Bytes / 1048576]).

pct(_, 0) -> 0.0;
pct(Part, Whole) -> 100.0 * Part / Whole.
