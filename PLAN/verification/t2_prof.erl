%% t2_prof — low-distortion whole-node profiler for the P0
%% "where do real applications spend CPU" measurement
%% (PLAN/T2/08 §8 P0; see PROFILE_RESULTS.md).
%%
%% Two complementary layers, both cheap enough not to perturb:
%%
%%   1. msacc (base states): exact cycle split between emulator code,
%%      GC, port work, aux, check_io, sleep — per scheduler type.
%%      Zero distortion; answers "how much CPU is Erlang code at all".
%%   2. Statistical sampling of running processes' current
%%      function/stack: every IntMs, snapshot which MFAs the running
%%      (status =:= running) processes are executing. Attributes the
%%      emulator share by function with no per-call instrumentation
%%      (unlike cprof/eprof/tprof, it cannot over-weight tiny bodies).
%%
%% Works on OTP 27+ with no dependencies; designed to be loaded into
%% foreign nodes (rabbitmqctl eval, Elixir :code.load_binary, remsh).
%%
%%   t2_prof:run(DurMs, IntMs)              -> prints + returns report
%%   t2_prof:run_to_file(DurMs, IntMs, File)-> writes report term
-module(t2_prof).
-export([run/2, run_to_file/3]).

run(DurMs, IntMs) ->
    Report = collect(DurMs, IntMs),
    print(Report),
    Report.

run_to_file(DurMs, IntMs, File) ->
    Report = collect(DurMs, IntMs),
    ok = file:write_file(File, io_lib:format("~p.~n", [Report])),
    print(Report),
    ok.

collect(DurMs, IntMs) ->
    _ = erlang:system_flag(microstate_accounting, true),
    erlang:system_flag(microstate_accounting, reset),
    Samples = sample_loop(erlang:monotonic_time(millisecond) + DurMs,
                          IntMs, #{}, 0),
    Msacc = erlang:statistics(microstate_accounting),
    _ = erlang:system_flag(microstate_accounting, false),
    {Funs, Stacks, Ticks} = Samples,
    #{msacc => summarize_msacc(Msacc),
      fun_samples => lists:reverse(lists:keysort(2, maps:to_list(Funs))),
      stack_samples => lists:reverse(lists:keysort(2, maps:to_list(Stacks))),
      ticks => Ticks}.

sample_loop(Deadline, IntMs, {Funs, Stacks}, Ticks) ->
    sample_loop(Deadline, IntMs, Funs, Stacks, Ticks);
sample_loop(Deadline, IntMs, Funs, Ticks) ->
    sample_loop(Deadline, IntMs, Funs, #{}, Ticks).

sample_loop(Deadline, IntMs, Funs0, Stacks0, Ticks) ->
    case erlang:monotonic_time(millisecond) >= Deadline of
        true ->
            {Funs0, Stacks0, Ticks};
        false ->
            Self = self(),
            {Funs, Stacks} =
                lists:foldl(
                  fun(P, Acc) when P =:= Self -> Acc;
                     (P, {FA, SA} = Acc) ->
                          case erlang:process_info(
                                 P, [status, current_function,
                                     current_stacktrace]) of
                              [{status, running},
                               {current_function, MFA},
                               {current_stacktrace, ST}] ->
                                  Key = mfa_key(MFA),
                                  SKey = stack_key(MFA, ST),
                                  {maps:update_with(
                                     Key, fun(N) -> N + 1 end, 1, FA),
                                   maps:update_with(
                                     SKey, fun(N) -> N + 1 end, 1, SA)};
                              _ ->
                                  Acc
                          end
                  end, {Funs0, Stacks0}, erlang:processes()),
            timer:sleep(IntMs),
            sample_loop(Deadline, IntMs, Funs, Stacks, Ticks + 1)
    end.

mfa_key({M, F, A}) -> {M, F, A};
mfa_key(Other) -> Other.

%% current_function plus up to two caller frames — enough context to
%% attribute generic helpers (lists, gen_server plumbing) to their
%% real owner without deep-stack cost.
stack_key(MFA, ST) ->
    Frames = [mfa_key({M, F, A}) || {M, F, A, _} <- lists:sublist(ST, 3)],
    case Frames of
        [] -> [mfa_key(MFA)];
        _ -> Frames
    end.

summarize_msacc(Msacc) ->
    %% Sum counters per thread class; report per-state share of the
    %% class's total and of its non-sleep (busy) total.
    PerClass =
        lists:foldl(
          fun(#{type := T, counters := C}, Acc) ->
                  maps:update_with(
                    T,
                    fun(Old) ->
                            maps:merge_with(
                              fun(_, A, B) -> A + B end, Old, C)
                    end,
                    C, Acc)
          end, #{}, Msacc),
    maps:map(
      fun(_T, C) ->
              Total = maps:fold(fun(_, V, S) -> S + V end, 0, C),
              Busy = Total - maps:get(sleep, C, 0),
              #{shares =>
                    [{K, pct(V, Total), pct(V, Busy)}
                     || {K, V} <- lists:reverse(
                                    lists:keysort(2, maps:to_list(C)))],
                busy_pct => pct(Busy, Total)}
      end, PerClass).

pct(_, 0) -> 0.0;
pct(V, T) -> round(10000 * V / T) / 100.

print(#{msacc := Msacc, fun_samples := Funs, ticks := Ticks} = R) ->
    io:format("~n=== msacc (state, % of total, % of busy) ===~n", []),
    maps:foreach(
      fun(Class, #{shares := Sh, busy_pct := B}) ->
              io:format("~p (busy ~.1f%):~n", [Class, B * 1.0]),
              [io:format("    ~-10s ~7.2f ~7.2f~n",
                         [atom_to_list(K), P1 * 1.0, P2 * 1.0])
               || {K, P1, P2} <- Sh, P1 > 0.05]
      end, Msacc),
    TotalS = lists:sum([N || {_, N} <- Funs]),
    io:format("~n=== running-process samples (~b ticks, ~b samples) ===~n",
              [Ticks, TotalS]),
    [io:format("  ~6.2f%  ~p~n", [100 * N / max(1, TotalS), MFA])
     || {MFA, N} <- lists:sublist(Funs, 40)],
    StackTotal = lists:sum([N || {_, N} <- maps:get(stack_samples, R)]),
    io:format("~n=== top stacks ===~n", []),
    [io:format("  ~6.2f%  ~p~n", [100 * N / max(1, StackTotal), St])
     || {St, N} <- lists:sublist(maps:get(stack_samples, R), 25)],
    ok.
