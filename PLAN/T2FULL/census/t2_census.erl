%% SPDX-License-Identifier: Apache-2.0
%% Copyright Ericsson AB 2026. All Rights Reserved.
%%
%% PLAN/T2FULL/19 §2 S0 — the addressable-share census driver (memo 17
%% §3). Profiles a workload's own-time (BEAM call_time tracing — own-time
%% is exactly memo 14's metric, captured programmatically), joins it with
%% the frontend census ({t2_census, Bin}) per {M,F,A}, and reports:
%%
%%   * per-class marginal coverage (own-time of functions whose SOLE
%%     blocker is class C) vs gross coverage (any function touching C) —
%%     the gap is the entanglement (17 §3);
%%   * union coverage (own-time of functions all of whose blockers lie in
%%     {call_fun, maps, bs_construction, bs_position}) — the memo-17 §5
%%     decision-rule number;
%%   * the region-compilation split (loop-shaped, union-blocked own-time
%%     with every blocker OUT of the hot loop vs some blocker IN it).
%%
%% Decision rule (17 §5): build the service frontend only if union
%% coverage >= 15% whole-workload on >= 1 canonical service.
-module(t2_census).
-export([run/3, run/4]).

-define(UNION, [call_fun, maps, bs_construction, bs_position]).

%% run(Name, Modules, WorkFun) — WorkFun/0 is the profiled workload; the
%% own-time denominator is the total call_time over Modules.
run(Name, Modules, WorkFun) ->
    run(Name, Modules, WorkFun, Modules).

%% MeasureMods = modules whose functions form the own-time universe
%% (may be broader than the censused Modules, e.g. the whole compiler).
run(Name, CensusMods, WorkFun, MeasureMods) ->
    _ = erts_debug:set_internal_state(available_internal_state, true),
    Own = profile(MeasureMods, WorkFun),               % #{{M,F,A} => Us}
    Census = census(CensusMods),                       % #{{M,F,A} => Info}
    Joined = join(Own, Census),
    report(Name, Joined),
    Joined.

%% ---- own-time profiling via call_time tracing -----------------------

profile(Mods, WorkFun) ->
    [catch erlang:trace_pattern({M, '_', '_'}, true, [call_time]) || M <- Mods],
    %% call_time counters only accrue for a process carrying the `call`
    %% trace flag; set_on_spawn extends it to any workers the work spawns.
    %% The trace patterns are call_time (counting), not message-generating,
    %% so no trace messages are produced.
    erlang:trace(self(), true, [call, set_on_spawn]),
    %% warm up + load, then zero the counters and run for real.
    _ = (catch WorkFun()),
    [catch erlang:trace_pattern({M, '_', '_'}, restart, [call_time]) || M <- Mods],
    _ = WorkFun(),
    Own = collect(Mods),
    erlang:trace(self(), false, [call, set_on_spawn]),
    [catch erlang:trace_pattern({M, '_', '_'}, false, [call_time]) || M <- Mods],
    Own.

collect(Mods) ->
    lists:foldl(
      fun(M, Acc) ->
              Fns = try M:module_info(functions) catch _:_ -> [] end,
              lists:foldl(
                fun({F, A}, Acc2) ->
                        Us = own_us(M, F, A),
                        case Us > 0 of
                            true -> Acc2#{{M, F, A} => Us};
                            false -> Acc2
                        end
                end, Acc, Fns)
      end, #{}, Mods).

own_us(M, F, A) ->
    case catch erlang:trace_info({M, F, A}, call_time) of
        {call_time, L} when is_list(L) ->
            lists:sum([S * 1000000 + Us || {_Pid, _Cnt, S, Us} <- L]);
        _ -> 0
    end.

%% ---- frontend census -------------------------------------------------

census(Mods) ->
    lists:foldl(
      fun(M, Acc) ->
              case code:get_object_code(M) of
                  {M, Bin, _} ->
                      case erts_debug:get_internal_state({t2_census, Bin}) of
                          L when is_list(L) ->
                              lists:foldl(
                                fun({Nm, Ar, _Sz, Elig, Loop, Cs}, Acc2) ->
                                        Acc2#{{M, Nm, Ar} =>
                                                  #{eligible => Elig,
                                                    loop => Loop,
                                                    classes => Cs}}
                                end, Acc, L);
                          _ -> Acc
                      end;
                  error -> Acc
              end
      end, #{}, Mods).

%% ---- join own-time with census --------------------------------------

join(Own, Census) ->
    maps:fold(
      fun(Key, Us, Acc) ->
              Info = maps:get(Key, Census, #{eligible => unknown,
                                             loop => false, classes => []}),
              Acc#{Key => Info#{own => Us}}
      end, #{}, Own).

%% ---- reporting -------------------------------------------------------

report(Name, J) ->
    Rows = maps:to_list(J),
    Total = sum_own(Rows),
    io:format("~n===== census: ~s =====~n", [Name]),
    io:format("total own-time: ~.1f ms across ~p profiled functions~n",
              [Total / 1000.0, length(Rows)]),
    EligOwn = sum_own([R || {_, #{eligible := true}} = R <- Rows]),
    UnkOwn = sum_own([R || {_, #{eligible := unknown}} = R <- Rows]),
    io:format("eligible (compiles as-is): ~s~n", [pct(EligOwn, Total)]),
    case UnkOwn > 0 of
        true -> io:format("  (~s own-time in functions with no census "
                          "join -- BIF/NIF/uncensused)~n", [pct(UnkOwn, Total)]);
        false -> ok
    end,

    %% Per-class gross vs marginal.
    io:format("~n-- per class: gross (any fn touching C) vs marginal "
              "(C is the SOLE blocker) --~n", []),
    Classes = [call_fun, maps, bs_construction, bs_position, exceptions,
               recv, float_reg, general_bif, other],
    [begin
         Gross = sum_own([R || R <- Rows, blocks(R, C)]),
         Marg = sum_own([R || R <- Rows, sole_blocker(R, C)]),
         io:format("  ~-16s gross ~-7s marginal ~-7s~n",
                   [atom_to_list(C), pct(Gross, Total), pct(Marg, Total)])
     end || C <- Classes],

    %% Union coverage (the decision-rule number).
    UnionOwn = sum_own([R || R <- Rows, union_blocked(R)]),
    io:format("~n-- UNION {call_fun,maps,bs_construction,bs_position} --~n", []),
    io:format("  union addressable coverage: ~s  (rule: >=15% funds a "
              "service frontend)~n", [pct(UnionOwn, Total)]),

    %% Region split among loop-shaped, union-blocked functions.
    RegionRows = [R || R <- Rows, union_blocked(R), is_loop(R)],
    RegionOK = sum_own([R || R <- RegionRows, all_blockers_out_of_loop(R)]),
    RegionNo = sum_own([R || R <- RegionRows, not all_blockers_out_of_loop(R)]),
    io:format("~n-- region compilation (loop-shaped, union-blocked) --~n", []),
    io:format("  blockers OUT of loop (region-compilable): ~s~n",
              [pct(RegionOK, Total)]),
    io:format("  blockers IN loop (region cannot help):     ~s~n",
              [pct(RegionNo, Total)]),

    %% Entanglement: the top own-time functions and their blocker sets.
    io:format("~n-- top own-time functions (blocker class sets) --~n", []),
    Top = lists:sublist(
            lists:reverse(lists:keysort(2, [{K, own(V)} || {K, V} <- Rows])),
            12),
    [begin
         #{eligible := E, classes := Cs} = maps:get(K, J),
         Set = [C || {C, _, _, _} <- Cs],
         Tag = case E of true -> "ELIGIBLE"; unknown -> "(no-join)";
                   false -> "blocked" end,
         {M, F, A} = K,
         io:format("  ~-7s ~p:~p/~p  ~s  ~p~n",
                   [pct(O, Total), M, F, A, Tag, Set])
     end || {K, O} <- Top],
    ok.

%% ---- predicates over a joined row -----------------------------------

own({_, V}) -> maps:get(own, V, 0);
own(V) when is_map(V) -> maps:get(own, V, 0).

sum_own(Rows) -> lists:sum([own(V) || {_, V} <- Rows]).

class_set({_, V}) -> [C || {C, _, _, _} <- maps:get(classes, V, [])].

blocks(R, C) -> lists:member(C, class_set(R)).

sole_blocker(R, C) -> class_set(R) =:= [C].

union_blocked({_, V} = R) ->
    Set = class_set(R),
    Set =/= [] andalso maps:get(eligible, V, unknown) =:= false
        andalso lists:all(fun(C) -> lists:member(C, ?UNION) end, Set).

is_loop({_, V}) -> maps:get(loop, V, false) =:= true.

%% Every blocking op that lies in a UNION class sits OUT of the hot loop.
all_blockers_out_of_loop({_, V}) ->
    lists:all(fun({C, _T, In, _Out}) ->
                      (not lists:member(C, ?UNION)) orelse In =:= 0
              end, maps:get(classes, V, [])).

pct(_, 0) -> "0.0%";
pct(X, Total) -> lists:flatten(io_lib:format("~.1f%", [100.0 * X / Total])).
