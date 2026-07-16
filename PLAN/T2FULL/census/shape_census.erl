%% SPDX-License-Identifier: Apache-2.0
%% Copyright Ericsson AB 2026. All Rights Reserved.
%%
%% PLAN/T2FULL/22 — the loop-shape split of the S0 census.
%%
%% Answers "what about non-loop hot functions?": re-slices the S0
%% own-time census by the instrument's per-function LoopShaped flag,
%% partitioning own-time into a shape x reachability grid:
%%
%%   reachability:  eligible (installs as-is)
%%                | union_addr (blocked, but every blocker in the
%%                             {call_fun,maps,bs_construction,bs_position}
%%                             union -- the inline/opcode-addressable pool)
%%                | hard (blocked on a non-union class)
%%                | nojoin (no census entry -- BIF/NIF)
%%   shape:         loop (instrument LoopShaped=true, self-recursion path)
%%                | nonloop
%%
%% Unlike census_run, this censuses EVERY measured module (not just the
%% candidate), so the library residue (maps:fold_1, lists:foldl_1, ...)
%% also gets a shape+reachability tag -- that residue is exactly the
%% "would an inliner help non-loops?" pool, and its shape decides whether
%% inlining it lands in the loop tier or a genuinely non-loop optimizer.
-module(shape_census).
-export([all/0, json/0, mapwl/0, compiler/0]).

-define(UNION, [call_fun, maps, bs_construction, bs_position]).

all() -> json(), mapwl(), compiler(), ok.

%% ---- three legs, mirroring census_run but census == measure ---------

json() ->
    Doc = json_doc(),
    Bin = iolist_to_binary(json:encode(Doc)),
    Work = fun() ->
                   _ = [json:encode(Doc) || _ <- lists:seq(1, 4000)],
                   _ = [json:decode(Bin) || _ <- lists:seq(1, 4000)],
                   ok
           end,
    Mods = [json, binary, unicode, unicode_util, lists],
    report("json (service)", t2_census:run("json", Mods, Work, Mods)).

json_doc() ->
    #{<<"id">> => 12345,
      <<"name">> => <<"a moderately sized record name string">>,
      <<"active">> => true, <<"score">> => 98.6,
      <<"tags">> => [<<"alpha">>, <<"beta">>, <<"gamma">>, <<"delta">>],
      <<"items">> =>
          [#{<<"k">> => N,
             <<"v">> => <<"value ", (integer_to_binary(N))/binary>>,
             <<"ok">> => (N rem 2) =:= 0} || N <- lists:seq(1, 40)],
      <<"nested">> =>
          #{<<"a">> => #{<<"b">> => #{<<"c">> => [1, 2, 3, 4, 5]}},
            <<"list">> => [true, false, null, 1.5, <<"s">>]}}.

mapwl() ->
    D = mapwl:setup(),
    Work = fun() -> mapwl:work(D) end,
    Mods = [mapwl, maps, lists],
    report("mapwl (map/term-heavy service)",
           t2_census:run("mapwl", Mods, Work, Mods)).

compiler() ->
    _ = application:load(compiler),
    CMods = case application:get_key(compiler, modules) of
                {ok, L} -> L; _ -> [compile]
            end,
    Helpers = [lists, maps, sets, ordsets, gb_sets, gb_trees, dict, orddict,
               string, erl_anno, erl_lint, erl_expand_records, erl_internal,
               binary],
    Mods = lists:usort(CMods ++ Helpers),
    Src = filename:join(code:lib_dir(stdlib), "src/proplists.erl"),
    Work = fun() ->
                   _ = [compile:file(Src, [binary, return_errors,
                                           {warn_format, 0}])
                        || _ <- lists:seq(1, 80)],
                   ok
           end,
    report("compiler (analysis)", t2_census:run("compiler", Mods, Work, Mods)).

%% ---- the shape x reachability grid ----------------------------------

report(Name, J) ->
    Rows = maps:to_list(J),
    Total = sum(Rows),
    io:format("~n########## SHAPE SPLIT: ~s ##########~n", [Name]),
    io:format("total own-time: ~.1f ms~n", [Total / 1000.0]),
    Cats = [eligible, union_addr, hard, nojoin],
    io:format("~n~-14s ~10s ~10s ~10s~n",
              ["reach\\shape", "loop", "non-loop", "row-total"]),
    [begin
         Lp = share(Rows, Total, C, loop),
         Nl = share(Rows, Total, C, nonloop),
         Tot = share(Rows, Total, C, any),
         io:format("~-14s ~9.1f% ~9.1f% ~9.1f%~n",
                   [atom_to_list(C), Lp, Nl, Tot])
     end || C <- Cats],
    ColLoop = share(Rows, Total, any, loop),
    ColNon = share(Rows, Total, any, nonloop),
    io:format("~-14s ~9.1f% ~9.1f% ~9.1f%~n",
              ["col-total", ColLoop, ColNon, 100.0]),

    %% The two numbers the non-loop question turns on.
    io:format("~n>> non-loop eligible own-time:      ~.1f%  "
              "(only straight-line/P4 opts apply)~n",
              [share(Rows, Total, eligible, nonloop)]),
    io:format(">> non-loop union-addressable:      ~.1f%  "
              "(non-loop inline/opcode pool)~n",
              [share(Rows, Total, union_addr, nonloop)]),

    %% What ARE the non-loop hot functions? Top 12 by own-time.
    io:format("~n-- top non-loop-shaped functions by own-time --~n"),
    NL = [{K, own(V)} || {K, V} <- Rows, shape(V) =:= nonloop],
    Top = lists:sublist(lists:reverse(lists:keysort(2, NL)), 12),
    [begin
         V = maps:get(K, J),
         {M, F, A} = K,
         io:format("  ~5.1f%  ~-14s ~p:~p/~p  ~p~n",
                   [100.0 * O / Total, cat(V), M, F, A,
                    [C || {C, _, _, _} <- maps:get(classes, V, [])]])
     end || {K, O} <- Top],
    ok.

%% ---- categorization --------------------------------------------------

cat(V) ->
    case maps:get(eligible, V, unknown) of
        true -> eligible;
        unknown -> nojoin;
        false ->
            Set = [C || {C, _, _, _} <- maps:get(classes, V, [])],
            case Set =/= [] andalso
                 lists:all(fun(C) -> lists:member(C, ?UNION) end, Set) of
                true -> union_addr;
                false -> hard
            end
    end.

shape(V) ->
    case maps:get(loop, V, false) of true -> loop; _ -> nonloop end.

share(Rows, Total, Cat, Shape) ->
    Sel = [own(V) || {_, V} <- Rows,
                     Cat =:= any orelse cat(V) =:= Cat,
                     Shape =:= any orelse shape(V) =:= Shape],
    case Total of 0 -> 0.0; _ -> 100.0 * lists:sum(Sel) / Total end.

own(V) -> maps:get(own, V, 0).
sum(Rows) -> lists:sum([own(V) || {_, V} <- Rows]).
