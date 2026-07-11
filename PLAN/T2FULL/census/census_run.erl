%% SPDX-License-Identifier: Apache-2.0
%% Copyright Ericsson AB 2026. All Rights Reserved.
%%
%% PLAN/T2FULL/19 §2 S0 — corpus runner. Three legs:
%%   * json      — a canonical byte-slinging service (reproduces memo 14)
%%   * mapwl     — the map/term-heavy service class (memo 17 §4 gap)
%%   * compiler  — the analysis class (memo 14 marginal ~3-6%)
-module(census_run).
-export([all/0, json_leg/0, map_leg/0, compiler_leg/0]).

all() ->
    json_leg(),
    map_leg(),
    compiler_leg(),
    ok.

%% ---- json (service / byte-slinging) ---------------------------------

json_leg() ->
    Doc = json_doc(),
    Bin = iolist_to_binary(json:encode(Doc)),
    Work = fun() ->
                   _ = [json:encode(Doc) || _ <- lists:seq(1, 4000)],
                   _ = [json:decode(Bin) || _ <- lists:seq(1, 4000)],
                   ok
           end,
    %% Whole-workload denominator: measure json plus the byte/BIF modules
    %% it drives (their time is real work but is not a T2 candidate, so it
    %% dilutes coverage to whole-workload terms). Census only json.
    Measure = [json, binary, unicode, unicode_util, lists],
    t2_census:run("json (service)", [json], Work, Measure).

json_doc() ->
    #{<<"id">> => 12345,
      <<"name">> => <<"a moderately sized record name string">>,
      <<"active">> => true,
      <<"score">> => 98.6,
      <<"tags">> => [<<"alpha">>, <<"beta">>, <<"gamma">>, <<"delta">>],
      <<"items">> =>
          [#{<<"k">> => N, <<"v">> => <<"value ", (integer_to_binary(N))/binary>>,
             <<"ok">> => (N rem 2) =:= 0}
           || N <- lists:seq(1, 40)],
      <<"nested">> =>
          #{<<"a">> => #{<<"b">> => #{<<"c">> => [1, 2, 3, 4, 5]}},
            <<"list">> => [true, false, null, 1.5, <<"s">>]}}.

%% ---- map/term-heavy service -----------------------------------------

map_leg() ->
    D = mapwl:setup(),
    Work = fun() -> mapwl:work(D) end,
    Measure = [mapwl, maps, lists],
    t2_census:run("mapwl (map/term-heavy service)", [mapwl], Work, Measure).

%% ---- compiler (analysis) --------------------------------------------

compiler_leg() ->
    _ = application:load(compiler),
    CMods = case application:get_key(compiler, modules) of
                {ok, L} -> L;
                _ -> [compile]
            end,
    %% stdlib helpers the compiler leans on, so their own-time is censused too.
    Helpers = [lists, maps, sets, ordsets, gb_sets, gb_trees, dict, orddict,
               string, erl_anno, erl_lint, erl_expand_records, erl_internal,
               binary],
    Mods = lists:usort(CMods ++ Helpers),
    %% A self-contained, include-free real stdlib source; compiling it
    %% many times gives a stable own-time distribution over compiler
    %% passes (the passes are source-size-independent in shape).
    Src = filename:join(code:lib_dir(stdlib), "src/proplists.erl"),
    Work = fun() ->
                   _ = [compile:file(Src, [binary, return_errors,
                                           {warn_format, 0}])
                        || _ <- lists:seq(1, 80)],
                   ok
           end,
    t2_census:run("compiler (analysis)", Mods, Work, Mods).
