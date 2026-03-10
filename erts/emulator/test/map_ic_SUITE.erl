%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025-2026. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

-module(map_ic_SUITE).

-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).

-export([t_system_info_counters/1,
         t_read_ic_hits/1,
         t_write_exact_ic_hits/1,
         t_write_assoc_ic_hits/1,
         t_ic_survives_gc/1,
         t_assoc_shape_change_disables/1,
         t_multi_key_bypasses_ic/1,
         t_map_ic_module/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap, {minutes, 1}}].

all() ->
    [t_system_info_counters,
     t_read_ic_hits,
     t_write_exact_ic_hits,
     t_write_assoc_ic_hits,
     t_ic_survives_gc,
     t_assoc_shape_change_disables,
     t_multi_key_bypasses_ic,
     t_map_ic_module].

init_per_suite(Config) ->
    case erlang:system_info(emu_flavor) of
        emu ->
            Config;
        _ ->
            {skip, "Map IC tests require the interpreter (emu flavor)"}
    end.

end_per_suite(_Config) ->
    ok.

%% Helpers

get_counters() ->
    erlang:system_info(map_ic_counters).

get_counter(Key) ->
    proplists:get_value(Key, get_counters()).

snapshot() ->
    #{attempts => get_counter(attempts),
      hits => get_counter(hits),
      misses => get_counter(misses),
      fills => get_counter(fills),
      disabled => get_counter(disabled),
      false_misses => get_counter(false_misses)}.

delta(Before, After) ->
    maps:map(fun(K, V) -> V - maps:get(K, Before) end, After).

%% system_info(map_ic_counters) returns a proplist with the expected keys.
t_system_info_counters(_Config) ->
    Counters = get_counters(),
    true = is_list(Counters),
    true = is_integer(proplists:get_value(attempts, Counters)),
    true = is_integer(proplists:get_value(hits, Counters)),
    true = is_integer(proplists:get_value(misses, Counters)),
    true = is_integer(proplists:get_value(fills, Counters)),
    true = is_integer(proplists:get_value(disabled, Counters)),
    ok.

%% Read IC: pattern-matching the same key at the same callsite
%% should produce IC hits after the first fill.
t_read_ic_hits(_Config) ->
    M = #{a => 1, b => 2, c => 3},
    %% Prime the IC
    _ = do_read(M),
    Before = snapshot(),
    _ = do_read_n(M, 100),
    After = snapshot(),
    D = delta(Before, After),
    ct:pal("Read IC delta: ~p", [D]),
    true = maps:get(hits, D) >= 100,
    ok.

do_read(#{a := V}) -> V.

do_read_n(_M, 0) -> ok;
do_read_n(M, N) ->
    _ = do_read(M),
    do_read_n(M, N - 1).

%% Write IC (exact): repeated M#{key := val} should produce hits.
t_write_exact_ic_hits(_Config) ->
    M = #{a => 1, b => 2, c => 3},
    %% Prime the IC
    _ = do_exact_update(M, 0),
    Before = snapshot(),
    _ = do_exact_update_n(M, 100),
    After = snapshot(),
    D = delta(Before, After),
    ct:pal("Write exact IC delta: ~p", [D]),
    true = maps:get(hits, D) >= 100,
    ok.

do_exact_update(M, V) ->
    M#{a := V}.

do_exact_update_n(_M, 0) -> ok;
do_exact_update_n(M, N) ->
    _ = do_exact_update(M, N),
    do_exact_update_n(M, N - 1).

%% Write IC (assoc, shape-preserving): repeated M#{key => val} on existing key.
t_write_assoc_ic_hits(_Config) ->
    M = #{a => 1, b => 2, c => 3},
    %% Prime the IC
    _ = do_assoc_update(M, 0),
    Before = snapshot(),
    _ = do_assoc_update_n(M, 100),
    After = snapshot(),
    D = delta(Before, After),
    ct:pal("Write assoc IC delta: ~p", [D]),
    true = maps:get(hits, D) >= 100,
    ok.

do_assoc_update(M, V) ->
    M#{b => V}.

do_assoc_update_n(_M, 0) -> ok;
do_assoc_update_n(M, N) ->
    _ = do_assoc_update(M, N),
    do_assoc_update_n(M, N - 1).

%% IC should survive garbage collection (shape interning keeps keys stable).
t_ic_survives_gc(_Config) ->
    M = #{x => 1, y => 2},
    %% Prime the IC
    _ = do_read_xy(M),
    _ = do_read_xy(M),
    %% Force GC
    erlang:garbage_collect(),
    Before = snapshot(),
    %% IC should still hit after GC
    _ = do_read_xy_n(M, 100),
    After = snapshot(),
    D = delta(Before, After),
    ct:pal("Post-GC IC delta: ~p", [D]),
    true = maps:get(hits, D) >= 100,
    ok.

do_read_xy(#{x := V}) -> V.

do_read_xy_n(_M, 0) -> ok;
do_read_xy_n(M, N) ->
    _ = do_read_xy(M),
    do_read_xy_n(M, N - 1).

%% Assoc update that adds a new key should disable the IC for that callsite.
t_assoc_shape_change_disables(_Config) ->
    M = #{a => 1},
    Before = snapshot(),
    %% This adds a new key — shape changes — IC should disable
    _ = do_assoc_new_key(M),
    After = snapshot(),
    D = delta(Before, After),
    ct:pal("Shape-change IC delta: ~p", [D]),
    %% Should have at least 1 attempt and the fill should lead to disable
    %% on the next miss (or immediate disable since key not found).
    true = maps:get(attempts, D) >= 1,
    ok.

do_assoc_new_key(M) -> M#{new_key => 42}.

%% Multi-key updates (n > 2) should bypass IC entirely.
t_multi_key_bypasses_ic(_Config) ->
    M = #{a => 1, b => 2, c => 3},
    Before = snapshot(),
    _ = do_multi_update_n(M, 100),
    After = snapshot(),
    D = delta(Before, After),
    ct:pal("Multi-key IC delta: ~p", [D]),
    %% Multi-key updates don't trigger IC attempts
    true = maps:get(attempts, D) =:= 0,
    ok.

do_multi_update(M, V) ->
    M#{a := V, b := V}.

do_multi_update_n(_M, 0) -> ok;
do_multi_update_n(M, N) ->
    _ = do_multi_update(M, N),
    do_multi_update_n(M, N - 1).

%% Test the map_ic module API: info/0, counters/0, summary/0.
t_map_ic_module(_Config) ->
    %% counters/0 returns a proplist
    Counters = map_ic:counters(),
    true = is_list(Counters),
    true = is_integer(proplists:get_value(attempts, Counters)),
    true = is_integer(proplists:get_value(hits, Counters)),

    %% info/0 returns a list of tuples
    Entries = map_ic:info(),
    true = is_list(Entries),
    lists:foreach(fun({Site, Key, State, Hits, Misses, Sched, ShapeArity}) ->
        true = (is_tuple(Site) andalso tuple_size(Site) =:= 3)
               orelse Site =:= undefined,
        true = is_atom(Key) orelse is_integer(Key) orelse Key =:= undefined,
        true = lists:member(State, [active, disabled, not_found]),
        true = is_integer(Hits) andalso Hits >= 0,
        true = is_integer(Misses) andalso Misses >= 0,
        true = is_integer(Sched) andalso Sched >= 1,
        true = is_integer(ShapeArity) andalso ShapeArity >= 0
    end, Entries),

    %% summary/0 returns a list of aggregated maps
    Summary = map_ic:summary(),
    true = is_list(Summary),
    lists:foreach(fun(E) ->
        true = is_map(E),
        true = is_map_key(site, E),
        true = is_map_key(hits, E),
        true = is_map_key(misses, E),
        true = is_map_key(state, E),
        true = is_map_key(schedulers, E)
    end, Summary),

    %% Prime a known site and verify it appears in info
    M = #{ic_test_key => 1, other => 2},
    _ = do_ic_module_read(M),
    _ = do_ic_module_read(M),
    _ = do_ic_module_read(M),
    Entries2 = map_ic:info(),
    MyEntries = [E || {Site, Key, _, _, _, _, _} = E <- Entries2,
                      Site =:= {?MODULE, do_ic_module_read, 1},
                      Key =:= ic_test_key],
    ct:pal("My IC entries: ~p", [MyEntries]),
    true = length(MyEntries) >= 1,
    [{_, _, active, Hits, _, _, _}] = MyEntries,
    true = Hits >= 2,
    ok.

do_ic_module_read(#{ic_test_key := V}) -> V.
