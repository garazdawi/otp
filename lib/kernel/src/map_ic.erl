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

%% @doc Diagnostics for the flatmap inline cache (IC).
%%
%% The map IC accelerates flatmap read/write operations by caching the
%% position of a key within a map's key tuple. It is enabled with the
%% `+mic true' VM flag and currently works with the interpreter emulator.
%%
%% This module exposes per-callsite IC statistics so you can see which
%% map operations in your code are hitting or missing the cache.
-module(map_ic).

-export([info/0, counters/0, summary/0, print_summary/0]).

%% @doc Return raw per-site IC entries from all schedulers.
%%
%% Each entry is a tuple:
%%   `{Site, Key, State, Hits, Misses, Scheduler, ShapeArity}'
%%
%% where `Site' is `{Module, Function, Arity}' or `undefined',
%% `Key' is the cached map key (atom, integer, etc.),
%% `State' is `active' or `disabled',
%% `Hits' and `Misses' are per-entry counters,
%% `Scheduler' is the scheduler number (1-based),
%% and `ShapeArity' is the number of keys in the cached map shape.
%%
%% Returns `[]' if the IC is not enabled (`+mic true').
-spec info() -> [tuple()].
info() ->
    erlang:nif_error(undef).

%% @doc Return global IC counters as a proplist.
%%
%% Keys: `attempts', `hits', `misses', `fills', `disabled'.
-spec counters() -> [{atom(), integer()}].
counters() ->
    erlang:nif_error(undef).

%% @doc Aggregate IC entries by callsite, summing across schedulers.
%%
%% Returns a list of maps sorted by total hits (descending):
%% ```
%% [#{site => {M,F,A}, key => Key, state => active|disabled|mixed,
%%    hits => TotalHits, misses => TotalMisses,
%%    shape_arity => Arity, schedulers => Count}, ...]
%% '''
-spec summary() -> [map()].
summary() ->
    Entries = info(),
    Grouped = aggregate_entries(Entries, #{}),
    Sorted = lists:sort(fun(#{hits := H1}, #{hits := H2}) -> H1 >= H2 end,
                        maps:values(Grouped)),
    Sorted.

%% @doc Print a formatted summary of IC statistics to stdout.
-spec print_summary() -> ok.
print_summary() ->
    Counters = counters(),
    io:format("=== Map Inline Cache Counters ===~n"),
    lists:foreach(fun({K, V}) ->
        io:format("  ~-12s ~w~n", [atom_to_list(K) ++ ":", V])
    end, Counters),
    io:nl(),

    Entries = summary(),
    case Entries of
        [] ->
            io:format("No IC entries (IC not enabled or no cached sites).~n");
        _ ->
            io:format("=== Per-Site IC Statistics (sorted by hits) ===~n"),
            io:format("~-40s ~-10s ~-8s ~8s ~8s ~5s ~5s~n",
                      ["Site", "Key", "State", "Hits", "Misses",
                       "Arity", "Scheds"]),
            io:format("~s~n", [lists:duplicate(89, $-)]),
            lists:foreach(fun print_entry/1, Entries)
    end,
    ok.


%% Internal helpers

aggregate_entries([], Acc) ->
    Acc;
aggregate_entries([{Site, Key, State, Hits, Misses, _Sched, ShapeArity} | Rest], Acc) ->
    SiteKey = {Site, Key},
    case Acc of
        #{SiteKey := Existing} ->
            #{hits := H, misses := M, schedulers := S,
              state := OldState} = Existing,
            NewState = merge_state(OldState, State),
            Updated = Existing#{hits := H + Hits,
                                misses := M + Misses,
                                schedulers := S + 1,
                                state := NewState},
            aggregate_entries(Rest, Acc#{SiteKey := Updated});
        _ ->
            Entry = #{site => Site,
                      key => Key,
                      state => State,
                      hits => Hits,
                      misses => Misses,
                      shape_arity => ShapeArity,
                      schedulers => 1},
            aggregate_entries(Rest, Acc#{SiteKey => Entry})
    end.

merge_state(Same, Same) -> Same;
merge_state(_, _) -> mixed.

print_entry(#{site := Site, key := Key, state := State,
              hits := Hits, misses := Misses,
              shape_arity := Arity, schedulers := Scheds}) ->
    SiteStr = format_site(Site),
    KeyStr = io_lib:format("~w", [Key]),
    io:format("~-40s ~-10s ~-8s ~8w ~8w ~5w ~5w~n",
              [SiteStr, KeyStr, State, Hits, Misses, Arity, Scheds]).

format_site({M, F, A}) ->
    io_lib:format("~s:~s/~w", [M, F, A]);
format_site(undefined) ->
    "<unknown>".
