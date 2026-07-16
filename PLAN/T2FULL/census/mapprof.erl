%% SPDX-License-Identifier: Apache-2.0
%% Copyright Ericsson AB 2026. All Rights Reserved.
%% S1b.3b profiler-capture test: a hot tail-recursive loop reading a
%% literal-shaped flatmap should capture the map's keys-tuple pointer into
%% its ErtsT2Profile.map_shape (dumped via {t2_profile, Mod}), matching
%% {map_keys_ptr, Map}.
-module(mapprof).
-export([run/0]).

base() -> #{a => 1, b => 2, c => 3}.

loop(0, _M, A) -> A;
loop(K, M, A) ->
    #{b := V} = M,
    loop(K - 1, M, A bxor V).

run() ->
    erts_debug:set_internal_state(available_internal_state, true),
    M = base(),
    KP = erts_debug:get_internal_state({map_keys_ptr, M}),
    _ = loop(5000000, M, 0),
    Prof = erts_debug:get_internal_state({t2_profile, mapprof}),
    St = erts_debug:get_internal_state(t2_stats),
    io:format("map_keys_ptr(M) = ~p~n", [KP]),
    io:format("t2_stats        = ~p~n", [St]),
    io:format("t2_profile      = ~p~n", [Prof]),
    Shapes = [S || {_Fn, _Ar, _Cnt, _Ns, _Ty, _Fun, S} <- Prof],
    io:format("captured shapes = ~p  (expect one == ~p)~n", [Shapes, KP]),
    halt().
