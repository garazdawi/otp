%% SPDX-License-Identifier: Apache-2.0
%% Copyright Ericsson AB 2026. All Rights Reserved.
%%
%% S1b.1 codegen-ceiling probe (PLAN/T2FULL/census/map_monomorphic_design.md).
%% Pairs with the T2_MAP_SPEC_IDX env-gated emitter hack in t2_emit.cpp: when
%% that env is set to <i>, EVERY T2 get_map_element emits IsFlatmapBounded + a
%% fixed values[i] load instead of the O(k) scan fragment. This module reads
%% key 1, which for maps built from {1..N} lives at flatmap index 0 -- so run
%% it with T2_MAP_SPEC_IDX=0 to exercise the specialized load, and without the
%% env (or under T1) for the scan baseline.
%%
%% Key 1 is the FAR end of the scan (found last), so this measures the largest
%% win the specialization can buy -- the S1a "far" column, now in real codegen.
%% `result' is printed so a run under the spec can be diffed against baseline:
%% same result == the indexed load returned the same value as the scan.
-module(mapspec).
-export([go/0]).

-define(REP, 20000000).

mk(N) -> maps:from_list([{I, (I * 7) band 255} || I <- lists:seq(1, N)]).

%% read key 1 (flatmap index 0 for {1..N} maps); odd trip count so the
%% bxor accumulator is value-sensitive (a wrong-index load changes it).
l(0, _M, A) -> A;
l(K, M, A) -> #{1 := V} = M, l(K - 1, M, A bxor V).

best(M) ->
    _ = l(200000, M, 0),
    lists:min([element(1, timer:tc(fun() -> l(?REP, M, 0) end)) || _ <- [1, 2, 3]])
        * 1000 / ?REP.

go() ->
    io:format("~-5s ~10s ~8s~n", ["size", "ns/op", "result"]),
    [begin
         M = mk(N),
         R = l(1001, M, 0),
         T = best(M),
         io:format("~-5w ~10.2f ~8w~n", [N, T, R])
     end || N <- [3, 5, 8, 16, 32]],
    halt().
