%% SPDX-License-Identifier: Apache-2.0
%% Copyright Ericsson AB 2026. All Rights Reserved.
%%
%% S1a make-or-break microbench (PLAN/T2FULL/census/map_monomorphic_design.md).
%% Question: on a small flatmap, how much does the get_map_element key SCAN
%% cost -- i.e. how much could a monomorphic shape-guarded fixed-index load
%% save? No codegen: measure `#{K := V} = M` (compiles to get_map_element)
%% for a key at position 0 (key 1) vs position N-1 (key N). Keys are integers
%% 1..N so term order == position. The delta is exactly the scan cost that a
%% shape guard removes; position-0 cost is the specialization ceiling.
-module(mapscan).
-export([go/0]).

-define(REP, 20000000).

mk(N) -> maps:from_list([{I, (I * 7) band 255} || I <- lists:seq(1, N)]).

le(0, _M, A) -> A;
le(K, M, A) -> le(K - 1, M, A bxor K).
lf(0, _M, A) -> A;
lf(K, M, A) -> #{1 := V} = M, lf(K - 1, M, A bxor V).
l3(0, _M, A) -> A;
l3(K, M, A) -> #{3 := V} = M, l3(K - 1, M, A bxor V).
l5(0, _M, A) -> A;
l5(K, M, A) -> #{5 := V} = M, l5(K - 1, M, A bxor V).
l8(0, _M, A) -> A;
l8(K, M, A) -> #{8 := V} = M, l8(K - 1, M, A bxor V).
l16(0, _M, A) -> A;
l16(K, M, A) -> #{16 := V} = M, l16(K - 1, M, A bxor V).
l32(0, _M, A) -> A;
l32(K, M, A) -> #{32 := V} = M, l32(K - 1, M, A bxor V).

best(F) ->
    _ = F(200000),
    lists:min([element(1, timer:tc(fun() -> F(?REP) end)) || _ <- [1, 2, 3]])
        * 1000 / ?REP.

go() ->
    %% Key 1 (smallest) is found LAST in the scan; key N (largest) is found
    %% FIRST -- so lf(key 1) = far end, lN(key N) = near end. A shape-guarded
    %% fixed-index load is position-independent, so its cost ~ the near-end
    %% (found-immediately) cost; the win it buys = (cost at the key's scan
    %% position) - near. Report near, far, per-key scan, and the win at the
    %% far key and at a uniform-mean position.
    _ = best(fun(K) -> le(K, #{}, 0) end),
    io:format("~-5s ~9s ~9s ~9s ~10s ~10s~n",
              ["size", "near", "far", "perkey", "far-win", "mean-win"]),
    Legs = [{3, fun l3/3}, {5, fun l5/3}, {8, fun l8/3},
            {16, fun l16/3}, {32, fun l32/3}],
    [begin
         M = mk(N),
         Far = best(fun(K) -> lf(K, M, 0) end),      %% key 1, scanned last
         Near = best(fun(K) -> LastF(K, M, 0) end),  %% key N, found first (spec ceiling)
         PerKey = (Far - Near) / (N - 1),            %% per-key linear-scan cost
         FarWin = (Far - Near) / Far,                %% spec saving on the far key
         MeanCost = Near + PerKey * (N - 1) / 2,
         MeanWin = (MeanCost - Near) / MeanCost,      %% spec saving at uniform-mean pos
         io:format("~-5w ~9.2f ~9.2f ~9.3f ~9.1f% ~9.1f%~n",
                   [N, Near, Far, PerKey, 100 * FarWin, 100 * MeanWin])
     end || {N, LastF} <- Legs],
    halt().
