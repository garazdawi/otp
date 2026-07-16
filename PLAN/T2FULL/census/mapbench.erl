%% SPDX-License-Identifier: Apache-2.0
%% Copyright Ericsson AB 2026. All Rights Reserved.
%%
%% S1b.3c end-to-end benchmark: read a LITERAL-shaped flatmap ("struct")
%% hot, so the map-shape profiler (S1b.3b) captures its shape and the
%% specializer (S1b.3c) can bake an O(1) shape-guarded load in place of the
%% key scan. Each reader is a tail-recursive loop (armed for tier-up) that
%% reads one field of a same-module literal map passed as an argument --
%% the shape the profiler samples. `result` is printed so a T2-specialized
%% run can be diffed against a T1 run for correctness (byte-identical).
%%
%% Run T1 baseline (no tier-up) vs T2 (tier-up + specialization) and
%% compare ns/op and result. Sampling is scheduler-1 only, so use +S 1.
-module(mapbench).
-export([go/0]).

-define(REP, 20000000).

%% literal "struct" shapes; keys are atoms so term order fixes the
%% flatmap index. Read a middle key (worst-ish scan position).
s3() -> #{f1 => 11, f2 => 22, f3 => 33}.
s8() ->
    #{f1 => 11, f2 => 22, f3 => 33, f4 => 44,
      f5 => 55, f6 => 66, f7 => 77, f8 => 88}.
s16() ->
    #{f01 => 1, f02 => 2, f03 => 3, f04 => 4, f05 => 5, f06 => 6,
      f07 => 7, f08 => 8, f09 => 9, f10 => 10, f11 => 11, f12 => 12,
      f13 => 13, f14 => 14, f15 => 15, f16 => 16}.

r3(0, _M, A) -> A;
r3(K, M, A) -> #{f2 := V} = M, r3(K - 1, M, A bxor V).
r8(0, _M, A) -> A;
r8(K, M, A) -> #{f5 := V} = M, r8(K - 1, M, A bxor V).
r16(0, _M, A) -> A;
r16(K, M, A) -> #{f09 := V} = M, r16(K - 1, M, A bxor V).

best(F, M) ->
    _ = F(200000, M, 0),
    lists:min([element(1, timer:tc(fun() -> F(?REP, M, 0) end)) || _ <- [1, 2, 3]])
        * 1000 / ?REP.

go() ->
    io:format("~-5s ~10s ~8s~n", ["size", "ns/op", "result"]),
    Legs = [{3, fun r3/3, s3()}, {8, fun r8/3, s8()}, {16, fun r16/3, s16()}],
    [begin
         R = F(1001, M, 0),
         T = best(F, M),
         io:format("~-5w ~10.2f ~8w~n", [N, T, R])
     end || {N, F, M} <- Legs],
    halt().
