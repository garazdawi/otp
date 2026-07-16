%% SPDX-License-Identifier: Apache-2.0
%% Copyright Ericsson AB 2026. All Rights Reserved.
%%
%% S1b.3d deopt-correctness validation for the map-shape specialization.
%% Warm mapbench:r16/3 on the literal shape s16 so the profiler samples its
%% keys pointer and the specializer bakes a shape-guarded O(1) load, then
%% feed r16 maps that MISS the baked shape and confirm every path is
%% byte-identical to T1: the shape guard must side-exit to T1, which does
%% the real lookup (found OR not-found) exactly as generic execution would.
%%
%% Run under the tier-up path: `T2_RETAIN=1 T2_TIER_THRESHOLD=1000 erl +S 1`
%% (sampling is scheduler-1 only). Every line must print OK / the expected
%% exception.
-module(mapval).
-export([go/0]).

go() ->
    erts_debug:set_internal_state(available_internal_state, true),
    S16 = mapbench:s16(),
    %% warm up on the literal shape -> profiler captures {mono,1,keys(S16)}
    %% and the function tiers up + specializes.
    _ = mapbench:r16(3000000, S16, 0),
    Prof = lists:last(erts_debug:get_internal_state({t2_profile, mapbench})),
    io:format("r16 profile = ~p~n", [Prof]),

    %% (1) hit: the exact literal shape -> fast path.
    check("hit/literal", S16, 9),

    %% (2) a DIFFERENT flatmap shape that still holds f09 -> shape guard
    %% misses, deopt to T1, T1 finds f09.
    Other = maps:from_list([{f09, 9} |
                            [{list_to_atom("g" ++ integer_to_list(I)), I}
                             || I <- lists:seq(1, 10)]]),
    check("miss/other-flatmap", Other, 9),

    %% (3) a HASHMAP (>32 keys) holding f09 -> not a flatmap at all, deopt,
    %% T1 finds f09.
    Big = maps:from_list([{f09, 9} |
                          [{list_to_atom("h" ++ integer_to_list(I)), I}
                           || I <- lists:seq(1, 40)]]),
    check("miss/hashmap", Big, 9),

    %% (4) a map WITHOUT f09 -> deopt, T1 re-runs the match and raises the
    %% same badmatch generic execution would.
    NoKey = #{z1 => 1, z2 => 2},
    R = try
            mapbench:r16(1, NoKey, 0),
            no_error
        catch
            Class:Reason -> {Class, Reason}
        end,
    io:format("miss/no-key         -> ~p (expect {error,{badmatch,_}})~n", [R]),
    halt().

check(Tag, M, Expect) ->
    R = mapbench:r16(1, M, 0),
    io:format("~-20s r16(1,M,0) = ~p (expect ~p) ~s~n",
              [Tag, R, Expect,
               case R =:= Expect of
                   true -> "OK";
                   false -> "FAIL"
               end]).
