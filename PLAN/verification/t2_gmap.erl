%% T2 G-map experiment — region-level shape specialization on
%% struct-like flatmaps (the Elixir-struct shape: instances created
%% by updating a template share the keys tuple, all accesses hit the
%% same shape).
%%
%% sum_scores/2 walks a list of 5-key maps reading two fields per
%% map. T1 pays a per-key linear scan of the keys tuple inside
%% get_map_elements; the T2 specialization (T2_GMAP=1) guards the
%% shape (two key-slot compares) and loads values at fixed offsets.
%%
%%   t2_gmap:bench(Reps) / t2_gmap:check()
-module(t2_gmap).
-export([bench/1, check/0, sum_scores/2, probe/0]).

template() ->
    #{'__struct__' => user, active => false, id => 0,
      name => <<"x">>, score => 0}.

instances(N) ->
    T = template(),
    [T#{id := I, score := I rem 1000,
        active := (I band 1) =:= 1} || I <- lists:seq(1, N)].

%% The T2 target: per element, read two fields of a same-shaped map.
sum_scores([M | T], Acc) ->
    #{active := A, score := S} = M,
    case A of
        true -> sum_scores(T, Acc + S);
        false -> sum_scores(T, Acc)
    end;
sum_scores([], Acc) ->
    Acc.

bench(Reps) ->
    L = instances(100_000),
    Times = [begin
                 T0 = erlang:monotonic_time(microsecond),
                 _ = loop(L, 10),
                 erlang:monotonic_time(microsecond) - T0
             end || _ <- lists:seq(1, Reps)],
    io:format("sum_scores 10x100k maps  min ~7b us~n", [lists:min(Times)]),
    ok.

loop(_L, 0) -> ok;
loop(L, N) -> _ = sum_scores(L, 0), loop(L, N - 1).

check() ->
    L = instances(50_000),
    R1 = sum_scores(L, 0),
    %% shapes that must side-exit correctly:
    Bigger = (template())#{extra1 => 1, extra2 => 2},  % 7-key flatmap
    Hash = maps:from_list([{I, I} || I <- lists:seq(1, 64)]), % hashmap
    NoScore = maps:remove(score, template()),
    R2 = sum_scores([template(), Bigger#{score := 5, active := true}], 0),
    R3 = (catch sum_scores([Hash], 0)),          % badmatch via T1
    R4 = (catch sum_scores([NoScore], 0)),       % badmatch via T1
    R5 = (catch sum_scores([template() | nope], 0)), % improper list
    R6 = sum_scores([(template())#{score := 1 bsl 61, active := true}], 0),
    H = erlang:phash2({R1, R2, R3, R4, R5, R6}, 1 bsl 32),
    io:format("gmap hash ~b~n", [H]),
    H.

probe() ->
    [M | _] = instances(2),
    {erts_internal:map_to_tuple_keys(template()),
     erts_internal:map_to_tuple_keys(M)}.
