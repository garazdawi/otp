%% SPDX-License-Identifier: Apache-2.0
%% Copyright Ericsson AB 2026. All Rights Reserved.
%%
%% T2-Full differential hammer. Runs a large battery of pure computations that
%% exercise the T2-eligible features (maps get/put, get_map_element with literal
%% keys, try/catch, binary matching + bs_position, list folds, records, closures,
%% arithmetic/bignum, guards). Every result is deterministic (no pids/refs/time).
%% Run twice -- `+JT2enable false` and `+JT2enable true` -- and diff the output.
%% Any difference is a T2 miscompile.
-module(t2hammer).
-export([main/0, run/0]).

main() ->
    R = run(),
    %% Print a stable, fully-forced representation.
    io:format("~p~n", [erlang:phash2(R, 1 bsl 30)]),
    io:format("~p~n", [R]),
    halt(0).

run() ->
    [{maps_get, maps_get()},
     {maps_lit_key, maps_lit_key()},
     {maps_put, maps_put()},
     {records, records()},
     {exceptions, exceptions()},
     {binaries, binaries()},
     {folds, folds()},
     {closures, closures()},
     {arith, arith()},
     {guards, guards()},
     {mixed, mixed()}].

%% ---- maps: get_map_element on many shapes, mono + poly sites ----
maps_get() ->
    Big = (maps:from_list([{list_to_atom("k" ++ integer_to_list(I)), I}
                           || I <- lists:seq(1, 40)]))#{a => 99},
    Shapes = [#{a => 1, b => 2, c => 3},
              #{a => 10, b => 20, c => 30, d => 40},
              #{x => 1, a => 2},
              Big],
    %% monomorphic-ish: repeatedly pull the same literal key
    Mono = [ get_a(M) || M <- Shapes, _ <- lists:seq(1, 100) ],
    %% key-miss (exception path)
    Miss = [ catch_get(M, zzz) || M <- Shapes ],
    %% get_map_elements (multi)
    Multi = [ get_ab(M) || M <- Shapes ],
    {lists:sum([X || X <- Mono, is_integer(X)]), Miss, Multi}.

get_a(#{a := A}) -> A;
get_a(_) -> no_a.

get_ab(#{a := A, b := B}) -> {A, B};
get_ab(#{a := A}) -> {A, no_b};
get_ab(_) -> none.

catch_get(M, K) ->
    try maps:get(K, M) of V -> {ok, V}
    catch C:E -> {C, E}
    end.

%% ---- literal-key access repeated to trigger shape specialization ----
maps_lit_key() ->
    M = #{alpha => 1, beta => 2, gamma => 3, delta => 4},
    loop_lit(300000, M, 0).

loop_lit(0, _M, Acc) -> Acc;
loop_lit(N, M, Acc) ->
    #{beta := B, delta := D} = M,
    loop_lit(N - 1, M, Acc + B + D).

%% ---- maps put / update ----
maps_put() ->
    M0 = #{a => 1, b => 2},
    M1 = M0#{a => 100},                  % update existing (assoc)
    M2 = M1#{c => 3},                    % add new
    M3 = maps:put(d, 4, M2),
    M4 = maps:update(b, 20, M3),
    M5 = maps:remove(a, M4),
    {lists:sort(maps:to_list(M5)), maps:size(M5), catch M0#{nope := 1}}.

%% ---- records (update_record) ----
-record(r, {a = 0, b = 0, c = 0, d = 0}).
records() ->
    R0 = #r{a = 1, b = 2},
    R1 = R0#r{a = 10},
    R2 = R1#r{c = 3, d = 4},
    R3 = upd_loop(100000, R2),
    {R3#r.a, R3#r.b, R3#r.c, R3#r.d,
     is_record(R3, r), erlang:element(1, R3)}.

upd_loop(0, R) -> R;
upd_loop(N, R) -> upd_loop(N - 1, R#r{a = R#r.a + 1}).

%% ---- exceptions: every class, nested, in loops ----
exceptions() ->
    [ex_try(fun() -> 1 + 1 end),
     ex_try(fun() -> throw(boom) end),
     ex_try(fun() -> error(bang) end),
     ex_try(fun() -> exit(gone) end),
     ex_try(fun() -> 1 = 2 end),                    % badmatch
     ex_try(fun() -> case foo of bar -> ok end end),% case_clause
     ex_try(fun() -> atom_to_list(42) end),         % badarg
     ex_try(fun() -> 1 / 0 end),                    % badarith
     ex_after(),
     ex_nested(),
     ex_loop(50000)].

ex_try(F) ->
    try F() of V -> {value, V}
    catch C:E:_St -> {C, E}
    end.

ex_after() ->
    Tab = ets:new(t, [private]),
    try ets:insert(Tab, {k, 1}), ets:lookup(Tab, k)
    after ets:delete(Tab)
    end.

ex_nested() ->
    try
        try throw(inner) catch throw:inner -> error(outer) end
    catch C:E -> {C, E}
    end.

ex_loop(N) -> ex_loop(N, 0).
ex_loop(0, Acc) -> Acc;
ex_loop(N, Acc) ->
    V = try
            case N rem 3 of
                0 -> throw(N);
                _ -> N
            end
        catch throw:X -> -X
        end,
    ex_loop(N - 1, Acc + V).

%% ---- binaries: bs_position, segments, bitstrings ----
binaries() ->
    B = <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10, "hello", 16#DEAD:16>>,
    {parse(B),
     [ split_at(B, N) || N <- [0, 1, 5, 10] ],
     bits(<<2#1011:4, 2#0110:4>>),
     utf(<<"héllo wörld"/utf8>>),
     scan_loop(mkbin(2000), 0)}.

parse(<<A, B, Rest/binary>>) -> {A, B, byte_size(Rest), Rest};
parse(_) -> short.

split_at(B, N) when N =< byte_size(B) ->
    <<H:N/binary, T/binary>> = B,
    {H, byte_size(T)}.

bits(<<X:4, Y:4>>) -> {X, Y}.

utf(B) -> {byte_size(B), [C || <<C/utf8>> <= B]}.

mkbin(N) -> list_to_binary(lists:seq(0, 255) ++ [ I band 255 || I <- lists:seq(0, N) ]).

scan_loop(<<>>, Acc) -> Acc;
scan_loop(<<Byte, Rest/binary>>, Acc) -> scan_loop(Rest, Acc + Byte);
scan_loop(<<>>, Acc) -> Acc.

%% ---- list folds / body recursion ----
folds() ->
    L = lists:seq(1, 5000),
    {lists:foldl(fun(X, A) -> X + A end, 0, L),
     lists:foldr(fun(X, A) -> X - A end, 0, L),
     lists:sum(lists:map(fun(X) -> X * X end, L)),
     length(lists:filter(fun(X) -> X rem 7 =:= 0 end, L)),
     bodyrec(L)}.

bodyrec([]) -> [];
bodyrec([H | T]) -> [H * 2 | bodyrec(T)].

%% ---- closures / funs (inlining) ----
closures() ->
    Add = fun(X) -> fun(Y) -> X + Y end end,
    Add5 = Add(5),
    F = fun F(0, Acc) -> Acc; F(N, Acc) -> F(N - 1, Acc + N) end,
    {Add5(10), [ apply_fun(Add5, N) || N <- lists:seq(1, 50) ], F(1000, 0),
     lists:map(fun(X) -> Add5(X) end, lists:seq(1, 20))}.

apply_fun(F, X) -> F(X).

%% ---- arithmetic, bignum, mixed types ----
arith() ->
    {fact(50),
     pow(2, 200),
     lists:sum([ I * I * I || I <- lists:seq(1, 300) ]),
     {1 bsl 100, (1 bsl 100) + 1, (1 bsl 100) div 7, (1 bsl 100) rem 13},
     {3.14 * 2, math:sqrt(2.0), trunc(9.99), round(2.5)}}.

fact(0) -> 1;
fact(N) -> N * fact(N - 1).

pow(_, 0) -> 1;
pow(B, E) -> B * pow(B, E - 1).

%% ---- guards ----
guards() ->
    [ classify(X) || X <- [0, 1, -1, 1.5, atom, "str", <<1>>, [], {a}, #{}, self_ref()] ].

self_ref() -> {tag, 42}.

classify(X) when is_integer(X), X > 0 -> pos_int;
classify(X) when is_integer(X), X < 0 -> neg_int;
classify(0) -> zero;
classify(X) when is_float(X) -> float;
classify(X) when is_atom(X) -> atom;
classify(X) when is_list(X) -> list;
classify(X) when is_binary(X) -> binary;
classify(X) when is_map(X) -> map;
classify(X) when is_tuple(X) -> tuple;
classify(_) -> other.

%% ---- mixed feature interplay ----
mixed() ->
    Data = [ #{id => I, name => list_to_atom("n" ++ integer_to_list(I)),
               vals => lists:seq(1, I rem 5)}
             || I <- lists:seq(1, 200) ],
    Sums = [ try lists:sum(maps:get(vals, D)) catch _:_ -> 0 end || D <- Data ],
    Grouped = lists:foldl(
                fun(#{id := Id} = D, Acc) ->
                        K = Id rem 4,
                        maps:update_with(K, fun(L) -> [D | L] end, [D], Acc)
                end, #{}, Data),
    {lists:sum(Sums), maps:size(Grouped),
     [ length(maps:get(K, Grouped)) || K <- lists:sort(maps:keys(Grouped)) ]}.
