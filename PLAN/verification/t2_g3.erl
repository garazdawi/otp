%% T2 G3 experiment — benchmark + correctness harness for the
%% erl_types:are_all_limited/2 specialization.
%%
%% Workloads exercise t_limit/2, whose is_limited/2 mirror drives
%% are_all_limited/2 over tuple/union/product element lists — the
%% shape that carried 47% of all calls in the dialyzer PLT-build
%% census (PLAN/verification/RESULTS.md).
%%
%% Run on any build; the T2 specialization is selected by the
%% T2_G3 env var at module load (off | a | b), so T1 baseline and
%% both stages come from the same binary.
%%
%%   t2_g3:bench(Reps)  -> timing table for three corpus shapes
%%   t2_g3:check()      -> corpus hash (must be identical across modes)
-module(t2_g3).
-export([bench/1, check/0, corpora/0]).

-define(K, 6).

%% --- corpora -------------------------------------------------------

%% Leafy-wide: tuples whose elements are all leaf types (atom /
%% integer / var). The pure inline-fast-path case.
leafy() ->
    Leaves = [erl_types:t_atom(list_to_atom("a" ++ integer_to_list(I)))
              || I <- lists:seq(1, 8)] ++
             [erl_types:t_integer(I) || I <- lists:seq(1, 8)] ++
             [erl_types:t_var(I) || I <- lists:seq(1, 4)],
    [erl_types:t_tuple([lists:nth(1 + (I + J) rem length(Leaves), Leaves)
                        || J <- lists:seq(1, 24)])
     || I <- lists:seq(1, 50)].

%% Mixed: tuples with ~25% container elements (nested tuples,
%% lists), forcing the slow-path call per container element.
mixed() ->
    Leaf = fun(I) -> erl_types:t_atom(list_to_atom("m" ++ integer_to_list(I))) end,
    Nested = fun(I) ->
                     erl_types:t_tuple([Leaf(I), erl_types:t_integer(I),
                                        erl_types:t_list(Leaf(I))])
             end,
    [erl_types:t_tuple(
       lists:flatten([[Leaf(I), Leaf(I + 1), erl_types:t_integer(I),
                       Nested(I)] || I <- lists:seq(1, 6)]))
     || _ <- lists:seq(1, 50)].

%% Deep: recursively nested tuples (depth ~8) — exercises the
%% mutual recursion is_limited <-> are_all_limited at every level.
deep() ->
    Mk = fun Mk(0, I) -> erl_types:t_atom(list_to_atom("d" ++ integer_to_list(I)));
             Mk(D, I) -> erl_types:t_tuple([Mk(D - 1, I), erl_types:t_integer(D),
                                            Mk(D - 1, I + 1)])
         end,
    [Mk(8, I) || I <- lists:seq(1, 10)].

%% From real terms: t_from_term over deterministic pseudo-random
%% Erlang terms (correctness corpus).
from_terms() ->
    rand:seed(exsss, {1, 2, 3}),
    [erl_types:t_from_term(rand_term(5)) || _ <- lists:seq(1, 500)].

rand_term(0) ->
    case rand:uniform(4) of
        1 -> rand:uniform(1000);
        2 -> list_to_atom("t" ++ integer_to_list(rand:uniform(50)));
        3 -> rand:uniform(1 bsl 70);          % bignum
        4 -> []
    end;
rand_term(D) ->
    case rand:uniform(5) of
        1 -> list_to_tuple([rand_term(D - 1) || _ <- lists:seq(1, rand:uniform(6))]);
        2 -> [rand_term(D - 1) || _ <- lists:seq(1, rand:uniform(4))];
        3 -> #{rand_term(0) => rand_term(D - 1)};
        4 -> rand_term(0);
        5 -> {rand_term(D - 1), rand_term(D - 1)}
    end.

corpora() ->
    [{leafy, leafy()}, {mixed, mixed()}, {deep, deep()}].

%% --- benchmark -----------------------------------------------------

bench(Reps) ->
    Corpora = corpora(),
    [begin
         Times = [begin
                      T0 = erlang:monotonic_time(microsecond),
                      run(C, 200),
                      erlang:monotonic_time(microsecond) - T0
                  end || _ <- lists:seq(1, Reps)],
         Sorted = lists:sort(Times),
         Median = lists:nth(max(1, Reps div 2), Sorted),
         Min = hd(Sorted),
         io:format("~-8s median ~7b us   min ~7b us~n", [Name, Median, Min]),
         {Name, Median, Min}
     end || {Name, C} <- Corpora].

run(_Corpus, 0) -> ok;
run(Corpus, N) ->
    lists:foreach(fun(T) -> _ = erl_types:t_limit(T, ?K) end, Corpus),
    run(Corpus, N - 1).

%% --- correctness ---------------------------------------------------

check() ->
    Corpus = from_terms() ++ leafy() ++ mixed() ++ deep(),
    Results = [erl_types:t_limit(T, K)
               || T <- Corpus, K <- [-1, 0, 1, 2, 3, 6, 50]],
    Hash = erlang:phash2(Results, 1 bsl 32),
    io:format("corpus size ~b, result hash ~b~n", [length(Results), Hash]),
    Hash.
