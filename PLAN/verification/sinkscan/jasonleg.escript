#!/usr/bin/env escript
%%! -mode(compile)
%% GALLOC-weighted Jason leg: per-function tier shares for the exact
%% functions named in GALLOC_RESULTS.md (thin-Bandit table), weighted by
%% their measured heap-word shares.
main([Path]) ->
    {ok, B} = file:read_file(Path),
    {Sites, _} = binary_to_term(B),
    %% {Weight% of total heap, MFA}
    Rows = [{9.98, {'Elixir.Jason.Decoder', object, 6}},
            {6.16, {'Elixir.Jason.Decoder', key, 6}},
            {5.03, {'Elixir.Jason.Decoder', array, 6}},
            {3.01, {'Elixir.Jason.Decoder', key, 5}},
            {2.33, {'Elixir.Jason.Decoder', value, 5}},
            {1.62, {'Elixir.Jason.Encode', encode_string, 2}},
            {1.41, {'Elixir.Jason.Encode', list_loop, 3}},
            {1.25, {'Elixir.Jason.Encode', escape_json_chunk, 5}}],
    io:format("~n~-44s ~7s ~5s | ~6s ~6s ~6s | sites(t1/t2/e) bins~n",
              ["GALLOC function", "weight%", "exW", "t1%", "t2%", "esc%"]),
    {SumW, SumT1, SumT2} =
        lists:foldl(
          fun({W, MFA}, {AW, A1, A2}) ->
                  Ss = [S || S <- Sites, element(2, S) =:= MFA],
                  {N1, N2, NE, W1, W2, WE, B1, B2, BE} = stats(Ss),
                  T = W1 + W2 + WE,
                  {S1, S2} = case T of 0 -> {0.0, 0.0}; _ -> {W1 / T, W2 / T} end,
                  {_, F, A} = MFA,
                  io:format("~-44s ~7.2f ~5w | ~6s ~6s ~6s | ~p/~p/~p bins=~p/~p/~p~n",
                            [io_lib:format("~s:~p/~p", [mshort(MFA), F, A]), W, T,
                             pc(S1), pc(S2), pc(1 - S1 - S2), N1, N2, NE, B1, B2, BE]),
                  {AW + W, A1 + W * S1, A2 + W * S2}
          end, {0.0, 0.0, 0.0}, Rows),
    io:format("~ncovered weight = ~.2f% of thin-Bandit heap volume~n", [SumW]),
    io:format("volume-weighted within covered: t1=~s t2=~s t1+t2=~s~n",
              [pc(SumT1 / SumW), pc(SumT2 / SumW), pc((SumT1 + SumT2) / SumW)]),
    io:format("as share of TOTAL heap volume: t1=~.2f%% t2=~.2f%% t1+t2=~.2f%%~n",
              [SumT1, SumT2, SumT1 + SumT2]).

mshort({M, _, _}) ->
    case string:split(atom_to_list(M), "Elixir.", all) of
        [_, R] -> R;
        _ -> atom_to_list(M)
    end.

stats(Ss) ->
    lists:foldl(
      fun({asite, _, _, Cat, W, Tier, _}, {N1, N2, NE, W1, W2, WE, B1, B2, BE}) ->
              {N1a, N2a, NEa} = case Tier of
                                    t1 -> {N1 + 1, N2, NE};
                                    t2 -> {N1, N2 + 1, NE};
                                    esc -> {N1, N2, NE + 1}
                                end,
              Exact = is_integer(W) andalso lists:member(Cat, [tuple, cons, record]),
              {W1a, W2a, WEa} = case {Exact, Tier} of
                                    {true, t1} -> {W1 + W, W2, WE};
                                    {true, t2} -> {W1, W2 + W, WE};
                                    {true, esc} -> {W1, W2, WE + W};
                                    _ -> {W1, W2, WE}
                                end,
              {B1a, B2a, BEa} = case {Cat, Tier} of
                                    {bin, t1} -> {B1 + 1, B2, BE};
                                    {bin, t2} -> {B1, B2 + 1, BE};
                                    {bin, esc} -> {B1, B2, BE + 1};
                                    _ -> {B1, B2, BE}
                                end,
              {N1a, N2a, NEa, W1a, W2a, WEa, B1a, B2a, BEa}
      end, {0, 0, 0, 0, 0, 0, 0, 0, 0}, Ss).

pc(F) -> lists:flatten(io_lib:format("~5.1f%", [100 * F])).
