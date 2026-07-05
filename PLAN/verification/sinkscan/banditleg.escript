#!/usr/bin/env escript
%%! -mode(compile)
%% The full thin-Bandit GALLOC-weighted leg (M0.2 headline table).
%% Combines three dumps: OTP corpus (sink_corpus.eterm), Jason
%% (sink_jason.eterm), Elixir stdlib (sink_exstdlib.eterm). Every GALLOC
%% row >= 0.9% of the thin-Bandit heap volume is mapped to its classified
%% function(s) or to a labelled non-SSA bucket:
%%   bif      — allocation happens inside a C BIF (no SSA site; not
%%              addressable by P5 sinking)
%%   runtime  — port/driver/GC-internal allocation (same)
%% Usage: escript banditleg.escript <corpus> <jason> <exstdlib>
main([CorpusP, JasonP, ExP]) ->
    Corpus = load(CorpusP), Jason = load(JasonP), Ex = load(ExP),
    Rows =
        [{46.05, "maps:from_list/1",            bif, []},
         {9.98,  "Jason.Decoder:object/6",      Jason, [{'Elixir.Jason.Decoder', object, 6}]},
         {6.16,  "Jason.Decoder:key/6",         Jason, [{'Elixir.Jason.Decoder', key, 6}]},
         {5.03,  "Jason.Decoder:array/6",       Jason, [{'Elixir.Jason.Decoder', array, 6}]},
         {4.10,  "String.Unicode:upcase/3",     Ex, [{'Elixir.String.Unicode', upcase, 3}]},
         {3.61,  "prim_inet:send/4",            runtime, []},
         {3.01,  "Jason.Decoder:key/5",         Jason, [{'Elixir.Jason.Decoder', key, 5}]},
         {2.33,  "Jason.Decoder:value/5",       Jason, [{'Elixir.Jason.Decoder', value, 5}]},
         {1.62,  "Jason.Encode:encode_string/2",Jason, [{'Elixir.Jason.Encode', encode_string, 2}]},
         {1.48,  "lists:reverse/1",             Corpus, [{lists, reverse, 1}]},
         {1.41,  "Jason.Encode:list_loop/3",    Jason, [{'Elixir.Jason.Encode', list_loop, 3}]},
         {1.25,  "Jason.Encode:escape_json_chunk/5", Jason, [{'Elixir.Jason.Encode', escape_json_chunk, 5}]},
         {1.13,  "erts_internal:garbage_collect/1", runtime, []},
         {1.11,  "String:downcase_ascii/1",     Ex, [{'Elixir.String', downcase_ascii, 1}]},
         {0.93,  "Enum.map (lists:map/2)",      Corpus, [{lists, map, 2}]}],
    io:format("~n~-34s ~7s ~5s | ~6s ~6s ~6s | sites(t1/t2/e) bins(t1/t2/e)~n",
              ["GALLOC row", "weight%", "exW", "t1%", "t2%", "esc%"]),
    {SumW, SumT1, SumT2} =
        lists:foldl(
          fun({W, Label, Src, MFAs}, {AW, A1, A2}) ->
                  {S1, S2} =
                      case Src of
                          bif ->
                              io:format("~-34s ~7.2f ~5s | ~6s~n",
                                        [Label, W, "-", "BIF-internal (not SSA-sinkable)"]),
                              {0.0, 0.0};
                          runtime ->
                              io:format("~-34s ~7.2f ~5s | ~6s~n",
                                        [Label, W, "-", "runtime-internal (port/GC path)"]),
                              {0.0, 0.0};
                          Sites ->
                              Ss = [S || S <- Sites, lists:member(element(2, S), MFAs)],
                              {N1, N2, NE, W1, W2, WE, B1, B2, BE} = stats(Ss),
                              T = W1 + W2 + WE,
                              {X1, X2} = case T of 0 -> {0.0, 0.0}; _ -> {W1 / T, W2 / T} end,
                              io:format("~-34s ~7.2f ~5w | ~6s ~6s ~6s | ~p/~p/~p bins=~p/~p/~p~n",
                                        [Label, W, T, pc(X1), pc(X2), pc(1 - X1 - X2),
                                         N1, N2, NE, B1, B2, BE]),
                              {X1, X2}
                      end,
                  {AW + W, A1 + W * S1, A2 + W * S2}
          end, {0.0, 0.0, 0.0}, Rows),
    io:format("~ncovered weight = ~.2f% of thin-Bandit heap volume (rows >= 0.9%)~n", [SumW]),
    io:format("volume-weighted within covered: t1=~s t2=~s t1+t2=~s~n",
              [pc(SumT1 / SumW), pc(SumT2 / SumW), pc((SumT1 + SumT2) / SumW)]),
    io:format("as share of TOTAL heap volume: t1=~.3f%% t2=~.3f%% t1+t2=~.3f%%~n",
              [SumT1, SumT2, SumT1 + SumT2]).

load(P) -> {ok, B} = file:read_file(P), {Sites, _} = binary_to_term(B), Sites.

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
