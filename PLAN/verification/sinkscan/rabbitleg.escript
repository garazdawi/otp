#!/usr/bin/env escript
%%! -mode(compile)
%% GALLOC-weighted RabbitMQ leg: tier shares for the functions named in
%% GALLOC_RESULTS.md (RabbitMQ MQTT table), weighted by measured heap-word
%% shares. Where GALLOC names a family (e.g. mc_mqtt:protocol_state+init),
%% the weight is applied to the aggregate.  parse_remaining_len is /3 in
%% v4.3.1 source (GALLOC frame said /5 — JIT frame arity; substitution
%% labelled in SINK_RESULTS).
main([Path]) ->
    {ok, B} = file:read_file(Path),
    {Sites, _} = binary_to_term(B),
    Rows =
        [{9.38, "trie_match_try/9",      [{rabbit_db_topic_exchange, trie_match_try, 9}]},
         {6.40, "mqtt parse_packet/4",   [{rabbit_mqtt_packet, parse_packet, 4}]},
         {4.42, "mqtt parse/2",          [{rabbit_mqtt_packet, parse, 2}]},
         {3.13, "mqtt parse_remaining_len", [{rabbit_mqtt_packet, parse_remaining_len, 3}]},
         {5.62, "mc:set_annotation/3",   [{mc, set_annotation, 3}]},
         {2.03, "mc_mqtt:protocol_state+init", [{mc_mqtt, protocol_state, 2}, {mc_mqtt, init, 1}]},
         {4.48, "qos0_queue:deliver/3",  [{rabbit_mqtt_qos0_queue, deliver, 3}]},
         {3.44, "queue_type deliver0-fun", [{rabbit_queue_type, '-deliver0/4-fun-4-', 3}]},
         {2.46, "trie_match/7",          [{rabbit_db_topic_exchange, trie_match, 7}]},
         {2.17, "topic match/3",         [{rabbit_db_topic_exchange, match, 3}]}],
    io:format("~n~-34s ~7s ~5s | ~6s ~6s ~6s | sites(t1/t2/e) bins(t1/t2/e)~n",
              ["GALLOC function", "weight%", "exW", "t1%", "t2%", "esc%"]),
    {SumW, SumT1, SumT2} =
        lists:foldl(
          fun({W, Label, MFAs}, {AW, A1, A2}) ->
                  Ss = [S || S <- Sites, lists:member(element(2, S), MFAs)],
                  {N1, N2, NE, W1, W2, WE, B1, B2, BE} = stats(Ss),
                  T = W1 + W2 + WE,
                  {S1, S2} = case T of 0 -> {0.0, 0.0}; _ -> {W1 / T, W2 / T} end,
                  io:format("~-34s ~7.2f ~5w | ~6s ~6s ~6s | ~p/~p/~p bins=~p/~p/~p~n",
                            [Label, W, T, pc(S1), pc(S2), pc(1 - S1 - S2),
                             N1, N2, NE, B1, B2, BE]),
                  {AW + W, A1 + W * S1, A2 + W * S2}
          end, {0.0, 0.0, 0.0}, Rows),
    io:format("~ncovered weight = ~.2f% of RabbitMQ-MQTT heap volume~n", [SumW]),
    io:format("volume-weighted within covered: t1=~s t2=~s t1+t2=~s~n",
              [pc(SumT1 / SumW), pc(SumT2 / SumW), pc((SumT1 + SumT2) / SumW)]),
    io:format("as share of TOTAL heap volume: t1=~.2f%% t2=~.2f%% t1+t2=~.2f%%~n",
              [SumT1, SumT2, SumT1 + SumT2]),
    %% whole-module structural view for the six modules
    io:format("~n-- whole-module structural (exact words) --~n"),
    [mline(M, Sites) || M <- [rabbit_db_topic_exchange, rabbit_mqtt_packet, mc,
                              mc_mqtt, rabbit_mqtt_qos0_queue, rabbit_queue_type]].

mline(M, Sites) ->
    Ss = [S || S <- Sites, element(1, element(2, S)) =:= M],
    {N1, N2, NE, W1, W2, WE, B1, B2, BE} = stats(Ss),
    T = W1 + W2 + WE,
    {S1, S2} = case T of 0 -> {0.0, 0.0}; _ -> {W1 / T, W2 / T} end,
    io:format("  ~-28s n=~4w exW=~5w t1=~s t2=~s esc=~s bins=~p/~p/~p~n",
              [M, N1 + N2 + NE, T, pc(S1), pc(S2), pc(1 - S1 - S2), B1, B2, BE]).

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
