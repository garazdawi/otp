%% gc_sites — attribute live tagged allocations (binaries, heap, ETS) to
%% the Erlang code that created them, via +M<S>atags code + instrument.
%% Names the module:fun/arity sites producing the binary garbage that
%% drives the bin_vheap major-GC storm (PLAN/verification/GC_RESULTS.md).
-module(gc_sites).
-export([run/1]).

run(DurMs) ->
    %% Sample a few times under load and aggregate per {MFA,Type} bytes.
    N = max(1, DurMs div 1500),
    Acc = lists:foldl(fun(_, A) -> timer:sleep(1500), sample(A) end, #{}, lists:seq(1, N)),
    Ranked = lists:reverse(lists:keysort(2, [{K, B} || {K, B} <- maps:to_list(Acc)])),
    io:format("~n=== live tagged allocations by origin (avg bytes over ~b samples) ===~n", [N]),
    io:format("~-58s ~-10s ~s~n", ["origin (MFA / pid)", "type", "bytes"]),
    [io:format("~-58s ~-10s ~14b~n", [fmt(O), T, B div N])
     || {{O, T}, B} <- lists:sublist(Ranked, 30)],
    Ranked.

sample(Acc) ->
    case instrument:allocations(#{flags => [per_mfa]}) of
        {ok, {Start, _Unscanned, Map}} ->
            maps:fold(
              fun(Origin, ByType, A1) ->
                      maps:fold(
                        fun(Type, Hist, A2) ->
                                B = hist_bytes(Hist, Start),
                                maps:update_with({Origin, Type},
                                                 fun(X) -> X + B end, B, A2)
                        end, A1, ByType)
              end, Acc, Map);
        Err ->
            io:format("instrument error: ~p~n", [Err]), Acc
    end.

%% Histogram is a tuple of counts; bucket i upper bound ~ Start*2^i.
hist_bytes(Hist, Start) when is_tuple(Hist) ->
    lists:sum([element(I, Hist) * (Start bsl (I - 1))
               || I <- lists:seq(1, tuple_size(Hist))]);
hist_bytes(_, _) -> 0.

fmt({M, F, A}) -> lists:flatten(io_lib:format("~p:~p/~p", [M, F, A]));
fmt(Other) -> lists:flatten(io_lib:format("~p", [Other])).
