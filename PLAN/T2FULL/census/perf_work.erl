%% SPDX-License-Identifier: Apache-2.0
%% Copyright Ericsson AB 2026. All Rights Reserved.
%%
%% Bare workload driver for perf sampling cross-check (PLAN/T2FULL/22).
%% Runs ONE census leg in a tight timed loop with NO tracing/instrument,
%% so a sampling profiler (linux perf, +JPperf symbols) sees the true
%% own-time distribution -- to be compared against the call_time census
%% (which per-call instrumentation biases toward small hot functions).
%%
%%   perf record -F 1999 -o /tmp/<leg>.data -- \
%%     erl +JPperf true -noshell -pa /tmp -run perf_work <leg>
-module(perf_work).
-export([json/0, mapwl/0, compiler/0]).

-define(SECS, 25).

json() ->
    Doc = json_doc(),
    Bin = iolist_to_binary(json:encode(Doc)),
    run_for(fun() -> _ = json:encode(Doc), _ = json:decode(Bin) end).

mapwl() ->
    D = mapwl:setup(),
    run_for(fun() -> mapwl:work(D) end).

compiler() ->
    _ = application:load(compiler),
    Src = filename:join(code:lib_dir(stdlib), "src/proplists.erl"),
    run_for(fun() ->
                    _ = compile:file(Src, [binary, return_errors,
                                           {warn_format, 0}])
            end).

run_for(F) ->
    End = erlang:monotonic_time(millisecond) + ?SECS * 1000,
    N = loop(End, F, 0),
    io:format("iters=~p~n", [N]),
    halt().

loop(End, F, N) ->
    case erlang:monotonic_time(millisecond) < End of
        true -> _ = F(), loop(End, F, N + 1);
        false -> N
    end.

json_doc() ->
    #{<<"id">> => 12345,
      <<"name">> => <<"a moderately sized record name string">>,
      <<"active">> => true, <<"score">> => 98.6,
      <<"tags">> => [<<"alpha">>, <<"beta">>, <<"gamma">>, <<"delta">>],
      <<"items">> =>
          [#{<<"k">> => N,
             <<"v">> => <<"value ", (integer_to_binary(N))/binary>>,
             <<"ok">> => (N rem 2) =:= 0} || N <- lists:seq(1, 40)],
      <<"nested">> =>
          #{<<"a">> => #{<<"b">> => #{<<"c">> => [1, 2, 3, 4, 5]}},
            <<"list">> => [true, false, null, 1.5, <<"s">>]}}.
