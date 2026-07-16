%% SPDX-License-Identifier: Apache-2.0
%% Copyright Ericsson AB 2026. All Rights Reserved.
%%
%% Minimal hot tier-2 loop, to demonstrate the T2 perf-symbol hook:
%% inner/2 is a self-tail-recursive arithmetic loop that T2 installs and
%% that dominates run time, so `perf` should resolve its samples to
%% "$T2:t2demo:inner/2" once beamasm_t2_register_perf names the blob.
-module(t2demo).
-export([run/0]).

run() ->
    _ = spin(50),                                  % warm: force T2 install
    End = erlang:monotonic_time(millisecond) + 15000,
    Acc = drive(End, 0),
    io:format("acc=~p~n", [Acc]),
    halt().

drive(End, Acc) ->
    Acc1 = inner(2000000, Acc),
    case erlang:monotonic_time(millisecond) < End of
        true -> drive(End, Acc1);
        false -> Acc1
    end.

spin(0) -> 0;
spin(K) -> _ = inner(2000000, 0), spin(K - 1).

inner(0, Acc) -> Acc;
inner(N, Acc) -> inner(N - 1, (Acc * 1103515245 + N + 12345) band 16#FFFFFFFF).
