%% T2 MVP benchmark — see ../T2_mvp.md
%%
%% Hot loop: walk a list of {Amount, Fee} 2-tuples, accumulate
%% Net = sum(Amount - Fee). Three real BEAM operations per
%% iteration of total/2:
%%   1. cons-cell decomposition (head/tail)
%%   2. tuple destructuring (extract A, F from a 2-tuple)
%%   3. non-tail call to diff/2
%%
%% The t2_assume_smallints attribute is added in step 3 of the
%% sequencing; without it this is the pure T1 baseline.
-module(t2_mvp).
-export([run/1, total/2, diff/2, mk_txns/2, time_run/1, time_run/2,
         time_all_sideexit/2]).

run(N) ->
    Txns = mk_txns(N, []),
    total(Txns, 0).

%% Setup. Not on the hot path.
mk_txns(0, Acc) -> Acc;
mk_txns(N, Acc) -> mk_txns(N - 1, [{N, N rem 7} | Acc]).

%% Hot loop.
total([], Net) -> Net;
total([{A, F} | Rest], Net) ->
    total(Rest, Net + diff(A, F)).

diff(A, F) -> A - F.

%% Measurement harness. Excludes mk_txns (setup) from the timed region.
time_run(N) -> time_run(N, 100).

time_run(N, Iters) ->
    Txns = mk_txns(N, []),
    Times = [ begin
                  T0 = erlang:monotonic_time(nanosecond),
                  _  = total(Txns, 0),
                  T1 = erlang:monotonic_time(nanosecond),
                  T1 - T0
              end || _ <- lists:seq(1, Iters) ],
    Sorted = lists:sort(Times),
    #{
        n        => N,
        iters    => Iters,
        median   => lists:nth(Iters div 2, Sorted),
        min      => hd(Sorted),
        max      => lists:last(Sorted),
        ns_per_iter => lists:nth(Iters div 2, Sorted) / N
    }.

%% Worst-case side-exit cost: every element's amount is a bignum, so
%% T2's combined fixnum check fails on every iteration and T1 handles
%% each iter. This is the steady-state cost of a wholly profile-
%% incorrect T2 compilation. Net stays bignum throughout but is never
%% read by T2's arithmetic path (the (A & F) guard fires before
%% reaching it), so the result remains correct.
time_all_sideexit(N, Iters) ->
    Big = 1 bsl 60,
    BigList = [ {Big + I, I rem 7} || I <- lists:seq(1, N) ],
    Times = [ begin
                  T0 = erlang:monotonic_time(nanosecond),
                  _  = total(BigList, 0),
                  T1 = erlang:monotonic_time(nanosecond),
                  T1 - T0
              end || _ <- lists:seq(1, Iters) ],
    Sorted = lists:sort(Times),
    #{
        n        => N,
        iters    => Iters,
        median   => lists:nth(Iters div 2, Sorted),
        min      => hd(Sorted),
        max      => lists:last(Sorted),
        ns_per_iter => lists:nth(Iters div 2, Sorted) / N
    }.
