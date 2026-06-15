-module(scanbench).
-export([go/0, plain/2, digit/2]).
%% naive single-byte scanners (the A1 target shape)
plain(<<B, R/bits>>, N) when B >= 32, B =< 126, B =/= $", B =/= $\\ -> plain(R, N+1);
plain(_, N) -> N.
digit(<<B, R/bits>>, N) when B >= $0, B =< $9 -> digit(R, N+1);
digit(_, N) -> N.
go() ->
    Plain = binary:copy(<<"abcdefghij0123456789ABCDEFGHIJklmnopqrst">>, 26214), %% ~1MB ascii
    Digits = binary:copy(<<"0123456789">>, 104857), %% ~1MB digits
    io:format("plain ~p bytes, digits ~p bytes~n",[byte_size(Plain),byte_size(Digits)]),
    N = 500,
    T = fun(F,Arg,Tag) ->
        T0 = erlang:monotonic_time(microsecond),
        R = lists:foldl(fun(_,_) -> scanbench:F(Arg,0) end, 0, lists:seq(1,N)),
        T1 = erlang:monotonic_time(microsecond),
        Us = (T1-T0)/N,
        io:format("  ~s: ~.1f us  (~.2f GB/s)  result=~p~n",
                  [Tag, Us, byte_size(Arg)/Us/1000, R]),
        Us
    end,
    io:format("=== current module (stock, no scan_loop) ===~n"),
    Sp = T(plain,Plain,"plain"), Sd = T(digit,Digits,"digit"),
    halt({Sp,Sd}).
