-module(sb).
-export([go/0]).

go() ->
    %% Two ~1MB inputs matching the A1 recognizer shapes.
    Plain  = binary:copy(<<"abcdefghij0123456789ABCDEFGHIJklmnopqrst">>, 26214),
    Digits = binary:copy(<<"0123456789">>, 104857),
    io:format("plain ~p bytes, digits ~p bytes~n",
              [byte_size(Plain), byte_size(Digits)]),
    N = 500,
    Time = fun(F, Arg) ->
        R0 = scanbench:F(Arg, 0),
        T0 = erlang:monotonic_time(microsecond),
        _  = lists:foldl(fun(_, _) -> scanbench:F(Arg, 0) end, 0, lists:seq(1, N)),
        T1 = erlang:monotonic_time(microsecond),
        {(T1 - T0) / N, R0}
    end,
    %% stock scanbench (no scan_loop) is already loaded via c:c/1
    {SpStock, RP} = Time(plain, Plain),
    {SdStock, RD} = Time(digit, Digits),
    %% recompile scanbench WITH scan_loop -> bs_scan + SWAR codegen
    {ok, scanbench, Bin} =
        compile:file("/tmp/scanbench.erl",
                     [binary, {d, x}, scan_loop, return_errors]),
    code:purge(scanbench),
    {module, scanbench} = code:load_binary(scanbench, "scanbench.beam", Bin),
    {SpScan, RP2} = Time(plain, Plain),
    {SdScan, RD2} = Time(digit, Digits),
    RP = RP2, RD = RD2,  %% results must match
    GBs = fun(B, Us) -> byte_size(B) / Us / 1000 end,
    io:format("~-7s ~10s ~10s ~9s~n", ["scan", "stock us", "scan us", "speedup"]),
    io:format("~-7s ~9.1f ~9.1f ~8.3fx  (~.2f -> ~.2f GB/s)~n",
              ["plain", SpStock, SpScan, SpStock / SpScan,
               GBs(Plain, SpStock), GBs(Plain, SpScan)]),
    io:format("~-7s ~9.1f ~9.1f ~8.3fx  (~.2f -> ~.2f GB/s)~n",
              ["digit", SdStock, SdScan, SdStock / SdScan,
               GBs(Digits, SdStock), GBs(Digits, SdScan)]),
    halt().
