-module(lex).
-export([go/0, scan_line/2, scan_digits/2, count_lines/1, numbers/1]).

%% ---- recognized scan idioms (single byte-class range, hi<0x7F -> SWAR) ----
scan_line(<<B, R/bits>>, N) when B >= 32, B =< 126 -> scan_line(R, N + 1);
scan_line(_, N) -> N.

scan_digits(<<B, R/bits>>, N) when B >= $0, B =< $9 -> scan_digits(R, N + 1);
scan_digits(_, N) -> N.

%% ---- workload 1: wc-like pass over a printable-ASCII log ----
%% Count lines and printable bytes. Each line is a long printable run
%% terminated by \n -> scan_line does (almost) all the work.
count_lines(Bin) -> count_lines(Bin, 0, 0).
count_lines(<<>>, L, B) -> {L, B};
count_lines(Bin, L, B) ->
    N = scan_line(Bin, 0),
    case Bin of
        <<_:N/binary, $\n, Rest/bits>> -> count_lines(Rest, L + 1, B + N);
        <<_:N/binary>> -> {L, B + N}
    end.

%% ---- workload 2: tokenize a numeric data stream ----
%% Count number tokens + total digits. scan_digits finds each run;
%% the token sub-binary is skipped O(1), no list/value materialised.
numbers(Bin) -> numbers(Bin, 0, 0).
numbers(<<>>, C, S) -> {C, S};
numbers(Bin, C, S) ->
    case scan_digits(Bin, 0) of
        0 ->
            <<_, Rest/bits>> = Bin,
            numbers(Rest, C, S);
        N ->
            <<_:N/binary, Rest/bits>> = Bin,
            numbers(Rest, C + 1, S + N)
    end.

%% ---- data generation (realistic shapes) ----
gen_log(Lines) ->
    rand:seed(exsss, {4, 5, 6}),
    iolist_to_binary(
      [[printable(80 + rand:uniform(80)), $\n] || _ <- lists:seq(1, Lines)]).

gen_nums(Count) ->
    rand:seed(exsss, {7, 8, 9}),
    iolist_to_binary(
      [[digits(12 + rand:uniform(8)), $\n] || _ <- lists:seq(1, Count)]).

%% serialized big integers (crypto/financial/scientific): long digit runs
gen_big(Count) ->
    rand:seed(exsss, {1, 1, 2}),
    iolist_to_binary(
      [[digits(200 + rand:uniform(600)), $\n] || _ <- lists:seq(1, Count)]).

printable(L) -> << <<(32 + rand:uniform(94))>> || _ <- lists:seq(1, L) >>.
digits(L) -> << <<($0 + rand:uniform(10) - 1)>> || _ <- lists:seq(1, L) >>.

%% ---- driver ----
go() ->
    Log  = gen_log(40000),
    Nums = gen_nums(180000),
    Big  = gen_big(6000),
    io:format("log ~.2f MB, nums ~.2f MB, big ~.2f MB~n",
              [byte_size(Log) / 1.0e6, byte_size(Nums) / 1.0e6,
               byte_size(Big) / 1.0e6]),
    Iters = 200,
    Time = fun(F, Arg) ->
        R0 = F(Arg),
        T0 = erlang:monotonic_time(microsecond),
        _  = lists:foldl(fun(_, _) -> F(Arg) end, 0, lists:seq(1, Iters)),
        T1 = erlang:monotonic_time(microsecond),
        {(T1 - T0) / Iters, R0}
    end,
    Lines = fun(B) -> lex:'count_lines'(B) end,
    Toks  = fun(B) -> lex:'numbers'(B) end,
    %% stock (no scan_loop), already loaded
    {LiUs0, LiR} = Time(Lines, Log),
    {NuUs0, NuR} = Time(Toks, Nums),
    {BiUs0, BiR} = Time(Toks, Big),
    %% recompile lex WITH scan_loop -> bs_scan + SWAR
    {ok, lex, Bin} = compile:file("/tmp/lex.erl",
                                  [binary, scan_loop, return_errors]),
    code:purge(lex),
    {module, lex} = code:load_binary(lex, "lex.beam", Bin),
    {LiUs1, LiR2} = Time(Lines, Log),
    {NuUs1, NuR2} = Time(Toks, Nums),
    {BiUs1, BiR2} = Time(Toks, Big),
    LiR = LiR2, NuR = NuR2, BiR = BiR2,  %% results must be identical
    GBs = fun(B, Us) -> byte_size(B) / Us / 1000 end,
    io:format("~-12s ~10s ~10s ~9s~n", ["workload", "stock us", "scan us", "speedup"]),
    io:format("~-12s ~9.1f ~9.1f ~8.3fx  (~.2f -> ~.2f GB/s)  ~p~n",
              ["log wc", LiUs0, LiUs1, LiUs0 / LiUs1,
               GBs(Log, LiUs0), GBs(Log, LiUs1), LiR]),
    io:format("~-12s ~9.1f ~9.1f ~8.3fx  (~.2f -> ~.2f GB/s)  ~p~n",
              ["num short", NuUs0, NuUs1, NuUs0 / NuUs1,
               GBs(Nums, NuUs0), GBs(Nums, NuUs1), NuR]),
    io:format("~-12s ~9.1f ~9.1f ~8.3fx  (~.2f -> ~.2f GB/s)  ~p~n",
              ["num big", BiUs0, BiUs1, BiUs0 / BiUs1,
               GBs(Big, BiUs0), GBs(Big, BiUs1), BiR]),
    halt().
