-module(scanfuzz).
-export([go/0, plain/2, digit/2, alpha/2, ctrl/2]).

%% Four recognizer-shaped scanners exercising every SWAR variant:
%%  plain - notset 0x20..0x7F minus {",\}   (hi==0x7F, exclusions)
%%  digit - range  0x30..0x39                (hi<0x7F, upper term)
%%  alpha - range  0x61..0x7A                (hi<0x7F, upper term)
%%  ctrl  - range  0x00..0x1F                (lo==0,   upper term)
plain(<<B, R/bits>>, N) when B >= 32, B =< 126, B =/= $", B =/= $\\ -> plain(R, N+1);
plain(_, N) -> N.
digit(<<B, R/bits>>, N) when B >= $0, B =< $9 -> digit(R, N+1);
digit(_, N) -> N.
alpha(<<B, R/bits>>, N) when B >= $a, B =< $z -> alpha(R, N+1);
alpha(_, N) -> N.
ctrl(<<B, R/bits>>, N) when B >= 0, B =< 16#1F -> ctrl(R, N+1);
ctrl(_, N) -> N.

go() ->
    rand:seed(exsss, {11,22,33}),
    %% stock (scalar) reference results captured first
    Cases = [{F, B} || F <- [plain, digit, alpha, ctrl],
                       _ <- lists:seq(1, 4000),
                       B <- [rndbin()]],
    Ref = [{F, B, scanfuzz:F(B, 0)} || {F, B} <- Cases],
    %% recompile WITH scan_loop -> bs_scan + SWAR codegen
    {ok, scanfuzz, Bin} =
        compile:file("/tmp/scanfuzz.erl", [binary, scan_loop, return_errors]),
    code:purge(scanfuzz),
    {module, scanfuzz} = code:load_binary(scanfuzz, "scanfuzz.beam", Bin),
    Bad = [{F, B, Exp, scanfuzz:F(B, 0)}
           || {F, B, Exp} <- Ref, scanfuzz:F(B, 0) =/= Exp],
    io:format("cases: ~p  mismatches: ~p~n", [length(Ref), length(Bad)]),
    [io:format("DIFF ~p ~p exp ~p got ~p~n", [F, B, E, G])
     || {F, B, E, G} <- lists:sublist(Bad, 8)],
    halt().

%% random length 0..40, bytes spanning all classes incl >=0x80 and stops
rndbin() ->
    L = rand:uniform(41) - 1,
    list_to_binary([rndbyte() || _ <- lists:seq(1, L)]).
rndbyte() ->
    case rand:uniform(8) of
        1 -> rand:uniform(256) - 1;      %% any byte 0..255 (incl >=0x80)
        2 -> $0 + rand:uniform(10) - 1;  %% digit
        3 -> $a + rand:uniform(26) - 1;  %% lower alpha
        4 -> $A + rand:uniform(26) - 1;  %% upper alpha
        5 -> $";                          %% plain stop
        6 -> $\\;                         %% plain stop
        7 -> rand:uniform(32) - 1;       %% control
        _ -> 32 + rand:uniform(95) - 1   %% printable
    end.
