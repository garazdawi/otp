

main([OtpVsn, WinPathFile]) ->
    try
        {ok, Bin} = file:read_file(WinPathFile),
        <<"OTP-", _/binary>> = Bin,
        %% VIProductVersion only accepts exactly four integers, so a version
        %% with more components than that has to be truncated. This mirrors
        %% what erts/etc/common/Makefile.in does for the .rc files.
        case [list_to_integer(Str) || Str <- string:lexemes(OtpVsn, ".")] of
            [A,B,C,D|_] -> io:format("~w.~w.~w.~w~n", [A,B,C,D]);
            [A,B,C] -> io:format("~w.~w.~w.0~n",[A,B,C]);
            [A,B] -> io:format("~w.~w.0.0~n",[A,B]);
            [A] -> io:format("~w.0.0.0~n",[A])
        end
    catch _:_R:_ST -> %% release candidate or development branch set fake version as 0.0.0.0
            %% io:format("Err: ~p ~p~n ~p~n",[_R,_ST, WinPathFile]),
            io:format("0.0.0.0~n")
    end.
