#!/usr/bin/env escript
%%! +S2
main(_) ->
    %% Build a small PLT: a real dialyzer workload (erl_types/erl_bif_types,
    %% dialyzer_* and stdlib helpers run hot).
    Files = filelib:wildcard(filename:join(code:lib_dir(compiler), "ebin/*.beam")),
    cprof:start(),
    try
        dialyzer:run([{analysis_type, plt_build},
                      {files, Files},
                      {output_plt, "/tmp/census_dialyzer.plt"}])
    catch C:R -> io:format("dialyzer: ~p:~p~n", [C, R])
    end,
    cprof:pause(),
    {AllTotal, Mods} = cprof:analyse(),
    {ok, Fd} = file:open("/tmp/census_dialyzer_counts.tsv", [write]),
    lists:foreach(fun({Mod, _MTot, Funs}) ->
        lists:foreach(fun({{M, F, A}, N}) when N > 0 ->
                              io:format(Fd, "~s\t~s/~b\t~b~n", [M, F, A, N]);
                         (_) -> ok
                      end, Funs)
    end, Mods),
    file:close(Fd),
    io:format("total calls counted: ~b~n", [AllTotal]).
