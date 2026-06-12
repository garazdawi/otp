#!/usr/bin/env escript
%%! +S10
main(Args) ->
    Files = filelib:wildcard(filename:join(code:lib_dir(compiler), "ebin/*.beam")),
    Opts0 = [{analysis_type, plt_build}, {files, Files},
             {output_plt, "/tmp/g3cpu.plt"}],
    Opts = case Args of
               ["1"] -> [{jobs, 1} | Opts0];
               _ -> Opts0
           end,
    {CpuT0, _} = erlang:statistics(runtime),
    T0 = erlang:monotonic_time(millisecond),
    dialyzer:run(Opts),
    T1 = erlang:monotonic_time(millisecond),
    {CpuT1, _} = erlang:statistics(runtime),
    io:format("wall ~b ms   cpu ~b ms~n", [T1 - T0, CpuT1 - CpuT0]).
