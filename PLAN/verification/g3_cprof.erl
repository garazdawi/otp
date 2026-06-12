#!/usr/bin/env escript
%%! +S2
main(_) ->
    Files = filelib:wildcard(filename:join(code:lib_dir(compiler), "ebin/*.beam")),
    cprof:start(),
    T0 = erlang:monotonic_time(millisecond),
    dialyzer:run([{analysis_type, plt_build}, {files, Files},
                  {output_plt, "/tmp/g3c.plt"}]),
    T1 = erlang:monotonic_time(millisecond),
    cprof:pause(),
    {Total, _} = cprof:analyse(),
    {_, _, Funs} = cprof:analyse(erl_types),
    Get = fun(F, A) ->
              case lists:keyfind({erl_types, F, A}, 1, Funs) of
                  {_, N} -> N; false -> 0
              end
          end,
    io:format("time ~b ms  total_calls ~b  are_all_limited ~b  is_limited ~b  t_limit ~b~n",
              [T1 - T0, Total, Get(are_all_limited, 2), Get(is_limited, 2),
               Get(t_limit, 2)]).
