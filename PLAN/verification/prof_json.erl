-module(prof_json).
-export([run/1]).
run(File) ->
    {ok, Bin} = file:read_file(File),
    erts_debug:set_internal_state(available_internal_state, true),
    _ = json:decode(Bin),  %% warm/load json module code
    erts_debug:set_internal_state(alloc_profile_sites, true),  %% reset
    erts_debug:set_internal_state(alloc_profile, true),
    [_ = json:decode(Bin) || _ <- lists:seq(1, 20)],
    Sites = erts_debug:get_internal_state(alloc_profile_sites),
    Total = lists:sum([W || {_, W} <- Sites]),
    Top = lists:sublist(lists:reverse(lists:keysort(2,
            [{MFA, W} || {MFA, W} <- Sites])), 15),
    io:format("total recorded ~p words (~.1f MB); top allocating functions:~n",
              [Total, Total * 8 / 1.0e6]),
    [io:format("  ~6.2f%  ~p~n", [100 * W / max(1, Total), MFA]) || {MFA, W} <- Top],
    halt().
