#!/usr/bin/env escript
%% -*- erlang -*-
%%
%% Exercise the emulator broadly on a clangcov (clang source-based
%% coverage) build and dump the native C + JIT-emitter coverage to the
%% directory given as the only argument (erts_debug:coverage({dump,Dir})).
%%
%% Run on the clangcov emulator, e.g.:
%%   ERL_AFLAGS="-emu_type clangcov" escript make/clangcov_probe.escript <dir>
%%
%% Used by the coverage workflow's native job to produce C/JIT coverage
%% without the ct/release machinery (which cannot select -emu_type
%% clangcov from a release that does not contain that emulator).
main([Dir]) ->
    io:format("build_type=~p~n", [erlang:system_info(build_type)]),
    ok = filelib:ensure_path(Dir),
    _ = exercise(),
    case erts_debug:coverage({dump, Dir}) of
        ok ->
            io:format("dumped native coverage to ~ts~n", [Dir]);
        Other ->
            io:format(standard_error, "coverage dump failed: ~p~n", [Other]),
            halt(1)
    end.

exercise() ->
    %% Compiler + JIT: compile a spread of real stdlib sources (heavy on
    %% the emitter and a large slice of the C runtime).
    {ok, Cwd} = file:get_cwd(),
    Src = filename:join([Cwd, "lib", "stdlib", "src"]),
    Files = lists:sublist(filelib:wildcard(filename:join(Src, "*.erl")), 40),
    _ = [try compile:file(F, [binary]) catch _:_ -> error end || F <- Files],
    %% Terms, arithmetic and the common BIFs.
    L = lists:seq(1, 200000),
    _ = lists:sort(L ++ lists:reverse(L)),
    _ = lists:foldl(fun(X, A) -> X + A end, 0, L),
    M = maps:from_list([{X, X * 2} || X <- lists:seq(1, 20000)]),
    _ = maps:fold(fun(_, V, A) -> V + A end, 0, M),
    %% ETS (hash + tree paths).
    Th = ets:new(h, [set]),
    Tt = ets:new(t, [ordered_set]),
    _ = [begin ets:insert(Th, {X, X}), ets:insert(Tt, {X, X}) end
         || X <- lists:seq(1, 20000)],
    _ = ets:select(Tt, [{{'$1', '$2'}, [{'>', '$1', 10000}], ['$2']}]),
    %% Binaries + strings.
    B = list_to_binary(lists:seq(0, 255)),
    _ = [binary:match(B, <<X>>) || X <- lists:seq(0, 255)],
    _ = << <<(X band 255):8>> || X <- lists:seq(1, 100000) >>,
    _ = string:uppercase(lists:flatten(lists:duplicate(1000, "abc "))),
    %% Processes + message passing.
    Self = self(),
    Pids = [spawn(fun() -> receive go -> Self ! done end end)
            || _ <- lists:seq(1, 2000)],
    _ = [P ! go || P <- Pids],
    _ = [receive done -> ok end || _ <- Pids],
    ok.
