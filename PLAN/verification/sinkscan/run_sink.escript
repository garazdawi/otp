#!/usr/bin/env escript
%%! -mode(compile)
%% M0.2 corpus driver. Compiles the hot corpus to optimized SSA via sinkscan
%% and writes a per-allocation-site dump ({[#asite{} tuples], DirOf}).
%% Mirrors elimscan's run_scan.escript.
%%
%% Usage:  escript run_sink.escript <otp_root> <out.eterm> [extra_dir ...]
%%   <otp_root>  OTP source checkout (has lib/ and erts/)
%%   <out.eterm> output path for the binary term dump
%%   extra_dir   optional extra source dirs (tag=path) appended to the corpus
%% Requires sinkscan.beam on the code path.

main([Root, Out | Extra]) ->
    Base = [{dialyzer, "lib/dialyzer/src"},
            {compiler,  "lib/compiler/src"},
            {stdlib,    "lib/stdlib/src"},
            {kernel,    "lib/kernel/src"},
            {erts,      "erts/preloaded/src"}],
    Dirs = [{T, filename:join(Root, R)} || {T, R} <- Base]
        ++ [parse_extra(E) || E <- Extra],
    {AllFiles, DirOf} =
        lists:foldl(
          fun({Tag, Dir}, {Fs, Map}) ->
                  These = filelib:wildcard(filename:join(Dir, "*.erl")),
                  Map1 = lists:foldl(
                           fun(F, M) ->
                                   Mod = list_to_atom(filename:basename(F, ".erl")),
                                   M#{Mod => Tag}
                           end, Map, These),
                  {Fs ++ These, Map1}
          end, {[], #{}}, Dirs),
    Incs = [filename:join(Root, I) ||
               I <- ["lib/dialyzer/src", "lib/dialyzer/include",
                     "lib/compiler/src",
                     "lib/stdlib/src", "lib/stdlib/include",
                     "lib/kernel/src", "lib/kernel/include",
                     "erts/preloaded/src"]]
        ++ [D || {_, D} <- [parse_extra(E) || E <- Extra]],
    io:format("scanning ~p files...~n", [length(AllFiles)]),
    T0 = erlang:monotonic_time(millisecond),
    Sites = sinkscan:scan(AllFiles, Incs),
    T1 = erlang:monotonic_time(millisecond),
    io:format("scanned ~p alloc sites in ~p ms~n", [length(Sites), T1 - T0]),
    ok = file:write_file(Out, term_to_binary({Sites, DirOf})),
    io:format("wrote ~s~n", [Out]);
main(_) ->
    io:format("usage: escript run_sink.escript <otp_root> <out.eterm> [tag=dir ...]~n"),
    halt(1).

parse_extra(E) ->
    case string:split(E, "=") of
        [Tag, Path] -> {list_to_atom(Tag), Path};
        _ -> {extra, E}
    end.
