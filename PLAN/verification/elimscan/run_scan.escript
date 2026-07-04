#!/usr/bin/env escript
%%! -mode(compile)
%% M0.1 corpus driver. Compiles the hot corpus to optimized SSA via
%% elimscan and writes a per-function stats dump ({[fstat], DirOf}).
%%
%% Usage:  escript run_scan.escript <otp_root> <out.eterm>
%%   <otp_root>  OTP source checkout (has lib/ and erts/)
%%   <out.eterm> output path for the binary term dump
%% Requires elimscan.beam on the code path (erlc elimscan.erl first, run
%% from that directory, or add -pa).

main([Root, Out]) ->
    Dirs = [{dialyzer, "lib/dialyzer/src"},
            {compiler,  "lib/compiler/src"},
            {stdlib,    "lib/stdlib/src"},
            {kernel,    "lib/kernel/src"},
            {erts,      "erts/preloaded/src"}],
    {AllFiles, DirOf} =
        lists:foldl(
          fun({Tag, Rel}, {Fs, Map}) ->
                  Glob = filename:join([Root, Rel, "*.erl"]),
                  These = filelib:wildcard(Glob),
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
                     "erts/preloaded/src"]],
    io:format("scanning ~p files...~n", [length(AllFiles)]),
    T0 = erlang:monotonic_time(millisecond),
    Stats = elimscan:scan(AllFiles, Incs),
    T1 = erlang:monotonic_time(millisecond),
    io:format("scanned ~p functions in ~p ms~n", [length(Stats), T1 - T0]),
    ok = file:write_file(Out, term_to_binary({Stats, DirOf})),
    io:format("wrote ~s~n", [Out]);
main(_) ->
    io:format("usage: escript run_scan.escript <otp_root> <out.eterm>~n"),
    halt(1).
