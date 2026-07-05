#!/usr/bin/env escript
%%! -mode(compile)
%% M0.2 Elixir/.beam-leg driver. Takes .beam files compiled WITH debug_info
%% (e.g. elixirc output), recovers Erlang abstract forms via the module's
%% debug_info backend (elixir_erl for Elixir), recompiles them to optimized
%% SSA (compile:forms -> core_to_ssa -> ssa_opt), and classifies every
%% allocation site with sinkscan. Cross-module resolution spans the given
%% beam set.
%%
%% Usage: escript run_beam_sink.escript <elixir_ebin> <out.eterm> <beamdir>...
%%   <elixir_ebin>  path to elixir's ebin (for the elixir_erl backend)
%% Requires sinkscan.beam next to this script.

main([ElixirEbin, Out | BeamDirs]) ->
    ScriptDir = filename:dirname(escript:script_name()),
    code:add_patha(ScriptDir),
    code:add_patha(ElixirEbin),
    Beams = lists:append([filelib:wildcard(filename:join(D, "*.beam")) || D <- BeamDirs]),
    io:format("recovering forms from ~p beams...~n", [length(Beams)]),
    Mods = lists:filtermap(fun load_beam/1, Beams),
    io:format("~p modules to SSA ok~n", [length(Mods)]),
    Sites = sinkscan:classify_mods(Mods),
    io:format("classified ~p alloc sites~n", [length(Sites)]),
    DirOf = maps:from_list([{M, beamleg} || {M, _} <- Mods]),
    ok = file:write_file(Out, term_to_binary({Sites, DirOf})),
    io:format("wrote ~s~n", [Out]);
main(_) ->
    io:format("usage: escript run_beam_sink.escript <elixir_ebin> <out.eterm> <beamdir>...~n"),
    halt(1).

load_beam(Beam) ->
    try
        {ok, {Mod, [{debug_info, {debug_info_v1, Backend, Meta}}]}} =
            beam_lib:chunks(Beam, [debug_info]),
        {ok, Forms} = Backend:debug_info(erlang_v1, Mod, Meta, []),
        case sinkscan:forms_to_ssa(Mod, Forms) of
            {Name, Body} -> {true, {Name, Body}};
            error -> false
        end
    catch C:E ->
            io:format(standard_error, "beam fail ~s: ~p:~p~n", [Beam, C, E]),
            false
    end.
