%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(ct_cover_to_lcov).
-moduledoc false.

%%% Convert the per-testcase coverage artifacts written by
%%% cth_coverage into LCOV tracefile (.info) format, suitable for
%%% genhtml, lcov tooling and coverage services.
%%%
%%% Each input file contains
%%%
%%%    term_to_binary([{Module, [{Line, Count}]}])
%%%
%%% with only executed (non-zero) lines present. All input files are
%%% merged by summing counts per {Module, Line} before the LCOV
%%% output is written.
%%%
%%% The sparse coverdata only contains EXECUTED lines. To also emit
%%% DA records for instrumented-but-unexecuted lines, supply the line
%%% manifest written by cth_coverage at the end of the run
%%% (<dir>/coverage.manifest, term_to_binary([{Module, [Line]}]) with
%%% ALL instrumented lines) via the {manifest, File} option of
%%% convert/3 or the --manifest escript argument. Un-hit manifest
%%% lines are then emitted as DA:<line>,0, making LF (instrumented
%%% lines) and LH (hit lines) meaningful. Without a manifest only
%%% executed lines are emitted, so every file appears fully covered
%%% (LH == LF) in LCOV consumers.

-export([convert/2, convert/3, main/1]).

-doc """
Read all `InFiles`, merge their per-line execution counts, and write
the result in LCOV tracefile format to `OutFile`.

Unreadable or malformed input files are skipped with a warning on
standard error.
""".
-spec convert(InFiles, OutFile) -> ok | {error, term()} when
      InFiles :: [file:filename()],
      OutFile :: file:filename().
convert(InFiles, OutFile) ->
    convert(InFiles, OutFile, []).

-doc """
As `convert/2`, with options.

`{manifest, ManifestFile}` names a line manifest written by
cth_coverage (`term_to_binary([{Module, [Line]}])` with all
instrumented lines). Manifest lines not present in the merged
coverdata are emitted with count 0, so instrumented-but-unexecuted
lines show up as un-hit. An unreadable or malformed manifest is an
error (not skipped): silently emitting all-covered output would
defeat its purpose.
""".
-spec convert(InFiles, OutFile, Opts) -> ok | {error, term()} when
      InFiles :: [file:filename()],
      OutFile :: file:filename(),
      Opts :: [{manifest, file:filename()}].
convert(InFiles, OutFile, Opts) when is_list(InFiles), is_list(Opts) ->
    case load_manifest(proplists:get_value(manifest, Opts)) of
        {ok, Manifest} ->
            Merged = lists:foldl(fun merge_file/2, #{}, InFiles),
            write_lcov(apply_manifest(Manifest, Merged), OutFile);
        {error, _} = Error ->
            Error
    end.

-doc """
Escript entry point.

Usage: `ct_cover_to_lcov [--manifest <file>] <out.info> <in1.coverdata> ...`

If a single input argument is given and it is a directory, all
`*.coverdata` files in it are converted. With `--manifest`, the line
manifest written by cth_coverage (`<dir>/coverage.manifest`) is used
to emit un-hit instrumented lines with count 0.
""".
-spec main(Args :: [string()]) -> ok | no_return().
main(["--manifest", ManifestFile | Args]) ->
    main(Args, [{manifest, ManifestFile}]);
main(Args) ->
    main(Args, []).

main([OutFile, MaybeDir], Opts) ->
    InFiles =
        case filelib:is_dir(MaybeDir) of
            true ->
                filelib:wildcard(filename:join(MaybeDir, "*.coverdata"));
            false ->
                [MaybeDir]
        end,
    run(InFiles, OutFile, Opts);
main([OutFile | InFiles], Opts) when InFiles =/= [] ->
    run(InFiles, OutFile, Opts);
main(_, _Opts) ->
    io:format(standard_error,
              "usage: ct_cover_to_lcov [--manifest <file>] "
              "<out.info> <in1.coverdata> ...~n"
              "       ct_cover_to_lcov [--manifest <file>] "
              "<out.info> <coverdata-dir>~n", []),
    erlang:halt(1).

run([], _OutFile, _Opts) ->
    io:format(standard_error,
              "ct_cover_to_lcov: error: no input .coverdata files~n", []),
    erlang:halt(1);
run(InFiles, OutFile, Opts) ->
    case convert(InFiles, OutFile, Opts) of
        ok ->
            ok;
        {error, Reason} ->
            io:format(standard_error,
                      "ct_cover_to_lcov: error: ~tp~n", [Reason]),
            erlang:halt(1)
    end.

%%%-----------------------------------------------------------------
%%% Reading and merging

merge_file(File, Acc) ->
    case file:read_file(File) of
        {ok, Bin} ->
            try binary_to_term(Bin) of
                Data when is_list(Data) ->
                    try
                        lists:foldl(fun merge_module/2, Acc, Data)
                    catch
                        _:_ ->
                            warn(File, malformed_coverdata),
                            Acc
                    end;
                _Other ->
                    warn(File, malformed_coverdata),
                    Acc
            catch
                error:badarg ->
                    warn(File, not_external_term_format),
                    Acc
            end;
        {error, Reason} ->
            warn(File, Reason),
            Acc
    end.

merge_module({Module, Lines}, Acc)
  when is_atom(Module), is_list(Lines) ->
    ModMap = lists:foldl(fun merge_line/2, maps:get(Module, Acc, #{}), Lines),
    Acc#{Module => ModMap}.

merge_line({Line, Count}, ModMap)
  when is_integer(Line), Line > 0, is_integer(Count), Count >= 0 ->
    maps:update_with(Line, fun(C) -> C + Count end, Count, ModMap).

warn(File, Reason) ->
    io:format(standard_error,
              "ct_cover_to_lcov: warning: skipping ~ts: ~tp~n",
              [File, Reason]).

%%%-----------------------------------------------------------------
%%% Line manifest (all instrumented lines, executed or not)

%% Load the manifest into #{Module => [Line]} (lines sorted, unique).
load_manifest(undefined) ->
    {ok, #{}};
load_manifest(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            try binary_to_term(Bin) of
                Data when is_list(Data) ->
                    try
                        {ok, lists:foldl(fun manifest_module/2, #{}, Data)}
                    catch
                        _:_ ->
                            {error, {manifest, File, malformed_manifest}}
                    end;
                _Other ->
                    {error, {manifest, File, malformed_manifest}}
            catch
                error:badarg ->
                    {error, {manifest, File, not_external_term_format}}
            end;
        {error, Reason} ->
            {error, {manifest, File, Reason}}
    end.

manifest_module({Module, Lines}, Acc) when is_atom(Module), is_list(Lines) ->
    Good = lists:usort([L || L <- Lines, is_integer(L), L > 0]),
    maps:update_with(Module, fun(Ls) -> lists:umerge(Good, Ls) end,
                     Good, Acc).

%% Extend the merged coverdata with count-0 entries for every
%% manifest line that was never hit. Modules present only in the
%% manifest get all-zero records; modules present only in the
%% coverdata are kept as-is.
apply_manifest(Manifest, Merged) when map_size(Manifest) =:= 0 ->
    Merged;
apply_manifest(Manifest, Merged) ->
    maps:fold(
      fun(Module, Lines, Acc) ->
              Hit = maps:get(Module, Acc, #{}),
              Acc#{Module => maps:merge(maps:from_keys(Lines, 0), Hit)}
      end, Merged, Manifest).

%%%-----------------------------------------------------------------
%%% LCOV output

write_lcov(Merged, OutFile) ->
    Records = [module_record(Module, maps:get(Module, Merged)) ||
                  Module <- lists:sort(maps:keys(Merged))],
    file:write_file(OutFile, Records).

module_record(Module, LineMap) ->
    Lines = lists:sort(maps:to_list(LineMap)),
    Hit = length([[] || {_Line, Count} <- Lines, Count > 0]),
    ["SF:", source_file(Module), "\n",
     [io_lib:format("DA:~w,~w~n", [Line, Count]) || {Line, Count} <- Lines],
     io_lib:format("LF:~w~n", [length(Lines)]),
     io_lib:format("LH:~w~n", [Hit]),
     "end_of_record\n"].

%% Best-effort resolution of the source file for Module. Never crashes;
%% falls back to "Module.erl" when nothing better can be derived.
source_file(Module) ->
    Source = try Module:module_info(compile) of
                 Info when is_list(Info) ->
                     proplists:get_value(source, Info);
                 _ ->
                     undefined
             catch
                 _:_ ->
                     undefined
             end,
    case Source of
        [_ | _] ->
            filename:absname(Source);
        _ ->
            source_from_beam(Module)
    end.

source_from_beam(Module) ->
    Default = atom_to_list(Module) ++ ".erl",
    try code:which(Module) of
        Beam when is_list(Beam), Beam =/= [] ->
            %% .../ebin/Module.beam -> .../src/Module.erl
            AppDir = filename:dirname(filename:dirname(Beam)),
            Src = filename:join([AppDir, "src", Default]),
            case filelib:is_regular(Src) of
                true -> filename:absname(Src);
                false -> Default
            end;
        _ ->
            Default
    catch
        _:_ ->
            Default
    end.
