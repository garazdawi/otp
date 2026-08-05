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
%%% TODO: the sparse coverdata written by cth_coverage only contains
%%%       EXECUTED lines, so this converter cannot emit DA records
%%%       for instrumented-but-unexecuted lines. As a consequence LF
%%%       equals the number of executed lines and every file appears
%%%       fully covered (LH == LF) in LCOV consumers. To fix, feed
%%%       this tool the full instrumented-line set per module: a
%%%       single code:get_coverage(line, Module) call returns ALL
%%%       instrumented lines including those with count 0 (which
%%%       cth_coverage currently filters out for sparsity), and emit
%%%       DA:<line>,0 for the un-hit ones.

-export([convert/2, main/1]).

-doc """
Read all `InFiles`, merge their per-line execution counts, and write
the result in LCOV tracefile format to `OutFile`.

Unreadable or malformed input files are skipped with a warning on
standard error.
""".
-spec convert(InFiles, OutFile) -> ok | {error, term()} when
      InFiles :: [file:filename()],
      OutFile :: file:filename().
convert(InFiles, OutFile) when is_list(InFiles) ->
    Merged = lists:foldl(fun merge_file/2, #{}, InFiles),
    write_lcov(Merged, OutFile).

-doc """
Escript entry point.

Usage: `ct_cover_to_lcov <out.info> <in1.coverdata> ...`

If a single input argument is given and it is a directory, all
`*.coverdata` files in it are converted.
""".
-spec main(Args :: [string()]) -> ok | no_return().
main([OutFile, MaybeDir]) ->
    InFiles =
        case filelib:is_dir(MaybeDir) of
            true ->
                filelib:wildcard(filename:join(MaybeDir, "*.coverdata"));
            false ->
                [MaybeDir]
        end,
    run(InFiles, OutFile);
main([OutFile | InFiles]) when InFiles =/= [] ->
    run(InFiles, OutFile);
main(_) ->
    io:format(standard_error,
              "usage: ct_cover_to_lcov <out.info> <in1.coverdata> ...~n"
              "       ct_cover_to_lcov <out.info> <coverdata-dir>~n", []),
    erlang:halt(1).

run([], _OutFile) ->
    io:format(standard_error,
              "ct_cover_to_lcov: error: no input .coverdata files~n", []),
    erlang:halt(1);
run(InFiles, OutFile) ->
    case convert(InFiles, OutFile) of
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
