%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2021-2025. All Rights Reserved.
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

-module(jit_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, groups/0, all/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).
-export([annotate/1, frame_pointers/1, jmsingle/1, named_labels/1,
         jit_cache_snapshot/1, jit_cache_fresh_vm/1, symbols/1,
         perfdirectory_set/1, perfdirectory_not_set/1]).
-export([frame_pointer_workload/0]).

suite() ->
    [{timetrap, {minutes, 4}}].

groups() ->
    [{perf, [symbols, annotate, frame_pointers,
             perfdirectory_set, perfdirectory_not_set]}].

all() ->
    [{group, perf}, jmsingle, named_labels,
     jit_cache_snapshot, jit_cache_fresh_vm].

init_per_suite(Config) ->
    case erlang:system_info(emu_flavor) of
        jit ->
            Config;
        _ ->
            {skip, "No point in running JIT tests on non-JIT emulator"}
    end.

end_per_suite(_Config) ->
    ok.

init_per_group(perf, Config) ->
    case os:find_executable("perf") of
        false ->
            {skip, "perf not found"};
        _Perf ->
            PerfVsn = os:cmd("perf version"),
            case re:run(PerfVsn, "perf version (\\d+)\\.(\\d+)",
                        [{capture,all_but_first,list}]) of
                {match,[Major0,Minor0]} ->
                    Major = list_to_integer(Major0),
                    Minor = list_to_integer(Minor0),
                    if
                        Major > 5; Major =:= 5, Minor >= 11 ->
                            BuildIdDir = "--buildid-dir " ++
                                filename:join(
                                  proplists:get_value(priv_dir, Config),
                                  ".debug"),
                            DataFile = filename:join(
                                         proplists:get_value(priv_dir, Config),
                                         "init_test.data"),
                            Cmd = "perf " ++ BuildIdDir ++ " record -q -o " ++ DataFile ++ " ls",
                            os:cmd(Cmd),
                            Script = os:cmd("perf " ++ BuildIdDir ++ " script -i " ++ DataFile),
                            ct:log("~ts",[Script]),
                            case re:run(Script, "^\\W+ls",[multiline]) of
                                {match, _} ->
                                    [{sobefore,get_tmp_so_files()},
                                     {buildiddir,BuildIdDir}|Config];
                                nomatch ->
                                    {skip, "could not run `"++ Cmd ++"`"}
                            end;
                        true ->
                            {skip,"too old perf version: " ++ PerfVsn}
                    end;
                _ ->
                    {skip,"unknown old perf version: " ++ PerfVsn}
            end
    end;
init_per_group(_, Config) ->
    Config.

end_per_group(perf, Config) ->
    %% perf inject writes data to /tmp and ~/.debug/tmp so we need to clean
    %% that up after the tests are done.
    SoToDelete = get_tmp_so_files() -- proplists:get_value(sobefore, Config),
    lists:foreach(
      fun(File) ->
              case file:delete(File) of
                  {error,eperm} ->
                      ok = file:del_dir_r(File);
                  ok ->
                      ok
              end
      end, SoToDelete),
    ok;
end_per_group(_, _Config) ->
    ok.

init_per_testcase(frame_pointers, Config) ->
    case erlang:system_info(system_architecture) of
        "aarch64" ++ _ ->
            Config;
        _ ->
            {skip, "AArch64-specific test"}
    end;
init_per_testcase(named_labels, Config) ->
    %% Only run named_labels on platforms where we know it works.
    case erlang:system_info(system_architecture) of
	"x86" ++ _ ->
	    [{asmbefore,get_tmp_asm_files()}|Config];
	"aarch64" ++ _ ->
	    [{asmbefore,get_tmp_asm_files()}|Config];
	_ ->
	    {skip, "Unsupported architecture"}
    end;
init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(named_labels, Config) ->
    AsmToDelete = get_tmp_asm_files() -- proplists:get_value(asmbefore, Config),
    lists:foreach(
      fun(File) ->
              ok = file:delete(File)
      end, AsmToDelete),
    ok;
end_per_testcase(_, _Config) ->
    ok.

jit_cache_snapshot(_Config) ->
    {?MODULE, CompressedBeam, _} = code:get_object_code(?MODULE),
    Beam = case CompressedBeam of
               <<16#1f, 16#8b, _/binary>> -> zlib:gunzip(CompressedBeam);
               _ -> CompressedBeam
           end,
    {ok, Cache} = erts_internal:beam_jit_cache(?MODULE, Beam),

    <<"EJITC001",
      1:32/little,
      80:32/little,
      Architecture:32/little,
      SectionCount:32/little,
      RelocationCount:32/little,
      0:32/little,
      ImageSize:64/little,
      Checksum:16/binary,
      SectionsOffset:64/little,
      RelocationsOffset:64/little,
      ImageOffset:64/little,
      _/binary>> = Cache,

    true = Architecture =:= 1 orelse Architecture =:= 2,
    true = SectionCount > 0,
    80 = SectionsOffset,
    RelocationsOffset = SectionsOffset + SectionCount * 40,
    ImageOffset = RelocationsOffset + RelocationCount * 64,
    MetadataOffset = ImageOffset + ImageSize,
    <<_:MetadataOffset/binary,
      "EJMETA03",
      FunctionCount:32/little,
      LabelCount:32/little,
      FixupCount:32/little,
      FixupGroupCount:32/little,
      _CodeHeaderOffset:64/little,
      _CodeEndOffset:64/little,
      _OnLoadOffset:64/little,
      Metadata/binary>> = Cache,
    true = FunctionCount > 0,
    true = LabelCount > 0,
    true = FixupGroupCount > 0,
    TrailerOffset = (FunctionCount + LabelCount) * 8 +
                    FixupGroupCount * 8 + FixupCount * 28,
    <<_:TrailerOffset/binary,
      "EJLDR001",
      DynamicLiteralCount:32/little,
      LambdaCount:32/little,
      NifBytes:32/little,
      InitialDynamicLiteralCount:32/little,
      LoaderMetadata/binary>> = Metadata,
    true = InitialDynamicLiteralCount =< DynamicLiteralCount,
    NifBytes = (FunctionCount + 7) div 8,
    AfterLiterals = skip_cached_literals(
                      DynamicLiteralCount - InitialDynamicLiteralCount,
                      LoaderMetadata),
    LambdaBytes = LambdaCount * 8,
    <<_:LambdaBytes/binary, _:NifBytes/binary>> = AfterLiterals,
    Checksum = erts_internal:beamfile_module_md5(Beam),

    try erts_internal:beam_jit_cache(<<"not an atom">>, Beam) of
        _ -> ct:fail(expected_badarg)
    catch
        error:badarg -> ok
    end,
    try erts_internal:beam_jit_cache(?MODULE, not_a_binary) of
        _ -> ct:fail(expected_badarg)
    catch
        error:badarg -> ok
    end,
    {error, badfile} =
        erts_internal:beam_jit_cache(?MODULE, <<"not a BEAM file">>),
    ok.

skip_cached_literals(0, Metadata) ->
    Metadata;
skip_cached_literals(Count,
                     <<_Kind:32/little, Size:32/little,
                       _Value:Size/binary, Rest/binary>>) ->
    skip_cached_literals(Count - 1, Rest).

jit_cache_fresh_vm(Config) ->
    Module = jit_cache_fresh_probe,
    Asm = {Module,
           [{id, 1}],
           [],
           #{},
           [{function, id, 1, 2,
             [{label, 1},
              {line, []},
              {func_info, {atom, Module}, {atom, id}, 1},
              {label, 2},
              return]}],
           3},
    {ok, Beam} = beam_asm:module(Asm, [], [], []),
    {ok, Cache} = erts_internal:beam_jit_cache(Module, Beam),
    <<First, Rest/binary>> = Cache,
    {ok, Fallback, false} =
        erts_internal:beam_jit_cache_prepare(
          Module, Beam, <<(First bxor 1), Rest/binary>>),
    ok = erlang:finish_loading([Fallback]),
    42 = Module:id(42),
    true = code:delete(Module),
    false = code:purge(Module),
    AtomModule = jit_cache_atom_probe,
    AtomAsm = {AtomModule,
               [{value, 0}],
               [],
               #{},
               [{function, value, 0, 2,
                 [{label, 1},
                  {line, []},
                  {func_info, {atom, AtomModule}, {atom, value}, 0},
                  {label, 2},
                  {move, {atom, ok}, {x, 0}},
                  return]}],
               3},
    {ok, AtomBeam} = beam_asm:module(AtomAsm, [], [], []),
    {ok, AtomCache} = erts_internal:beam_jit_cache(AtomModule, AtomBeam),
    {ok, AtomPrepared, true} =
        erts_internal:beam_jit_cache_prepare(
          AtomModule, AtomBeam, AtomCache),
    ok = erlang:finish_loading([AtomPrepared]),
    ok = AtomModule:value(),
    true = code:delete(AtomModule),
    false = code:purge(AtomModule),
    OnLoadModule = jit_cache_on_load_probe,
    OnLoadAsm = {OnLoadModule,
                 [{value, 0}],
                 [],
                 #{},
                 [{function, init, 0, 2,
                   [{label, 1},
                    {line, []},
                    {func_info, {atom, OnLoadModule}, {atom, init}, 0},
                    {label, 2},
                    on_load,
                    {move, {atom, ok}, {x, 0}},
                    return]},
                  {function, value, 0, 4,
                   [{label, 3},
                    {line, []},
                    {func_info, {atom, OnLoadModule}, {atom, value}, 0},
                    {label, 4},
                    {move, {integer, 42}, {x, 0}},
                    return]}],
                 5},
    {ok, OnLoadBeam} = beam_asm:module(OnLoadAsm, [], [], []),
    {ok, OnLoadCache} =
        erts_internal:beam_jit_cache(OnLoadModule, OnLoadBeam),
    {ok, OnLoadPrepared, true} =
        erts_internal:beam_jit_cache_prepare(
          OnLoadModule, OnLoadBeam, OnLoadCache),
    {on_load, [OnLoadModule]} = erlang:finish_loading([OnLoadPrepared]),
    ok = erlang:call_on_load_function(OnLoadModule),
    true = erlang:finish_after_on_load(OnLoadModule, true),
    42 = OnLoadModule:value(),
    true = code:delete(OnLoadModule),
    false = code:purge(OnLoadModule),
    PrivDir = proplists:get_value(priv_dir, Config),
    BeamPath = filename:join(PrivDir, "jit_cache_fresh_probe.beam"),
    CachePath = filename:join(PrivDir, "jit_cache_fresh_probe.ejit"),
    ok = file:write_file(BeamPath, Beam),
    ok = file:write_file(CachePath, Cache),
    Eval = lists:flatten(
             io_lib:format(
               "{ok,B}=file:read_file(~tp),"
               "{ok,C}=file:read_file(~tp),"
               "{ok,P,true}=erts_internal:beam_jit_cache_prepare("
               "jit_cache_fresh_probe,B,C),"
               "ok=erlang:finish_loading([P]),"
               "true=erlang:module_loaded(jit_cache_fresh_probe),"
               "42=jit_cache_fresh_probe:id(42),"
               "io:format(\"CACHE_OK~n\",[]),halt().",
               [BeamPath, CachePath])),
    Command = ct:get_progname() ++ " -noshell -eval '" ++ Eval ++ "'",
    Port = open_port({spawn, Command},
                     [binary, exit_status, stderr_to_stdout]),
    {0, Output} = collect_port(Port, []),
    {match, _} = re:run(Output, "CACHE_OK"),
    ok.

collect_port(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            collect_port(Port, [Data | Acc]);
        {Port, {exit_status, Status}} ->
            {Status, iolist_to_binary(lists:reverse(Acc))}
    after 30000 ->
        port_close(Port),
        ct:fail({fresh_vm_timeout,
                 iolist_to_binary(lists:reverse(Acc))})
    end.

get_tmp_so_files() ->
    {ok, Home} = init:get_argument(home),
    filelib:wildcard("/tmp/jitted-*.so") ++
        filelib:wildcard(filename:join([Home,".debug","tmp","jitted-*.so"])).

symbols(Config) ->
    BuildIdDir = proplists:get_value(buildiddir, Config),
    DataFile = filename:join(
                 proplists:get_value(priv_dir, Config),
                 atom_to_list(?FUNCTION_NAME) ++ ".data"),
    os:cmd("perf "++ BuildIdDir ++" record -q -o " ++ DataFile ++ " " ++
           "erl +S 1 +JPperf true -noshell -eval '[lists:seq(1,10000) || _ <- lists:seq(1,1000)].' -s init stop"),
    Script = os:cmd("perf "++ BuildIdDir ++" script -i " ++ DataFile),
    case re:run(Script,"\\$lists:seq[^/]+/[0-9]",[global,{capture,first,list}]) of
        {match,Matches} ->
            ct:log("Found these symbols: ~p",[lists:usort(Matches)]),
            ok;
        nomatch ->
            ct:fail("Did not find lists:seq symbol in:~n~ts",[Script])
    end.

frame_pointers(Config) ->
    BuildIdDir = proplists:get_value(buildiddir, Config),
    DataFile = filename:join(
                 proplists:get_value(priv_dir, Config),
                 atom_to_list(?FUNCTION_NAME) ++ ".data"),
    ModulePath = filename:dirname(code:which(?MODULE)),
    Cmd = "perf " ++ BuildIdDir ++ " record -q -F 99 -g " ++
        "--call-graph fp -o " ++ DataFile ++ " " ++
        "erl +S 1 +JPperf map -noshell -pa " ++ ModulePath ++ " " ++
        "-s jit_SUITE frame_pointer_workload -s init stop",
    "" = os:cmd(Cmd),
    Script = os:cmd("perf " ++ BuildIdDir ++ " script -i " ++ DataFile),
    Samples = string:split(Script, "\n\n", all),
    Depths = [length(Matches) || Sample <- Samples,
               {match, Matches} <-
                   [re:run(Sample, "\\$jit_SUITE:frame_pointer_[abc]/1",
                           [global, {capture, first, list}])]],
    case Depths =/= [] andalso lists:max(Depths) >= 8 of
        true ->
            ok;
        false ->
            ct:fail("Did not find a deep Erlang call chain")
    end.

frame_pointer_workload() ->
    End = erlang:monotonic_time(millisecond) + 2000,
    frame_pointer_repeat(End).

frame_pointer_repeat(End) ->
    _ = frame_pointer_a(32),
    case erlang:monotonic_time(millisecond) < End of
        true -> frame_pointer_repeat(End);
        false -> ok
    end.

frame_pointer_a(0) -> 1;
frame_pointer_a(N) -> 1 + frame_pointer_b(N - 1).

frame_pointer_b(0) -> 1;
frame_pointer_b(N) -> 1 + frame_pointer_c(N - 1).

frame_pointer_c(0) -> 1;
frame_pointer_c(N) -> 1 + frame_pointer_a(N - 1).
    
annotate(Config) ->
    BuildIdDir = proplists:get_value(buildiddir, Config),
    DataFile = filename:join(
                 proplists:get_value(priv_dir, Config),
                 atom_to_list(?FUNCTION_NAME) ++ ".data"),
    os:cmd("perf "++ BuildIdDir ++" record -k mono -q -o " ++ DataFile ++ " " ++
           "erl +S 1 +JPperf true -noshell -eval '[lists:seq(1,10000) || _ <- lists:seq(1,1000)].' -s init stop"),
    Script = os:cmd("perf "++ BuildIdDir ++" script -i " ++ DataFile),

    %% When doing "perf inject" the symbol of each function is changed
    %% from $lists:seq_loop/3 to lists:seq_loop/3. I don't know why that
    %% is, seems like a very odd bug in perf...
    {match, Symbols} = re:run(Script, "lists:seq[^/]+/[0-9]+",
                              [global,{capture,first,list}]),
    [[Symbol]|_] = lists:usort(Symbols),
    JitFile = DataFile ++ ".jit.data",
    "" = os:cmd("perf "++ BuildIdDir ++" inject --jit -i " ++ DataFile ++ " -o " ++ JitFile),
    Anno = os:cmd("perf "++ BuildIdDir ++" annotate --stdio -i " ++ JitFile ++ " " ++ Symbol ++ ""),
    case re:run(Anno,"Source code & Disassembly") of
        {match,_} ->
            case re:run(Anno, "<" ++ Symbol ++ ">:") of
                {match,_} ->
                    ok;
                nomatch ->
                    ct:fail("Did not find instructions for ~ts.~n~ts",
                            [Symbol, Anno])
            end;
        nomatch ->
            ct:fail("Did not find disassembly for ~ts.~n~ts",
                    [Symbol, Anno])
    end.

run_jmsingle_test(Param, ExpectSuccess, ErrorMsg) ->
    Cmd = "erl +JMsingle " ++ Param ++ " -noshell " ++
        "-eval \"erlang:display(all_is_well),erlang:halt(0).\"",
    Result = os:cmd(Cmd),
    SuccessfulEmulatorStart =
        case Result of
            "all_is_well" ++ _ ->
                true;
            _ ->
                Error = "Failed to allocate executable+writable memory",
                case string:find(Result, Error) of
                    nomatch -> false;
                    _ -> internal_error
                end
        end,
    case SuccessfulEmulatorStart of
        ExpectSuccess ->
            ok;
        _ ->
            ct:fail(ErrorMsg)
    end.

jmsingle(Config) ->
    %% Smoke test to check that the emulator starts with the +JMsingle
    %% true/false option and fails with a non-boolean, that is, we
    %% parse the command line correctly.
    case os:type() of
        {_, BSD} when BSD =:= netbsd;
                      BSD =:= openbsd ->
            %% +JMsingle true might not work on these platforms, and dump core
            %% because the emulator cannot be started.
            %% 
            %% Set the cwd to a temporary directory that we'll delete when the
            %% test is done.
            {ok, Cwd} = file:get_cwd(),
            TestDir = filename:join(proplists:get_value(priv_dir, Config),
                                    "jmsingle"),
            ok = file:make_dir(TestDir),
            try
                ok = file:set_cwd(TestDir),
                run_jmsingle_test("true", internal_error,
                                  "Emulator did not print the correct diagnostic "
                                  "(crashed?) with +JMsingle true")
            after
                file:set_cwd(Cwd),
                file:del_dir_r(TestDir)
            end;
        {_, _} ->
            run_jmsingle_test("true", true,
                              "Emulator did not start with +JMsingle true")
    end,
    run_jmsingle_test("false", true,
                      "Emulator did not start with +JMsingle false"),
    run_jmsingle_test("broken", false,
                      "Emulator started with bad +JMsingle parameter").

get_tmp_asm_files() ->
    {ok, Cwd} = file:get_cwd(),
    filelib:wildcard(filename:join(Cwd, "*.asm")).

named_labels(_Config) ->
    %% Check that pretty printing of named labels is working. We do
    %% that by loading this module in an emulator running with +JDdump
    %% true and then checking that the produced jit_SUITE.asm contains
    %% a label for each exported function. We also check that the
    %% label for the non-exported function get_tmp_asm_files/0, which
    %% is used by this test, is present.
    Exports = proplists:get_value(exports, ?MODULE:module_info()),
    ModName = atom_to_list(?MODULE),
    ModulePath = filename:dirname(code:which(?MODULE)),
    Cmd = "erl +JDdump true -noshell -pa " ++ ModulePath ++ " -eval \""
        ++ ModName ++ ":module_info(),erlang:halt(0).\"",
    os:cmd(Cmd),
    {ok, Cwd} = file:get_cwd(),
    AsmFile = filename:join(Cwd, ModName ++ ".asm"),
    {ok, Data} = file:read_file(AsmFile),
    Expected = sets:from_list([ lists:flatten(io_lib:format("~p/~p", [N,A]))
                                || {N,A} <- Exports]
                              ++ ["get_tmp_asm_files/0"]),
    StripSeqNo = fun(Lbl) ->
                         %% In the Arm JIT, labels have the form
                         %% @Mod:Name/Arity-SequenceNumber, so strip
                         %% out the Mod part and the sequence number
                         %% as we can't know anything about them.
                         case re:run(Lbl, "^.*\:(.*)-[0-9]*$",
                                     [{capture,all_but_first,list}]) of
                             {match,[R]} -> R;
                             nomatch -> Lbl
                         end
                 end,
    case re:run(Data, "^(.*)\:\n",
                [global,multiline,{capture,all_but_first,list}]) of
        {match,Labels} ->
            Found = sets:from_list([ StripSeqNo(NA) || [NA] <- Labels]),
            case sets:is_subset(Expected, Found) of
                true ->
                    ok;
                false ->
                    ct:fail("Expected ~p, found ~p",
                            [sets:to_list(Expected), sets:to_list(Found)])
            end;
        _ ->
            ct:fail("No labels found in assembly dump")
    end.

perfdirectory_set(_Config) ->
    %% Confirm that when +JPperfdirectory is set, the jit/perf files
    %% end up in the appropriate directory
    TempDir = string:strip(os:cmd("mktemp -d"), right, $\n),
    run_perfdirectory_test("+JPperfdirectory " ++ TempDir, TempDir).

perfdirectory_not_set(_Config) ->
    %% Confirm that when +JPperfdirectory is not set, the jit/perf files
    %% end up in /tmp
    run_perfdirectory_test("", "/tmp").

run_perfdirectory_test(AdditionalFlags, ExpectedDirectory) ->
    Cmd = "erl -noinput -eval 'io:format(\"~s~n\", [os:getpid()]).' -s init stop +JPperf true " ++ AdditionalFlags,
    ThePid = string:strip(os:cmd(Cmd), right, $\n),
    JitDumpFile = ExpectedDirectory ++ "/jit-" ++ ThePid ++ ".dump",
    PerfMapFile = ExpectedDirectory ++ "/perf-" ++ ThePid ++ ".map",
    case lists:all(fun filelib:is_file/1, [JitDumpFile, PerfMapFile]) of
        true ->
            ok;
        _ ->
            ct:fail("Expected ~s and ~s to exist", [JitDumpFile, PerfMapFile])
    end.
