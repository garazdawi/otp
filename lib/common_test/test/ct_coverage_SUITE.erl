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

%%%-------------------------------------------------------------------
%%% @doc Test of the cth_coverage hook, which captures per-testcase
%%% native line coverage into one coverdata file per test case, plus
%%% separate bucket files for the configuration functions
%%% (init/end_per_suite and init/end_per_group).
%%%
%%% A tiny helper module (covhelper) and an inner test suite
%%% (covinner_SUITE) are generated and compiled with the compiler
%%% options `line_coverage' and `force_line_counters' so that they are
%%% line-counted from load time, regardless of the global coverage
%%% mode. The inner suite is then executed with ct:run_test/1 on a
%%% peer node (Common Test cannot be started recursively on the test
%%% node itself) with cth_coverage installed, and the coverdata files
%%% it produces are inspected.
%%%
%%% Note: per_testcase_isolation performs the inner test run;
%%% config_phase_buckets examines the config-phase buckets written by
%%% that same run, so the cases must execute in the order of all/0.
%%% @end
%%%-------------------------------------------------------------------
-module(ct_coverage_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0, init_per_suite/1, end_per_suite/1]).
-export([per_testcase_isolation/1, config_phase_buckets/1,
         report_generation/1]).

suite() ->
    [{timetrap, {minutes, 2}}].

all() ->
    [per_testcase_isolation,
     config_phase_buckets,
     report_generation].

init_per_suite(Config) ->
    case code:coverage_support() of
        false ->
            {skip, "native coverage not supported on this emulator "
                   "(requires the JIT)"};
        true ->
            PrivDir = proplists:get_value(priv_dir, Config),
            WorkDir = filename:join(PrivDir, "cov_work"),
            CovDir = filename:join(PrivDir, "coverdata"),
            InnerLogDir = filename:join(PrivDir, "inner_logs"),
            ok = filelib:ensure_path(WorkDir),
            ok = filelib:ensure_path(CovDir),
            ok = filelib:ensure_path(InnerLogDir),
            ok = write_and_compile(WorkDir, "covhelper.erl", helper_src()),
            ok = write_and_compile(WorkDir, "covinner_SUITE.erl", inner_src()),
            [{work_dir, WorkDir},
             {cov_dir, CovDir},
             {inner_logdir, InnerLogDir} | Config]
    end.

end_per_suite(_Config) ->
    ok.

%%%-------------------------------------------------------------------
%%% Test cases

%% Run the inner suite under cth_coverage on a peer node and check
%% that each test case gets its own coverdata file containing exactly
%% the helper lines that this test case (and nothing else) executed.
per_testcase_isolation(Config) ->
    WorkDir = proplists:get_value(work_dir, Config),
    CovDir = proplists:get_value(cov_dir, Config),
    InnerLogDir = proplists:get_value(inner_logdir, Config),
    %% The output directory is communicated to the hook through the
    %% CT_COVERAGE_DIR environment variable of the peer node.
    {ok, Peer, Node} = ?CT_PEER(#{args => ["-pa", WorkDir],
                                  env => [{"CT_COVERAGE_DIR", CovDir}]}),
    Opts = [{dir, WorkDir},
            {suite, covinner_SUITE},
            {ct_hooks, [cth_coverage]},
            {logdir, InnerLogDir},
            {auto_compile, false}],
    Result = try
                 erpc:call(Node, ct, run_test, [Opts], 90000)
             after
                 peer:stop(Peer)
             end,
    case Result of
        {2, 0, {0, 0}} ->
            ok;
        Other ->
            ct:fail({unexpected_inner_run_result, Other})
    end,
    TcA = helper_cov(read_coverdata(CovDir, "covinner_SUITE.tc_a.coverdata")),
    TcB = helper_cov(read_coverdata(CovDir, "covinner_SUITE.tc_b.coverdata")),
    %% tc_a executed covhelper:a/0 and nothing else in covhelper.
    assert_covered(helper_line(a), TcA, {covhelper, a, tc_a}),
    assert_not_covered([helper_line(b), helper_line(si), helper_line(gi)],
                       TcA, tc_a),
    %% tc_b executed covhelper:b/0 and nothing else in covhelper.
    assert_covered(helper_line(b), TcB, {covhelper, b, tc_b}),
    assert_not_covered([helper_line(a), helper_line(si), helper_line(gi)],
                       TcB, tc_b),
    ok.

%% Coverage executed in the configuration functions must end up in the
%% per-phase bucket files, not in any test case file. Examines the
%% coverdata written by the inner run in per_testcase_isolation.
config_phase_buckets(Config) ->
    CovDir = proplists:get_value(cov_dir, Config),
    Ips = helper_cov(
            read_coverdata(CovDir,
                           "covinner_SUITE.init_per_suite.coverdata")),
    assert_covered(helper_line(si), Ips, {covhelper, si, init_per_suite}),
    assert_not_covered([helper_line(a), helper_line(b), helper_line(gi)],
                       Ips, init_per_suite),
    Ipg = helper_cov(
            read_coverdata(CovDir,
                           "covinner_SUITE.g.init_per_group.coverdata")),
    assert_covered(helper_line(gi), Ipg, {covhelper, gi, init_per_group}),
    assert_not_covered([helper_line(a), helper_line(b), helper_line(si)],
                       Ipg, init_per_group),
    ok.

%% Render the per-testcase coverdata written by the inner run as an HTML
%% attribution report and verify that a source line is attributed to
%% exactly the test case that executed it.
report_generation(Config) ->
    CovDir = proplists:get_value(cov_dir, Config),
    WorkDir = proplists:get_value(work_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    OutDir = filename:join(PrivDir, "attrib_html"),
    Files = filelib:wildcard(filename:join(CovDir, "*.coverdata")),
    %% covhelper/covinner_SUITE live in a flat work dir, not an ebin/src
    %% app layout, so tell the reporter where their sources are.
    SMap = #{covhelper => filename:join(WorkDir, "covhelper.erl"),
             covinner_SUITE => filename:join(WorkDir, "covinner_SUITE.erl")},
    Manifest = filename:join(CovDir, "coverage.manifest"),
    Opts = [{source_map, SMap}, {title, "covinner attribution"}] ++
           [{manifest, Manifest} || filelib:is_regular(Manifest)],
    ok = ct_cover_to_html:convert(Files, OutDir, Opts),
    true = filelib:is_regular(filename:join(OutDir, "index.html")),
    HelperPage = filename:join(OutDir, "covhelper.html"),
    true = filelib:is_regular(HelperPage),
    {ok, Bin} = file:read_file(HelperPage),
    Html = unicode:characters_to_list(Bin),
    %% covhelper:a/0 sits alone on helper_line(a) and is executed only by
    %% tc_a; likewise b/0 by tc_b. The embedded attribution must say so.
    assert_attributed(Html, helper_line(a), "covinner_SUITE.tc_a"),
    assert_attributed(Html, helper_line(b), "covinner_SUITE.tc_b"),
    %% The source was supplied, so the page must carry it (not the fallback).
    nomatch = string:find(Html, "source not available"),
    ok.

assert_attributed(Html, Line, TestId) ->
    Expect = lists:flatten(
               io_lib:format("\"~w\":{\"n\":1,\"t\":[\"~ts\"]}", [Line, TestId])),
    case string:find(Html, Expect) of
        nomatch -> ct:fail({attribution_missing, Line, TestId, Expect});
        _ -> ok
    end.

%%%-------------------------------------------------------------------
%%% Generated modules
%%%
%%% Each covhelper function sits alone on its own line, so executing a
%%% function bumps the counter of exactly one known line.

helper_src() ->
    ["-module(covhelper).",
     "-export([a/0, b/0, si/0, gi/0]).",
     "a() -> ok_a.",
     "b() -> ok_b.",
     "si() -> ok_si.",
     "gi() -> ok_gi."].

inner_src() ->
    ["-module(covinner_SUITE).",
     "-export([suite/0, all/0, groups/0,",
     "         init_per_suite/1, end_per_suite/1,",
     "         init_per_group/2, end_per_group/2,",
     "         tc_a/1, tc_b/1]).",
     "suite() -> [{timetrap, {seconds, 30}}].",
     "all() -> [{group, g}].",
     "groups() -> [{g, [], [tc_a, tc_b]}].",
     "init_per_suite(Config) -> ok_si = covhelper:si(), Config.",
     "end_per_suite(_Config) -> ok.",
     "init_per_group(g, Config) -> ok_gi = covhelper:gi(), Config.",
     "end_per_group(g, _Config) -> ok.",
     "tc_a(_Config) -> ok_a = covhelper:a(), ok.",
     "tc_b(_Config) -> ok_b = covhelper:b(), ok."].

%% Source line number of a covhelper function's single-line body.
helper_line(Fun) ->
    line_of(atom_to_list(Fun) ++ "() ->", helper_src()).

line_of(Prefix, Lines) ->
    line_of(Prefix, Lines, 1).

line_of(Prefix, [Line | Lines], N) ->
    case lists:prefix(Prefix, Line) of
        true -> N;
        false -> line_of(Prefix, Lines, N + 1)
    end.

write_and_compile(Dir, FileName, SrcLines) ->
    File = filename:join(Dir, FileName),
    Src = [[Line, $\n] || Line <- SrcLines],
    ok = file:write_file(File, Src),
    case compile:file(File, [line_coverage, force_line_counters,
                             {outdir, Dir}, report, return]) of
        {ok, _Mod, _Warnings} ->
            ok;
        Error ->
            ct:fail({compile_failed, File, Error})
    end.

%%%-------------------------------------------------------------------
%%% Coverdata helpers

%% A coverdata file contains term_to_binary([{Module, [{Line, Count}]}])
%% with only non-zero lines.
read_coverdata(CovDir, Name) ->
    File = filename:join(CovDir, Name),
    case file:read_file(File) of
        {ok, Bin} ->
            binary_to_term(Bin);
        {error, Reason} ->
            ct:fail({missing_coverdata, File, Reason})
    end.

%% The [{Line, Count}] entry for covhelper; the generated inner suite
%% is instrumented too, so other modules may legitimately appear.
helper_cov(Coverage) when is_list(Coverage) ->
    case lists:keyfind(covhelper, 1, Coverage) of
        {covhelper, Lines} -> Lines;
        false -> []
    end.

assert_covered(Line, Lines, What) ->
    case lists:keyfind(Line, 1, Lines) of
        {Line, Count} when is_integer(Count), Count >= 1 ->
            ok;
        Other ->
            ct:fail({line_not_covered, What, Line, Other, Lines})
    end.

assert_not_covered(ExcludedLines, Lines, What) ->
    case [L || L <- ExcludedLines, lists:keymember(L, 1, Lines)] of
        [] ->
            ok;
        Leaked ->
            ct:fail({unexpected_lines_covered, What, Leaked, Lines})
    end.
