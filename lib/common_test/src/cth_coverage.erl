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
-module(cth_coverage).
-moduledoc false.

%%% Common Test hook capturing per-testcase native line coverage.
%%%
%%% For every test case, the coverage counters of all natively
%%% instrumented modules (see code:set_coverage_mode/1 and the
%%% `line_coverage' compiler option) are reset before
%%% init_per_testcase and read after end_per_testcase. The lines
%%% executed by the test case (including its init/end_per_testcase)
%%% are written to one file per test case:
%%%
%%%    <dir>/<Suite>.<Case>.coverdata
%%%
%%% containing term_to_binary([{Module, [{Line, Count}]}]) with only
%%% lines that have a non-zero count (sparse). The output directory is
%%% taken from the hook option {dir, Dir}, or the OS environment
%%% variable CT_COVERAGE_DIR, defaulting to "./ct_coverage".
%%%
%%% This is v1 and only covers code running on the main test node.
%%%
%%% TODO: peer-node fan-out. Coverage executed on peer nodes started
%%%       by a test case is not captured. Fan out
%%%       code:reset_coverage/1 and code:get_coverage/2 via erpc to
%%%       nodes() (peers must run with the same coverage mode) and
%%%       merge the per-node results.
%%% TODO: parallel-group serialization. In a parallel group, test
%%%       cases overlap in time, so per-case reset/read attributes
%%%       coverage to the wrong case. Detect parallel groups (see
%%%       cth_log_redirect's use of tc_group_properties) and fall back
%%%       to one bucket for the whole group, or serialize collection.
%%% TODO: config-phase buckets. Coverage executed in
%%%       init/end_per_suite and init/end_per_group is currently lost
%%%       (reset at the next pre_init_per_testcase). Add the
%%%       pre/post_init_per_suite and group callbacks and write
%%%       "<Suite>.init_per_suite.coverdata"-style buckets.
%%% TODO: native (C) coverage. When the emulator is built with gcov,
%%%       dump/reset C-level coverage per test case as well (via
%%%       erts_debug coverage support), so ERTS changes can be mapped
%%%       to the test cases that exercise them.

%% CTH Callbacks
-export([id/1, init/2,
         pre_init_per_testcase/4,
         post_end_per_testcase/5,
         terminate/1]).

-behaviour(ct_hooks).

-record(state, {enabled = false :: boolean(),
                dir :: file:filename() | undefined,
                level = line :: line,
                modules = [] :: [module()]}).

-define(DEFAULT_DIR, "ct_coverage").

id(_Opts) ->
    ?MODULE.

init(_Id, Opts) ->
    %% Gate only on coverage_support(): modules compiled with
    %% +force_line_counters are line-counted regardless of the global
    %% coverage mode, so the per-module probe (instrumented_modules/1)
    %% is what decides what to collect -- not code:get_coverage_mode/0.
    case code:coverage_support() of
        true ->
            Dir = filename:absname(output_dir(Opts)),
            _ = filelib:ensure_path(Dir),
            {ok, #state{enabled = true, dir = Dir, level = line}};
        false ->
            %% No native coverage on this emulator (needs the JIT).
            %% Log once and become a no-op.
            logger:notice("cth_coverage: native coverage not supported on "
                          "this emulator; per-testcase coverage collection "
                          "is disabled"),
            {ok, #state{enabled = false}}
    end.

pre_init_per_testcase(_Suite, _TC, Config,
                      #state{enabled = true, level = Level} = State) ->
    Modules = instrumented_modules(Level),
    _ = [reset_coverage(M) || M <- Modules],
    {Config, State#state{modules = Modules}};
pre_init_per_testcase(_Suite, _TC, Config, State) ->
    {Config, State}.

post_end_per_testcase(Suite, TC, _Config, Return,
                      #state{enabled = true, level = Level} = State) ->
    %% Re-scan for instrumented modules instead of using the list from
    %% pre_init_per_testcase; modules loaded during the test case are
    %% instrumented from load time and belong to this test case too.
    Coverage =
        lists:filtermap(
          fun(M) ->
                  case covered_lines(Level, M) of
                      [] -> false;
                      Lines -> {true, {M, Lines}}
                  end
          end, instrumented_modules(Level)),
    ok = write_coverdata(State#state.dir, Suite, TC, Coverage),
    {Return, State};
post_end_per_testcase(_Suite, _TC, _Config, Return, State) ->
    {Return, State}.

terminate(_State) ->
    ok.

%%%-----------------------------------------------------------------
%%% Internal functions

output_dir(Opts) ->
    case proplists:get_value(dir, Opts) of
        undefined ->
            case os:getenv("CT_COVERAGE_DIR") of
                false -> ?DEFAULT_DIR;
                EnvDir -> EnvDir
            end;
        Dir ->
            Dir
    end.

%% All currently loaded modules that carry native coverage data.
%% Only instrumented modules return data from code:get_coverage/2;
%% all others raise badarg. Deliberately not cached across test cases
%% since modules can be loaded (or reloaded) at any time during a run.
instrumented_modules(Level) ->
    [M || {M, _File} <- code:all_loaded(), has_coverage(Level, M)].

has_coverage(Level, M) ->
    try code:get_coverage(Level, M) of
        _ -> true
    catch
        _:_ -> false
    end.

reset_coverage(M) ->
    %% The module may have been purged since it was listed.
    try code:reset_coverage(M)
    catch _:_ -> ok
    end.

%% Lines with a non-zero execution count. In line_counters mode
%% code:get_coverage(line, M) returns [{Line, Count}]; in line mode it
%% returns [{Line, boolean()}], in which case true is treated as 1.
covered_lines(Level, M) ->
    try code:get_coverage(Level, M) of
        LineData ->
            [{Line, cov_count(Cov)} ||
                {Line, Cov} <- LineData, Cov =/= false, Cov =/= 0]
    catch
        _:_ -> []
    end.

cov_count(true) -> 1;
cov_count(N) when is_integer(N) -> N.

write_coverdata(_Dir, _Suite, _TC, []) ->
    ok;
write_coverdata(Dir, Suite, TC, Coverage) ->
    Name = lists:flatten(io_lib:format("~tw.~tw.coverdata", [Suite, TC])),
    File = filename:join(Dir, Name),
    _ = filelib:ensure_path(Dir),
    case file:write_file(File, term_to_binary(Coverage)) of
        ok ->
            ok;
        {error, Reason} ->
            logger:warning("cth_coverage: failed to write ~ts: ~p",
                           [File, Reason]),
            ok
    end.
