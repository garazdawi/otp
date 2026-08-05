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
%%% Coverage executed in the configuration functions is captured into
%%% separate bucket files of the same format:
%%%
%%%    <dir>/<Suite>.init_per_suite.coverdata
%%%    <dir>/<Suite>.end_per_suite.coverdata
%%%    <dir>/<Suite>.<Group>.init_per_group.coverdata
%%%    <dir>/<Suite>.<Group>.end_per_group.coverdata
%%%
%%% Buckets with no executed lines produce no file.
%%%
%%% When the whole test run ends (hook terminate) a one-time line
%%% manifest is written:
%%%
%%%    <dir>/coverage.manifest
%%%
%%% containing term_to_binary([{Module, [Line]}]) with ALL
%%% instrumented lines of every instrumented module, including lines
%%% never executed, unioned across all nodes. ct_cover_to_lcov uses
%%% it to emit DA records with count 0 for un-hit lines, making the
%%% LCOV LF/LH totals meaningful.
%%%
%%% Reset and collection fan out to all connected peer nodes
%%% ([node() | nodes()]). The instrumented-modules probe runs on each
%%% node (each node has its own loaded set and its own counters), via
%%% erpc for remote nodes, and the per-node results are merged by
%%% summing the counts of each {Module, Line} across nodes. A node
%%% that does not support native coverage, cannot load this module, or
%%% fails/times out mid-call is skipped silently and never fails the
%%% test case. Peer nodes stopped before the coverage is read (for
%%% example in the middle of a test case) are not captured; hidden
%%% nodes are not visited.
%%%
%%% TODO: parallel-group serialization. In a parallel group, test
%%%       cases overlap in time, so per-case reset/read attributes
%%%       coverage to the wrong case. Detect parallel groups (see
%%%       cth_log_redirect's use of tc_group_properties) and fall back
%%%       to one bucket for the whole group, or serialize collection.
%%% TODO: native (C) coverage. When the emulator is built with gcov,
%%%       dump/reset C-level coverage per test case as well (via
%%%       erts_debug coverage support), so ERTS changes can be mapped
%%%       to the test cases that exercise them.

%% CTH Callbacks
-export([id/1, init/2,
         pre_init_per_suite/3,
         post_init_per_suite/4,
         pre_end_per_suite/3,
         post_end_per_suite/4,
         pre_init_per_group/4,
         post_init_per_group/5,
         pre_end_per_group/4,
         post_end_per_group/5,
         pre_init_per_testcase/4,
         post_end_per_testcase/5,
         terminate/1]).

%% Exported only for erpc use on peer nodes; not part of the hook API.
-export([reset_local/1, collect_local/1, manifest_local/1]).

-behaviour(ct_hooks).

-record(state, {enabled = false :: boolean(),
                dir :: file:filename() | undefined,
                level = line :: line}).

-define(DEFAULT_DIR, "ct_coverage").

%% Upper bound for each remote reset/collect call so that a wedged
%% peer node cannot hang the test run.
-define(REMOTE_TIMEOUT, 15000).

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

%%%-----------------------------------------------------------------
%%% Suite configuration buckets

pre_init_per_suite(_Suite, InitData,
                   #state{enabled = true, level = Level} = State) ->
    ok = reset_all(Level),
    {InitData, State};
pre_init_per_suite(_Suite, InitData, State) ->
    {InitData, State}.

post_init_per_suite(Suite, _Config, Return,
                    #state{enabled = true, level = Level} = State) ->
    ok = write_coverdata(State#state.dir, [Suite, init_per_suite],
                         collect_all(Level)),
    {Return, State};
post_init_per_suite(_Suite, _Config, Return, State) ->
    {Return, State}.

pre_end_per_suite(_Suite, EndData,
                  #state{enabled = true, level = Level} = State) ->
    ok = reset_all(Level),
    {EndData, State};
pre_end_per_suite(_Suite, EndData, State) ->
    {EndData, State}.

post_end_per_suite(Suite, _Config, Return,
                   #state{enabled = true, level = Level} = State) ->
    ok = write_coverdata(State#state.dir, [Suite, end_per_suite],
                         collect_all(Level)),
    {Return, State};
post_end_per_suite(_Suite, _Config, Return, State) ->
    {Return, State}.

%%%-----------------------------------------------------------------
%%% Group configuration buckets

pre_init_per_group(_Suite, _Group, InitData,
                   #state{enabled = true, level = Level} = State) ->
    ok = reset_all(Level),
    {InitData, State};
pre_init_per_group(_Suite, _Group, InitData, State) ->
    {InitData, State}.

post_init_per_group(Suite, Group, _Config, Return,
                    #state{enabled = true, level = Level} = State) ->
    ok = write_coverdata(State#state.dir, [Suite, Group, init_per_group],
                         collect_all(Level)),
    {Return, State};
post_init_per_group(_Suite, _Group, _Config, Return, State) ->
    {Return, State}.

pre_end_per_group(_Suite, _Group, EndData,
                  #state{enabled = true, level = Level} = State) ->
    ok = reset_all(Level),
    {EndData, State};
pre_end_per_group(_Suite, _Group, EndData, State) ->
    {EndData, State}.

post_end_per_group(Suite, Group, _Config, Return,
                   #state{enabled = true, level = Level} = State) ->
    ok = write_coverdata(State#state.dir, [Suite, Group, end_per_group],
                         collect_all(Level)),
    {Return, State};
post_end_per_group(_Suite, _Group, _Config, Return, State) ->
    {Return, State}.

%%%-----------------------------------------------------------------
%%% Test cases

pre_init_per_testcase(_Suite, _TC, Config,
                      #state{enabled = true, level = Level} = State) ->
    ok = reset_all(Level),
    {Config, State};
pre_init_per_testcase(_Suite, _TC, Config, State) ->
    {Config, State}.

post_end_per_testcase(Suite, TC, _Config, Return,
                      #state{enabled = true, level = Level} = State) ->
    ok = write_coverdata(State#state.dir, [Suite, TC], collect_all(Level)),
    {Return, State};
post_end_per_testcase(_Suite, _TC, _Config, Return, State) ->
    {Return, State}.

terminate(#state{enabled = true, dir = Dir, level = Level}) ->
    ok = write_manifest(Dir, manifest_all(Level));
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

%% The main test node and all connected (visible) peer nodes.
coverage_nodes() ->
    [node() | nodes()].

%% Reset the coverage counters on every node.
reset_all(Level) ->
    _ = [reset_on_node(N, Level) || N <- coverage_nodes()],
    ok.

%% Collect the coverage from every node and merge the results, summing
%% the counts of each {Module, Line} across nodes. A line is covered
%% if any node executed it.
collect_all(Level) ->
    merge_coverage([collect_on_node(N, Level) || N <- coverage_nodes()]).

reset_on_node(Node, Level) when Node =:= node() ->
    reset_local(Level);
reset_on_node(Node, Level) ->
    %% A node that is down, does not have this module, or does not
    %% support coverage must never fail the test case; skip it.
    try erpc:call(Node, ?MODULE, reset_local, [Level], ?REMOTE_TIMEOUT)
    catch _:_ -> ok
    end.

collect_on_node(Node, Level) when Node =:= node() ->
    collect_local(Level);
collect_on_node(Node, Level) ->
    try erpc:call(Node, ?MODULE, collect_local, [Level], ?REMOTE_TIMEOUT)
    catch _:_ -> []
    end.

%% Runs on the node whose coverage is reset (locally or via erpc).
reset_local(Level) ->
    case code:coverage_support() of
        true ->
            _ = [reset_coverage(M) || M <- instrumented_modules(Level)],
            ok;
        false ->
            ok
    end.

%% Runs on the node whose coverage is read (locally or via erpc).
%% The instrumented-modules probe is re-run on every collection
%% instead of reusing the list from the reset; modules loaded since
%% the reset are instrumented from load time and belong to this
%% bucket too.
collect_local(Level) ->
    case code:coverage_support() of
        true ->
            lists:filtermap(
              fun(M) ->
                      case covered_lines(Level, M) of
                          [] -> false;
                          Lines -> {true, {M, Lines}}
                      end
              end, instrumented_modules(Level));
        false ->
            []
    end.

%%%-----------------------------------------------------------------
%%% Line manifest (all instrumented lines, executed or not)

%% The full instrumented-line set from every node, unioned per module.
manifest_all(Level) ->
    merge_manifest([manifest_on_node(N, Level) || N <- coverage_nodes()]).

manifest_on_node(Node, Level) when Node =:= node() ->
    manifest_local(Level);
manifest_on_node(Node, Level) ->
    %% As for collection: a node that is down, does not have this
    %% module, or does not support coverage is skipped silently.
    try erpc:call(Node, ?MODULE, manifest_local, [Level], ?REMOTE_TIMEOUT)
    catch _:_ -> []
    end.

%% Runs on the node whose instrumented-line set is read (locally or
%% via erpc). Same probe as collect_local/1 but keeps EVERY
%% instrumented line, not only those with a non-zero count.
manifest_local(Level) ->
    case code:coverage_support() of
        true ->
            lists:filtermap(
              fun(M) ->
                      case instrumented_lines(Level, M) of
                          [] -> false;
                          Lines -> {true, {M, Lines}}
                      end
              end, instrumented_modules(Level));
        false ->
            []
    end.

%% Every instrumented line of M, regardless of execution count.
instrumented_lines(Level, M) ->
    try code:get_coverage(Level, M) of
        LineData -> [Line || {Line, _Cov} <- LineData, is_integer(Line)]
    catch
        _:_ -> []
    end.

%% Union per-node manifests per module. As in merge_coverage/1,
%% entries without the expected shape are ignored.
merge_manifest(PerNode) ->
    Merged = lists:foldl(fun merge_manifest_node/2, #{}, PerNode),
    lists:sort([{M, lists:usort(Lines)} || {M, Lines} <- maps:to_list(Merged)]).

merge_manifest_node(NodeLines, Acc) when is_list(NodeLines) ->
    lists:foldl(fun merge_manifest_module/2, Acc, NodeLines);
merge_manifest_node(_Bad, Acc) ->
    Acc.

merge_manifest_module({M, Lines}, Acc) when is_atom(M), is_list(Lines) ->
    Good = [Line || Line <- Lines, is_integer(Line)],
    maps:update_with(M, fun(Ls) -> Good ++ Ls end, Good, Acc);
merge_manifest_module(_Bad, Acc) ->
    Acc.

%% Write the one-time line manifest, read by ct_cover_to_lcov to emit
%% DA:<Line>,0 records for lines never executed. An empty manifest
%% (no instrumented modules) produces no file.
write_manifest(_Dir, []) ->
    ok;
write_manifest(Dir, Manifest) ->
    File = filename:join(Dir, "coverage.manifest"),
    _ = filelib:ensure_path(Dir),
    case file:write_file(File, term_to_binary(Manifest)) of
        ok ->
            ok;
        {error, Reason} ->
            logger:warning("cth_coverage: failed to write ~ts: ~p",
                           [File, Reason]),
            ok
    end.

%% Merge per-node sparse coverage lists, summing counts per
%% {Module, Line}. Entries that do not have the expected shape (for
%% example from a peer running a different version of this module)
%% are ignored rather than failing the test case.
merge_coverage([NodeCov]) ->
    %% Common case: no peer nodes.
    NodeCov;
merge_coverage(PerNode) ->
    Merged = lists:foldl(fun merge_node/2, #{}, PerNode),
    lists:sort([{M, lists:sort(maps:to_list(Lines))}
                || {M, Lines} <- maps:to_list(Merged)]).

merge_node(NodeCov, Acc) when is_list(NodeCov) ->
    lists:foldl(fun merge_module/2, Acc, NodeCov);
merge_node(_Bad, Acc) ->
    Acc.

merge_module({M, Lines}, Acc) when is_atom(M), is_list(Lines) ->
    LineMap =
        lists:foldl(
          fun({Line, Count}, LM) when is_integer(Line), is_integer(Count) ->
                  maps:update_with(Line, fun(C) -> C + Count end, Count, LM);
             (_Bad, LM) ->
                  LM
          end, maps:get(M, Acc, #{}), Lines),
    Acc#{M => LineMap};
merge_module(_Bad, Acc) ->
    Acc.

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

%% Write one coverage bucket. NameParts (atoms) are joined with "."
%% into the file name, e.g. [Suite, TC] -> "<Suite>.<TC>.coverdata".
%% Empty buckets produce no file.
write_coverdata(_Dir, _NameParts, []) ->
    ok;
write_coverdata(Dir, NameParts, Coverage) ->
    Name = lists:flatten(
             [lists:join($., [io_lib:format("~tw", [P]) || P <- NameParts]),
              ".coverdata"]),
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
