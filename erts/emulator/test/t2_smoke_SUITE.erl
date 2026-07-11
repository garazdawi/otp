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
%%% T2-Full smoke coverage (PLAN/T2FULL/18 waves W3/W4).
%%%
%%% Every case runs against a peer node so the T2 env gates never leak
%%% into the test node. The cases bank, per plan layer:
%%%
%%%   selftests        L2/L3  boot-time HIR/ranges/loop (+ emit) self-
%%%                           tests; a failure aborts the peer's boot,
%%%                           so "peer boots" is the assertion. On
%%%                           non-aarch64 this pins the emit stub's
%%%                           no-op contract.
%%%   build_isel_sweep L5     reconstruct+validate SSA for every
%%%                           function of a stdlib corpus via the
%%%                           {t2_build_ssa,M,F,A} debug BIF; zero
%%%                           build failures, some successes. The peer
%%%                           also runs the load-time T2_BUILD/T2_ISEL
%%%                           sweep, so an isel crash fails the case.
%%%   retain_noop      -      T2_RETAIN=1 is safe everywhere: stats
%%%                           BIF live, and on non-aarch64 exactly
%%%                           zero installs (the backend is
%%%                           aarch64-only).
%%%   organic_tier_up  L7     counter-trip -> queue -> async worker ->
%%%                           install, on a pinned hot loop with a
%%%                           forced flat threshold. aarch64-only;
%%%                           loud skip elsewhere.
%%%-------------------------------------------------------------------
-module(t2_smoke_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([selftests/1,
         build_isel_sweep/1,
         retain_noop/1,
         organic_tier_up/1]).

%% Run on the peer (by name, over erpc).
-export([hot_sum/2, sweep_module/1]).

suite() ->
    [{ct_hooks, [ts_install_cth]},
     {timetrap, {minutes, 2}}].

all() ->
    [selftests,
     build_isel_sweep,
     retain_noop,
     organic_tier_up].

init_per_suite(Config) ->
    case erlang:system_info(emu_flavor) of
        jit -> Config;
        _ -> {skip, "T2 exists only in the JIT emulator"}
    end.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TC, Config) ->
    Config.

end_per_testcase(_TC, Config) ->
    case proplists:get_value(peer, Config) of
        undefined -> ok;
        Peer -> catch peer:stop(Peer)
    end,
    ok.

%%%-------------------------------------------------------------------
%%% Cases
%%%-------------------------------------------------------------------

%% L2/L3: the boot-time self-tests. Failures call
%% erts_exit(ERTS_ABORT_EXIT, ...), so the peer failing to boot is the
%% failure mode; a booted peer that can evaluate code passed them.
selftests(_Config) ->
    {ok, Peer, Node} =
        peer:start(#{name => peer:random_name(?MODULE),
                     env => [{"T2_SELFTEST", "1"},
                             {"T2_EMIT_SELFTEST", "1"}]}),
    try
        4 = erpc:call(Node, erlang, '+', [2, 2]),
        jit = erpc:call(Node, erlang, system_info, [emu_flavor])
    after
        peer:stop(Peer)
    end,
    ok.

%% L5: SSA reconstruction over a corpus, asserted through the debug
%% BIF: zero build failures, a healthy number of reconstructions. The
%% load-time sweep (T2_BUILD/T2_ISEL) runs on the same peer, so an
%% isel/build crash at module load also fails the case.
build_isel_sweep(_Config) ->
    PA = filename:dirname(code:which(?MODULE)),
    {ok, Peer, Node} =
        peer:start(#{name => peer:random_name(?MODULE),
                     env => [{"T2_RETAIN", "1"},
                             {"T2_BUILD", "1"},
                             {"T2_ISEL", "1"}],
                     args => ["-pa", PA]}),
    try
        _ = erpc:call(Node, erts_debug, set_internal_state,
                      [available_internal_state, true]),
        Corpus = [lists, proplists, orddict, gb_trees, gb_sets, queue,
                  string, binary, base64, unicode_util],
        Results =
            [erpc:call(Node, ?MODULE, sweep_module, [M]) || M <- Corpus],
        Ok = lists:sum([O || {O, _NE, _F} <- Results]),
        NotElig = lists:sum([NE || {_O, NE, _F} <- Results]),
        Failed = lists:append([F || {_O, _NE, F} <- Results]),
        ct:log("reconstructed=~p not_eligible=~p failed=~p",
               [Ok, NotElig, Failed]),
        %% Zero build failures, and the sweep must not be vacuous.
        [] = Failed,
        true = Ok > 0
    after
        peer:stop(Peer)
    end,
    ok.

%% T2_RETAIN=1 must be safe on every arch: the stats BIF is live and,
%% off aarch64, nothing can install (the backend does not exist), so
%% the install counter stays exactly zero.
retain_noop(_Config) ->
    {ok, Peer, Node} =
        peer:start(#{name => peer:random_name(?MODULE),
                     env => [{"T2_RETAIN", "1"}]}),
    try
        _ = erpc:call(Node, erts_debug, set_internal_state,
                      [available_internal_state, true]),
        _ = [_ = erpc:call(Node, code, ensure_loaded, [M])
             || M <- [lists, maps, binary, string]],
        Stats = erpc:call(Node, erts_debug, get_internal_state, [t2_stats]),
        true = is_tuple(Stats),
        Installed = element(3, Stats),
        case is_aarch64() of
            false -> 0 = Installed;
            true -> true = is_integer(Installed)
        end
    after
        peer:stop(Peer)
    end,
    ok.

%% L7: the asynchronous tier-up path end to end — counter trip on a
%% pinned scheduler, work queue, async compile, install — with a
%% forced flat threshold for determinism. The loop's results must stay
%% correct across the install.
organic_tier_up(_Config) ->
    case is_aarch64() of
        false ->
            {skip, "T2 backend emits on aarch64 only"};
        true ->
            organic_tier_up_run()
    end.

organic_tier_up_run() ->
    PA = filename:dirname(code:which(?MODULE)),
    {ok, Peer, Node} =
        peer:start(#{name => peer:random_name(?MODULE),
                     env => [{"T2_RETAIN", "1"},
                             {"T2_TIER_THRESHOLD", "50"}],
                     args => ["-pa", PA]}),
    try
        _ = erpc:call(Node, erts_debug, set_internal_state,
                      [available_internal_state, true]),
        {module, ?MODULE} = erpc:call(Node, code, ensure_loaded, [?MODULE]),
        List = lists:seq(1, 1000),
        Expected = lists:sum(List),
        %% Drive the loop on scheduler 1 (the profiling counters are
        %% scheduler-1-only) until the tier installs, then re-check the
        %% result on the installed code.
        Driver =
            fun Drive(0) ->
                    error(no_install);
                Drive(N) ->
                    [Expected = ?MODULE:hot_sum(List, 0)
                     || _ <- lists:seq(1, 100)],
                    Stats = erts_debug:get_internal_state(t2_tier_stats),
                    case element(6, Stats) of
                        Installed when Installed >= 1 ->
                            {ok, Stats};
                        _ ->
                            receive after 100 -> ok end,
                            Drive(N - 1)
                    end
            end,
        Me = self(),
        {Pid, Mon} =
            spawn_monitor(
              fun() ->
                      R = erpc:call(
                            Node, erlang, apply,
                            [fun() ->
                                     {P, M} =
                                         spawn_opt(
                                           fun() ->
                                                   exit({done, Driver(100)})
                                           end,
                                           [monitor, {scheduler, 1}]),
                                     receive
                                         {'DOWN', M, process, P, Why} -> Why
                                     end
                             end, []]),
                      Me ! {self(), R}
              end),
        Result =
            receive
                {Pid, R} -> R;
                {'DOWN', Mon, process, Pid, Crash} -> error({driver, Crash})
            end,
        {done, {ok, TierStats}} = Result,
        ct:log("t2_tier_stats after install: ~p", [TierStats]),
        %% And the loop still computes the right answer on T2 code.
        Expected = erpc:call(Node, ?MODULE, hot_sum, [List, 0])
    after
        peer:stop(Peer)
    end,
    ok.

%%%-------------------------------------------------------------------
%%% Peer-side helpers (called by name over erpc)
%%%-------------------------------------------------------------------

%% A T2-eligible self-recursive integer tail loop (the tier's bread
%% and butter): trips the entry counter and gets a real T2 blob.
hot_sum([], Acc) ->
    Acc;
hot_sum([H | T], Acc) ->
    hot_sum(T, Acc + H).

%% Reconstruct+validate every function of M through the debug BIF.
%% Returns {OkCount, NotEligibleCount, FailedList}.
sweep_module(M) ->
    {module, M} = code:ensure_loaded(M),
    Funcs = M:module_info(functions),
    lists:foldl(
      fun({F, A}, {Ok, NE, Failed}) ->
              case erts_debug:get_internal_state({t2_build_ssa, M, F, A}) of
                  undefined ->
                      %% No retention for the module at all: count as
                      %% a failure so the sweep cannot go vacuous.
                      {Ok, NE, [{M, no_retention} | Failed]};
                  {error, not_eligible} ->
                      {Ok, NE + 1, Failed};
                  {error, not_found} ->
                      {Ok, NE, [{M, F, A, not_found} | Failed]};
                  {error, _, Reason} ->
                      {Ok, NE, [{M, F, A, Reason} | Failed]};
                  _Ssa ->
                      {Ok + 1, NE, Failed}
              end
      end, {0, 0, []}, Funcs).

is_aarch64() ->
    Arch = erlang:system_info(system_architecture),
    string:find(Arch, "aarch64") =/= nomatch orelse
        string:find(Arch, "arm64") =/= nomatch.
