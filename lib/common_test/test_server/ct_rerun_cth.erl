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

%%% @doc Common Test hook that records failed test cases to a file, so that a
%%% surrounding runner (ts_run) can re-run only those cases and decide whether
%%% a failure is genuine or flaky. Only real test cases are recorded; failures
%%% in init/end_per_suite|group|testcase are ignored (they are not individually
%%% re-runnable).
-module(ct_rerun_cth).

%% CT hook callbacks
-export([init/2, on_tc_fail/4]).

-record(state, {file :: file:filename()}).

init(_Id, Opts) ->
    File = proplists:get_value(file, Opts),
    {ok, #state{file = File}}.

on_tc_fail(Suite, Case0, _Reason, State = #state{file = File}) ->
    Case = case Case0 of
               {C, _Group} -> C;
               C -> C
           end,
    case lists:member(Case, [init_per_suite, end_per_suite,
                             init_per_group, end_per_group,
                             init_per_testcase, end_per_testcase]) of
        true ->
            ok;
        false when File =/= undefined ->
            _ = file:write_file(File,
                                io_lib:format("~w ~w~n", [Suite, Case]),
                                [append]);
        false ->
            ok
    end,
    State.
