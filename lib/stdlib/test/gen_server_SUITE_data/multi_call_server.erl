%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2017-2025. All Rights Reserved.
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
-module(multi_call_server).

-behaviour(gen_server).

%% API
-export([start/2, multicall_suspender/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2]).

start(Tester, Name) ->
    {ok, Srv} = gen_server:start_link({local, Name},
                                      ?MODULE, [], []),
    Tester ! {self(), Srv},
    receive after infinity -> ok end.

multicall_suspender(Tester, Suspendee) ->
    true = erlang:suspend_process(Suspendee),
    receive
        {Tester, resume_it} ->
            erlang:resume_process(Suspendee)
    end.

init([]) ->
    {ok, []}.

handle_call(started_p, From, State) ->
    io:format("FRAZ"),
    {reply, ok, State};
handle_call({delayed_answer, T}, From, State) ->
    {noreply,{reply_to,From,State},T};
handle_call(ping, From, State) ->
    {reply, pong, State}.

handle_info(timeout, {reply_to, From, State}) ->
    gen_server:reply(From, delayed),
    {noreply, State}.
