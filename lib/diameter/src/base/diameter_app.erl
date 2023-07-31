%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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

-module(diameter_app).
-callback handle_request(Packet :: term(), SvcName :: term(), Peer :: term()) -> Action :: term().
-callback handle_error(Reason :: term(), Request :: term(), SvcName :: term(), Peer :: term()) -> Result :: term().
-callback handle_answer(Packet :: term(), Request :: term(), SvcName :: term(), Peer :: term()) -> Result :: term().
-callback prepare_retransmit(Packet :: term(), SvcName :: term(), Peer :: term()) -> Action :: term().

-callback prepare_request(Packet :: term(), SvcName :: term(), Peer :: term()) -> Action :: term().
-callback pick_peer(term(), term(), term(), term()) -> term().
-callback peer_down(SvcName :: term(), Peer :: term(), State :: term()) -> NewState :: term().
-callback peer_up(SvcName :: term(), Peer :: term(), State :: term()) -> NewState :: term().
-behaviour(application).

%% application callbacks
-export([start/2,
         stop/1]).

%% start/2

start(_Type, _Args) ->
    diameter_sup:start_link().

%% stop/1

stop(_) ->
    ok.
