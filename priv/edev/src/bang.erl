%%
%% %CopyrightBegin%
%%                                                                                                                                   
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.                                                                             
%% 
%% The contents of this file are subject to the Erlang Public License,                                                               
%% Version 1.1, (the "License"); you may not use this file except in                                                                 
%% compliance with the License. You should have received a copy of the                                                               
%% Erlang Public License along with this software. If not, it can be                                                                 
%% retrieved online at http://www.erlang.org/.                                                                                       
%% 
%% Software distributed under the License is distributed on an "AS IS"                                                               
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See                                                               
%% the License for the specific language governing rights and limitations                                                            
%% under the License.                                                                                                                
%% 
%% %CopyrightEnd%

%%%-------------------------------------------------------------------
%%% File    : bang.erl
%%% Author  : Rickard Green <rickard.s.green@ericsson.com>
%%% Description : 
%%%
%%% Created :  3 Dec 2008 by Rickard Green <rickard.s.green@ericsson.com>
%%%-------------------------------------------------------------------
-module(bang).
-export([bang/2]).

-export([benchmark_arguments/0, benchmark_unit/0]).

benchmark_arguments() ->
    [{bang, [S,M]} || S <- [1000, 10], M <- [10, 1000]].

benchmark_unit() -> "ms".



bang(S, M) ->
    Parent = self(),
    Done   = make_ref(),
    Bang   = {make_ref(),make_ref(),make_ref(),make_ref(),make_ref()},
    Rec    = spawn_opt(fun () -> rec(Bang, S*M), Parent ! Done end, [link]),
    T0     = now(),
    lists:foreach(fun 
	(_) ->
	    spawn_link(fun () -> send(Rec, Bang, M) end)
	end, lists:seq(1, S)),
    receive Done -> ok end,
    T1     = now(),
    timer:now_diff(T1, T0)/1000.

send(_T, _M, 0) -> ok;
send(T, M, N)   -> T ! M, send(T, M, N-1).

rec(_M, 0) -> ok;
rec(M, N)  -> receive M -> rec(M, N-1) end.
