%%
%% %CopyrightBegin%
%%                                                                                                                                   
%% Copyright Ericsson AB 2009. All Rights Reserved.                                                                             
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
%%% File    : ehb.erl
%%% Author  : Rickard Green
%%% Description : An Erlang HackBench version
%%%
%%% Created : 24 Aug 2009 by Rickard Green
%%%-------------------------------------------------------------------
-module(ehb).

-export([go/0, go/2]).
-export([benchmark_arguments/0, benchmark_unit/0]).


-define(ACK, 100).
-define(DATA, {a,b,c,d,e,f,g,h,i,j,k,l}). %% 104 bytes on a 64-bit machine
-define(GSIZE, 20).

benchmark_arguments() ->
    [{go, [N,M]} || N <- [100, 500], M <- [1000, 500]].

benchmark_unit() -> "ms".


go() ->
    go(100, 1000).

go(Groups, Loops) ->
    Master = self(),
    Start = now(),
    Gs = lists:map(fun (_) -> group(Master, Loops) end,
		   lists:seq(1, Groups)),
    lists:foreach(fun (G) -> receive {G, ready} -> ok end end, Gs),
    lists:foreach(fun (G) -> G ! {Master, go} end, Gs),
    lists:foreach(fun (G) -> receive {G, done} -> ok end end, Gs),
    End = now(),
    timer:now_diff(End, Start)/1000.


group(Master, Loop) ->
    spawn_link(
      fun () ->
	      GMaster = self(),
	      Rs = lists:map(fun (_) ->
				     spawn_link(fun () ->
							receiver(GMaster,
								 ?GSIZE)
						end)
			     end,
			     lists:seq(1, ?GSIZE)),
	      Ss = lists:map(fun (_) ->
				     spawn_link(fun () ->
							receive
							    {GMaster, go} ->
								sender(Rs,Loop)
							end
						end)
			     end,
			     lists:seq(1, ?GSIZE)),
	      Master ! {self(), ready},
	      receive {Master, go} -> ok end,
	      lists:foreach(fun (S) -> S ! {GMaster, go} end, Ss),
	      lists:foreach(fun (R) -> receive {R, done} -> ok end end, Rs),
	      Master ! {self(), done}
      end).

sender(Rs, 0) ->
    lists:foreach(fun (R) -> R ! done end, Rs);
sender(Rs, Loop) when Loop > ?ACK ->
    sender_ack(Rs, ?ACK),
    sender(Rs, Loop - ?ACK);
sender(Rs, Loop) ->
    lists:foreach(fun (R) -> R ! ?DATA end, Rs),
    sender(Rs, Loop-1).

sender_ack(Rs, 2) ->
    lists:foreach(fun (R) ->
			  R ! ?DATA,
			  R ! {self(), are_you_keeping_up}
		  end,
		  Rs),
    lists:foreach(fun (R) ->
			  receive {R, i_am_keeping_up} -> ok end,
			  R ! ?DATA
		  end, Rs),
    ok;
sender_ack(Rs, N) ->
    lists:foreach(fun (R) -> R ! ?DATA end, Rs),
    sender_ack(Rs, N-1).

receiver(GMaster, SendersLeft) ->
    receive
	done ->
	    case SendersLeft of
		1 -> GMaster ! {self(), done};
		_ -> receiver(GMaster, SendersLeft - 1)
	    end;
	Msg ->
	    case Msg of
		{From, are_you_keeping_up} -> From ! {self(), i_am_keeping_up};
		_ -> ok
	    end,
	    receiver(GMaster, SendersLeft)
    end.
