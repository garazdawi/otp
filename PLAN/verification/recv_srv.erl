%% Minimal gen_server for T2FULL M0.R workload 1 (ping-pong under load).
-module(recv_srv).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() -> gen_server:start_link(?MODULE, [], []).

init([]) -> {ok, 0}.

%% Trivial work so the receive path, not the handler, dominates.
handle_call({work, C}, _From, N) -> {reply, {ok, C}, N + 1}.

handle_cast(_, N) -> {noreply, N}.
