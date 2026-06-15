-module(galloc_sites).
-export([run/0, alpha/1, beta/1, gamma/1]).
alpha(0) -> []; alpha(N) -> [{N,N} | alpha(N-1)].          %% tuples+cons
beta(0) -> []; beta(N) -> [N | beta(N-1)].                 %% cons only
gamma(0) -> ok; gamma(N) -> _ = erlang:make_tuple(10, N), gamma(N-1). %% C-path (not per-site)

run() ->
    erts_debug:set_internal_state(available_internal_state, true),
    erts_debug:set_internal_state(alloc_profile_sites, true),  %% reset sites
    erts_debug:set_internal_state(alloc_profile, true),        %% enable on self
    _ = alpha(100000),
    _ = beta(50000),
    _ = gamma(50000),
    Sites = erts_debug:get_internal_state(alloc_profile_sites),
    Mine = [{F, W} || {{galloc_sites, F, _A}, W} <- Sites],
    io:format("per-function sites (this module):~n"),
    [io:format("  ~-8w ~p words~n", [F, W]) || {F, W} <- lists:sort(Mine)],
    halt().
