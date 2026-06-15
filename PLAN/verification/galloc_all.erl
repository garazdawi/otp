-module(galloc_all).
-export([run/0]).
build(0, A) -> A; build(N, A) -> build(N-1, [N|A]).
run() ->
    erts_debug:set_internal_state(available_internal_state, true),
    erts_debug:set_internal_state(alloc_profile_sites, true),     %% reset sites
    erts_debug:set_internal_state(alloc_profile_all, true),       %% GLOBAL on
    Parent = self(),
    %% Worker does NOT call set(alloc_profile) — relies on global default.
    spawn(fun() -> _ = build(100000, []), Parent ! done end),
    receive done -> ok end,
    erts_debug:set_internal_state(alloc_profile_all, false),      %% GLOBAL off
    Sites = erts_debug:get_internal_state(alloc_profile_sites),
    W = [Words || {{galloc_all, build, 2}, Words} <- Sites],
    io:format("worker (no per-proc enable) attributed build/2 = ~p (exp 200000)~n", [W]),
    halt().
