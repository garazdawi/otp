-module(galloc_test).
-export([run/0, build/2]).

build(0, Acc) -> Acc;
build(N, Acc) -> build(N - 1, [N | Acc]).

enable() -> erts_debug:set_internal_state(alloc_profile, true).
disable() -> erts_debug:set_internal_state(alloc_profile, false).
read() -> erts_debug:get_internal_state(alloc_profile).

run() ->
    erts_debug:set_internal_state(available_internal_state, true),
    N = 100000,
    %% 1. active: build N cons cells (~2N words)
    enable(),
    _ = build(N, []),
    W1 = read(),
    %% 2. active reset, build 2N
    enable(),
    _ = build(2 * N, []),
    W2 = read(),
    %% 3. gate off: build N, expect 0
    disable(),
    _ = build(N, []),
    W3 = read(),
    io:format("N=~p  W1=~p (exp ~~~p)  W2=~p (ratio ~.3f)  W3_off=~p~n",
              [N, W1, 2 * N, W2, W2 / max(1, W1), W3]),
    halt().
