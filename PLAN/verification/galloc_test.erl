-module(galloc_test).
-export([run/0]).

enable() -> erts_debug:set_internal_state(alloc_profile, true).
read() -> erts_debug:get_internal_state(alloc_profile).
fsize(T) -> erts_debug:flat_size(T).

conslist(0, A) -> A; conslist(N, A) -> conslist(N-1, [N|A]).
tuplist(0, A) -> A; tuplist(N, A) -> tuplist(N-1, [{N, N+1, N+2} | A]).
floatlist(0, A) -> A; floatlist(N, A) -> floatlist(N-1, [N/3 | A]).
binlist(0, A) -> A; binlist(N, A) -> binlist(N-1, [<<N:32, N:32, N:32>> | A]).
maplist(0, A) -> A; maplist(N, A) -> maplist(N-1, [#{a => N, b => N+1} | A]).
%% C-path: list_to_tuple is a BIF that HAllocs the tuple in C (no test_heap).
biftuplist(0, A) -> A; biftuplist(N, A) ->  biftuplist(N-1, [erlang:make_tuple(5, N) | A]).

%% exact: term has no shared substructure, reservation == term size.
exact(Name, Fun) ->
    enable(), T = Fun(), W = read(), E = fsize(T),
    io:format("~-11s recorded=~-9w flat_size=~-9w  match=~p~n",
              [Name, W, E, W =:= E]).

%% proportional: 2x input -> ~2x recorded (oracle-free).
prop(Name, Fun) ->
    enable(), _ = Fun(50000), W1 = read(),
    enable(), _ = Fun(100000), W2 = read(),
    io:format("~-11s W(50k)=~-9w W(100k)=~-9w  ratio=~.3f~n",
              [Name, W1, W2, W2 / max(1, W1)]).

run() ->
    erts_debug:set_internal_state(available_internal_state, true),
    N = 50000,
    io:format("-- JIT-inline allocation (test_heap hook): exact match --~n"),
    exact("conslist",  fun() -> conslist(N, []) end),
    exact("tuplelist", fun() -> tuplist(N, []) end),
    exact("floatlist", fun() -> floatlist(N, []) end),
    exact("heapbin",   fun() -> binlist(N, []) end),
    io:format("-- maps (literal keys shared -> flat_size over-counts): proportional --~n"),
    prop("maplist", fun(M) -> maplist(M, []) end),
    io:format("-- C-path BIF allocation (list_to_tuple): EXPECTED gap until increment 2b --~n"),
    exact("biftuple",  fun() -> biftuplist(N, []) end),
    halt().
