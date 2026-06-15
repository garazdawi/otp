-module(galloc_test).
-export([run/0]).

enable() -> erts_debug:set_internal_state(alloc_profile, true).
read() -> erts_debug:get_internal_state(alloc_profile).
fsize(T) -> erts_debug:flat_size(T).

conslist(0, A) -> A; conslist(N, A) -> conslist(N-1, [N|A]).
tuplist(0, A) -> A; tuplist(N, A) -> tuplist(N-1, [{N, N+1, N+2} | A]).
floatlist(0, A) -> A; floatlist(N, A) -> floatlist(N-1, [N/3 | A]).
binlist(0, A) -> A; binlist(N, A) -> binlist(N-1, [<<N:32, N:32, N:32>> | A]).
biftuplist(0, A) -> A; biftuplist(N, A) -> biftuplist(N-1, [erlang:make_tuple(5, N) | A]).
maplist(0, A) -> A; maplist(N, A) -> maplist(N-1, [#{a => N, b => N+1} | A]).
biglist(0, A) -> A; biglist(N, A) -> biglist(N-1, [(1 bsl 70) + N | A]).
dynmap(0, M) -> M; dynmap(N, M) -> dynmap(N-1, maps:put(N, N, M)).

%% Exact: term has no shared/over-reserved substructure -> recorded
%% gross words == flat_size. Covers JIT-inline AND C-path (make_tuple).
exact(Name, Fun) ->
    enable(), T = Fun(), W = read(), E = fsize(T),
    io:format("~-11s recorded=~-9w flat_size=~-9w  match=~p~n",
              [Name, W, E, W =:= E]).

%% Proportional: oracle-free (shared literals / conservative
%% over-reservation / superlinear growth make flat_size != gross).
prop(Name, Fun) ->
    enable(), _ = Fun(50000), W1 = read(),
    enable(), _ = Fun(100000), W2 = read(),
    R = W2 / max(1, W1),
    io:format("~-11s W(50k)=~-9w W(100k)=~-9w  ratio=~.3f ok=~p~n",
              [Name, W1, W2, R, R > 1.8]).

run() ->
    erts_debug:set_internal_state(available_internal_state, true),
    N = 50000,
    io:format("-- exact (recorded gross words == flat_size) --~n"),
    exact("conslist",  fun() -> conslist(N, []) end),     % JIT-inline
    exact("tuplelist", fun() -> tuplist(N, []) end),      % JIT-inline
    exact("floatlist", fun() -> floatlist(N, []) end),    % JIT-inline
    exact("heapbin",   fun() -> binlist(N, []) end),      % JIT-inline (bs_create_bin)
    exact("biftuple",  fun() -> biftuplist(N, []) end),   % C-path (HAllocX)
    io:format("-- proportional (gross != flat_size by construction) --~n"),
    prop("maplist", fun(M) -> maplist(M, []) end),        % literal keys shared
    prop("bignum",  fun(M) -> biglist(M, []) end),        % gc_bif over-reserves (HeapFragOnlyAlloc)
    prop("dynmap",  fun(M) -> dynmap(M, #{}) end),        % superlinear (C-path)
    halt().
