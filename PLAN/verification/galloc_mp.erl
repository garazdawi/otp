-module(galloc_mp).
-export([run/0]).
build(0, A) -> A; build(N, A) -> build(N-1, [N|A]).

run() ->
    erts_debug:set_internal_state(available_internal_state, true),
    Parent = self(),
    %% Worker allocates heavily but does NOT enable profiling.
    W = spawn(fun() ->
                  receive go -> ok end,
                  _ = build(500000, []),
                  Parent ! worker_done
              end),
    %% Parent enables, runs worker concurrently, then allocates a known amount.
    erts_debug:set_internal_state(alloc_profile, true),
    W ! go,
    receive worker_done -> ok end,
    _ = build(50000, []),   %% exactly 100000 words for the parent
    P = erts_debug:get_internal_state(alloc_profile),
    %% Second profiling process, independent counter.
    spawn(fun() ->
              erts_debug:set_internal_state(alloc_profile, true),
              _ = build(50000, []),
              Parent ! {other, erts_debug:get_internal_state(alloc_profile)}
          end),
    Other = receive {other, V} -> V end,
    io:format("parent=~p (exp 100000, isolated from worker's 1M)  other_proc=~p (exp 100000)~n",
              [P, Other]),
    io:format("parent_isolated=~p other_isolated=~p~n",
              [P =:= 100000, Other =:= 100000]),
    halt().
