%% T2 G3 subject 1 — branchy gen_server-shaped dispatch.
%%
%% A 12-clause dispatcher over tuple-shaped messages with guards,
%% driven with a skewed mix (85% get / 10% put / 5% incr). The T2
%% specialization (T2_G31=1) emits only the hot {get, I} arm with
%% fused guards; every other message side-exits to the full T1
%% clause dispatch — cold-arm pruning exactly as 04 §10.3 designs it.
%%
%%   t2_g31:bench(Reps)  -> isolated dispatch + gen_server roundtrips
%%   t2_g31:check()      -> result hash (must match across modes)
-module(t2_g31).
-behaviour(gen_server).
-export([bench/1, check/0]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([dispatch/2]).  %% prevent inlining-away; T2 target

-define(STATE, {10, 20, 30, 40, 50, 60, 70, 80, 90, 100}).

%% --- the dispatcher (T2 target: dispatch/2) ----------------------

dispatch({get, I}, State) when is_integer(I), I >= 1, I =< 10 ->
    element(I, State);
dispatch({put, I, V}, State) when is_integer(I), I >= 1, I =< 10 ->
    {state, setelement(I, State, V)};
dispatch({incr, I}, State) when is_integer(I), I >= 1, I =< 10 ->
    {state, setelement(I, State, element(I, State) + 1)};
dispatch({decr, I}, State) when is_integer(I), I >= 1, I =< 10 ->
    {state, setelement(I, State, element(I, State) - 1)};
dispatch({swap, I, J}, State) when is_integer(I), is_integer(J) ->
    A = element(I, State), B = element(J, State),
    {state, setelement(J, setelement(I, State, B), A)};
dispatch({peek, I}, State) when is_integer(I), I >= 1, I =< 10 ->
    element(I, State);
dispatch({touch, _I}, State) ->
    {state, State};
dispatch({cas, I, Old, New}, State) when is_integer(I) ->
    case element(I, State) of
        Old -> {state, setelement(I, State, New)};
        _ -> {state, State}
    end;
dispatch(stats, State) ->
    lists:sum(tuple_to_list(State));
dispatch(reset, _State) ->
    {state, ?STATE};
dispatch({batch, Msgs}, State) when is_list(Msgs) ->
    {state, lists:foldl(fun(M, S) ->
                                case dispatch(M, S) of
                                    {state, NS} -> NS;
                                    _ -> S
                                end
                        end, State, Msgs)};
dispatch(_Other, State) ->
    {state, State}.

%% --- gen_server with the same shape (T2 target: handle_call/3) ---

init([]) -> {ok, ?STATE}.

handle_call({get, I}, _From, State) when is_integer(I), I >= 1, I =< 10 ->
    {reply, element(I, State), State};
handle_call({put, I, V}, _From, State) when is_integer(I), I >= 1, I =< 10 ->
    {reply, ok, setelement(I, State, V)};
handle_call({incr, I}, _From, State) when is_integer(I), I >= 1, I =< 10 ->
    {reply, ok, setelement(I, State, element(I, State) + 1)};
handle_call({swap, I, J}, _From, State) when is_integer(I), is_integer(J) ->
    A = element(I, State), B = element(J, State),
    {reply, ok, setelement(J, setelement(I, State, B), A)};
handle_call(stats, _From, State) ->
    {reply, lists:sum(tuple_to_list(State)), State};
handle_call(reset, _From, _State) ->
    {reply, ok, ?STATE};
handle_call(_Other, _From, State) ->
    {reply, unknown, State}.

handle_cast(_, State) -> {noreply, State}.

%% --- workload ------------------------------------------------------

%% Deterministic skewed mix: 85% get, 10% put, 5% incr.
msgs(N) ->
    rand:seed(exsss, {7, 7, 7}),
    [case rand:uniform(100) of
         R when R =< 85 -> {get, 1 + (R rem 10)};
         R when R =< 95 -> {put, 1 + (R rem 10), R};
         R -> {incr, 1 + (R rem 10)}
     end || _ <- lists:seq(1, N)].

run_dispatch([], S, Acc) -> {S, Acc};
run_dispatch([M | T], S, Acc) ->
    case dispatch(M, S) of
        {state, NS} -> run_dispatch(T, NS, Acc);
        V -> run_dispatch(T, S, Acc bxor V)
    end.

bench(Reps) ->
    Msgs = msgs(100_000),
    DTimes = [begin
                  T0 = erlang:monotonic_time(microsecond),
                  _ = run_dispatch(Msgs, ?STATE, 0),
                  erlang:monotonic_time(microsecond) - T0
              end || _ <- lists:seq(1, Reps)],
    io:format("dispatch  100k msgs   min ~7b us~n", [lists:min(DTimes)]),
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    SrvMsgs = msgs(50_000),
    STimes = [begin
                  T0 = erlang:monotonic_time(microsecond),
                  [_ = gen_server:call(Pid, M) || M <- SrvMsgs],
                  erlang:monotonic_time(microsecond) - T0
              end || _ <- lists:seq(1, Reps)],
    gen_server:stop(Pid),
    io:format("server    50k calls   min ~7b us~n", [lists:min(STimes)]),
    ok.

check() ->
    Msgs = msgs(50_000) ++
        [stats, reset, {swap, 1, 2}, {peek, 3}, {touch, 4},
         {cas, 5, 50, 555}, {cas, 5, nope, 1}, {batch, msgs(100)},
         {get, 11}, {get, 0}, {get, x}, bogus],
    {S, Acc} = run_dispatch(Msgs, ?STATE, 0),
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    Replies = [gen_server:call(Pid, M) || M <- msgs(10_000)],
    gen_server:stop(Pid),
    Hash = erlang:phash2({S, Acc, Replies}, 1 bsl 32),
    io:format("g31 hash ~b~n", [Hash]),
    Hash.
