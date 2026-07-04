%% T2FULL M0.R -- receive-path classification measurement driver.
%%
%% Counter semantics (mirrors erts/emulator/beam/erl_process.c):
%%   hit_first      matched, never waited, scan depth 1 (first message)
%%   hit_scan_2_4   matched, never waited, scan depth 2..4
%%   hit_scan_5p    matched, never waited, scan depth >= 5
%%   wait_match     matched after >=1 suspend on empty/exhausted queue
%%   timeout_imm    after-clause fired, never waited (e.g. `after 0`)
%%   timeout_waited after-clause fired after having waited (`after N` elapsed)
%%   msgs_scanned   non-matching messages advanced past, all instances
%%
%% A "hit" is hit_first+hit_scan_2_4+hit_scan_5p: a receive that a T2
%% in-IR fast path could in principle serve without a scheduler round-trip.
-module(recv_bench).
-export([main/1, enable/0, reset/0, read/0, report/1, delta_report/2,
         validate/0,
         w1_genserver/2, w2_ring/3, w2_flood/2, w4_dialyzer/1]).

%% ---- counter access ------------------------------------------------------

enable() ->
    erts_debug:set_internal_state(available_internal_state, true).

reset() ->
    erts_debug:set_internal_state(recv_stats, true).

read() ->
    erts_debug:get_internal_state(recv_stats).

get(K, L) -> proplists:get_value(K, L, 0).

%% Pretty-print an absolute snapshot with derived percentages.
report(Label) -> pp(Label, read()).

%% Snapshot delta helper for callers that prefer explicit before/after.
delta_report(Label, Before) ->
    After = read(),
    D = [{K, get(K, After) - get(K, Before)} || {K, _} <- After],
    pp(Label, D).

pp(Label, L) ->
    HitF = get(hit_first, L),
    Hit24 = get(hit_scan_2_4, L),
    Hit5 = get(hit_scan_5p, L),
    Hit = HitF + Hit24 + Hit5,
    WM = get(wait_match, L),
    TI = get(timeout_imm, L),
    TW = get(timeout_waited, L),
    Scan = get(msgs_scanned, L),
    Inst = Hit + WM + TI + TW,
    P = fun(X) -> case Inst of 0 -> 0.0; _ -> 100.0 * X / Inst end end,
    io:format("~n===== ~s =====~n", [Label]),
    io:format("instances       : ~b~n", [Inst]),
    io:format("hit             : ~b (~.2f%)~n", [Hit, P(Hit)]),
    io:format("  hit_first     : ~b (~.2f%)  [matched 1st message]~n", [HitF, P(HitF)]),
    io:format("  hit_scan_2_4  : ~b (~.2f%)~n", [Hit24, P(Hit24)]),
    io:format("  hit_scan_5p   : ~b (~.2f%)~n", [Hit5, P(Hit5)]),
    io:format("wait_match      : ~b (~.2f%)~n", [WM, P(WM)]),
    io:format("timeout_imm     : ~b (~.2f%)~n", [TI, P(TI)]),
    io:format("timeout_waited  : ~b (~.2f%)~n", [TW, P(TW)]),
    io:format("msgs_scanned    : ~b (avg ~.3f non-matching/instance)~n",
              [Scan, case Inst of 0 -> 0.0; _ -> Scan / Inst end]),
    {Inst, Hit, WM, TI + TW}.

%% ---- validation microbenchmarks (prove counter semantics) ----------------

validate() ->
    enable(),
    io:format("~n#### VALIDATION (each block is a controlled receive shape) ####~n"),

    %% V1: `after 0` on an empty queue, N times -> N timeout_imm, nothing else.
    reset(),
    N = 100000,
    lists:foreach(fun(_) -> receive _ -> ok after 0 -> ok end end, lists:seq(1, N)),
    pp(io_lib:format("V1: ~b x (receive after 0) on empty queue", [N]), read()),

    %% V2: pre-queue N messages, receive them all; each matches the first
    %% message in queue -> N hit_first.
    reset(),
    self() ! start,
    receive start -> ok end, %% drain the marker (1 hit_first)
    [self() ! {msg, I} || I <- lists:seq(1, N)],
    drain_n(N),
    pp(io_lib:format("V2: pre-queue ~b msgs, receive-all (expect hit_first)", [N]),
       read()),

    %% V3: strict synchronous ping-pong -> receiver waits every time.
    reset(),
     pingpong(50000),
    pp("V3: synchronous ping-pong 50000 (expect wait_match)", read()),

    %% V4: selective receive scanning K junk msgs before the match, R times.
    reset(),
    K = 6, R = 20000,
    selective(K, R),
    pp(io_lib:format("V4: ~b x selective receive past ~b junk (expect hit_scan_5p)",
                     [R, K]), read()),
    ok.

drain_n(0) -> ok;
drain_n(N) -> receive {msg, _} -> drain_n(N - 1) end.

pingpong(N) ->
    Parent = self(),
    P = spawn(fun() -> pong(Parent) end),
    ping(P, N).
ping(_P, 0) -> ok;
ping(P, N) -> P ! {ping, self()}, receive pong -> ping(P, N - 1) end.
pong(Parent) ->
    receive {ping, From} -> From ! pong, pong(Parent) end.

selective(_K, 0) -> ok;
selective(K, R) ->
    %% queue K junk messages then the wanted one; a selective receive
    %% skips the junk and matches wanted -> scan depth K+1.
    [self() ! {junk, I} || I <- lists:seq(1, K)],
    self() ! wanted,
    receive wanted -> ok end,
    %% clean up the junk we left behind so queues don't grow unbounded
    [receive {junk, _} -> ok after 0 -> ok end || _ <- lists:seq(1, K)],
    selective(K, R - 1).

%% ---- Workload 1: gen_server ping-pong under concurrent load --------------
%% One gen_server, Clients concurrent callers each looping gen_server:call
%% for DurMs. Server backlog -> server receive should be mostly hit; each
%% client blocks for its reply -> client reply-receive is wait_match.

%% Deadline-based clients (no `receive after 0` poll) so the only receive
%% instances measured are the gen_server's own: the server's request
%% receive and each client's reply receive inside gen_server:call.
w1_genserver(Clients, DurMs) ->
    enable(),
    {ok, Pid} = recv_srv:start_link(),
    Parent = self(),
    T0 = erlang:monotonic_time(millisecond),
    Deadline = T0 + DurMs,
    reset(),
    Ws = [spawn_link(fun() -> w1_client(Pid, 0, Deadline, Parent) end)
          || _ <- lists:seq(1, Clients)],
    Total = lists:sum([receive {done, C} -> C end || _ <- Ws]),
    T1 = erlang:monotonic_time(millisecond),
    {Inst, Hit, WM, TO} =
        pp(io_lib:format("W1 gen_server: ~b clients, ~b ms, ~b calls",
                         [Clients, DurMs, Total]), read()),
    io:format("W1 throughput   : ~.1f k calls/s~n",
              [Total / max(1, T1 - T0)]),
    gen_server:stop(Pid),
    {Inst, Hit, WM, TO, Total, T1 - T0}.

w1_client(Pid, C, Deadline, Parent) ->
    _ = gen_server:call(Pid, {work, C}),
    case (C band 1023) =:= 0 andalso
         erlang:monotonic_time(millisecond) >= Deadline of
        true -> Parent ! {done, C};
        false -> w1_client(Pid, C + 1, Deadline, Parent)
    end.

%% ---- Workload 2a: loaded process ring ------------------------------------
%% N processes in a ring; inject Tokens tokens that circulate for DurMs.
%% With Tokens >= N the queues stay backlogged -> high hit rate.

w2_ring(N, Tokens, DurMs) ->
    enable(),
    Parent = self(),
    reset(),
    Ring = build_ring(N, Parent),
    First = hd(Ring),
    T0 = erlang:monotonic_time(millisecond),
    [First ! {tok, I} || I <- lists:seq(1, Tokens)],
    timer:sleep(DurMs),
    [P ! stop || P <- Ring],
    Hops = lists:sum([receive {hops, H} -> H end || _ <- Ring]),
    T1 = erlang:monotonic_time(millisecond),
    {Inst, Hit, WM, TO} =
        pp(io_lib:format("W2 ring: ~b procs, ~b tokens, ~b ms, ~b hops",
                         [N, Tokens, DurMs, Hops]), read()),
    io:format("W2 throughput   : ~.1f k hops/s~n", [Hops / max(1, T1 - T0)]),
    {Inst, Hit, WM, TO, Hops, T1 - T0}.

build_ring(N, Parent) ->
    Pids = [spawn_link(fun() -> ring_wait(Parent) end) || _ <- lists:seq(1, N)],
    Next = tl(Pids) ++ [hd(Pids)],
    [P ! {next, Nx} || {P, Nx} <- lists:zip(Pids, Next)],
    Pids.

ring_wait(Parent) ->
    receive {next, Nx} -> ring_loop(Nx, 0, Parent) end.
ring_loop(Nx, H, Parent) ->
    receive
        {tok, _} = T -> Nx ! T, ring_loop(Nx, H + 1, Parent);
        stop -> Parent ! {hops, H}
    end.

%% ---- Workload 2b: burst flood to a sink (near-100% hit control) ----------
w2_flood(Msgs, Bursts) ->
    enable(),
    Parent = self(),
    reset(),
    Sink = spawn_link(fun() -> flood_sink(0, Parent) end),
    T0 = erlang:monotonic_time(millisecond),
    lists:foreach(
      fun(_) -> [Sink ! m || _ <- lists:seq(1, Msgs)], timer:sleep(1) end,
      lists:seq(1, Bursts)),
    Sink ! {stop, self()},
    Got = receive {sunk, G} -> G end,
    T1 = erlang:monotonic_time(millisecond),
    {Inst, Hit, WM, TO} =
        pp(io_lib:format("W2b flood: ~b bursts x ~b msgs, ~b received",
                         [Bursts, Msgs, Got]), read()),
    io:format("W2b throughput  : ~.1f k msg/s~n", [Got / max(1, T1 - T0)]),
    {Inst, Hit, WM, TO, Got, T1 - T0}.

flood_sink(G, Parent) ->
    receive
        m -> flood_sink(G + 1, Parent);
        {stop, _} -> Parent ! {sunk, G}
    end.

%% ---- Workload 4: dialyzer PLT build (compute control) --------------------
w4_dialyzer(PltPath) ->
    enable(),
    _ = file:delete(PltPath),
    reset(),
    T0 = erlang:monotonic_time(millisecond),
    R = (catch dialyzer:run([{analysis_type, plt_build},
                             {apps, [erts, kernel, stdlib]},
                             {output_plt, PltPath}])),
    T1 = erlang:monotonic_time(millisecond),
    io:format("W4 dialyzer run result: ~p~n", [case R of {'EXIT', E} -> {crash, E}; _ -> ok end]),
    {Inst, Hit, WM, TO} =
        pp(io_lib:format("W4 dialyzer --build_plt erts kernel stdlib (~b ms)",
                         [T1 - T0]), read()),
    {Inst, Hit, WM, TO, T1 - T0}.

%% ---- entry point ---------------------------------------------------------
main(_) ->
    validate(),
    w1_genserver(64, 5000),
    w2_ring(200, 4000, 5000),
    w2_flood(200000, 40),
    w4_dialyzer("/tmp/recv_m0r.plt"),
    ok.
