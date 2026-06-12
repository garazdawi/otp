set -e
apt-get update -qq >/dev/null 2>&1
apt-get install -y -qq --no-install-recommends procps curl xz-utils ca-certificates >/dev/null 2>&1
PERF=$(ls /usr/bin/perf_* 2>/dev/null | head -1); PERF=${PERF:-perf}
echo "container OTP: $(erl -noshell -eval 'io:format("~s",[erlang:system_info(otp_release)]),halt().')"
curl -sL -o /rmq.tar.xz https://github.com/rabbitmq/rabbitmq-server/releases/download/v4.3.1/rabbitmq-server-generic-unix-4.3.1.tar.xz
tar xf /rmq.tar.xz -C /
R=/rabbitmq_server-4.3.1
export RABBITMQ_SERVER_ADDITIONAL_ERL_ARGS="+JPperf map"
$R/sbin/rabbitmq-plugins --offline enable rabbitmq_mqtt >/dev/null 2>&1
$R/sbin/rabbitmq-server > /broker.out 2>&1 &
ok=""
for i in $(seq 1 45); do
  $R/sbin/rabbitmqctl status >/dev/null 2>&1 && ok=1 && break
  sleep 2
done
[ -n "$ok" ] || { echo "BROKER FAILED TO START"; tail -15 /broker.out; exit 42; }
$R/sbin/rabbitmqctl status 2>/dev/null | grep -E "RabbitMQ version|Erlang/OTP" | head -2
cat > /tmp/mqtt_load.erl <<'ERL'
-module(mqtt_load).
-export([run/6]).
run(Host, Port, NPubs, NSubs, Bytes, DurMs) ->
    Parent = self(),
    Subs = [spawn_link(fun() -> sub(Host, Port, I, Parent) end)
            || I <- lists:seq(0, NSubs - 1)],
    [receive {sub_ready, _} -> ok end || _ <- Subs],
    Pubs = [spawn_link(fun() -> pub(Host, Port, I rem NSubs, Bytes, Parent) end)
            || I <- lists:seq(0, NPubs - 1)],
    timer:sleep(DurMs),
    [P ! stop || P <- Pubs],
    Sent = lists:sum([receive {sent, C} -> C end || _ <- Pubs]),
    [S ! stop || S <- Subs],
    _ = [receive {rcvd, B} -> B end || _ <- Subs],
    io:format("sent ~b msgs (~.1f k/s)~n", [Sent, Sent * 1000 / DurMs / 1000]).
connect(Host, Port, ClientId) ->
    {ok, S} = gen_tcp:connect(Host, Port,
                              [binary, {active, false}, {nodelay, true},
                               {sndbuf, 1 bsl 20}, {recbuf, 1 bsl 20}]),
    Id = list_to_binary(ClientId),
    ok = gen_tcp:send(S, packet(1, <<0,4,"MQTT",4,2,0,60, (byte_size(Id)):16, Id/binary>>)),
    {ok, <<32,2,_,0>>} = gen_tcp:recv(S, 4, 5000),
    S.
packet(Type, Body) -> [<<Type:4, 0:4>>, remlen(byte_size(Body)), Body].
packet(Type, F, Body) -> [<<Type:4, F:4>>, remlen(byte_size(Body)), Body].
remlen(N) when N < 128 -> <<N>>;
remlen(N) -> <<(N rem 128 + 128), (remlen(N div 128))/binary>>.
pub(Host, Port, Topic, Bytes, Parent) ->
    S = connect(Host, Port, "p" ++ integer_to_list(erlang:unique_integer([positive]))),
    T = list_to_binary("t/" ++ integer_to_list(Topic)),
    Pkt = iolist_to_binary(packet(3, 0, <<(byte_size(T)):16, T/binary,
                                          (binary:copy(<<$x>>, Bytes))/binary>>)),
    pub_loop(S, binary:copy(Pkt, 50), 0, Parent).
pub_loop(S, B, N, Parent) ->
    receive stop -> Parent ! {sent, N * 50}, gen_tcp:close(S)
    after 0 -> ok = gen_tcp:send(S, B), timer:sleep(3), pub_loop(S, B, N + 1, Parent)
    end.
sub(Host, Port, I, Parent) ->
    S = connect(Host, Port, "s" ++ integer_to_list(I)),
    T = list_to_binary("t/" ++ integer_to_list(I)),
    ok = gen_tcp:send(S, packet(8, 2, <<1:16, (byte_size(T)):16, T/binary, 0>>)),
    {ok, <<144, _/binary>>} = gen_tcp:recv(S, 0, 5000),
    Parent ! {sub_ready, ok},
    inet:setopts(S, [{active, true}]),
    sub_loop(S, 0, Parent).
sub_loop(S, B, Parent) ->
    receive
        stop -> Parent ! {rcvd, B}, gen_tcp:close(S);
        {tcp, S, D} -> sub_loop(S, B + byte_size(D), Parent);
        {tcp_closed, S} -> Parent ! {rcvd, B}
    end.
ERL
erlc -o /tmp /tmp/mqtt_load.erl
cat > /tmp/gc_inst.erl <<'GCI'
%% gc_inst — generation-granularity GC lifetime instrumentation.
%%
%% Answers the lever question "faster GC vs less garbage" from idea
%% #50, at the granularity that decides it, using the existing
%% `garbage_collection` trace events — no custom VM build, so it runs
%% in stock erlang:28/29 containers against RabbitMQ and Bandit.
%%
%% The key metric is the SURVIVAL FRACTION: of the words a process
%% allocates into its young heap, what fraction live long enough to be
%% copied at least once by a minor GC. A copying generational collector
%% pays in proportion to what it *copies* (the survivors); dead words
%% cost nothing. So:
%%   - low survival  -> most allocated words are already dead at the
%%                      first minor GC -> short-lived garbage dominates
%%                      -> "create less garbage" is the lever (and the
%%                      GC is already only copying the small live set).
%%   - high survival -> the GC repeatedly copies live data -> "faster
%%                      GC / better promotion / bigger young heap" lever.
%%
%% Per minor GC of a traced process (from the trace Info proplists):
%%   processed = heap_size at gc_minor_start   (young words scanned)
%%   survived  = heap_size at gc_minor_end + (old_heap_size delta)
%%   reclaimed = processed - survived          (the garbage)
%%
%%   gc_inst:run(TopN, DurMs)        -> prints + returns the report
-module(gc_inst).
-export([run/2, run_to_file/3]).

run(TopN, DurMs) ->
    R = collect(TopN, DurMs),
    print(R),
    R.

run_to_file(TopN, DurMs, File) ->
    R = collect(TopN, DurMs),
    ok = file:write_file(File, io_lib:format("~p.~n", [R])),
    print(R),
    ok.

collect(TopN, DurMs) ->
    Self = self(),
    Pids = busiest(TopN),
    Tracer = spawn(fun() -> tracer(Self, new_acc()) end),
    [catch erlang:trace(P, true, [garbage_collection, {tracer, Tracer}])
     || P <- Pids],
    T0 = erlang:monotonic_time(millisecond),
    timer:sleep(DurMs),
    [catch erlang:trace(P, false, [garbage_collection]) || P <- Pids],
    Wall = erlang:monotonic_time(millisecond) - T0,
    Tracer ! {stop, Self},
    receive {acc, Acc} -> ok after 30000 -> Acc = new_acc() end,
    finalize(Acc, length(Pids), Wall).

%% Rank processes by reduction delta over a short pre-window so we trace
%% what's *currently* busy, not cumulative history.
busiest(TopN) ->
    Snap = fun() ->
                   [{P, reds(P)} || P <- processes(), P =/= self()]
           end,
    Before = maps:from_list(Snap()),
    timer:sleep(300),
    Deltas = [{P, R - maps:get(P, Before, R)} || {P, R} <- Snap()],
    [P || {P, _} <- lists:sublist(
                      lists:reverse(lists:keysort(2, Deltas)), TopN)].

reds(P) ->
    case process_info(P, reductions) of
        {reductions, R} -> R;
        _ -> 0
    end.

new_acc() ->
    #{processed => 0, survived => 0, reclaimed => 0,
      minor => 0, major => 0,
      major_processed => 0, major_survived => 0,
      starts => #{},          % Pid -> pending start Info
      block_sizes => [],      % heap_block_size samples
      per_proc => #{}}.       % {Mod,Fun,Ar} of pid -> {processed, survived}

tracer(Owner, Acc0) ->
    receive
        {stop, Owner} ->
            Owner ! {acc, Acc0};
        {trace, Pid, Tag, Info} ->
            tracer(Owner, on_event(Pid, Tag, Info, Acc0));
        _ ->
            tracer(Owner, Acc0)
    end.

on_event(Pid, gc_minor_start, Info, Acc) ->
    stash(Pid, minor, Info, Acc);
on_event(Pid, gc_major_start, Info, Acc) ->
    stash(Pid, major, Info, Acc);
on_event(Pid, gc_minor_end, EInfo, Acc) ->
    close(Pid, minor, EInfo, Acc);
on_event(Pid, gc_major_end, EInfo, Acc) ->
    close(Pid, major, EInfo, Acc);
on_event(_, _, _, Acc) ->
    Acc.

stash(Pid, Kind, Info, Acc) ->
    Starts = maps:get(starts, Acc),
    Acc#{starts := Starts#{Pid => {Kind, Info}}}.

close(Pid, Kind, EInfo, Acc) ->
    Starts = maps:get(starts, Acc),
    case maps:take(Pid, Starts) of
        {{Kind, SInfo}, Starts1} ->
            G = gv(heap_size, SInfo),
            He = gv(heap_size, EInfo),
            OHb = gv(old_heap_size, SInfo),
            OHe = gv(old_heap_size, EInfo),
            Promoted = max(0, OHe - OHb),
            Survived = He + Promoted,
            Processed = G,
            Reclaimed = max(0, Processed - Survived),
            Blk = gv(heap_block_size, SInfo),
            Acc1 = Acc#{starts := Starts1,
                        block_sizes := [Blk | maps:get(block_sizes, Acc)]},
            Acc2 = bump_proc(Pid, Processed, Survived, Acc1),
            case Kind of
                minor ->
                    Acc2#{processed := maps:get(processed, Acc2) + Processed,
                          survived := maps:get(survived, Acc2) + Survived,
                          reclaimed := maps:get(reclaimed, Acc2) + Reclaimed,
                          minor := maps:get(minor, Acc2) + 1};
                major ->
                    Acc2#{major_processed :=
                              maps:get(major_processed, Acc2) + Processed,
                          major_survived :=
                              maps:get(major_survived, Acc2) + Survived,
                          major := maps:get(major, Acc2) + 1}
            end;
        _ ->
            Acc#{starts := maps:remove(Pid, Starts)}
    end.

bump_proc(Pid, Proc, Surv, Acc) ->
    Key = case process_info(Pid, initial_call) of
              {initial_call, MFA} -> MFA;
              _ -> unknown
          end,
    PP = maps:get(per_proc, Acc),
    {P0, S0} = maps:get(Key, PP, {0, 0}),
    Acc#{per_proc := PP#{Key => {P0 + Proc, Surv + S0}}}.

gv(K, Info) -> proplists:get_value(K, Info, 0).

finalize(Acc, NPids, Wall) ->
    #{processed := P, survived := S, reclaimed := R,
      minor := Mi, major := Ma,
      major_processed := MP, major_survived := MS,
      block_sizes := Blks, per_proc := PP} = Acc,
    WallS = Wall / 1000,
    Sorted = lists:reverse(lists:keysort(2,
               [{K, Pr, Su} || {K, {Pr, Su}} <- maps:to_list(PP)])),
    #{traced_procs => NPids,
      wall_s => WallS,
      minor_gcs => Mi,
      major_gcs => Ma,
      minor_per_proc_per_s =>
          safe_div(Mi, NPids * WallS),
      young_words_processed => P,
      young_words_survived => S,
      young_words_reclaimed => R,
      survival_fraction => safe_div(S, P),
      garbage_fraction => safe_div(R, P),
      garbage_words_per_s => safe_div(R, WallS),
      major_survival_fraction => safe_div(MS, MP),
      heap_block_words => summarize(Blks),
      top_allocators =>
          [{K, Pr, safe_div(Su, Pr)} || {K, Pr, Su} <- lists:sublist(Sorted, 12)]}.

safe_div(_, 0) -> 0.0;
safe_div(_, 0.0) -> 0.0;
safe_div(A, B) -> A / B.

summarize([]) -> #{};
summarize(L) ->
    Sorted = lists:sort(L),
    N = length(Sorted),
    #{min => hd(Sorted), p50 => lists:nth(N div 2 + 1, Sorted),
      max => lists:last(Sorted)}.

print(R) ->
    #{traced_procs := N, wall_s := W, minor_gcs := Mi, major_gcs := Ma,
      survival_fraction := SF, garbage_fraction := GF,
      garbage_words_per_s := GR, young_words_processed := YP,
      major_survival_fraction := MSF, minor_per_proc_per_s := MPP,
      heap_block_words := Blk, top_allocators := Top} = R,
    io:format("~n=== GC lifetime (gen granularity) — ~b procs, ~.1fs ===~n",
              [N, W]),
    io:format("minor GCs ~b (~.1f/proc/s), major GCs ~b~n", [Mi, MPP, Ma]),
    io:format("young words processed ~b~n", [YP]),
    io:format("** survival fraction ~.4f  (garbage ~.4f) **~n", [SF, GF]),
    io:format("garbage production ~.2f Mword/s (~.1f MB/s)~n",
              [GR / 1.0e6, GR * 8 / 1.0e6]),
    io:format("major-gc survival fraction ~.4f~n", [MSF]),
    io:format("heap block words ~p~n", [Blk]),
    io:format("top allocators (initial_call: words_processed, survival):~n"),
    [io:format("  ~14b  surv ~.3f  ~p~n", [Pr, S, K]) || {K, Pr, S} <- Top],
    ok.

GCI
erlc -o /tmp /tmp/gc_inst.erl 2>/dev/null
erl -noshell -pa /tmp -eval '(catch mqtt_load:run("127.0.0.1",1883,6,6,256,40000)), halt().' > /load.out 2>&1 &
LOAD=$!
sleep 12
$R/sbin/rabbitmqctl --timeout 60 eval 'code:add_patha("/tmp"), code:ensure_loaded(gc_inst), io:format("~p~n",[gc_inst:run(60, 12000)]).' 2>&1 | tail -40
wait $LOAD; cat /load.out
