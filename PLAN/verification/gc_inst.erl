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
