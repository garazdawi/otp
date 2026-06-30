%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(auto_hibernate).
-moduledoc """
Automatic hibernation of idle processes (idea #1, step 2).

This is a `m:gen_server` that periodically scans the processes of the node and
hibernates the ones that have been idle for a configurable number of scan
rounds. Hibernating shrinks an idle process' heap to the minimal size needed to
hold its live data (see `erlang:hibernate/1`); with the `compressed` option the
heap is additionally stored in compressed form (`erlang:hibernate/2`),
reclaiming even more memory while the process sleeps.

Idle processes are detected purely from reduction counts, so no per-process
instrumentation is required: a process whose reduction count is unchanged
between two consecutive scans did no work in that interval and is considered
idle. A process is hibernated once it has been idle for `idle_rounds`
consecutive scans, and is not hibernated again until it does some work (its
reduction count changes).

The service is not started automatically; start it with `start_link/0,1`.
""".

-behaviour(gen_server).

-export([start_link/0, start_link/1, stop/0, scan_now/0, info/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-type option() ::
        {interval, pos_integer()}        %% milliseconds between scans
      | {idle_rounds, pos_integer()}     %% idle scans before hibernating
      | {reds_tolerance, non_neg_integer()} %% reductions still counted as idle
      | {compressed, boolean()}          %% also compress the heap
      | {exclude, [pid()] | fun((pid()) -> boolean())}.
-export_type([option/0]).

-define(DEFAULT_INTERVAL, 5000).
-define(DEFAULT_IDLE_ROUNDS, 2).
-define(DEFAULT_REDS_TOLERANCE, 0).

-record(state, {interval     :: pos_integer(),
                idle_rounds  :: pos_integer(),
                reds_tolerance :: non_neg_integer(),
                compressed   :: boolean(),
                exclude      :: fun((pid()) -> boolean()),
                %% pid => last observed reduction count
                reds = #{}   :: #{pid() => non_neg_integer()},
                %% pid => number of consecutive idle rounds, or the atom
                %% 'hibernated' once the process has been hibernated
                idle = #{}   :: #{pid() => non_neg_integer() | hibernated},
                hibernated_total = 0 :: non_neg_integer(),
                timer :: reference() | undefined}).

%%% ----------------------------------------------------------------------------
%%% API
%%% ----------------------------------------------------------------------------

-doc(#{ equiv => start_link([]) }).
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

-doc """
Starts the automatic hibernation service and links it to the caller.

Options:

- `{interval, Milliseconds}` - time between scans (default 5000).
- `{idle_rounds, N}` - number of consecutive idle scans before a process is
  hibernated (default 2).
- `{reds_tolerance, N}` - a process is still considered idle if its reduction
  count grew by at most `N` between two scans (default 0). Detection uses only
  signal-free `process_info` items (`status`, `reductions`), so scanning never
  perturbs the inspected processes; the default of 0 (exact equality) is
  therefore safe.
- `{compressed, Boolean}` - if `true`, also compress the heap of hibernated
  processes (default `false`).
- `{exclude, PidsOrPred}` - processes to never hibernate, either a list of pids
  or a predicate `fun((pid()) -> boolean())` returning `true` to exclude.
""".
-spec start_link([option()]) -> {ok, pid()} | {error, term()}.
start_link(Options) when is_list(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

-doc "Stops the service.".
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

-doc "Forces an immediate scan and returns the number of processes hibernated.".
-spec scan_now() -> {ok, non_neg_integer()}.
scan_now() ->
    gen_server:call(?MODULE, scan_now, infinity).

-doc "Returns statistics about the service.".
-spec info() -> map().
info() ->
    gen_server:call(?MODULE, info, infinity).

%%% ----------------------------------------------------------------------------
%%% gen_server callbacks
%%% ----------------------------------------------------------------------------

-doc false.
init(Options) ->
    Interval = proplists:get_value(interval, Options, ?DEFAULT_INTERVAL),
    IdleRounds = proplists:get_value(idle_rounds, Options, ?DEFAULT_IDLE_ROUNDS),
    Tol = proplists:get_value(reds_tolerance, Options, ?DEFAULT_REDS_TOLERANCE),
    Compressed = proplists:get_value(compressed, Options, false),
    Exclude = make_exclude(proplists:get_value(exclude, Options, [])),
    State = #state{interval = Interval,
                   idle_rounds = IdleRounds,
                   reds_tolerance = Tol,
                   compressed = Compressed,
                   exclude = Exclude},
    {ok, arm_timer(State)}.

-doc false.
handle_call(scan_now, _From, State0) ->
    {N, State} = scan(State0),
    {reply, {ok, N}, arm_timer(State)};
handle_call(info, _From, State) ->
    Reply = #{interval => State#state.interval,
              idle_rounds => State#state.idle_rounds,
              reds_tolerance => State#state.reds_tolerance,
              compressed => State#state.compressed,
              tracked => map_size(State#state.reds),
              currently_hibernated =>
                  length([P || {P, hibernated} <- maps:to_list(State#state.idle)]),
              hibernated_total => State#state.hibernated_total},
    {reply, Reply, State};
handle_call(_Req, _From, State) ->
    {reply, {error, bad_request}, State}.

-doc false.
handle_cast(_Msg, State) ->
    {noreply, State}.

-doc false.
handle_info(scan, State0) ->
    {_N, State} = scan(State0),
    {noreply, arm_timer(State)};
handle_info(_Info, State) ->
    {noreply, State}.

-doc false.
terminate(_Reason, _State) ->
    ok.

%%% ----------------------------------------------------------------------------
%%% Internal
%%% ----------------------------------------------------------------------------

make_exclude(Pred) when is_function(Pred, 1) ->
    Pred;
make_exclude(Pids) when is_list(Pids) ->
    Set = sets:from_list(Pids, [{version, 2}]),
    fun(P) -> sets:is_element(P, Set) end.

arm_timer(State = #state{interval = Interval}) ->
    State#state{timer = erlang:send_after(Interval, self(), scan)}.

%% One scan round: update the idle bookkeeping and hibernate processes that
%% have been idle long enough.
scan(State = #state{reds = PrevReds, idle = PrevIdle,
                    idle_rounds = Threshold, reds_tolerance = Tol,
                    exclude = Exclude, compressed = Compressed}) ->
    Self = self(),
    HibOpts = case Compressed of
                  true -> [compressed];
                  false -> []
              end,
    {Reds, Idle, ToHib} =
        lists:foldl(
          fun(P, Acc) when P =:= Self ->
                  %% Never hibernate the scanner itself.
                  Acc;
             (P, {RAcc, IAcc, HAcc}) ->
                  %% Only signal-free process_info items are used, so a scan
                  %% never sends a signal to the inspected process and thus
                  %% never perturbs its reduction count (nor wakes/decompresses
                  %% an already hibernated process).
                  case erlang:process_info(P, [status, reductions]) of
                      undefined ->
                          {RAcc, IAcc, HAcc};   %% died between processes/1 and now
                      [{status, hibernated}, {reductions, R}] ->
                          %% The VM reports it as hibernated (by us or by other
                          %% means, e.g. proc_lib:hibernate). Leave it be.
                          {RAcc#{P => R}, IAcc#{P => hibernated}, HAcc};
                      [{status, Status}, {reductions, R}] ->
                          RAcc1 = RAcc#{P => R},
                          PrevR = maps:get(P, PrevReds, undefined),
                          IsIdle = Status =:= waiting
                              andalso is_integer(PrevR)
                              andalso R - PrevR =< Tol,
                          case IsIdle of
                              false ->
                                  %% Active (did work, not waiting, or first
                                  %% time seen) -> reset.
                                  {RAcc1, IAcc#{P => 0}, HAcc};
                              true ->
                                  case maps:get(P, PrevIdle, 0) of
                                      hibernated ->
                                          %% Was hibernated but is no longer;
                                          %% it woke -> reset.
                                          {RAcc1, IAcc#{P => 0}, HAcc};
                                      N when N + 1 >= Threshold ->
                                          case Exclude(P) of
                                              true ->
                                                  {RAcc1, IAcc#{P => N + 1}, HAcc};
                                              false ->
                                                  {RAcc1, IAcc#{P => hibernated},
                                                   [P | HAcc]}
                                          end;
                                      N ->
                                          {RAcc1, IAcc#{P => N + 1}, HAcc}
                                  end
                          end
                  end
          end,
          {#{}, #{}, []},
          erlang:processes()),
    N = hibernate_all(ToHib, HibOpts, 0),
    {N, State#state{reds = Reds, idle = Idle,
                    hibernated_total = State#state.hibernated_total + N}}.

hibernate_all([P | Ps], HibOpts, N) ->
    Hibernated =
        try erlang:hibernate(P, HibOpts) of
            true -> 1;
            _ -> 0
        catch
            _:_ -> 0
        end,
    hibernate_all(Ps, HibOpts, N + Hibernated);
hibernate_all([], _HibOpts, N) ->
    N.
