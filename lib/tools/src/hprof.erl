%%
%%
%% Copyright WhatsApp Inc. and its affiliates. All rights reserved.
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
%%
%%-------------------------------------------------------------------
%%
%% @author Maxim Fedorov <maximfca@gmail.com>
%% Erlang Process Heap profiler.
%%
-module(hprof).
-moduledoc """
Process Heap Profiling Tool

`hprof` provides convenience helpers for Erlang process heap profiling.
Underlying mechanism is the Erlang trace BIFs.

Heap profiling can be done ad-hoc, to understand heap allocations done by your
program, or run in a server-aided mode for deeper introspection of the code
running in production.

> #### Warning {: .warning }
>
> Avoid hot code reload for modules that are participating in the memory
> tracing. Reloading a module turns tracing off, discarding accumulated
> statistics. `hprof` may not report correct amounts when code reload happened
> during profiling session.

Heap allocations happen for all Erlang terms that do not fit a single machine
word. For example, a function returning tuple of 2 elements needs to allocate
this tuple on the process heap. Actual consumption is more than 2 machine words,
because Erlang runtime needs to store tuple size and other internal information.

> #### Note {: .info }
>
> When profiling is enabled, expect a slowdown in the program execution.
>
> For profiling convenience, heap allocations are accumulated for functions that
> are not enabled in trace pattern. Consider this call stack example:
>
> ```text
>       top_traced_function(...)
>       not_traced_function()
>       bottom_traced_function()
> ```
>
> Allocations that happened within `not_traced_function` will be accounted into
> `top_traced_function`. However allocations happened within
> `bottom_traced_function` are not included in the `top_traced_function`. If you
> want to account only own allocations, you need to trace all functions.

## Ad-hoc profiling

Basic profiling providing accumulated memory allocation data. You can choose to
print per-process statistics, total statistics, or omit printing and extract
machine-readable data that you can later sort/print:

```text
      1> hprof:profile(lists, seq, [1, 16]).

      ****** Process <0.179.0>    -- 100.00 % of total allocations ***
      MODUL FUN/ARITY   CALLS  WORDS  PER CALL  [     %]
      lists seq_loop/3      5     32         6  [100.00]
      32            [ 100.0]
      ok
```

By default tracing is enabled for all functions of all modules. When functions
are created in the interactive shell, parts of shell code are also traced. It is
however possible to limit the trace to specific functions or modules:

```erlang
      1> hprof:profile(fun() -> lists:seq(1, 16) end).

      ****** Process <0.224.0>    -- 100.00 % of total allocations ***
      MODULE   FUN/ARITY         CALLS  WORDS  PER CALL  [    %]
      erl_eval match_list/6          1      3         3  [ 3.19]
      erl_eval do_apply/7            1      3         3  [ 3.19]
      lists    reverse/1             1      4         4  [ 4.26]
      erl_eval add_bindings/2        1      5         5  [ 5.32]
      erl_eval expr_list/7           3      7         2  [ 7.45]
      erl_eval ret_expr/3            4     16         4  [17.02]
      erl_eval merge_bindings/4      3     24         8  [25.53]
      lists    seq_loop/3            5     32         6  [34.04]

      2> hprof:profile(fun() -> lists:seq(1, 16) end, #{pattern => [{lists, seq_loop, '_'}]}).
      ****** Process <0.247.0>    -- 100.00 % of total allocations ***
      MODUL FUN/ARITY   CALLS  WORDS  PER CALL  [     %]
      lists seq_loop/3      5     32         6  [100.00]
```

Ad-hoc profiling results may be printed in a few different ways. Following
examples are using `test` module defined like this:

```erlang
      -module(test).
      -export([test_spawn/0]).
      test_spawn() ->
          {Pid, MRef} = spawn_monitor(fun () -> lists:seq(1, 32) end),
          receive
              {'DOWN', MRef, process, Pid, normal} ->
                  done
          end.
```

Default format prints per-process statistics.

```text
        2> hprof:profile(test, test_spawn, []).

        ****** Process <0.176.0>    -- 23.66 % of total allocations ***
        MODULE FUN/ARITY        CALLS  WORDS  PER CALL  [    %]
        erlang spawn_monitor/1      1      2         2  [ 9.09]
        erlang spawn_opt/4          1      6         6  [27.27]
        test   test_spawn/0         1     14        14  [63.64]
                                          22            [100.0]

        ****** Process <0.177.0>    -- 76.34 % of total allocations ***
        MODULE FUN/ARITY   CALLS  WORDS  PER CALL  [    %]
        erlang apply/2         1      7         7  [ 9.86]
        lists  seq_loop/3      9     64         7  [90.14]
                                     71            [100.0]
```

This example prints the combined memory allocation of all processes, sorted by
total allocated words in the descending order

```erlang
        5> hprof:profile(test, test_spawn, [], #{report => {total, {words, descending}}}).

        MODULE FUN/ARITY        CALLS  WORDS  PER CALL  [    %]
        lists  seq_loop/3           9     64         7  [68.82]
        test   test_spawn/0         1     14        14  [15.05]
        erlang apply/2              1      7         7  [ 7.53]
        erlang spawn_opt/4          1      6         6  [ 6.45]
        erlang spawn_monitor/1      1      2         2  [ 2.15]
                                          93            [100.0]
```

You can also collect the profile for further inspection.

```erlang
      6> {done, ProfileData} = hprof:profile(fun test:test_spawn/0, #{report => return}).
      <...>
      7> hprof:format(hprof:inspect(ProfileData, process, {percent, descending})).

      ****** Process <0.223.0>    -- 23.66 % of total allocations ***
      MODULE FUN/ARITY        CALLS  WORDS  PER CALL  [    %]
      test   test_spawn/0         1     14        14  [63.64]
      erlang spawn_opt/4          1      6         6  [27.27]
      erlang spawn_monitor/1      1      2         2  [ 9.09]
      22            [100.0]

      ****** Process <0.224.0>    -- 76.34 % of total allocations ***
      MODULE FUN/ARITY   CALLS  WORDS  PER CALL  [    %]
      lists  seq_loop/3      9     64         7  [90.14]
      erlang apply/2         1      7         7  [ 9.86]
      71            [100.0]
```

By default, basic profiling takes into account all processes spawned from the
user-provided function (using `set_on_spawn` argument for trace/3 BIF). You can
limit the trace to a single process:

```text
      2> hprof:profile(test, test_spawn, [], #{set_on_spawn => false}).

      ****** Process <0.183.0>    -- 100.00 % of total allocations ***
      MODULE FUN/ARITY        CALLS  WORDS  PER CALL  [    %]
      erlang spawn_monitor/1      1      2         2  [ 9.09]
      erlang spawn_opt/4          1      6         6  [27.27]
      test   test_spawn/0         1     14        14  [63.64]
```

[](){: #pg_example }

Erlang programs may perform memory-intensive operations in processes that are
different from the original one. You can include multiple, new or even all
processes in the trace.

```text
      7> pg:start_link().
      {ok,<0.252.0>}
      8> hprof:profile(fun () -> pg:join(group, self()) end, #{rootset => [pg]}).
      ****** Process <0.252.0>    -- 52.86 % of total allocations ***
      MODULE     FUN/ARITY                 CALLS  WORDS  PER CALL  [    %]
      pg         leave_local_update_ets/5      1      2         2  [ 1.80]
      gen        reply/2                       1      3         3  [ 2.70]
      erlang     monitor/2                     1      3         3  [ 2.70]
      gen_server try_handle_call/4             1      3         3  [ 2.70]
      gen_server try_dispatch/4                1      3         3  [ 2.70]
      maps       iterator/1                    2      4         2  [ 3.60]
      maps       take/2                        1      6         6  [ 5.41]
      pg         join_local_update_ets/5       1      8         8  [ 7.21]
      pg         handle_info/2                 1      8         8  [ 7.21]
      pg         handle_call/3                 1      9         9  [ 8.11]
      gen_server loop/7                        2      9         4  [ 8.11]
      ets        lookup/2                      2     10         5  [ 9.01]
      pg         join_local/3                  1     11        11  [ 9.91]
      pg         notify_group/5                2     16         8  [14.41]
      erlang     setelement/3                  2     16         8  [14.41]
      111            [100.0]

      ****** Process <0.255.0>    -- 47.14 % of total allocations ***
      MODULE   FUN/ARITY         CALLS  WORDS  PER CALL  [    %]
      erl_eval match_list/6          1      3         3  [ 3.03]
      erlang   monitor/2             1      3         3  [ 3.03]
      lists    reverse/1             2      4         2  [ 4.04]
      pg       join/3                1      4         4  [ 4.04]
      erl_eval add_bindings/2        1      5         5  [ 5.05]
      erl_eval do_apply/7            2      6         3  [ 6.06]
      gen      call/4                1      8         8  [ 8.08]
      erl_eval expr_list/7           4     10         2  [10.10]
      gen      do_call/4             1     16        16  [16.16]
      erl_eval ret_expr/3            4     16         4  [16.16]
      erl_eval merge_bindings/4      3     24         8  [24.24]
      99            [100.0]
```

There is no default limit on the profiling time. It is possible to define such
limit for ad-hoc profile. If function being profiled does not return in a
specified amount of time, process is terminated with `kill` reason. Any unlinked
children started by the user-supplied function are kept, it is developer's
responsibility to ensure cleanup.

```erlang
      9> hprof:profile(timer, sleep, [100000], #{timeout => 1000}).
```

By default, only one ad-hoc or server-aided profiling session is allowed at any
point in time. It is possible to force multiple ad-hoc sessions concurrently,
but it is developer responsibility to ensure non-overlapping trace patterns.

```erlang
      1> hprof:profile(fun() -> lists:seq(1, 32) end,
          #{registered => false, pattern => [{lists, '_', '_'}]}).
```

## Server-aided profiling

Memory profiling can be done when your system is up and running. You can start
the `hprof` server, add trace patterns and processes to trace while your system
handles actual traffic. You can extract the data any time, inspect, and print.
The example below traces activity of all processes supervised by kernel:

```erlang
      1> hprof:start().
      {ok,<0.200.0>}
      2> hprof:enable_trace({all_children, kernel_sup}).
      34
      3> hprof:set_pattern('_', '_' , '_').
      16728
      4> Sample = hprof:collect().
      [{gen_server,try_dispatch,4,[{<0.154.0>,2,6}]},
      {erlang,iolist_to_iovec,1,[{<0.161.0>,1,8}]},
      <...>
      5 > hprof:format(hprof:inspect(Sample)).

      ****** Process <0.154.0>    -- 14.21 % of total allocations ***
      MODULE     FUN/ARITY       CALLS  WORDS  PER CALL  [    %]
      maps       iterator/1          2      4         2  [15.38]
      gen_server try_dispatch/4      2      6         3  [23.08]
      net_kernel handle_info/2       2     16         8  [61.54]
                                           26            [100.0]

      ****** Process <0.161.0>    -- 85.79 % of total allocations ***
      MODULE     FUN/ARITY            CALLS  WORDS  PER CALL  [    %]
      disk_log   handle/2                 2      2         1  [ 1.27]
      disk_log_1 maybe_start_timer/1      1      3         3  [ 1.91]
      disk_log_1 mf_write_cache/1         1      3         3  [ 1.91]
      <...>
```

[](){: #inspect_example }

It is possible to profile the entire running system, and then examine individual
processes:

```erlang
      1> hprof:start(), hprof:enable_trace(processes), hprof:set_pattern('_', '_' , '_').
      9041
      2> timer:sleep(10000), hprof:disable_trace(processes), Sample = hprof:collect().
      [{user_drv,server,3,[{<0.64.0>,12,136}]},
      {user_drv,contains_ctrl_g_or_ctrl_c,1,[{<0.64.0>,80,10}]},
      <...>
      3> Inspected = hprof:inspect(Sample, process, words), Shell = maps:get(self(), Inspected).
      {2743,
      [{shell,{enc,0},1,2,2,0.07291286912139992},
      <...>
      4> hprof:format(Shell).

      MODULE                 FUN/ARITY                             CALLS  WORDS  PER CALL  [    %]
      <...>
      erl_lint               start/2                                   2    300       150  [10.94]
      shell                  used_records/1                          114    342         3  [12.47]
```
""".
-moduledoc(#{since => "OTP @OTP-18756@"}).

%% API
-export([
    start/0,
    start_link/0,
    stop/0,
    set_pattern/3,
    clear_pattern/3,
    get_trace_map/0,
    enable_trace/1, enable_trace/2,
    disable_trace/1, disable_trace/2,
    pause/0,
    continue/0,
    restart/0,
    collect/0,
    %% ad-hoc profiling
    profile/1, profile/2, profile/3, profile/4,
    %% Analysis API
    inspect/1, inspect/3,
    format/1, format/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-compile([warn_missing_spec]).

%% typedefs for easier digestion

%% Trace spec: module() or '_', function or '_', arity or '_'
-type trace_pattern() :: {module(), Fun :: atom(), arity() | '_'}.

%% Trace map: accumulated view of multiple trace patterns
-doc "Traced functions (with their arities) grouped by module name.".
-type trace_map() :: #{module() => [{Fun :: atom(), arity()}]}.

%% Single trace_info call with associated module/function/arity
-doc "Raw data extracted from tracing BIFs.".
-type trace_info() :: {module(), Fun :: atom(), Arity :: non_neg_integer(),
    [{pid(), Count :: pos_integer(), Words :: pos_integer()}]}.

%% Combined report for a single function (one or all processes).
-doc "Inspected data for a single function of the specified `Module`.".
-type profile_line() :: {module(), Function :: {atom(), arity()},
    Count :: pos_integer(), Words :: pos_integer(), WordsPerCall :: non_neg_integer(), Percent :: float()}.

%% Single profiling attempt result.
-doc """
Profile of a single process, or combined profile of multiple processes, sorted
by a selected column.
""".
-type profile_result() :: {TotalWords :: non_neg_integer(), [profile_line()]}.

%% Convenience type used to sort the profiling results.
-doc """
Column to sort by `inspect/3`, or [`profile`](`profile/2`).

- **`module`** - Module name.

- **`function`** - Function name.

- **`calls`** - Number of calls to the function.

- **`words`** - Total number of words allocated throughout all calls to the
  function.

- **`words_per_call`** - Number of words allocated on average per function call.

- **`percent`** - Percentage of `words` to a total amount allocated during the
  entire profile collection.
""".
-type column() :: module | function | calls | words | words_per_call | percent.

%% Sort by
-type sort_by() :: column() | {column(), ascending} | {column(), descending}.

%% Selected options allowed for enable/disable trace
-doc """
Options for enabling heap profiling of the selected processes, see
`enable_trace/2`.
""".
-type trace_options() :: #{
    set_on_spawn => boolean()
}.

%% Convenience type to define which processes to trace
-type rootset() :: [process()] |   %% list of pids/registered names
    processes |
    existing_processes |
    new_processes.

-doc "Ad-hoc profiler options, see [`profile`](`profile/2`).".
-type profile_options() :: #{
    timeout => timeout(),                           %% stop profiling after the timeout
    pattern => trace_pattern() | [trace_pattern()], %% list of patterns to trace
    set_on_spawn => boolean(),                      %% trace spawned processes or not (true by default)
    rootset => rootset(),                           %% extra processes to trace
    report => return | process | total | {process, sort_by()} | {total, sort_by()},   %% print or return results
    device => io:device(),                          %% device to report to
    registered => false | {local, atom()}           %% register the profiler process (to detect concurrent attempts)
}.

%%--------------------------------------------------------------------
%% Server-aided API
-doc """
Starts the server, not supervised. Profiling server stores current trace
patterns and ensures a single instance of heap profiler is running.
""".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec start() -> {'ok', Pid} | {'error', Reason} when Pid :: pid(), Reason :: {'already_started', Pid}.
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% @doc Starts the process and links it to the caller.
-doc "Starts the server, supervised by the calling process.".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec start_link() -> {'ok', Pid} | {'error', Reason} when Pid :: pid(), Reason :: {'already_started', Pid}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-doc "Stops the `hprof`, disabling memory tracing that has been enabled.".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

-doc """
Turns tracing on for the supplied pattern. Requires running `hprof`. Patterns
are additive, following the same rules as `erlang:trace_pattern/3`. Returns
number of functions matching the supplied pattern.

```erlang
1> hprof:set_pattern(lists, seq, '_').
2
2> hprof:set_pattern(lists, keyfind, 3).
1
3> hprof:get_trace_map().
#{lists => [{keyfind,3},{seq,2},{seq,3}]}
```

If there are no functions matching the pattern, error is returned

```erlang
> hprof:set_pattern(no_module, func, '_').
{error,{trace_pattern,no_module,func,'_'}}
```
""".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec set_pattern(module(), atom(), arity() | '_') -> ok | {error, {trace_pattern, trace_pattern()}}.
set_pattern(Mod, Fun, Arity) ->
    gen_server:call(?MODULE, {set_pattern, Mod, Fun, Arity}, infinity).

%% @doc Stops tracing all or specific function patterns.
-doc """
Turns tracing off for the supplied pattern.

```erlang
1> hprof:set_pattern(lists, seq, '_').
2
2> hprof:clear_pattern(lists, seq, 3).
1
3> hprof:get_trace_map().
#{lists => [{seq,2}]}
```
""".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec clear_pattern(module(), atom(), arity() | '_') -> ok.
clear_pattern(Mod, Fun, Arity) ->
    gen_server:call(?MODULE, {clear_pattern, Mod, Fun, Arity}, infinity).

%% @doc Returns current trace map.
-doc "Returns a map of module names to functions with their arities.".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec get_trace_map() -> trace_map().
get_trace_map() ->
    gen_server:call(?MODULE, get_trace_map).

%% @doc Returns statistics for current trace map.
-doc false.
-spec collect() -> [trace_info()].
collect() ->
    gen_server:call(?MODULE, collect, infinity).

%% Process identified by a PID or a registered name.
-doc "Either process identified (pid), or a registered process name.".
-type process() :: pid() | atom().

%% @doc Shortcut for erlang:trace/3 BIF touching only memory tracing flags.
%%      Returns number of successful operations, and list of those unsuccessful
%%      if the list was supplied. By default applies set_on_spawn flag.
-doc """
The same as
[`enable_trace` ](`enable_trace/2`)`(Spec, #{set_on_spawn => true})`.
""".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec enable_trace(Spec) -> non_neg_integer()
    when Spec :: pid() |
        processes |
        new_processes |
        existing_processes |
        {children | all_children, process()};
    ([process()]) -> non_neg_integer() | {non_neg_integer(), [process()]}.
enable_trace(Rootset) ->
    enable_trace(Rootset, #{set_on_spawn => true}).

-doc """
Similar to `erlang:trace/3`, but supports a few more options for heap tracing
convenience.

`Spec` is either a process identifier (pid) for a local process, one of the
following atoms, or a list of local process identifiers or their registered
names:

- **`processes`** - All currently existing processes and all that will be
  created in the future.

- **`existing_processes`** - All currently existing processes.

- **`new_processes`** - All processes that will be created in the future.

- **`children`** - All currently running processes that were directly spawned by
  the specified process. This mode is helpful for tracing workers of a single
  supervisor.

- **`all_children`** - All currently running processes that were spawned by the
  specified process, or any recursive descendant of it. This mode is designed to
  facilitate tracing of supervision trees.

> #### Note {: .info }
>
> Heap profiling server does not keep track of processes that were added to the
> tracing set. It is permitted to stop the profiling server (wiping out any
> accumulated data), restart the server, set entirely different tracing pattern
> keeping the list of traced processes for future use. Use
> [`disable_trace`(processes)](`disable_trace/2`) to clear the list of traced
> processes.

Specify `Options` to modify heap tracing behaviour:

- **`set_on_spawn`** - Automatically start heap tracing for processes spawned by
  the traced process. On by default.
""".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec enable_trace(Spec, trace_options()) -> non_neg_integer()
    when Spec :: pid() |
        processes |
        new_processes |
        existing_processes |
        {children | all_children, process()};
    ([process()], trace_options()) -> non_neg_integer() | {non_neg_integer(), [process()]}.
enable_trace(Procs, Options) when Procs =:= processes; Procs =:= new_processes; Procs =:= existing_processes ->
    erlang:trace(Procs, true, trace_options(Options));
enable_trace({Children, PidOrName}, Options) when Children =:= children; Children =:= all_children ->
    Pids = children(Children, PidOrName),
    toggle_trace(Pids, true, trace_options(Options), 0, []);
enable_trace(Pid, Options) when is_pid(Pid); is_atom(Pid) ->
    toggle_process_trace(Pid, true, trace_options(Options));
enable_trace(List, Options) when is_list(List) ->
    toggle_trace(List, true, trace_options(Options), 0, []).

-doc """
The same as
[`disable_trace` ](`disable_trace/2`)`(Spec, #{set_on_spawn => true})`.
""".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec disable_trace(Spec) -> non_neg_integer()
    when Spec :: pid() |
        processes |
        new_processes |
        existing_processes |
        {children | all_children, process()};
    ([process()]) -> non_neg_integer() | {non_neg_integer(), [process()]}.
disable_trace(Rootset) ->
    disable_trace(Rootset, #{set_on_spawn => true}).

-doc """
Stops accumulating heap traces for specified processes. See `enable_trace/2` for
options description.

Profile accumulated before process is removed from the traced list is retained.
This allows to enable tracing for many or even all processes in the system,
sleep for a short period of time, then disable tracing for all processes,
avoiding system overload, but keeping profile data.
""".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec disable_trace(Spec, trace_options()) -> non_neg_integer()
    when Spec :: pid() |
        processes |
        new_processes |
        existing_processes |
        {children | all_children, process()};
    ([process()], trace_options()) -> non_neg_integer() | {non_neg_integer(), [process()]}.
disable_trace(Procs, Options) when Procs =:= processes; Procs =:= new_processes; Procs =:= existing_processes ->
    erlang:trace(Procs, false, trace_options(Options));
disable_trace({Children, PidOrName}, Options) when Children =:= children; Children =:= all_children ->
    Pids = children(Children, PidOrName),
    toggle_trace(Pids, false, trace_options(Options), 0, []);
disable_trace(Pid, Options) when is_pid(Pid); is_atom(Pid) ->
    toggle_process_trace(Pid, false, trace_options(Options));
disable_trace(List, Options) when is_list(List) ->
    toggle_trace(List, false, trace_options(Options), 0, []).

%% @doc Pauses tracing for the entire trace_map
-doc """
Pauses trace collection for all currently traced functions, keeping all traces
intact. Use `continue/0` to resume trace collection.
""".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec pause() -> ok | not_running.
pause() ->
    gen_server:call(?MODULE, pause, infinity).

%% @doc Continues paused tracing.
-doc "Resumes previously paused heap profiling.".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec continue() -> ok | not_paused.
continue() ->
    gen_server:call(?MODULE, continue, infinity).

%% @doc Restarts tracing, clearing current statistics. Profiling could be
%%      running or paused.
-doc """
Clears accumulated profiles. If profiling was paused prior to calling `restart`,
it gets continued.
""".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec restart() -> ok.
restart() ->
    gen_server:call(?MODULE, restart, infinity).

%%--------------------------------------------------------------------
%% Common API

%% @doc Transforms raw collected data into shape suitable for analysis and printing.
-doc """
The same as [`inspect` ](`inspect/3`)`(Profile, process, percent)`. Transforms
raw profile into a map of process identifiers to a tuple containing total count
of words allocated, and a list of all traced functions sorted in the ascending
order by the allocation percentage.
""".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec inspect([trace_info()]) -> #{pid() => profile_result()}.
inspect(Profile) ->
    inspect(Profile, process, percent).

-doc """
Transforms raw data returned by tracing BIFs into a form convenient for
subsequent analysis and formatting. Returns a map of process identifiers with
corresponding profile data sorted by the selected column.

Inspected profile can be leveraged to print
[individual process allocations](`m:hprof#inspect_example`).

Combines raw profile from multiple processes into a single summary sorted by the
selected column.

A single profiling session may contain data from thousands or even millions
processes. This inspection mode allows to quickly glance through the allocation
summary, discarding process identifiers and keeping only totals.
""".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec inspect([trace_info()], Report :: process, sort_by()) -> #{pid() => profile_result()};
    ([trace_info()], Report :: total, sort_by()) -> profile_result().
inspect(Profile, process, SortBy) ->
    maps:map(
        fun (_Pid, {Total, Stats}) ->
            {Total, inspect_sort(Stats, SortBy)}
        end, inspect_processes(Profile, #{}));
inspect(Profile, total, SortBy) ->
    GrandTotal = lists:sum([Words || {_M, _F, _A, Mem} <- Profile, {_P, _C, Words} <- Mem]),
    TotalStats = [inspect_total(M, F, A, GrandTotal, Mem) || {M, F, A, Mem} <- Profile],
    {GrandTotal, inspect_sort(TotalStats, SortBy)}.

%% @doc Formats inspect()-ed totals and per-function data
-doc(#{equiv => format/2}).
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec format(profile_result() | #{pid => profile_result()}) -> ok.
format(Inspected) ->
    format_impl([], Inspected).

-doc "Formats profile transformed with [`inspect` ](`inspect/3`)to a specified device.".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec format(io:device(), profile_result() | #{pid => profile_result()}) -> ok.
format(IoDevice, Inspected) ->
    format_impl(IoDevice, Inspected).

%%--------------------------------------------------------------------
%% Ad-hoc API

%% @doc Runs the function/MFA with heap tracing enabled.
-doc(#{equiv => profile/4}).
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec profile(fun(() -> term())) -> ok | {term(), [trace_info()]}.
profile(Fun) when is_function(Fun) ->
    profile(Fun, #{}).

-doc(#{equiv => profile/4}).
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec profile(fun(() -> term()), profile_options()) -> ok | {term(), [trace_info()]}.
profile(Fun, Options) when is_function(Fun) ->
    do_profile(Fun, Options).

-doc(#{equiv => profile/4}).
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec profile(module(), Fun :: atom(), Args :: [term()]) -> ok | {term(), [trace_info()]}.
profile(Module, Function, Args) when is_atom(Module), is_atom(Function), is_list(Args) ->
    profile(Module, Function, Args, #{}).

-doc """
Produces ad-hoc heap profile for function `Fun` or `Module`:`Function` call. By
default, result is formatted to the output device, use `report` option to change
this behaviour.

Ad-hoc profiling starts a new instance of `hprof` server, runs the profiling
routine, extracts results and shuts the server down. If `hprof` is already
running (for server-aided profiling), default ad-hoc profiler options block this
call to avoid mixing results from several independent instances. Use
`registered => false` option to override this behaviour.

Ad-hoc profiler supports following`Options`:

- **`device`** - Specifies I/O devices to print the profile to. Useful to
  redirect text output to console or `standard_error`.

- **`pattern`** - Specifies trace pattern, or a list of trace patterns to
  enable. By default, all functions (`{'_', '_', '_'}`) are traced.

- **`registered`** - Specifies `hprof` registered process name. Use `false` to
  leave the process unregistered, or `{local, myname}` to register the process
  under a different name.

- **`report`** - Controls output format. The default is `process`, printing
  per-process heap profiling data sorted by percentage of the total allocation.
  Specify `report => return` to suppress printing and get the raw data for
  further evaluation with `inspect/3` and formatting with `format/2`.

- **`rootset`** - Includes extra processes in the trace list. Useful for
  profiling allocations for `m:gen_server`, calls, or other allocations caused
  by inter-process communications. See [example](`m:hprof#pg_example`).

- **`set_on_spawn`** - Automatically start heap tracing for processes spawned by
  the traced process. On by default.

- **`timeout`** - Terminate profiling after the specified amount of time
  (milliseconds).
""".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec profile(module(), Fun :: atom(), Args :: [term()], profile_options()) -> ok | {term(), [trace_info()]}.
profile(Module, Function, Args, Options) when is_atom(Module), is_atom(Function), is_list(Args) ->
    do_profile({Module, Function, Args}, Options).

%%--------------------------------------------------------------------
%% gen_server implementation
-record(hprof_state, {
    trace_map = #{} :: trace_map(),
    paused = false :: boolean(),
    ad_hoc = undefined :: undefined |
        {pid(), Timer :: reference() | false, Patterns :: [trace_pattern()],
            RootSet :: rootset(), ReplyTo :: gen_server:from()}
}).

-type state() :: #hprof_state{}.

-doc false.
-spec init([]) -> {ok, state()}.
init([]) ->
    false = erlang:process_flag(trap_exit, true), %% need this for reliable terminate/2 call
    {ok, #hprof_state{}}.

-doc false.
-spec handle_call(term(), gen_server:from(), state()) -> {reply | noreply, term(), state()}.
handle_call({set_pattern, M, F, A}, _From, #hprof_state{trace_map = Map} = State) ->
    {Reply, NewMap} = enable_pattern(M, F, A, Map),
    {reply, Reply, State#hprof_state{trace_map = NewMap}};
handle_call({clear_pattern, M, F, A}, _From, #hprof_state{trace_map = Map} = State) ->
    {Ret, NewMap} = disable_pattern(M, F, A, Map),
    {reply, Ret, State#hprof_state{trace_map = NewMap}};
handle_call(get_trace_map, _From, #hprof_state{trace_map = Map} = State) ->
    {reply, Map, State};
handle_call(pause, _From, #hprof_state{paused = true} = State) ->
    {reply, not_running, State};
handle_call(pause, _From, #hprof_state{trace_map = Map, paused = false} = State) ->
    foreach(Map, pause),
    {reply, ok, State#hprof_state{paused = true}};
handle_call(continue, _From, #hprof_state{paused = false} = State) ->
    {reply, running, State};
handle_call(continue, _From, #hprof_state{trace_map = Map} = State) ->
    foreach(Map, true),
    {reply, ok, State#hprof_state{paused = false}};
handle_call(restart, _From, #hprof_state{trace_map = Map} = State) ->
    foreach(Map, restart),
    {reply, ok, State#hprof_state{paused = false}};
handle_call(collect, _From, #hprof_state{trace_map = Map} = State) ->
    {reply, collect(Map), State};
handle_call({profile, What, Options}, From, #hprof_state{ad_hoc = undefined, trace_map = Map} = State) ->
    %% ad-hoc profile routed via gen_server to handle 'EXIT' signal
    {Pid, Timer, Patterns, RootSet, NewMap} = ad_hoc_run(What, Options, Map),
    {noreply, State#hprof_state{ad_hoc = {Pid, Timer, Patterns, RootSet, From}, trace_map = NewMap}};
handle_call({profile, _What, _Options}, _From, State) ->
    {reply, {error, running}, State}.

-doc false.
-spec handle_cast(term(), state()) -> no_return().
handle_cast(_Req, _State) ->
    erlang:error(notsup).

-doc false.
-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'EXIT', Pid, Reason}, #hprof_state{ad_hoc = {Pid, Timer, Patterns, RootSet, From},
    trace_map = Map} = State) ->
    _ = disable_trace(RootSet),
    Profile = collect(Map),
    gen:reply(From, {Reason, Profile}),
    Timer =/= false andalso erlang:cancel_timer(Timer),
    {noreply, State#hprof_state{ad_hoc = undefined, trace_map = disable_patterns(Patterns, Map)}};

handle_info({cancel, Pid}, #hprof_state{ad_hoc = {Pid, _Timer, Patterns, RootSet, From},
    trace_map = Map} = State) ->
    _ = disable_trace(RootSet),
    Profile = collect(Map),
    gen:reply(From, {{'EXIT', timeout}, Profile}),
    {noreply, State#hprof_state{ad_hoc = undefined, trace_map = disable_patterns(Patterns, Map)}}.

-doc false.
-spec terminate(term(), state()) -> ok.
terminate(_Reason, #hprof_state{trace_map = Map}) ->
    clear_pattern(Map),
    ok.

%%--------------------------------------------------------------------
%% Internal implementation

-include_lib("kernel/include/logger.hrl").

%% Add the trace of the specified module to the accumulator
collect_trace(Mod, FunList, Acc) ->
    {Fail, Ret} = lists:foldl(
        fun ({Fun, Arity}, {Fail, Prev}) ->
            case combine_trace(erlang:trace_info({Mod, Fun, Arity}, call_memory)) of
                skip ->
                    {Fail, Prev};
                fail ->
                    {true, Prev};
                Tr ->
                    {Fail, [{Mod, Fun, Arity, Tr} | Prev]}
            end
        end, {false, Acc}, FunList),
    %% module may have been hot-code reloaded, or tracing was broken by something else
    Fail andalso begin
        ?LOG_WARNING(
            "hprof encountered an error tracing module ~s, was it reloaded or untraced?",
            [Mod])
        end,
    Ret.

combine_trace({call_memory, []}) ->
    skip;
%% It is possible that due to hot code reload event
%% some function is no longer traced, while it was supposed to.
%% Reinstating tracing automatically is wrong thing to do, because
%% statistics won't be correct anyway. Hence the warning in the user
%% guide, guarding against hot code reload while tracing.
combine_trace({call_memory, false}) ->
    fail;
combine_trace({call_memory, Mem}) ->
    case [{Pid, Calls, Words} || {Pid, Calls, Words} <- Mem, Words > 0] of
        [] ->
            skip;
        NonZero ->
            NonZero
    end.

%% Inspection: iterate over collected traces, return map of
%%  #{Pid => [{M, {F, A}, Calls, TotalWords, WordsPerCall, Percentage}], unsorted.
inspect_processes([], Acc) ->
    maps:map(
        fun (_Pid, {Total, Lines}) ->
            {Total, [{M, {F, A}, Calls, Words, PerCall, Words * 100 / Total}
                || {M, F, A, Calls, Words, PerCall} <- Lines]}
        end, Acc);
inspect_processes([{_M, _F, _A, []} | Tail], Acc) ->
    inspect_processes(Tail, Acc);
inspect_processes([{M, F, A, [{Pid, Calls, Words} | MemTail]} | Tail], Acc) ->
    ProfLine = {M, F, A, Calls, Words, Words div Calls},
    inspect_processes([{M, F, A, MemTail} | Tail],
        maps:update_with(Pid, fun ({Grand, L}) -> {Grand + Words, [ProfLine | L]} end, {Words, [ProfLine]}, Acc)).

%% Inspection: remove Pid information from the Profile, return list of
%%  [{M, F, A, TotalCalls, TotalWords, WordsPerCall, Percentage}]
inspect_total(M, F, A, GrandTotal, Mem) ->
    {TC, TW} = lists:foldl(
        fun ({_Pid, Calls, Words}, {TotalCalls, TotalWords}) ->
            {TotalCalls + Calls, TotalWords + Words}
        end, {0, 0}, Mem),
    {M, {F, A}, TC, TW, TW div TC, TW * 100 / GrandTotal}.

%% Returns "sort by" column index
column(module) -> {1, ascending};
column(function) -> {2, ascending};
column(calls) -> {3, ascending};
column(words) -> {4, ascending};
column(words_per_call) -> {5, ascending};
column(percent) -> {6, ascending}.

%% Sorts by column name, ascending/descending
inspect_sort(Profile, undefined) ->
    Profile;
inspect_sort(Profile, {Column, ascending}) when is_integer(Column) ->
    lists:keysort(Column, Profile);
inspect_sort(Profile, {Column, descending}) when is_integer(Column) ->
    lists:reverse(lists:keysort(Column, Profile));
inspect_sort(Profile, {Column, Direction}) when is_atom(Column) ->
    {Col, _Skip} = column(Column),
    inspect_sort(Profile, {Col, Direction});
inspect_sort(Profile, Column) when is_atom(Column) ->
    inspect_sort(Profile, column(Column)).

%% Formats the inspected profile to the Device, which could be [] meaning
%%  default output.
format_impl(Device, Empty) when Empty =:= #{} ->
    format_out(Device, "Memory trace is empty~n", []);
format_impl(Device, Inspected) when is_map(Inspected) ->
    %% grab the total-total words
    GrandTotal = maps:fold(fun (_Pid, {Total, _Profile}, Acc) -> Acc + Total end, 0, Inspected),
    %% per-process printout
    maps:foreach(
        fun(Pid, {Total, _} = Profile) ->
            format_out(Device, "~n****** Process ~w    -- ~.2f % of total allocations *** ~n",
                [Pid, 100 * Total / GrandTotal]),
            format_impl(Device, Profile)
        end, Inspected);
format_impl(Device, {Total, Inspected}) when is_list(Inspected) ->
    %% viewport size
    %% Viewport = case io:columns() of {ok, C} -> C; _ -> 80 end,
    %% layout: module and fun/arity columns are resizable, the rest are not
    %% convert all lines to strings
    {Widths, Lines} = lists:foldl(
        fun ({Mod, {F, A}, Calls, Words, WPC, Percent}, {Widths, Ln}) ->
            Line = [atom_to_list(Mod), lists:flatten(io_lib:format("~tw/~w", [F, A])),
                integer_to_list(Calls), integer_to_list(Words), integer_to_list(WPC),
                float_to_list(Percent, [{decimals, 2}])],
            NewWidths = [erlang:max(Old, New) || {Old, New} <- lists:zip([string:length(L) || L <- Line], Widths)],
            {NewWidths, [Line | Ln]}
        end, {[0, 0, 5, 5, 8, 5], []}, Inspected),
    %% figure our max column widths according to viewport (cut off module/funArity)
    FilteredWidths = Widths,
    %% figure out formatting line
    Fmt = lists:flatten(io_lib:format("~~.~ws ~~.~wts  ~~~ws  ~~~ws  ~~~ws  [~~~ws]~~n", FilteredWidths)),
    %% print using this format
    format_out(Device, Fmt, ["MODULE", "FUN/ARITY", "CALLS", "WORDS", "PER CALL", "%"]),
    [format_out(Device, Fmt, Line) || Line <- lists:reverse(Lines)],
    format_out(Device, Fmt, [" ", " ", " ", integer_to_list(Total), " ", "100.0"]).

%% format implementation that uses [] as a way to tell "default output"
format_out([], Fmt, Args) ->
    io:format(Fmt, Args);
format_out(Device, Fmt, Args) ->
    io:format(Device, Fmt, Args).

%% pattern collapse code
enable_pattern('_', '_', '_', _Acc) ->
    %% need to re-trace everything, probably some new modules were loaded
    %% discard any existing trace pattern
    lists:foldl(
        fun({Mod, _}, {Total, Acc}) ->
            Plus = erlang:trace_pattern({Mod, '_', '_'}, true, [call_memory]),
            {Total + Plus, Acc#{Mod => Mod:module_info(functions)}}
        end, {0, #{}}, code:all_loaded());
enable_pattern(Mod, '_', '_', Acc) ->
    %% code may have been hot-loaded, redo the trace
    case erlang:trace_pattern({Mod, '_', '_'}, true, [call_memory]) of
        0 ->
            {{error, {trace_pattern, Mod, '_', '_'}}, Acc};
        Traced ->
            {Traced, Acc#{Mod => Mod:module_info(functions)}}
    end;
enable_pattern(Mod, Fun, '_', Acc) ->
    case erlang:trace_pattern({Mod, Fun, '_'}, true, [call_memory]) of
        0 ->
            {{error, {trace_pattern, Mod, Fun, '_'}}, Acc};
        Traced ->
            Added = [{F, A} || {F, A} <- Mod:module_info(functions), F =:= Fun],
            NewMap = maps:update_with(Mod,
                fun (FAs) ->
                    Added ++ [{F, A} || {F, A} <- FAs, F =/= Fun]
                end, Added, Acc),
            {Traced, NewMap}
    end;
enable_pattern(Mod, Fun, Arity, Acc) ->
    case erlang:trace_pattern({Mod, Fun, Arity}, true, [call_memory]) of
        0 ->
            {{error, {trace_pattern, Mod, Fun, Arity}}, Acc};
        1 ->
            {1, maps:update_with(Mod,
                fun (FAs) -> [{Fun, Arity} | FAs -- [{Fun, Arity}]] end, [{Fun, Arity}], Acc)}
    end.

%% pattern collapse code for un-tracing
disable_pattern('_', '_', '_', _Acc) ->
    Traced = erlang:trace_pattern({'_', '_', '_'}, false, [call_memory]),
    {Traced, #{}};
disable_pattern(Mod, '_', '_', Acc) when is_map_key(Mod, Acc) ->
    Traced = erlang:trace_pattern({Mod, '_', '_'}, false, [call_memory]),
    {Traced, maps:remove(Mod, Acc)};
disable_pattern(Mod, Fun, '_', Acc) when is_map_key(Mod, Acc) ->
    Traced = erlang:trace_pattern({Mod, Fun, '_'}, false, [call_memory]),
    {Traced, maps:update_with(Mod,
        fun (FAs) -> [{F, A} || {F, A} <- FAs, F =/= Fun] end, Acc)};
disable_pattern(Mod, Fun, Arity, Acc) when is_map_key(Mod, Acc) ->
    Traced = erlang:trace_pattern({Mod, Fun, Arity}, false, [call_memory]),
    {Traced, maps:update_with(Mod, fun (FAs) -> FAs -- [{Fun, Arity}] end, Acc)};
disable_pattern(Mod, Fun, Arity, Acc) ->
    {{error, {not_traced, Mod, Fun, Arity}}, Acc}.

disable_patterns(Patterns, Map) ->
    lists:foldl(fun ({M, F, A}, Acc) -> {_, New} = disable_pattern(M, F, A, Acc), New end, Map, Patterns).

%% ad-hoc profiler implementation
do_profile(What, Options) ->
    %% start a new hprof server, potentially registered to a new name
    Pid = start_result(start_internal(maps:get(registered, Options, {local, ?MODULE}))),
    try
        {Ret, Profile} = gen_server:call(Pid, {profile, What, Options}, infinity),
        return_profile(maps:get(report, Options, process), Profile, Ret,
            maps:get(device, Options, []))
    after
        gen_server:stop(Pid)
    end.

start_internal(false) ->
    gen_server:start_link(?MODULE, [], []);
start_internal({local, Name}) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

start_result({ok, Pid}) -> Pid;
start_result({error, Reason}) -> erlang:error(Reason).

return_profile(return, Profile, Ret, _Device) ->
    {Ret, Profile};
return_profile(process, Profile, Ret, Device) ->
    return_profile({process, percent}, Profile, Ret, Device);
return_profile(total, Profile, Ret, Device) ->
    return_profile({total, percent}, Profile, Ret, Device);
return_profile({Agg, Sort}, Profile, _Ret, Device) ->
    format_impl(Device, inspect(Profile, Agg, Sort)).

%% @doc clears tracing for the entire trace map passed
-spec clear_pattern(trace_map()) -> ok.
clear_pattern(Existing) ->
    maps:foreach(
        fun (Mod, FunArity) ->
            [erlang:trace_pattern({Mod, F, A}, false, [call_memory]) || {F, A} <- FunArity]
        end, Existing).

trace_options(#{set_on_spawn := false}) ->
    [call, silent];
trace_options(_) ->
    [call, silent, set_on_spawn].

children(Children, PidOrName) when is_atom(PidOrName) ->
    case erlang:whereis(PidOrName) of
        undefined -> [];
        Pid -> children(Children, Pid)
    end;
children(children, Pid) when is_pid(Pid) ->
    [P || P <- erlang:processes(), erlang:process_info(P, parent) =:= {parent, Pid}];
children(all_children, Pid) when is_pid(Pid) ->
    %% build a process tree (could use a digraph too)
    Tree = maps:groups_from_list(
        fun (P) ->
            case erlang:process_info(P, parent) of
                undefined -> undefined;
                {parent, Parent} -> Parent
            end
        end, erlang:processes()),
    select_pids(Tree, Pid).

select_pids(Tree, Pid) ->
    case maps:find(Pid, Tree) of
        error -> [];
        {ok, Children} ->
            Children ++ lists:concat([select_pids(Tree, C) || C <- Children])
    end.

toggle_process_trace(Pid, On, Flags) when is_pid(Pid) ->
    try
        1 = erlang:trace(Pid, On, Flags)
    catch _:_ ->
        0
    end;
toggle_process_trace(Name, On, Flags) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined ->
            0;
        Pid ->
            toggle_process_trace(Pid, On, Flags)
    end.

toggle_trace([], _On, _Flags, Success, []) ->
    Success;
toggle_trace([], _On, _Flags, Success, Failure) ->
    {Success, lists:reverse(Failure)};
toggle_trace([Pid | Tail], On, Flags, Success, Failure) when is_pid(Pid) ->
    {NS, NF} =
        try
            1 = erlang:trace(Pid, On, Flags),
            {Success + 1, Failure}
        catch _:_ ->
            {Success, [Pid | Failure]}
        end,
    toggle_trace(Tail, On, Flags, NS, NF);
toggle_trace([Name | Tail], On, Flags, Success, Failure) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined ->
            toggle_trace(Tail, On, Flags, Success, [Name | Failure]);
        Pid ->
            {NS, NF} =
                try
                    1 = erlang:trace(Pid, On, Flags),
                    {Success + 1, Failure}
                catch _:_ ->
                    {Success, [Name | Failure]}
                end,
            toggle_trace(Tail, On, Flags, NS, NF)
    end.

%% @doc Collects memory tracing data (usable for inspect()) for
%%      all traced functions.
-spec collect(trace_map()) -> [trace_info()].
collect(Pattern) ->
    maps:fold(fun collect_trace/3, [], Pattern).

foreach(Map, Action) ->
    maps:foreach(
        fun (Mod, Funs) ->
            [erlang:trace_pattern({Mod, F, A}, Action, [call_memory]) || {F, A} <- Funs]
        end, Map).

ad_hoc_run(What, Options, Map) ->
    %% add missing patterns
    Patterns = make_list(maps:get(pattern, Options, {'_', '_', '_'})),
    NewMap = lists:foldl(
        fun({M, F, A}, Acc) ->
            {_, NewMap} = enable_pattern(M, F, A, Acc),
            NewMap
        end, Map, Patterns),
    %% check whether spawned processes are also traced
    OnSpawn = maps:get(set_on_spawn, Options, true),
    %% enable tracing for items in the rootset
    RootSet = maps:get(rootset, Options, []),
    _ = enable_trace(RootSet), %% ignore errors when setting up rootset trace
    %% spawn a separate process to run the user-supplied MFA
    %% if RootSet is 'processes' or 'new_processes', skip the trace flags
    Flags = trace_flags(RootSet, OnSpawn),
    Pid = spawn_profiled(What, Flags),
    %% start timer to terminate the function being profiled if it takes too long
    %%  to complete
    Timer = is_map_key(timeout, Options) andalso
        erlang:send_after(maps:get(timeout, Options), self(), {cancel, Pid}),
    {Pid, Timer, Patterns, RootSet, NewMap}.

trace_flags(processes, _) -> [];
trace_flags(new_processes, _) -> [];
trace_flags(_, true) -> [call, silent, set_on_spawn];
trace_flags(_, false) -> [call, silent].

make_list({M, F, A}) -> [{M, F, A}];
make_list(List) -> List.

spawn_profiled(Fun, Flags) when is_function(Fun) ->
    spawn_link(
        fun() ->
            Flags =/= [] andalso begin 1 = erlang:trace(self(), true, Flags) end,
            Ret = catch Fun(),
            Flags =/= [] andalso begin 1 = erlang:trace(self(), false, Flags) end,
            exit(Ret)
        end);
spawn_profiled({M, F, A}, Flags) ->
    spawn_link(
        fun() ->
            Flags =/= [] andalso begin 1 = erlang:trace(self(), true, Flags) end,
            Ret = catch erlang:apply(M, F, A),
            Flags =/= [] andalso begin 1 = erlang:trace(self(), false, Flags) end,
            exit(Ret)
        end).
