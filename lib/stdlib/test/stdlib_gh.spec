{suites,"../stdlib_test",all}.
{skip_groups,"../stdlib_test",stdlib_bench_SUITE,
             [binary,base64,gen_server,gen_statem,unicode],
             "Benchmark only"}.
{skip_groups,"../stdlib_test",ets_SUITE,
             [benchmark],
             "Benchmark only"}.
{event_handler, {cte_track, []}}.
{enable_builtin_hooks, false}.
{ct_hooks, [{cth_log_redirect, [{mode, replace}]}]}.
