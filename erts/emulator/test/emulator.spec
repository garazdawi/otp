{enable_builtin_hooks, false}.
{ct_hooks, [ts_install_cth]}.
{suites,"../emulator_test",all}.
{skip_groups,"../emulator_test",hash_SUITE,[phash2_benchmark],"Benchmark only"}.
