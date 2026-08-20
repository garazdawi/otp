{suites,"../kernel_test", all}.
{skip_suites,"../kernel_test",[logger_stress_SUITE],"Benchmarks only"}.
{skip_cases, "../kernel_test", global_SUITE,
 [many_nodes, lost_connection2, simple_resolve2, stress_partition],
 "Broken in docker"}.
{skip_cases, "../kernel_test", logger_disk_log_h_SUITE,
 [op_switch_to_flush],
 "Times out in docker: the overload flood outruns the container's disk"}.
