% {merge_tests,false}.
{alias,dir,"../ssl_test"}.

{suites,dir,all}.
{skip_suites,dir,[openssl_ocsp_SUITE],"Unstable testcases"}.
{skip_groups,all_nodes, dir, openssl_renegotiate_SUITE, 'dtlsv1.2',
 {cases,[erlang_client_openssl_server_renegotiate,
         erlang_client_openssl_server_renegotiate_after_client_data]},
 "Broken testcases"}.
{skip_groups,dir,ssl_bench_SUITE,setup,"Benchmarks run separately"}.
{skip_groups,dir,ssl_bench_SUITE,payload,"Benchmarks run separately"}.
{skip_groups,dir,ssl_bench_SUITE,pem_cache,"Benchmarks run separately"}.
{skip_groups,dir,ssl_dist_bench_SUITE,setup,"Benchmarks run separately"}.
{skip_groups,dir,ssl_dist_bench_SUITE,roundtrip,"Benchmarks run separately"}.
{skip_groups,dir,ssl_dist_bench_SUITE,throughput,"Benchmarks run separately"}.
{skip_groups,dir,ssl_dist_bench_SUITE,sched_utilization,"Benchmarks run separately"}.

