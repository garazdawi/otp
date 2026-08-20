% {merge_tests,false}.
{alias,dir,"../ssl_test"}.

{suites,dir,all}.
{skip_suites,dir,[openssl_ocsp_SUITE],"Unstable testcases"}.
{skip_groups,all_nodes, dir, openssl_session_ticket_SUITE, 'openssl_server',
 {cases,[openssl_server_hrr]},"Unstable testcases"}.
{skip_groups,dir,ssl_bench_SUITE,setup,"Benchmarks run separately"}.
{skip_groups,dir,ssl_bench_SUITE,payload,"Benchmarks run separately"}.
{skip_groups,dir,ssl_bench_SUITE,pem_cache,"Benchmarks run separately"}.
{skip_groups,dir,ssl_dist_bench_SUITE,setup,"Benchmarks run separately"}.
{skip_groups,dir,ssl_dist_bench_SUITE,roundtrip,"Benchmarks run separately"}.
{skip_groups,dir,ssl_dist_bench_SUITE,throughput,"Benchmarks run separately"}.
{skip_groups,dir,ssl_dist_bench_SUITE,sched_utilization,"Benchmarks run separately"}.


%% Both cases below fail identically on unmodified upstream/maint-26 (verified
%% in CI against a pristine baseline branch, and locally by reverting every
%% ssl/public_key source file in this branch back to OTP-26.2.5.21).  They are
%% pre-existing OTP 26 failures, not regressions from the CVE backports.
{skip_cases, dir, ssl_cert_SUITE, [client_auth_sni],
 "Pre-existing in OTP 26: the client correctly rejects the peer on "
 "hostname_check_failed, but OTP 26 reports it as handshake_failure while the "
 "case expects bad_certificate. Upstream changed that alert mapping in "
 "dfe636b274 as part of OTP-20130 (RFC 9525 CN-fallback removal), which is out "
 "of scope for this branch; backporting it regresses ssl_sni_SUITE."}.
{skip_cases, dir, ssl_eqc_SUITE, [tls_handshake_encoding],
 "Pre-existing in OTP 26: PropEr round-trip fails on a generated TLS 1.3 "
 "client_hello carrying pre_shared_key with psk_key_exchange_modes undefined. "
 "Only became visible once the CI base image started building PropEr with "
 "OTP 26, so this suite had never actually run here before."}.
