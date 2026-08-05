{suites, "../diameter_test", all}.
%% SCTP is actually available on the GitHub Actions runners (gen_sctp:open/0
%% succeeds, so the suite's own no_sctp self-skip does not trigger), but the
%% cases are timing benchmarks: they assert sub-2s delivery and <100ms
%% turnaround variance, which do not hold on shared/virtualized CI runners.
%% Skip the suite like the other benchmark suites rather than let those
%% timing assertions flake.
{skip_suites, "../diameter_test", [diameter_gen_sctp_SUITE], "Timing benchmark, unreliable on shared CI runners"}.
