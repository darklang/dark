Every workload in this directory reads its iteration count from `PERF_ITERS`, defaulting to 0.

That is what lets `perf-suite` separate the body from everything around it: it runs each workload
once at 0 iterations and once at N, and the difference is the work. Startup, parsing the script
and loading packages all appear in both runs and cancel. Without that the numbers are dominated by
fixed cost -- for the original workload, startup was 7.6 MB of a 13.4 MB total.
