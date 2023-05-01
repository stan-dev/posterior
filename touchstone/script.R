# see `help(run_script, package = 'touchstone')` on how to run this
# interactively

# OPTIONAL: Add directories you want to be available in this file or during the
# benchmarks.
# touchstone::pin_assets("bench")

# installs branches to benchmark
touchstone::branch_install()

# benchmarks for this package are stored in bench/bench-*.R
# add calls to touchstone::run_benchmark() there to create benchmarks
# bench_files <- dir(touchstone::path_pinned_asset("bench"), "bench-.*\\.R", full.names = TRUE)
# for (file in bench_files) {
#   source(file)
# }

repair_draws_n_variables = function(n_variables) {
  touchstone::benchmark_run(
    expr_before_benchmark = {
      set.seed(1234)
      x_sorted <- posterior::as_draws_matrix(posterior::rvar_rng(rnorm, 10))
    },
    "repair_draws_{n_variables}_variables" := {
      posterior::repair_draws(x_sorted)
    },
    n = 30
  )
}

repair_draws_n_variables(10)
repair_draws_n_variables(1000)


# create artifacts used downstream in the GitHub Action
touchstone::benchmark_analyze()
