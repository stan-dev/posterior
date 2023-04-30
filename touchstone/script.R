# see `help(run_script, package = 'touchstone')` on how to run this
# interactively

# OPTIONAL: Add directories you want to be available in this file or during the
# benchmarks.
# touchstone::pin_assets("some/dir")

# installs branches to benchmark
touchstone::branch_install()

# benchmark a function call from your package (two calls per branch)
touchstone::benchmark_run(
  # expr_before_benchmark = source("dir/data.R"), #<-- OPTIONAL setup before benchmark
  random_test = rnorm(1000), #<- TODO put the call you want to benchmark here
  n = 2
)

# create artifacts used downstream in the GitHub Action
touchstone::benchmark_analyze()
