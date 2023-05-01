# touchstone setup --------------------------------------------------------

# see `help(run_script, package = 'touchstone')` on how to run this
# interactively. In short:
# 1. Run `touchstone::activate(base_branch = "master")` to begin a session.
# 2. Execute touchstone::benchmark_run commands interactively below.
# 3. Run `touchstone::deactivate()` to reset session settings.

# OPTIONAL: Add directories you want to be available in this file or during the
# benchmarks.
# touchstone::pin_assets("path/to/file/or/dir")

# installs branches to benchmark
touchstone::branch_install()


# summarise_draws() -------------------------------------------------------

for (n_variables in c(10, 100)) {
  touchstone::benchmark_run(
    expr_before_benchmark = {
      set.seed(1234)
      x <- posterior::as_draws_matrix(posterior::rvar_rng(rnorm, !!n_variables))
    },
    "summarise_draws_{n_variables}_variables" := {
      posterior::summarise_draws(x)
    },
    n = 30
  )
}


# touchstone analysis -----------------------------------------------------

# create artifacts used downstream in the GitHub Action
touchstone::benchmark_analyze()
