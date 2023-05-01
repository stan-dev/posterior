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
