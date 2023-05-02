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


# as_draws() --------------------------------------------------------------

draws_types <-  c("matrix", "array", "list", "df", "rvars")
for (dest_type in draws_types) {
  touchstone::benchmark_run(
    expr_before_benchmark = {
      iterations <- 500
      chains <- 4
      I <- 30
      J <- 30
      x_array <- posterior::as_draws_array(array(1:(iterations*chains*I*J), dim = c(iterations, chains, I*J), dimnames = list(
        NULL, NULL, paste0("x[", rep(as.roman(1:I), each = J), ",", rep(as.roman(1:J), I), "]")
      )))
      xs <- lapply(setdiff(!!draws_types, !!dest_type), function(src_type) {
        as_draws_src <- get(paste0("as_draws_", src_type), envir = getNamespace("posterior"))
        as_draws_src(x_array)
      })
      as_draws_dest <- get(paste0("as_draws_", !!dest_type), envir = getNamespace("posterior"))
    },
    "as_draws_{dest_type}" := {
      for (x in xs) {
        as_draws_dest(x)
      }
    },
    n = 10
  )
}


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
    n = 10
  )
}


# touchstone analysis -----------------------------------------------------

# create artifacts used downstream in the GitHub Action
touchstone::benchmark_analyze()
