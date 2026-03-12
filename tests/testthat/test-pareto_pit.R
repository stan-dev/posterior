# ---- basic behavior ----

test_that("pareto_pit returns named vector of correct length", {
  set.seed(1)
  x <- as_draws_matrix(matrix(rnorm(2000), ncol = 4))
  y <- c(-1, 0, 1, 2)
  result <- pareto_pit(x, y)

  expect_length(result, 4)
  expect_named(result, variables(x))
  expect_true(all(result >= 0 & result <= 1))
})

test_that("pareto_pit bulk values match pit", {
  # For observations well inside the bulk, pareto_pit and pit should agree
  set.seed(42)
  ndraws <- 1000
  x <- as_draws_matrix(matrix(rnorm(ndraws * 3), ncol = 3))
  # observations near the median (well within bulk)
  y <- c(0, 0.1, -0.1)

  raw <- pit(x, y)
  refined <- pareto_pit(x, y)

  expect_equal(unname(refined), unname(raw))
})

test_that("pareto_pit differs from pit in tails", {
  set.seed(42)
  ndraws <- 1000
  x <- as_draws_matrix(matrix(rnorm(ndraws * 2), ncol = 2))
  # extreme observations in the tails
  y <- c(-4, 4)

  raw <- pit(x, y)
  refined <- pareto_pit(x, y)

  # refined values should differ from raw ECDF in the tails
  expect_false(isTRUE(all.equal(unname(refined), unname(raw))))
  expect_true(all(refined >= 0 & refined <= 1))
})

# ---- known-distribution round-trip ----

test_that("pareto_pit gives values closer to true CDF than raw pit for right tail", {
  set.seed(123)
  ndraws <- 500
  nvars <- 200

  # t-distribution with 3 df (heavy tails where GPD fits well)
  x <- as_draws_matrix(matrix(rt(ndraws * nvars, df = 3), ncol = nvars))

  # Observations far in the right tail
  y_right <- rep(qt(0.999, df = 3), nvars)
  true_pit <- rep(0.999, nvars)

  raw <- pit(x, y_right)
  refined <- pareto_pit(x, y_right)

  # On average, pareto_pit should be closer to the true CDF value
  raw_mse <- mean((raw - true_pit)^2)
  refined_mse <- mean((refined - true_pit)^2)

  expect_lt(refined_mse, raw_mse)
})

test_that("pareto_pit gives values closer to true CDF in left tail", {
  set.seed(123)
  ndraws <- 500
  nvars <- 200

  # t-distribution with 3 df
  x <- as_draws_matrix(matrix(rt(ndraws * nvars, df = 3), ncol = nvars))

  # Observations far in the left tail
  y_left <- rep(qt(0.001, df = 3), nvars)
  true_pit <- rep(0.001, nvars)

  raw <- pit(x, y_left)
  refined <- pareto_pit(x, y_left)

  raw_mse <- mean((raw - true_pit)^2)
  refined_mse <- mean((refined - true_pit)^2)

  expect_lt(refined_mse, raw_mse)
})

test_that("pareto_pit provides more varied estimates than ECDF in extreme tails", {
  # When observations are beyond most draws, ECDF gives 0 or 1 for many,
  # while GPD can extrapolate and give more varied (and accurate) estimates
  set.seed(123)
  ndraws <- 500
  nvars <- 200

  x <- as_draws_matrix(matrix(rt(ndraws * nvars, df = 3), ncol = nvars))
  y_right <- rep(qt(0.9995, df = 3), nvars)

  raw <- pit(x, y_right)
  refined <- pareto_pit(x, y_right)

  # Raw ECDF should have many values exactly at 1
  # Refined should have more varied values
  expect_gt(length(unique(refined)), length(unique(raw)))
})

# ---- discrete randomization ----

test_that("pareto_pit uses randomization for discrete observations in bulk", {
  set.seed(1)
  x <- as_draws_matrix(matrix(rep(c(0, 1, 2, 3), each = 250), ncol = 1))
  y <- 2  # matches some draws exactly

  results <- replicate(100, {
    pareto_pit(x, y)
  })

  # With randomization, results should vary
  expect_gt(sd(results), 0)
  # All results should be in valid range
  expect_true(all(results >= 0 & results <= 1))
})

# ---- ndraws_tail argument ----

test_that("pareto_pit respects ndraws_tail argument", {
  set.seed(42)
  ndraws <- 1000
  x <- as_draws_matrix(matrix(rnorm(ndraws), ncol = 1))
  y <- 3  # right tail

  result_small <- pareto_pit(x, y, ndraws_tail = 10)
  result_large <- pareto_pit(x, y, ndraws_tail = 100)

  # Different ndraws_tail should give different results
  expect_false(isTRUE(all.equal(unname(result_small), unname(result_large))))
})

# ---- edge cases ----

test_that("pareto_pit handles constant draws gracefully", {
  x <- as_draws_matrix(matrix(rep(5, 200), ncol = 1))
  y <- 5

  # Should not error; falls back to raw PIT
  result <- pareto_pit(x, y)
  expect_length(result, 1)
  expect_true(result >= 0 & result <= 1)
})

test_that("pareto_pit handles NA/Inf draws gracefully", {
  x <- as_draws_matrix(matrix(c(1:99, NA), ncol = 1))
  y <- 50

  # should_return_NA triggers, falls back to raw PIT
  expect_warning(result <- pareto_pit(x, y))
  expect_length(result, 1)
})

test_that("pareto_pit works when all observations are in bulk", {
  set.seed(42)
  ndraws <- 1000
  x <- as_draws_matrix(matrix(rnorm(ndraws * 3), ncol = 3))
  y <- c(0, 0, 0)

  result <- pareto_pit(x, y)
  raw <- pit(x, y)

  expect_equal(unname(result), unname(raw))
})

# ---- S3 dispatch ----

test_that("pareto_pit.default coerces to draws_matrix", {
  set.seed(42)
  x_mat <- matrix(rnorm(2000), ncol = 2)
  y <- c(-3, 3)

  result <- pareto_pit(x_mat, y)
  expect_length(result, 2)
  expect_true(all(result >= 0 & result <= 1))
})

test_that("pareto_pit works with different draws formats", {
  set.seed(42)
  n_chains <- 4
  n_vars <- 3
  n_draws <- 100

  test_array <- array(
    rnorm(n_draws * n_vars * n_chains),
    dim = c(n_draws, n_chains, n_vars)
  )
  y <- c(0, 0, 0)

  result_matrix <- pareto_pit(as_draws_matrix(test_array), y)
  result_df <- pareto_pit(as_draws_df(test_array), y)
  result_list <- pareto_pit(as_draws_list(test_array), y)

  expect_equal(unname(result_matrix), unname(result_df))
  expect_equal(unname(result_matrix), unname(result_list))
})

# ---- rvar method ----

test_that("pareto_pit.rvar returns correct shape", {
  set.seed(42)
  x_rvar <- rvar(matrix(rnorm(1000 * 6), nrow = 1000, ncol = 6))
  dim(x_rvar) <- c(2, 3)
  y_arr <- array(c(0, 0, 0, 0, 0, 0), dim = c(2, 3))

  result <- pareto_pit(x_rvar, y_arr)

  expect_equal(dim(result), c(2, 3))
  expect_true(all(result >= 0 & result <= 1))
})

test_that("pareto_pit.rvar matches draws_matrix result", {
  set.seed(42)
  x_rvar <- rvar(matrix(rnorm(1000 * 4), nrow = 1000, ncol = 4))
  y_vec <- c(-3, 0, 1, 3)

  result_rvar <- c(pareto_pit(x_rvar, y_vec))
  result_matrix <- unname(pareto_pit(as_draws_matrix(c(x_rvar)), y_vec))

  expect_equal(result_rvar, result_matrix)
})

# ---- values are in valid range ----

test_that("pareto_pit values are always in [0, 1]", {
  set.seed(42)
  ndraws <- 500
  x <- as_draws_matrix(matrix(rnorm(ndraws * 10), ncol = 10))
  y <- qnorm(c(0.001, 0.01, 0.05, 0.1, 0.3, 0.7, 0.9, 0.95, 0.99, 0.999))

  result <- pareto_pit(x, y)
  expect_true(all(result >= 0 & result <= 1))
})

test_that("pareto_pit handles observations beyond all draws", {
  set.seed(42)
  ndraws <- 500
  x <- as_draws_matrix(matrix(rnorm(ndraws), ncol = 1))
  y_extreme_right <- 100
  y_extreme_left <- -100

  result_right <- pareto_pit(x, y_extreme_right)
  result_left <- pareto_pit(x, y_extreme_left)

  expect_true(result_right > 0.99)
  expect_true(result_left < 0.01)
  expect_true(result_right >= 0 & result_right <= 1)
  expect_true(result_left >= 0 & result_left <= 1)
})

# ---- monotonicity in tails ----

test_that("pareto_pit is monotonic in the right tail", {
  set.seed(42)
  ndraws <- 1000
  x <- as_draws_matrix(matrix(rnorm(ndraws), ncol = 1))
  y_seq <- seq(2, 4, by = 0.2)

  results <- sapply(y_seq, function(yi) pareto_pit(x, yi))
  # PIT values should be non-decreasing
  expect_true(all(diff(results) >= 0))
})

test_that("pareto_pit is monotonic in the left tail", {
  set.seed(42)
  ndraws <- 1000
  x <- as_draws_matrix(matrix(rnorm(ndraws), ncol = 1))
  y_seq <- seq(-4, -2, by = 0.2)

  results <- sapply(y_seq, function(yi) pareto_pit(x, yi))
  # PIT values should be non-decreasing
  expect_true(all(diff(results) >= 0))
})

# ---- weighted pareto_pit ----

test_that("pareto_pit with uniform weights matches unweighted", {
  set.seed(42)
  ndraws <- 500
  nvars <- 4
  x <- as_draws_matrix(matrix(rnorm(ndraws * nvars), ncol = nvars))
  y <- c(-3, 0, 1, 3)
  w_uniform <- matrix(1, nrow = ndraws, ncol = nvars)

  result_none <- pareto_pit(x, y)
  result_unif <- pareto_pit(x, y, weights = w_uniform)

  expect_equal(result_none, result_unif)
})

test_that("pareto_pit with log weights matches standard weights", {
  set.seed(42)
  ndraws <- 500
  nvars <- 4
  x <- as_draws_matrix(matrix(rnorm(ndraws * nvars), ncol = nvars))
  y <- c(-3, 0, 1, 3)
  w <- matrix(runif(ndraws * nvars, 0.5, 2), nrow = ndraws, ncol = nvars)

  result_std <- pareto_pit(x, y, weights = w)
  result_log <- pareto_pit(x, y, weights = log(w), log = TRUE)

  expect_equal(result_std, result_log)
})

test_that("pareto_pit with non-uniform weights differs from unweighted", {
  set.seed(42)
  ndraws <- 500
  nvars <- 2
  x <- as_draws_matrix(matrix(rnorm(ndraws * nvars), ncol = nvars))
  y <- c(-3, 3)  # tail observations to ensure GPD refinement kicks in
  # strongly non-uniform weights: heavily upweight positive draws
  w <- matrix(
    ifelse(x > 0, 10, 1),
    nrow = ndraws, ncol = nvars
  )

  result_none <- pareto_pit(x, y)
  result_wt <- pareto_pit(x, y, weights = w)

  expect_false(isTRUE(all.equal(unname(result_none), unname(result_wt))))
  expect_true(all(result_wt >= 0 & result_wt <= 1))
})

test_that("pareto_pit weighted values are in [0, 1]", {
  set.seed(42)
  ndraws <- 500
  nvars <- 10
  x <- as_draws_matrix(matrix(rt(ndraws * nvars, df = 3), ncol = nvars))
  y <- qt(c(0.001, 0.01, 0.05, 0.1, 0.3, 0.7, 0.9, 0.95, 0.99, 0.999), df = 3)
  w <- matrix(runif(ndraws * nvars, 0.1, 5), nrow = ndraws, ncol = nvars)

  result <- pareto_pit(x, y, weights = w)
  expect_true(all(result >= 0 & result <= 1))
})

test_that("pareto_pit weighted bulk matches pit weighted bulk", {
  set.seed(42)
  ndraws <- 500
  nvars <- 3
  x <- as_draws_matrix(matrix(rnorm(ndraws * nvars), ncol = nvars))
  y <- c(0, 0.1, -0.1)  # well in the bulk
  w <- matrix(runif(ndraws * nvars, 0.5, 2), nrow = ndraws, ncol = nvars)

  raw <- pit(x, y, weights = w)
  refined <- pareto_pit(x, y, weights = w)

  expect_equal(unname(refined), unname(raw))
})

test_that("pareto_pit.rvar passes weights through", {
  set.seed(42)
  ndraws <- 500
  nvars <- 4
  x_rvar <- rvar(matrix(rnorm(ndraws * nvars), nrow = ndraws, ncol = nvars))
  y_vec <- c(-3, 0, 1, 3)
  w <- matrix(runif(ndraws * nvars, 0.5, 2), nrow = ndraws, ncol = nvars)

  result_rvar <- c(pareto_pit(x_rvar, y_vec, weights = w))
  result_matrix <- unname(pareto_pit(as_draws_matrix(c(x_rvar)), y_vec, weights = w))

  expect_equal(result_rvar, result_matrix)
})

test_that("pareto_pit.default passes weights through", {
  set.seed(42)
  ndraws <- 500
  nvars <- 2
  x_mat <- matrix(rnorm(ndraws * nvars), ncol = nvars)
  y <- c(-3, 3)
  w <- matrix(runif(ndraws * nvars, 0.5, 2), nrow = ndraws, ncol = nvars)

  result <- pareto_pit(x_mat, y, weights = w)
  expect_length(result, 2)
  expect_true(all(result >= 0 & result <= 1))
})
