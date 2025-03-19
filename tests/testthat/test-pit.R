# Tests for validate_loo_weights
test_that("validate_loo_weights checks for correct dimensions", {
  loo_weights <- matrix(1, nrow = 2, ncol = 3)
  draws <- matrix(1, nrow = 2, ncol = 2)
  expect_error(
    validate_loo_weights(loo_weights, draws),
    "Dimension of `loo_weights` must match that of `x`."
  )
})

test_that("validate_loo_weights checks for finite values", {
  loo_weights <- matrix(c(1, Inf, 3), nrow = 1)
  draws <- matrix(1, nrow = 1, ncol = 3)
  expect_error(
    validate_loo_weights(loo_weights, draws),
    "All weigths in `loo_weights` must be finite."
  )
})

test_that(
  "validate_loo_weights checks for non-negative weights when log = FALSE",
  {
    loo_weights <- matrix(c(1, -1, 3), nrow = 1)
    draws <- matrix(1, nrow = 1, ncol = 3)
    expect_error(
      validate_loo_weights(loo_weights, draws, log = FALSE),
      "`loo-weights` must be non-negative when log = FALSE."
    )
  }
)

test_that("validate_loo_weights returns log-weights when log = FALSE", {
  loo_weights <- matrix(c(1, 2, 3), nrow = 1)
  draws <- matrix(1, nrow = 1, ncol = 3)
  result <- validate_loo_weights(loo_weights, draws, log = FALSE)
  expect_equal(result, log(loo_weights))
})

test_that("validate_loo_weights works for valid log-weights", {
  loo_weights <- matrix(c(1, 2, 3), nrow = 1)
  draws <- matrix(1, nrow = 1, ncol = 3)
  result <- validate_loo_weights(loo_weights, draws)
  expect_equal(result, loo_weights)
})

# test normalize_log_weights
test_that("normalize_log_weights returns log-normalized columns", {
  set.seed(1)
  x <- matrix(log(runif(200)), ncol = 10)
  result <- colSums(exp(normalize_log_weights(x)))
  expect_equal(result, rep(1, 10))
})

# tests for pit.default
test_that("pit.default works without loo_weights", {
  x <- matrix(c(1, 2, 3, 5), nrow = 2)
  y <- c(3, 4)
  result <- pit.default(x, y)

  expect_length(result, ncol(x))
  expect_equal(result, c(1, 0.5))
})

test_that("pit.default works with log-weights", {
  x <- matrix(c(1, 2, 3, 5), nrow = 2)
  y <- c(3, 4)
  loo_weights <- matrix(log(c(1, 1, 1, 1)), nrow = 2)
  result <- pit.default(x, y, loo_weights = loo_weights)

  expect_length(result, ncol(x))
  expect_equal(result, c(1, 0.5))
})

test_that("pit.default works with non-log weights", {
  x <- matrix(c(1, 2, 3, 5), nrow = 2)
  y <- c(3, 4)
  loo_weights <- matrix(c(1, 1, 1, 1), nrow = 2)
  result <- pit.default(x, y, loo_weights = loo_weights, log = FALSE)

  expect_length(result, ncol(x))
  expect_equal(result, c(1, 0.5))
})

test_that("pit.default handles randomized PIT with no weights", {
  x <- matrix(c(rep(c(0, 1), 1000), rep(c(3, 3), 1000)), nrow = 2)
  y <- rep(c(1, 3), each = 1000)
  result <- pit.default(x, y)

  expect_length(result, ncol(x))
  expect_true(all(result[1:1000] >= .5 & result[1:1000] <= 1))
  expect_true(all(result[1001:2000] >= 0 & result[1001:2000] <= 1))
})

test_that("pit.default handles randomized PIT with weights", {
  x <- matrix(c(rep(c(0, 1), 1000), rep(c(3, 3), 1000)), nrow = 2)
  y <- rep(c(1, 3), each = 1000)
  loo_weights <- matrix(.5, nrow = 2, ncol = 2000)
  result <- pit.default(x, y, loo_weights, log = FALSE)

  expect_length(result, ncol(x))
  expect_true(all(result[1:1000] >= .5 & result[1:1000] <= 1))
  expect_true(all(result[1001:2000] >= 0 & result[1001:2000] <= 1))
})

test_that("pit.default handles randomized PIT with log-weights", {
  x <- matrix(c(rep(c(0, 1), 1000), rep(c(3, 3), 1000)), nrow = 2)
  y <- rep(c(1, 3), each = 1000)
  loo_weights <- matrix(.5, nrow = 2, ncol = 2000)
  result <- pit.default(x, y, loo_weights, log = FALSE)

  expect_length(result, ncol(x))
  expect_true(all(result[1:1000] >= .5 & result[1:1000] <= 1))
  expect_true(all(result[1001:2000] >= 0 & result[1001:2000] <= 1))
})

test_that("pit.default warns for PIT values exceeding 1", {
  x <- matrix(c(1, 2, 3, 4), nrow = 2)
  y <- c(3, 5)
  loo_weights <- matrix(log(c(100, 200, 300, 4000000000)), nrow = 2)

  expect_warning(
    result <- pit.default(x, y, loo_weights = loo_weights),
    regex = paste(
      "Some PIT values larger than 1! ",
      "This is usually due to numerical inaccuracies\\. ",
      "Largest value: 1\\s*",
      "Rounding PIT > 1 to 1\\.",
      sep = ""
    ),
    fixed = FALSE
  )
  expect_true(all(result <= 1))
})

test_that("pit works with draws objects", {
  set.seed(1)
  n_chains <- 4
  n_vars <- 3
  n_draws <- 100

  test_array <- array(
    rnorm(n_draws * n_vars * n_chains),
    dim = c(n_draws, n_chains, n_vars)
  )

  y <- rnorm(n_vars)

  pit_true <- sapply(1:n_vars, \(v) mean(test_array[, , v] < y[v]))

  x <- as_draws(test_array)

  expect_equal(pit(x, y), pit_true)
  expect_equal(pit(as_draws_df(test_array), y), pit_true)
  expect_equal(pit(as_draws_matrix(test_array), y), pit_true)
  expect_equal(pit(as_draws_list(test_array), y), pit_true)
  expect_equal(pit(as_draws_rvars(test_array), y), pit_true)

})

test_that("pit works with rvars", {

  set.seed(1)
  n_col <- 4
  n_row <- 3
  n_draws <- 100

  test_array <- array(
    rnorm(n_draws * n_col * n_row),
    dim = c(n_draws, n_row, n_col)
  )

  loo_weights <- array(
    rnorm(n_draws * n_col * n_row),
    dim = c(n_draws, n_row, n_col)
  )

  y <- array(rnorm(n_col * n_row), dim = c(n_row, n_col))

  expect_equal(dim(pit(rvar(test_array), y)), dim(y))
  expect_true(all(
    pit(rvar(test_array), y) ==
      array(
        pit(array(test_array, dim = c(n_draws, n_col * n_row)), c(y)),
        dim(y)
      )
  ))
  expect_equal(dim(pit(rvar(test_array), y, rvar(loo_weights))), dim(y))
  expect_true(all(
    pit(rvar(test_array), y, rvar(loo_weights)) ==
      array(
        pit(array(test_array, dim = c(n_draws, n_col * n_row)),
            c(y),
            array(loo_weights, dim = c(n_draws, n_col * n_row))),
        dim(y)
      )
  ))
})

test_that("pit doesn't error for empty draws", {
  expect_numeric(pit(empty_draws_array(), c()))
  expect_numeric(pit(rvar(), c()))
})
