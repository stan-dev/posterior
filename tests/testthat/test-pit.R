test_that("validate_y handles numeric inputs correctly", {
  expect_equal(
    validate_y(c(1, 2, 3), as_draws_matrix(matrix(1, ncol = 3))),
    c(1, 2, 3)
  )
})

test_that("validate_y raises an error for non-numeric 'y'", {
  expect_error(validate_y(c("a", "b", "c")), "`y` must be numeric.")
})

test_that("validate_y raises an error for NAs in 'y'", {
  expect_error(validate_y(c(1, NA, 3)), "NAs not allowed in `y`.")
})

test_that("validate_y checks 'y' length against nvariables(x)", {
  x <- example_draws() |> as_draws_matrix()
  expect_error(
    validate_y(1:3, x),
    "`y` must be a vector of length `nvariables(x)`.",
    fixed = TRUE
  )
})

test_that("validate_y verifies dimensions for array inputs", {
  x <- posterior::example_draws() |> rvar()
  y <- posterior::example_draws() |> rvar() |> mean()
  expect_equal(validate_y(y, x), y)

  y_mismatched <- array(1:8, dim = c(2, 2, 2))
  expect_error(
    validate_y(y_mismatched, x),
    "`dim(y)` must match `dim(x)`.",
    fixed = TRUE
  )
})

# test normalize_log_weights
test_that("normalize_log_weights returns log-normalized columns", {
  set.seed(1)
  x <- matrix(log(runif(200)), ncol = 10)
  result <- colSums(exp(normalize_log_weights(x)))
  expect_equal(result, rep(1, 10))
})

# tests for pit.default
test_that("pit works without weights", {
  x <- matrix(c(1, 2, 3, 5), nrow = 2)
  y <- c(3, 4)
  result <- pit(x, y)

  expect_length(result, ncol(x))
  expect_equal(unname(result), c(1, 0.5))
})

test_that("pit.default works with log-weights", {
  x <- matrix(c(1, 2, 3, 5), nrow = 2)
  y <- c(3, 4)
  weights <- matrix(log(c(1, 1, 1, 1)), nrow = 2)
  result <- pit.default(x, y, weights = weights, log = TRUE)

  expect_length(result, ncol(x))
  expect_equal(unname(result), c(1, 0.5))
})

test_that("pit.default works with non-log weights", {
  x <- matrix(c(1, 2, 3, 5), nrow = 2)
  y <- c(3, 4)
  weights <- matrix(c(1, 1, 1, 1), nrow = 2)
  result <- pit.default(x, y, weights = weights)

  expect_length(result, ncol(x))
  expect_equal(unname(result), c(1, 0.5))
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
  weights <- matrix(.5, nrow = 2, ncol = 2000)
  result <- pit.default(x, y, weights)

  expect_length(result, ncol(x))
  expect_true(all(result[1:1000] >= .5 & result[1:1000] <= 1))
  expect_true(all(result[1001:2000] >= 0 & result[1001:2000] <= 1))
})

test_that("pit.default handles randomized PIT with log-weights", {
  x <- matrix(c(rep(c(0, 1), 1000), rep(c(3, 3), 1000)), nrow = 2)
  y <- rep(c(1, 3), each = 1000)
  weights <- matrix(log(.5), nrow = 2, ncol = 2000)
  result <- pit.default(x, y, weights, log = TRUE)

  expect_length(result, ncol(x))
  expect_true(all(result[1:1000] >= .5 & result[1:1000] <= 1))
  expect_true(all(result[1001:2000] >= 0 & result[1001:2000] <= 1))
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

  expect_equal(unname(pit(x, y)), pit_true)
  expect_equal(unname(pit(as_draws_df(test_array), y)), pit_true)
  expect_equal(unname(pit(as_draws_matrix(test_array), y)), pit_true)
  expect_equal(unname(pit(as_draws_list(test_array), y)), pit_true)
  expect_equal(unname(pit(as_draws_rvars(test_array), y)), pit_true)
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

  weights <- matrix(
    runif(n_draws * n_col * n_row),
    ncol = n_row * n_col
  )

  y <- array(rnorm(n_col * n_row), dim = c(n_row, n_col))

  result <- pit(rvar(test_array), y)
  expect_equal(dim(result), dim(y))
  expect_true(all(
    result ==
      array(
        pit(array(test_array, dim = c(n_draws, n_col * n_row)), c(y)),
        dim(y)
      )
  ))

  result <- pit(rvar(test_array), y, weights)
  expect_equal(dim(result), dim(y))
  expect_true(all(result == array(
    pit(
      array(test_array, dim = c(n_draws, n_col * n_row)),
      c(y),
      array(weights, dim = c(n_draws, n_col * n_row))
    ),
    dim(y)
  )))
})

test_that("pit doesn't error for empty draws", {
  expect_numeric(pit(empty_draws_array(), numeric(0)))
  expect_numeric(pit(rvar(), numeric(0)))
})
