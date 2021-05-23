test_that("weight_draws works on draws_matrix", {
  x <- as_draws_matrix(example_draws())
  weights <- rexp(ndraws(x))

  x1 <- weight_draws(x, weights)
  weights1 <- weights(x1, normalize = FALSE)
  expect_equal(weights1, weights)

  x2 <- weight_draws(x, log(weights), log = TRUE)
  weights2 <- weights(x2)
  expect_equal(weights2, weights / sum(weights))
})

test_that("weight_draws works on draws_array", {
  x <- as_draws_array(example_draws())
  weights <- rexp(ndraws(x))

  x1 <- weight_draws(x, weights)
  weights1 <- weights(x1)
  expect_equal(weights1, weights / sum(weights))

  x2 <- weight_draws(x, log(weights), log = TRUE)
  weights2 <- weights(x2, normalize = FALSE)
  expect_equal(weights2, weights)
})

test_that("weight_draws works on draws_df", {
  x <- as_draws_df(example_draws())
  weights <- rexp(ndraws(x))

  x1 <- weight_draws(x, weights)
  weights1 <- weights(x1, normalize = FALSE)
  expect_equal(weights1, weights)

  x2 <- weight_draws(x, log(weights), log = TRUE)
  weights2 <- weights(x2)
  expect_equal(weights2, weights / sum(weights))
})

test_that("weight_draws works on draws_list", {
  x <- as_draws_list(example_draws())
  weights <- rexp(ndraws(x))

  x1 <- weight_draws(x, weights)
  weights1 <- weights(x1)
  expect_equal(weights1, weights / sum(weights))

  x2 <- weight_draws(x, log(weights), log = TRUE)
  weights2 <- weights(x2, normalize = FALSE)
  expect_equal(weights2, weights)
})

test_that("weight_draws works on draws_rvars", {
  x <- as_draws_rvars(example_draws())
  weights <- rexp(ndraws(x))

  x1 <- weight_draws(x, weights)
  weights1 <- weights(x1)
  expect_equal(weights1, weights / sum(weights))

  x2 <- weight_draws(x, log(weights), log = TRUE)
  weights2 <- weights(x2, normalize = FALSE)
  expect_equal(weights2, weights)
})
