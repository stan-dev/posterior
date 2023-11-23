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

# conversion preserves weights --------------------------------------------

test_that("conversion between formats preserves weights", {
  draws <- list(
    matrix = weight_draws(draws_matrix(x = 1:10), 1:10),
    array = weight_draws(draws_array(x = 1:10), 1:10),
    df = weight_draws(draws_df(x = 1:10), 1:10),
    list = weight_draws(draws_list(x = 1:10), 1:10),
    rvars = weight_draws(draws_rvars(x = 1:10), 1:10)
  )

  # chain/iteration/draw columns are placed at the end by conversion functions,
  # so our reference format will keep that order
  reserved = names(draws$df) %in% reserved_df_variables()
  draws$df = draws$df[, c(names(draws$df)[!reserved], names(draws$df)[reserved])]

  for (type in names(draws)) {
    expect_equal(as_draws_matrix(draws[[!!type]]), draws$matrix)
    expect_equal(as_draws_array(draws[[!!type]]), draws$array)
    expect_equal(as_draws_df(draws[[!!type]]), draws$df)
    expect_equal(as_draws_list(draws[[!!type]]), draws$list)
    expect_equal(as_draws_rvars(draws[[!!type]]), draws$rvars)
  }
})

# pareto smoothing ----------------

test_that("pareto smoothing smooths weights in weight_draws", {
  x <- example_draws()
  lw <- sort(log(abs(rt(ndraws(x), 1))))
  weighted <- weight_draws(x, lw, pareto_smooth = FALSE, log = TRUE)
  smoothed <- weight_draws(x, lw, pareto_smooth = TRUE, log = TRUE)
  expect_false(all(weights(weighted) == weights(smoothed)))
})
