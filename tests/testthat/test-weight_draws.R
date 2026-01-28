test_that("weight_draws works on draws_matrix", {
  x <- as_draws_matrix(example_draws())
  weights <- rexp(ndraws(x))

  x1 <- weight_draws(x, weights)
  weights1 <- weights(x1, normalize = FALSE)
  expect_equal(weights1, weights)

  x2 <- weight_draws(x, log(weights), log = TRUE)
  weights2 <- weights(x2)
  expect_equal(weights2, weights / sum(weights))

  # test replacement of weights
  expect_equal(weight_draws(x1, weights2), weight_draws(x, weights2))
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

  # test replacement of weights
  expect_equal(weight_draws(x1, weights2), weight_draws(x, weights2))
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

  # test replacement of weights
  expect_equal(weight_draws(x1, weights2), weight_draws(x, weights2))
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

  # test replacement of weights
  expect_equal(weight_draws(x1, weights2), weight_draws(x, weights2))
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

  # test replacement of weights
  expect_equal(weight_draws(x1, weights2), weight_draws(x, weights2))
})

test_that("weights are propagated to variables in draws_rvars", {
  d <- draws_rvars(x = rvar(1:10, log_weights = 2:11), y = 3:12)
  expect_equal(log_weights(d$x), 2:11)
  expect_equal(log_weights(d$y), 2:11)

  d <- draws_rvars(x = 1:10, y = 3:12, .log_weight = 2:11)
  expect_equal(log_weights(d$x), 2:11)
  expect_equal(log_weights(d$y), 2:11)

  expect_error(
    draws_rvars(x = rvar(1:10, log_weights = 1:10), y = rvar(3:12, log_weights = 2:11)),
    "different log weights"
  )

  expect_error(
    draws_rvars(x = rvar(1:10, log_weights = 1:10), .log_weight = 2:11),
    "different log weights"
  )
})

# removing weights works --------------------------------------------------

test_that("weights can be removed", {
  x <- list(
    matrix = as_draws_matrix(example_draws()),
    array = as_draws_array(example_draws()),
    df = as_draws_df(example_draws()),
    list = as_draws_list(example_draws()),
    rvars = as_draws_rvars(example_draws()),
    rvar = as_draws_rvars(example_draws())$mu
  )

  weights <- rexp(ndraws(example_draws()))
  x_weighted <- lapply(x, weight_draws, weights)

  for (type in names(x)) {
    expect_equal(weight_draws(x_weighted[[!!type]], NULL), x[[!!type]])
  }
})

# conversion preserves weights --------------------------------------------

test_that("conversion between formats preserves weights", {
  draws <- list(
    matrix = weight_draws(draws_matrix(x = 1:10), 1:10),
    array = weight_draws(draws_array(x = 1:10), 1:10),
    df = weight_draws(draws_df(x = 1:10), 1:10),
    list = weight_draws(draws_list(x = 1:10), 1:10),
    rvars = weight_draws(draws_rvars(x = 1:10), 1:10),
    rvar = weight_draws(rvar(x = 1:10), 1:10)
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

# assertions on weights vector ------------------------------------------------

test_that("weights must match draws", {
  x <- example_draws()
  types <- list(as_draws_matrix, as_draws_array, as_draws_df, as_draws_list, as_draws_rvars)
  for (type in types) {
    expect_error(weight_draws((!!type)(x), 1), "weights must match .* draws")
  }
})


## pit now allows for weights to be a draws matrix, so this no longer passes
## test_that("weights must be a vector, not array/matrix", {
##   x <- example_draws()
##   w <- seq_len(ndraws(x))
##   expect_error(weight_draws(x, matrix(w)), "Must be.*vector.*not.*matrix")
## })
