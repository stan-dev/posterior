test_that("resample_draws returns expected format", {
  x <- example_draws()
  w <- runif(ndraws(x), 0, 10)

  x <- as_draws_matrix(x)
  x_rs <- resample_draws(x, weights = w, method = "stratified")
  expect_true(is_draws_matrix(x_rs))
  expect_equal(ndraws(x_rs), ndraws(x))

  x <- as_draws_array(x)
  x_rs <- resample_draws(x, weights = w, method = "deterministic", ndraws = 200)
  expect_true(is_draws_array(x_rs))
  expect_equal(ndraws(x_rs), 200)

  x <- as_draws_df(x)
  x_rs <- resample_draws(x, weights = w, method = "simple")
  expect_true(is_draws_df(x_rs))
  expect_equal(ndraws(x_rs), ndraws(x))

  x <- as_draws_list(x)
  x_rs <- resample_draws(x, w, method = "simple_no_replace", ndraws = 100)
  expect_true(is_draws_list(x_rs))
  expect_equal(ndraws(x_rs), 100)
  expect_error(
    resample_draws(x, w, method = "simple_no_replace"),
    "Argument 'ndraws' is required"
  )

  x <- as_draws_rvars(x)
  x_rs <- resample_draws(x, weights = w, method = "simple")
  expect_true(is_draws_rvars(x_rs))
  expect_equal(ndraws(x_rs), ndraws(x))
})

test_that("Resampling algorithms return the correct result in expectation", {
  set.seed(1234)
  x <- as_draws_df(cbind(mu = 1:10000))
  w <- 1:10000 / 777
  expected_mean <- sum(x$mu * (w / sum(w)))

  x_rs <- resample_draws(x, w, method = "stratified")
  mean_rs <- mean(x_rs$mu)
  expect_true(mean_rs > 6660 && mean_rs < 6670)

  x_rs <- resample_draws(x, w, method = "deterministic")
  mean_rs <- mean(x_rs$mu)
  expect_true(mean_rs > 6660 && mean_rs < 6670)

  x_rs <- resample_draws(x, w, method = "simple")
  mean_rs <- mean(x_rs$mu)
  expect_true(mean_rs > 6650 && mean_rs < 6690)

  # method 'simple_no_replace' will be biased for weights with large variance
})

test_that("resample_draws uses stored weights when available", {
  x <- example_draws()
  expect_error(resample_draws(x),
    "No weights are provided and none can be found within the draws object"
  )
  w <- runif(ndraws(x), 0, 10)
  x <- weight_draws(x, w)
  x_rs <- resample_draws(x)
  expect_true(is_draws_array(x_rs))
  expect_equal(ndraws(x_rs), ndraws(x))
  # .log_weight variable has been dropped
  expect_true(!".log_weight" %in% variables(x_rs, reserved = TRUE))
})
