test_that("resample_draws returns expected format", {
  x <- example_draws()
  w <- runif(ndraws(x), 0, 10)

  x <- as_draws_matrix(x)
  x_rs <- resample_draws(x, weights = w, method = "stratified")
  expect_class(x_rs, "draws_matrix")
  expect_equal(ndraws(x_rs), ndraws(x))

  x <- as_draws_array(x)
  x_rs <- resample_draws(x, weights = w, method = "deterministic", ndraws = 200)
  expect_class(x_rs, "draws_array")
  expect_equal(ndraws(x_rs), 200)

  x <- as_draws_df(x)
  x_rs <- resample_draws(x, weights = w, method = "simple")
  expect_class(x_rs, "draws_df")
  expect_equal(ndraws(x_rs), ndraws(x))

  x <- as_draws_list(x)
  x_rs <- resample_draws(x, w, method = "simple_no_replace", ndraws = 100)
  expect_class(x_rs, "draws_list")
  expect_equal(ndraws(x_rs), 100)
  expect_error(
    resample_draws(x, w, method = "simple_no_replace"),
    "Argument 'ndraws' is required"
  )
})

test_that("Resampling algorithms return the correct result in expectation", {
  set.seed(1234)
  x <- as_draws_df(cbind(mu = 1:10000))
  w <- 1:10000 / 777
  expected_mean <- sum(x$mu * (w / sum(w)))

  x_rs <- resample_draws(x, w, method = "stratified")
  expect_true(mean(x_rs$mu) > 6660 && x_rs < 6670)

  x_rs <- resample_draws(x, w, method = "deterministic")
  expect_true(mean(x_rs$mu) > 6660 && x_rs < 6670)

  x_rs <- resample_draws(x, w, method = "simple")
  expect_true(mean(x_rs$mu) > 6650 && x_rs < 6690)

  # method 'simple_no_replace' will be biased for weights with large variance
})
