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
