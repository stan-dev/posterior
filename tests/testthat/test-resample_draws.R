test_that("resample_draws returns expected format", {
  x <- as_draws_df(example_draws())
  w <- runif(ndraws(x), 0, 10)

  x_rs <- resample_draws(x, weights = w, method = "simple")
  expect_class(x_rs, "draws_df")
  expect_equal(ndraws(x_rs), ndraws(x))

  x_rs <- resample_draws(x, w, method = "simple_no_replace", ndraws = 100)
  expect_class(x_rs, "draws_df")
  expect_equal(ndraws(x_rs), 100)
  expect_error(
    resample_draws(x, w, method = "simple_no_replace"),
    "Argument 'ndraws' is required"
  )

  x <- as_draws_matrix(x)
  x_rs <- resample_draws(x, weights = w, method = "stratified")
  expect_class(x_rs, "draws_matrix")
  expect_equal(ndraws(x_rs), ndraws(x))

  x_rs <- resample_draws(x, weights = w, method = "deterministic", ndraws = 200)
  expect_class(x_rs, "draws_matrix")
  expect_equal(ndraws(x_rs), 200)

  expect_error(
    resample_draws(as_draws_array(x), w),
    "'resample_draws' requires formats which can be subsetted"
  )
})
