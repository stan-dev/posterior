test_that("summarise_draws works correctly", {
  x <- as_draws_df(example_draws())
  sum_x <- summarise_draws(x)
  expect_true(all(default_convergence_measures() %in% names(sum_x)))
  expect_true(all(c("q5", "q95") %in% names(sum_x)))
  expect_equal(sum_x$variable, variables(x))
  expect_equal(mean(x$mu), sum_x$mean[sum_x$variable == "mu"])

  sum_x <- summarise_draws(x, mean, median)
  expect_true(all(c("mean", "median") %in% names(sum_x)))

  sum_x <- summarise_draws(x, default_mcse_measures())
  expect_true(all(c("mcse_q5", "mcse_q95") %in% names(sum_x)))

  sum_x <- summarise_draws(x, ~quantile(.x, probs = c(0.4, 0.6)))
  expect_true(all(c("40%", "60%") %in% names(sum_x)))
})

test_that("aliases of summarise_draws work", {
  x <- as_draws_array(example_draws())
  sum_x <- summarise_draws(x)
  sum_x2 <- summarize_draws(x)
  expect_equal(sum_x, sum_x2)
  sum_x3 <- summary(x)
  expect_equal(sum_x, sum_x3)
})

test_that("summarise_draws errors if name 'variable' is used", {
  x <- example_draws()
  variable <- function(x) mean(x)
  expect_error(
    summarise_draws(x, "variable"),
    "Name 'variable' is reserved in 'summarise_draws'"
  )
})
