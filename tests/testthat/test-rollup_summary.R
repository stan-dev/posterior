test_that("rollup_summary works correctly", {
  set.seed(1234)
  x_array <- as_draws_array(example_draws(example = "multi_normal"))
  x_array <- mutate_variables(x_array, y = rnorm(ndraws(x_array)))
  x <- as_draws_df(x_array)

  sum_x <- summarise_draws(x)
  rollup <- rollup_summary(sum_x)
  expect_equal(rollup, rollup_summary(sum_x))
  expect_equal(rollup, rollup_summary(x_array))

  sum_x <- summarise_draws(x, "mean", "sd")

  rollup <- rollup_summary(sum_x)
  expect_equal(rollup$unrolled, sum_x[sum_x$variable == "y", ])
  expect_equal(rollup$rolled$variable, c("mu", "Sigma"))
  expect_equal(rollup$rolled$dim, c("3", "3,3"))
  expect_equal(names(rollup$rolled), c("variable", "dim", "mean_min", "mean_max", "sd_min", "sd_max"))
  expect_equal(rollup$rolled$mean_max[1], max(sum_x[startsWith(sum_x$variable, "mu"),"mean"]))

  rollup <- rollup_summary(sum_x, variable = "Sigma")
  expect_equal(rollup$unrolled, sum_x[!startsWith(sum_x$variable, "Sigma"), ])
  expect_equal(rollup$rolled$variable, c("Sigma"))
  expect_equal(rollup$rolled$dim, c("3,3"))
  expect_equal(names(rollup$rolled), c("variable", "dim", "mean_min", "mean_max", "sd_min", "sd_max"))
  expect_equal(rollup$rolled$mean_min, min(sum_x[startsWith(sum_x$variable, "Sigma"),]$mean))

  rollup <- rollup_summary(sum_x, "mean", "min")
  expect_equal(names(rollup$rolled), c("variable", "dim", "mean_mean", "mean_min", "sd_mean", "sd_min"))
  expect_equal(rollup$rolled$mean_mean[1], mean(sum_x[startsWith(sum_x$variable, "mu"),]$mean))

  rollup <- rollup_summary(sum_x, mean = c("median", "mean"), .funs = list(mean = "stop", sd = "min"))
  expect_equal(names(rollup$rolled), c("variable", "dim", "mean_median", "mean_mean", "sd_min"))
  expect_equal(rollup$rolled$mean_median[1], median(sum_x[startsWith(sum_x$variable, "mu"),]$mean))

  x2 <- draws_rvars(x = c(rvar(matrix(1:20, ncol = 2)), NA))
  sum_x2 <- summarise_draws(x2, min, max)
  rollup <- rollup_summary(sum_x2, list(min = function(x) min(x, na.rm = TRUE)), max)
  expect_equal(rollup$rolled$variable, "x")
  expect_equal(rollup$rolled$dim, "3")
  expect_equal(rollup$rolled$min_min, 1)
  expect_equal(rollup$rolled$min_max, NA_real_)
  expect_equal(rollup$rolled$max_min, 10)
  expect_equal(rollup$rolled$max_max, NA_real_)
})

test_that("chaining rollups works", {
  set.seed(1234)
  x <- example_draws(example = "multi_normal")
  x <- mutate_variables(x, y = rnorm(ndraws(x)))
  x <- as_draws_df(x)

  sum_x <- summarise_draws(x, "mean", "sd")

  rollup <- rollup_summary(
    rollup_summary(sum_x, variable = "mu", sd = "min"),
    variable = "Sigma", sd = "max"
  )
  expect_equal(rollup$unrolled$variable, "y")
  expect_equal(rollup$rolled$variable, c("mu", "Sigma"))
  expect_equal(names(rollup$rolled), c("variable", "dim", "mean_min", "mean_max", "sd_min", "sd_max"))
  expect_equal(rollup$rolled$sd_min, c(min(sum_x[startsWith(sum_x$variable, "mu"),]$sd), NA_real_))
  expect_equal(rollup$rolled$sd_max, c(NA_real_, max(sum_x[startsWith(sum_x$variable, "Sigma"),]$sd)))
})

test_that("rollup on draws-like object works", {
  x <- as_draws_array(example_draws())
  expect_equal(rollup_summary(unclass(x)), rollup_summary(x))
})

test_that("rollup on data frames works", {
  x <- example_draws()
  sum_x <- summarise_draws(x)
  df_sum_x <- as.data.frame(sum_x)
  df_sum_x$variable <- factor(df_sum_x$variable)

  expect_equal(rollup_summary(df_sum_x)$rolled, rollup_summary(sum_x)$rolled)
})

test_that("NULL rollup functions work", {
  x <- example_draws()

  expect_equal(
    as.data.frame(rollup_summary(x, .funs = NULL)$rolled),
    data.frame(variable = "theta", dim = "8", stringsAsFactors = FALSE)
  )
})

test_that("printing works", {
  x <- rollup_summary(example_draws())

  for (color in c(TRUE, FALSE)) {
    out <- capture.output(print(x, color = color))
    expect_match(out, "<rollup_summary>", fixed = TRUE, all = FALSE)
    expect_match(out, "$unrolled", fixed = TRUE, all = FALSE)
    expect_match(out, "variable +mean +median", all = FALSE)
    expect_match(out, "$rolled", fixed = TRUE, all = FALSE)
    expect_match(out, "variable +dim +mean_min +mean_max", all = FALSE)
  }
})
