test_that("gpdfit returns NA when first quartile equals minimum", {
  # gpdfit should return NA if xstar == x[1]
  # This can happen when the distribution is far from a generalized Pareto distribution

  # Case 1: All values are the same (constant)
  x_constant <- rep(5, 100)
  result <- gpdfit(x_constant)
  expect_true(is.na(result$k))
  expect_true(is.na(result$sigma))

  # Case 2: First quartile equals minimum (many repeated minimum values)
  # More than 25% of values are at the minimum
  x_many_min <- c(rep(1, 30), seq(2, 10, length.out = 70))
  result <- gpdfit(x_many_min)
  expect_true(is.na(result$k))
  expect_true(is.na(result$sigma))

  # Case 3: Exactly 25% at minimum (edge case where xstar == x[1])
  x_quarter_min <- c(rep(0, 25), seq(1, 10, length.out = 75))
  result <- gpdfit(x_quarter_min)
  expect_true(is.na(result$k))
  expect_true(is.na(result$sigma))

})

test_that("gpdfit returns valid estimates for exponential-like tails", {
  set.seed(1234)

  # Exponential distribution (k should be close to 0)
  x <- sort(rexp(1000))
  result <- gpdfit(x)

  expect_false(is.na(result$k))
  expect_false(is.na(result$sigma))
  expect_true(is.finite(result$k))
  expect_true(result$sigma > 0)
})

test_that("gpdfit handles sort_x argument correctly", {
  set.seed(1234)

  x <- rexp(100)
  x_sorted <- sort(x)

  # Results should be the same whether we sort or provide pre-sorted data
  result_sort <- gpdfit(x, sort_x = TRUE)
  result_presorted <- gpdfit(x_sorted, sort_x = FALSE)

  expect_equal(result_sort$k, result_presorted$k)
  expect_equal(result_sort$sigma, result_presorted$sigma)
})

test_that("gpdfit handles wip argument correctly", {
  set.seed(1234)

  x <- sort(rexp(100))

  result_wip <- gpdfit(x, wip = TRUE)
  result_no_wip <- gpdfit(x, wip = FALSE)

  # Both should give valid results
  expect_false(is.na(result_wip$k))
  expect_false(is.na(result_no_wip$k))

  # With WIP prior, k should be pulled toward 0.5
  expect_true(abs(0.5 - result_wip$k) < abs(0.5 - result_no_wip$k))
})
