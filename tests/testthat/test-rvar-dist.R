test_that("distributional functions work", {
  x_values = c(2,4,3,5)
  x = rvar(x_values)

  x_density = density(x_values, cut = 0)
  expect_equal(density(x, at = x_density$x), x_density$y)

  x_cdf = ecdf(x_values)(x_values)
  expect_equal(cdf(x, x_values), x_cdf)

  expect_equal(quantile(x, 1:4/4), quantile(x_values, 1:4/4))
})
