test_that("distributional functions work on a scalar rvar", {
  x_values <- c(2,4,3,5)
  x <- rvar(x_values)

  x_density <- density(x_values, cut = 0)
  expect_equal(density(x, at = x_density$x), x_density$y)

  x_cdf <- ecdf(x_values)(x_values)
  expect_equal(cdf(x, x_values), x_cdf)

  expect_equal(quantile(x, 1:4/4), quantile(x_values, 1:4/4, names = FALSE))
})

test_that("distributional functions work on an rvar array", {
  x <- rvar(array(1:12, dim = c(3,2,2)))

  d11 <- c(density(1:3, n = 3, from = 1, to = 3)$y, rep(0, 9))
  d21 <- c(rep(0, 3), density(4:6, n = 3, from = 4, to = 6)$y, rep(0, 6))
  d12 <- c(rep(0, 6), density(7:9, n = 3, from = 7, to = 9)$y, rep(0, 3))
  d22 <- c(rep(0, 9), density(10:12, n = 3, from = 10, to = 12)$y)
  x_density <- array(c(d11, d21, d12, d22), dim = c(12, 2, 2))
  expect_equal(density(x, at = 1:12), x_density)

  cdf11 <- ecdf(1:3)(1:12)
  cdf21 <- ecdf(4:6)(1:12)
  cdf12 <- ecdf(7:9)(1:12)
  cdf22 <- ecdf(10:12)(1:12)
  x_cdf <- array(c(cdf11, cdf21, cdf12, cdf22), dim = c(12, 2, 2))
  expect_equal(cdf(x, 1:12), x_cdf)

  p <- ppoints(9, a = 1)
  q11 <- quantile(1:3, p)
  q21 <- quantile(4:6, p)
  q12 <- quantile(7:9, p)
  q22 <- quantile(10:12, p)
  x_quantiles <- array(c(q11, q21, q12, q22), dim = c(9, 2, 2), dimnames = list(NULL))
  expect_equal(quantile(x, p), x_quantiles)
})
