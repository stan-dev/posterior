test_that("distributional functions work on a scalar rvar", {
  x_values <- c(2,4,3,5)
  x <- rvar(x_values)

  x_density <- density(x_values, cut = 0)
  expect_equal(density(x, at = x_density$x), x_density$y)

  x_cdf <- ecdf(x_values)(x_values)
  expect_equal(cdf(x, x_values), x_cdf)

  expect_equal(quantile(x, 1:4/4), quantile(x_values, 1:4/4, names = FALSE))

  expect_equal(quantile(rvar(1:4), 0:4/4 + .Machine$double.eps, type = 1), c(1:4, 4))
  expect_equal(quantile(rvar(1:4), 0:4/4, type = 2), c(1, 1.5, 2.5, 3.5, 4))
  expect_equal(quantile(rvar(1:4), 0:4/4 + .Machine$double.eps, type = 3), c(1, 1:4))
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
  x_quantiles <- array(c(q11, q21, q12, q22), dim = c(9, 2, 2))
  expect_equal(quantile(x, p), x_quantiles)
})

test_that("distributional functions work on an rvar_factor", {
  x_values <- c(2,2,2,4,4,4,4,3,5,3)
  x_letters <- letters[x_values]
  x <- rvar_factor(x_letters, levels = letters[1:5])
  x2 <- c(rvar_factor(letters), rvar_factor(letters))

  expect_equal(density(x, letters[1:6]), c(0, .3, .2, .4, .1, NA_real_))
  expect_equal(density(x2, letters[1:3]), array(rep(1/26, 6), dim = c(3,2)))

  expect_equal(cdf(x, letters[1:5]), c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
  expect_equal(cdf(x2, letters[1:3]), array(rep(NA_real_, 6), dim = c(3,2)))

  expect_equal(quantile(x, 1:4/4), c(NA_real_, NA_real_, NA_real_, NA_real_))
  expect_equal(quantile(x2, 1:3/3), array(rep(NA_real_, 6), dim = c(3,2)))
})

test_that("distributional functions work on an rvar_ordered", {
  x_values <- c(2,2,2,4,4,4,4,3,5,3)
  x_letters <- letters[x_values]
  x <- rvar_ordered(x_letters, levels = letters[1:6])

  expect_equal(density(x, letters[1:7]), c(0, .3, .2, .4, .1, 0, NA))

  expect_equal(cdf(x, letters[1:7]), c(0, .3, .5, .9, 1, 1, NA))

  expect_equal(quantile(x, c(.3, .5, .9, 1)), letters[2:5])
})

# weighted rvar -----------------------------------------------------------

test_that("weighted rvar works", {
  x1_draws = qnorm(ppoints(10))
  x2_draws = qnorm(ppoints(10), 5)
  w1 = rep(1, 10)
  w2 = rep(2, 10)
  w3 = rep(0, 10)
  x = rvar(c(x1_draws, x2_draws, rep(10, 10)), log_weights = log(c(w1, w2, w3)))

  expect_equal(
    density(x, 0:9, bw = 2.25),
    density(draws_of(x), weights = weights(x), bw = 2.25, from = 0, to = 9, n = 10)$y,
    tolerance = 1e-4
  )
  expect_equal(cdf(x, 0:9), ecdf(x1_draws)(0:9)/3 + ecdf(x2_draws)(0:9)*2/3)
  expect_equal(quantile(x, cdf(x, c(x1_draws, x2_draws)), type = 1), c(x1_draws, x2_draws))
  expect_equal(quantile(x, cdf(x, c(x1_draws, x2_draws)), type = 4), c(x1_draws, x2_draws))
  expect_equal(unname(quantile2(x, cdf(x, c(x1_draws, x2_draws)), type = 1)), c(x1_draws, x2_draws))
  expect_equal(unname(quantile2(x, cdf(x, c(x1_draws, x2_draws)), type = 4)), c(x1_draws, x2_draws))

  x_na <- rvar(c(draws_of(x), NA_real_), log_weights = c(log_weights(x), 1))
  expect_equal(quantile(x_na, c(0.25, 0.5, 0.75), type = 4), c(NA_real_, NA_real_, NA_real_))
  expect_equal(
    quantile(x_na, c(0.25, 0.5, 0.75), type = 7, na.rm = TRUE),
    quantile(x, c(0.25, 0.5, 0.75), type = 7)
  )

  expect_equal(quantile(rvar(1), 0.5), 1)
  expect_equal(quantile(rvar(), 0.5), numeric())
})

test_that("weighted rvar_factor works", {
  x = rvar_factor(c("b", "g", "f", "g"), levels = letters, log_weights = log(c(1/2, 1/6, 1/6, 1/6)))

  expect_equal(density(x, letters), c(0, 1/2, 0, 0, 0, 1/6, 1/3, rep(0, 19)))
  expect_equal(cdf(x, letters), rep(NA_real_, 26))
  expect_equal(quantile(x, c(0.2, 0.8)), rep(NA_real_, 2))
})

test_that("weighted rvar_ordered works", {
  x = rvar_ordered(c("b", "g", "f", "g"), levels = letters, log_weights = log(c(1/2, 1/6, 1/6, 1/6)))

  expect_equal(density(x, letters), c(0, 1/2, 0, 0, 0, 1/6, 1/3, rep(0, 19)))
  expect_equal(cdf(x, letters), cumsum(c(0, 1/2, 0, 0, 0, 1/6, 1/3, rep(0, 19))))
  expect_equal(quantile(x, c(0.2, 0.6, 0.8)), c("b", "f", "g"))

  xl = weight_draws(rvar_ordered(letters), 1:26)
  expect_equal(quantile(xl, cdf(xl, letters) - .Machine$double.eps), letters)
})

