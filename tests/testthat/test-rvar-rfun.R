test_that("rdo works", {
  x_array <- array(1:24, dim = c(4,2,3))
  x <- new_rvar(x_array)
  y_array <- array(c(2:13,12:1), dim = c(4,3,2))
  y <- new_rvar(y_array)

  xy_ref <- new_rvar(abind::abind(along = 0,
    x_array[1,,] %*% y_array[1,,],
    x_array[2,,] %*% y_array[2,,],
    x_array[3,,] %*% y_array[3,,],
    x_array[4,,] %*% y_array[4,,]
  ))
  expect_equal(rdo(x %*% y), xy_ref)
})

test_that("rfun works", {
  x_array <- array(1:24, dim = c(4,2,3))
  x <- new_rvar(x_array)
  y_array <- array(c(2:13,12:1), dim = c(4,3,2))
  y <- new_rvar(y_array)

  xy_ref <- new_rvar(abind::abind(along = 0,
    x_array[1,,] %*% y_array[1,,],
    x_array[2,,] %*% y_array[2,,],
    x_array[3,,] %*% y_array[3,,],
    x_array[4,,] %*% y_array[4,,]
  ))
  expect_equal(rfun(function(a,b) a %*% b)(x, y), xy_ref)

  expect_error(rfun(function(a,b) a %*% b, rvar_args = "c"), "All arguments .* must be names of formal arguments of `.f`")
})

test_that("rfun works on functions without rvar arguments", {
  i <- 0
  f <- function(x) {
    i <<- i + 1
    i
  }
  expect_equal(rfun(f, rvar_args = NULL, ndraws = 10)(NULL), rvar(1:10))
})

test_that("rfun works on primitive functions", {
  x = rvar(1:10)

  rfun_sin = rfun(sin)
  expect_equal(names(formals(rfun_sin)), "x")
  expect_equal(rfun_sin(x), sin(x))
})

test_that("rfun works on dots arguments", {
  g <- function(x, y, z = 2) {
    c(x, y, z)
  }
  f <- function(x, ...) {
    g(x, ...)
  }

  expect_equal(rfun(f)(z = rvar(1:3), x = rvar(3:5), 3), c(rvar(3:5), 3, rvar(1:3)))
})

test_that("rfun works on formula functions", {
  expect_equal(rfun(~ . ^ 2)(rvar(1:3)), rvar((1:3)^2))
  expect_equal(rfun(~ .x ^ 2)(rvar(1:3)), rvar((1:3)^2))
  expect_equal(rfun(~ ..1 ^ 2)(rvar(1:3)), rvar((1:3)^2))
  expect_equal(rfun(~ .x ^ .y)(rvar(1:3), 2), rvar((1:3)^2))
  expect_equal(rfun(~ ..1 ^ ..2)(rvar(1:3), 2), rvar((1:3)^2))
})

test_that("rdo allows setting the dimensions of the result", {
  expect_equal(rdo(1:10, dim = c(2,5), ndraws = 1), rvar(array(1:10, dim = c(1,2,5))))
})

test_that("rvar_rng works", {
  set.seed(1234)

  mu <- rvar_rng(rnorm, 10, mean = 1:10, sd = 1)
  sigma <- rvar_rng(rgamma, 1, shape = 1, rate = 1)
  x <- rvar_rng(rnorm, 10, mu, sigma)

  expect_equal(mean(x), 1:10, tolerance = 0.1)
  expect_equal(apply(draws_of(x), 2, sd), rep(1.7, 10), tolerance = 0.1)
})

test_that("rvar_rng recycling works with numeric and rvar arguments", {
  # a fake random number generator that we can use to ensure draws are being
  # correctly reshaped and lined up
  rfake = function(n, tens, ones) {
    rep_len(tens * 10 + ones, length.out = n)
  }

  # numeric arguments
  ref <- rvar(array(c(11, 11, 22, 22, 13, 13, 24, 24), dim = c(2,4)))
  expect_equal(rvar_rng(rfake, 4, 1:2, 1:4, ndraws = 2), ref)

  # mixed numeric and rvar arguments
  ones <- rvar(array(1:8, dim = c(2,4)))
  ref <- rvar(array(c(11, 12, 23, 24, 15, 16, 27, 28), dim = c(2,4)))
  expect_equal(rvar_rng(rfake, 4, 1:2, ones), ref)

  # rvar arguments
  x <- c(15, 26, 37, 48)
  expect_equal(rvar_rng(rfake, 1, rvar(1:4), rvar(5:8)), rvar(x))
  expect_equal(rvar_rng(rfake, 2, rvar(1:4), rvar(5:8)), rvar(array(c(x, x), dim = c(4, 2))))

  tens <- rvar(array(1:4, dim = c(2,2)))
  ones <- rvar(array(1:8, dim = c(2,4)))
  ref <- rvar(array(c(11, 22, 33, 44, 15, 26, 37, 48), dim = c(2,4)))
  expect_equal(rvar_rng(rfake, 4, tens, ones), ref)
})

test_that("rvar_rng requires rvar arguments to be 1 dimensional", {
  expect_error(rvar_rng(rnorm, 1, as_rvar(matrix(1:4, nrow = 2))), "must be single-dimensional")
})
