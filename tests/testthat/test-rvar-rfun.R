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
