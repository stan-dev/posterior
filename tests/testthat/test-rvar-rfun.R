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
  expect_identical(rdo(x %*% y), xy_ref)
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
  expect_identical(rfun(function(a,b) a %*% b)(x, y), xy_ref)
})

test_that("rvar_rng works", {
  set.seed(1234)

  mu <- rvar_rng(rnorm, 10, mean = 1:10, sd = 1)
  sigma <- rvar_rng(rgamma, 1, shape = 1, rate = 1)
  x <- rvar_rng(rnorm, 10, mu, sigma)

  expect_equal(mean(x), 1:10, tolerance = 0.1)
  expect_equal(apply(draws_of(x), 2, sd), rep(1.7, 10), tolerance = 0.1)
})
