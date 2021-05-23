test_that("mutate_variables works correctly for draws_df objects", {
  x <- as_draws_df(example_draws())

  x <- mutate_variables(x, tau2 = tau^2)
  expect_equal(x$tau2, x$tau^2)

  x <- mutate_variables(x, scale = 1.96 * tau, lower = mu - scale)
  expect_equal(x$lower, x$mu - 1.96 * x$tau)

  x <- mutate_variables(x, mean = mean(mu))
  expect_equal(x$mean, rep(mean(x$mu), NROW(x)))

  expect_error(
    mutate_variables(x, mu = as.character(mu)),
    "does not evaluate to a numeric vector"
  )
  expect_error(
    mutate_variables(x, mu = mu[1:2]),
    "does not evaluate to a vector of length 1 or 400"
  )
})

test_that("mutate_variables works correctly for draws_list objects", {
  x <- as_draws_list(example_draws())

  x <- mutate_variables(x, tau2 = tau^2)
  expect_equal(x[[1]]$tau2, x[[1]]$tau^2)
  expect_equal(x[[2]]$tau2, x[[2]]$tau^2)

  x <- mutate_variables(x, scale = 1.96 * tau, lower = mu - scale)
  expect_equal(x[[2]]$lower, x[[2]]$mu - 1.96 * x[[2]]$tau)

  x <- mutate_variables(x, mean = mean(mu))
  expect_equal(x[[1]]$mean, rep(mean(x[[1]]$mu), niterations(x)))

  expect_error(
    mutate_variables(x, mu = as.character(mu)),
    "does not evaluate to a numeric vector"
  )
  expect_error(
    mutate_variables(x, mu = mu[1:2]),
    "does not evaluate to a vector of length 1 or 100"
  )
})

test_that("mutate_variables works correctly for draws_matrix objects", {
  x <- as_draws_matrix(example_draws())
  x <- mutate_variables(x, tau2 = tau^2)
  expect_equal(x[, "tau2"], x[, "tau"]^2, check.attributes = FALSE)
})

test_that("mutate_variables works correctly for draws_array objects", {
  x <- as_draws_array(example_draws())
  x <- mutate_variables(x, tau2 = tau^2)
  expect_equal(x[, , "tau2"], x[, , "tau"]^2, check.attributes = FALSE)
})

test_that("mutate_variables works correctly for draws_rvars objects", {
  x <- as_draws_rvars(example_draws())
  x <- mutate_variables(x, tau_sq = tau^2, theta_2_sq = theta[[2]]^2)
  expect_equal(x$tau_sq, x$tau^2, check.attributes = FALSE)
  expect_equal(x$theta_2_sq, x$theta[[2]]^2, check.attributes = FALSE)
})

