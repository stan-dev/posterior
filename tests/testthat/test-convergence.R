test_that("rhat diagnostics return reasonable values", {
  tau <- extract_variable_matrix(example_draws(), "tau")

  rhat <- rhat_basic(tau)
  expect_true(rhat > 0.99 & rhat < 1.05)

  rhat <- rhat(tau)
  expect_true(rhat > 0.99 & rhat < 1.05)
})

test_that("ess diagnostics return reasonable values", {
  tau <- extract_variable_matrix(example_draws(), "tau")
  mu <- extract_variable_matrix(example_draws("multi_normal"), "mu[1]")

  ess <- ess_basic(tau)
  expect_true(ess > 250 & ess < 310)

  ess <- ess_mean(tau)
  expect_true(ess > 250 & ess < 310)

  ess <- ess_sd(tau)
  expect_true(ess > 230 & ess < 280)

  ess <- ess_bulk(tau)
  expect_true(ess > 230 & ess < 280)

  # some chains are constant for the computed tail quantiles
  ess <- ess_tail(tau)
  expect_true(ess > 180 & ess < 220)
  # use a different example to obtain non-NA value
  ess <- ess_tail(mu)
  expect_true(ess > 330 & ess < 380)

  ess <- ess_quantile(tau, probs = c(0.2, 0.8))
  expect_equal(names(ess), c("ess_q20", "ess_q80"))
  expect_true(ess[1] > 150 & ess[1] < 210)
  expect_true(ess[2] > 350 & ess[2] < 420)

  ess <- ess_median(tau)
  expect_true(ess > 350 & ess < 420)
})

test_that("negative ess estimates are avoided with a warning", {
  x <- stats::arima.sim(list(ar=-0.9), 1000)
  expect_warning(ess <- ess_basic(x), "The ESS has been capped")
  expect_equal(ess, 3000)
})

test_that("mcse diagnostics return reasonable values", {
  tau <- extract_variable_matrix(example_draws(), "tau")

  mcse <- mcse_mean(tau)
  expect_true(mcse > 0.15 & mcse < 0.25)

  mcse <- mcse_sd(tau)
  expect_true(mcse > 0.15 & mcse < 0.25)

  mcse <- mcse_quantile(tau, probs = c(0.2, 0.8))
  expect_equal(names(mcse), c("mcse_q20", "mcse_q80"))
  expect_true(mcse[1] > 0.16 & mcse[1] < 0.21)
  # due to right skewness of tau the 90%ile is way more uncertain
  expect_true(mcse[2] > 0.3 & mcse[2] < 0.7)

  mcse <- mcse_median(tau)
  expect_true(mcse > 0.2 & mcse < 0.3)

  # check if issue #252 remains fixed
  se <- mcse_quantile(tau, c(0.999999, 1))
  expect_equivalent(se[1], se[2])

  se <- mcse_quantile(tau, c(0.000001, 0))
  expect_equivalent(se[1], se[2])
})

test_that("convergence diagnostics accept vectors as input", {
  set.seed(1234)
  x <- rnorm(1000)

  rhat <- rhat(x)
  expect_true(rhat > 0.99 & rhat < 1.01)

  ess <- ess_bulk(x)
  expect_true(ess > 900 & ess < 1100)

  ess <- ess_tail(x)
  expect_true(ess > 750 & ess < 850)

  mcse <- mcse_mean(x)
  expect_true(mcse > 0.02 & mcse < 0.04)
})

test_that("convergence diagnostics handle special cases correctly", {
  set.seed(1234)

  x <- c(rnorm(10), NA)
  expect_true(is.na(rhat_basic(x)))
  expect_true(is.na(ess_basic(x)))

  x <- c(rnorm(10), Inf)
  expect_true(is.na(rhat_basic(x)))
  expect_true(is.na(ess_basic(x)))

  x <- rep(1, 10)
  expect_true(is.na(rhat_basic(x)))
  expect_true(is.na(ess_basic(x)))

  # constant-per-chain checks deactivated for now
  # x <- cbind(1, rnorm(10))
  # expect_true(is.na(rhat_basic(x)))
  # expect_true(is.na(ess_basic(x)))
})

test_that("convergence diagnostics throw correct errors", {
  mu <- extract_variable_matrix(example_draws(), "mu")
  expect_error(ess_quantile(mu, probs = 1.2), "'probs' must contain values between 0 and 1")
  expect_error(mcse_quantile(mu, probs = 1.2), "'probs' must contain values between 0 and 1")
})

test_that("convergence functions work with rvars", {
  tau <- extract_variable_matrix(example_draws(), "tau")
  tau_rvar <- rvar(tau, with_chains = TRUE)

  expect_equal(ess_basic(tau_rvar), ess_basic(tau))
  expect_equal(ess_bulk(tau_rvar), ess_bulk(tau))
  expect_equal(ess_tail(tau_rvar), ess_tail(tau))
  # convergence functions with multiple return values on rvars returns a column
  # vector; should be equal to the NULL-dimension vector format when transposed
  expect_equal(t(ess_quantile(tau_rvar, probs = c(.1, .9))), t(ess_quantile(tau, probs = c(.1, .9))))
  expect_equal(ess_sd(tau_rvar), ess_sd(tau))
  expect_equal(mcse_mean(tau_rvar), mcse_mean(tau))
  expect_equal(t(mcse_quantile(tau_rvar, probs = c(.1, .9))), t(mcse_quantile(tau, probs = c(.1, .9))))
  expect_equal(mcse_sd(tau_rvar), mcse_sd(tau))
  expect_equal(rhat_basic(tau_rvar), rhat_basic(tau))
  expect_equal(rhat(tau_rvar), rhat(tau))
})

test_that("autocovariance returns correct results", {
  x <- rnorm(100)
  ac1 <- autocovariance(x)
  ac2 <- acf(x, type = "covariance", lag.max = length(x), plot = FALSE)$acf[, 1, 1]
  expect_equal(ac1, ac2)

  x <- arima.sim(list(ar = c(0.5, -0.3)), 100)
  ac1 <- autocovariance(x)
  ac2 <- acf(x, type = "covariance", lag.max = length(x), plot = FALSE)$acf[, 1, 1]
  expect_equal(ac1, ac2)
})
