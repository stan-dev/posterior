test_that("rhat diagnostics return reasonable values", {
  tau <- extract_one_variable_matrix(example_draws(), "tau")

  rhat <- rhat_basic(tau)
  expect_true(rhat > 0.99 & rhat < 1.05)

  rhat <- rhat(tau)
  expect_true(rhat > 0.99 & rhat < 1.05)
})

test_that("ess diagnostics return reasonable values", {
  tau <- extract_one_variable_matrix(example_draws(), "tau")

  ess <- ess_basic(tau)
  expect_true(ess > 380 & ess < 420)

  ess <- ess_mean(tau)
  expect_true(ess > 380 & ess < 420)

  ess <- ess_sd(tau)
  expect_true(ess > 380 & ess < 420)

  ess <- ess_bulk(tau)
  expect_true(ess > 380 & ess < 420)

  ess <- ess_tail(tau)
  expect_true(ess > 280 & ess < 320)

  ess <- ess_quantile(tau, probs = c(0.1, 0.9))
  expect_equal(names(ess), c("q10", "q90"))
  expect_true(all(ess > 280 & ess < 350))

  ess <- ess_median(tau)
  expect_true(ess > 350 & ess < 420)
})

test_that("mcse diagnostics return reasonable values", {
  tau <- extract_one_variable_matrix(example_draws(), "tau")

  mcse <- mcse_mean(tau)
  expect_true(mcse > 0.1 & mcse < 0.2)

  mcse <- mcse_sd(tau)
  expect_true(mcse > 0.1 & mcse < 0.2)

  mcse <- mcse_quantile(tau, probs = c(0.1, 0.9))
  expect_equal(names(mcse), c("q10", "q90"))
  expect_true(mcse[1] > 0.05 & mcse[1] < 0.12)
  # due to right skewness of tau the 90%ile is way more uncertain
  expect_true(mcse[2] > 0.8 & mcse[2] < 1)

  mcse <- mcse_median(tau)
  expect_true(mcse > 0.1 & mcse < 0.2)
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
})
