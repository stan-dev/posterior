test_that("rhat diagnostics return reasonable values", {
  tau <- extract_variable_matrix(example_draws(), "tau")

  rhat <- rhat_basic(tau)
  expect_true(rhat > 0.99 & rhat < 1.05)

  rhat <- rhat(tau)
  expect_true(rhat > 0.99 & rhat < 1.05)
})

test_that("ess diagnostics return reasonable values", {
  tau <- extract_variable_matrix(example_draws(), "tau")

  ess <- ess_basic(tau)
  expect_true(ess > 250 & ess < 310)

  ess <- ess_mean(tau)
  expect_true(ess > 250 & ess < 310)

  ess <- ess_sd(tau)
  expect_true(ess > 250 & ess < 310)

  ess <- ess_bulk(tau)
  expect_true(ess > 230 & ess < 280)

  ess <- ess_tail(tau)
  expect_true(ess > 170 & ess < 220)

  ess <- ess_quantile(tau, probs = c(0.1, 0.9))
  expect_equal(names(ess), c("ess_q10", "ess_q90"))
  expect_true(ess[1] > 150 & ess[1] < 200)
  expect_true(ess[2] > 280 & ess[2] < 330)

  ess <- ess_median(tau)
  expect_true(ess > 350 & ess < 420)
})

test_that("mcse diagnostics return reasonable values", {
  tau <- extract_variable_matrix(example_draws(), "tau")

  mcse <- mcse_mean(tau)
  expect_true(mcse > 0.15 & mcse < 0.25)

  mcse <- mcse_sd(tau)
  expect_true(mcse > 0.15 & mcse < 0.25)

  mcse <- mcse_quantile(tau, probs = c(0.1, 0.9))
  expect_equal(names(mcse), c("mcse_q10", "mcse_q90"))
  expect_true(mcse[1] > 0.1 & mcse[1] < 0.15)
  # due to right skewness of tau the 90%ile is way more uncertain
  expect_true(mcse[2] > 0.3 & mcse[2] < 0.7)

  mcse <- mcse_median(tau)
  expect_true(mcse > 0.2 & mcse < 0.3)
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

test_that("convergence diagnostics throw correct errors", {
  mu <- extract_variable_matrix(example_draws(), "mu")
  expect_error(ess_quantile(mu, probs = 1.2), "'probs' must contain values between 0 and 1")
  expect_error(mcse_quantile(mu, probs = 1.2), "'probs' must contain values between 0 and 1")
})

test_that("rstar returns reasonable values", {
  x <- example_draws()
  val <- rstar(x)
  expect_true(val > 0.8 & val < 10)
})

test_that("rstar with uncertainty returns vectors of correct length", {
  x <- example_draws()
  val <- rstar(x, method = "gbm", uncertainty = T, verbose = F)
  expect_equal(length(val), 1000)
  val <- rstar(x, method = "knn", uncertainty = T, nsim = 10)
  expect_equal(length(val), 10)
})

test_that("incorrect nsim values throws error", {
  x <- example_draws()
  expect_error(rstar(x, method = "knn", nsim = 0), "nsim must exceed 1.")
})

test_that("rstar with uncertainty returns reasonable values", {
  x <- example_draws()
  val <- rstar(x, method = "gbm", uncertainty = T, verbose = F)
  expect_true(median(val) > 0.8 & median(val) < 10)
})

test_that("rstar accepts different classifiers", {
  x <- example_draws()
  val <- rstar(x, method = "gbm", verbose=F)
  expect_true(is.numeric(val))
  val <- rstar(x, method = "knn")
  expect_true(is.numeric(val))
})

test_that("rstar throws error when using invalid classifier", {
  x <- example_draws()
  expect_error(rstar(x, method = "gbmnn"),
               "Model gbmnn is not in caret's built-in library")
})

test_that("rstar accepts different hyperparameters", {
  x <- example_draws()

  # use fast hyperparameters
  caret_grid <- tibble(interaction.depth=c(3),
                       n.trees = 1,
                       shrinkage=c(0.1),
                       n.minobsinnode=10)
  start <- Sys.time()
  val <- rstar(x, method = "gbm", verbose=F,
               hyperparameters = caret_grid)
  end <- Sys.time()
  dif1 <- end - start
  # use slower hyperparameters
  caret_grid <- tibble(interaction.depth=c(3),
                       n.trees = 1000,
                       shrinkage=c(0.1),
                       n.minobsinnode=10)
  start <- Sys.time()
  val <- rstar(x, method = "gbm", verbose=F,
               hyperparameters = caret_grid)
  end <- Sys.time()
  dif2 <- end - start
  expect_true(dif1 < dif2)
})

test_that("rstar accepts different training proportion", {
  x <- example_draws()
  val1 <- rstar(x, method = "knn")
  val2 <- rstar(x, method = "knn", training_proportion = 0.1)
  expect_true(val1 > val2)
})

test_that("rstar throws error when passed invalid training_proportion", {
  x <- example_draws()
  expect_error(rstar(x, method = "knn", training_proportion = 0),
               "training_proportion must be greater than zero and less than 1.")
  expect_error(rstar(x, method = "knn", training_proportion = 1),
               "training_proportion must be greater than zero and less than 1.")
})

test_that("split-chain R* returns generally higher values", {
  x <- example_draws()
  n <- 5
  vals_split <- vector(length = n)
  vals_unsplit <- vector(length = n)
  for(i in 1:n) {
    vals_split[i] <- rstar(x, method = "knn")
    vals_unsplit[i] <- rstar(x, method = "knn", split_chains = F)
  }
  expect_true(median(vals_split) > median(vals_unsplit))
})
