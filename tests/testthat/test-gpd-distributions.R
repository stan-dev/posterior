# --- qgeneralized_pareto tests ------------------------------------------------

test_that("qgeneralized_pareto returns correct quantiles for k != 0", {
  # Known values for GPD(mu=0, sigma=1, k=0.5)
  # Q(p) = (1/k) * ((1-p)^(-k) - 1) = 2 * ((1-p)^(-0.5) - 1)
  p <- c(0, 0.25, 0.5, 0.75, 1)
  expected <- 2 * ((1 - p)^(-0.5) - 1)
  result <- qgeneralized_pareto(p, mu = 0, sigma = 1, k = 0.5)
  expect_equal(result, expected)
})

test_that("qgeneralized_pareto returns correct quantiles for k == 0 (exponential)", {
  # When k == 0, GPD reduces to exponential: Q(p) = mu - sigma * log(1-p)
  p <- c(0.1, 0.5, 0.9)
  expected <- -log(1 - p)
  result <- qgeneralized_pareto(p, mu = 0, sigma = 1, k = 0)
  expect_equal(result, expected)
})

test_that("qgeneralized_pareto respects mu and sigma", {
  p <- c(0.1, 0.5, 0.9)
  base <- qgeneralized_pareto(p, mu = 0, sigma = 1, k = 0.2)

  # Shifting mu shifts quantiles
  shifted <- qgeneralized_pareto(p, mu = 5, sigma = 1, k = 0.2)
  expect_equal(shifted, base + 5)

  # Scaling sigma scales quantiles (relative to mu)
  scaled <- qgeneralized_pareto(p, mu = 0, sigma = 3, k = 0.2)
  expect_equal(scaled, base * 3)
})

test_that("qgeneralized_pareto handles lower.tail = FALSE", {
  p <- c(0.1, 0.5, 0.9)
  result_lower <- qgeneralized_pareto(p, mu = 0, sigma = 1, k = 0.2)
  result_upper <- qgeneralized_pareto(1 - p, mu = 0, sigma = 1, k = 0.2, lower.tail = FALSE)
  expect_equal(result_lower, result_upper)
})

test_that("qgeneralized_pareto handles log.p = TRUE", {
  p <- c(0.1, 0.5, 0.9)
  result <- qgeneralized_pareto(p, mu = 0, sigma = 1, k = 0.2)
  result_log <- qgeneralized_pareto(log(p), mu = 0, sigma = 1, k = 0.2, log.p = TRUE)
  expect_equal(result, result_log)
})

test_that("qgeneralized_pareto returns NaN for invalid sigma", {
  result <- qgeneralized_pareto(0.5, mu = 0, sigma = -1, k = 0.2)
  expect_true(is.nan(result))

  result <- qgeneralized_pareto(0.5, mu = 0, sigma = 0, k = 0.2)
  expect_true(is.nan(result))

  result <- qgeneralized_pareto(c(0.1, 0.5), mu = 0, sigma = NA, k = 0.2)
  expect_length(result, 2)
  expect_true(all(is.nan(result)))
})

test_that("qgeneralized_pareto errors on vector parameters", {
  expect_error(qgeneralized_pareto(0.5, mu = c(0, 1), sigma = 1, k = 0.2))
  expect_error(qgeneralized_pareto(0.5, mu = 0, sigma = c(1, 2), k = 0.2))
  expect_error(qgeneralized_pareto(0.5, mu = 0, sigma = 1, k = c(0.1, 0.2)))
})

# --- pgeneralized_pareto tests ------------------------------------------------

test_that("pgeneralized_pareto returns correct probabilities for k != 0", {
  # CDF: p = 1 - (1 + k*z)^(-1/k), z = (q-mu)/sigma
  q <- c(0, 1, 2, 5)
  k <- 0.5
  expected <- 1 - (1 + k * q)^(-1 / k)
  result <- pgeneralized_pareto(q, mu = 0, sigma = 1, k = k)
  expect_equal(result, expected)
})

test_that("pgeneralized_pareto returns correct probabilities for k == 0 (exponential)", {
  q <- c(0, 0.5, 1, 2, 5)
  expected <- 1 - exp(-q)
  result <- pgeneralized_pareto(q, mu = 0, sigma = 1, k = 0)
  expect_equal(result, expected)
})

test_that("pgeneralized_pareto and qgeneralized_pareto are inverses (k != 0)", {
  p <- seq(0.05, 0.95, by = 0.05)
  params <- list(
    list(mu = 0, sigma = 1, k = 0.2),
    list(mu = 5, sigma = 2, k = -0.3),
    list(mu = -1, sigma = 0.5, k = 0.5)
  )
  for (par in params) {
    q <- qgeneralized_pareto(p, mu = par$mu, sigma = par$sigma, k = par$k)
    p_back <- pgeneralized_pareto(q, mu = par$mu, sigma = par$sigma, k = par$k)
    expect_equal(p_back, p, tolerance = 1e-12)
  }
})

test_that("pgeneralized_pareto and qgeneralized_pareto are inverses (k == 0)", {
  p <- seq(0.05, 0.95, by = 0.05)
  q <- qgeneralized_pareto(p, mu = 0, sigma = 1, k = 0)
  p_back <- pgeneralized_pareto(q, mu = 0, sigma = 1, k = 0)
  expect_equal(p_back, p, tolerance = 1e-12)
})

test_that("pgeneralized_pareto clamps values outside support", {
  # Below the location parameter, CDF should be 0
  result <- pgeneralized_pareto(-1, mu = 0, sigma = 1, k = 0.2)
  expect_equal(result, 0)

  # For k < 0 there is an upper bound at mu - sigma/k
  # k = -0.5, sigma = 1 => upper bound = 0 - 1/(-0.5) = 2
  result <- pgeneralized_pareto(3, mu = 0, sigma = 1, k = -0.5)
  expect_equal(result, 1)
})

test_that("pgeneralized_pareto handles lower.tail = FALSE", {
  q <- c(0.5, 1, 2)
  result_lower <- pgeneralized_pareto(q, mu = 0, sigma = 1, k = 0.2)
  result_upper <- pgeneralized_pareto(q, mu = 0, sigma = 1, k = 0.2, lower.tail = FALSE)
  expect_equal(result_lower + result_upper, rep(1, 3))
})

test_that("pgeneralized_pareto handles log.p = TRUE", {
  q <- c(0.5, 1, 2)
  result <- pgeneralized_pareto(q, mu = 0, sigma = 1, k = 0.2)
  result_log <- pgeneralized_pareto(q, mu = 0, sigma = 1, k = 0.2, log.p = TRUE)
  expect_equal(result_log, log(result))
})

test_that("pgeneralized_pareto returns NaN for invalid sigma", {
  result <- pgeneralized_pareto(1, mu = 0, sigma = -1, k = 0.2)
  expect_true(is.nan(result))

  result <- pgeneralized_pareto(1, mu = 0, sigma = 0, k = 0.2)
  expect_true(is.nan(result))

  result <- pgeneralized_pareto(c(1, 2), mu = 0, sigma = NA, k = 0.2)
  expect_length(result, 2)
  expect_true(all(is.nan(result)))
})

test_that("pgeneralized_pareto errors on vector parameters", {
  expect_error(pgeneralized_pareto(1, mu = c(0, 1), sigma = 1, k = 0.2))
  expect_error(pgeneralized_pareto(1, mu = 0, sigma = c(1, 2), k = 0.2))
  expect_error(pgeneralized_pareto(1, mu = 0, sigma = 1, k = c(0.1, 0.2)))
})

test_that("pgeneralized_pareto at mu equals 0", {
  # CDF at the location parameter should be 0
  expect_equal(pgeneralized_pareto(0, mu = 0, sigma = 1, k = 0), 0)
  expect_equal(pgeneralized_pareto(0, mu = 0, sigma = 1, k = 0.5), 0)
  expect_equal(pgeneralized_pareto(5, mu = 5, sigma = 2, k = 0.3), 0)
})

# --- gpdfit tests -------------------------------------------------------------

test_that("gpdfit with uniform weights matches unweighted fit", {
  set.seed(42)
  x <- sort(rexp(200))
  fit_no_wt <- gpdfit(x, sort_x = FALSE)
  fit_uniform <- gpdfit(x, sort_x = FALSE, weights = rep(1, 200))
  expect_equal(fit_uniform$k, fit_no_wt$k)
  expect_equal(fit_uniform$sigma, fit_no_wt$sigma)
})

test_that("gpdfit with scaled uniform weights matches unweighted fit", {
  set.seed(42)
  x <- sort(rexp(200))
  fit_no_wt <- gpdfit(x, sort_x = FALSE)
  fit_scaled <- gpdfit(x, sort_x = FALSE, weights = rep(7.3, 200))
  expect_equal(fit_scaled$k, fit_no_wt$k)
  expect_equal(fit_scaled$sigma, fit_no_wt$sigma)
})

test_that("gpdfit with non-uniform weights differs from unweighted fit", {
  set.seed(42)
  x <- sort(rexp(200))
  w <- c(rep(1, 150), rep(5, 50))
  fit_unif <- gpdfit(x, sort_x = FALSE)
  fit_wt <- gpdfit(x, sort_x = FALSE, weights = w)
  expect_false(isTRUE(all.equal(fit_unif$k, fit_wt$k)))
  expect_false(isTRUE(all.equal(fit_unif$sigma, fit_wt$sigma)))
})

test_that("gpdfit sort_x = TRUE reorders weights correctly", {
  set.seed(42)
  x <- rexp(200)
  w <- runif(200, 0.5, 2)

  fit_auto <- gpdfit(x, sort_x = TRUE, weights = w)

  ord <- order(x)
  fit_manual <- gpdfit(x[ord], sort_x = FALSE, weights = w[ord])

  expect_equal(fit_auto$k, fit_manual$k)
  expect_equal(fit_auto$sigma, fit_manual$sigma)
})

test_that("gpdfit with zero-weight observations effectively ignores them", {
  set.seed(42)
  x <- rexp(100)

  # Fit on just the original 100 observations
  fit_base <- gpdfit(x, sort_x = TRUE)

  # Add 50 outlier observations with zero weight
  x_extra <- c(x, rexp(50, rate = 0.01))
  w_extra <- c(rep(1, 100), rep(0, 50))
  fit_zero <- gpdfit(x_extra, sort_x = TRUE, weights = w_extra)

  # Not exactly equal (N changes the grid and WIP prior), but close
  expect_equal(fit_zero$k, fit_base$k, tolerance = 0.15)
  expect_equal(fit_zero$sigma, fit_base$sigma, tolerance = 0.15)
})

test_that("gpdfit upweighting tail draws changes the fit", {
  set.seed(42)
  x <- sort(rexp(300))

  fit_unif <- gpdfit(x, sort_x = FALSE)

  # Upweight the largest values
  w <- rep(1, 300)
  w[251:300] <- 3
  fit_heavy <- gpdfit(x, sort_x = FALSE, weights = w)

  expect_false(isTRUE(all.equal(fit_unif$sigma, fit_heavy$sigma)))
})

test_that("gpdfit with weights = NULL is backward compatible", {
  set.seed(42)
  x <- rexp(150)

  fit1 <- gpdfit(x)
  fit2 <- gpdfit(x, weights = NULL)
  expect_identical(fit1, fit2)
})
