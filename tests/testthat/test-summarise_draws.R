test_that("summarise_draws works correctly", {
  x <- as_draws_df(example_draws())
  sum_x <- summarise_draws(x)
  expect_true(all(default_convergence_measures() %in% names(sum_x)))
  expect_true(all(c("q5", "q95") %in% names(sum_x)))
  expect_equal(sum_x$variable, variables(x))
  expect_equal(mean(x$mu), as.numeric(sum_x$mean[sum_x$variable == "mu"]))

  sum_x <- summarise_draws(x, mean, median)
  expect_true(all(c("mean", "median") %in% names(sum_x)))

  sum_x <- summarise_draws(x, default_mcse_measures())
  expect_true(all(c("mcse_q5", "mcse_q95") %in% names(sum_x)))

  sum_x <- summarise_draws(x, ~quantile(.x, probs = c(0.4, 0.6)))
  expect_true(all(c("40%", "60%") %in% names(sum_x)))

  x[1, 1] <- NA
  sum_x <- summarise_draws(x)
  expect_true(is.na(sum_x[1, "q5"]))
  expect_true(all(c("q5", "q95") %in% names(sum_x)))
})

test_that("aliases of summarise_draws work", {
  x <- as_draws_array(example_draws())
  sum_x <- summarise_draws(x)
  sum_x2 <- summarize_draws(x)
  expect_equal(sum_x, sum_x2)
  sum_x3 <- summary(x)
  expect_equal(sum_x, sum_x3)
})

test_that("summarise_draws errors if name 'variable' is used", {
  x <- example_draws()
  variable <- function(x) mean(x)
  expect_error(
    summarise_draws(x, "variable"),
    "Name 'variable' is reserved in 'summarise_draws'"
  )
})

test_that("summarise_draws default method works", {
  expect_identical(
    summarise_draws(matrix(1:20, 10, 2)),
    summarise_draws(as_draws_matrix(matrix(1:20, 10, 2)))
  )
})

test_that("summarise_draws doesn't error for empty draws", {
  expect_identical(
    summarise_draws(empty_draws_array()),
    empty_draws_summary()
  )
})

test_that("summarise_draws and summary work for rvars", {
  d <- as_draws_rvars(example_draws())
  d_theta <- draws_rvars(x = d$theta)
  names(d_theta) <- "d$theta"
  ref <- summarise_draws(d_theta)

  expect_identical(summarise_draws(d$theta), ref)
  expect_identical(summary(d$theta), ref)
})

test_that("summarise_draws warns if all variable names are reserved", {
  x <- subset_draws(as_draws_df(example_draws()), variable = "mu")
  variables(x) <- ".log_weight"
  expect_warning(summarize_draws(x), "no variables with unreserved names")
})

test_that(paste(
  "multicore summarise_draws is identical to single-core summarise_draws",
  "including if some chunks contain no variables"
), {
  set.seed(1)
  cores <- 2
  nc <- 4

  n <- 20
  test_array <- array(data = rnorm(1000*nc*n), dim = c(1000,nc,n))
  x <- as_draws_array(test_array)
  sum_x <- summarise_draws(x)
  parsum_x <- summarise_draws(x, .cores = cores)
  expect_equal(sum_x, parsum_x)

  dimnames(x)$variable[2] <- reserved_variables()[1]
  sum_x <- summarise_draws(x)
  parsum_x <- summarise_draws(x, .cores = cores)
  expect_equal(sum_x, parsum_x)

  # test that externally defined summary functions can be found
  mean2 <- function(x) sum(x) / length(x)
  sum_x <- summarise_draws(x, mean2)
  parsum_x <- summarise_draws(x, mean2, .cores = cores)
  expect_identical(sum_x, parsum_x)

  n <- 2
  test_array <- array(data = rnorm(1000*nc*n), dim = c(1000,nc,n))
  x <- as_draws_array(test_array)
  sum_x <- summarise_draws(x)
  parsum_x <- summarise_draws(x, .cores = cores)
  expect_identical(sum_x, parsum_x)

  dimnames(x)$variable[2] <- reserved_variables()[1]
  sum_x <- summarise_draws(x)
  parsum_x <- summarise_draws(x, .cores = cores)
  expect_identical(sum_x, parsum_x)

  n <- 1
  test_array <- array(data = rnorm(1000*nc*n), dim = c(1000,nc,n))
  x <- as_draws_array(test_array)
  sum_x <- summarise_draws(x)
  parsum_x <- summarise_draws(x, .cores = cores)
  expect_identical(sum_x, parsum_x)

  dimnames(x)$variable[1] <- reserved_variables()[1]
  suppressWarnings(sum_x <- summarise_draws(x))
  suppressWarnings(parsum_x <- summarise_draws(x, .cores = cores))
  expect_identical(sum_x, parsum_x)
})

test_that("summarise_draws supports tibble::num correctly", {
  x <- example_draws()
  expect_output(
    print(summarise_draws(x, .num_args = list(sigfig = 2, notation="dec"))),
    "<dec:2>"
  )
})

test_that("summarise_draws errors for invalid cores specification", {
  x <- example_draws()
  expect_error(
    summarise_draws(x, .cores = -1),
    "'.cores' must be a positive integer"
  )
  expect_error(
    summarise_draws(x, .cores = NULL),
    "Cannot coerce '.cores' to a single integer value"
  )
})


test_that("summarise_draws works with variance()", {
  draws_array <- as_draws_array(example_draws())
  draws_matrix <- as_draws_matrix(draws_array)
  draws_df <- as_draws_df(draws_array)
  draws_list <- as_draws_list(draws_array)
  draws_rvars <- as_draws_rvars(draws_array)

  ref <- data.frame(
    variable = variables(draws_array),
    variance = as.vector(
      apply(draws_array, 3, function(x) var(as.vector(x)))
    ),
    stringsAsFactors = FALSE
  )
  class(ref) <- posterior:::class_draws_summary()
  attr(ref, "num_args") <- list()

  expect_equal(summarise_draws(draws_array, variance), ref)
  expect_equal(summarise_draws(draws_matrix, variance), ref)
  expect_equal(summarise_draws(draws_df, variance), ref)
  expect_equal(summarise_draws(draws_list, variance), ref)
  expect_equal(summarise_draws(draws_rvars, variance), ref)

  # for consistency, draws_matrix and draws_array
  # have the same implementation of variance()
  expect_equal(variance(draws_array), var(as.vector(draws_array)))
  expect_equal(variance(draws_matrix), var(as.vector(draws_matrix)))
})


test_that("draws summaries can be converted to data frames", {
  draws_matrix <- as_draws_matrix(example_draws())

  ref <- data.frame(
    variable = variables(draws_matrix),
    mean = as.vector(colMeans(draws_matrix)),
    # include quantiles here to ensure we test on some negative numbers,
    # which would cause problems if formatted prematurely (#275)
    q5 = as.vector(apply(draws_matrix, 2, quantile, probs = 0.05)),
    q95 = as.vector(apply(draws_matrix, 2, quantile, probs = 0.95)),
    stringsAsFactors = FALSE
  )
  attr(ref, "num_args") <- list()

  expect_equal(as.data.frame(summarise_draws(draws_matrix, mean, quantile2)), ref)
})
