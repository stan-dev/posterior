test_that("summarise_draws works correctly", {
  x <- as_draws_df(example_draws())
  sum_x <- summarise_draws(x)
  expect_true(all(default_convergence_measures() %in% names(sum_x)))
  expect_true(all(c("q5", "q95") %in% names(sum_x)))
  expect_equal(sum_x$variable, variables(x))
  expect_equal(mean(x$mu), sum_x$mean[sum_x$variable == "mu"])

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

test_that("multicore summarise_draws is identical to single-core summarise_draws 
          including if some chunks contain no variables", {
            set.seed(1)
            nc <- 4
            n <- 2
            test_array <- array(data = rnorm(1000*nc*n), dim = c(1000,nc,n))
            x <- as_draws_array(test_array)
            sum_x <- summarise_draws(x)
            parsum_x <- summarise_draws(x, cores = 2)
            expect_identical(sum_x, parsum_x)
            
            dimnames(x)$variable[2] <- reserved_variables()[1]
            sum_x <- summarise_draws(x)
            parsum_x <- summarise_draws(x, cores = 2)
            expect_identical(sum_x, parsum_x)
            
            n <- 1
            test_array <- array(data = rnorm(1000*nc*n), dim = c(1000,nc,n))
            x <- as_draws_array(test_array)
            sum_x <- summarise_draws(x)
            parsum_x <- summarise_draws(x, cores = 2)
            expect_identical(sum_x, parsum_x)
            
            dimnames(x)$variable[1] <- reserved_variables()[1]
            suppressWarnings(sum_x <- summarise_draws(x))
            suppressWarnings(parsum_x <- summarise_draws(x, cores = 2))
            expect_identical(sum_x, parsum_x)
})
