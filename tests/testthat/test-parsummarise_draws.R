test_that("parsummarise_draws works correctly", {
  x <- as_draws_df(example_draws())
  sum_x <- summarise_draws(x)
  parsum_x <- parsummarise_draws(x)
  expect_identical(sum_x, parsum_x)
  parsum2_x <- parsummarise_draws(x, cores = 2)
  expect_identical(sum_x, parsum2_x)
  
  sum_x <- summarise_draws(x, mean, median)
  parsum1_x <- parsummarise_draws(x, mean, median)
  parsum2_x <- parsummarise_draws(x, mean, median)
  expect_equal(sum_x, parsum_x)
  expect_equal(sum_x, parsum_x)
  
  
  sum_x <- summarise_draws(x, default_mcse_measures())
  parsum_x <- parsummarise_draws(x, default_mcse_measures())
  expect_identical(sum_x, parsum_x)
  
  sum_x <- summarise_draws(x, ~quantile(.x, probs = c(0.4, 0.6)))
  parsum_x <- parsummarise_draws(x, ~quantile(.x, probs = c(0.4, 0.6)))
  expect_identical(sum_x, parsum_x)
  
  x[1, 1] <- NA
  sum_x <- summarise_draws(x)
  parsum_x <- parsummarise_draws(x)
  expect_identical(sum_x, parsum_x)
})

test_that("aliases of parsummarise_draws work", {
  x <- as_draws_array(example_draws())
  parsum_x <- parsummarise_draws(x)
  parsum_x2 <- parsummarize_draws(x)
  expect_equal(parsum_x, parsum_x2)
  sum_x3 <- summary(x)
  expect_equal(parsum_x, sum_x3)
})

test_that("parsummarise_draws errors if name 'variable' is used", {
  x <- example_draws()
  variable <- function(x) mean(x)
  expect_error(
    parsummarise_draws(x, "variable"),
    "Name 'variable' is reserved in 'summarise_draws'"
  )
})

test_that("summarise_draws default method works", {
  expect_identical(
    summarise_draws(matrix(1:20, 10, 2)),
    parsummarise_draws(as_draws_matrix(matrix(1:20, 10, 2)))
  )
})

test_that("summarise_draws doesn't error for empty draws", {
  expect_identical(
    parsummarise_draws(empty_draws_array()),
    empty_draws_summary()
  )
})

test_that("summarise_draws and summary work for rvars", {
  d <- as_draws_rvars(example_draws())
  d_theta <- draws_rvars(x = d$theta)
  names(d_theta) <- "d$theta"
  ref <- summarise_draws(d_theta)
  par_ref <- parsummarise_draws(d_theta)
  expect_identical(ref, par_ref)
})

test_that("summarise_draws warns if all variable names are reserved", {
  x <- subset_draws(as_draws_df(example_draws()), variable = "mu")
  variables(x) <- ".log_weight"
  expect_warning(parsummarize_draws(x), "no variables with unreserved names")
})

test_that("parsummarise_draws is identical to summarise_draws if some chunks 
          contain no variables", {
  set.seed(1)
  nc <- 4
  n <- 3
  test_array <- array(data = rnorm(1000*nc*n), dim = c(1000,nc,n))
  x <- as_draws_array(test_array)
  sum_x <- summarise_draws(x)
  parsum_x <- parsummarise_draws(x, cores = 4)
  expect_identical(sum_x, parsum_x)
  
  dimnames(x)$variable[3] <- reserved_variables()[1]
  sum_x <- summarise_draws(x)
  parsum_x <- parsummarise_draws(x, cores = 4)
  expect_identical(sum_x, parsum_x)
  
  n <- 1
  test_array <- array(data = rnorm(1000*nc*n), dim = c(1000,nc,n))
  x <- as_draws_array(test_array)
  sum_x <- summarise_draws(x)
  parsum_x <- parsummarise_draws(x, cores = 4)
  expect_identical(sum_x, parsum_x)
  
  dimnames(x)$variable[1] <- reserved_variables()[1]
  sum_x <- summarise_draws(x)
  parsum_x <- parsummarise_draws(x, cores = 4)
  expect_identical(sum_x, parsum_x)
})
