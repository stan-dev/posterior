test_that("multicore summarise_draws finds externally defined summary functions", {
  set.seed(1)
  cores <- 2
  nc <- 4

  n <- 20
  test_array <- array(data = rnorm(1000*nc*n), dim = c(1000,nc,n))
  x <- as_draws_array(test_array)
  dimnames(x)$variable[2] <- reserved_variables()[1]

  mean2 <- function(x) sum(x) / length(x)
  sum_x <- summarise_draws(x, mean2)
  parsum_x <- summarise_draws(x, mean2, .cores = cores)
  expect_identical(sum_x, parsum_x)
})
