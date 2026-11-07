test_that("multicore summarise_draws is identical to single-core summarise_draws", {
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
})
