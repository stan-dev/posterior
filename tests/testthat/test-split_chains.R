test_that("split_chains(<draws_rvar>) works correctly", {
  x_array <- array(1:48, dim = c(4, 3, 4))
  x_rvar <- rvar(x_array, with_chains = TRUE)
  x_draws <- draws_rvars(x = x_rvar)

  x_split_array <- abind::abind(x_array[1:2,,], x_array[3:4,,], along = 2)
  x_split_rvar <- rvar(x_split_array, with_chains = TRUE)
  x_split_draws <- draws_rvars(x = x_split_rvar)

  expect_equal(split_chains(x_draws), x_split_draws)
})
