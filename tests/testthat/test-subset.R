test_that("subset works correctly for draws_matrix objects", {
  x <- as_draws_matrix(example_draws())
  x_sub <- subset(x, variable = c("mu", "tau"), iteration = 5:10)
  expect_equal(x[5:10, c("mu", "tau")], x_sub, check.attributes = FALSE)
  expect_equal(draw_ids(x_sub), 1:6)
})

test_that("subset works correctly for draws_array objects", {
  x <- as_draws_array(example_draws())
  x_sub <- subset(x, variable = c("mu", "tau"), iteration = 5:10, chain = 3:4)
  expect_equal(x[5:10, 3:4, c("mu", "tau")], x_sub, check.attributes = FALSE)
  expect_equal(iteration_ids(x_sub), 1:6)
  expect_equal(chain_ids(x_sub), 1:2)
})

test_that("subset works correctly for draws_df objects", {
  x <- as_draws_df(example_draws())
  x_sub <- subset(x, variable = c("mu", "tau"), iteration = 5:10, chain = 3:4)
  expect_equal(x$mu[x$.iteration %in% 5:10 & x$.chain %in% 3:4], x_sub$mu)
  expect_equal(iteration_ids(x_sub), 1:6)
  expect_equal(chain_ids(x_sub), 1:2)
})

test_that("subset works correctly for draws_list objects", {
  x <- as_draws_list(example_draws())
  x_sub <- subset(x, variable = c("theta[1]"), iteration = 5:10, chain = 3:4)
  expect_equal(variables(x_sub), "theta[1]")
  expect_equal(iteration_ids(x_sub), 1:6)
  expect_equal(chain_ids(x_sub), 1:2)
})

test_that("variables can be subsetted via regular expressions", {
  x <- as_draws_df(example_draws())
  x_sub <- subset(x, variable = c("theta\\[", "m"), regex = TRUE)
  expect_equal(variables(x_sub), c(paste0("theta[", 1:8, "]"), "mu"))
})

test_that("thin_draws works correctly", {
  x <- as_draws_array(example_draws())
  expect_equal(niterations(thin_draws(x, 5L)), niterations(x) / 5)
  expect_equal(x, thin_draws(x, thin = 1L))
  expect_error(thin_draws(x, -1), "'thin' must be a positive integer")
  expect_error(thin_draws(x, 1000), "'thin' must be smaller than")
})
