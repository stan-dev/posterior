test_that("subset_draws works correctly for draws_matrix objects", {
  x <- as_draws_matrix(example_draws())
  x_sub <- subset_draws(x, variable = c("mu", "tau"), iteration = 5:10)
  expect_equal(x[5:10, c("mu", "tau")], x_sub, check.attributes = FALSE)
  expect_equal(draw_ids(x_sub), 1:6)

  x_sub <- subset_draws(x, draw = c(2, 2, 4, 4), unique = FALSE)
  expect_equal(niterations(x_sub), 4)
  expect_equivalent(x_sub[1, ], x_sub[2, ])
})

test_that("subset_draws works correctly for draws_array objects", {
  x <- as_draws_array(example_draws())

  x_sub <- subset_draws(x, variable = c("mu", "tau"), iteration = 5:10, chain = 3:4)
  expect_equal(x[5:10, 3:4, c("mu", "tau")], x_sub, check.attributes = FALSE)
  expect_equal(iteration_ids(x_sub), 1:6)
  expect_equal(chain_ids(x_sub), 1:2)

  x_sub <- subset_draws(x, chain = c(1, 1), unique = FALSE)
  expect_equal(nchains(x_sub), 2)
  expect_equivalent(x_sub[, 1, ], x_sub[, 2, ])
})

test_that("subset works correctly for draws_df objects", {
  x <- as_draws_df(example_draws())
  x_sub <- subset_draws(x, variable = c("mu", "tau"), iteration = 5:10, chain = 3:4)
  expect_equal(x$mu[x$.iteration %in% 5:10 & x$.chain %in% 3:4], x_sub$mu)
  expect_equal(iteration_ids(x_sub), 1:6)
  expect_equal(chain_ids(x_sub), 1:2)

  x_sub <- subset_draws(x, draw = c(5, 5, 6), unique = FALSE)
  expect_equal(ndraws(x_sub), 3)
  expect_equal(x_sub$mu[1], x_sub$mu[2])
})

test_that("subset_draws works correctly for draws_list objects", {
  x <- as_draws_list(example_draws())
  x_sub <- subset_draws(x, variable = c("theta[1]"), iteration = 5:10, chain = 3:4)
  expect_equal(variables(x_sub), "theta[1]")
  expect_equal(iteration_ids(x_sub), 1:6)
  expect_equal(chain_ids(x_sub), 1:2)

  x_sub <- subset_draws(x, iteration = c(1, 1, 2), unique = FALSE)
  expect_equal(niterations(x_sub), 3)
  expect_equal(x_sub[[1]]$mu[1], x_sub[[1]]$mu[2])
})

test_that("variables can be subsetted via regular expressions", {
  x <- as_draws_df(example_draws())
  x_sub <- subset_draws(x, variable = c("theta\\[", "m"), regex = TRUE)
  expect_equal(variables(x_sub), c(paste0("theta[", 1:8, "]"), "mu"))

  # do the same thing using the 'subset' alias
  x_sub <- subset(x, variable = c("theta\\[", "m"), regex = TRUE)
  expect_equal(variables(x_sub), c(paste0("theta[", 1:8, "]"), "mu"))
})
