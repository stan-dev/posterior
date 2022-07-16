test_that("subset_draws works correctly for draws_matrix objects", {
  x <- as_draws_matrix(example_draws())
  x_sub <- subset_draws(x, variable = c("mu", "tau"), iteration = 5:10)
  x_sub2 <- x[c(5:10, 105:110, 205:210, 305:310), c("mu", "tau")]
  expect_equal(x_sub, x_sub2, check.attributes = FALSE)
  expect_equal(iteration_ids(x_sub), 1:6)

  x_sub <- subset_draws(x, draw = c(2, 2, 4, 4), unique = FALSE)
  expect_equal(niterations(x_sub), 4)
  expect_equivalent(x_sub[1, ], x_sub[2, ])

  x <- weight_draws(x, rep(1, ndraws(x)))
  x_sub <- subset_draws(x, variable = "mu")
  expect_equal(variables(x_sub, reserved = TRUE), c("mu", ".log_weight"))
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

  expect_message(
    x_sub <- subset_draws(x, draw = c(1, 200, 10)),
    "Merging chains in order to subset via 'draw'"
  )
  expect_equal(niterations(x_sub), 3)

  x <- weight_draws(x, rep(1, ndraws(x)))
  x_sub <- subset_draws(x, variable = "mu")
  expect_equal(variables(x_sub, reserved = TRUE), c("mu", ".log_weight"))
})

test_that("subset_draws works correctly for draws_df objects", {
  x <- as_draws_df(example_draws())
  x_sub <- subset_draws(x, variable = c("mu", "tau"), iteration = 5:10, chain = 3:4)
  expect_equal(x$mu[x$.iteration %in% 5:10 & x$.chain %in% 3:4], x_sub$mu)
  expect_equal(iteration_ids(x_sub), 1:6)
  expect_equal(chain_ids(x_sub), 1:2)

  x_sub <- subset_draws(x, draw = c(5, 5, 6), unique = FALSE)
  expect_equal(ndraws(x_sub), 3)
  expect_equal(x_sub$mu[1], x_sub$mu[2])

  x <- weight_draws(x, rep(1, ndraws(x)))
  x_sub <- subset_draws(x, variable = "mu")
  expect_equal(names(x_sub), c("mu", ".log_weight", ".chain", ".iteration", ".draw"))
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

  expect_message(
    x_sub <- subset_draws(x, draw = c(1, 200, 10)),
    "Merging chains in order to subset via 'draw'"
  )
  expect_equal(niterations(x_sub), 3)

  x <- weight_draws(x, rep(1, ndraws(x)))
  x_sub <- subset_draws(x, variable = "mu")
  expect_equal(variables(x_sub, reserved = TRUE), c("mu", ".log_weight"))
})

test_that("subset_draws works correctly for draws_rvars objects", {
  x <- as_draws_rvars(example_draws())
  x_sub <- subset_draws(x, variable = c("mu"), iteration = 5:10, chain = 3:4)
  expect_equal(variables(x_sub), "mu")
  expect_equal(iteration_ids(x_sub), 1:6)
  expect_equal(chain_ids(x_sub), 1:2)

  x_sub <- subset_draws(x, iteration = c(1, 1, 2), unique = FALSE)
  expect_equal(niterations(x_sub), 3)
  expect_equal(draws_of(x_sub[[1]]$mu)[1], draws_of(x_sub[[1]]$mu)[2])

  expect_message(
    x_sub <- subset_draws(x, draw = c(1, 200, 10)),
    "Merging chains in order to subset via 'draw'"
  )
  expect_equal(niterations(x_sub), 3)

  x <- weight_draws(x, rep(1, ndraws(x)))
  x_sub <- subset_draws(x, variable = "mu")
  expect_equal(variables(x_sub, reserved = TRUE), c("mu", ".log_weight"))
})

test_that("subset_draws works correctly for rvar objects", {
  x <- as_draws_rvars(example_draws())$theta

  expect_error(subset_draws(x, variable = "mu"), "Cannot subset an rvar by variable")

  x_sub <- subset_draws(x, iteration = c(1, 1, 2), unique = FALSE)
  expect_equal(niterations(x_sub), 3)
  expect_equal(draws_of(x_sub)[1], draws_of(x_sub)[2])

  expect_message(
    x_sub <- subset_draws(x, draw = c(1, 200, 10)),
    "Merging chains in order to subset via 'draw'"
  )
  expect_equal(niterations(x_sub), 3)
})

test_that("variables can be subsetted via regular expressions", {
  x <- as_draws_df(example_draws())
  x_sub <- subset_draws(x, variable = c("theta\\[", "m"), regex = TRUE)
  expect_equal(variables(x_sub), c(paste0("theta[", 1:8, "]"), "mu"))

  # do the same thing using the 'subset' alias
  x_sub <- subset(x, variable = c("theta\\[", "m"), regex = TRUE)
  expect_equal(variables(x_sub), c(paste0("theta[", 1:8, "]"), "mu"))
})

test_that("variables can be subsetted via non-scalar selection", {
  x <- as_draws_df(example_draws())
  x_sub <- subset_draws(x, variable = "theta")
  expect_equal(variables(x_sub), c(paste0("theta[", 1:8, "]")))
})

test_that("subset_draws speed is tolerable with many variables", {
  # some machines will be slower and so this test is unreliable on CRAN
  skip_on_cran()
  x <- as_draws_matrix(matrix(rnorm(10 * 300000), nrow = 10))
  tt <- system.time(x2 <- subset_draws(x, colnames(x)))
  expect_equal(x, x2)
  expect_lt(tt[["elapsed"]], 1)
})

test_that("subset_draws errors if selecting missing variables", {
  x <- as_draws_matrix(example_draws())
  expect_error(
    subset_draws(x, variable = c("theta[2]", "X", "theta[3]", "Y")),
    "The following variables are missing in the draws object: {'X', 'Y'}",
    fixed = TRUE
  )
})

test_that("subset_draws preserves variable order", {
  x <- as_draws_matrix(example_draws())
  x <- subset_draws(x, variable = c("theta[2]", "theta[1]"))
  expect_equal(variables(x), c("theta[2]", "theta[1]"))
})

test_that("subset_draws preserves variable order with vectors", {
  x <- as_draws_matrix(example_draws())
  theta_names <- paste0("theta[", 1:8, "]")
  # Expect variables to be returned in the order listed:
  v1 <- variables(subset_draws(x, variable = c("theta", "mu")))
  expect_equal(v1, c(theta_names, "mu"))
  v2 <- variables(subset_draws(x, variable = c("mu", "theta")))
  expect_equal(v2, c("mu", theta_names))
  v3 <- variables(subset_draws(x, variable = c("mu", "theta", "tau")))
  expect_equal(v3, c("mu", theta_names, "tau"))
  # No duplication:
  v4 <- variables(subset_draws(x, variable = c("mu", "mu", "theta")))
  expect_equal(v4, c("mu", theta_names))
  v5 <- variables(subset_draws(x, variable = c("mu", "theta", "theta")))
  expect_equal(v5, c("mu", theta_names))
  v6 <- variables(subset_draws(x, variable = c("theta", "mu", "theta")))
  expect_equal(v6, c(theta_names, "mu"))

  # Output is sorted numerically, not alphabetically
  x2 <- as_draws_matrix(matrix(rep.int(1, 11 * 3), ncol = 11))
  colnames(x2) <- paste0("a[",1:11,"]")
  v7 <- variables(subset_draws(x2, variable = "a"))
  expect_equal(v7, colnames(x2))
})

test_that("non-unique subsetting for draws_df same as doing it with draws_list", {
  x_df <- as_draws_df(example_draws())
  x_list <- as_draws_list(example_draws())

  x_df_sub <- subset_draws(x_df, chain = c(1,1,2), iteration = c(1:2, 1:50),
                           unique = FALSE)
  x_list_sub <- subset_draws(x_list, chain = c(1,1,2), iteration = c(1:2, 1:50),
                             unique = FALSE)
  expect_equal(x_df_sub, as_draws_df(x_list_sub))
})
