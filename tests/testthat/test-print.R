test_that("print.draws_matrix runs without errors", {
  x <- as_draws_matrix(example_draws())
  expect_output(print(x), "A draws_matrix: 400 draws, and 10 variables")

  x <- weight_draws(x, rep(1, ndraws(x)))
  expect_output(print(x), "hidden reserved variables \\{'\\.log_weight'\\}")
})

test_that("print.draws_array runs without errors", {
  x <- as_draws_array(example_draws())
  expect_output(print(x),
    "A draws_array: 100 iterations, 4 chains, and 10 variables"
  )

  x <- weight_draws(x, rep(1, ndraws(x)))
  expect_output(print(x), "hidden reserved variables \\{'\\.log_weight'\\}")
})

test_that("print.draws_df runs without errors", {
  x <- as_draws_df(example_draws())
  expect_output(print(x),
    "A draws_df: 100 iterations, 4 chains, and 10 variables"
  )

  x <- weight_draws(x, rep(1, ndraws(x)))
  expect_output(print(x), "'\\.log_weight'")
})

test_that("print.draws_list runs without errors", {
  x <- as_draws_list(example_draws())
  expect_output(print(x),
    "A draws_list: 100 iterations, 4 chains, and 10 variables"
  )

  x <- weight_draws(x, rep(1, ndraws(x)))
  expect_output(print(x), "hidden reserved variables \\{'\\.log_weight'\\}")
})

test_that("print.draws_rvar runs without errors", {
  x <- as_draws_rvar(example_draws())
  expect_output(print(x),
    "A draws_rvar: 100 iterations, 4 chains, and 3 variables"
  )

  x <- weight_draws(x, rep(1, ndraws(x)))
  expect_output(print(x), "hidden reserved variables \\{'\\.log_weight'\\}")
})
