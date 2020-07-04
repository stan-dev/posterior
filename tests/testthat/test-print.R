test_that("print.draws_matrix runs without errors", {
  x <- as_draws_matrix(example_draws())
  expect_output(print(x), "A draws_matrix: 400 draws, and 10 variables")
})

test_that("print.draws_array runs without errors", {
  x <- as_draws_array(example_draws())
  expect_output(print(x),
    "A draws_array: 100 iterations, 4 chains, and 10 variables"
  )
})

test_that("print.draws_df runs without errors", {
  x <- as_draws_df(example_draws())
  expect_output(print(x),
    "A draws_df: 100 iterations, 4 chains, and 10 variables"
  )
})

test_that("print.draws_list runs without errors", {
  x <- as_draws_list(example_draws())
  expect_output(print(x),
    "A draws_list: 100 iterations, 4 chains, and 10 variables"
  )
})
