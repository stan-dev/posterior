test_that("extract_variable_matrix works the same for different formats", {
  draws_array <- as_draws_array(example_draws())
  mu_array <- extract_variable(draws_array, "mu")

  draws_df <- as_draws_df(example_draws())
  mu_df <- extract_variable(draws_df, "mu")
  expect_equal(mu_df, mu_array)

  draws_list <- as_draws_list(example_draws())
  mu_list <- extract_variable(draws_list, "mu")
  expect_equal(mu_list, mu_array)

  draws_matrix <- as_draws_matrix(example_draws())
  mu_matrix <- extract_variable(draws_matrix, "mu")
  expect_equal(as.vector(mu_matrix), as.vector(mu_array))
})
