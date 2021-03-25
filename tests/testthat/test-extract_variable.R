test_that("extract_variable works the same for different formats", {
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

  draws_rvars <- as_draws_rvars(example_draws())
  mu_matrix <- extract_variable(draws_rvars, "mu")
  expect_equal(as.vector(mu_matrix), as.vector(mu_array))
})

test_that("extract_variable works for draws_rvars on an indexed variable", {
  draws_array <- as_draws_array(example_draws())
  theta1_array <- extract_variable(draws_array, "theta[1]")

  draws_rvars <- as_draws_rvars(example_draws())
  theta1_matrix <- extract_variable(draws_rvars, "theta[1]")
  expect_equal(as.vector(theta1_matrix), as.vector(theta1_array))

  expect_error(extract_variable(draws_rvars, "theta"), "Cannot extract non-scalar value")
})

test_that("extract_variable default method works", {
  # it should convert matrix to draws object
  x <- matrix(1:20, nrow = 10, ncol = 2)
  colnames(x) <- c("A", "B")
  expect_equal(extract_variable(x, "A"), 1:10)
  expect_equal(extract_variable(x, "B"), 11:20)
})
