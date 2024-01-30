test_that("extract_variable_array works the same for different formats", {
  draws <- list(
    array = as_draws_array(example_draws()),
    df = as_draws_df(example_draws()),
    list = as_draws_list(example_draws()),
    matrix = as_draws_matrix(example_draws()),
    rvars = as_draws_rvars(example_draws())
  )

  mu = draws_of(draws$rvars$mu, with_chains = TRUE)
  theta1 = draws_of(draws$rvars$theta[1], with_chains = TRUE)
  theta = draws_of(draws$rvars$theta, with_chains = TRUE)

  for (type in names(draws)) {
    expect_equal(extract_variable_array(draws[[!!type]], "mu"), mu)
    expect_equal(extract_variable_array(draws[[!!type]], "theta[1]"), theta1)
    expect_equal(extract_variable_array(draws[[!!type]], "theta"), theta)
  }

  # rvars are converted to draws on the way in, thus the variable name to
  # use to extract the array is the generic "x"
  expect_equal(extract_variable_array(draws$rvars$mu, "x"), mu)
})

test_that("extract_variable_array works for factor types", {
  draws_rvars <- draws_rvars(y = rvar(1:26, nchains = 2), x = rvar_factor(letters, nchains = 2))
  x_array <- array(1:26, dim = c(13, 2, 1), dimnames = list(NULL))
  levels(x_array) <- letters
  class(x_array) <- "factor"

  expect_equal(extract_variable_array(draws_rvars, "x"), x_array)
  expect_equal(extract_variable_array(as_draws_df(draws_rvars), "x"), x_array)
  expect_equal(extract_variable_array(as_draws_list(draws_rvars), "x"), x_array)
})
