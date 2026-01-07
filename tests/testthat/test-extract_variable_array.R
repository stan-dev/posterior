test_that("extract_variable_array works the same for different formats", {
  draws <- list(
    array = as_draws_array(example_draws()),
    df = as_draws_df(example_draws()),
    list = as_draws_list(example_draws()),
    matrix = as_draws_matrix(example_draws()),
    rvars = as_draws_rvars(example_draws())
  )
  
  for (type in names(draws)) {
    mu <- extract_variable_array(draws[[type]], variable = "mu")
    expect_equal(length(dim(mu)), 3)
    expect_equal(dim(mu)[1], niterations(draws[[type]]))
    expect_equal(dim(mu)[2], nchains(draws[[type]]))
    expect_equal(dim(mu)[3], 1)
    
    theta <- extract_variable_array(draws[[type]], variable = "theta")
    expect_equal(length(dim(theta)), 3)
    expect_equal(dim(theta)[1], niterations(draws[[type]]))
    expect_equal(dim(theta)[2], nchains(draws[[type]]))
    expect_equal(dim(theta)[3], 8)
  }
})

test_that("extract_variable_array with_chains parameter works", {
  x <- example_draws()
  
  # Test with_chains = TRUE (default)
  mu_with_chains <- extract_variable_array(x, "mu", with_chains = TRUE)
  expect_equal(dim(mu_with_chains)[1], niterations(x))
  expect_equal(dim(mu_with_chains)[2], nchains(x))
  
  # Test with_chains = FALSE
  mu_no_chains <- extract_variable_array(x, "mu", with_chains = FALSE)
  expect_equal(dim(mu_no_chains)[1], ndraws(x))
  expect_equal(dim(mu_no_chains)[2], 1)  # scalar variable
  
  # Test that the draws are the same, just reshaped
  expect_equal(c(mu_with_chains), c(mu_no_chains))
})

test_that("extract_variable_array with_chains works for different formats", {
  draws <- list(
    array = as_draws_array(example_draws()),
    df = as_draws_df(example_draws()),
    list = as_draws_list(example_draws()),
    matrix = as_draws_matrix(example_draws()),
    rvars = as_draws_rvars(example_draws())
  )
  
  for (type in names(draws)) {
    # Test with chains
    result_with <- extract_variable_array(draws[[type]], "mu", with_chains = TRUE)
    expect_equal(dim(result_with)[1], niterations(draws[[type]]))
    expect_equal(dim(result_with)[2], nchains(draws[[type]]))
    
    # Test without chains 
    result_without <- extract_variable_array(draws[[type]], "mu", with_chains = FALSE)
    expect_equal(dim(result_without)[1], ndraws(draws[[type]]))
    
    # Values should be the same
    expect_equal(c(result_with), c(result_without))
  }
})

test_that("extract_variable_array with_chains works with indexed variables", {
  x <- example_draws(example = "multi_normal")
  
  # Test specific indexed variable
  mu1_with <- extract_variable_array(x, "mu[1]", with_chains = TRUE)
  mu1_without <- extract_variable_array(x, "mu[1]", with_chains = FALSE)
  
  # With chains: should be 3D array (niterations, nchains, 1)
  expect_equal(length(dim(mu1_with)), 3)
  expect_equal(dim(mu1_with)[3], 1)
  
  # Without chains: should be 2D array (ndraws, 1)  
  expect_equal(length(dim(mu1_without)), 2)
  expect_equal(dim(mu1_without)[2], 1)
  
  # Values should match
  expect_equal(c(mu1_with), c(mu1_without))
})

test_that("extract_list_of_variable_arrays passes with_chains parameter", {
  x <- example_draws()
  
  # Test that with_chains is passed through to extract_variable_array
  vars_with <- extract_list_of_variable_arrays(x, c("mu", "tau"), with_chains = TRUE)
  vars_without <- extract_list_of_variable_arrays(x, c("mu", "tau"), with_chains = FALSE)
  
  # Check dimensions
  expect_equal(dim(vars_with$mu)[1], niterations(x))
  expect_equal(dim(vars_with$mu)[2], nchains(x))
  expect_equal(dim(vars_without$mu)[1], ndraws(x))
  
  # Values should be the same
  expect_equal(c(vars_with$mu), c(vars_without$mu))
  expect_equal(c(vars_with$tau), c(vars_without$tau))
  
  # Compare with individual extractions
  expect_equal(vars_with$mu, extract_variable_array(x, "mu", with_chains = TRUE))
  expect_equal(vars_without$mu, extract_variable_array(x, "mu", with_chains = FALSE))
})

test_that("extract_variable_array preserves factor levels with indexed variables", {
  # Create rvar_factor with known levels (vector, not matrix)
  mu <- rvar_factor(
    array(sample(c("low", "med", "high"), 200, replace = TRUE), dim = c(100, 2))
  )
  draws <- as_draws_rvars(list(mu = mu))
  
  # Extract indexed variable with both with_chains settings
  mu1_chains <- extract_variable_array(draws, "mu[1]", with_chains = TRUE)
  mu1_nochains <- extract_variable_array(draws, "mu[1]", with_chains = FALSE)
  
  # Both should be factors
  expect_s3_class(mu1_chains, "factor")
  expect_s3_class(mu1_nochains, "factor")
  
  # Both should have the same levels
  expect_equal(levels(mu1_chains), levels(mu1_nochains))
  expect_equal(levels(mu1_chains), c("high", "low", "med"))
  
  # Values should be the same
  expect_equal(c(mu1_chains), c(mu1_nochains))
})

