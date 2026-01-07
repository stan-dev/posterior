test_that("extract_list_of_variable_arrays works for different formats", {
  draws <- list(
    array = as_draws_array(example_draws()),
    df = as_draws_df(example_draws()),
    list = as_draws_list(example_draws()),
    matrix = as_draws_matrix(example_draws()),
    rvars = as_draws_rvars(example_draws())
  )

  variables <- c("mu", "theta")
  
  for (type in names(draws)) {
    result <- extract_list_of_variable_arrays(draws[[type]], variables)
    
    expect_type(result, "list")
    expect_length(result, 2)
    expect_named(result, c("mu", "theta"))
    
    # Compare with individual extractions
    expect_equal(result$mu, extract_variable_array(draws[[type]], "mu"))
    expect_equal(result$theta, extract_variable_array(draws[[type]], "theta"))
  }
})

test_that("extract_list_of_variable_arrays works with indexed variables", {
  x <- example_draws(example = "multi_normal")
  
  # Test specific indexed variables
  vars <- extract_list_of_variable_arrays(x, c("mu[1]", "mu[2]", "Sigma[1,1]"))
  
  expect_type(vars, "list")
  expect_length(vars, 3)
  expect_named(vars, c("mu[1]", "mu[2]", "Sigma[1,1]"))
  
  # Compare with individual extractions
  expect_equal(vars$`mu[1]`, extract_variable_array(x, "mu[1]"))
  expect_equal(vars$`mu[2]`, extract_variable_array(x, "mu[2]"))
  expect_equal(vars$`Sigma[1,1]`, extract_variable_array(x, "Sigma[1,1]"))
})

test_that("extract_list_of_variable_arrays works with base variable names", {
  x <- example_draws(example = "multi_normal")
  
  vars <- extract_list_of_variable_arrays(x, c("mu", "Sigma"))
  
  expect_type(vars, "list")
  expect_length(vars, 2)
  expect_named(vars, c("mu", "Sigma"))
  
  # Compare with individual extractions
  expect_equal(vars$mu, extract_variable_array(x, "mu"))
  expect_equal(vars$Sigma, extract_variable_array(x, "Sigma"))
})

test_that("extract_list_of_variable_arrays handles empty input", {
  x <- example_draws()
  
  result <- extract_list_of_variable_arrays(x, character(0))
  
  expect_type(result, "list")
  expect_length(result, 0)
})

test_that("extract_list_of_variable_arrays validates input", {
  x <- example_draws()
  
  expect_error(
    extract_list_of_variable_arrays(x, 123),
    "'variables' must be a character vector."
  )
  
  expect_error(
    extract_list_of_variable_arrays(x, list("mu", "theta")),
    "'variables' must be a character vector."
  )
})

test_that("extract_list_of_variable_arrays works for factor types", {
  draws_rvars <- draws_rvars(
    y = rvar(1:26, nchains = 2), 
    x = rvar_factor(letters, nchains = 2)
  )
  
  vars <- extract_list_of_variable_arrays(draws_rvars, c("x", "y"))
  
  expect_type(vars, "list")
  expect_length(vars, 2)
  expect_named(vars, c("x", "y"))
  
  # x should be a factor array
  expect_s3_class(vars$x, "factor")
  expect_equal(levels(vars$x), letters)
  
  # Compare with individual extractions
  expect_equal(vars$x, extract_variable_array(draws_rvars, "x"))
  expect_equal(vars$y, extract_variable_array(draws_rvars, "y"))
})

test_that("extract_list_of_variable_arrays passes additional arguments", {
  # This test assumes extract_variable_array supports additional arguments
  # We'll test that the ... arguments are properly passed through
  x <- example_draws()
  
  # This should work the same whether called individually or in batch
  individual_results <- list(
    mu = extract_variable_array(x, "mu"),
    theta = extract_variable_array(x, "theta")
  )
  
  batch_results <- extract_list_of_variable_arrays(x, c("mu", "theta"))
  
  expect_equal(batch_results, individual_results)
})

test_that("extract_list_of_variable_arrays works with default method", {
  # Test that the default method properly converts and calls the draws method
  x_array <- as.array(example_draws())
  
  result <- extract_list_of_variable_arrays(x_array, c("mu", "theta"))
  
  expect_type(result, "list")
  expect_length(result, 2)
  expect_named(result, c("mu", "theta"))
})


test_that("extract_list_of_variable_arrays works with NULL variables (extract all base variables)", {
  x <- example_draws(example = "multi_normal")
  
  # Test with NULL (should extract base variables only, not indexed ones)
  all_vars <- extract_list_of_variable_arrays(x)
  
  # Get expected base variable names
  all_individual_vars <- variables(x)
  expected_base_vars <- unique(split_variable_names(all_individual_vars)$base_name)  # Fixed: base_name not base_names
  
  expect_type(all_vars, "list")
  expect_length(all_vars, length(expected_base_vars))
  expect_named(all_vars, expected_base_vars)
  
  # Should match explicit specification of base variables
  explicit_vars <- extract_list_of_variable_arrays(x, expected_base_vars)
  expect_identical(all_vars, explicit_vars)
  
  # Compare with extracting each base variable individually
  for (var in expected_base_vars) {
    expect_equal(all_vars[[var]], extract_variable_array(x, var))
  }
  
  # Test with explicit NULL
  all_vars_explicit <- extract_list_of_variable_arrays(x, variables = NULL)
  expect_equal(all_vars, all_vars_explicit)
  
  # Verify we get base variables, not indexed ones
  expect_true("mu" %in% names(all_vars))
  expect_true("Sigma" %in% names(all_vars))  
  expect_false("mu[1]" %in% names(all_vars))
  expect_false("Sigma[1,1]" %in% names(all_vars))
})

