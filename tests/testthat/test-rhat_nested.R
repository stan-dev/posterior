test_that("rhat_nested returns reasonable values", {
  tau <- extract_variable_matrix(example_draws(), "tau")

  nested_rhat <- rhat_nested(tau, superchain_ids = c(1, 2, 1, 2))
  expect_true(nested_rhat > 1 & nested_rhat < 1.05)

  nested_rhat <- rhat_nested(tau, superchain_ids = c(1, 2, 1, 2))
  expect_true(nested_rhat > 1 & nested_rhat < 1.05)
})


test_that("rhat_nested handles special cases correctly", {
  set.seed(1234)
  x <- c(rnorm(10), NA)
  expect_true(is.na(rhat_nested(x, superchain_ids = c(1))))

  x <- c(rnorm(10), Inf)
  expect_true(is.na(rhat_nested(x, superchain_ids = c(1, 2, 1, 2))))

  tau <- extract_variable_matrix(example_draws(), "tau")
  expect_warning(
    rhat_nested(tau, superchain_ids = c(1, 1, 1, 3)),
    "Number of chains per superchain is not the same for each superchain, returning NA."
  )

  tau <- extract_variable_matrix(example_draws(), "tau")
  expect_warning(
    rhat_nested(tau, superchain_ids = c(1, 2)),
    "Length of superchain_ids not equal to number of chains, returning NA."
  )
})
