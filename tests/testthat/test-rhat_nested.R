test_that("rhat_nested returns reasonable values", {
  tau <- extract_variable_matrix(example_draws(), "tau")

  rhat <- rhat_nested(tau, superchain_ids = c(1, 2, 1, 2))
  expect_true(rhat > 0.99 & rhat < 1.05)

  rhat <- rhat_nested(tau, superchain_ids = c(1, 2, 1, 2))
  expect_true(rhat > 0.99 & rhat < 1.05)
})


