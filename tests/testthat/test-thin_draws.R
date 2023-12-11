test_that("thin_draws works correctly", {
  x <- as_draws_array(example_draws())
  expect_equal(niterations(thin_draws(x, 5L)), niterations(x) / 5)
  expect_equal(x, thin_draws(x, thin = 1L))
  expect_error(thin_draws(x, -1), "'thin' must be a positive integer")
  expect_error(thin_draws(x, 1000), "'thin' must be smaller than")
})

test_that("thin_draws works on rvars", {
  x <- example_draws()

  expect_equal(thin_draws(as_draws_rvars(x)$theta, 10L), as_draws_rvars(thin_draws(x, 10L))$theta)
})

test_that("automatic thinning works as expected", {
  x <- as_draws_array(example_draws())
  mu_1 <- subset_draws(x, variable = "mu", chain = 1)

  ess_tail_mu_1 <- ess_tail(mu_1)
  ess_bulk_mu_1 <- ess_bulk(mu_1)

  thin_by <- round(ndraws(mu_1) / min(ess_tail_mu_1, ess_bulk_mu_1))
  expect_equal(thin_draws(mu_1), thin_draws(mu_1, thin = thin_by))

})
