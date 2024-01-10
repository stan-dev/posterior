test_that("thin_draws works correctly", {
  x <- as_draws_array(example_draws())
  expect_equal(niterations(thin_draws(x, 5L)), niterations(x) / 5)
  expect_equal(x, thin_draws(x, thin = 1L))
  expect_error(thin_draws(x, -1), "'thin' must be greater than or equal to 1")
  expect_error(thin_draws(x, 1000), "'thin' must be smaller than")
})

test_that("thin_draws works on rvars", {
  x <- example_draws()

  expect_equal(thin_draws(as_draws_rvars(x)$theta, 10L), as_draws_rvars(thin_draws(x, 10L))$theta)
})

test_that("automatic thinning works as expected", {
  x <- as_draws_array(example_draws())
  mu <- subset_draws(x, "mu")
  mu_1 <- subset_draws(mu, chain = 1)
  mu_2 <- subset_draws(mu, chain = 2)
  mu_3 <- subset_draws(mu, chain = 3)
  mu_4 <- subset_draws(mu, chain = 4)

  ess_mu_1 <- SW(min(ess_tail(mu_1), ess_bulk(mu_1)))
  ess_mu_2 <- SW(min(ess_tail(mu_2), ess_bulk(mu_2)))
  ess_mu_3 <- SW(min(ess_tail(mu_3), ess_bulk(mu_3)))
  ess_mu_4 <- SW(min(ess_tail(mu_4), ess_bulk(mu_4)))

  thin_by <- niterations(mu) / mean(
    c(ess_mu_1, ess_mu_2, ess_mu_3, ess_mu_4))
  
  expect_equal(thin_draws(mu), thin_draws(mu, thin = thin_by))

})
