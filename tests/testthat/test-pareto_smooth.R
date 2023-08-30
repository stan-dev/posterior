test_that("pareto_khat returns expected reasonable values", {
  tau <- extract_variable_matrix(example_draws(), "tau")

  pk <- pareto_khat(tau)
  expect_true(names(pk) == "khat")

})

test_that("pareto_khat handles tail argument", {

  # as tau is bounded (0, Inf) the left pareto k should be lower than
  # right
  tau <- extract_variable_matrix(example_draws(), "tau")
  pkl <- pareto_khat(tau, tail = "left")
  pkr <- pareto_khat(tau, tail = "right")
  pkb <- pareto_khat(tau)
  expect_true(pkl$khat < pkr$khat)
  expect_equal(pkr$khat, pkb$khat)
})

test_that("pareto_khat handles ndraws_tail argument", {

  tau <- extract_variable_matrix(example_draws(), "tau")
  pk10 <- pareto_khat(tau, tail = "right", ndraws_tail = 10)
  pk25 <- pareto_khat(tau, tail = "right", ndraws_tail = 25)
  expect_true(pk10$khat > pk25$khat)

  expect_warning(pareto_khat(tau, tail = "both", ndraws_tail = 201),
                 "Number of tail draws cannot be more than half ",
                 "the total number of draws if both tails are fit, ",
                 "changing to 200.")

  expect_warning(pareto_khat(tau, tail = "both", ndraws_tail = 4),
                 "Number of tail draws cannot be less than 5. Changing to 5.")

})


test_that("pareto_khat handles r_eff argument", {

  tau <- extract_variable_matrix(example_draws(), "tau")
  pk1 <- pareto_khat(tau, r_eff = 1)
  pk0.6 <- pareto_khat(tau, r_eff = 0.6)
  expect_true(pk1$khat < pk0.6$khat)

})


test_that("pareto_khat diagnostics messages are as expected", {

  diags <- list(
    khat = 0.5,
    min_ss = 10,
    khat_threshold = 0.55,
    convergence_rate = 0.99
  )

  expect_message(pareto_k_diagmsg(diags),
                   paste0('To halve the RMSE, approximately 4.1 times bigger S is needed.'))

  diags$khat <- 0.6

    expect_message(pareto_k_diagmsg(diags),
                   paste0('S is too small, and sample size larger than 10 is needed for reliable results.\n'))

    diags$khat <- 0.71
    diags$khat_threshold <- 0.8

    expect_message(pareto_k_diagmsg(diags),
                   paste0('To halve the RMSE, approximately 4.1 times bigger S is needed.\n', 'Bias dominates RMSE, and the variance based MCSE is underestimated.\n'))


    diags$khat <- 1.1

    expect_message(pareto_k_diagmsg(diags),
                   paste0('All estimates are unreliable. If the distribution of ratios is bounded,\n',
                  'further draws may improve the estimates, but it is not possible to predict\n',
                  'whether any feasible sample size is sufficient.'))

})

test_that("pareto_diags returns expected diagnostics", {

  tau <- extract_variable_matrix(example_draws(), "tau")

  pk <- pareto_diags(tau)
  expect_true(all(names(pk) == c("khat", "min_ss", "khat_threshold", "convergence_rate")))
})


test_that("pareto_khat diagnostics handles verbose argument", {
  tau <- extract_variable_matrix(example_draws(), "tau")

  expect_message(pareto_khat(tau, extra_diags = TRUE, verbose = TRUE))
  expect_message(pareto_khat(tau, extra_diags = FALSE, verbose = TRUE))
  expect_no_message(pareto_khat(tau, extra_diags = TRUE, verbose = FALSE))

})

test_that("pareto_khat diagnostics accept vectors as input", {
  set.seed(1234)
  x <- rnorm(1000)

  pk <- pareto_khat(x)
  expect_true(pk < 1)
})

test_that("pareto_khat diagnostics handle special cases correctly", {
  set.seed(1234)

  x <- c(rnorm(50), NA)
  expect_warning(pareto_khat(x))
  expect_true(is.na(suppressWarnings(pareto_khat(x))))

  x <- c(rnorm(50), Inf)
  expect_warning(pareto_khat(x))
  expect_true(is.na(suppressWarnings(pareto_khat(x))))

  x <- rep(1, 50)
  expect_warning(pareto_khat(x))
  expect_true(is.na(suppressWarnings(pareto_khat(x))))

})


test_that("pareto_khat functions work with matrix with chains", {

  tau_chains <- extract_variable_matrix(example_draws(), "tau")
  tau_nochains <- extract_variable(example_draws(), "tau")

  expect_equal(pareto_khat(tau_chains, ndraws_tail = 20),
               pareto_khat(tau_nochains, ndraws_tail = 20))

  ps_chains <- pareto_smooth(tau_chains, ndraws_tail = 20)
  ps_nochains <- pareto_smooth(tau_nochains, ndraws_tail = 20)

  expect_equal(as.numeric(ps_chains$x), as.numeric(ps_nochains$x))

})

test_that("pareto_khat functions work with rvars with and without chains", {

  tau_chains <- extract_variable_matrix(example_draws(), "tau")
  tau_nochains <- extract_variable(example_draws(), "tau")

  tau_rvar_chains <- rvar(tau_chains, with_chains = TRUE)
  tau_rvar_nochains <- rvar(tau_nochains)

  expect_equal(pareto_khat(tau_chains, ndraws_tail = 20),
               pareto_khat(tau_rvar_chains, ndraws_tail = 20))

  expect_equal(pareto_khat(tau_rvar_chains, ndraws_tail = 20),
               pareto_khat(tau_rvar_nochains, ndraws_tail = 20))


  expect_equal(pareto_diags(tau_chains, ndraws_tail = 20),
               pareto_diags(tau_rvar_chains, ndraws_tail = 20))

  expect_equal(pareto_diags(tau_rvar_chains, ndraws_tail = 20),
               pareto_diags(tau_rvar_nochains, ndraws_tail = 20))

  ps_chains <- pareto_smooth(tau_chains, ndraws_tail = 20)
  ps_rvar_chains <- pareto_smooth(tau_rvar_chains, ndraws_tail = 20)

  ps_nochains <- pareto_smooth(tau_nochains, ndraws_tail = 20)
  ps_rvar_nochains <- pareto_smooth(tau_rvar_nochains, ndraws_tail = 20)

  expect_equal(ps_rvar_chains$x, rvar(ps_chains$x, with_chains = TRUE))

  expect_equal(ps_rvar_nochains$x, rvar(ps_nochains$x))


  ps_chains <- pareto_smooth(tau_chains, ndraws_tail = 20, extra_diags = TRUE)
  ps_rvar_chains <- pareto_smooth(tau_rvar_chains, ndraws_tail = 20, extra_diags = TRUE)

  ps_nochains <- pareto_smooth(tau_nochains, ndraws_tail = 20, extra_diags = TRUE)
  ps_rvar_nochains <- pareto_smooth(tau_rvar_nochains, ndraws_tail = 20, extra_diags = TRUE)

  expect_equal(ps_rvar_chains$x, rvar(ps_chains$x, with_chains = TRUE))

  expect_equal(ps_rvar_nochains$x, rvar(ps_nochains$x))

})

test_that("pareto_smooth returns x with smoothed tail", {
  tau <- extract_variable_matrix(example_draws(), "tau")

  tau_smoothed <- pareto_smooth(tau, ndraws_tail = 10, tail = "right")$x

  expect_equal(sort(tau)[1:390], sort(tau_smoothed)[1:390])

  expect_false(isTRUE(all.equal(sort(tau), sort(tau_smoothed))))

})
