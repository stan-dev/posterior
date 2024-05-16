test_that("pareto_khat handles constant tail correctly", {

  # left tail is constant, so khat should be NA, but for "both" it
  # should be the same as the right tail
  x <- c(rep(-100, 10), sort(rnorm(100)))

  expect_true(is.na(pareto_khat(x, tail = "left", ndraws_tail = 10)))
  expect_equal(
    pareto_khat(x, tail = "right", ndraws_tail = 10),
    pareto_khat(x, tail = "both", ndraws_tail = 10)
  )

})


test_that("pareto_khat handles tail argument", {

  # as tau is bounded (0, Inf) the left pareto k should be lower than
  # right
  tau <- extract_variable_matrix(example_draws(), "tau")
  pkl <- pareto_khat(tau, tail = "left")
  pkr <- pareto_khat(tau, tail = "right")
  pkb <- pareto_khat(tau)
  expect_true(pkl < pkr)
  expect_equal(pkr, pkb)
})

test_that("pareto_khat handles ndraws_tail argument", {

  tau <- extract_variable_matrix(example_draws(), "tau")
  pk10 <- pareto_khat(tau, tail = "right", ndraws_tail = 10)
  pk25 <- pareto_khat(tau, tail = "right", ndraws_tail = 25)
  expect_true(pk10 > pk25)

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
  expect_true(pk1 < pk0.6)

})


test_that("pareto_khat diagnostics messages are as expected", {

  diags <- list(
    khat = 0.5,
    min_ss = 10,
    khat_threshold = 0.55,
    convergence_rate = 0.99
  )

  expect_message(pareto_k_diagmsg(diags),
                   paste0("Pareto k-hat = 0.5.\n"))

  diags$khat <- 0.6

    expect_message(pareto_k_diagmsg(diags),
                   paste0("Pareto k-hat = 0.6. Sample size is too small, for given Pareto k-hat. Sample size larger than 10 is needed for reliable results.\n"))

    diags$khat <- 0.71
    diags$khat_threshold <- 0.8

    expect_message(pareto_k_diagmsg(diags),
                   paste0("Pareto k-hat = 0.71. Bias dominates when k-hat > 0.7, making empirical mean estimate of the Pareto-smoothed draws unreliable.\n"))


    diags$khat <- 1.1

    expect_message(pareto_k_diagmsg(diags),
                   paste0("Pareto k-hat = 1.1. Mean does not exist, making empirical mean estimate of the draws not applicable.\n"))

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

  ps_chains <- pareto_smooth(tau_chains, ndraws_tail = 20, return_k = TRUE)
  ps_nochains <- pareto_smooth(tau_nochains, ndraws_tail = 20, return_k = TRUE)

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

  ps_chains <- pareto_smooth(tau_chains, ndraws_tail = 20, return_k = TRUE)
  ps_rvar_chains <- pareto_smooth(tau_rvar_chains, ndraws_tail = 20, return_k = TRUE)

  ps_nochains <- pareto_smooth(tau_nochains, ndraws_tail = 20, return_k = TRUE)
  ps_rvar_nochains <- pareto_smooth(tau_rvar_nochains, ndraws_tail = 20, return_k = TRUE)

  expect_equal(ps_rvar_chains$x, rvar(ps_chains$x, with_chains = TRUE))

  expect_equal(ps_rvar_nochains$x, rvar(ps_nochains$x))


  ps_chains <- pareto_smooth(tau_chains, ndraws_tail = 20, extra_diags = TRUE, return_k = TRUE)
  ps_rvar_chains <- pareto_smooth(tau_rvar_chains, ndraws_tail = 20, extra_diags = TRUE, return_k = TRUE)

  ps_nochains <- pareto_smooth(tau_nochains, ndraws_tail = 20, extra_diags = TRUE, return_k = TRUE)
  ps_rvar_nochains <- pareto_smooth(tau_rvar_nochains, ndraws_tail = 20, extra_diags = TRUE, return_k = TRUE)

  expect_equal(ps_rvar_chains$x, rvar(ps_chains$x, with_chains = TRUE))

  expect_equal(ps_rvar_nochains$x, rvar(ps_nochains$x))

})

test_that("pareto_smooth returns x with smoothed tail(s)", {
  mu <- extract_variable_matrix(example_draws(), "mu")

  mu_smoothed_right <- pareto_smooth(mu, ndraws_tail = 10, tail = "right", return_k = TRUE)$x

  mu_smoothed_left <- pareto_smooth(mu, ndraws_tail = 10, tail = "left", return_k = TRUE)$x

  mu_smoothed_both <- pareto_smooth(mu, ndraws_tail = 10, tail = "both", return_k = TRUE)$x

  expect_equal(sort(mu)[1:390], sort(mu_smoothed_right)[1:390])
  expect_equal(sort(mu_smoothed_both)[11:400], sort(mu_smoothed_right)[11:400])

  expect_equal(sort(mu)[11:400], sort(mu_smoothed_left)[11:400])
  expect_equal(sort(mu_smoothed_both)[1:390], sort(mu_smoothed_left)[1:390])

  expect_false(isTRUE(all.equal(sort(mu), sort(mu_smoothed_left))))
  expect_false(isTRUE(all.equal(sort(mu), sort(mu_smoothed_right))))
  expect_false(isTRUE(all.equal(sort(mu), sort(mu_smoothed_both))))

  expect_false(isTRUE(all.equal(sort(mu_smoothed_both), sort(mu_smoothed_left))))
  expect_false(isTRUE(all.equal(sort(mu_smoothed_both), sort(mu_smoothed_right))))

})

test_that("pareto_smooth works for log_weights", {
  w <- c(1:25, 1e3, 1e3, 1e3)
  lw <- log(w)

  ps <- pareto_smooth(lw, are_log_weights = TRUE, verbose = FALSE, ndraws_tail = 10, return_k = TRUE)

  # only right tail is smoothed
  expect_equal(ps$x[1:15] + max(lw), lw[1:15])

  expect_true(ps$diagnostics$khat > 0.7)

})
