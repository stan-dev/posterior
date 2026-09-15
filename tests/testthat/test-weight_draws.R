test_that("weight_draws rejects weights with no probability mass", {
  x <- example_draws()
  n <- ndraws(x)
  msg <- "All weights are zero"

  expect_error(weight_draws(x, rep(0, n)), msg)
  expect_error(weight_draws(x, rep(-Inf, n), log = TRUE), msg)
  # underflow is the usual cause, so the message points at log = TRUE
  expect_error(weight_draws(x, exp(rep(-800, n))), "log = TRUE", fixed = TRUE)

  # the error is classed, so callers can handle it per variable
  expect_error(weight_draws(x, rep(0, n)), class = "posterior_degenerate_weights_error")

  # a single draw carrying all the mass is still valid
  w <- c(1, rep(0, n - 1))
  expect_equal(unname(weights(weight_draws(x, w))), w)

  # all formats reject it
  for (fmt in list(as_draws_array, as_draws_df, as_draws_list, as_draws_rvars)) {
    expect_error(weight_draws(fmt(x), rep(0, n)), msg)
  }
})

test_that("weight_draws works on draws_matrix", {
  x <- as_draws_matrix(example_draws())
  weights <- rexp(ndraws(x))

  x1 <- weight_draws(x, weights)
  weights1 <- weights(x1, normalize = FALSE)
  expect_equal(weights1, weights)

  x2 <- weight_draws(x, log(weights), log = TRUE)
  weights2 <- weights(x2)
  expect_equal(weights2, weights / sum(weights))
})

test_that("weight_draws works on draws_array", {
  x <- as_draws_array(example_draws())
  weights <- rexp(ndraws(x))

  x1 <- weight_draws(x, weights)
  weights1 <- weights(x1)
  expect_equal(weights1, weights / sum(weights))

  x2 <- weight_draws(x, log(weights), log = TRUE)
  weights2 <- weights(x2, normalize = FALSE)
  expect_equal(weights2, weights)
})

test_that("weight_draws works on draws_df", {
  x <- as_draws_df(example_draws())
  weights <- rexp(ndraws(x))

  x1 <- weight_draws(x, weights)
  weights1 <- weights(x1, normalize = FALSE)
  expect_equal(weights1, weights)

  x2 <- weight_draws(x, log(weights), log = TRUE)
  weights2 <- weights(x2)
  expect_equal(weights2, weights / sum(weights))
})

test_that("weight_draws works on draws_list", {
  x <- as_draws_list(example_draws())
  weights <- rexp(ndraws(x))

  x1 <- weight_draws(x, weights)
  weights1 <- weights(x1)
  expect_equal(weights1, weights / sum(weights))

  x2 <- weight_draws(x, log(weights), log = TRUE)
  weights2 <- weights(x2, normalize = FALSE)
  expect_equal(weights2, weights)
})

test_that("weight_draws works on draws_rvars", {
  x <- as_draws_rvars(example_draws())
  weights <- rexp(ndraws(x))

  x1 <- weight_draws(x, weights)
  weights1 <- weights(x1)
  expect_equal(weights1, weights / sum(weights))

  x2 <- weight_draws(x, log(weights), log = TRUE)
  weights2 <- weights(x2, normalize = FALSE)
  expect_equal(weights2, weights)
})

# conversion preserves weights --------------------------------------------

test_that("weight_draws handles a mix of finite and -Inf log weights", {
  x <- example_draws()
  n <- ndraws(x)
  zero <- c(rep(TRUE, 3), rep(FALSE, n - 3))
  log_wts <- log(seq_len(n))
  log_wts[zero] <- -Inf
  wts <- exp(log_wts)

  for (fmt in list(as_draws_matrix, as_draws_array, as_draws_df,
                   as_draws_list, as_draws_rvars)) {
    xf <- fmt(x)
    from_log <- weight_draws(xf, log_wts, log = TRUE)
    from_ordinary <- weight_draws(xf, wts)

    # -Inf log weights and ordinary zeros describe the same distribution
    expect_equal(weights(from_log), weights(from_ordinary))
    expect_equal(weights(from_log, log = TRUE),
                 weights(from_ordinary, log = TRUE))

    w <- weights(from_log)
    expect_equal(unname(w[zero]), rep(0, sum(zero)))
    expect_true(all(w[!zero] > 0))
    expect_equal(sum(w), 1)

    # the zero-weight draws stay at -Inf on the log scale rather than
    # underflowing to something finite
    expect_equal(unname(weights(from_log, log = TRUE)[zero]),
                 rep(-Inf, sum(zero)))

    # unnormalized weights round-trip back to the input
    expect_equal(unname(weights(from_log, normalize = FALSE)), wts)
    expect_equal(unname(weights(from_log, normalize = FALSE, log = TRUE)),
                 log_wts)
  }
})

test_that("weights() normalizes very negative unnormalized log weights", {
  x <- example_draws()
  n <- ndraws(x)
  # a log likelihood summed over a few hundred observations lands here, and
  # every value is finite, so nothing upstream rejects it
  xw <- weight_draws(x, rep(-1000, n), log = TRUE)

  expect_equal(unname(weights(xw)), rep(1 / n, n))
  expect_equal(unname(weights(xw, log = TRUE)), rep(-log(n), n))
})

test_that("weights() errors when subsetting has removed all the mass", {
  x <- example_draws()
  n <- ndraws(x)
  # valid at construction: the first 10 draws carry all the mass
  xw <- weight_draws(x, c(rep(1, 10), rep(0, n - 10)))
  expect_silent(weights(xw))

  # dropping those draws leaves nothing behind
  # (subsetting by draw merges chains, hence the message)
  sub <- suppressMessages(subset_draws(xw, draw = 50:100))
  expect_error(weights(sub), "All draws have zero weight")
  expect_error(weights(sub, log = TRUE), "All draws have zero weight")

  # the unnormalized weights are still well defined, so they are still returned
  expect_equal(unname(weights(sub, normalize = FALSE)), rep(0, ndraws(sub)))
  expect_equal(unname(weights(sub, normalize = FALSE, log = TRUE)),
               rep(-Inf, ndraws(sub)))

  # a subset that keeps some mass is unaffected
  kept <- suppressMessages(subset_draws(xw, draw = 1:5))
  expect_silent(w <- weights(kept))
  expect_equal(sum(w), 1)
})

test_that("conversion between formats preserves weights", {
  draws <- list(
    matrix = weight_draws(draws_matrix(x = 1:10), 1:10),
    array = weight_draws(draws_array(x = 1:10), 1:10),
    df = weight_draws(draws_df(x = 1:10), 1:10),
    list = weight_draws(draws_list(x = 1:10), 1:10),
    rvars = weight_draws(draws_rvars(x = 1:10), 1:10)
  )

  # chain/iteration/draw columns are placed at the end by conversion functions,
  # so our reference format will keep that order
  reserved = names(draws$df) %in% reserved_df_variables()
  draws$df = draws$df[, c(names(draws$df)[!reserved], names(draws$df)[reserved])]

  for (type in names(draws)) {
    expect_equal(as_draws_matrix(draws[[!!type]]), draws$matrix)
    expect_equal(as_draws_array(draws[[!!type]]), draws$array)
    expect_equal(as_draws_df(draws[[!!type]]), draws$df)
    expect_equal(as_draws_list(draws[[!!type]]), draws$list)
    expect_equal(as_draws_rvars(draws[[!!type]]), draws$rvars)
  }
})

# pareto smoothing ----------------

test_that("pareto smoothing smooths weights in weight_draws", {
  x <- example_draws()
  lw <- sort(log(abs(rt(ndraws(x), 1))))
  weighted <- weight_draws(x, lw, pareto_smooth = FALSE, log = TRUE)
  smoothed <- weight_draws(x, lw, pareto_smooth = TRUE, log = TRUE)
  expect_false(all(weights(weighted) == weights(smoothed)))
})
