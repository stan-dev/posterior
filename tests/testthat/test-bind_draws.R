test_that("bind_draws works for draws_matrix objects", {
  draws1 <- as_draws_matrix(example_draws())
  draws2 <- subset_draws(draws1, iteration = 10:20)
  draws3 <- as_draws_df(data.frame(nu = rnorm(ndraws(draws1))))

  draws_new <- bind_draws(draws1, draws3, along = "variable")
  expect_equal(
    variables(draws_new),
    c(variables(draws1), variables(draws3))
  )
  expect_equal(ndraws(draws_new), ndraws(draws1))

  expect_error(bind_draws(draws1, draws3, along = "chain"),
               "Cannot bind 'draws_matrix' objects along 'chain'")

  draws_new <- bind_draws(draws1, draws2, along = "iteration")
  expect_equal(
    niterations(draws_new),
    niterations(draws1) + niterations(draws2)
  )
  expect_equal(variables(draws_new), variables(draws1))

  draws_new <- bind_draws(draws1, draws2, along = "draw")
  expect_equal(
    niterations(draws_new),
    niterations(draws1) + niterations(draws2)
  )
  expect_equal(variables(draws_new), variables(draws1))
})

test_that("bind_draws works for draws_array objects", {
  draws1 <- as_draws_array(example_draws())
  draws2 <- subset_draws(draws1, chain = 2)
  draws3 <- subset_draws(draws1, iteration = 10:20)
  draws4 <- as_draws_df(data.frame(
    nu = rnorm(ndraws(draws1)),
    .chain = rep(chain_ids(draws1), each = niterations(draws1))
  ))

  draws_new <- bind_draws(draws1, draws4, along = "variable")
  expect_equal(
    variables(draws_new),
    c(variables(draws1), variables(draws4))
  )
  expect_equal(ndraws(draws_new), ndraws(draws1))

  draws_new <- bind_draws(draws1, draws2, along = "chain")
  expect_equal(nchains(draws_new), nchains(draws1) + nchains(draws2))
  expect_equal(variables(draws_new), variables(draws1))

  draws_new <- bind_draws(draws1, draws3, along = "iteration")
  expect_equal(
    niterations(draws_new),
    niterations(draws1) + niterations(draws3)
  )
  expect_equal(variables(draws_new), variables(draws1))

  expect_error(bind_draws(draws1, draws3, along = "draw"),
               "Cannot bind 'draws_array' objects along 'draw'")
})

test_that("bind_draws works for draws_df objects", {
  draws1 <- as_draws_df(example_draws())
  draws2 <- subset_draws(draws1, chain = 2)
  draws3 <- subset_draws(draws1, iteration = 10:20)
  draws4 <- as_draws_list(data.frame(
    nu = rnorm(ndraws(draws1)),
    .chain = rep(chain_ids(draws1), each = niterations(draws1))
  ))

  draws_new <- bind_draws(draws1, draws4, along = "variable")
  expect_equal(
    variables(draws_new),
    c(variables(draws1), variables(draws4))
  )
  expect_equal(ndraws(draws_new), ndraws(draws1))

  draws_new <- bind_draws(draws1, draws2, along = "chain")
  expect_equal(nchains(draws_new), nchains(draws1) + nchains(draws2))
  expect_equal(variables(draws_new), variables(draws1))

  draws_new <- bind_draws(draws1, draws3, along = "iteration")
  expect_equal(
    niterations(draws_new),
    niterations(draws1) + niterations(draws3)
  )
  expect_equal(variables(draws_new), variables(draws1))

  draws_new <- bind_draws(draws1, draws3, along = "draw")
  expect_equal(ndraws(draws_new), ndraws(draws1) + ndraws(draws3))
  expect_equal(nchains(draws_new), 1L)

  draws_new <- bind_draws(NULL, draws1)
  expect_equal(draws_new, draws1)
})

test_that("bind_draws works for draws_list objects", {
  draws1 <- as_draws_list(example_draws())
  draws2 <- subset_draws(draws1, chain = 2)
  draws3 <- subset_draws(draws1, iteration = 10:20)
  draws4 <- as_draws_df(data.frame(
    nu = rnorm(ndraws(draws1)),
    .chain = rep(chain_ids(draws1), each = niterations(draws1))
  ))

  draws_new <- bind_draws(draws1, draws4, along = "variable")
  expect_equal(
    variables(draws_new),
    c(variables(draws1), variables(draws4))
  )
  expect_equal(ndraws(draws_new), ndraws(draws1))

  draws_new <- bind_draws(draws1, draws2, along = "chain")
  expect_equal(nchains(draws_new), nchains(draws1) + nchains(draws2))
  expect_equal(variables(draws_new), variables(draws1))

  draws_new <- bind_draws(draws1, draws3, along = "iteration")
  expect_equal(
    niterations(draws_new),
    niterations(draws1) + niterations(draws3)
  )
  expect_equal(variables(draws_new), variables(draws1))

  expect_error(bind_draws(draws1, draws3, along = "draw"),
               "Cannot bind 'draws_list' objects along 'draw'")
})

test_that("bind_draws works for draws_rvar objects", {
  draws1 <- as_draws_rvar(example_draws())
  draws2 <- subset_draws(draws1, chain = 2)
  draws3 <- subset_draws(draws1, iteration = 10:20)
  draws4 <- as_draws_list(data.frame(
    nu = rnorm(ndraws(draws1)),
    .chain = rep(chain_ids(draws1), each = niterations(draws1))
  ))

  draws_new <- bind_draws(draws1, draws4, along = "variable")
  expect_equal(
    variables(draws_new),
    c(variables(draws1), variables(draws4))
  )
  expect_equal(ndraws(draws_new), ndraws(draws1))

  draws_new <- bind_draws(draws1, draws2, along = "chain")
  expect_equal(nchains(draws_new), nchains(draws1) + nchains(draws2))
  expect_equal(variables(draws_new), variables(draws1))

  expect_error(bind_draws(draws1, draws3, along = "iteration"),
    "Cannot bind 'draws_rvar' objects along 'iteration'")

  draws_new <- bind_draws(draws1, draws3, along = "draw")
  expect_equal(ndraws(draws_new), ndraws(draws1) + ndraws(draws3))
  expect_equal(nchains(draws_new), 1L)

  draws_new <- bind_draws(NULL, draws1)
  expect_equal(draws_new, draws1)
})

test_that("bind_draws errors if all NULL", {
  expect_error(bind_draws(NULL, NULL), "All objects passed to 'bind_draws' are NULL")
})

test_that("bind_draws errors for inputs with incompatible variables", {
  x1 <- x2 <- example_draws()
  variables(x1) <- gsub("theta", "beta", variables(x1))
  expect_error(bind_draws(x1, x2, along = "chain"), "'variables' of bound objects do not match")
})

