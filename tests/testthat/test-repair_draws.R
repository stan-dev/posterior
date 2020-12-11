test_that("repair_draws works correctly on draws_matrix objects", {
  x <- as_draws_matrix(example_draws())
  x <- x[10:6, ]

  x_rep <- repair_draws(x, order = TRUE)
  expect_equal(x[5:1, ], x_rep, check.attributes = FALSE)
  expect_equal(rownames(x_rep), as.character(1:5))

  x_rep <- repair_draws(x, order = FALSE)
  expect_equal(x, x_rep, check.attributes = FALSE)
  expect_equal(rownames(x_rep), as.character(1:5))
})

test_that("repair_draws works correctly on draws_array objects", {
  x <- as_draws_array(example_draws())
  x <- x[10:6, c(4, 1), ]

  x_rep <- repair_draws(x, order = TRUE)
  expect_equal(x[5:1, 2:1, ], x_rep, check.attributes = FALSE)
  expect_equal(rownames(x_rep), as.character(1:5))
  expect_equal(colnames(x_rep), as.character(1:2))

  x_rep <- repair_draws(x, order = FALSE)
  expect_equal(x, x_rep, check.attributes = FALSE)
  expect_equal(rownames(x_rep), as.character(1:5))
  expect_equal(colnames(x_rep), as.character(1:2))
})

test_that("repair_draws works correctly on draws_df objects", {
  x <- as_draws_df(example_draws())
  x <- subset(x, iteration = 1:5)
  x <- x[c(16, 11, 8, 2), ]

  x_rep <- repair_draws(x, order = TRUE)
  expect_equal(x$mu[order(x$.chain, x$.iteration)], x_rep$mu)
  expect_equal(x_rep$.iteration, rep(1, 4))
  expect_equal(x_rep$.chain, 1:4)

  x_rep <- repair_draws(x, order = FALSE)
  expect_equal(x$mu, x_rep$mu, check.attributes = FALSE)
  expect_equal(x_rep$.iteration, rep(1, 4))
  expect_equal(x_rep$.chain, 4:1)
})

test_that("repair_draws works correctly on draws_list objects", {
  x <- as_draws_list(example_draws())
  x <- x[c(4, 2)]

  x_rep <- repair_draws(x, order = TRUE)
  expect_equal(x[2], x_rep[1], check.attributes = FALSE)
  expect_equal(names(x_rep), as.character(1:2))

  x_rep <- repair_draws(x, order = FALSE)
  expect_equal(x, x_rep, check.attributes = FALSE)
  expect_equal(names(x_rep), as.character(1:2))
})

test_that("repair_draws works correctly on draws_rvars objects", {
  x <- as_draws_rvars(example_draws())
  draws_of(x$mu) <- draws_of(x$mu)[c(16, 11, 8, 2),, drop = FALSE]
  draws_of(x$tau) <- draws_of(x$tau)[c(16, 11, 8, 2),, drop = FALSE]
  draws_of(x$theta) <- draws_of(x$theta)[c(16, 11, 8, 2),, drop = FALSE]

  x_rep <- repair_draws(x, order = TRUE)
  expect_equal(
    draws_of(x_rep$mu),
    draws_of(x$mu)[order(as.integer(rownames(draws_of(x$mu)))),],
    check.attributes = FALSE
  )
  expect_equal(niterations(x_rep), 4)
  expect_equal(nchains(x_rep), 1)

  x_rep <- repair_draws(x, order = FALSE)
  expect_equal(draws_of(x$mu), draws_of(x_rep$mu), check.attributes = FALSE)
  expect_equal(niterations(x_rep), 4)
  expect_equal(nchains(x_rep), 1)

  x_ord <- order_draws(x)
  expect_equal(
    draws_of(x_ord$mu),
    draws_of(x$mu)[order(as.integer(rownames(draws_of(x$mu)))),],
    check.attributes = FALSE
  )
  expect_equal(
    draws_of(x_ord$theta),
    draws_of(x$theta)[order(as.integer(rownames(draws_of(x$theta)))),],
    check.attributes = FALSE
  )
})
