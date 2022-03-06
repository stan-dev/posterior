test_that("transformations to and from draws_matrix objects work", {
  draws_matrix <- as_draws_matrix(example_draws())

  draws_array <- as_draws_array(draws_matrix)
  draws_matrix2 <- as_draws_matrix(draws_array)
  expect_equal(draws_matrix, draws_matrix2)

  draws_df <- as_draws_df(draws_matrix)
  draws_matrix2 <- as_draws_matrix(draws_df)
  expect_equal(draws_matrix, draws_matrix2)

  draws_list <- as_draws_list(draws_matrix)
  draws_matrix2 <- as_draws_matrix(draws_list)
  expect_equal(draws_matrix, draws_matrix2)

  draws_rvars <- as_draws_rvars(draws_matrix)
  draws_matrix2 <- as_draws_matrix(draws_rvars)
  expect_equal(draws_matrix, draws_matrix2)
})

test_that("transformations to and from draws_array objects work", {
  draws_array <- as_draws_array(example_draws())

  draws_matrix <- as_draws_matrix(draws_array)
  draws_array2 <- as_draws_array(draws_matrix)
  expect_equal(draws_array, draws_array2)

  draws_df <- as_draws_df(draws_array)
  draws_array2 <- as_draws_array(draws_df)
  expect_equal(draws_array, draws_array2)

  draws_list <- as_draws_list(draws_array)
  draws_array2 <- as_draws_array(draws_list)
  expect_equal(draws_array, draws_array2)

  draws_rvars <- as_draws_rvars(draws_array)
  draws_array2 <- as_draws_array(draws_rvars)
  expect_equal(draws_array, draws_array2)
})

test_that("transformations to and from draws_df objects work", {
  draws_df <- as_draws_df(example_draws())

  draws_matrix <- as_draws_matrix(draws_df)
  draws_df2 <- as_draws_df(draws_matrix)
  expect_equal(draws_df, draws_df2)

  draws_array <- as_draws_array(draws_df)
  draws_df2 <- as_draws_df(draws_array)
  expect_equal(draws_df, draws_df2)

  draws_list <- as_draws_list(draws_df)
  draws_df2 <- as_draws_df(draws_list)
  expect_equal(draws_df, draws_df2)

  draws_rvars <- as_draws_rvars(draws_df)
  draws_df2 <- as_draws_df(draws_rvars)
  expect_equal(draws_df, draws_df2)

  # test that a single draw does not lead to a drop of dimensions
  draws_rvars2 <- subset_draws(draws_rvars, draw = 1)
  draws_df2 <- as_draws_df(draws_rvars2)
  expect_equal(subset_draws(draws_df, draw = 1), draws_df2)
})

test_that("transformations to and from draws_list objects work", {
  draws_list <- as_draws_list(example_draws())

  draws_matrix <- as_draws_matrix(draws_list)
  draws_list2 <- as_draws_list(draws_matrix)
  expect_equal(draws_list, draws_list2)

  draws_array <- as_draws_array(draws_list)
  draws_list2 <- as_draws_list(draws_array)
  expect_equal(draws_list, draws_list2)

  draws_df <- as_draws_df(draws_list)
  draws_list2 <- as_draws_list(draws_df)
  expect_equal(draws_list, draws_list2)

  draws_rvars <- as_draws_rvars(draws_list)
  draws_list2 <- as_draws_list(draws_rvars)
  expect_equal(draws_list, draws_list2)
})

test_that("transformations to and from draws_rvars objects work", {
  draws_rvars <- as_draws_rvars(example_draws())

  draws_matrix <- as_draws_matrix(draws_rvars)
  draws_rvars2 <- as_draws_rvars(draws_matrix)
  expect_equal(draws_rvars, draws_rvars2)

  draws_array <- as_draws_array(draws_rvars)
  draws_rvars2 <- as_draws_rvars(draws_array)
  expect_equal(draws_rvars, draws_rvars2)

  draws_df <- as_draws_df(draws_rvars)
  draws_rvars2 <- as_draws_rvars(draws_df)
  expect_equal(draws_rvars, draws_rvars2)

  draws_list <- as_draws_list(draws_rvars)
  draws_rvars2 <- as_draws_rvars(draws_list)
  expect_equal(draws_rvars, draws_rvars2)
})

test_that("matrices can be transformed to draws_matrix objects", {
  x <- round(rnorm(200), 2)
  x <- array(x, dim = c(40, 5))
  dimnames(x) <- list(NULL, paste0("theta", 1:5))

  y <- as_draws(x)
  expect_is(y, "draws_matrix")
  expect_equal(variables(y), colnames(x))
  expect_equal(niterations(y), 40)
  expect_equal(nchains(y), 1)
})

test_that("arrays can be transformed to draws_array objects", {
  x <- round(rnorm(200), 2)
  x <- array(x, dim = c(10, 4, 5))
  dimnames(x) <- list(NULL, NULL, paste0("theta", 1:5))

  y <- as_draws(x)
  expect_is(y, "draws_array")
  expect_equal(variables(y), dimnames(x)[[3]])
  expect_equal(niterations(y), 10)
  expect_equal(nchains(y), 4)
})

test_that("data.frames can be transformed to draws_df objects", {
  x <- data.frame(
    v1 = rnorm(100),
    v2 = rnorm(100)
  )
  y <- as_draws(x)
  expect_is(y, "draws_df")
  expect_equal(variables(y), names(x))
  expect_equal(niterations(y), 100)
  expect_equal(nchains(y), 1)

  # are .iteration and .chain automatically used?
  x2 <- x
  x2$.iteration <- 1:100
  x2$.chain <- rep(1:4, each = 25)
  y2 <- as_draws(x2)
  expect_is(y2, "draws_df")
  expect_equal(variables(y2), names(x))
  expect_equal(niterations(y2), 25)
  expect_equal(nchains(y2), 4)
})

test_that("lists can be transformed to draws_list objects", {
  x <- list(
    list(
      v1 = rnorm(50),
      v2 = rnorm(50)
    ),
    list(
      v1 = rnorm(50),
      v2 = rnorm(50)
    )
  )
  y <- as_draws(x)
  expect_is(y, "draws_list")
  expect_equal(variables(y), names(x[[1]]))
  expect_equal(ndraws(y), 100)
  expect_equal(nchains(y), 2)
})

test_that("numeric vectors can be transformed to draws_matrix objects", {
  draws_matrix <- draws_matrix(a = 1:10, b = 11:20, c = 1)
  draws_matrix2 <- as_draws_matrix(cbind(1:10, 11:20, 1))
  expect_equivalent(draws_matrix, draws_matrix2)
})

test_that("numeric vectors can be transformed to draws_array objects", {
  draws_array <- draws_array(a = 1:10, b = 11:20, c = 1, .nchains = 2)
  draws_array2 <- array(c(1:10, 11:20, rep(1, 10)), c(5, 2, 3))
  dimnames(draws_array2)[[3]] <- c("a", "b", "c")
  draws_array2 <- as_draws_array(draws_array2)
  expect_equal(draws_array, draws_array2)
})

test_that("numeric vectors can be transformed to draws_df objects", {
  draws_df <- draws_df(a = 1:10, b = 11:20, c = 1, .nchains = 2)
  draws_array <- array(c(1:10, 11:20, rep(1, 10)), c(5, 2, 3))
  dimnames(draws_array)[[3]] <- c("a", "b", "c")
  draws_df2 <- as_draws_df(draws_array)
  expect_equal(draws_df, draws_df2)
})

test_that("numeric vectors can be transformed to draws_list objects", {
  draws_list <- draws_list(a = 1:10, b = 11:20, c = 1, .nchains = 2)
  draws_array <- array(c(1:10, 11:20, rep(1, 10)), c(5, 2, 3))
  dimnames(draws_array)[[3]] <- c("a", "b", "c")
  draws_list2 <- as_draws_list(draws_array)
  expect_equal(draws_list, draws_list2)
})

test_that("lists of data.frames are correctly transformed to draws_list objects", {
  x <- list(data.frame(y=1:10, x = 0), data.frame(y = 11:20, x = 1))
  y <- as_draws_list(x)
  expect_equal(y[[1]]$y, 1:10)
  expect_true(is.list(y[[1]]))
  expect_true(!is.data.frame(y[[1]]))
})

test_that("numeric vectors can be transformed to draws_rvars objects", {
  draws_rvars <- draws_rvars(a = 1:10, b = 11:20, c = 1, .nchains = 2)
  draws_array <- array(c(1:10, 11:20, rep(1, 10)), c(5, 2, 3))
  dimnames(draws_array)[[3]] <- c("a", "b", "c")
  draws_rvars2 <- as_draws_rvars(draws_array)
  expect_equal(draws_rvars, draws_rvars2)
})

test_that("rvars can be transformed to draws objects", {
  rv_array <- array(c(1:10, 11:20, rep(1, 10)), c(5, 2, 3))
  rv <- rvar(rv_array)
  draws_rvars <- draws_rvars(x = rv)
  expect_equal(as_draws(rv), draws_rvars)
  expect_equal(as_draws_rvars(rv), draws_rvars)
  expect_equal(as_draws_matrix(rv), as_draws_matrix(draws_rvars))
  expect_equal(as_draws_array(rv), as_draws_array(draws_rvars))
  expect_equal(as_draws_df(rv), as_draws_df(draws_rvars))
  expect_equal(as_draws_list(rv), as_draws_list(draws_rvars))
})

test_that("mcmc and mcmc.list objects can be transformed to draws objects", {
  # don't want to add coda as dependency so construct equivalent of
  # mcmc and mcmc.list objects
  x1 <- matrix(1:20, 10, 2)
  x2 <- matrix(21:40, 10, 2)
  dimnames(x1) <- dimnames(x2) <- list(1:10, c("A", "B"))
  attr(x1, "mcpar") <- attr(x2, "mcpar") <- c(1, 10, 1)
  class(x1) <- class(x2) <- "mcmc"
  xlist <- structure(list(x1 = x1, x2 = x2), class = "mcmc.list")

  mcmc_draws <- list(
    as_draws(x1),
    as_draws_matrix(x1),
    as_draws_array(x1),
    as_draws_df(x1),
    as_draws_list(x1),
    as_draws_rvars(x1)
  )
  mcmc_list_draws <- list(
    as_draws(xlist),
    as_draws_matrix(xlist),
    as_draws_array(xlist),
    as_draws_df(xlist),
    as_draws_list(xlist),
    as_draws_rvars(xlist)
  )
  for (j in seq_along(mcmc_draws)) {
    xj <- mcmc_draws[[j]]
    expect_equal(ndraws(xj), 10)
    expect_equal(nvariables(xj), 2)
    expect_equal(nchains(xj), 1)
    expect_equal(variables(xj), c("A", "B"))

    xj <- mcmc_list_draws[[j]]
    expect_equal(ndraws(xj), 20)
    expect_equal(nchains(xj), 2)
    expect_equal(nvariables(xj), 2)
    expect_equal(variables(xj), c("A", "B"))
  }
})

test_that("empty draws objects can be converted", {
  empty_draws <- list(
    empty_draws_matrix(),
    empty_draws_array(),
    empty_draws_list(),
    empty_draws_df(),
    empty_draws_rvars()
  )
  for (j in seq_along(empty_draws)) {
    # basically just check they don't error and preserve 0 draws
    empty_j <- empty_draws[[j]]
    expect_equal(ndraws(as_draws_matrix(empty_j)), 0)
    expect_equal(ndraws(as_draws_array(empty_j)), 0)
    expect_equal(ndraws(as_draws_df(empty_j)), 0)
    expect_equal(ndraws(as_draws_list(empty_j)), 0)
    expect_equal(ndraws(as_draws_rvars(empty_j)), 0)
  }
})

test_that("as_draws throws appropriate error if no close format", {
  expect_error(
    as_draws("A"),
    "Don't know how to transform an object of class 'character'"
  )
  expect_error(
    as_draws(TRUE),
    "Don't know how to transform an object of class 'logical'"
  )
})

test_that("draws_* constructors throw correct errors", {
  expect_error(draws_array(a = 1, .nchains = 0), "Number of chains must be positive")
  expect_error(draws_df(a = 1, .nchains = 0), "Number of chains must be positive")
  expect_error(draws_list(a = 1, .nchains = 0), "Number of chains must be positive")

  expect_error(draws_array(a = 1, .nchains = 2), "Number of chains does not divide the number of draws")
  expect_error(draws_df(a = 1, .nchains = 2), "Number of chains does not divide the number of draws")
  expect_error(draws_list(a = 1, .nchains = 2), "Number of chains does not divide the number of draws")
})

test_that("as_draws_rvars correctly reshapes missing, out-of-order, and string array indices", {
  x_array <- as_draws_array(example_draws())
  variables(x_array) <- paste0("var[", rep(1:2, each = 5), ",", rep(1:5, 2), "]")
  x_rvars <- as_draws_rvars(x_array)

  x_array2 <- remove_variables(x_array, "var[2,3]")
  x_rvars2 <- x_rvars
  x_rvars2$var[2,3] <- NA
  expect_equal(as_draws_rvars(x_array2), x_rvars2)

  x_array2 <- subset_draws(x_array, variable = c("var[1,4]", "var[2,3]"))
  x_rvars2 <- x_rvars
  x_rvars2$var <- x_rvars2$var[1:2,1:4]
  x_rvars2$var[1,1:3] <- NA
  x_rvars2$var[2,c(1:2,4)] <- NA
  expect_equal(as_draws_rvars(x_array2), x_rvars2)

  x_rvars2 <- x_rvars
  rownames(x_rvars2$var) <- letters[1:2]
  colnames(x_rvars2$var) <- rev(letters[1:5])
  expect_equal(as_draws_rvars(as_draws_array(x_rvars2)), x_rvars2)
})

test_that("as_draws_rvars can accept lists of lists as input", {
  # for https://github.com/stan-dev/posterior/issues/192
  draws_list <- as_draws_list(example_draws())
  list_of_lists <- unclass(draws_list)

  expect_equal(as_draws_rvars(example_draws()), as_draws_rvars(list_of_lists))
})

test_that("draws_df does not munge variable names", {
  draws_df <- draws_df(`x[1]` = 1:2, `x[2]` = 3:4)
  expect_equal(variables(draws_df), c("x[1]", "x[2]"))
})

test_that("draws_df can roundtrip through data.frame", {
  draws_df <- draws_df(`x[1]` = 1:2, `x[2]` = 3:4)
  expect_equal(as_draws_df(as.data.frame(draws_df)), draws_df)
  expect_equal(as_draws_df(tibble::as_tibble(draws_df)), draws_df)
  draws_dataframe <- as.data.frame(draws_df)
  draws_dataframe$.draw <- NULL
  expect_equal(as_draws_df(draws_dataframe), draws_df)
})

test_that("draws_df drops the draws class when metadata is removed", {
  draws_df <- as_draws_df(example_draws())

  expect_equal(dplyr::count(draws_df, .chain), tibble::tibble(.chain = 1:4, n = 100L))
})
