# function for making rvars from arrays that expects last index to be
# draws (for testing so that when array structure changes tests don't have to)
rvar_from_array = function(x) {
  .dim = dim(x)
  last_dim = length(.dim)
  new_rvar(aperm(x, c(last_dim, seq_len(last_dim - 1))))
}

# creating rvars ----------------------------------------------------------

test_that("rvar creation with custom dim works", {
  x_matrix <- array(1:24, dim = c(2,12))
  x_array <- array(1:24, dim = c(2,3,4))

  expect_equal(rvar(x_matrix, dim = c(3,4)), rvar(x_array))
})

test_that("rvar can be created with specified number of chains", {
  x_array <- array(1:20, dim = c(4,5))

  expect_error(rvar(x_array, nchains = 0))
  expect_equal(rvar(x_array, nchains = 1), rvar(x_array))
  expect_equal(nchains(rvar(x_array, nchains = 2)), 2)
  expect_error(rvar(x_array, nchains = 3), "Number of chains does not divide the number of draws")
})

test_that("rvar constructor using with_chains works", {
  # multidimensional rvar with chains
  x_array_nochains <- array(1:24, dim = c(6,2,2), dimnames = list(
    NULL, A = c("a1", "a2"), B = c("b1", "b2")
  ))
  x_array_chains <- array(1:24, dim = c(3,2,2,2), dimnames = list(
    NULL, NULL, A = c("a1", "a2"), B = c("b1", "b2")
  ))
  x_nochains <- rvar(x_array_nochains, nchains = 2)
  x_chains <- rvar(x_array_chains, with_chains = TRUE)
  expect_equal(x_chains, x_nochains)

  # scalar rvar with chains
  x2_array_nochains <- 1:24
  x2_array_chains <- array(1:24, dim = c(6,4))
  x2_nochains <- rvar(x2_array_nochains, nchains = 4)
  x2_chains <- rvar(x2_array_chains, with_chains = TRUE)
  expect_equal(x2_chains, x2_nochains)

  # NULL rvar
  expect_equal(rvar(with_chains = TRUE), rvar())

  # can't use with_chains when no chain dimension information provided
  expect_error(rvar(1, with_chains = TRUE))
})

test_that("NULL rvar creates a 0-length numeric rvar with 1 draw", {
  expect_equal(rvar(), rvar(numeric()))
  expect_equal(rvar(NULL), rvar(numeric()))
  expect_equal(as_rvar(NULL), rvar(numeric()))
  expect_equal(draws_of(rvar()), array(numeric(), dim = c(1, 0), dimnames = list(1, NULL)))
})

# draws_of ----------------------------------------------------------------

test_that("draws_of using with_chains works", {
  # retrieving a multidimensional rvar with draws_of using with_chains
  x_array_nochains <- array(1:24, dim = c(6,2,2), dimnames = list(
    NULL, A = c("a1", "a2"), B = c("b1", "b2")
  ))
  x_array_chains <- array(1:24, dim = c(3,2,2,2), dimnames = list(
    NULL, NULL, A = c("a1", "a2"), B = c("b1", "b2")
  ))
  x <- rvar(x_array_nochains, nchains = 2)
  expect_equal(draws_of(x, with_chains = TRUE), x_array_chains)

  # setting a multidimensional rvar with draws_of using with_chains
  x2_array_nochains <- x_array_nochains + 2
  x2_array_chains <- array(1:24 + 2, dim = c(2,3,2,2), dimnames = list(
    NULL, NULL, A = c("a1", "a2"), B = c("b1", "b2")
  ))
  x2 <- x
  draws_of(x2, with_chains = TRUE) <- x2_array_chains
  expect_equal(x2, rvar(x2_array_nochains, nchains = 3))

  # retrieving a scalar rvar with draws_of using with_chains
  x2_array_nochains <- 1:24
  x2_array_chains <- array(1:24, dim = c(6,4,1), dimnames = list(NULL))
  x2 <- rvar(x2_array_nochains, nchains = 4)
  expect_equal(draws_of(x2, with_chains = TRUE), x2_array_chains)

  # setting a scalar rvar with draws_of using with_chains
  x3_array_nochains <- 1:24 + 2
  x3_array_chains <- array(1:24 + 2, dim = c(12,2), dimnames = list(NULL))
  x3 <- x2
  draws_of(x3, with_chains = TRUE) <- x3_array_chains
  expect_equal(x3, rvar(x3_array_nochains, nchains = 2))

  # NULL rvar
  expect_equal(draws_of(rvar(), with_chains = TRUE), array(numeric(), dim = c(1,1,0), dimnames = list(NULL)))

  x_null <- x
  draws_of(x_null, with_chains = TRUE) = numeric()
  expect_equal(x_null, rvar())

  # can't use with_chains when no chain dimension information provided
  expect_error(draws_of(x, with_chains = TRUE) <- 1)
})

# unique, duplicated, etc -------------------------------------------------

test_that("unique.rvar and duplicated.rvar work", {
  x <- rvar_from_array(matrix(c(1,2,1, 1,2,1, 3,3,3), nrow = 3))
  unique_x <- rvar_from_array(matrix(c(1,2, 1,2, 3,3), nrow = 2))

  expect_equal(unique(x), unique_x)
  expect_equal(as.vector(duplicated(x)), c(FALSE, FALSE, TRUE))
  expect_equal(anyDuplicated(x), 3)

  x <- rvar(array(c(1,2, 2,3, 1,2, 3,3, 1,2, 2,3), dim = c(2, 2, 3)))
  unique_x <- x
  unique_x_2 <- rvar(array(c(1,2, 2,3, 1,2, 3,3), dim = c(2, 2, 2)))
  expect_equal(unique(x), unique_x)
  expect_equal(unique(x, MARGIN = 2), unique_x_2)

  expect_error(unique(x, MARGIN = 0), "MARGIN = 0 is invalid")
  expect_error(unique(x, MARGIN = 3), "MARGIN = 3 is invalid")
})

# tibbles -----------------------------------------------------------------

test_that("rvars work in tibbles", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  x_array = array(1:20, dim = c(4,5))
  x = rvar_from_array(x_array)
  df = tibble::tibble(x, y = x + 1)

  expect_equal(df$x, x)
  expect_equal(df$y, rvar_from_array(x_array + 1))
  expect_equal(dplyr::mutate(df, z = x)$z, x)

  expect_equal(dplyr::mutate(df, z = x * 2)$z, rvar_from_array(x_array * 2))
  expect_equal(
    dplyr::mutate(dplyr::group_by(df, 1:4), z = x * 2)$z,
    rvar_from_array(x_array * 2)
  )

  df = tibble::tibble(g = letters[1:4], x)
  ref = tibble::tibble(
    a = rvar_from_array(x_array[1,, drop = FALSE]),
    b = rvar_from_array(x_array[2,, drop = FALSE]),
    c = rvar_from_array(x_array[3,, drop = FALSE]),
    d = rvar_from_array(x_array[4,, drop = FALSE])
  )
  expect_equal(tidyr::pivot_wider(df, names_from = g, values_from = x), ref)
  expect_equal(tidyr::pivot_longer(ref, a:d, names_to = "g", values_to = "x"), df)

  df$y = df$x + 1
  ref2 = tibble::tibble(
    y = df$y,
    a = c(df$x[[1]], NA, NA, NA),
    b = c(rvar(NA), df$x[[2]], NA, NA),
    c = c(rvar(NA), NA, df$x[[3]], NA),
    d = c(rvar(NA), NA, NA, df$x[[4]]),
  )
  expect_equal(tidyr::pivot_wider(df, names_from = g, values_from = x), ref2)
})

# broadcasting ------------------------------------------------------------

test_that("broadcast_array works", {
  expect_equal(broadcast_array(5, c(1,2,3,1)), array(rep(5, 6), dim = c(1,2,3,1)))
  expect_equal(
    broadcast_array(array(1:4, c(1,4), dimnames = list("x", letters[1:4])), c(2,4)),
    array(rep(1:4, each = 2), c(2,4), dimnames = list(NULL, letters[1:4]))
  )
  expect_equal(
    broadcast_array(array(1:4, c(4,1)), c(4,2)),
    array(c(1:4, 1:4), c(4,2))
  )
  expect_equal(
    broadcast_array(array(1:2, dimnames = list(c("a","b"))), c(2,1,1,1)),
    array(1:2, c(2,1,1,1), dimnames = list(c("a","b"), NULL, NULL, NULL))
  )

  expect_error(broadcast_array(array(1:9, dim = c(3,3)), c(1,9)))
  expect_error(broadcast_array(array(1:9, dim = c(3,3)), c(9)))
})


# conforming chains / draws -----------------------------------------------

test_that("warnings for unequal draws/chains are correct", {
  options(posterior.warn_on_merge_chains = TRUE)
  expect_warning(
    expect_equal(rvar(1:10) + rvar(1:10, nchains = 2), rvar(1:10 + 1:10)),
    "Chains were dropped due to chain information not matching"
  )
  options(posterior.warn_on_merge_chains = FALSE)

  expect_error(
    draws_rvars(x = rvar(1:10), y = rvar(1:11)),
    "variables have different number of draws"
  )

  expect_error(
    rvar(1:10, nchains = 0),
    "chains must be >= 1"
  )
})

# rep ---------------------------------------------------------------------

test_that("rep works", {
  x_array = array(1:10, dim = c(5,2))
  x = rvar(x_array)

  expect_equal(rep(x, times = 3), new_rvar(cbind(x_array, x_array, x_array)))
  expect_equal(rep.int(x, 3), new_rvar(cbind(x_array, x_array, x_array)))
  each_twice = cbind(x_array[,1], x_array[,1], x_array[,2], x_array[,2])
  expect_equal(rep(x, each = 2), new_rvar(each_twice))
  expect_equal(rep(x, each = 2, times = 3), new_rvar(cbind(each_twice, each_twice, each_twice)))
  expect_equal(rep(x, length.out = 3), new_rvar(cbind(x_array, x_array[,1])))
  expect_equal(rep_len(x, 3), new_rvar(cbind(x_array, x_array[,1])))
})

# all.equal ---------------------------------------------------------------------

test_that("all.equal works", {
  x_array = array(1:10, dim = c(5,2))
  x = rvar(x_array)

  expect_true(all.equal(x, x))
  expect_true(!isTRUE(all.equal(x, x + 1)))
  expect_true(!isTRUE(all.equal(x, "a")))
})

# apply functions ---------------------------------------------------------

test_that("apply family functions work", {
  x_array = array(1:24, dim = c(2,3,4))
  x = rvar(x_array)

  expect_equal(lapply(x, function(x) sum(draws_of(x))), as.list(apply(draws_of(x), 2, sum)))
  expect_equal(sapply(x, function(x) sum(draws_of(x))), apply(draws_of(x), 2, sum))
  expect_equal(vapply(x, function(x) sum(draws_of(x)), numeric(1)), apply(draws_of(x), 2, sum))
  expect_equal(apply(x, 1, function(x) sum(draws_of(x))), apply(draws_of(x), 2, sum))
  expect_equal(apply(x, 1:2, function(x) sum(draws_of(x))), apply(draws_of(x), 2:3, sum))
})
