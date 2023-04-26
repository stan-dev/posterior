# creating factor rvars ----------------------------------------------------------

test_that("0-length rvar factors are constructured correctly", {
  ref <- factor()
  dim(ref) <- c(1, 0)
  dimnames(ref) <- list("1", NULL)
  expect_equal(draws_of(rvar(factor())), ref)
  expect_equal(draws_of(rvar(character())), ref)

  oldClass(ref) <- c("ordered", "factor")
  expect_equal(draws_of(rvar(ordered(NULL))), ref)
})

test_that("rvar_factor() input with a levels attribute preserves levels", {
  x_array <- matrix(1:10, nrow = 2)
  attr(x_array, "levels") <- letters[1:10]

  ref_draws <- factor(letters[1:10])
  dim(ref_draws) <- c(2, 5)
  dimnames(ref_draws) <- list(1:2, NULL)

  x <- rvar_factor(x_array)
  expect_equal(draws_of(x), ref_draws)
  expect_true(inherits(x, "rvar_factor"))
})

# unique, duplicated, etc -------------------------------------------------

test_that("unique.rvar_factor and duplicated.rvar_factor work", {
  x <- rvar(matrix(letters[c(1,1,3, 2,2,3, 1,1,3)], nrow = 3))
  unique_x <- rvar(matrix(letters[c(1,1,3, 2,2,3)], nrow = 3))

  expect_equal(unique(x), unique_x)
  expect_equal(as.vector(duplicated(x)), c(FALSE, FALSE, TRUE))
  expect_equal(anyDuplicated(x), 3)

  x <- rvar(array(letters[c(1,2, 2,3, 1,2, 3,3, 1,2, 2,3)], dim = c(2, 2, 3)))
  unique_x <- x
  unique_x_2 <- rvar(array(letters[c(1,2, 2,3, 1,2, 3,3)], dim = c(2, 2, 2)))
  expect_equal(unique(x), unique_x)
  expect_equal(unique(x, MARGIN = 2), unique_x_2)
})

# tibbles -----------------------------------------------------------------

test_that("rvar_factors work in tibbles", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  x_array <- factor(letters[1:20])
  dim(x_array) <- c(5,4)
  x_array_ordered <- ordered(x_array)
  dim(x_array_ordered) <- c(5,4)
  x = rvar(x_array)
  df = tibble::tibble(x, y = as_rvar_ordered(x))

  expect_equal(df$x, x)
  expect_equal(df$y, rvar(x_array_ordered))
  expect_equal(dplyr::mutate(df, z = x)$z, x)

  expect_equal(dplyr::mutate(df, z = as_rvar_ordered(x))$z, rvar(x_array_ordered))
  expect_equal(
    dplyr::group_by(dplyr::mutate(df, z = as_rvar_ordered(x)), 1:4)$z,
    rvar(x_array_ordered)
  )

  df = tibble::tibble(g = letters[1:4], x)
  ref = tibble::tibble(
    a = rvar(x_array[,1, drop = FALSE]),
    b = rvar(x_array[,2, drop = FALSE]),
    c = rvar(x_array[,3, drop = FALSE]),
    d = rvar(x_array[,4, drop = FALSE])
  )
  expect_equal(tidyr::pivot_wider(df, names_from = g, values_from = x), ref)
  expect_equal(tidyr::pivot_longer(ref, a:d, names_to = "g", values_to = "x"), df)

  df$y = as_rvar_ordered(df$x)
  ref2 = tibble::tibble(
    y = df$y,
    a = c(df$x[[1]], NA, NA, NA),
    b = c(rvar(factor(NA)), df$x[[2]], NA, NA),
    c = c(rvar(factor(NA)), NA, df$x[[3]], NA),
    d = c(rvar(factor(NA)), NA, NA, df$x[[4]]),
  )
  expect_equal(tidyr::pivot_wider(df, names_from = g, values_from = x), ref2)
})

# rep ---------------------------------------------------------------------

test_that("rep works", {
  x_array = array(letters[1:10], dim = c(5,2))
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
  x_array <- array(letters[1:10], dim = c(5,2))
  x <- rvar(x_array)
  x_array_ordered <- ordered(x_array)
  dim(x_array_ordered) <- c(5,2)
  x_ordered <- rvar(x_array_ordered)

  expect_true(all.equal(x, x))
  expect_true(!isTRUE(all.equal(x, x_ordered)))
  expect_true(!isTRUE(all.equal(x, "a")))
})
