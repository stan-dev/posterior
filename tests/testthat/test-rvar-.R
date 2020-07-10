# indexing ----------------------------------------------------------------

# function for making rvars from arrays that expects last index to be
# draws (for testing so that when array structure changes tests don't have to)
rvar_from_array = function(x) {
  .dim = dim(x)
  last_dim = length(.dim)
  new_rvar(aperm(x, c(last_dim, seq_len(last_dim - 1))))
}

test_that("indexing with [[ works on a vector", {
  x_array <- array(1:20, dim = c(5,4), dimnames = list(NULL, A = paste0("a", 1:4)))
  x = new_rvar(x_array)

  # [[ indexing should drop names (but not indices)
  x_array_ref = x_array
  dimnames(x_array_ref) <- NULL

  expect_identical(x[[3]], new_rvar(x_array_ref[,3, drop = FALSE]))
  expect_identical(x[["a2"]], new_rvar(x_array_ref[,2, drop = FALSE]))

  expect_error(x[[]])
  expect_error(x[[NA]])
  expect_error(x[[NA_integer_]])
  expect_error(x[[6]])
  expect_error(x[[1,1]])
  expect_error(x[[1,1,1]])
  expect_error(x[[NULL]])
  expect_error(x[[-1]])

  # different behavior from base vectors
  # base vectors convert these to numeric
  expect_error(x[[TRUE]])
  expect_error(x[[FALSE]])
})

test_that("indexing with [[ works on a matrix", {
  x_array = array(
    1:24, dim = c(2,4,3),
    dimnames = list(NULL, A = paste0("a", 1:4), B = paste0("b", 1:3))
  )
  x = new_rvar(x_array)

  x_array_ref = x_array
  dim(x_array_ref) <- c(2,12)

  expect_identical(x[[2]], new_rvar(x_array_ref[,2, drop = TRUE]))
  expect_identical(x[[12]], new_rvar(x_array_ref[,12, drop = TRUE]))
  expect_identical(x[[2,3]], new_rvar(x_array[,2,3, drop = TRUE]))

  # invalid indexing should result in errors
  expect_error(x[[1,]])
  expect_error(x[[1,1,1]])
  expect_error(x[[13]])

  # different from base vectors
  # don't allow name-based [[ indexing on 2+D arrays
  expect_error(x[["a2"]])

  # extending a NULL rvar should work...
  x_null = rvar()
  x_null[[1]] <- 5
  expect_equal(x_null, rvar(5))
})

test_that("assignment with [[ works", {
  x_array = array(
    1:24, dim = c(2,4,3),
    dimnames = list(NULL, A = paste0("a", 1:4), B = paste0("b", 1:3))
  )
  x = new_rvar(x_array)

  expect_identical(
    {x2 <- x; x2[[2]] <- 1; x2},
    new_rvar({xr <- x_array; xr[,2,1] <- 1; xr})
  )
  expect_identical(
    {x2 <- x; x2[[12]] <- 1; x2},
    new_rvar({xr <- x_array; xr[,4,3] <- 1; xr})
  )
  expect_identical(
    {x2 <- x; x2[[12]] <- new_rvar(c(1,2)); x2},
    new_rvar({xr <- x_array; xr[,4,3] <- c(1,2); xr})
  )
  expect_identical(
    {x2 <- x; x2[["a2","b3"]] <- new_rvar(c(1,2)); x2},
    new_rvar({xr <- x_array; xr[,2,3] <- c(1,2); xr})
  )

  # constant should have ndraws increased to value when assigned to
  x = new_rvar(array(1:2, dim = c(1,2)))
  expect_equal(
    {x[[1]] <- new_rvar(array(1:2, dim = c(2,1))); x},
    new_rvar(array(c(1,2,2,2), dim = c(2,2)))
  )
})

test_that("indexing with [ works on a vector", {
  x_array = array(1:20, dim = c(4,5), dimnames = list(A = paste0("a", 1:4), NULL))
  x = rvar_from_array(x_array)

  expect_identical(x[], x)

  expect_identical(x[3], rvar_from_array(x_array[3,, drop = FALSE]))
  expect_identical(x["a2"], rvar_from_array(x_array["a2",, drop = FALSE]))
  expect_identical(x[c(1,3)], rvar_from_array(x_array[c(1,3),, drop = FALSE]))
  expect_identical(x[c("a2","a4")], rvar_from_array(x_array[c("a2","a4"),, drop = FALSE]))

  expect_identical(x[c(-1,-3)], rvar_from_array(x_array[c(-1,-3),, drop = FALSE]))

  expect_identical(x[TRUE], rvar_from_array(x_array[TRUE,, drop = FALSE]))
  expect_identical(x[c(TRUE,FALSE)], rvar_from_array(x_array[c(TRUE,FALSE),, drop = FALSE]))
  expect_identical(x[c(TRUE,FALSE,TRUE)], rvar_from_array(x_array[c(TRUE,FALSE,TRUE),, drop = FALSE]))
  expect_identical(x[c(TRUE,FALSE,FALSE,TRUE)], rvar_from_array(x_array[c(TRUE,FALSE,FALSE,TRUE),, drop = FALSE]))

  # dropping should preserve names (hence the drop = FALSE on x_array for this test)
  expect_identical(x["a1", drop = TRUE], rvar_from_array(x_array["a1",, drop = FALSE]))
  expect_identical(x[1:2, drop = TRUE], rvar_from_array(x_array[1:2,, drop = FALSE]))

  # indexing beyond the end of the array should result in NAs, to mimic normal vector indexing
  expect_identical(x[c(4,5)], rvar_from_array(x_array[c(4,NA_integer_),, drop = FALSE]))
  expect_identical(x[c(8,9)], rvar_from_array(x_array[c(NA_integer_,NA_integer_),, drop = FALSE]))

  expect_identical(x[NA], rvar_from_array(x_array[NA,, drop = FALSE]))
  expect_identical(x[NA_integer_], rvar_from_array(x_array[NA_integer_,, drop = FALSE]))
  expect_identical(x[rep(NA_integer_,7)], rvar_from_array(x_array[rep(NA_integer_,7),, drop = FALSE]))

  expect_identical(x[NULL], new_rvar())

  expect_error(x[1,1])

  # extending a NULL rvar should work...
  x_null = rvar()
  x_null[1] <- 5
  expect_equal(x_null, rvar(5))
})

test_that("indexing with [ works on a matrix", {
  x_array = array(
    1:24, dim = c(4,3,2),
    dimnames = list(A = paste0("a", 1:4), B = paste0("b", 1:3))
  )
  x = rvar_from_array(x_array)

  expect_identical(x[], x)

  expect_identical(x[2], rvar_from_array(x_array[2,,, drop = FALSE]))
  expect_identical(x["a2"], rvar_from_array(x_array["a2",,, drop = FALSE]))
  expect_identical(x[c(1,2)], rvar_from_array(x_array[c(1,2),,, drop = FALSE]))
  expect_identical(x[,c(1,3)], rvar_from_array(x_array[,c(1,3),, drop = FALSE]))
  expect_identical(x[,c("b2","b3")], rvar_from_array(x_array[,c("b2","b3"),, drop = FALSE]))

  expect_identical(x[,c(-1,-3)], rvar_from_array(x_array[,c(-1,-3),, drop = FALSE]))

  expect_identical(x[TRUE], rvar_from_array(x_array[TRUE,,, drop = FALSE]))
  expect_identical(x[c(TRUE,FALSE)], rvar_from_array(x_array[c(TRUE,FALSE),,, drop = FALSE]))
  expect_identical(x[c(TRUE,FALSE,TRUE)], rvar_from_array(x_array[c(TRUE,FALSE,TRUE),,, drop = FALSE]))
  expect_identical(x[c(TRUE,FALSE,FALSE,TRUE)], rvar_from_array(x_array[c(TRUE,FALSE,FALSE,TRUE),,, drop = FALSE]))

  # dropping works
  expect_identical(x["a1", drop = TRUE], rvar_from_array(drop(x_array["a1",,, drop = TRUE])))
  expect_identical(x[1:2, drop = TRUE], rvar_from_array(x_array[1:2,,, drop = TRUE]))

  # indexing beyond the end of the array should result in NAs, to mimic normal vector indexing
  expect_identical(x[c(4,5)], rvar_from_array(x_array[c(4,NA_integer_),,, drop = FALSE]))
  expect_identical(x[c(8,9)], rvar_from_array(x_array[c(NA_integer_,NA_integer_),,, drop = FALSE]))

  expect_identical(x[NA], rvar_from_array(x_array[NA,,, drop = FALSE]))
  expect_identical(x[NA_integer_], rvar_from_array(x_array[NA_integer_,,, drop = FALSE]))
  expect_identical(x[rep(NA_integer_,7)], rvar_from_array(x_array[rep(NA_integer_,7),,, drop = FALSE]))

  expect_identical(x[NULL], new_rvar())

  expect_error(x[1,1,1])
})

test_that("assignment with [ works", {
  x_array = array(
    1:24, dim = c(2,4,3),
    dimnames = list(NULL, A = paste0("a", 1:4), B = paste0("b", 1:3))
  )
  x = new_rvar(x_array)

  expect_identical(
    {x2 <- x; x2[2,1] <- 1; x2},
    new_rvar({xr <- x_array; xr[,2,1] <- 1; xr})
  )
  expect_identical(
    {x2 <- x; x2[2,] <- 1; x2},
    new_rvar({xr <- x_array; xr[,2,] <- 1; xr})
  )
  expect_identical(
    {x2 <- x; x2[,2] <- new_rvar(c(1,2)); x2},
    new_rvar({xr <- x_array; xr[,,2] <- c(1,2); xr})
  )
  expect_identical(
    {x2 <- x; x2["a2","b3"] <- new_rvar(c(1,2)); x2},
    new_rvar({xr <- x_array; xr[,2,3] <- c(1,2); xr})
  )

  # constant should have ndraws increased to value when assigned to
  x = new_rvar(array(1:2, dim = c(1,2)))
  expect_equal(
    {x[1] <- new_rvar(array(1:2, dim = c(2,1))); x},
    new_rvar(array(c(1,2,2,2), dim = c(2,2)))
  )
})

# unique, duplicated, etc -------------------------------------------------

test_that("unique.rvar and duplicated.rvar work", {
  x <- rvar_from_array(matrix(c(1,2,1, 1,2,1, 3,3,3), nrow = 3))
  unique_x <- rvar_from_array(matrix(c(1,2, 1,2, 3,3), nrow = 2))

  expect_equal(unique(x), unique_x)
  expect_equal(as.vector(duplicated(x)), c(FALSE, FALSE, TRUE))
  expect_equal(anyDuplicated(x), 3)
})


# tibbles -----------------------------------------------------------------

test_that("rvars work in tibbles", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  x_array = array(1:20, dim = c(4,5))
  x = rvar_from_array(x_array)
  df = tibble::tibble(x, y = x + 1)

  expect_identical(df$x, x)
  expect_identical(df$y, rvar_from_array(x_array + 1))
  expect_identical(dplyr::mutate(df, z = x)$z, x)

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
    broadcast_array(array(1:4, c(1,4)), c(2,4)),
    array(rep(1:4, each = 2), c(2,4))
  )
  expect_equal(
    broadcast_array(array(1:4, c(4,1)), c(4,2)),
    array(c(1:4, 1:4), c(4,2))
  )
})

# rep ---------------------------------------------------------------------

test_that("rep works", {
  x_array = array(1:10, dim = c(5,2))
  x = rvar(x_array)

  expect_equal(rep(x, times = 3), new_rvar(cbind(x_array, x_array, x_array)))
  each_twice = cbind(x_array[,1], x_array[,1], x_array[,2], x_array[,2])
  expect_equal(rep(x, each = 2), new_rvar(each_twice))
  expect_equal(rep(x, each = 2, times = 3), new_rvar(cbind(each_twice, each_twice, each_twice)))
  expect_equal(rep(x, length.out = 3), new_rvar(cbind(x_array, x_array[,1])))
})

# all.equal ---------------------------------------------------------------------

test_that("all.equal works", {
  x_array = array(1:10, dim = c(5,2))
  x = rvar(x_array)

  expect_true(all.equal(x, x))
  expect_true(!isTRUE(all.equal(x, x + 1)))
})

# as.list ---------------------------------------------------------------------

test_that("as.list works", {
  x_array = array(
    1:24, dim = c(2,4,3),
    dimnames = list(NULL, A = paste0("a", 1:4), B = paste0("b", 1:3))
  )
  x = new_rvar(x_array)

  expect_equal(as.list(x),
    list(
      a1 = new_rvar(x_array[,1,]),
      a2 = new_rvar(x_array[,2,]),
      a3 = new_rvar(x_array[,3,]),
      a4 = new_rvar(x_array[,4,])
    )
  )
})


# is.na -------------------------------------------------------------------

test_that("is.na works", {
  x = c(rvar(NA), 1)
  expect_equal(is.na(x), c(TRUE, FALSE))
})
