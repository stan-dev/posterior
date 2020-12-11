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

  expect_error(rvar(x_array, .nchains = 0))
  expect_equal(rvar(x_array, .nchains = 1), rvar(x_array))
  expect_equal(nchains(rvar(x_array, .nchains = 2)), 2)
  expect_error(rvar(x_array, .nchains = 3), "Number of chains does not divide the number of draws")
})

# indexing ----------------------------------------------------------------

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

  expect_error({x2 <- x; x2[[-1]] <- 1})
  expect_error({x2 <- rvar(1:10); x2[[2]] <- c(4,5,6)})
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


# dim ---------------------------------------------------------------------

test_that("assigning NULL dim to rvar works", {
  x <- rvar(array(1:20, dim = c(2,2,5)))
  dim(x) <- NULL
  expect_equal(x, rvar(array(1:20, dim = c(2,10))))
})


# is.matrix/array ---------------------------------------------------------

test_that("is.matrix/array on rvar works", {
  x_mat <- rvar(array(1:24, dim = c(2,2,6)))
  x_arr <- rvar(array(1:24, dim = c(2,2,3,2)))

  expect_true(is.matrix(x_mat))
  expect_true(is.array(x_mat))
  expect_false(is.matrix(x_arr))
  expect_true(is.array(x_arr))
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

  expect_error(broadcast_array(array(1:9, dim = c(3,3)), c(1,9)))
  expect_error(broadcast_array(array(1:9, dim = c(3,3)), c(9)))
})


# conforming chains / draws -----------------------------------------------

test_that("warnings for unequal draws/chains are correct", {
  expect_warning(
    expect_equal(rvar(1:10) + rvar(1:10, .nchains = 2), rvar(1:10 + 1:10)),
    "chains were dropped"
  )

  expect_error(
    draws_rvars(x = rvar(1:10), y = rvar(1:11)),
    "variables have different number of draws"
  )

  expect_error(
    rvar(1:10, .nchains = 0),
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

# as.list / as.vector -----------------------------------------------------------

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

test_that("as.vector works", {
  x = rvar(array(1:12, dim = c(2, 2, 3)))
  dimnames(x) <- list(c("a","b"), c("c","d","e"))

  expect_equal(as.vector(x), rvar(array(1:12, dim = c(2, 6))))
})


# is.na -------------------------------------------------------------------

test_that("is.na works", {
  x = c(rvar(NA), 1)
  expect_equal(is.na(x), c(TRUE, FALSE))
})


# density / cdf / quantile ------------------------------------------------

test_that("distributional functions work", {
  x_values = c(2,4,3,5)
  x = rvar(x_values)

  x_density = density(x_values, cut = 0)
  expect_equal(density(x, at = x_density$x), x_density$y)

  x_cdf = ecdf(x_values)(x_values)
  expect_equal(cdf(x, x_values), x_cdf)

  expect_equal(quantile(x, 1:4/4), quantile(x_values, 1:4/4))
})


# as.data.frame -----------------------------------------------------------

test_that("as.data.frame works on rvar", {
  x = rvar(array(1:9, dim = c(3,3)))
  y = rvar(array(2:10, dim = c(3,3)))

  expect_equal(as.data.frame(y), data.frame(y = y))
  expect_equal(names(as.data.frame(y)), "y")
  x_col <- x
  dim(x_col) <- c(3,1)
  expect_equal(as.data.frame(x_col), data.frame(V1 = x))
})


# c / cbind / rbind -----------------------------------------------------------

test_that("c works on rvar", {
  x <- rvar(array(1:9, dim = c(3,3)))
  y <- rvar(array(2:10, dim = c(3,3), dimnames = list(NULL, c("a","b","c"))))
  x_y <- rvar(array(c(1:9, 2:10), dim = c(3,6), dimnames = list(NULL, c("","","","a","b","c"))))

  expect_equal(c(x), x)
  expect_equal(c(x, NULL), x)
  expect_equal(c(x, list(b = 1)), c(as.list(x), list(b = 1)))
  expect_equal(c(x, y), x_y)
  expect_equal(c(x, NULL, y), x_y)
  expect_equal(c(x, x), rvar(array(c(1:9, 1:9), dim = c(3,6))))

  expect_equal(c(z = c(x, y)),
    rvar(array(c(1:9, 2:10), dim = c(3,6), dimnames = list(NULL, c("z1","z2","z3","z.a","z.b","z.c"))))
  )

  expect_equal(c(x, 5), rvar(array(c(1:9, 5, 5, 5), dim = c(3,4))))

  x_col <- x
  dim(x_col) <- c(3,1)
  expect_equal(c(x_col), x)
  expect_equal(c(x_col, y), x_y)
})

test_that("cbind works on rvar", {
  x = rvar(array(1:9, dim = c(3,3)))
  y = rvar(array(2:10, dim = c(3,3)))

  expect_equal(cbind(rvar(array(1:9, dim = c(3,3)))), rvar(array(1:9, dim = c(3,3,1))))
  expect_equal(cbind(x), rvar(array(1:9, dim = c(3,3,1), dimnames = list(NULL, NULL, "x"))))

  expect_equal(cbind(x, y, deparse.level = 0), rvar(array(c(1:9, 2:10), dim = c(3,3,2))))
  expect_equal(cbind(a = x, y, deparse.level = 0),
    rvar(array(c(1:9, 2:10), dim = c(3,3,2), dimnames = list(NULL, NULL, c("a", ""))))
  )
  expect_equal(cbind(a = x, y),
    rvar(array(c(1:9, 2:10), dim = c(3,3,2), dimnames = list(NULL, NULL, c("a", "y"))))
  )
  expect_equal(cbind(x, b = y, deparse.level = 0),
    rvar(array(c(1:9, 2:10), dim = c(3,3,2), dimnames = list(NULL, NULL, c("", "b"))))
  )
  expect_equal(cbind(x, y + 1, deparse.level = 2),
    rvar(array(c(1:9, 2:10 + 1), dim = c(3,3,2), dimnames = list(NULL, NULL, c("x", "y + 1"))))
  )
  expect_equal(cbind(x, y, y + 1, deparse.level = 2),
    rvar(array(c(1:9, 2:10, 2:10 + 1), dim = c(3,3,3), dimnames = list(NULL, NULL, c("x", "y", "y + 1"))))
  )
  expect_equal(cbind(x, y, y + 1, deparse.level = 2), cbind(cbind(x, y), y + 1, deparse.level = 2))

  x_col <- x
  dim(x_col) <- c(3,1)
  expect_equal(cbind(x_col, y, deparse.level = 0), rvar(array(c(1:9, 2:10), dim = c(3,3,2))))
  expect_equal(cbind(a = x_col, y),
    rvar(array(c(1:9, 2:10), dim = c(3,3,2), dimnames = list(NULL, NULL, c("", "y"))))
  )
  dimnames(x_col)[[2]] = "b"
  expect_equal(cbind(a = x_col, y),
    rvar(array(c(1:9, 2:10), dim = c(3,3,2), dimnames = list(NULL, NULL, c("b", "y"))))
  )

  expect_equal(cbind(NULL, x), cbind(x))
  expect_equal(cbind(x, NULL, y), cbind(x, y))

  expect_equal(cbind(data.frame(x), y + 1), data.frame(x = x, `y + 1` = y + 1, check.names = FALSE))
  expect_equal(cbind(x + 1, data.frame(y)), data.frame(`x + 1` = x + 1, y = y, check.names = FALSE))
})

test_that("rbind works on rvar", {
  x <- rvar(array(1:9, dim = c(3,3)))
  y <- rvar(array(2:10, dim = c(3,3)))
  x_y_array <- abind(
      array(1:9, dim = c(3,1,3)),
      array(2:10, dim = c(3,1,3)),
      along = 2
    )
  x_yp1_array <- abind(
    array(1:9, dim = c(3,1,3)),
    array(2:10 + 1, dim = c(3,1,3)),
    along = 2
  )

  expect_equal(rbind(rvar(array(1:9, dim = c(3,3)))), rvar(array(1:9, dim = c(3,1,3))))
  expect_equal(rbind(x), rvar(array(1:9, dim = c(3,1,3), dimnames = list(NULL, "x", NULL))))

  expect_equal(rbind(x, y, deparse.level = 0), rvar(x_y_array))
  expect_equal(rbind(a = x, y, deparse.level = 0), rvar(x_y_array, dimnames = list(c("a",""), NULL)))
  expect_equal(rbind(a = x, y), rvar(x_y_array, dimnames = list(c("a","y"), NULL)))
  expect_equal(rbind(x, b = y, deparse.level = 0), rvar(x_y_array, dimnames = list(c("","b"), NULL)))
  expect_equal(rbind(x, y + 1, deparse.level = 2), rvar(x_yp1_array, dimnames = list(c("x","y + 1"), NULL)))
  expect_equal(rbind(x, y, y + 1, deparse.level = 2), rbind(rbind(x, y), y + 1, deparse.level = 2))

  x_row <- x
  dim(x_row) <- c(1,3)
  expect_equal(rbind(x_row, y, deparse.level = 0), rvar(x_y_array))
  expect_equal(rbind(a = x_row, y), rvar(x_y_array, dimnames = list(c("","y"), NULL)))
  dimnames(x_row)[[1]] = "b"
  expect_equal(rbind(a = x_row, y), rvar(x_y_array, dimnames = list(c("b","y"), NULL)))

  expect_equal(rbind(NULL, x), rbind(x))
  expect_equal(rbind(x, NULL, y), rbind(x, y))

  expect_equal(rbind(data.frame(x), data.frame(x = y)), data.frame(x = c(x, y)))
})

