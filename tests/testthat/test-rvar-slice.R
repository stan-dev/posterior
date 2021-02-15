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

  # logical index the length of the array works
  flat_index <- c(
    TRUE,FALSE,FALSE,
    FALSE,TRUE,FALSE,
    TRUE,TRUE, FALSE,
    FALSE,TRUE,TRUE)
  x_array_flat <- x_array
  dim(x_array_flat) <- c(12,2)
  expect_equal(x[flat_index], rvar_from_array(x_array_flat[flat_index,]))

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
  x2 = new_rvar(array(1:2, dim = c(1,2)))
  expect_equal(
    {x2[1] <- new_rvar(array(1:2, dim = c(2,1))); x2},
    new_rvar(array(c(1,2,2,2), dim = c(2,2)))
  )

  # logical index the length of the array works
  flat_index <- c(
    TRUE,FALSE,FALSE,
    FALSE,TRUE,FALSE,
    TRUE,TRUE, FALSE,
    FALSE,TRUE,TRUE)
  x_array_flat <- x_array
  dim(x_array_flat) <- c(2,12)
  x_array_flat[,flat_index] <- rep(1:6, each = 2)
  dim(x_array_flat) <- c(2,4,3)
  dimnames(x_array_flat) <- list(NULL, a = 1:4, b = 1:3)
  expect_equal(
    {x2 <- x; dimnames(x2) <- list(a = 1:4, b = 1:3); x2[flat_index] <- 1:6; x2},
    new_rvar(x_array_flat)
  )

})
