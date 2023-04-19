# function for making rvars from arrays that expects last index to be
# draws (for testing so that when array structure changes tests don't have to)
rvar_from_array = function(x) {
  .dim = dim(x)
  last_dim = length(.dim)
  new_rvar(aperm(x, c(last_dim, seq_len(last_dim - 1))))
}


# [[ indexing -------------------------------------------------------------

test_that("indexing with [[ works on a vector", {
  x_array <- array(1:20, dim = c(5,4), dimnames = list(NULL, A = paste0("a", 1:4)))
  x = new_rvar(x_array)

  # [[ indexing should drop names (but not indices)
  x_array_ref = x_array
  dimnames(x_array_ref) <- NULL

  expect_equal(x[[3]], new_rvar(x_array_ref[,3, drop = FALSE]))
  expect_equal(x[["a2"]], new_rvar(x_array_ref[,2, drop = FALSE]))

  expect_error(x[[]])
  expect_error(x[[NA]], "Missing indices not allowed")
  expect_error(x[[NA_integer_]], "Missing indices not allowed")
  expect_error(x[[6]], "out of bounds")
  expect_error(x[[1,1]], "out of bounds")
  expect_error(x[[1,1,1]], "out of bounds")
  expect_error(x[[NULL]], "Cannot select zero elements")
  expect_error(x[[1:2]], "Cannot select more than one element")
  expect_error(x[[-1]], "out of bounds")

  # different behavior from base vectors
  # base vectors convert these to numeric
  expect_error(x[[TRUE]], "Logical indices not allowed")
  expect_error(x[[FALSE]], "Logical indices not allowed")
})

test_that("indexing with [[ works on a matrix", {
  x_array = array(
    1:24, dim = c(2,4,3),
    dimnames = list(NULL, A = paste0("a", 1:4), B = paste0("b", 1:3))
  )
  x = new_rvar(x_array)

  x_array_ref = x_array
  dim(x_array_ref) <- c(2,12)

  expect_equal(x[[2]], new_rvar(x_array_ref[,2, drop = TRUE]))
  expect_equal(x[[12]], new_rvar(x_array_ref[,12, drop = TRUE]))
  expect_equal(x[[2,3]], new_rvar(x_array[,2,3, drop = TRUE]))

  # invalid indexing should result in errors
  expect_error(x[[1,]], "Missing indices not allowed")
  expect_error(x[[1,1,1]], "out of bounds")
  expect_error(x[[13]], "out of bounds")

  # different from base vectors
  # don't allow name-based [[ indexing on 2+D arrays
  expect_error(x[["a2"]])

  # extending a NULL rvar should work...
  x_null = rvar()
  x_null[[1]] <- 5
  expect_equal(x_null, rvar(5))
})


# [[ assignment -----------------------------------------------------------

test_that("assignment with [[ works", {
  x_array = array(
    1:24, dim = c(2,4,3),
    dimnames = list(NULL, A = paste0("a", 1:4), B = paste0("b", 1:3))
  )
  x = new_rvar(x_array)

  expect_equal(
    {x2 <- x; x2[[2]] <- 1; x2},
    new_rvar({xr <- x_array; xr[,2,1] <- 1; xr})
  )
  expect_equal(
    {x2 <- x; x2[[12]] <- 1; x2},
    new_rvar({xr <- x_array; xr[,4,3] <- 1; xr})
  )
  expect_equal(
    {x2 <- x; x2[[12]] <- new_rvar(c(1,2)); x2},
    new_rvar({xr <- x_array; xr[,4,3] <- c(1,2); xr})
  )
  expect_equal(
    {x2 <- x; x2[["a2","b3"]] <- new_rvar(c(1,2)); x2},
    new_rvar({xr <- x_array; xr[,2,3] <- c(1,2); xr})
  )

  expect_error({x2 <- x; x2[[1,1,1]] <- 1}, "out of bounds")

  # constant should have ndraws increased to value when assigned to
  x = new_rvar(array(1:2, dim = c(1,2)))
  expect_equal(
    {x[[1]] <- new_rvar(array(1:2, dim = c(2,1))); x},
    new_rvar(array(c(1,2,2,2), dim = c(2,2)))
  )

  expect_error({x2 <- x; x2[[-1]] <- 1}, "out of bounds")
  expect_error({x2 <- rvar(1:10); x2[[2]] <- c(4,5,6)})
})


# [ indexing --------------------------------------------------------------

test_that("indexing with [ works on a vector", {
  x_array = array(1:20, dim = c(4,5), dimnames = list(A = paste0("a", 1:4), NULL))
  x = rvar_from_array(x_array)

  expect_equal(x[], x)

  expect_equal(x[3], rvar_from_array(x_array[3,, drop = FALSE]))
  expect_equal(x["a2"], rvar_from_array(x_array["a2",, drop = FALSE]))
  expect_equal(x[c(1,3)], rvar_from_array(x_array[c(1,3),, drop = FALSE]))
  expect_equal(x[c("a2","a4")], rvar_from_array(x_array[c("a2","a4"),, drop = FALSE]))

  expect_equal(x[c(-1,-3)], rvar_from_array(x_array[c(-1,-3),, drop = FALSE]))

  expect_equal(x[TRUE], rvar_from_array(x_array[TRUE,, drop = FALSE]))
  expect_equal(x[c(TRUE,FALSE)], rvar_from_array(x_array[c(TRUE,FALSE),, drop = FALSE]))
  expect_equal(x[c(TRUE,FALSE,TRUE)], rvar_from_array(x_array[c(TRUE,FALSE,TRUE),, drop = FALSE]))
  expect_equal(x[c(TRUE,FALSE,FALSE,TRUE)], rvar_from_array(x_array[c(TRUE,FALSE,FALSE,TRUE),, drop = FALSE]))

  # dropping should preserve names (hence the drop = FALSE on x_array for this test)
  expect_equal(x["a1", drop = TRUE], rvar_from_array(x_array["a1",, drop = FALSE]))
  expect_equal(x[1:2, drop = TRUE], rvar_from_array(x_array[1:2,, drop = FALSE]))

  # indexing beyond the end of the array should result in NAs, to mimic normal vector indexing
  expect_equal(x[c(4,5)], rvar_from_array(x_array[c(4,NA_integer_),, drop = FALSE]))
  expect_equal(x[c(8,9)], rvar_from_array(x_array[c(NA_integer_,NA_integer_),, drop = FALSE]))

  expect_equal(x[NA], rvar_from_array(x_array[NA,, drop = FALSE]))
  expect_equal(x[NA_integer_], rvar_from_array(x_array[NA_integer_,, drop = FALSE]))
  expect_equal(x[rep(NA_integer_,7)], rvar_from_array(x_array[rep(NA_integer_,7),, drop = FALSE]))

  expect_equal(x[NULL], new_rvar(array(numeric(), dim = c(5, 0))))

  expect_error(x[1,1])

  # extending a NULL rvar should work...
  x_null = rvar()
  x_null[1] <- 5
  expect_equal(x_null, rvar(5))
})

test_that("indexing with [ works on an array", {
  x_array = array(
    1:24, dim = c(4,3,2),
    dimnames = list(A = paste0("a", 1:4), B = paste0("b", 1:3))
  )
  x = rvar_from_array(x_array)

  expect_equal(x[], x)

  expect_equal(x[2], rvar_from_array(array(x_array[2,1,], dim = c(1,2))))
  expect_equal(x[2,], rvar_from_array(x_array[2,,, drop = FALSE]))
  expect_equal(x["a2",], rvar_from_array(x_array["a2",,, drop = FALSE]))
  expect_equal(x[c(1,2)], rvar_from_array(array(x_array[c(1,2),1,], dim = c(2,2))))
  expect_equal(x[c(1,2),], rvar_from_array(x_array[c(1,2),,, drop = FALSE]))
  expect_equal(x[,c(1,3)], rvar_from_array(x_array[,c(1,3),, drop = FALSE]))
  expect_equal(x[,c("b2","b3")], rvar_from_array(x_array[,c("b2","b3"),, drop = FALSE]))

  expect_equal(x[,c(-1,-3)], rvar_from_array(x_array[,c(-1,-3),, drop = FALSE]))

  expect_equal(x[TRUE], rvar_from_array(array(x_array, dim = c(12,2))))
  expect_equal(x[c(TRUE,FALSE)], rvar_from_array(array(x_array, dim = c(12,2))[c(TRUE,FALSE),]))
  expect_equal(x[c(TRUE,FALSE,TRUE)], rvar_from_array(array(x_array, dim = c(12,2))[c(TRUE,FALSE,TRUE),]))
  expect_equal(x[c(TRUE,FALSE,FALSE,TRUE)], rvar_from_array(array(x_array, dim = c(12,2))[c(TRUE,FALSE,FALSE,TRUE),]))

  expect_equal(x[TRUE,], rvar_from_array(x_array[TRUE,,, drop = FALSE]))
  expect_equal(x[c(TRUE,FALSE),], rvar_from_array(x_array[c(TRUE,FALSE),,, drop = FALSE]))
  expect_equal(x[c(TRUE,FALSE,TRUE),], rvar_from_array(x_array[c(TRUE,FALSE,TRUE),,, drop = FALSE]))
  expect_equal(x[c(TRUE,FALSE,FALSE,TRUE),], rvar_from_array(x_array[c(TRUE,FALSE,FALSE,TRUE),,, drop = FALSE]))

  # dropping works
  expect_equal(x["a1",, drop = TRUE], rvar_from_array(x_array["a1",,, drop = TRUE]))
  expect_equal(x[1:2,, drop = TRUE], rvar_from_array(x_array[1:2,,, drop = TRUE]))
  expect_equal(x[1,2, drop = TRUE], rvar_from_array(array(x_array[1,2,], dim = c(1,2))))
  expect_equal(x[1,1:2, drop = TRUE], rvar_from_array(x_array[1,1:2,, drop = TRUE]))

  # indexing beyond the end of the array should result in NAs, to mimic normal vector indexing
  expect_equal(x[c(4,25)], rvar_from_array(array(x_array[c(4,NA_integer_),1,], dim = c(2,2))))
  expect_equal(x[c(4,5),], rvar_from_array(x_array[c(4,NA_integer_),,, drop = FALSE]))
  expect_equal(x[c(8,9),], rvar_from_array(x_array[c(NA_integer_,NA_integer_),,, drop = FALSE]))

  expect_equal(x[NA], rvar_from_array(array(x_array[NA,,], dim = c(12,2))))
  expect_equal(x[NA,], rvar_from_array(x_array[NA,,, drop = FALSE]))
  expect_equal(x[NA_integer_], rvar_from_array(array(c(NA_integer_,NA_integer_), dim = c(1,2))))
  expect_equal(x[NA_integer_,], rvar_from_array(x_array[NA_integer_,,, drop = FALSE]))
  expect_equal(x[rep(NA_integer_,7)], rvar_from_array(array(rep(NA_integer_,14), dim = c(7,2))))

  expect_equal(x[NULL], new_rvar(array(numeric(), dim = c(2, 0))))

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

  # matrix indexing with an array
  x_array <- array(1:24, dim = c(2,2,3,2))
  x <- rvar_from_array(x_array)
  expect_equal(x[rbind(c(1,2,3),c(2,2,3),c(2,1,1))], x[c(11,12,2)])

  # indexing while leaving remaining indices to be filled in automatically
  expect_equal(x[1,], x[1,,])
})

test_that("indexing with x[<rvar>] for logical index works", {
  .dimnames <- list(1:4, A = paste0("a", 1:3), B = paste0("b", 1:2))
  x_array <- array(1:24, dim = c(4,3,2), dimnames = .dimnames)
  x <- rvar(x_array, nchains = 2)
  x_1_chain <- rvar(x_array)

  expect_equal(x[rvar(FALSE)], rvar())
  expect_equal(x[rvar(NA)], rvar(array(
    rep(NA_integer_, 24), dim = c(4,3,2), dimnames = c(list(rep(NA, 4)), .dimnames[-1])
  )))
  expect_equal(x[rvar(TRUE)], x_1_chain)
  expect_equal(x[rvar(c(TRUE,FALSE,TRUE,FALSE))], rvar(x_array[c(TRUE,FALSE,TRUE,FALSE),,]))

  expect_error(x[rvar(1:4)], "scalar logical")
  expect_error(x[rvar(c(TRUE,TRUE))], "different number of draws")
  expect_error(x[rvar(c(TRUE,TRUE,TRUE,TRUE,TRUE))], "different number of draws")
  expect_error(x[as_rvar(c(TRUE,FALSE))], "scalar logical")
})


# [ assignment ------------------------------------------------------------

test_that("assignment with [ works", {
  x_array = array(
    1:24, dim = c(2,4,3),
    dimnames = list(NULL, A = paste0("a", 1:4), B = paste0("b", 1:3))
  )
  x = new_rvar(x_array)

  expect_equal(
    {x2 <- x; x2[2,1] <- 1; x2},
    new_rvar({xr <- x_array; xr[,2,1] <- 1; xr})
  )
  expect_equal(
    {x2 <- x; x2[2,] <- 1; x2},
    new_rvar({xr <- x_array; xr[,2,] <- 1; xr})
  )
  expect_equal(
    {x2 <- x; x2[,2] <- new_rvar(c(1,2)); x2},
    new_rvar({xr <- x_array; xr[,,2] <- c(1,2); xr})
  )
  expect_equal(
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

  # matrix indexing assignment and unidimensional index assignment with an array works
  x_array <- array(
    1:24, dim = c(2,2,3,2),
    dimnames = list(A = paste0("a", 1:2), B = paste0("b", 1:2), C = paste0("c", 1:3))
  )

  x_ref <- rvar_from_array(x_array)
  x_ref[1,2,3] <- rvar(1:2)
  x_ref[2,2,3] <- rvar(3:4)
  x_ref[2,1,1] <- rvar(5:6)

  x <- rvar_from_array(x_array)
  x[rbind(c(1,2,3),c(2,2,3),c(2,1,1))] <- rvar(matrix(1:6, nrow = 2))
  expect_equal(x, x_ref)

  x <- rvar_from_array(x_array)
  x[c(11,12,2)] <- rvar(matrix(1:6, nrow = 2))
  expect_equal(x, x_ref)
})

test_that("assignment with x[<rvar>] for logical index works", {
  .dimnames <- list(1:4, A = paste0("a", 1:3), B = paste0("b", 1:2))
  x_array <- array(1:24, dim = c(4,3,2), dimnames = .dimnames)
  x <- rvar(x_array, nchains = 2)
  x_1_chain <- rvar(x_array)

  expect_equal({x2 <- x; x2[rvar(FALSE)] <- 1; x2}, x)
  expect_equal({x2 <- x; x2[rvar(NA)] <- 1; x2}, x)
  expect_equal({x2 <- x; x2[rvar(TRUE)] <- x_1_chain + 1; x2}, x + 1)

  ref_array <- x_array
  ref_array[1,,] <- 99:101
  ref_array[3,,] <- 99:101
  ref <- rvar(ref_array, nchains = 2)
  expect_equal({x2 <- x; x2[rvar(c(TRUE,FALSE,TRUE,FALSE))] <- 99:101; x2}, ref)
  ref_array[1,,] <- 99
  ref_array[3,,] <- 100
  ref <- rvar(ref_array, nchains = 2)
  expect_equal({x2 <- x; x2[rvar(c(TRUE,FALSE,TRUE,FALSE))] <- rvar(c(99, 100)); x2}, ref)
  ref_array[1,,] <- 99
  ref_array[3,,] <- 99
  ref <- rvar(ref_array, nchains = 2)
  expect_equal({x2 <- x; x2[rvar(c(TRUE,FALSE,TRUE,FALSE))] <- 99; x2}, ref)
  expect_equal(
    {x2 <- x; x2[rvar(c(TRUE,FALSE,TRUE,FALSE))] <- x2[rvar(c(TRUE,FALSE,TRUE,FALSE))]; x2},
    x
  )
  ref_array <- x_array
  ref_array[2,,] <- 1
  ref_array[4,,] <- 1
  ref <- rvar(ref_array, nchains = 2)
  expect_equal({x2 <- x; x2[rvar(c(NA,TRUE,FALSE,TRUE))] <- 1; x2}, ref)

  expect_error({x2 <- x; x2[as_rvar(c(TRUE,FALSE))] <- c(99,100)}, "scalar logical")
  expect_error({x2 <- x; x2[rvar(c(TRUE,FALSE,TRUE,FALSE))] <- c(99,100)}, "Cannot broadcast")
})
