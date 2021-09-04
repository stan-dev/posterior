# numeric summaries -------------------------------------------------------

test_that("numeric summaries work", {
  x_array <- array(1:24, dim = c(4,2,3), dimnames = list(NULL, a = c("a1", "a2"), b = c("b1", "b2", "b3")))
  x <- new_rvar(x_array)

  expect_equal(median(x), apply(x_array, c(2,3), median))
  expect_equal(sum(x), apply(x_array, c(2,3), sum))
  expect_equal(prod(x), apply(x_array, c(2,3), prod))
  expect_equal(min(x), apply(x_array, c(2,3), min))
  expect_equal(max(x), apply(x_array, c(2,3), max))
})


# mean --------------------------------------------------------------------

test_that("means work", {
  x_array <- array(1:24, dim = c(4,2,3), dimnames = list(NULL, a = c("a1", "a2"), b = c("b1", "b2", "b3")))
  x <- new_rvar(x_array)

  expect_equal(Pr(x < 2), apply(x_array < 2, c(2,3), mean))
  expect_error(Pr(x))
  expect_equal(E(x), apply(x_array, c(2,3), mean))
  expect_equal(mean(x), apply(x_array, c(2,3), mean))
  # E() and Pr() should also work on base arrays
  expect_equal(Pr(x_array < 2), mean(x_array < 2))
  expect_error(Pr(x_array))
  expect_equal(E(x_array), mean(x_array))

  # test vector rvars as well since these should be summarized down to vectors
  # (not one-dimensional arrays)
  y_array <- array(1:24, dim = c(4,6), dimnames = list(NULL, paste0("a", 1:6)))
  y <- new_rvar(y_array)
  expect_equal(mean(y), apply(y_array, 2, mean))
})


# spread ------------------------------------------------------------------

test_that("spread functions work", {
  x_array <- array(1:24, dim = c(4,2,3), dimnames = list(NULL, a = c("a1", "a2"), b = c("b1", "b2", "b3")))
  x <- new_rvar(x_array)

  expect_equal(sd(x), apply(x_array, c(2,3), sd))
  expect_equal(variance(x), apply(x_array, c(2,3), var))
  expect_equal(var(x), apply(x_array, c(2,3), var))
  expect_equal(mad(x), apply(x_array, c(2,3), mad))

  y_array <- array(1:24, dim = c(4,6), dimnames = list(NULL, paste0("a", 1:6)))
  y <- new_rvar(y_array)
  expect_equal(sd(y), apply(y_array, 2, sd))
  expect_equal(variance(y), apply(y_array, 2, var))
  expect_equal(var(y), apply(y_array, 2, var))
  expect_equal(mad(y), apply(y_array, 2, mad))
})


# range -------------------------------------------------------------------

test_that("range works", {
  x_array <- array(1:24, dim = c(4,2,3), dimnames = list(NULL, a = c("a1", "a2"), b = c("b1", "b2", "b3")))
  x <- new_rvar(x_array)

  expect_equal(range(x), apply(x_array, c(2,3), range))

  y_array <- array(1:24, dim = c(4,6), dimnames = list(NULL, paste0("a", 1:6)))
  y <- new_rvar(y_array)
  expect_equal(range(y), apply(y_array, 2, range))

  # range over a scalar should return a vector
  z_array <- array(1:6, dim = c(6,1), dimnames = list(NULL, "a"))
  z <- new_rvar(z_array)
  expect_equal(range(z), range(z_array))
})


# logical summaries -------------------------------------------------------

test_that("logical summaries work", {
  x_array <- array(1:24, dim = c(4,2,3), dimnames = list(NULL, a = c("a1", "a2"), b = c("b1", "b2", "b3")))
  x <- new_rvar(x_array)

  expect_equal(all(x > 10), apply(x_array > 10, c(2,3), all))
  expect_equal(any(x > 10), apply(x_array > 10, c(2,3), any))

  y_array <- array(1:24, dim = c(4,6), dimnames = list(NULL, paste0("a", 1:6)))
  y <- new_rvar(y_array)
  expect_equal(all(y > 10), apply(y_array > 10, 2, all))
  expect_equal(any(y > 10), apply(y_array > 10, 2, any))
})


# special value predicates ------------------------------------------------

test_that("special value predicates work", {
  x_array <- array(c(1,NA,3:4, 5:6,Inf,8, 9,-Inf,11:12, NaN,14:24),
    dim = c(4,2,3), dimnames = list(NULL, a = c("a1", "a2"), b = c("b1", "b2", "b3"))
  )
  x <- new_rvar(x_array)

  .dimnames = list(a = c("a1", "a2"), b = c("b1", "b2", "b3"))
  expect_equal(is.finite(x), array(c(rep(FALSE, 4), rep(TRUE, 2)), dim = c(2,3), dimnames = .dimnames))
  expect_equal(is.infinite(x), array(c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE), dim = c(2,3), dimnames = .dimnames))
  expect_equal(is.nan(x), array(c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE), dim = c(2,3), dimnames = .dimnames))
  expect_equal(is.na(x), array(c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE), dim = c(2,3), dimnames = .dimnames))

  y_array <- array(x_array, dim = c(4,6), dimnames = list(NULL, paste0("a", 1:6)))
  y <- new_rvar(y_array)
  expect_equal(is.finite(y), matrixStats::colAlls(apply(y_array, 2, is.finite), useNames = TRUE))
  expect_equal(is.infinite(y), matrixStats::colAnys(apply(y_array, 2, is.infinite), useNames = TRUE))
  expect_equal(is.nan(y), matrixStats::colAnys(apply(y_array, 2, is.nan), useNames = TRUE))
  expect_equal(is.na(y), matrixStats::colAnys(apply(y_array, 2, is.na), useNames = TRUE))
})


# anyNA -------------------------------------------------------------------

test_that("anyNA works", {
  x_array <- array(1:24, dim = c(4,2,3), dimnames = list(NULL, a = c("a1", "a2"), b = c("b1", "b2", "b3")))
  x <- new_rvar(x_array)

  expect_equal(anyNA(x), FALSE)
  x_with_na <- x
  x_with_na[2,1] <- NA
  expect_equal(anyNA(x_with_na), TRUE)
})
