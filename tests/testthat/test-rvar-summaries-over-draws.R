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
})


# spread ------------------------------------------------------------------

test_that("spread functions work", {
  x_array <- array(1:24, dim = c(4,2,3), dimnames = list(NULL, a = c("a1", "a2"), b = c("b1", "b2", "b3")))
  x <- new_rvar(x_array)

  expect_equal(sd(x), apply(x_array, c(2,3), sd))
  expect_equal(variance(x), apply(x_array, c(2,3), var))
  expect_equal(var(x), apply(x_array, c(2,3), var))
  expect_equal(mad(x), apply(x_array, c(2,3), mad))
})


# range -------------------------------------------------------------------

test_that("range works", {
  x_array <- array(1:24, dim = c(4,2,3))
  x <- new_rvar(x_array)

  expect_equal(range(x), apply(x_array, c(2,3), range), check.attributes = FALSE)
})


# logical summaries -------------------------------------------------------

test_that("logical summaries work", {
  x_array <- array(1:24, dim = c(4,2,3), dimnames = list(NULL, a = c("a1", "a2"), b = c("b1", "b2", "b3")))
  x <- new_rvar(x_array)

  expect_equal(all(x > 10), apply(x_array > 10, c(2,3), all))
  expect_equal(any(x > 10), apply(x_array > 10, c(2,3), any))
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
