# numeric summaries -------------------------------------------------------

test_that("numeric summaries work", {
  x_array <- array(1:24, dim = c(4,2,3), dimnames = list(NULL, a = c("a1", "a2"), b = c("b1", "b2", "b3")))
  x <- new_rvar(x_array)
  x_letters <- array(letters[1:24], dim = c(4,2,3), dimnames = list(NULL, a = c("a1", "a2"), b = c("b1", "b2", "b3")))
  x_ord <- rvar_ordered(x_letters, levels = letters)
  x_fct <- rvar_factor(x_letters, levels = letters)

  expect_equal(median(x), apply(x_array, c(2,3), median))
  expect_equal(sum(x), apply(x_array, c(2,3), sum))
  expect_equal(prod(x), apply(x_array, c(2,3), prod))
  expect_equal(min(x), apply(x_array, c(2,3), min))
  expect_equal(max(x), apply(x_array, c(2,3), max))

  ordered_out <- function(x) structure(
    x, dim = c(2,3), dimnames = list(a = c("a1", "a2"), b = c("b1", "b2", "b3")),
    levels = letters, class = c("ordered", "factor")
  )
  expect_equal(median(x_ord), ordered_out(c(2, 6, 10, 14, 18, 22)))
  expect_equal(min(x_ord), ordered_out(c(1, 5, 9, 13, 17, 21)))
  expect_equal(max(x_ord), ordered_out(c(4, 8, 12, 16, 20, 24)))
  expect_error(sum(x_ord))
  expect_error(prod(x_ord))

  expect_error(median(x_fct))
  expect_error(min(x_fct))
  expect_error(max(x_fct))
  expect_error(sum(x_ord))
  expect_error(prod(x_ord))
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

  expect_error(mean(rvar_factor("a")))
  expect_error(mean(rvar_ordered("a")))
})


# spread ------------------------------------------------------------------

test_that("spread functions work", {
  x_array <- array(1:24, dim = c(4,2,3), dimnames = list(NULL, a = c("a1", "a2"), b = c("b1", "b2", "b3")))
  x <- new_rvar(x_array)
  x_letters <- array(letters[1:24], dim = c(4,2,3), dimnames = list(NULL, a = c("a1", "a2"), b = c("b1", "b2", "b3")))
  x_ord <- rvar_ordered(x_letters, levels = letters)
  x_fct <- rvar_factor(x_letters, levels = letters)

  expect_equal(sd(x), apply(x_array, c(2,3), sd))
  expect_equal(variance(x), apply(x_array, c(2,3), var))
  expect_equal(var(x), apply(x_array, c(2,3), var))
  expect_equal(mad(x), apply(x_array, c(2,3), mad))

  expect_error(sd(x_ord))
  expect_error(variance(x_ord))
  expect_error(var(x_ord))
  expect_equal(mad(x_ord), apply(x_array, c(2,3), mad))

  expect_error(sd(x_fct))
  expect_error(variance(x_fct))
  expect_error(var(x_fct))
  expect_error(mad(x_fct))

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
  x_letters <- array(letters[1:24], dim = c(4,2,3), dimnames = list(NULL, a = c("a1", "a2"), b = c("b1", "b2", "b3")))
  x_ord <- rvar_ordered(x_letters, levels = letters)
  x_fct <- rvar_factor(x_letters, levels = letters)

  expect_equal(range(x), apply(x_array, c(2,3), range))
  expect_equal(range(x_ord),
    structure(c(1, 4, 5, 8, 9, 12, 13, 16, 17, 20, 21, 24), levels = letters,
      class = c("ordered", "factor"), dim = c(2, 2, 3),
      dimnames = list(NULL, a = c("a1", "a2"), b = c("b1", "b2", "b3"))
    )
  )
  expect_error(range(x_fct))

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

  expect_error(all(rvar("a")))
  expect_error(any(rvar("a")))
})


# special value predicates ------------------------------------------------

test_that("special value predicates work", {
  x_array <- array(c(1,NA,3:4, 5:6,Inf,8, 9,-Inf,11:12, NaN,14:24),
    dim = c(4,2,3), dimnames = list(NULL, a = c("a1", "a2"), b = c("b1", "b2", "b3"))
  )
  x <- new_rvar(x_array)
  x_letters <- array(c("a",NA,letters[3:12], NaN, letters[14:24]), dim = c(4,2,3), dimnames = list(NULL, a = c("a1", "a2"), b = c("b1", "b2", "b3")))
  x_ord <- rvar_ordered(x_letters, levels = letters)
  x_fct <- rvar_factor(x_letters, levels = letters)

  .dimnames = list(a = c("a1", "a2"), b = c("b1", "b2", "b3"))
  expect_equal(is.finite(x), array(c(rep(FALSE, 4), rep(TRUE, 2)), dim = c(2,3), dimnames = .dimnames))
  expect_equal(is.infinite(x), array(c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE), dim = c(2,3), dimnames = .dimnames))
  expect_equal(is.nan(x), array(c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE), dim = c(2,3), dimnames = .dimnames))
  expect_equal(is.na(x), array(c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE), dim = c(2,3), dimnames = .dimnames))

  .dimnames = list(a = c("a1", "a2"), b = c("b1", "b2", "b3"))
  expect_equal(is.finite(x_ord), array(c(FALSE, TRUE, TRUE, FALSE, rep(TRUE, 2)), dim = c(2,3), dimnames = .dimnames))
  expect_equal(is.finite(x_fct), array(c(FALSE, TRUE, TRUE, FALSE, rep(TRUE, 2)), dim = c(2,3), dimnames = .dimnames))
  expect_equal(is.infinite(x_ord), array(rep(FALSE, 6), dim = c(2,3), dimnames = .dimnames))
  expect_equal(is.infinite(x_fct), array(rep(FALSE, 6), dim = c(2,3), dimnames = .dimnames))
  expect_equal(is.nan(x_ord), array(c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), dim = c(2,3), dimnames = .dimnames))
  expect_equal(is.nan(x_fct), array(c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), dim = c(2,3), dimnames = .dimnames))
  expect_equal(is.na(x_ord), array(c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE), dim = c(2,3), dimnames = .dimnames))
  expect_equal(is.na(x_fct), array(c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE), dim = c(2,3), dimnames = .dimnames))

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
  x_letters <- array(letters[1:24], dim = c(4,2,3), dimnames = list(NULL, a = c("a1", "a2"), b = c("b1", "b2", "b3")))
  x_ord <- rvar_ordered(x_letters, levels = letters)
  x_fct <- rvar_factor(x_letters, levels = letters)

  expect_equal(anyNA(x), FALSE)
  expect_equal(anyNA(x_fct), FALSE)
  expect_equal(anyNA(x_ord), FALSE)
  x[2,1] <- NA
  expect_equal(anyNA(x), TRUE)
  x_fct[2,1] <- NA
  expect_equal(anyNA(x_fct), TRUE)
  x_ord[2,1] <- NA
  expect_equal(anyNA(x_ord), TRUE)
})
