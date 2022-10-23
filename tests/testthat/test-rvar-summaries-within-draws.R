# numeric summaries -------------------------------------------------------

test_that("numeric summary functions work", {
  x_array <- array(1:24, dim = c(4,2,3))
  x <- new_rvar(x_array)
  x_letters <- array(letters[1:24], dim = c(4,2,3))
  x_ord <- rvar_ordered(x_letters, levels = letters)
  x_fct <- rvar_factor(x_letters, levels = letters)

  expect_equal(draws_of(rvar_mean(x)), apply(x_array, 1, mean), check.attributes = FALSE)
  expect_equal(draws_of(rvar_median(x)), apply(x_array, 1, median), check.attributes = FALSE)
  expect_equal(draws_of(rvar_sum(x)), apply(x_array, 1, sum), check.attributes = FALSE)
  expect_equal(draws_of(rvar_prod(x)), apply(x_array, 1, prod), check.attributes = FALSE)
  expect_equal(draws_of(rvar_min(x)), apply(x_array, 1, min), check.attributes = FALSE)
  expect_equal(draws_of(rvar_max(x)), apply(x_array, 1, max), check.attributes = FALSE)

  expect_error(rvar_mean(x_ord))
  expect_equal(rvar_median(x_ord), rvar_ordered(letters[apply(x_array, 1, median)], levels = letters))
  expect_error(rvar_sum(x_ord))
  expect_error(rvar_prod(x_ord))
  expect_equal(rvar_min(x_ord), rvar_ordered(letters[apply(x_array, 1, min)], levels = letters))
  expect_equal(rvar_max(x_ord), rvar_ordered(letters[apply(x_array, 1, max)], levels = letters))

  expect_error(rvar_mean(x_fct))
  expect_error(rvar_median(x_fct))
  expect_error(rvar_sum(x_fct))
  expect_error(rvar_prod(x_fct))
  expect_error(rvar_min(x_fct))
  expect_error(rvar_max(x_fct))

  # default values on empty input
  expect_equal(rvar_mean(), as_rvar(NA_real_))
  expect_equal(rvar_median(), as_rvar(NA_real_))
  expect_equal(rvar_sum(), as_rvar(0))
  expect_equal(rvar_prod(), as_rvar(1))
  expect_equal(rvar_min(), as_rvar(Inf))
  expect_equal(rvar_max(), as_rvar(-Inf))

  # test argument passing
  x[1,2] <- NA
  expect_equal(
    draws_of(rvar_mean(x, na.rm = TRUE)),
    apply(draws_of(x), 1, function(x) mean(x, na.rm = TRUE)),
    check.attributes = FALSE
  )
})


# spread ------------------------------------------------------------------

test_that("spread summary functions work", {
  x_array <- array(1:24, dim = c(4,2,3))
  x <- new_rvar(x_array)
  x_letters <- array(letters[1:24], dim = c(4,2,3))
  x_ord <- rvar_ordered(x_letters, levels = letters)
  x_fct <- rvar_factor(x_letters, levels = letters)

  expect_equal(draws_of(rvar_sd(x)), apply(x_array, 1, sd), check.attributes = FALSE)
  expect_equal(draws_of(rvar_var(x)), apply(x_array, 1, function(x) var(as.vector(x))), check.attributes = FALSE)
  expect_equal(draws_of(rvar_mad(x)), apply(x_array, 1, mad), check.attributes = FALSE)
  expect_equal(draws_of(rvar_mad(x, constant = 1)), apply(x_array, 1, mad, constant = 1), check.attributes = FALSE)

  expect_error(rvar_sd(x_ord))
  expect_error(rvar_var(x_ord))
  expect_equal(rvar_mad(x_ord, constant = 1), rvar(apply(x_array, 1, mad, constant = 1)))

  expect_error(rvar_sd(x_fct))
  expect_error(rvar_var(x_fct))
  expect_error(rvar_mad(x_fct))

  # default values on empty input
  expect_equal(rvar_sd(), as_rvar(NA_real_))
  expect_equal(rvar_var(), as_rvar(NA_real_))
  expect_equal(rvar_mad(), as_rvar(NA_real_))

  # test argument passing on var since it requires some finagling
  x[1,2] <- NA
  expect_equal(
    draws_of(rvar_var(x, na.rm = TRUE)),
    apply(draws_of(x), 1, function(x) var(as.vector(x), na.rm = TRUE)),
    check.attributes = FALSE
  )
})


# range -------------------------------------------------------------------

test_that("rvar_range works", {
  x_array <- array(1:24, dim = c(4,2,3))
  x <- new_rvar(x_array)
  x_letters <- array(letters[1:24], dim = c(4,2,3))
  x_ord <- rvar_ordered(x_letters, levels = letters)

  expect_equal(draws_of(rvar_range(x)), t(apply(x_array, 1, range)), check.attributes = FALSE)
  expect_equal(rvar_range(x_ord), rvar_ordered(array(letters[t(apply(x_array, 1, range))], dim = c(4, 2)), levels = letters))
  expect_error(rvar_range(rvar_factor("a")))

  # default values on empty input
  expect_equal(rvar_range(), as_rvar(c(Inf, -Inf)))
})


# quantiles ---------------------------------------------------------------

test_that("rvar_quantile works", {
  x_array <- array(1:24, dim = c(4,2,3))
  x <- new_rvar(x_array)
  x_letters <- array(letters[1:24], dim = c(4,2,3))
  x_ord <- rvar_ordered(x_letters, levels = letters)

  p <- c(0.25, 0.5, 0.75)
  quantiles <- t(apply(x_array, 1, quantile, probs = p, names = TRUE))
  dimnames(quantiles)[1] <- list(1:4)
  expect_equal(draws_of(rvar_quantile(x, probs = p, names = TRUE)), quantiles)

  ord_quantiles <- t(apply(x_array, 1, quantile, probs = p, type = 1, names = TRUE))
  dimnames(ord_quantiles)[1] <- list(1:4)
  expect_equal(
    rvar_quantile(x_ord, probs = p, names = TRUE),
    rvar_ordered(structure(ord_quantiles, levels = letters, class = c("ordered", "factor")))
  )

  dimnames(quantiles)[2] <- NULL
  expect_equal(draws_of(rvar_quantile(x, probs = p, names = FALSE)), quantiles)

  q50 <- array(apply(x_array, 1, quantile, probs = 0.5), dim = c(4, 1), dimnames = list(1:4, "50%"))
  expect_equal(draws_of(rvar_quantile(x, probs = 0.5, names = TRUE)), q50)

  # passing NULL should still result in a vector with length = length(probs)
  expect_equal(rvar_quantile(NULL, probs = c(0.25, 0.75)), as_rvar(c(NA_real_, NA_real_)))
})


# logical summaries -------------------------------------------------------

test_that("logical summaries work", {
  x_array <- array(1:24, dim = c(4,2,3))
  x <- new_rvar(x_array)

  expect_equal(draws_of(rvar_all(x > 6)), as.matrix(apply(x_array > 6, 1, all)), check.attributes = FALSE)
  expect_equal(draws_of(rvar_any(x > 6)), as.matrix(apply(x_array > 6, 1, any)), check.attributes = FALSE)

  expect_error(rvar_all(rvar("a")))
  expect_error(rvar_any(rvar("a")))

  # default values on empty input
  expect_equal(rvar_all(), as_rvar(TRUE))
  expect_equal(rvar_any(), as_rvar(FALSE))
})


# special value predicates ------------------------------------------------

test_that("special value predicates work", {
  x_array <- c(1, Inf, -Inf, NaN, NA)
  x <- new_rvar(x_array)
  x_letters <- factor(letters[c(1, 2, 3, NaN, NA)])
  x_ord <- rvar_ordered(x_letters)
  x_fct <- rvar_factor(x_letters)

  expect_equal(draws_of(rvar_is_finite(x)), as.matrix(is.finite(x_array)), check.attributes = FALSE)
  expect_equal(draws_of(rvar_is_infinite(x)), as.matrix(is.infinite(x_array)), check.attributes = FALSE)
  expect_equal(draws_of(rvar_is_nan(x)), as.matrix(is.nan(x_array)), check.attributes = FALSE)
  expect_equal(draws_of(rvar_is_na(x)), as.matrix(is.na(x_array)), check.attributes = FALSE)

  expect_equal(draws_of(rvar_is_finite(x_ord)), as.matrix(is.finite(x_letters)), check.attributes = FALSE)
  expect_equal(draws_of(rvar_is_infinite(x_ord)), as.matrix(is.infinite(x_letters)), check.attributes = FALSE)
  expect_equal(draws_of(rvar_is_nan(x_ord)), as.matrix(is.nan(x_letters)), check.attributes = FALSE)
  expect_equal(draws_of(rvar_is_na(x_ord)), as.matrix(is.na(x_letters)), check.attributes = FALSE)

  expect_equal(draws_of(rvar_is_finite(x_fct)), as.matrix(is.finite(x_letters)), check.attributes = FALSE)
  expect_equal(draws_of(rvar_is_infinite(x_fct)), as.matrix(is.infinite(x_letters)), check.attributes = FALSE)
  expect_equal(draws_of(rvar_is_nan(x_fct)), as.matrix(is.nan(x_letters)), check.attributes = FALSE)
  expect_equal(draws_of(rvar_is_na(x_fct)), as.matrix(is.na(x_letters)), check.attributes = FALSE)
})
