# summary functions -------------------------------------------------------

test_that("summary functions work", {
  x_array = array(1:24, dim = c(4,2,3))
  x = new_rvar(x_array)

  expect_equal(Pr(x < 2), apply(x_array < 2, c(2,3), mean), check.attributes = FALSE)
  expect_error(Pr(x))
  expect_equal(E(x), apply(x_array, c(2,3), mean), check.attributes = FALSE)
  # E() and Pr() should also work on base arrays
  expect_equal(Pr(x_array < 2), mean(x_array < 2))
  expect_error(Pr(x_array))
  expect_equal(E(x_array), mean(x_array))

  expect_equal(mean(x), apply(x_array, c(2,3), mean), check.attributes = FALSE)
  expect_equal(median(x), apply(x_array, c(2,3), median), check.attributes = FALSE)
  expect_equal(variance(x), apply(x_array, c(2,3), var), check.attributes = FALSE)

  expect_equal(draws_of(rvar_mean(x)), apply(x_array, 1, mean), check.attributes = FALSE)
  expect_equal(draws_of(rvar_median(x)), apply(x_array, 1, median), check.attributes = FALSE)
  expect_equal(draws_of(min(x)), apply(x_array, 1, min), check.attributes = FALSE)
  expect_equal(draws_of(max(x)), apply(x_array, 1, max), check.attributes = FALSE)
  expect_equal(draws_of(range(x)), t(apply(x_array, 1, range)), check.attributes = FALSE)

  expect_equal(anyNA(x), FALSE)
  x_with_na <- x
  x_with_na[2,1] <- NA
  expect_equal(anyNA(x_with_na), TRUE)
  expect_equal(is.na(x_with_na), array(c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE), dim = c(2,3)), check.attributes = FALSE)

  expect_equal(draws_of(is.finite(x)), is.finite(x_array), check.attributes = FALSE)
  expect_equal(draws_of(is.infinite(x)), is.infinite(x_array), check.attributes = FALSE)
  expect_equal(draws_of(is.nan(x)), is.nan(x_array), check.attributes = FALSE)
})
