# basic operators ---------------------------------------------------------

test_that("math operators works", {
  x_array = array(1:24, dim = c(4,2,3), dimnames = list(NULL,letters[1:2],letters[3:5]))
  x = new_rvar(x_array)
  y_array = array(c(2:13,12:1), dim = c(4,2,3))
  y = new_rvar(y_array)

  expect_identical(log(x), new_rvar(log(x_array)))

  expect_identical(-x, new_rvar(-x_array))

  expect_identical(x + 2, new_rvar(x_array + 2))
  expect_identical(2 + x, new_rvar(x_array + 2))
  expect_identical(x + y, new_rvar(x_array + y_array))

  expect_identical(x - 2, new_rvar(x_array - 2))
  expect_identical(2 - x, new_rvar(2 - x_array))
  expect_identical(x - y, new_rvar(x_array - y_array))

  expect_identical(x * 2, new_rvar(x_array * 2))
  expect_identical(2 * x, new_rvar(x_array * 2))
  expect_identical(x * y, new_rvar(x_array * y_array))

  expect_identical(x / 2, new_rvar(x_array / 2))
  expect_identical(2 / x, new_rvar(2 / x_array))
  expect_identical(x / y, new_rvar(x_array / y_array))

  expect_identical(x ^ 2, new_rvar((x_array) ^ 2))
  expect_identical(2 ^ x, new_rvar(2 ^ (x_array)))
  expect_identical(x ^ y, new_rvar(x_array ^ y_array))

  # ensure broadcasting of constants retains shape
  z2 <- new_rvar(array(1, dim = c(1,1)))
  z4 <- new_rvar(array(2, dim = c(1,1,1,1)))
  expect_equal(z2 + z4, new_rvar(array(3, dim = c(1,1,1,1))))
})

test_that("logical operators work", {
  x_array = c(TRUE,TRUE,FALSE,FALSE)
  y_array = c(TRUE,FALSE,TRUE,FALSE)
  x = as_rvar(x_array)
  y = as_rvar(y_array)

  expect_identical(x | y_array, as_rvar(x_array | y_array))
  expect_identical(y_array | x, as_rvar(x_array | y_array))
  expect_identical(x | y, as_rvar(x_array | y_array))

  expect_identical(x & y_array, as_rvar(x_array & y_array))
  expect_identical(y_array & x, as_rvar(x_array & y_array))
  expect_identical(x & y, as_rvar(x_array & y_array))
})

test_that("comparison operators work", {
  x_array = array(1:24, dim = c(4,2,3))
  x = new_rvar(x_array)
  y_array = array(c(2:13,12:1), dim = c(4,2,3))
  y = new_rvar(y_array)

  expect_identical(x < 5, new_rvar(x_array < 5))
  expect_identical(5 < x, new_rvar(5 < x_array))
  expect_identical(x < y, new_rvar(x_array < y_array))

  expect_identical(x <= 5, new_rvar(x_array <= 5))
  expect_identical(5 <= x, new_rvar(5 <= x_array))
  expect_identical(x <= y, new_rvar(x_array <= y_array))

  expect_identical(x > 5, new_rvar(x_array > 5))
  expect_identical(5 > x, new_rvar(5 > x_array))
  expect_identical(x > y, new_rvar(x_array > y_array))

  expect_identical(x >= 5, new_rvar(x_array >= 5))
  expect_identical(5 >= x, new_rvar(5 >= x_array))
  expect_identical(x >= y, new_rvar(x_array >= y_array))

  expect_identical(x == 5, new_rvar(x_array == 5))
  expect_identical(5 == x, new_rvar(5 == x_array))
  expect_identical(x == y, new_rvar(x_array == y_array))

  expect_identical(x != 5, new_rvar(x_array != 5))
  expect_identical(5 != x, new_rvar(5 != x_array))
  expect_identical(x != y, new_rvar(x_array != y_array))
})

test_that("functions in the Math generic with extra arguments work", {
  expect_equal(round(rvar(11), -1), rvar(10))
  expect_equal(signif(rvar(11), 1), rvar(10))
  expect_equal(log(rvar(c(2,4,8)), base = 2), rvar(1:3))
})

test_that("cumulative functions work", {
  x_array = array(1:12, dim = c(2,2,3))
  x = new_rvar(x_array)

  cumsum_ref = new_rvar(rbind(
    cumsum(draws_of(x)[1,,]),
    cumsum(draws_of(x)[2,,])
  ))
  expect_equal(cumsum(x), cumsum_ref)

  cumprod_ref = new_rvar(rbind(
    cumprod(draws_of(x)[1,,]),
    cumprod(draws_of(x)[2,,])
  ))
  expect_equal(cumprod(x), cumprod_ref)

  cummax_ref = new_rvar(rbind(
    cummax(draws_of(x)[1,,]),
    cummax(draws_of(x)[2,,])
  ))
  expect_equal(cummax(x), cummax_ref)

  cummin_ref = new_rvar(rbind(
    cummin(draws_of(x)[1,,]),
    cummin(draws_of(x)[2,,])
  ))
  expect_equal(cummin(x), cummin_ref)
})

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

# matrix stuff ------------------------------------------------------------

test_that("matrix multiplication works", {
  x_array = array(1:24, dim = c(4,2,3))
  x = new_rvar(x_array)
  y_array = array(c(2:13,12:1), dim = c(4,3,2))
  y = new_rvar(y_array)

  xy_ref = new_rvar(abind::abind(along = 0,
    x_array[1,,] %*% y_array[1,,],
    x_array[2,,] %*% y_array[2,,],
    x_array[3,,] %*% y_array[3,,],
    x_array[4,,] %*% y_array[4,,]
  ))
  expect_identical(x %**% y, xy_ref)


  x_array = array(1:6, dim = c(2,3))
  x = new_rvar(x_array)
  y_array = array(7:12, dim = c(2,3))
  y = new_rvar(y_array)

  xy_ref = new_rvar(abind::abind(along = 0,
    x_array[1,] %*% y_array[1,],
    x_array[2,] %*% y_array[2,]
  ))
  expect_identical(x %**% y, xy_ref)


  # dimension name preservation
  m1 <- as_rvar(diag(1:3))
  dimnames(m1) <- list(a = paste0("a", 1:3), b = paste0("b", 1:3))
  m2 <- as_rvar(diag(1:3)[,1:2])
  dimnames(m2) <- list(c = paste0("c", 1:3), d = paste0("d", 1:2))
  expect_equal(dimnames(m1 %**% m2), list(a = paste0("a", 1:3), d = paste0("d", 1:2)))


  # errors
  x_array = array(1:24, dim = c(4,1,2,3))
  x = new_rvar(x_array)

  expect_error(x %**% 1, "not a vector or matrix")
  expect_error(1 %**% x, "not a vector or matrix")

})

test_that("diag works", {
  Sigma <- as_draws_rvar(example_draws("multi_normal"))$Sigma

  expect_equal(diag(Sigma), c(Sigma[1,1], Sigma[2,2], Sigma[3,3]))

  Sigma_ref <- Sigma
  Sigma_ref[1,1] <- 2
  Sigma_ref[2,2] <- 3
  Sigma_ref[3,3] <- 4

  Sigma_test <- Sigma
  diag(Sigma_test) <- 2:4
  expect_equal(Sigma_test, Sigma_ref)
})

test_that("Cholesky decomposition works", {
  Sigma <- as_draws_rvar(example_draws("multi_normal"))$Sigma

  # adding dimensions because we should expect these to be dropped
  dimnames(Sigma) <- list(
    a = paste0("a", 1:3),
    b = paste0("b", 1:3)
  )

  expect_equal(chol(Sigma), rdo(chol(Sigma)))
})

# array transpose and permutation -----------------------------------------

test_that("vector transpose works", {
  x_array = array(1:6, dim = c(2,3), dimnames = list(NULL, c("a","b","c")))
  x = new_rvar(x_array)
  x_array_t = array(1:6, dim = c(2,1,3), dimnames = list(NULL, NULL, c("a","b","c")))
  x_t = new_rvar(x_array_t)
  x_array_t_t = array(1:6, dim = c(2,3,1), dimnames = list(NULL, c("a","b","c"), NULL))
  x_t_t = new_rvar(x_array_t_t)

  # ensure it works with dimnames...
  expect_equal(t(x), x_t)
  expect_equal(t(t(x)), x_t_t)

  # ... and without dimnames
  dimnames(x) = NULL
  dimnames(x_t) = NULL
  dimnames(x_t_t) = NULL
  expect_equal(t(x), x_t)
  expect_equal(t(t(x)), x_t_t)
})

test_that("matrix transpose works", {
  x_array = array(1:24, dim = c(4,3,2))
  x = new_rvar(x_array)
  x_t = new_rvar(aperm(x_array, c(1,3,2)))

  expect_error(t(rvar()))
  expect_equal(t(x), x_t)
  expect_equal(t(new_rvar(array(1:10, c(1,10)))), new_rvar(array(1:10, c(1,1,10))))
  expect_equal(t(new_rvar(array(1:10, c(2,1,5)))), new_rvar(array(1:10, c(2,5,1))))
  expect_equal(t(new_rvar(array(1:10, c(2,1,5)))), new_rvar(array(1:10, c(2,5,1))))
  expect_equal(t(new_rvar(array(1:10, c(2,5)))), new_rvar(array(1:10, c(2,1,5))))
})

test_that("array permutation works", {
  x_array = array(
    1:24, dim = c(2,2,3,2),
    dimnames = list(NULL, A = paste0("a", 1:2), B = paste0("b", 1:3), C = paste0("c", 1:2))
  )
  x = new_rvar(x_array)
  x_perm = new_rvar(aperm(x_array, c(1,2,4,3)))

  expect_equal(aperm(x, c(1,3,2)), x_perm)
})
