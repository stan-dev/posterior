normalize = function(x) {
  # dimnames may be a list of NULLs after some ops, we don't care about this difference
  # for comparison purposes in tests, so fix it
  if (all(sapply(dimnames(x), is.null))) {
    dimnames(x) <- NULL
  }
  x
}

test_that("arithmetic works", {
  x_array = array(1:24, dim = c(2,3,4))
  x = new_rvar(x_array)
  y_array = array(c(2:13,12:1), dim = c(2,3,4))
  y = new_rvar(y_array)

  expect_identical(normalize(x + 2), new_rvar(x_array + 2))
  expect_identical(normalize(2 + x), new_rvar(x_array + 2))
  expect_identical(normalize(x + y), new_rvar(x_array + y_array))

  expect_identical(normalize(x - 2), new_rvar(x_array - 2))
  expect_identical(normalize(2 - x), new_rvar(2 - x_array))
  expect_identical(normalize(x - y), new_rvar(x_array - y_array))

  expect_identical(normalize(x * 2), new_rvar(x_array * 2))
  expect_identical(normalize(2 * x), new_rvar(x_array * 2))
  expect_identical(normalize(x * y), new_rvar(x_array * y_array))

  expect_identical(normalize(x / 2), new_rvar(x_array / 2))
  expect_identical(normalize(2 / x), new_rvar(2 / x_array))
  expect_identical(normalize(x / y), new_rvar(x_array / y_array))

  expect_identical(normalize(x ^ 2), new_rvar((x_array) ^ 2))
  expect_identical(normalize(2 ^ x), new_rvar(2 ^ (x_array)))
  expect_identical(normalize(x ^ y), new_rvar(x_array ^ y_array))
})

test_that("logical operators work", {
  x_array = c(TRUE,TRUE,FALSE,FALSE)
  y_array = c(TRUE,FALSE,TRUE,FALSE)
  x = as_rvar(x_array)
  y = as_rvar(y_array)

  expect_identical(normalize(x | y_array), normalize(as_rvar(x_array | y_array)))
  expect_identical(normalize(y_array | x), normalize(as_rvar(x_array | y_array)))
  expect_identical(normalize(x | y), normalize(as_rvar(x_array | y_array)))

  expect_identical(normalize(x & y_array), normalize(as_rvar(x_array & y_array)))
  expect_identical(normalize(y_array & x), normalize(as_rvar(x_array & y_array)))
  expect_identical(normalize(x & y), normalize(as_rvar(x_array & y_array)))
})

test_that("comparison operators work", {
  x_array = array(1:24, dim = c(2,3,4))
  x = new_rvar(x_array)
  y_array = array(c(2:13,12:1), dim = c(2,3,4))
  y = new_rvar(y_array)

  expect_identical(normalize(x < 5), new_rvar(x_array < 5))
  expect_identical(normalize(5 < x), new_rvar(5 < x_array))
  expect_identical(normalize(x < y), new_rvar(x_array < y_array))

  expect_identical(normalize(x <= 5), new_rvar(x_array <= 5))
  expect_identical(normalize(5 <= x), new_rvar(5 <= x_array))
  expect_identical(normalize(x <= y), new_rvar(x_array <= y_array))

  expect_identical(normalize(x > 5), new_rvar(x_array > 5))
  expect_identical(normalize(5 > x), new_rvar(5 > x_array))
  expect_identical(normalize(x > y), new_rvar(x_array > y_array))

  expect_identical(normalize(x >= 5), new_rvar(x_array >= 5))
  expect_identical(normalize(5 >= x), new_rvar(5 >= x_array))
  expect_identical(normalize(x >= y), new_rvar(x_array >= y_array))

  expect_identical(normalize(x == 5), new_rvar(x_array == 5))
  expect_identical(normalize(5 == x), new_rvar(5 == x_array))
  expect_identical(normalize(x == y), new_rvar(x_array == y_array))

  expect_identical(normalize(x != 5), new_rvar(x_array != 5))
  expect_identical(normalize(5 != x), new_rvar(5 != x_array))
  expect_identical(normalize(x != y), new_rvar(x_array != y_array))
})

test_that("matrix multiplication works", {
  x_array = array(1:24, dim = c(2,3,4))
  x = new_rvar(x_array)
  y_array = array(c(2:13,12:1), dim = c(3,2,4))
  y = new_rvar(y_array)

  xy_ref = new_rvar(abind::abind(rev.along = 0,
    x_array[,,1] %*% y_array[,,1],
    x_array[,,2] %*% y_array[,,2],
    x_array[,,3] %*% y_array[,,3],
    x_array[,,4] %*% y_array[,,4]
  ))
  expect_identical(x %*% y, xy_ref)

  x_array = array(1:6, dim = c(3,2))
  x = new_rvar(x_array)
  y_array = array(7:12, dim = c(3,2))
  y = new_rvar(y_array)

  xy_ref = new_rvar(abind::abind(rev.along = 0,
    x_array[,1] %*% y_array[,1],
    x_array[,2] %*% y_array[,2]
  ))
  expect_identical(x %*% y, xy_ref)
})
