# function for making rvars from arrays that expects last index to be
# draws (for testing so that when array structure changes tests don't have to)
rvar_from_array = function(x) {
  .dim = dim(x)
  last_dim = length(.dim)
  new_rvar(aperm(x, c(last_dim, seq_len(last_dim - 1))))
}

test_that("arithmetic works", {
  x_array = array(1:24, dim = c(2,3,4))
  x = rvar_from_array(x_array)
  y_array = array(c(2:13,12:1), dim = c(2,3,4))
  y = rvar_from_array(y_array)

  expect_identical(x + 2, rvar_from_array(x_array + 2))
  expect_identical(2 + x, rvar_from_array(x_array + 2))
  expect_identical(x + y, rvar_from_array(x_array + y_array))

  expect_identical(x - 2, rvar_from_array(x_array - 2))
  expect_identical(2 - x, rvar_from_array(2 - x_array))
  expect_identical(x - y, rvar_from_array(x_array - y_array))

  expect_identical(x * 2, rvar_from_array(x_array * 2))
  expect_identical(2 * x, rvar_from_array(x_array * 2))
  expect_identical(x * y, rvar_from_array(x_array * y_array))

  expect_identical(x / 2, rvar_from_array(x_array / 2))
  expect_identical(2 / x, rvar_from_array(2 / x_array))
  expect_identical(x / y, rvar_from_array(x_array / y_array))

  expect_identical(x ^ 2, rvar_from_array((x_array) ^ 2))
  expect_identical(2 ^ x, rvar_from_array(2 ^ (x_array)))
  expect_identical(x ^ y, rvar_from_array(x_array ^ y_array))
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
  x_array = array(1:24, dim = c(2,3,4))
  x = rvar_from_array(x_array)
  y_array = array(c(2:13,12:1), dim = c(2,3,4))
  y = rvar_from_array(y_array)

  expect_identical(x < 5, rvar_from_array(x_array < 5))
  expect_identical(5 < x, rvar_from_array(5 < x_array))
  expect_identical(x < y, rvar_from_array(x_array < y_array))

  expect_identical(x <= 5, rvar_from_array(x_array <= 5))
  expect_identical(5 <= x, rvar_from_array(5 <= x_array))
  expect_identical(x <= y, rvar_from_array(x_array <= y_array))

  expect_identical(x > 5, rvar_from_array(x_array > 5))
  expect_identical(5 > x, rvar_from_array(5 > x_array))
  expect_identical(x > y, rvar_from_array(x_array > y_array))

  expect_identical(x >= 5, rvar_from_array(x_array >= 5))
  expect_identical(5 >= x, rvar_from_array(5 >= x_array))
  expect_identical(x >= y, rvar_from_array(x_array >= y_array))

  expect_identical(x == 5, rvar_from_array(x_array == 5))
  expect_identical(5 == x, rvar_from_array(5 == x_array))
  expect_identical(x == y, rvar_from_array(x_array == y_array))

  expect_identical(x != 5, rvar_from_array(x_array != 5))
  expect_identical(5 != x, rvar_from_array(5 != x_array))
  expect_identical(x != y, rvar_from_array(x_array != y_array))
})

test_that("matrix multiplication works", {
  x_array = array(1:24, dim = c(2,3,4))
  x = rvar_from_array(x_array)
  y_array = array(c(2:13,12:1), dim = c(3,2,4))
  y = rvar_from_array(y_array)

  xy_ref = rvar_from_array(abind::abind(rev.along = 0,
    x_array[,,1] %*% y_array[,,1],
    x_array[,,2] %*% y_array[,,2],
    x_array[,,3] %*% y_array[,,3],
    x_array[,,4] %*% y_array[,,4]
  ))
  expect_identical(x %*% y, xy_ref)


  x_array = array(1:6, dim = c(3,2))
  x = rvar_from_array(x_array)
  y_array = array(7:12, dim = c(3,2))
  y = rvar_from_array(y_array)

  xy_ref = rvar_from_array(abind::abind(rev.along = 0,
    x_array[,1] %*% y_array[,1],
    x_array[,2] %*% y_array[,2]
  ))
  expect_identical(x %*% y, xy_ref)


  x_array = array(1:24, dim = c(1,2,3,4))
  x = rvar_from_array(x_array)

  expect_error(x %*% 1, "not a vector or matrix")
  expect_error(1 %*% x, "not a vector or matrix")
})


# array transpose and permutation -----------------------------------------

test_that("matrix transpose works", {
  x_array = array(1:24, dim = c(2,3,4))
  x = rvar_from_array(x_array)
  x_t = rvar_from_array(aperm(x_array, c(2,1,3)))

  expect_error(t(rvar()))
  expect_equal(t(x), x_t)
  expect_equal(t(rvar_from_array(array(1:5, c(10,1)))), rvar_from_array(array(1:5, c(1,10,1))))
})
