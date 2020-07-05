test_that("rdo works", {
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
  expect_identical(rdo(x %*% y), xy_ref)
})

test_that("rfun works", {
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
  expect_identical(rfun(function(a,b) a %*% b)(x, y), xy_ref)
})
