test_that("for_each_draw works with unnamed variables", {
  x <- draws_rvars(
    x0 = rvar(),
    x1 = rvar(1:2),
    x2 = rvar(array(1:4, dim = c(2,2))),
    x3 = rvar(array(1:8, dim = c(2,2,2))),
    x4 = rvar(array(1:16, dim = c(2,2,2,2)))
  )

  draw_list <- numeric()
  x0_list <- list()
  x1_list <- list()
  x2_list <- list()
  x3_list <- list()
  x4_list <- list()
  for_each_draw(x, {
    # have to use <<- due to data masking
    draw_list <<- c(draw_list, .draw)
    x0_list[[.draw]] <<- x0
    x1_list[[.draw]] <<- x1
    x2_list[[.draw]] <<- x2
    x3_list[[.draw]] <<- x3
    x4_list[[.draw]] <<- x4
  })

  expect_equal(draw_list, 1:2)
  expect_equal(x0_list, list(numeric(), numeric()))
  expect_equal(x1_list, list(1, 2))
  expect_equal(x2_list, list(draws_of(x$x2)[1,], draws_of(x$x2)[2,]))
  expect_equal(x3_list, list(
    array(draws_of(x$x3)[1,,], dim = c(2,2), dimnames = list(NULL, NULL)),
    array(draws_of(x$x3)[2,,], dim = c(2,2), dimnames = list(NULL, NULL))
  ))
  expect_equal(x4_list, list(
    array(draws_of(x$x4)[1,,,], dim = c(2,2,2), dimnames = list(NULL, NULL, NULL)),
    array(draws_of(x$x4)[2,,,], dim = c(2,2,2), dimnames = list(NULL, NULL, NULL))
  ))
})

test_that("for_each_draw works with named variables", {
  x <- draws_rvars(
    x1 = rvar(array(1:2, dim = c(2,1), dimnames = list(NULL, "a"))),
    x2 = rvar(array(1:4, dim = c(2,2), dimnames = list(NULL, letters[1:2]))),
    x3 = rvar(array(1:8, dim = c(2,2,2), dimnames = list(NULL, letters[1:2], letters[3:4]))),
    x4 = rvar(array(1:16, dim = c(2,2,2,2), dimnames = list(NULL, letters[1:2], letters[3:4], letters[5:6])))
  )

  x1_list <- list()
  x2_list <- list()
  x3_list <- list()
  x4_list <- list()
  for_each_draw(x, {
    # have to use <<- due to data masking
    x1_list[[.draw]] <<- x1
    x2_list[[.draw]] <<- x2
    x3_list[[.draw]] <<- x3
    x4_list[[.draw]] <<- x4
  })

  expect_equal(x1_list, list(c(a = 1), c(a = 2)))
  expect_equal(x2_list, list(draws_of(x$x2)[1,], draws_of(x$x2)[2,]))
  expect_equal(x3_list, list(draws_of(x$x3)[1,,], draws_of(x$x3)[2,,]))
  expect_equal(x4_list, list(draws_of(x$x4)[1,,,], draws_of(x$x4)[2,,,]))
})
