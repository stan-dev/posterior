test_that("assigning NULL dim to rvar works", {
  x <- rvar(array(1:20, dim = c(2,2,5)))
  dim(x) <- NULL
  expect_equal(x, rvar(array(1:20, dim = c(2,10))))
})
