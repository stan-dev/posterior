test_that("assigning NULL dim to rvar works", {
  x <- rvar(array(1:20, dim = c(2,2,5)))
  dim(x) <- NULL
  expect_equal(x, rvar(array(1:20, dim = c(2,10))))
})


# names -------------------------------------------------------------------

test_that("unname() works", {
  x_array = array(1:24, dim = c(2,3,4), dimnames = list(NULL, A = paste0("a", 1:3), B = paste0("b", 1:4)))
  x = rvar(x_array)

  expect_equal(unname(x), rvar(unname(x_array)))
})
