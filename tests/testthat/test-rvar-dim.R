test_that("assigning NULL dim to rvar works", {
  x <- rvar(array(1:20, dim = c(2,2,5)))
  dim(x) <- NULL
  expect_equal(x, rvar(array(1:20, dim = c(2,10))))
})


# names -------------------------------------------------------------------

test_that("unname() works", {
  x_array <- array(1:24, dim = c(2,3,4), dimnames = list(NULL, A = paste0("a", 1:3), B = paste0("b", 1:4)))
  x <- rvar(x_array)

  expect_equal(unname(x), rvar(unname(x_array)))
})


# drop --------------------------------------------------------------------

test_that("drop() works", {
  .dim <- c(2,3,4)
  .dimnames <- list(NULL, A = paste0("a", 1:3), B = paste0("b", 1:4))
  x_array <- array(1:24, dim = .dim, dimnames = .dimnames)
  x <- rvar(x_array)

  x_1 <- rvar(array(x_array, dim = c(.dim, 1), dimnames = c(.dimnames, list(NULL))))
  expect_equal(drop(x_1), x)

  x_vector <- rvar(array(x_array, dim = c(2,12)))
  names(x_vector) <- paste0("a", 1:12)
  expect_equal(drop(x_vector), x_vector)
})

test_that("adrop() works", {
  .dim <- c(2,3,4)
  .dimnames <- list(NULL, A = paste0("a", 1:3), B = paste0("b", 1:4))
  x_array <- array(1:24, dim = .dim, dimnames = .dimnames)
  x <- rvar(x_array)

  x_1 <- rvar(array(x_array, dim = c(.dim, 1), dimnames = c(.dimnames, list(NULL))))
  expect_equal(abind::adrop(x_1, drop = NULL), x_1)
  expect_equal(abind::adrop(x_1, drop = 3), x)
  expect_equal(abind::adrop(x_1, drop = c(FALSE, FALSE, TRUE)), x)

  x_vector <- rvar(array(x_array, dim = c(2,12)))
  x_vector_unnamed <- x_vector
  names(x_vector) <- paste0("a", 1:12)
  expect_equal(abind::adrop(x_vector, drop = FALSE), x_vector)
  expect_equal(abind::adrop(x_vector, drop = FALSE, named.vector = FALSE), x_vector_unnamed)
})
