# as_rvar -----------------------------------------------------------------

test_that("as_rvar works", {
  expect_equal(draws_of(as_rvar(1L)), matrix(1L, dimnames = list("1", NULL)))
  expect_equal(draws_of(as_rvar(c(TRUE, FALSE))), matrix(c(TRUE, FALSE), nrow = 1, dimnames = list("1", NULL)))
  expect_equal(draws_of(as_rvar(1:3L)), matrix(1:3L, nrow = 1, dimnames = list("1", NULL)))
  expect_equal(draws_of(as_rvar(1:3L)), matrix(1:3L, nrow = 1, dimnames = list("1", NULL)))

  expect_equal(nchains(as_rvar(1, nchains = 2)), 2)

  expect_equal(draws_of(as_rvar(1:6, dim = c(2,3))), array(1:6, dim = c(1,2,3), dimnames = list("1", NULL, NULL)))
  expect_equal(
    draws_of(as_rvar(1:6, dim = c(2,3), dimnames = list(letters[1:2], letters[1:3]))),
    array(1:6, dim = c(1,2,3), dimnames = list("1", letters[1:2], letters[1:3]))
  )
})

test_that("as_rvar preserves dimension names", {
  m <- diag(1:3)
  dimnames(m) <- list(a = paste0("a", 1:3), b = paste0("b", 1:3))
  m_rvar <- as_rvar(m)
  expect_equal(dimnames(m_rvar), dimnames(m))

  x <- 1:3
  names(x) <- c("a","b","c")
  x_rvar <- as_rvar(x)
  expect_equal(names(x_rvar), names(x))
})


# type predicates ---------------------------------------------------------

test_that("is.matrix/array on rvar works", {
  x_mat <- rvar(array(1:24, dim = c(2,2,6)))
  x_arr <- rvar(array(1:24, dim = c(2,2,3,2)))

  expect_true(is.matrix(x_mat))
  expect_true(is.array(x_mat))
  expect_false(is.matrix(x_arr))
  expect_true(is.array(x_arr))
})


# type conversion -----------------------------------------------------------

test_that("as.list works", {
  x_array = array(
    1:24, dim = c(2,4,3),
    dimnames = list(NULL, A = paste0("a", 1:4), B = paste0("b", 1:3))
  )
  x = new_rvar(x_array)

  expect_equal(as.list(x),
    list(
      a1 = new_rvar(x_array[,1,]),
      a2 = new_rvar(x_array[,2,]),
      a3 = new_rvar(x_array[,3,]),
      a4 = new_rvar(x_array[,4,])
    )
  )
})

test_that("as.vector works", {
  x = rvar(array(1:12, dim = c(2, 2, 3)))
  dimnames(x) <- list(c("a","b"), c("c","d","e"))

  expect_equal(as.vector(x), rvar(array(1:12, dim = c(2, 6))))
})

test_that("as.data.frame works on rvar", {
  x = rvar(array(1:9, dim = c(3,3)))
  y = rvar(array(2:10, dim = c(3,3)))

  expect_equal(as.data.frame(y), data.frame(y = y))
  expect_equal(names(as.data.frame(y)), "y")
  x_col <- x
  dim(x_col) <- c(3,1)
  expect_equal(as.data.frame(x_col), data.frame(V1 = x))
})


