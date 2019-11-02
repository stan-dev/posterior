test_that('duplicate variable names are not allowed', {
  x = matrix(11:20, ncol = 2, dimnames = list(NULL, c("a", "a")))

  expect_error(as_draws_matrix(x))
  expect_error(as_draws_df(x))
  expect_error(as_draws_list(x))
  expect_error(as_draws_array(x))
})

test_that("variables() and variables<-() work on draws_matrix", {
  x <- as_draws_matrix(matrix(11:20, ncol = 2, dimnames = list(NULL, c("a", "b"))))

  ref <- as_draws_matrix(matrix(11:20, ncol = 2, dimnames = list(NULL, c("x", "y[1]"))))

  expect_equal(variables(x), c("a", "b"))
  expect_equal(variables(ref), c("x", "y[1]"))

  variables(x) <- c("x", "y[1]")
  expect_equal(x, ref)
})

test_that("variables() and variables<-() work on draws_array", {
  x <- as_draws_array(matrix(11:20, ncol = 2, dimnames = list(NULL, c("a", "b"))))

  ref <- as_draws_array(matrix(11:20, ncol = 2, dimnames = list(NULL, c("x", "y[1]"))))

  expect_equal(variables(x), c("a", "b"))
  expect_equal(variables(ref), c("x", "y[1]"))

  variables(x) <- c("x", "y[1]")
  expect_equal(x, ref)
})

test_that("variables() and variables<-() work on draws_list", {
  x <- as_draws_list(matrix(11:20, ncol = 2, dimnames = list(NULL, c("a", "b"))))

  ref <- as_draws_list(matrix(11:20, ncol = 2, dimnames = list(NULL, c("x", "y[1]"))))

  expect_equal(variables(x), c("a", "b"))
  expect_equal(variables(ref), c("x", "y[1]"))

  variables(x) <- c("x", "y[1]")
  expect_equal(x, ref)
})

test_that("variables() and variables<-() work on draws_df", {
  x <- as_draws_list(matrix(11:20, ncol = 2, dimnames = list(NULL, c("a", "b"))))

  ref <- as_draws_list(matrix(11:20, ncol = 2, dimnames = list(NULL, c("x", "y[1]"))))

  expect_equal(variables(x), c("a", "b"))
  expect_equal(variables(ref), c("x", "y[1]"))

  variables(x) <- c("x", "y[1]")
  expect_equal(x, ref)
})
