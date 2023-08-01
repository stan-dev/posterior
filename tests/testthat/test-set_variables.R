# since set_variables is just a wrapper around variables() this only
# tests it on draws_matrix for now. See the tests for variables() for
# format-specific tests.
test_that("set_variables works on draws_matrix", {
  x <- as_draws_matrix(matrix(11:20, ncol = 2, dimnames = list(NULL, c("a", "b[1]"))))

  ref <- as_draws_matrix(matrix(11:20, ncol = 2, dimnames = list(NULL, c("c", "d"))))
  expect_equal(set_variables(x, c("c", "d")), ref)
})

test_that("cannot set a variable to a reserved word", {
  x <- as_draws_matrix(matrix(11:20, ncol = 2, dimnames = list(NULL, c("a", "b[1]"))))
  err <- "Variable names.*are reserved"
  expect_error(set_variables(x, c(".chain", "b[1]")), err)
  expect_error(set_variables(x, c(".iteration", "b[1]")), err)
  expect_error(set_variables(x, c(".draw", "b[1]")), err)
})

test_that("cannot set duplicate variable names", {
  x <- as_draws_matrix(matrix(11:20, ncol = 2, dimnames = list(NULL, c("a", "b[1]"))))
  expect_error(set_variables(x, c("a", "a")), "Duplicate variable names are not allowed")
})

