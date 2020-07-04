# since rename_variables is just a wrapper around variables() this only
# tests it on draws_matrix for now. See the tests for variables() for
# format-specific tests.
test_that("rename_variables works on draws_matrix", {
  x = as_draws_matrix(matrix(11:20, ncol = 2, dimnames = list(NULL, c("a", "b[1]"))))

  ref = as_draws_matrix(matrix(11:20, ncol = 2, dimnames = list(NULL, c("mu", "b[1]"))))
  expect_equal(rename_variables(x, mu = a), ref)
  expect_equal(rename_variables(x, mu = "a"), ref)

  ref = as_draws_matrix(matrix(11:20, ncol = 2, dimnames = list(NULL, c("c", "d"))))
  expect_equal(rename_variables(x, c = a, d = `b[1]`), ref)
  expect_equal(rename_variables(x, c = a, d = "b[1]"), ref)

  # renaming can be chained
  expect_equal(rename_variables(x, e = a, d = `b[1]`, c = e), ref)

  # no renaming
  expect_equal(rename_variables(x), x)
})

test_that("rename_variables works for non-scalar variables", {
  x <- example_draws()
  x <- rename_variables(x, alpha = theta)
  vars <- c("mu", "tau", paste0("alpha[", 1:8, "]"))
  expect_equal(variables(x), vars)
})

test_that("cannot rename a variable to a reserved word", {
  x = as_draws_matrix(matrix(11:20, ncol = 2, dimnames = list(NULL, c("a", "b[1]"))))
  err = "Variable names.*are reserved"
  expect_error(rename_variables(x, .chain = a), err)
  expect_error(rename_variables(x, .iteration = a), err)
  expect_error(rename_variables(x, .draw = a), err)
})

test_that("cannot rename a variable to an existing variable name", {
  x = as_draws_matrix(matrix(11:20, ncol = 2, dimnames = list(NULL, c("a", "b[1]"))))
  expect_error(rename_variables(x, a = "b[1]"), "Duplicate variable names are not allowed")
})

test_that("cannot rename a variable to an empty name", {
  x = as_draws_matrix(matrix(11:20, ncol = 2, dimnames = list(NULL, c("a", "b[1]"))))
  expect_error(rename_variables(x, a), "Cannot rename a variable to an empty name")
})

test_that("cannot rename a non-existent variable", {
  x = as_draws_matrix(matrix(11:20, ncol = 2, dimnames = list(NULL, c("a", "b[1]"))))
  expect_error(rename_variables(x, a = c), "The following variables are missing")
})
