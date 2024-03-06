test_that('duplicate variable names are not allowed', {
  x = matrix(11:20, ncol = 2, dimnames = list(NULL, c("a", "a")))

  err = "Duplicate variable names are not allowed"
  expect_error(as_draws_matrix(x), err)
  expect_error(as_draws_df(x), err)
  expect_error(as_draws_list(x), err)
  expect_error(as_draws_array(x), err)
})

test_that("variables() work with NULL", {
  expect_equal(variables(NULL), NULL)
  expect_equal(nvariables(NULL), 0)
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
  x <- as_draws_df(matrix(11:20, ncol = 2, dimnames = list(NULL, c("a", "b"))))

  ref <- as_draws_df(matrix(11:20, ncol = 2, dimnames = list(NULL, c("x", "y[1]"))))

  expect_equal(variables(x), c("a", "b"))
  expect_equal(variables(ref), c("x", "y[1]"))

  variables(x) <- c("x", "y[1]")
  expect_equal(x, ref)
})

test_that("variables() and variables<-() work on draws_rvars", {
  x <- as_draws_rvars(matrix(11:20, ncol = 2, dimnames = list(NULL, c("a", "b"))))

  ref <- as_draws_rvars(matrix(11:20, ncol = 2, dimnames = list(NULL, c("x", "y[1]"))))

  # variables works a bit differently for draws_rvars
  expect_equal(variables(x), c("a", "b"))
  expect_equal(variables(ref), c("x", "y"))

  variables(x) <- c("x", "y")
  expect_equal(x, ref)
})

test_that("variables() works on draws_df with duplicate columns", {
  # in the annoying case where someone manually changes a draws_df to have duplicate columns,
  # make sure that variables() returns the correct result...
  x <- as_draws_df(matrix(11:20, ncol = 2, dimnames = list(NULL, c("a", "b"))))
  names(x)[names(x) == "b"] = "a"

  expect_equal(variables(x), c("a", "a"))
})

test_that("variables() works on NULL", {
  expect_equal(variables(NULL), NULL)
})

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

test_that("with_indices works", {
  x <- example_draws()
  draws <- list(
    array = as_draws_array(x),
    df = as_draws_df(x),
    list = as_draws_list(x),
    matrix = as_draws_matrix(x),
    rvars = as_draws_rvars(x)
  )

  mu_tau_theta = c(
    "mu", "tau", "theta[1]", "theta[2]", "theta[3]", "theta[4]",
    "theta[5]", "theta[6]", "theta[7]", "theta[8]"
  )
  a_b_c = c("a", "b", "c[1]", "c[2]", "c[3]", "c[4]", "c[5]", "c[6]", "c[7]", "c[8]")
  for (type in names(draws)) {
    expect_equal(variables(draws[[!!type]], with_indices = TRUE), mu_tau_theta)
    expect_equal(variables(draws[[!!type]], with_indices = FALSE), c("mu", "tau", "theta"))
    expect_equal(nvariables(draws[[!!type]], with_indices = TRUE), 10)
    expect_equal(nvariables(draws[[!!type]], with_indices = FALSE), 3)

    expect_equal(
      variables(set_variables(draws[[!!type]], a_b_c, with_indices = TRUE), with_indices = FALSE),
      c("a", "b", "c")
    )
    expect_equal(
      variables(set_variables(draws[[!!type]], c("a","b","c"), with_indices = FALSE), with_indices = TRUE),
      a_b_c
    )

    expect_error(
      set_variables(draws[[!!type]], c("a","c","c"), with_indices = FALSE),
      "[Dd]uplicate"
    )
  }

  for (type in head(names(draws), -1)) {
    expect_error(
      set_variables(draws[[!!type]], c("a","b"), with_indices = FALSE),
      "base name.*[Ll]engths must match"
    )
  }

  expect_error(
    set_variables(
      draws$rvars,
      c("a", "b", "XX[1]", "c[2]", "c[3]", "c[4]", "c[5]", "c[6]", "c[7]", "c[8]"),
      with_indices = TRUE
    )
  )
})
