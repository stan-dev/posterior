test_that("remove_variables works correctly for draws_matrix objects", {
  x <- as_draws_matrix(example_draws())
  expect_equal(posterior:::remove_variables(x, NULL), x)
  x <- posterior:::remove_variables(x, c("mu", "tau"))
  expect_equal(variables(x), paste0("theta[", 1:8, "]"))
})

test_that("remove_variables works correctly for draws_array objects", {
  x <- as_draws_array(example_draws())
  expect_equal(posterior:::remove_variables(x, NULL), x)
  x <- posterior:::remove_variables(x, c("mu", "tau"))
  expect_equal(variables(x), paste0("theta[", 1:8, "]"))
})

test_that("remove_variables works correctly for draws_df objects", {
  x <- as_draws_df(example_draws())
  expect_equal(posterior:::remove_variables(x, NULL), x)
  x <- posterior:::remove_variables(x, c("mu", "tau"))
  expect_equal(variables(x), paste0("theta[", 1:8, "]"))
})

test_that("remove_variables works correctly for draws_list objects", {
  x <- as_draws_list(example_draws())
  expect_equal(posterior:::remove_variables(x, NULL), x)
  x <- posterior:::remove_variables(x, c("mu", "tau"))
  expect_equal(variables(x), paste0("theta[", 1:8, "]"))
})

test_that("remove_variables works correctly for draws_rvars objects", {
  x <- as_draws_rvars(example_draws())
  expect_equal(posterior:::remove_variables(x, NULL), x)
  x <- posterior:::remove_variables(x, c("mu", "tau"))
  expect_equal(variables(x), "theta")
})
