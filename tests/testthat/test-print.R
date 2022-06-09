test_that("print.draws_matrix runs without errors", {
  x <- as_draws_matrix(example_draws())
  expect_output(print(x),
    "A draws_matrix: 100 iterations, 4 chains, and 10 variables"
  )

  x <- weight_draws(x, rep(1, ndraws(x)))
  expect_output(
    print(x),
    "hidden reserved variables ..\\.log_weight.."
  )
})

test_that("print.draws_array runs without errors", {
  x <- as_draws_array(example_draws())
  expect_output(print(x),
    "A draws_array: 100 iterations, 4 chains, and 10 variables"
  )

  x <- weight_draws(x, rep(1, ndraws(x)))
  expect_output(
    print(x),
    "hidden reserved variables ..\\.log_weight.."
  )
})

test_that("print.draws_df runs without errors", {
  x <- as_draws_df(example_draws())
  expect_output(print(x),
    "A draws_df: 100 iterations, 4 chains, and 10 variables"
  )

  x <- weight_draws(x, rep(1, ndraws(x)))
  expect_output(print(x), "'\\.log_weight'")

  x <- subset(x, variable = c("mu", "tau"))
  x <- mutate_variables(x, tau2 = tau^2)
  expect_output(print(x), "tau2")
})

test_that("print.draws_list runs without errors", {
  x <- as_draws_list(example_draws())
  expect_output(print(x),
    "A draws_list: 100 iterations, 4 chains, and 10 variables"
  )

  x <- weight_draws(x, rep(1, ndraws(x)))
  expect_output(
    print(x),
    "hidden reserved variables ..\\.log_weight.."
  )
})

test_that("print.draws_rvars runs without errors", {
  skip_on_cran()
  skip_on_os("windows")
  x <- as_draws_rvars(example_draws())
  out <- capture.output(print(x))
  expect_match(
    out,
    regexp = "A draws_rvars: 100 iterations, 4 chains, and 3 variables",
    all = FALSE
  )

  x <- weight_draws(x, rep(1, ndraws(x)))
  expect_output(
    print(x),
    "hidden reserved variables ..\\.log_weight.."
  )
})

test_that("print.draws_array handles reserved variables correctly", {
  x <- as_draws_array(example_draws())
  variables(x)[1] <- ".log_weight" # reserved name
  expect_output(print(x, max_variables = 1), "variable = tau")
  expect_output(
    print(x),
    "hidden reserved variables ..\\.log_weight.."
  )
})

test_that("print.draws_matrix handles reserved variables correctly", {
  x <- as_draws_matrix(example_draws())
  variables(x)[1] <- ".log_weight" # reserved name
  expect_output(print(x, max_variables = 1), "tau")
  expect_output(
    print(x),
    "hidden reserved variables ..\\.log_weight.."
  )
})

test_that("print.draws_df handles reserved variables correctly", {
  x <- as_draws_df(example_draws())
  variables(x)[1] <- ".log_weight" # reserved name
  expect_output(print(x, max_variables = 1), "tau")
  expect_output(
    print(x),
    "hidden reserved variables {'.log_weight', '.chain', '.iteration', '.draw'}",
    fixed = TRUE
  )
})

test_that("print.draws_list handles reserved variables correctly", {
  x <- as_draws_list(example_draws())
  variables(x)[1] <- ".log_weight" # reserved name
  expect_output(print(x, max_variables = 1), "tau")
  expect_output(
    print(x),
    "hidden reserved variables ..\\.log_weight.."
  )
})

test_that("print.draws_rvars handles reserved variables correctly", {
  skip_on_os("windows")
  x <- as_draws_rvars(example_draws())
  variables(x)[1] <- ".log_weight" # reserved name
  expect_output(print(x, max_variables = 1), "tau")
  expect_output(
    print(x),
    "hidden reserved variables ..\\.log_weight.."
  )
})

test_that("print.draws_df correctly handles data frames with unrepaired draws", {
  x <- as_draws_df(list(x = 1:10, y = 2:11))
  x_slice <- x[c(1,3,5),]
  expect_output(
    print(x_slice),
"x +y
1 +1 +2
2 +3 +4
3 +5 +6"
  )
})
