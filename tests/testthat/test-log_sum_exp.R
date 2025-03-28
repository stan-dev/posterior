# Ensure that log_sum_exp agrees with expected behaviour from log(sum(exp(x)))
test_that("log_sum_exp of for x containing Inf is Inf", {
  x <- c(Inf, 1)
  expect_equal(log_sum_exp(x), Inf)
})

test_that("log_sum_exp of -Inf is -Inf", {
  x <- c(-Inf, -Inf)
  expect_equal(log_sum_exp(x), -Inf)
})

test_that("log_sum_exp of empty input is -Inf", {
  x <- numeric(0)
  expect_equal(log_sum_exp(x), -Inf)
})

test_that("log_sum_exp works", {
  set.seed(1)
  x <- log(runif(10))
  expect_equal(log_sum_exp(x), log(sum(exp(x))))
})
