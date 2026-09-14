# Ensure that log_sum_exp agrees with expected behaviour from log(sum(exp(x)))
test_that("log_sum_exp of for x containing Inf is Inf", {
  x <- c(Inf, 1)
  expect_equal(log_sum_exp(x), Inf)
})

test_that("log_sum_exp of -Inf is -Inf", {
  x <- c(-Inf, -Inf)
  expect_equal(log_sum_exp(x), -Inf)
})

test_that("log_sum_exp ignores -Inf terms among finite ones", {
  x <- c(-Inf, log(2), -Inf, log(3))
  expect_equal(log_sum_exp(x), log(5))
  expect_equal(log_sum_exp(x), log_sum_exp(c(log(2), log(3))))

  # a single finite term survives any number of zero-probability terms
  expect_equal(log_sum_exp(c(rep(-Inf, 1000), -700)), -700)
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
