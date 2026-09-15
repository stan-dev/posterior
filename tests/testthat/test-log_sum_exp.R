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

test_that("log_sum_exp shifts by the maximum", {
  # exp(x - 0) underflows for very negative x, so the shift has to be max(x)
  expect_equal(log_sum_exp(c(-800, -800)), -800 + log(2))
  expect_equal(log_sum_exp(rep(-1000, 5)), -1000 + log(5))
  expect_equal(log_sum_exp(c(-800, -Inf)), -800)
  expect_equal(log_sum_exp(c(-745, -745)), -745 + log(2))

  # the result must shift with the input rather than depend on its scale
  x <- c(-1, -2, -3)
  for (shift in c(0, 500, -500, -1000)) {
    expect_equal(log_sum_exp(x + shift), log_sum_exp(x) + shift)
  }
})

test_that("log_sum_exp works", {
  set.seed(1)
  x <- log(runif(10))
  expect_equal(log_sum_exp(x), log(sum(exp(x))))
})
