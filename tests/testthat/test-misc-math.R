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

test_that("log1m_exp agrees with log(1 - exp(x))", {
  x <- c(-5, -1, -0.1)
  expect_equal(log1m_exp(x), log(1 - exp(x)))
})

test_that("log1m_exp is stable near zero", {
  # 1 - exp(-1e-20) rounds to 0, so the naive version gives -Inf
  expect_equal(log1m_exp(-1e-20), log(1e-20))
})

test_that("log1m_exp handles the boundaries", {
  expect_equal(log1m_exp(0), -Inf)
  expect_equal(log1m_exp(-Inf), 0)
})

test_that("log_normalize gives weights that sum to 1", {
  set.seed(1)
  x <- log(runif(10))
  expect_equal(sum(exp(log_normalize(x))), 1)
  expect_equal(log_normalize(x), log(exp(x) / sum(exp(x))))
})

test_that("log_normalize is stable for large inputs", {
  # exp(1000) overflows, so the naive version gives NaN
  expect_equal(log_normalize(c(1000, 1000)), rep(log(0.5), 2))
  expect_equal(log_normalize(c(-1000, -1000)), rep(log(0.5), 2))
})

test_that("log_normalize keeps zero weights at -Inf", {
  expect_equal(log_normalize(c(-Inf, 0, 0)), c(-Inf, log(0.5), log(0.5)))
})
