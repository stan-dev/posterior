# Tests for uniformity_test (main entry point) ---------------------------------
set.seed(123)

test_that("uniformity_test returns list with pvalue and pointwise", {
  pit <- runif(20)
  for (test in c("POT", "PIET", "PRIT")) {
    res <- posterior:::uniformity_test(pit, test)
    expect_type(res, "list")
    expect_named(res, c("pvalue", "pointwise"))
    expect_length(res$pvalue, 1)
    expect_true(res$pvalue >= 0 && res$pvalue <= 1)
    expect_equal(length(res$pointwise), length(pit))
  }
})

test_that("uniformity_test is deterministic for same inputs", {
  pit <- runif(15)
  for (test in c("POT", "PIET", "PRIT")) {
    res1 <- posterior:::uniformity_test(pit, test)
    res2 <- posterior:::uniformity_test(pit, test)
    expect_equal(res1$pvalue, res2$pvalue)
    expect_equal(res1$pointwise, res2$pointwise)
  }
})

test_that("uniformity_test handles single-element pit", {
  pit <- 0.4
  res <- posterior:::uniformity_test(pit, "PIET")
  expect_equal(length(res$pointwise), 1)
  expect_equal(res$pointwise, 0)  # Shapley for n=1 is 0
  expect_true(is.finite(res$pvalue))
  
})

test_that("uniformity_test errors on invalid test", {
  pit <- runif(5)
  expect_error(posterior:::uniformity_test(pit, "INVALID"),
               "'test' should be one of")
})

# Tests for the dependence-aware uniformity tests ------------------------------
test_that("piet_test computes correct p-values", {
  x <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  # Equivalent to 2 * min(1-x, x)
  expected <- c(0.2, 0.5, 1.0, 0.5, 0.2)
  
  expect_equal(.piet_test(x), expected, tolerance = 1e-7)
})

test_that("piet_test handles boundary values of 0 and 1", {
  x <- c(0, 1)
  expected <- c(0, 0)
  
  expect_equal(.piet_test(x), expected)
})

test_that("piet_test handles extreme values stably", {
  # Testing values very close to 0 and 1
  x <- c(1e-17, 1 - 1e-17)
  expected <- c(2e-17, 2e-17)
  
  # Tolerance needs to be adjusted for very small numbers
  expect_equal(.piet_test(x), expected, tolerance = 1e-16)
})

test_that("piet_test handles NA, NaN, and empty inputs correctly", {
  # NA and NaN propagation
  x_na <- c(0.5, NA, NaN)
  res_na <- .piet_test(x_na)
  
  expect_equal(res_na[1], 1.0)
  expect_true(is.na(res_na[2]))
  expect_true(is.nan(res_na[3]))
})

test_that("pot_test calculates correct p-values", {
  # Manually calculating expected values for x = c(0.2, 0.8), n = 2. 
  # Note: If X ~ Beta(1, b) then X ~ Kumaraswamy(1, b) with CDF 1 - (1 - x)^b 
  # and if X ~ Beta(a, 1) then X ~ Kumaraswamy(a, 1) with CDF to x^a
  # Beta(1, 2) CDF at 0.2 is 1 - (1 - 0.2)^2 = 0.36
  # Beta(2, 1) CDF at 0.8 is 0.8^2 = 0.64
  # p-values: 2 * min(0.36, 1-0.36) = 0.72; 2 * min(0.64, 1-0.64) = 0.72
  x <- c(0.8, 0.2)
  expected <- c(0.72, 0.72)
  
  expect_equal(.pot_test(x), expected)
})

test_that("pot_test handles boundary values correctly", {
  x <- c(0, 1)
  expected <- c(0, 0)
  
  expect_equal(.pot_test(x), expected)
})

test_that("pot_test bounds p-values between 0 and 1 for extreme out-of-bounds inputs", {
  # pbeta handles values outside [0, 1] by returning 0
  x <- c(-0.5, 1.5)
  expected <- c(0, 0)
  
  expect_equal(.pot_test(x), expected)
})

test_that("pot_test handles NAs", {
  x_na <- c(0.5, NA) # resulting in a = 2, b = 1
  # Beta(2, 1) at 0.5 = 0.25 -> 2 * min(0.25, 0.75) = 0.5
  expected <- c(0.5, NA)

  expect_equal(.pot_test(x_na), expected)
})

test_that("prit_test computes correct p-values", {
  # Let n = 2, x = c(0.5, 0.5) 
  # scaled_ecdf = 2 * c(1, 1) = c(2, 2)
  # probs1 = pbinom(1, 2, 0.5) = 0.75
  # probs2 = pbinom(2, 2, 0.5) = 1.00
  # p_val = 2 * min(1 - 0.75, 1.00) = 2 * 0.25 = 0.5
  
  x <- c(0.5, 0.5)
  expect_equal(.prit_test(x), c(0.5, 0.5))
})

# Test for computation of Shapley values -------------------------------------

test_that("compute_shapley_values handles empty vector and single element", {
  result_empty <- .compute_shapley_values(numeric(0))
  result_single <- .compute_shapley_values(5)

  expect_equal(result_empty, numeric(0))
  expect_equal(length(result_empty), 0)
  expect_equal(result_single, 0)
  expect_equal(length(result_single), 1)
})

test_that("compute_shapley_values for simple case", {
  x <- c(1, 2)
  result <- .compute_shapley_values(x)
  
  # Manual calculation for n=2:
  # harmonic_number = 1 + 1/2 = 1.5
  # For i=1: mean_others = 2/1 = 2
  # shapley[1] = (1/2)*1 + ((1.5-1)/2)*(1-2) = 0.5 - 0.25 = 0.25
  # For i=2: mean_others = 1/1 = 1
  # shapley[2] = (1/2)*2 + ((1.5-1)/2)*(2-1) = 1 + 0.25 = 1.25
  
  expected <- c(0.25, 1.25)
  expect_equal(result, expected, tolerance = 1e-10)
  expect_equal(length(result), 2)
})

test_that("compute_shapley_values handles mixed input values", {
  x <- c(-0.2, 0, 2, 3.1, 4.2)
  result <- .compute_shapley_values(x)
  expect_equal(length(result), 5)
  expect_true(all(is.finite(result)))
})

# Test for (truncated) Cauchy combination test ----------------------------------
test_that("cauchy_combination_test handles truncate = FALSE", {
  # avg = mean(-qcauchy(x))
  # result = 1 - pcauchy(avg)
  
  x <- c(0.1, 0.2, 0.3)
  result <- .cauchy_combination_test(x, truncate = FALSE)
  expected <- 1 - pcauchy(mean(-qcauchy(x)))
  
  expect_equal(result, expected, tolerance = 1e-10)
  expect_true(is.finite(result))
  expect_true(result >= 0 && result <= 1)
})

test_that("cauchy_combination_test handles truncate = TRUE", {
  # avg = mean(-qcauchy(x))
  # result = 1 - pcauchy(avg)
  
  x <- c(0.1, 0.2, 0.3, 0.4, 0.7, 0.8)
  result <- .cauchy_combination_test(x, truncate = TRUE)
  expected <- 1 - pcauchy(mean(-qcauchy(c(0.1, 0.2, 0.3, 0.4))))
  
  expect_equal(result, expected, tolerance = 1e-10)
  expect_true(is.finite(result))
  expect_true(result >= 0 && result <= 1)
})

test_that("cauchy_combination_test handles boundary values", {
  # x = 0: -qdf(0) = Inf and cdf(Inf) = 1 -> 1 - 1 = 0
  # x = 1: -qdf(1) = -Inf and cdf(-Inf) = 0 -> 1 - 0 = 1 
  
  expect_equal(.cauchy_combination_test(0, truncate = FALSE), 0)
  expect_equal(.cauchy_combination_test(1, truncate = FALSE), 1)
  expect_true(is.nan(.cauchy_combination_test(c(0, 1), truncate = FALSE)))
  # TODO: if 1 included in vector, CCT will always evaluate to 0 
  # as the mean evaluates to Inf and 1 - cdf(Inf) = 1 - 1 = 0
  expect_equal(.cauchy_combination_test(c(0, 0.3, 0.4, 1), truncate = TRUE), 0)
})

# Test for compute_cauchy -----------------------------------------------------

test_that("compute_cauchy computes correct transformations", {
  # For x = 0.5: tan((0.5 - 0.5) * pi) = tan(0) = 0
  expect_equal(.compute_cauchy(0.5), 0, tolerance = 1e-10)
  
  # For x = 0.25: tan((0.5 - 0.25) * pi) = tan(0.25 * pi) = 1
  expect_equal(.compute_cauchy(0.25), 1, tolerance = 1e-10)
  
  # For x = 0.75: tan((0.5 - 0.75) * pi) = tan(-0.25 * pi) = -1
  expect_equal(.compute_cauchy(0.75), -1, tolerance = 1e-10)
})