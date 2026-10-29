test_that("warning_no_call() adds the cause and shared classes", {
  w <- tryCatch(
    warning_no_call("a", 1, class = "posterior_warning_test"),
    warning = identity
  )
  expect_s3_class(w, "posterior_warning_test")
  expect_s3_class(w, "posterior_warning")
  expect_equal(conditionMessage(w), "a1")
  expect_null(conditionCall(w))
})
