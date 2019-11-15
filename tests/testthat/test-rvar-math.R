normalize = function(x) {
  # dimnames may be a list of NULLs after some ops, we don't care about this difference
  # for comparison purposes in tests, so fix it
  if (all(sapply(dimnames(x), is.null))) {
    dimnames(x) <- NULL
  }
  x
}

test_that("rvar arithmetic works", {
  x = new_rvar(1:10)

  expect_identical(normalize(x + 1), new_rvar(1:10 + 1))
})
