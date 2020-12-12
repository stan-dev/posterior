test_that("as_rvar works", {
  expect_equal(draws_of(as_rvar(1L)), matrix(1L, dimnames = list("1", NULL)))
  expect_equal(draws_of(as_rvar(c(TRUE, FALSE))), matrix(c(TRUE, FALSE), nrow = 1, dimnames = list("1", NULL)))
  expect_equal(draws_of(as_rvar(1:3L)), matrix(1:3L, nrow = 1, dimnames = list("1", NULL)))
  expect_equal(draws_of(as_rvar(1:3L)), matrix(1:3L, nrow = 1, dimnames = list("1", NULL)))

  expect_equal(nchains(as_rvar(1, .nchains = 2)), 2)

  expect_equal(draws_of(as_rvar(1:6, dim = c(2,3))), array(1:6, dim = c(1,2,3), dimnames = list("1", NULL, NULL)))
  expect_equal(
    draws_of(as_rvar(1:6, dim = c(2,3), dimnames = list(letters[1:2], letters[1:3]))),
    array(1:6, dim = c(1,2,3), dimnames = list("1", letters[1:2], letters[1:3]))
  )
})
