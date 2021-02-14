test_that("as_rvar works", {
  expect_equal(draws_of(as_rvar(1L)), matrix(1L, dimnames = list("1", NULL)))
  expect_equal(draws_of(as_rvar(c(TRUE, FALSE))), matrix(c(TRUE, FALSE), nrow = 1, dimnames = list("1", NULL)))
  expect_equal(draws_of(as_rvar(1:3L)), matrix(1:3L, nrow = 1, dimnames = list("1", NULL)))
  expect_equal(draws_of(as_rvar(1:3L)), matrix(1:3L, nrow = 1, dimnames = list("1", NULL)))

  expect_equal(nchains(as_rvar(1, nchains = 2)), 2)

  expect_equal(draws_of(as_rvar(1:6, dim = c(2,3))), array(1:6, dim = c(1,2,3), dimnames = list("1", NULL, NULL)))
  expect_equal(
    draws_of(as_rvar(1:6, dim = c(2,3), dimnames = list(letters[1:2], letters[1:3]))),
    array(1:6, dim = c(1,2,3), dimnames = list("1", letters[1:2], letters[1:3]))
  )
})

test_that("as_rvar preserves dimension names", {
  m <- diag(1:3)
  dimnames(m) <- list(a = paste0("a", 1:3), b = paste0("b", 1:3))
  m_rvar <- as_rvar(m)
  expect_equal(dimnames(m_rvar), dimnames(m))

  x <- 1:3
  names(x) <- c("a","b","c")
  x_rvar <- as_rvar(x)
  expect_equal(names(x_rvar), names(x))
})
