# modal_category ----------------------------------------------------------

test_that("modal_category works on vectors", {
  expect_equal(modal_category(NULL), NULL)
  expect_equal(modal_category(logical()), logical())
  expect_equal(modal_category(double()), double())
  expect_equal(modal_category(integer()), integer())
  expect_equal(modal_category(character()), character())
  expect_equal(modal_category(factor()), character())
  expect_equal(modal_category(ordered(NULL)), character())
  expect_equal(modal_category(NA), NA)

  expect_equal(modal_category(c(1,2.1,2.1,3,3)), 2.1)
  expect_equal(modal_category(c("a","b","b","c","c")), "b")
  expect_equal(modal_category(factor(c("a","b","b","c","c"))), "b")
})

test_that("modal_category works on rvars", {
  expect_equal(modal_category(rvar()), double())
  expect_equal(modal_category(rvar_factor()), character())
  expect_equal(modal_category(rvar_ordered()), character())

  expect_equal(modal_category(c(rvar(c(1,2.1,2.1,3,3)), rvar(1))), c(2.1, 1))
  expect_equal(modal_category(c(rvar(c("a","b","b","c","c")), rvar("c"))), c("b","c"))
})


# entropy -----------------------------------------------------------------

test_that("entropy works on vectors", {
  expect_equal(entropy(NULL), 0)
  expect_equal(entropy(1), 0)
  expect_equal(entropy(NA), NA_real_)

  x <- factor(c("a", "a", "b", "c"), levels = c("a", "b", "c", "d"))
  p <- c(0.5, 0.25, 0.25)
  # dividing by log(4), not log(3), to account for 0 in the "d" level
  expect_equal(entropy(x), -sum(p * log(p)) / log(4))
  # if we treat it as an integer, we divide by log(3) since it won't
  # know about the missing level
  expect_equal(entropy(as.integer(x)), -sum(p * log(p)) / log(3))
})

test_that("entropy works on rvars", {
  expect_equal(entropy(rvar()), numeric())

  x <- rvar(array(c("a","a","b","c", "d","d","d","d", "a","b","c","d"), dim = c(4, 3)))
  p <- c(0.5, 0.25, 0.25)
  # dividing by log(4), not log(3), to account for 0 in the "d" level
  expect_equal(entropy(x), c(-sum(p * log(p)) / log(4), 0, 1))

  # if we treat it as an integer, we divide by log(3) since it won't
  # know about the missing level
  expect_equal(entropy(as_rvar_numeric(x)), c(-sum(p * log(p)) / log(3), 0, 1))
})


# dissent -----------------------------------------------------------------

test_that("dissent works on vectors", {
  expect_equal(dissent(NULL), 0)
  expect_equal(dissent(1), 0)
  expect_equal(dissent(NA), NA_real_)

  x <- ordered(c("a", "a", "b", "c"), levels = c("a", "b", "c", "d"))
  p <- c(0.5, 0.25, 0.25)
  # dividing by 3, not 2, to account for the "d" level
  expect_equal(dissent(x), -sum(p * log2(1 - abs(1:3 - 1.75) / 3)))
  # if we treat it as an integer, we divide by 2 since it won't
  # know about the missing level at the end
  expect_equal(dissent(as.integer(x)), -sum(p * log2(1 - abs(1:3 - 1.75) / 2)))
})

test_that("dissent works on rvars", {
  expect_equal(dissent(rvar_ordered()), numeric())

  x <- rvar_ordered(array(c("a","a","b","c", "d","d","d","d", "a","a","d","d"), dim = c(4, 3)))
  p <- c(0.5, 0.25, 0.25)
  # dividing by 3, not 2, to account for the "d" level
  expect_equal(dissent(x), c(-sum(p * log2(1 - abs(1:3 - 1.75) / 3)), 0, 1))

  # if we treat it as an integer, we divide by 2 since it won't
  # know about the missing level at the end
  expect_equal(dissent(as_rvar_numeric(x)), c(-sum(p * log2(1 - abs(1:3 - 1.75) / 2)), 0, 1))
})
