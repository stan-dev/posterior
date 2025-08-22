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


# weighted summaries ------------------------------------------------------

test_that("weighted discrete summaries work", {
  x <- c(0, 0, 0, 3, 3, 1)
  levs <- c("h","e","f","g")
  x_factor <- factor(c("h","h","h","g","g","e"), levels = levs)
  x_ordered <- ordered(c("h","h","h","g","g","e"), levels = levs)
  xw <- c(1, 2, 0, 3, 0)
  xw_factor <- factor(c("e","f","h","g","h"), levels = levs)
  xw_ordered <- ordered(c("e","f","h","g","h"), levels = levs)
  w <- c(1, 0, 1.25, 2, 1.75)

  expect_equal(modal_category(xw, w), modal_category(x))
  expect_equal(modal_category(xw_factor, w), modal_category(x_factor))
  expect_equal(modal_category(xw_ordered, w), modal_category(x_ordered))

  # entropy(xw, w) is equal to entropy(x_factor) because entropy(x_factor)
  # accounts for the missing level just as entropy(xw, w) accounts for the
  # element with 0 weight. entropy(x) cannot do this.
  expect_equal(entropy(xw, w), entropy(x_factor))
  expect_equal(entropy(xw_factor, w), entropy(x_factor))
  expect_equal(entropy(xw_ordered, w), entropy(x_ordered))

  expect_equal(dissent(xw, w), dissent(x))
  expect_equal(dissent(xw_factor, w), dissent(x_factor))
  expect_equal(dissent(xw_ordered, w), dissent(x_ordered))
})
