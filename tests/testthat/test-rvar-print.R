
# print -------------------------------------------------------------------

test_that("print(<rvar>) works", {
  x <- rvar(array(1:12, dim = c(2,2,3)))
  x_with_chains <- rvar(array(1:12, dim = c(2,2,3)), nchains = 2)

  out <- capture.output(print(rvar(), color = FALSE))
  expect_match(
    out,
    regexp = "rvar<1>\\[0\\] mean . sd:",
    all = FALSE
  )
  expect_match(
    out,
    regexp = "rvar\\(\\)",
    all = FALSE
  )

  out <- capture.output(print(x, color = FALSE))
  expect_match(
    out,
    regexp = "rvar<2>\\[2,3\\] mean . sd:",
    all = FALSE
  )
  expect_match(
    out,
    regexp = "     \\[,1\\]         \\[,2\\]         \\[,3\\]        ",
    all = FALSE
  )
  expect_match(
    out,
    regexp = "\\[1,\\]  1.5 . 0.71   5.5 . 0.71   9.5 . 0.71 ",
    all = FALSE
  )
  out <- capture.output(print(x_with_chains, color = FALSE))
  expect_match(
    out,
    regexp = "rvar<1,2>\\[2,3\\] mean . sd:",
    all = FALSE
  )
  expect_match(
    out,
    regexp = "     \\[,1\\]         \\[,2\\]         \\[,3\\]        ",
    all = FALSE
  )
  expect_match(
    out,
    regexp = "\\[1,\\]  1.5 . 0.71   5.5 . 0.71   9.5 . 0.71 ",
    all = FALSE
  )
  expect_match(
    out,
    regexp = "\\[2,\\]  3.5 . 0.71   7.5 . 0.71  11.5 . 0.71",
    all = FALSE
  )
})

test_that("print(<rvar_factor>) works", {
  x <- rvar_factor(array(letters[1:12], dim = c(2,2,3)), nchains = 2)

  out <- capture.output(print(x, color = FALSE))
  expect_match(
    out,
    regexp = "rvar_factor<1,2>\\[2,3\\] mode <entropy>:",
    all = FALSE
  )
  expect_match(
    out,
    regexp = "\\[1,\\] a <0\\.28>  e <0\\.28>  i <0\\.28> ",
    all = FALSE
  )
  expect_match(
    out,
    regexp = "\\[2,\\] c <0\\.28>  g <0\\.28>  k <0\\.28> ",
    all = FALSE
  )
  expect_match(
    out,
    regexp = "12 levels: a b c d e f g h i j k l",
    all = FALSE
  )
})

test_that("print(<rvar_ordered>) works", {
  x <- rvar_ordered(array(letters[1:12], dim = c(2,2,3)), nchains = 2)

  out <- capture.output(print(x, color = FALSE))
  expect_match(
    out,
    regexp = "rvar_ordered<1,2>\\[2,3\\] mode <dissent>:",
    all = FALSE
  )
  expect_match(
    out,
    regexp = "\\[1,\\] a <0.067>  e <0.067>  i <0.067> ",
    all = FALSE
  )
  expect_match(
    out,
    regexp = "\\[2,\\] c <0.067>  g <0.067>  k <0.067> ",
    all = FALSE
  )
  expect_match(
    out,
    regexp = "12 levels: a < b < c < d < e < f < g < h < i < j < k < l",
    all = FALSE
  )
})


# str ---------------------------------------------------------------------

test_that("str(<rvar>) works", {
  x <- rvar(array(1:24, dim = c(2,3,4)))
  x_with_chains <- rvar(array(1:24, dim = c(2,3,4)), nchains = 2)

  expect_output(str(rvar()),
    " rvar<1>\\[0\\] "
  )
  out <- capture.output(str(rvar(1:3)))
  expect_match(
    out,
    regexp = " rvar<3>\\[1\\]  2 . 1",
    all = FALSE
  )
  out <- capture.output(str(x, vec.len = 5))
  expect_match(
    out,
    regexp = " rvar<2>\\[3,4\\]  1.5 . 0.71  3.5 . 0.71  5.5 . 0.71  7.5 . 0.71  9.5 . 0.71 ...",
    all = FALSE
  )

  x_with_attrs <- x
  dimnames(x_with_attrs)[1] <- list(c("a","b","c"))
  attr(draws_of(x_with_attrs), "foo") <- list(1,2)
  attr(x_with_attrs, "bar") <- list(1,2)

  out <- capture.output(str(x_with_attrs, vec.len = 5))
  expect_match(
    out,
    regexp = " rvar<2>\\[3,4\\]  1.5 . 0.71  3.5 . 0.71  5.5 . 0.71  7.5 . 0.71  9.5 . 0.71 ...",
    all = FALSE
  )
  expect_match(
    out,
    regexp = " - dimnames\\(\\*\\)=List of 2",
    all = FALSE
  )
  expect_match(
    out,
    regexp = '  \\.\\.\\$ : chr \\[1:3\\] "a" "b" "c"',
    all = FALSE
  )
})

test_that("str(<rvar_factor>) works", {
  x <- rvar_factor(array(letters[1:24], dim = c(2,3,4)), nchains = 2)
  x[[1]] <- "a"
  attr(x, "foo") <- "bar"

  out <- capture.output(str(x, vec.len = 5))
  expect_match(
    out,
    regexp = " rvar_factor<1,2>\\[3,4\\]  a <0\\.00>  c <0\\.22>  e <0\\.22>  g <0\\.22>  i <0\\.22> \\.\\.\\.",
    all = FALSE
  )
  expect_match(
    out,
    regexp = " - 24 levels: a b c d \\.\\.\\. x",
    all = FALSE
  )
  expect_match(
    out,
    regexp = ' - attr\\(\\*, "foo"\\)= chr "bar"',
    all = FALSE
  )
})

test_that("str(<rvar_ordered>) works", {
  x <- rvar_ordered(array(letters[1:24], dim = c(2,3,4)), nchains = 2)
  x[[1]] <- "a"
  attr(x, "foo") <- "bar"

  out <- capture.output(str(x, vec.len = 5))
  expect_match(
    out,
    regexp = " rvar_ordered<1,2>\\[3,4\\]  a <0\\.000>  c <0\\.032>  e <0\\.032>  g <0\\.032>  i <0\\.032> \\.\\.\\.",
    all = FALSE
  )
  expect_match(
    out,
    regexp = " - 24 levels: a < b < c < d < \\.\\.\\. < x",
    all = FALSE
  )
  expect_match(
    out,
    regexp = ' - attr\\(\\*, "foo"\\)= chr "bar"',
    all = FALSE
  )
})


# other -------------------------------------------------------------------

test_that("glimpse on rvar works", {
  skip_on_cran()
  x_vec <- rvar(array(1:24, dim = c(6,4)))
  x_matrix <- rvar(array(1:24, dim = c(2,3,4)))

  expect_equal(format_glimpse(rvar()), character(0))
  expect_equal(
    format_glimpse(x_vec),
    c("3.5 ± 1.9", "9.5 ± 1.9", "15.5 ± 1.9", "21.5 ± 1.9"),
    check.attributes = FALSE
  )
  expect_equal(format_glimpse(x_matrix), "<rvar[3 x 4]>")
})

test_that("format summary arg works", {
  x = rvar(c(1,9,10))

  expect_equal(as.vector(format(x, summary = "mean_sd")), "6.7 \u00b1 4.9")
  expect_equal(as.vector(format(x, summary = "median_mad")), "9 \u00b1 1.5")
  expect_error(format(x, summary = "foo"))
})

test_that("vec_ptype_abbr and vec_ptype_full work", {
  x <- rvar(array(1:24, dim = c(2,3,4)))
  x_with_chains <- rvar(array(1:24, dim = c(2,3,4)), nchains = 2)

  expect_equal(vec_ptype_abbr(x, suffix_shape = FALSE), "rvar")
  expect_equal(vec_ptype_full(rvar(1:3)), "rvar<3>")
  expect_equal(vec_ptype_full(x), "rvar<2>[,4]")
  expect_equal(vec_ptype_full(x_with_chains), "rvar<1,2>[,4]")

  x_fct <- rvar_factor(array(letters[1:24], dim = c(2,3,4)))
  expect_equal(vec_ptype_abbr(x_fct, suffix_shape = FALSE), "rvar_fct")

  x_ord <- rvar_ordered(array(letters[1:24], dim = c(2,3,4)))
  expect_equal(vec_ptype_abbr(x_ord, suffix_shape = FALSE), "rvar_ord")
})

test_that("formatting NA rvars works", {
  expect_equal(c(format(rvar(c(NA, 1)))), "NA \u00b1 NA")
  expect_equal(c(format(rvar_factor(c(NA, 1)))), "NA <NA>")
  expect_equal(c(format(rvar_ordered(c(NA, 1)))), "NA <NA>")
})
