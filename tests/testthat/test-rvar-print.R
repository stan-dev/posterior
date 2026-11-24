normalize_str_output <- function(x) {
  sub('= *chr "bar"', '= chr "bar"', x)
}

# print -------------------------------------------------------------------

test_that("print(<rvar>) works", {
  x <- rvar(array(1:12, dim = c(2,2,3)))
  x_with_chains <- rvar(array(1:12, dim = c(2,2,3)), nchains = 2)

  expect_snapshot({
    print(rvar())
    print(x)
    print(x_with_chains)
    print(x, summary = "median_mad")
  })
})

test_that("print(<rvar_factor>) works", {
  x <- rvar_factor(array(letters[1:12], dim = c(2,2,3)), nchains = 2)

  expect_snapshot(print(x))
})

test_that("print(<rvar_ordered>) works", {
  x <- rvar_ordered(array(letters[1:12], dim = c(2,2,3)), nchains = 2)

  expect_snapshot(print(x))
})


# str ---------------------------------------------------------------------

test_that("str(<rvar>) works", {
  x <- rvar(array(1:24, dim = c(2,3,4)))

  x_with_attrs <- x
  dimnames(x_with_attrs)[1] <- list(c("a","b","c"))
  attr(draws_of(x_with_attrs), "foo") <- list(1,2)
  attr(x_with_attrs, "bar") <- list(1,2)

  expect_snapshot({
    str(rvar())
    str(rvar(1:3))
    str(x, vec.len = 5)
    str(x_with_attrs, vec.len = 5)
  })
})

test_that("str(<rvar_factor>) works", {
  x <- rvar_factor(array(letters[1:24], dim = c(2,3,4)), nchains = 2)
  x[[1]] <- "a"
  attr(x, "foo") <- "bar"

  expect_snapshot(
    str(x, vec.len = 5),
    transform = normalize_str_output
  )
})

test_that("str(<rvar_ordered>) works", {
  x <- rvar_ordered(array(letters[1:24], dim = c(2,3,4)), nchains = 2)
  x[[1]] <- "a"
  attr(x, "foo") <- "bar"

  expect_snapshot(
    str(x, vec.len = 5),
    transform = normalize_str_output
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
    ignore_attr = c("dim", "dimnames")
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
