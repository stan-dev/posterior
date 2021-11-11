test_that("basic print.rvar works", {
  x <- rvar(array(1:12, dim = c(2,2,3)))
  x_with_chains <- rvar(array(1:12, dim = c(2,2,3)), nchains = 2)

  expect_output(print(rvar(), color = FALSE),
"rvar<1>[0] mean ± sd:
NULL",
    fixed = TRUE
  )

  expect_output(print(x, color = FALSE),
"rvar<2>[2,3] mean ± sd:\
     [,1]         [,2]         [,3]        \
[1,]  1.5 ± 0.71   5.5 ± 0.71   9.5 ± 0.71 \
[2,]  3.5 ± 0.71   7.5 ± 0.71  11.5 ± 0.71 ",
    fixed = TRUE
  )

  expect_output(print(x_with_chains, color = FALSE),
"rvar<1,2>[2,3] mean ± sd:\
     [,1]         [,2]         [,3]        \
[1,]  1.5 ± 0.71   5.5 ± 0.71   9.5 ± 0.71 \
[2,]  3.5 ± 0.71   7.5 ± 0.71  11.5 ± 0.71 ",
    fixed = TRUE
  )
})

test_that("basic str.rvar works", {
  x <- rvar(array(1:24, dim = c(2,3,4)))
  x_with_chains <- rvar(array(1:24, dim = c(2,3,4)), nchains = 2)

  expect_output(str(rvar()),
    " rvar<1>[0] ",
    fixed = TRUE
  )
  expect_output(str(rvar(1:3)),
    " rvar<3>[1]  2 ± 1",
    fixed = TRUE
  )
  expect_output(str(x, vec.len = 5),
    " rvar<2>[3,4]  1.5 ± 0.71  3.5 ± 0.71  5.5 ± 0.71  7.5 ± 0.71  9.5 ± 0.71 ...",
    fixed = TRUE
  )
  expect_output(str(x_with_chains, vec.len = 5),
    " rvar<1,2>[3,4]  1.5 ± 0.71  3.5 ± 0.71  5.5 ± 0.71  7.5 ± 0.71  9.5 ± 0.71 ...",
    fixed = TRUE
  )

  x_with_attrs <- x
  dimnames(x_with_attrs)[1] = list(c("a","b","c"))
  attr(draws_of(x_with_attrs), "foo") = list(1,2)
  attr(x_with_attrs, "bar") = list(1,2)

  expect_output(str(x_with_attrs, vec.len = 5),
    ' rvar<2>[3,4]  1.5 ± 0.71  3.5 ± 0.71  5.5 ± 0.71  7.5 ± 0.71  9.5 ± 0.71 ...
 - dimnames(*)=List of 2
  ..$ : chr [1:3] "a" "b" "c"
  ..$ : NULL
 - attr(draws_of(*), "foo")=List of 2
  ..$ : num 1
  ..$ : num 2
 - attr(*, "bar")=List of 2
  ..$ : num 1
  ..$ : num 2',
    fixed = TRUE
  )

})

test_that("glimpse on rvar works", {
  x_vec <- rvar(array(1:24, dim = c(6,4)))
  x_matrix <- rvar(array(1:24, dim = c(2,3,4)))

  expect_equal(format_glimpse(rvar()), NULL)
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
})
