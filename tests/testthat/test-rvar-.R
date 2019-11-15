test_that("indexing with [[ works on a vector", {
  x_array <- array(1:20, dim = c(4,5), dimnames = list(A = paste0("a", 1:4), NULL))
  x = new_rvar(x_array)

  # [[ indexing should drop names (but not indices)
  x_array_ref = x_array
  dimnames(x_array_ref) <- NULL

  expect_identical(x[[3]], new_rvar(x_array_ref[3,, drop = FALSE]))
  expect_identical(x[["a2"]], new_rvar(x_array_ref[2,, drop = FALSE]))

  expect_error(x[[]])
  expect_error(x[[NA]])
  expect_error(x[[NA_integer_]])
  expect_error(x[[6]])
  expect_error(x[[1,1]])
  expect_error(x[[NULL]])

  # different behavior from base vectors
  # base vectors convert these to numeric
  expect_error(x[[TRUE]])
  expect_error(x[[FALSE]])
})

test_that("indexing with [ works on a vector", {
  x_array = array(1:20, dim = c(4,5), dimnames = list(A = paste0("a", 1:4), NULL))
  x = new_rvar(x_array)

  expect_identical(x[], x)

  expect_identical(x[3], new_rvar(x_array[3,, drop = FALSE]))
  expect_identical(x["a2"], new_rvar(x_array["a2",, drop = FALSE]))
  expect_identical(x[c(1,3)], new_rvar(x_array[c(1,3),, drop = FALSE]))
  expect_identical(x[c("a2","a4")], new_rvar(x_array[c("a2","a4"),, drop = FALSE]))

  expect_identical(x[c(-1,-3)], new_rvar(x_array[c(-1,-3),, drop = FALSE]))

  expect_identical(x[TRUE], new_rvar(x_array[TRUE,, drop = FALSE]))
  expect_identical(x[c(TRUE,FALSE)], new_rvar(x_array[c(TRUE,FALSE),, drop = FALSE]))
  expect_identical(x[c(TRUE,FALSE,TRUE)], new_rvar(x_array[c(TRUE,FALSE,TRUE),, drop = FALSE]))
  expect_identical(x[c(TRUE,FALSE,FALSE,TRUE)], new_rvar(x_array[c(TRUE,FALSE,FALSE,TRUE),, drop = FALSE]))

  # dropping should preserve names
  expect_identical(x["a1", drop = TRUE], new_rvar(x_array["a1",, drop = FALSE]))
  expect_identical(x[1:2, drop = TRUE], new_rvar(x_array[1:2,, drop = FALSE]))

  # indexing beyond the end of the array should result in NAs, to mimic normal vector indexing
  expect_identical(x[c(4,5)], new_rvar(x_array[c(4,NA_integer_),, drop = FALSE]))
  expect_identical(x[c(8,9)], new_rvar(x_array[c(NA_integer_,NA_integer_),, drop = FALSE]))

  expect_identical(x[NA], new_rvar(x_array[NA,, drop = FALSE]))
  expect_identical(x[NA_integer_], new_rvar(x_array[NA_integer_,, drop = FALSE]))
  expect_identical(x[rep(NA_integer_,7)], new_rvar(x_array[rep(NA_integer_,7),, drop = FALSE]))

  expect_identical(x[NULL], new_rvar())

  expect_error(x[1,1])
})

test_that("rvars work in tibbles", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  x_array = array(1:20, dim = c(4,5), dimnames = list(A = paste0("a", 1:4), NULL))
  x = new_rvar(x_array)
  df = tibble::tibble(x, y = x + 1)

  expect_identical(df$x, x)
  expect_identical(df$y, new_rvar(x_array + 1))
  expect_identical(dplyr::mutate(df, z = x)$z, x)

  # TODO: fix
  # expect_equal(dplyr::mutate(df, z = x * 2)$z, new_rvar(x_array * 2))

  df = tibble::tibble(x, g = letters[1:4])
  ref = tibble::tibble(
    a = new_rvar(x_array["a1",, drop = FALSE]),
    b = new_rvar(x_array["a2",, drop = FALSE]),
    c = new_rvar(x_array["a3",, drop = FALSE]),
    d = new_rvar(x_array["a4",, drop = FALSE])
  )
  expect_identical(tidyr::pivot_wider(df, names_from = g, values_from = x), ref)

  # TODO: fix (will crash R)
  # df$y = df$x + 1
  # tidyr::pivot_wider(df, names_from = g, values_from = x)
})
