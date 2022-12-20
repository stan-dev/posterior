
# c.rvar ------------------------------------------------------------------

test_that("c works on rvar", {
  x <- rvar(array(1:9, dim = c(3,3)))
  y <- rvar(array(2:10, dim = c(3,3), dimnames = list(NULL, c("a","b","c"))))
  x_y <- rvar(array(c(1:9, 2:10), dim = c(3,6), dimnames = list(NULL, c("","","","a","b","c"))))

  expect_equal(c(x), x)
  expect_equal(c(x, NULL), x)
  expect_equal(c(x, y), x_y)
  expect_equal(c(x, NULL, y), x_y)
  expect_equal(c(x, x), rvar(array(c(1:9, 1:9), dim = c(3,6))))

  expect_equal(c(z = x),
    rvar(array(1:9, dim = c(3,3), dimnames = list(NULL, c("z1","z2","z3"))))
  )

  expect_equal(c(z = c(x, y)),
    rvar(array(c(1:9, 2:10), dim = c(3,6), dimnames = list(NULL, c("z1","z2","z3","z.a","z.b","z.c"))))
  )

  expect_equal(c(x, 5), rvar(array(c(1:9, 5, 5, 5), dim = c(3,4))))
  expect_equal(vctrs::vec_c(5, x), rvar(array(c(5, 5, 5, 1:9), dim = c(3,4))))
  expect_equal(c(x, 5L), rvar(array(c(1:9, 5, 5, 5), dim = c(3,4))))
  expect_equal(vctrs::vec_c(5L, x), rvar(array(c(5, 5, 5, 1:9), dim = c(3,4))))

  expect_equal(c(x == 1, TRUE), rvar(array(c(1:9 == 1, TRUE, TRUE, TRUE), dim = c(3,4))))
  expect_equal(vctrs::vec_c(TRUE, x == TRUE), rvar(array(c(TRUE, TRUE, TRUE, 1:9 == 1), dim = c(3,4))))

  x_col <- x
  dim(x_col) <- c(3,1)
  expect_equal(c(x_col), x)
  expect_equal(c(x_col, y), x_y)
})

test_that("c works on rvar_factor", {
  x <- rvar(array(letters[1:9], dim = c(3,3)))
  y <- rvar(array(letters[2:10], dim = c(3,3), dimnames = list(NULL, c("a","b","c"))))
  x_y <- rvar(array(letters[c(1:9, 2:10)], dim = c(3,6), dimnames = list(NULL, c("","","","a","b","c"))))

  expect_equal(c(x), x)
  expect_equal(c(x, NULL), x)
  expect_equal(c(x, y), x_y)
  expect_equal(c(x, NULL, y), x_y)
  expect_equal(c(x, x), rvar(array(letters[c(1:9, 1:9)], dim = c(3,6))))

  expect_equal(c(x, "a"),
    rvar(array(c(letters[1:9], "a", "a", "a"), dim = c(3,4)))
  )
  expect_equal(vctrs::vec_c("a", x),
    rvar(array(c("a", "a", "a", letters[1:9]), dim = c(3,4)))
  )
  expect_equal(c(x, factor("a")),
    rvar(array(c(letters[1:9], "a", "a", "a"), dim = c(3,4)))
  )
  expect_equal(vctrs::vec_c(factor("a"), x),
    rvar(array(c("a", "a", "a", letters[1:9]), dim = c(3,4)))
  )
  expect_equal(c(x, "xx"),
    rvar(array(c(letters[1:9], "xx", "xx", "xx"), dim = c(3,4)))
  )
  expect_equal(c(x, factor("xx")),
    rvar(array(c(letters[1:9], "xx", "xx", "xx"), dim = c(3,4)))
  )
  expect_equal(c(x, ordered("xx")),
    rvar(array(c(letters[1:9], "xx", "xx", "xx"), dim = c(3,4)))
  )
  expect_equal(vctrs::vec_c(ordered("xx"), x),
    rvar_factor(array(c("xx", "xx", "xx", letters[1:9]), dim = c(3,4)), levels = c("xx", letters[1:9]))
  )

  expect_error(c(x, 5), "Can't combine")
  expect_error(c(x, 5L), "Can't combine")
  expect_error(c(rvar(), factor("a")), "Can't combine")
  expect_error(vctrs::vec_c(factor("a"), rvar()), "Can't combine")

  x_col <- x
  dim(x_col) <- c(3,1)
  expect_equal(c(x_col), x)
  expect_equal(c(x_col, y), x_y)
})

test_that("c works on rvar_ordered", {
  x <- rvar_ordered(array(letters[1:9], dim = c(3,3)), levels = letters)
  y <- rvar_ordered(array(letters[2:10], dim = c(3,3), dimnames = list(NULL, c("a","b","c"))), levels = letters)
  x_y <- rvar_ordered(
    array(letters[c(1:9, 2:10)], dim = c(3,6), dimnames = list(NULL, c("","","","a","b","c"))), levels = letters
  )

  expect_equal(c(x), x)
  expect_equal(c(x, NULL), x)
  expect_equal(c(x, y), x_y)
  expect_equal(c(x, NULL, y), x_y)
  expect_equal(c(x, x), rvar_ordered(array(letters[c(1:9, 1:9)], dim = c(3,6)), levels = letters))

  # demotion to factor when levels aren't all in common
  x_partial <- rvar_ordered(array(letters[1:9], dim = c(3,3)))
  y_partial <- rvar_ordered(array(letters[2:10], dim = c(3,3), dimnames = list(NULL, c("a","b","c"))))
  x_y_fct <- rvar_factor(array(letters[c(1:9, 2:10)], dim = c(3,6), dimnames = list(NULL, c("","","","a","b","c"))))
  expect_equal(c(x, y), x_y)

  expect_equal(
    c(x, "a"),
    rvar_ordered(array(c(letters[1:9], "a", "a", "a"), dim = c(3,4)), levels = letters)
  )
  expect_equal(vctrs::vec_c("a", x),
    rvar_ordered(array(c("a", "a", "a", letters[1:9]), dim = c(3,4)), levels = letters)
  )
  expect_equal(c(x, factor("a")),
    rvar_factor(array(c(letters[1:9], "a", "a", "a"), dim = c(3,4)), levels = letters)
  )
  expect_equal(vctrs::vec_c(factor("a"), x),
    rvar_factor(array(c("a", "a", "a", letters[1:9]), dim = c(3,4)), levels = letters)
  )
  expect_equal(c(x, "xx"),
    rvar_factor(array(c(letters[1:9], "xx", "xx", "xx"), dim = c(3,4)), levels = c(letters, "xx"))
  )
  expect_equal(c(x, factor("xx")),
    rvar_factor(array(c(letters[1:9], "xx", "xx", "xx"), dim = c(3,4)), levels = c(letters, "xx"))
  )
  expect_equal(c(x, ordered("xx")),
    rvar_factor(array(c(letters[1:9], "xx", "xx", "xx"), dim = c(3,4)), levels = c(letters, "xx"))
  )
  expect_equal(vctrs::vec_c(ordered("xx"), x),
    rvar_factor(array(c("xx", "xx", "xx", letters[1:9]), dim = c(3,4)), levels = c("xx", letters))
  )
  expect_equal(vctrs::vec_c(ordered("a"), x),
    rvar_factor(array(c("a", "a", "a", letters[1:9]), dim = c(3,4)), levels = letters)
  )
  expect_equal(vctrs::vec_c(ordered("a", levels = letters), x),
    rvar_ordered(array(c("a", "a", "a", letters[1:9]), dim = c(3,4)), levels = letters)
  )

  expect_error(c(x, 5), "Can't combine")
  expect_error(c(x, 5L), "Can't combine")
  expect_error(c(rvar(), ordered("a")), "Can't combine")
  expect_error(vctrs::vec_c(ordered("a"), rvar()), "Can't combine")

  x_col <- x
  dim(x_col) <- c(3,1)
  expect_equal(c(x_col), x)
  expect_equal(c(x_col, y), x_y)
})


# cbind.rvar --------------------------------------------------------------

test_that("cbind works on rvar", {
  x = rvar(array(1:9, dim = c(3,3)))
  y = rvar(array(2:10, dim = c(3,3)))

  expect_equal(cbind(rvar(array(1:9, dim = c(3,3)))), rvar(array(1:9, dim = c(3,3,1))))
  expect_equal(cbind(x), rvar(array(1:9, dim = c(3,3,1), dimnames = list(NULL, NULL, "x"))))

  expect_equal(cbind(x, y, deparse.level = 0), rvar(array(c(1:9, 2:10), dim = c(3,3,2))))
  expect_equal(cbind(a = x, y, deparse.level = 0),
    rvar(array(c(1:9, 2:10), dim = c(3,3,2), dimnames = list(NULL, NULL, c("a", ""))))
  )
  expect_equal(cbind(a = x, y),
    rvar(array(c(1:9, 2:10), dim = c(3,3,2), dimnames = list(NULL, NULL, c("a", "y"))))
  )
  expect_equal(cbind(x, b = y, deparse.level = 0),
    rvar(array(c(1:9, 2:10), dim = c(3,3,2), dimnames = list(NULL, NULL, c("", "b"))))
  )
  expect_equal(cbind(x, y + 1, deparse.level = 2),
    rvar(array(c(1:9, 2:10 + 1), dim = c(3,3,2), dimnames = list(NULL, NULL, c("x", "y + 1"))))
  )
  expect_equal(cbind(x, y, y + 1, deparse.level = 2),
    rvar(array(c(1:9, 2:10, 2:10 + 1), dim = c(3,3,3), dimnames = list(NULL, NULL, c("x", "y", "y + 1"))))
  )
  expect_equal(cbind(x, y, y + 1, deparse.level = 2), cbind(cbind(x, y), y + 1, deparse.level = 2))

  x_col <- x
  dim(x_col) <- c(3,1)
  expect_equal(cbind(x_col, y, deparse.level = 0), rvar(array(c(1:9, 2:10), dim = c(3,3,2))))
  expect_equal(cbind(a = x_col, y),
    rvar(array(c(1:9, 2:10), dim = c(3,3,2), dimnames = list(NULL, NULL, c("", "y"))))
  )
  dimnames(x_col)[[2]] = "b"
  expect_equal(cbind(a = x_col, y),
    rvar(array(c(1:9, 2:10), dim = c(3,3,2), dimnames = list(NULL, NULL, c("b", "y"))))
  )

  expect_equal(cbind(NULL, x), cbind(x))
  expect_equal(cbind(x, NULL, y), cbind(x, y))
})

test_that("cbind works on rvar_factor", {
  x = rvar(array(letters[1:9], dim = c(3,3)))
  y = rvar(array(letters[2:10], dim = c(3,3)))

  expect_equal(cbind(rvar(array(letters[1:9], dim = c(3,3)))), rvar_factor(array(letters[1:9], dim = c(3,3,1))))
  expect_equal(cbind(x), rvar_factor(array(letters[1:9], dim = c(3,3,1), dimnames = list(NULL, NULL, "x"))))

  expect_equal(cbind(x, y, deparse.level = 0), rvar_factor(array(letters[c(1:9, 2:10)], dim = c(3,3,2))))
  expect_equal(cbind(a = x, y, deparse.level = 0),
    rvar_factor(array(letters[c(1:9, 2:10)], dim = c(3,3,2), dimnames = list(NULL, NULL, c("a", ""))))
  )
  expect_equal(cbind(a = x, y),
    rvar_factor(array(letters[c(1:9, 2:10)], dim = c(3,3,2), dimnames = list(NULL, NULL, c("a", "y"))))
  )
  expect_equal(cbind(x, b = y, deparse.level = 0),
    rvar_factor(array(letters[c(1:9, 2:10)], dim = c(3,3,2), dimnames = list(NULL, NULL, c("", "b"))))
  )

  x_col <- x
  dim(x_col) <- c(3,1)
  expect_equal(cbind(x_col, y, deparse.level = 0), rvar_factor(array(letters[c(1:9, 2:10)], dim = c(3,3,2))))
  expect_equal(cbind(a = x_col, y),
    rvar(array(letters[c(1:9, 2:10)], dim = c(3,3,2), dimnames = list(NULL, NULL, c("", "y"))))
  )
  dimnames(x_col)[[2]] = "b"
  expect_equal(cbind(a = x_col, y),
    rvar_factor(array(letters[c(1:9, 2:10)], dim = c(3,3,2), dimnames = list(NULL, NULL, c("b", "y"))))
  )

  expect_equal(cbind(NULL, x), cbind(x))
  expect_equal(cbind(x, NULL, y), cbind(x, y))
})

test_that("cbind works on rvar with data frames", {
  # these do not work on R < 4 for some reason related to how data frames
  # handle binding (so, not much we can do about it?)
  skip_if_not(R.version$major >= 4)

  x = rvar(array(1:9, dim = c(3,3)))
  y = rvar(array(2:10, dim = c(3,3)))

  expect_equal(cbind(data.frame(x), y + 1), data.frame(x = x, `y + 1` = y + 1, check.names = FALSE))
  expect_equal(cbind(x + 1, data.frame(y)), data.frame(`x + 1` = x + 1, y = y, check.names = FALSE))
})


# rbind.rvar --------------------------------------------------------------

test_that("rbind works on rvar", {
  x <- rvar(array(1:9, dim = c(3,3)))
  y <- rvar(array(2:10, dim = c(3,3)))
  x_y_array <- abind(
    array(1:9, dim = c(3,1,3)),
    array(2:10, dim = c(3,1,3)),
    along = 2
  )
  x_yp1_array <- abind(
    array(1:9, dim = c(3,1,3)),
    array(2:10 + 1, dim = c(3,1,3)),
    along = 2
  )

  expect_equal(rbind(rvar(array(1:9, dim = c(3,3)))), rvar(array(1:9, dim = c(3,1,3))))
  expect_equal(rbind(x), rvar(array(1:9, dim = c(3,1,3), dimnames = list(NULL, "x", NULL))))

  expect_equal(rbind(x, y, deparse.level = 0), rvar(x_y_array))
  expect_equal(rbind(a = x, y, deparse.level = 0), rvar(x_y_array, dimnames = list(c("a",""), NULL)))
  expect_equal(rbind(a = x, y), rvar(x_y_array, dimnames = list(c("a","y"), NULL)))
  expect_equal(rbind(x, b = y, deparse.level = 0), rvar(x_y_array, dimnames = list(c("","b"), NULL)))
  expect_equal(rbind(x, y + 1, deparse.level = 2), rvar(x_yp1_array, dimnames = list(c("x","y + 1"), NULL)))
  expect_equal(rbind(x, y, y + 1, deparse.level = 2), rbind(rbind(x, y), y + 1, deparse.level = 2))

  x_row <- x
  dim(x_row) <- c(1,3)
  expect_equal(rbind(x_row, y, deparse.level = 0), rvar(x_y_array))
  expect_equal(rbind(a = x_row, y), rvar(x_y_array, dimnames = list(c("","y"), NULL)))
  dimnames(x_row)[[1]] = "b"
  expect_equal(rbind(a = x_row, y), rvar(x_y_array, dimnames = list(c("b","y"), NULL)))

  expect_equal(rbind(NULL, x), rbind(x))
  expect_equal(rbind(x, NULL, y), rbind(x, y))

  expect_equal(rbind(data.frame(x), data.frame(x = y)), data.frame(x = c(x, y)))
})

test_that("rbind works on rvar_factor", {
  x <- rvar_factor(array(letters[1:9], dim = c(3,3)))
  y <- rvar_factor(array(letters[2:10], dim = c(3,3)))
  x_y_array <- abind(
    array(letters[1:9], dim = c(3,1,3)),
    array(letters[2:10], dim = c(3,1,3)),
    along = 2
  )

  expect_equal(rbind(rvar(array(letters[1:9], dim = c(3,3)))), rvar_factor(array(letters[1:9], dim = c(3,1,3))))
  expect_equal(rbind(x), rvar(array(letters[1:9], dim = c(3,1,3), dimnames = list(NULL, "x", NULL))))

  expect_equal(rbind(x, y, deparse.level = 0), rvar_factor(x_y_array))
  expect_equal(rbind(a = x, y, deparse.level = 0), rvar_factor(x_y_array, dimnames = list(c("a",""), NULL)))
  expect_equal(rbind(a = x, y), rvar_factor(x_y_array, dimnames = list(c("a","y"), NULL)))
  expect_equal(rbind(x, b = y, deparse.level = 0), rvar_factor(x_y_array, dimnames = list(c("","b"), NULL)))

  x_row <- x
  dim(x_row) <- c(1,3)
  expect_equal(rbind(x_row, y, deparse.level = 0), rvar_factor(x_y_array))
  expect_equal(rbind(a = x_row, y), rvar_factor(x_y_array, dimnames = list(c("","y"), NULL)))
  dimnames(x_row)[[1]] = "b"
  expect_equal(rbind(a = x_row, y), rvar_factor(x_y_array, dimnames = list(c("b","y"), NULL)))

  expect_equal(rbind(NULL, x), rbind(x))
  expect_equal(rbind(x, NULL, y), rbind(x, y))

  # rbind does not work with rvar_factor because it treats anything with non-null
  # levels() as a factor and skips the rvar code entirely, causing an error. So
  # we'll test with vctrs::vec_rbind() instead
  expect_equal(vctrs::vec_rbind(data.frame(x), data.frame(x = y)), data.frame(x = c(x, y)))
})
