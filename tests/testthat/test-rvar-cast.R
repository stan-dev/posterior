# as_rvar -----------------------------------------------------------------

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


# type predicates ---------------------------------------------------------

test_that("is.matrix/array on rvar works", {
  x_mat <- rvar(array(1:24, dim = c(2,2,6)))
  x_arr <- rvar(array(1:24, dim = c(2,2,3,2)))

  expect_true(is.matrix(x_mat))
  expect_true(is.array(x_mat))
  expect_false(is.matrix(x_arr))
  expect_true(is.array(x_arr))
})


# type conversion -----------------------------------------------------------

test_that("as.list works", {
  x_array = array(
    1:24, dim = c(2,4,3),
    dimnames = list(NULL, A = paste0("a", 1:4), B = paste0("b", 1:3))
  )
  x = new_rvar(x_array)

  expect_equal(as.list(x),
    list(
      a1 = new_rvar(x_array[,1,]),
      a2 = new_rvar(x_array[,2,]),
      a3 = new_rvar(x_array[,3,]),
      a4 = new_rvar(x_array[,4,])
    )
  )
})

test_that("as.vector works", {
  x = rvar(array(1:12, dim = c(2, 2, 3)))
  dimnames(x) <- list(c("a","b"), c("c","d","e"))

  expect_equal(as.vector(x), rvar(array(1:12, dim = c(2, 6))))
})

test_that("as.data.frame and as_tibble work on rvars", {
  x1 = rvar(array(1:9, dim = c(3,3)),
    dimnames = list(A = paste0("a", 1:3))
  )
  x2 = rvar(array(1:12, dim = c(2,2,3)),
    dimnames = list(A = paste0("a", 1:2), B = paste0("b", 1:3))
  )
  x3 = rvar(array(1:24, dim = c(2,2,2,4)),
    dimnames = list(A = paste0("a", 1:2), B = paste0("b", 1:2), C = paste0("c", 1:4))
  )

  # constructing reference data frames with rvars in them without having that
  # code call as.data.frame() (defeating the purpose of the test) requires
  # bypassing the data.frame() constructor being called on an rvar, as it would
  # call as.data.frame.rvar(). Hence the twisty code below.

  # nulls
  df0 <- data.frame()
  df0[["rvar()"]] <- rvar()
  row.names(df0) <- numeric()
  expect_equal(as.data.frame(rvar()), df0)

  tibble0 <- as_tibble(df0)
  names(tibble0) <- "value"
  expect_equal(as_tibble(rvar()), tibble0)

  # 1-dim arrays
  df1 <- as.data.frame(mean(x1))
  names(df1) <- "x1"
  df1$x1 <- x1
  dimnames(df1$x1)["A"] <- list(NULL)
  expect_equal(as.data.frame(x1), df1)

  tibble1 <- as_tibble(df1)
  names(tibble1) <- "value"
  expect_equal(as_tibble(x1), tibble1)

  # 2-dim arrays
  df2 <- as.data.frame(mean(x2))
  for (i in 1:3) {
    col <- x2[,i,drop = TRUE]
    dimnames(col) <- list(NULL)
    df2[[i]] <- col
  }
  expect_equal(as.data.frame(x2), df2)
  expect_equal(dimnames(as.data.frame(unname(x2))), dimnames(as.data.frame(mean(unname(x2)))))

  tibble2 <- as_tibble(df2)
  expect_equal(as_tibble(x2), tibble2)

  # 3-dim arrays
  df3 <- as.data.frame(mean(x3))
  for (c_i in 1:4) for (b_i in 1:2) {
    col <- x3[,b_i,c_i,drop = TRUE]
    dimnames(col) <- list(NULL)
    df3[[b_i + (c_i - 1) * 2]] <- col
  }
  expect_equal(as.data.frame(x3), df3)
  expect_equal(dimnames(as.data.frame(unname(x3))), dimnames(as.data.frame(mean(unname(x3)))))

  tibble3 <- as_tibble(df3)
  expect_equal(as_tibble(x3), tibble3)
})


