# as_rvar -----------------------------------------------------------------

test_that("as_rvar works", {
  expect_equal(draws_of(as_rvar(1L)), matrix(1L, dimnames = list("1", NULL)))
  expect_equal(draws_of(as_rvar(c(TRUE, FALSE))), matrix(c(TRUE, FALSE), nrow = 1, dimnames = list("1", NULL)))
  expect_equal(draws_of(as_rvar(1:3L)), matrix(1:3L, nrow = 1, dimnames = list("1", NULL)))

  expect_equal(nchains(as_rvar(1, nchains = 2)), 2)

  expect_equal(draws_of(as_rvar(1:6, dim = c(2,3))), array(1:6, dim = c(1,2,3), dimnames = list("1", NULL, NULL)))
  expect_equal(
    draws_of(as_rvar(1:6, dim = c(2,3), dimnames = list(letters[1:2], letters[1:3]))),
    array(1:6, dim = c(1,2,3), dimnames = list("1", letters[1:2], letters[1:3]))
  )

  expect_equal(
    draws_of(as_rvar(factor(letters[1:3], levels = letters[3:1]))),
    structure(matrix(3:1, nrow = 1, dimnames = list("1", NULL)), levels = letters[3:1], class = "factor")
  )
  expect_equal(
    draws_of(as_rvar(ordered(letters[1:3], levels = letters[3:1]))),
    structure(matrix(3:1, nrow = 1, dimnames = list("1", NULL)), levels = letters[3:1], class = c("ordered", "factor"))
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


# as_rvar_numeric/integer/logical ------------------------------------------------

test_that("as_rvar_numeric works", {
  x_array = array(
    as.numeric(1:24), dim = c(2,4,3),
    dimnames = list(NULL, A = paste0("a", 1:4), B = paste0("b", 1:3))
  )
  x <- rvar(x_array)
  x_array_letters = array(
    letters[1:24], dim = c(2,4,3),
    dimnames = list(NULL, A = paste0("a", 1:4), B = paste0("b", 1:3))
  )
  x_fct <- rvar_factor(x_array)
  x_ord <- rvar_ordered(x_array)

  expect_equal(as_rvar_numeric(x_fct), x)
  expect_equal(as_rvar_numeric(x_ord), x)
  expect_type(draws_of(as_rvar_numeric(x_ord)), "double")
  expect_type(draws_of(as_rvar_numeric(x_fct)), "double")
})

test_that("as_rvar_integer works", {
  x_array = array(
    1L:24L, dim = c(2,4,3),
    dimnames = list(NULL, A = paste0("a", 1:4), B = paste0("b", 1:3))
  )
  x <- rvar(x_array)
  x_array_letters = array(
    letters[1:24], dim = c(2,4,3),
    dimnames = list(NULL, A = paste0("a", 1:4), B = paste0("b", 1:3))
  )
  x_fct <- rvar_factor(x_array)
  x_ord <- rvar_ordered(x_array)

  expect_equal(as_rvar_integer(x_fct), x)
  expect_equal(as_rvar_integer(x_ord), x)
  expect_type(draws_of(as_rvar_integer(x_ord)), "integer")
  expect_type(draws_of(as_rvar_integer(x_fct)), "integer")
})

test_that("as_rvar_logical works", {
  x_array = array(
    rep(c(TRUE, FALSE), 12), dim = c(2,4,3),
    dimnames = list(NULL, A = paste0("a", 1:4), B = paste0("b", 1:3))
  )
  x <- rvar(x_array)
  x_array_letters = array(
    rep(c("TRUE", "FALSE"), 12), dim = c(2,4,3),
    dimnames = list(NULL, A = paste0("a", 1:4), B = paste0("b", 1:3))
  )
  x_fct <- rvar_factor(x_array)
  x_ord <- rvar_ordered(x_array)

  expect_equal(as_rvar_logical(x_fct), x)
  expect_equal(as_rvar_logical(x_ord), x)
  expect_type(draws_of(as_rvar_logical(x_ord)), "logical")
  expect_type(draws_of(as_rvar_logical(x_fct)), "logical")
})


# as_rvar_factor -----------------------------------------------------------------

test_that("as_rvar_factor works", {
  expect_equal(
    draws_of(as_rvar_factor(array(1:4, dim = c(2,2)))),
    structure(1:4L, levels = c("1", "2", "3", "4"), dim = c(1, 2, 2), dimnames = list("1", NULL, NULL), class = "factor")
  )
  expect_equal(
    draws_of(as_rvar_factor(array(c(TRUE, TRUE, FALSE, FALSE), dim = c(2,2)))),
    structure(c(2, 2, 1, 1), levels = c("FALSE", "TRUE"), dim = c(1, 2, 2), dimnames = list("1", NULL, NULL), class = "factor")
  )
  expect_equal(
    draws_of(as_rvar_factor(array(letters[1:4], dim = c(2,2)))),
    structure(1:4L, levels = letters[1:4], dim = c(1, 2, 2), dimnames = list("1", NULL, NULL), class = "factor")
  )
  expect_equal(
    draws_of(as_rvar_factor(`dim<-`(factor(letters[1:4], levels = letters[4:1]), c(2,2)))),
    structure(4:1, dim = c(1, 2, 2), dimnames = list("1", NULL, NULL), levels = letters[4:1], class = "factor")
  )
  expect_equal(
    draws_of(as_rvar_factor(`dim<-`(ordered(letters[1:4], levels = letters[4:1]), c(2,2)))),
    structure(4:1, dim = c(1, 2, 2), dimnames = list("1", NULL, NULL), levels = letters[4:1], class = c("ordered", "factor"))
  )

  expect_equal(nchains(as_rvar_factor(1, nchains = 2)), 2)

  expect_equal(
    as_rvar_factor(rvar(array(1:12, dim = c(2,2,3)))),
    rvar_factor(array(as.character(1:12), dim = c(2,2,3)), levels = as.character(1:12))
  )
})

test_that("as_rvar_ordered works", {
  expect_equal(
    draws_of(as_rvar_ordered(array(1:4, dim = c(2,2)))),
    structure(1:4L, levels = c("1", "2", "3", "4"), dim = c(1, 2, 2), dimnames = list("1", NULL, NULL), class = c("ordered", "factor"))
  )
  expect_equal(
    draws_of(as_rvar_ordered(as_rvar(array(1:4, dim = c(2,2))))),
    structure(1:4L, levels = c("1", "2", "3", "4"), dim = c(1, 2, 2), dimnames = list("1", NULL, NULL), class = c("ordered", "factor"))
  )
  expect_equal(
    draws_of(as_rvar_ordered(as_rvar_factor(array(1:4, dim = c(2,2))))),
    structure(1:4L, levels = c("1", "2", "3", "4"), dim = c(1, 2, 2), dimnames = list("1", NULL, NULL), class = c("ordered", "factor"))
  )
  expect_equal(
    draws_of(as_rvar_ordered(array(c(TRUE, TRUE, FALSE, FALSE), dim = c(2,2)))),
    structure(c(2, 2, 1, 1), levels = c("FALSE", "TRUE"), dim = c(1, 2, 2), dimnames = list("1", NULL, NULL), class = c("ordered", "factor"))
  )
  expect_equal(
    draws_of(as_rvar_ordered(array(letters[1:4], dim = c(2,2)))),
    structure(1:4L, levels = letters[1:4], dim = c(1, 2, 2), dimnames = list("1", NULL, NULL), class = c("ordered", "factor"))
  )
  expect_equal(
    draws_of(as_rvar_ordered(`dim<-`(factor(letters[1:4], levels = letters[4:1]), c(2,2)))),
    structure(4:1, dim = c(1, 2, 2), dimnames = list("1", NULL, NULL), levels = letters[4:1], class = c("ordered", "factor"))
  )
  expect_equal(
    draws_of(as_rvar_ordered(`dim<-`(ordered(letters[1:4], levels = letters[4:1]), c(2,2)))),
    structure(4:1, dim = c(1, 2, 2), dimnames = list("1", NULL, NULL), levels = letters[4:1], class = c("ordered", "factor"))
  )

  expect_equal(nchains(as_rvar_ordered(1, nchains = 2)), 2)

  expect_equal(
    as_rvar_ordered(rvar(array(1:12, dim = c(2,2,3)))),
    rvar_ordered(array(as.character(1:12), dim = c(2,2,3)), levels = as.character(1:12))
  )
})

test_that("as_rvar(<character>) produces an rvar_factor ", {
  expect_equal(
    draws_of(as_rvar(array(letters[1:4], dim = c(2,2)))),
    structure(1:4L, levels = letters[1:4], dim = c(1, 2, 2), dimnames = list("1", NULL, NULL), class = "factor")
  )
})

# casting to/from rvar/distribution ---------------------------------------

test_that("casting to/from rvar/distribution objects works", {
  x_dist <- distributional::dist_sample(list(a = c(1,1), b = 3:4))
  null_dist <- vctrs::vec_ptype(x_dist)
  x_rvar <- rvar(matrix(c(1,1,3:4), ncol = 2, dimnames = list(NULL, c("a","b"))))

  # casting to rvar
  expect_equal(vctrs::vec_cast(x_dist, rvar()), x_rvar)
  expect_equal(as_rvar(x_dist), x_rvar)

  # casting to rvar with a broadcast
  x_dist_bc <- distributional::dist_sample(list(a = 1, b = 3:4))
  expect_equal(vctrs::vec_cast(x_dist_bc, rvar()), x_rvar)

  # can't cast non-sample distributions to rvar
  expect_error(vctrs::vec_cast(distributional::dist_normal(), rvar()))

  # can't cast samples of incompatible sizes to rvar
  expect_error(vctrs::vec_cast(distributional::dist_sample(list(1:3, 1:2)), rvar()))

  # casting to distribution
  expect_equal(vctrs::vec_cast(x_rvar, null_dist), x_dist)

  # can't cast multivariate rvars to distributions
  x_mv <- rvar(array(1:8, dim = c(2,2,2)))
  expect_error(vctrs::vec_cast(x_mv, null_dist))
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

test_that("vec_is_list(<rvar>) is FALSE", {
  expect_equal(vctrs::vec_is_list(rvar()), FALSE)
})


# type conversion -----------------------------------------------------------

test_that("as.list works", {
  x_array <- array(
    1:24, dim = c(2,4,3),
    dimnames = list(NULL, A = paste0("a", 1:4), B = paste0("b", 1:3))
  )
  x <- new_rvar(x_array)

  expect_equal(as.list(x),
    list(
      a1 = new_rvar(x_array[,1,]),
      a2 = new_rvar(x_array[,2,]),
      a3 = new_rvar(x_array[,3,]),
      a4 = new_rvar(x_array[,4,])
    )
  )

  x_array <- array(
    1:12, dim = c(6, 2),
    dimnames = list(NULL, A = paste0("a", 1:2))
  )
  x <- new_rvar(x_array)

  expect_equal(as.list(x),
    list(
      a1 = new_rvar(x_array[,1]),
      a2 = new_rvar(x_array[,2])
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
    dimnames(draws_of(col)) <- list(NULL)
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
    dimnames(draws_of(col)) <- list(NULL)
    df3[[b_i + (c_i - 1) * 2]] <- col
  }
  expect_equal(as.data.frame(x3), df3)
  expect_equal(dimnames(as.data.frame(unname(x3))), dimnames(as.data.frame(mean(unname(x3)))))

  tibble3 <- as_tibble(df3)
  expect_equal(as_tibble(x3), tibble3)
})

test_that("as.character works", {
  x <- rvar(c(1,1))
  expect_equal(as.character(x), format(x))
  x <- rvar_factor(letters[1:2])
  expect_equal(as.character(x), format(x))
  x <- rvar_ordered(letters[1:2])
  expect_equal(as.character(x), format(x))
})

test_that("proxy restore works", {
  x1 = rvar(array(1:9, dim = c(3,3)),
    dimnames = list(A = paste0("a", 1:3))
  )
  x2 = rvar(array(1:12, dim = c(2,2,3)),
    dimnames = list(A = paste0("a", 1:2), B = paste0("b", 1:3))
  )
  x3 = rvar(array(1:24, dim = c(2,2,2,4)),
    dimnames = list(A = paste0("a", 1:2), B = paste0("b", 1:2), C = paste0("c", 1:4))
  )

  expect_equal(vec_restore(vec_proxy(unname(x1)), rvar()), unname(x1))
  expect_equal(vec_restore(vec_proxy(unname(x2)), rvar()), unname(x2))
  expect_equal(vec_restore(vec_proxy(unname(x3)), rvar()), unname(x3))
  expect_equal(vec_restore(vec_proxy(x1), rvar()), x1)
  expect_equal(vec_restore(vec_proxy(x2), rvar()), x2)
  expect_equal(vec_restore(vec_proxy(x3), rvar()), x3)

  expect_equal(
    vec_restore(c(
      vec_proxy(vctrs::vec_init(rvar(), 2)),
      vec_proxy(rvar(1:10, nchains = 2))
    ), rvar()),
    rvar(array(c(rep(NA, 10), rep(NA, 10), 1:10), dim = c(10, 3)), nchains = 2)
  )

  expect_equal(
    vec_restore(c(
      vec_proxy(vctrs::vec_init(rvar_factor(), 2)),
      vec_proxy(rvar_factor(letters[1:10], nchains = 2))
    ), rvar_factor()),
    rvar_factor(array(letters[c(rep(NA, 10), rep(NA, 10), 1:10)], dim = c(10, 3)), nchains = 2)
  )

  expect_equal(
    vec_restore(c(
      vec_proxy(vctrs::vec_init(rvar_ordered(), 2)),
      vec_proxy(rvar_ordered(letters[1:10], nchains = 2))
    ), rvar_ordered()),
    rvar_ordered(array(letters[c(rep(NA, 10), rep(NA, 10), 1:10)], dim = c(10, 3)), nchains = 2)
  )
})

test_that("proxy restore works when combining factors with non-factors", {
  ref_1a = as_rvar_factor(c("1", "a"), levels = c("1", "a"))
  ref_a1 = as_rvar_factor(c("a", "1"), levels = c("a", "1"))

  expect_equal(
    vec_restore(c(vec_proxy(rvar(1)), vec_proxy(rvar_factor("a"))), to = rvar()),
    ref_1a
  )
  expect_equal(
    # even if the second element is ordered, because new levels are added by the
    # first element, we expect order to be dropped and the result to be unordered
    vec_restore(c(vec_proxy(rvar(1)), vec_proxy(rvar_ordered("a"))), to = rvar()),
    ref_1a
  )
  expect_equal(
    vec_restore(c(vec_proxy(rvar_factor("a")), vec_proxy(rvar(1))), to = rvar()),
    ref_a1
  )
  expect_equal(
    vec_restore(c(vec_proxy(rvar_ordered("a")), vec_proxy(rvar(1))), to = rvar()),
    ref_a1
  )
})

# vctrs comparison proxies ---------------------------------------------------

test_that("vctrs grouping works", {
  x <- c(rvar(1:10), rvar(1:10), 1, rvar(1:10), 1)

  expect_equal(vctrs::vec_identify_runs(x), structure(c(1, 1, 2, 3, 4), n = 4))
  expect_equal(vctrs::vec_group_id(x), structure(c(1, 1, 2, 1, 2), n = 2))
})

test_that("vctrs comparison and ordering is not allowed on rvars", {
  x <- rvar(1)

  expect_error(vctrs::vec_order(x), "rvar does not support")
  expect_error(vctrs::vec_compare(x, x), "rvar does not support")
})
