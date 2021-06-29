test_that("indices work for NULL", {
   x <- NULL

   expect_equal(iteration_ids(NULL), NULL)
   expect_equal(chain_ids(NULL), NULL)
   expect_equal(draw_ids(NULL), NULL)

   expect_equal(niterations(NULL), 0)
   expect_equal(nchains(NULL), 0)
   expect_equal(ndraws(NULL), 0)
})

test_that("indices of draws_matrix objects are correct", {
   x <- as_draws_matrix(example_draws())

   expect_equal(iteration_ids(x), 1:100)
   expect_equal(chain_ids(x), 1:4)
   expect_equal(draw_ids(x), 1:NROW(x))

   expect_equal(niterations(x), NROW(x) / 4)
   expect_equal(nchains(x), 4)
   expect_equal(ndraws(x), NROW(x))

   rownames(x) <- NULL
   expect_equal(draw_ids(x), 1:NROW(x))
})

test_that("indices of draws_array objects are correct", {
  x <- as_draws_array(example_draws())

  expect_equal(iteration_ids(x), 1:NROW(x))
  expect_equal(chain_ids(x), 1:NCOL(x))
  expect_equal(draw_ids(x), 1:(NROW(x) * NCOL(x)))

  expect_equal(niterations(x), NROW(x))
  expect_equal(nchains(x), NCOL(x))
  expect_equal(ndraws(x), NROW(x) * NCOL(x))

  colnames(x) <- NULL
  rownames(x) <- NULL
  expect_equal(iteration_ids(x), 1:NROW(x))
  expect_equal(chain_ids(x), 1:NCOL(x))
})

test_that("indices of draws_df objects are correct", {
  x <- as_draws_df(example_draws())

  expect_equal(iteration_ids(x), unique(x$.iteration))
  expect_equal(chain_ids(x), unique(x$.chain))
  expect_equal(draw_ids(x), unique(x$.draw))

  expect_equal(niterations(x), length(unique(x$.iteration)))
  expect_equal(nchains(x), length(unique(x$.chain)))
  expect_equal(ndraws(x), length(unique(x$.draw)))
})

test_that("indices of draws_list objects are correct", {
  x <- as_draws_list(example_draws())

  expect_equal(iteration_ids(x), 1:length(x[[1]][[1]]))
  expect_equal(chain_ids(x), 1:length(x))
  expect_equal(draw_ids(x), 1:(length(x[[1]][[1]]) * length(x)))

  expect_equal(niterations(x), length(x[[1]][[1]]))
  expect_equal(nchains(x), length(x))
  expect_equal(ndraws(x), length(x[[1]][[1]]) * length(x))

  names(x) <- NULL
  expect_equal(chain_ids(x), 1:length(x))
})

test_that("indices of draws_rvars objects are correct", {
  x <- as_draws_rvars(example_draws())

  expect_equal(iteration_ids(x), 1:(length(draws_of(x[[1]]))/4))
  expect_equal(chain_ids(x), 1:4)
  expect_equal(draw_ids(x), 1:length(draws_of(x[[1]])))

  expect_equal(niterations(x), length(draws_of(x[[1]]))/4)
  expect_equal(nchains(x), 4)
  expect_equal(ndraws(x), length(draws_of(x[[1]])))
})

test_that("indexing draws_array with [ and drop works correctly", {
  x <- example_draws()
  x1 <- x[,,1]
  x2 <- x[,,1, drop=TRUE]
  expect_s3_class(x1, "draws_array")
  if (R.version$major >= "4") {
    expect_equal(class(x2), c("matrix", "array"))
  } else {
    expect_equal(class(x2), "matrix")
  }
  expect_length(dim(x1), 3)
  expect_length(dim(x2), 2)
  expect_equal(x2, extract_variable_matrix(x, "mu"))

  # drop=TRUE shouldn't do anything if multiple parameters selected
  x3 <- x[,,1:2, drop=TRUE]
  expect_s3_class(x3, "draws_array")
})

test_that("indexing draws_matrix with [ and drop works correctly", {
  x <- as_draws_matrix(example_draws())
  x1 <- x[,1]
  x2 <- x[,1, drop=TRUE]
  expect_s3_class(x1, "draws_matrix")
  expect_equal(class(x2), "numeric")
  expect_length(dim(x1), 2)
  expect_null(dim(x2))

  # drop=TRUE shouldn't do anything if multiple parameters selected
  x3 <- x[,1:2, drop=TRUE]
  expect_s3_class(x3, "draws_matrix")
})

test_that("indexing draws dimension draws_matrix triggers a warning", {
  options(posterior.warn_on_merge_chains = TRUE)
  x <- as_draws_matrix(example_draws())
  expect_warning(
    x1 <- x[1:3, ],
    "Chains were dropped due to manually indexing draws"
  )
  x <- merge_chains(x)
  x2 <- x[1:3, ]
  expect_equal(x1, x2)
  options(posterior.warn_on_merge_chains = FALSE)
})
