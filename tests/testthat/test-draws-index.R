test_that("indices of draws_matrix objects are correct", {
   x <- as_draws_matrix(example_draws())

   expect_equal(iteration_ids(x), 1:NROW(x))
   expect_equal(chain_ids(x), 1)
   expect_equal(draw_ids(x), 1:NROW(x))

   expect_equal(niterations(x), NROW(x))
   expect_equal(nchains(x), 1)
   expect_equal(ndraws(x), NROW(x))
})

test_that("indices of draws_array objects are correct", {
  x <- as_draws_array(example_draws())

  expect_equal(iteration_ids(x), 1:NROW(x))
  expect_equal(chain_ids(x), 1:NCOL(x))
  expect_equal(draw_ids(x), 1:(NROW(x) * NCOL(x)))

  expect_equal(niterations(x), NROW(x))
  expect_equal(nchains(x), NCOL(x))
  expect_equal(ndraws(x), NROW(x) * NCOL(x))
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
})

