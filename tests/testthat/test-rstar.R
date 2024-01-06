
test_that("rstar returns reasonable values", {
  skip_if_not_installed("caret")
  skip_if_not_installed("randomForest")
  x <- example_draws()
  val <- rstar(x)
  expect_true(val > 0.8 & val < 10)
})

test_that("rstar works with 1d example", {
  skip_if_not_installed("caret")
  skip_if_not_installed("randomForest")
  x <- example_draws()
  x <- as_draws_df(x)
  # remove all bar one variable
  x <- x[, c(variables(x)[1], ".chain", ".iteration", ".draw")]
  val <- rstar(x)
  expect_true(val > 0.5 & val < 10)
})

test_that("rstar works with draws_df example", {
  skip_if_not_installed("caret")
  skip_if_not_installed("randomForest")
  x <- example_draws()
  x <- as_draws_df(x)
  val <- rstar(x)
  expect_true(val > 0.5 & val < 10)
})

test_that("rstar with uncertainty returns vectors of correct length", {
  skip_if_not_installed("caret")
  x <- example_draws()
  val <- rstar(x, method = "gbm", uncertainty = T, verbose = F)
  expect_equal(length(val), 1000)
  val <- rstar(x, method = "knn", uncertainty = T, nsimulations = 10)
  expect_equal(length(val), 10)
})

test_that("incorrect nsimulations values throws error", {
  skip_if_not_installed("caret")
  x <- example_draws()
  expect_error(rstar(x, method = "knn", nsimulations = 0),
               "'nsimulations' must be greater than or equal to 1.")
})

test_that("rstar with uncertainty returns reasonable values", {
  skip_if_not_installed("caret")
  x <- example_draws()
  val <- rstar(x, method = "gbm", uncertainty = T, verbose = F)
  expect_true(max(val) > 0.3 & min(val) < 10)
})

test_that("rstar accepts different classifiers", {
  skip_if_not_installed("caret")
  x <- example_draws()
  val <- rstar(x, method = "gbm", verbose=F)
  expect_true(is.numeric(val))
  val <- rstar(x, method = "knn")
  expect_true(is.numeric(val))
})

test_that("rstar accepts different hyperparameters", {
  skip_if_not_installed("caret")
  x <- example_draws()

  # use fast hyperparameters
  caret_grid <- data.frame(interaction.depth=c(3),
                       n.trees = 1,
                       shrinkage=c(0.1),
                       n.minobsinnode=10)
  start <- Sys.time()
  val <- rstar(x, method = "gbm", verbose=F,
               hyperparameters = caret_grid)
  end <- Sys.time()
  dif1 <- end - start
  # use slower hyperparameters
  caret_grid <- data.frame(interaction.depth=c(3),
                       n.trees = 1000,
                       shrinkage=c(0.1),
                       n.minobsinnode=10)
  start <- Sys.time()
  val <- rstar(x, method = "gbm", verbose=F,
               hyperparameters = caret_grid)
  end <- Sys.time()
  dif2 <- end - start
  expect_true(dif1 < dif2)
})

test_that("rstar accepts different training proportion", {
  skip_if_not_installed("caret")
  x <- example_draws()
  set.seed(12345)
  val1 <- rstar(x, method = "knn")
  val2 <- rstar(x, method = "knn", training_proportion = 0.1)
  expect_true(val1 > val2)
})

test_that("rstar throws error when passed invalid training_proportion", {
  skip_if_not_installed("caret")
  x <- example_draws()
  expect_error(rstar(x, method = "knn", training_proportion = 0),
               "'training_proportion' must be greater than 0 and less than 1")
  expect_error(rstar(x, method = "knn", training_proportion = 1),
               "'training_proportion' must be greater than 0 and less than 1")
})

test_that("split-chain R* returns generally higher values", {
  skip_if_not_installed("caret")
  skip_on_cran()  # reduces test time
  x <- example_draws()
  n <- 10
  vals_split <- vector(length = n)
  vals_unsplit <- vector(length = n)
  for(i in 1:n) {
    vals_split[i] <- rstar(x, method = "knn")
    vals_unsplit[i] <- rstar(x, method = "knn", split = FALSE)
  }
  expect_true(median(vals_split) > median(vals_unsplit))
})
