# Copyright (C) 2020 Ben Lambert, Aki Vehtari
# See LICENSE.md for more details

#' Calculate R* convergence diagnostic
#'
#' The `rstar()` function generates a measure of convergence for MCMC draws
#' based on whether it is possible to determine the Markov chain that generated
#' a draw with probability greater than chance. To do so, it fits a machine
#' learning classifier to a training set of MCMC draws and evaluates its
#' predictive accuracy on a testing set: giving the ratio of accuracy to
#' predicting a chain uniformly at random.
#'
#' @family diagnostics
#' @param x (draws) A [`draws_df`] object or one coercible to a `draws_df` object.
#' @template args-conv-split
#'
#' @param uncertainty (logical). Indicates whether to provide a vector of R*
#'   values representing uncertainty in the calculated value (if `TRUE`) or a
#'   single value (if `FALSE`). The default is `TRUE.`
#'
#' @param method (string) The machine learning classifier to use (must be
#'   available in the \pkg{caret} package). The default is `"rf"`, which calls
#'   the random forest classifier.
#'
#' @param hyperparameters (named list) Hyperparameter settings passed to the classifier.
#'   The default for the random forest classifier (`method = "rf"`) is
#'   `list(mtry = floor(sqt(nvariables(x))))`.
#'   The default for the gradient-based model (`method = "gbm"`) is
#'   `list(interaction.depth = 3, n.trees = 50, shrinkage = 0.1, n.minobsinnode = 10)`.
#'
#' @param nsimulations (positive integer) The number of R* values in the
#'   returned vector if `uncertainty` is `TRUE`. The default is `1000.`
#'
#' @param training_proportion (positive real) The proportion (in `(0,1)`) of
#'   iterations in used to train the classifier. The default is `0.7`.
#'
#' @param ... Other arguments passed to `caret::train()`.
#'
#' @details The `rstar()` function provides a measure of MCMC convergence based
#'   on whether it is possible to determine the chain that generated a
#'   particular draw with a probability greater than chance. To do so, it fits a
#'   machine learning classifier to a subset of the original MCMC draws (the
#'   training set) and evaluates its predictive accuracy on the remaining draws
#'   (the testing set). If predictive accuracy exceeds chance (i.e. predicting
#'   the chain that generated a draw uniformly at random), the diagnostic
#'   measure R* will be above 1, indicating that convergence has yet to occur.
#'   This statistic is recently developed, and it is currently unclear what is a
#'   reasonable threshold for diagnosing convergence.
#'
#'   The statistic, R*, is stochastic, meaning that each time the test is run,
#'   unless the random seed is fixed, it will generally produce a different
#'   result. To minimize the implications of this stochasticity, it is
#'   recommended to repeatedly run this function to calculate a distribution of
#'   R*; alternatively, an approximation to this distribution can be obtained by
#'   setting `uncertainty = TRUE`, although this approximation of uncertainty
#'   will generally have a lower mean.
#'
#'   By default, a random forest classifier is used (`method = "rf"`), which tends
#'   to perform best for target distributions of around 4 dimensions and above.
#'   For lower dimensional targets, gradient boosted models (called via
#'   `method = "gbm"`) tend to have a higher classification accuracy. On a given
#'   MCMC sample, it is recommended to try both of these classifiers.
#'
#' @return A numeric vector of length 1 (by default) or length `nsimulations`
#'   (if `uncertainty = TRUE`).
#'
#' @references Ben Lambert, Aki Vehtari (2020) R*: A robust MCMC convergence
#'   diagnostic with uncertainty using gradient-boosted machines.
#'   *arXiv preprint* `arXiv:2003.07900`.
#'
#' @examples
#' \donttest{
#' if (require("caret", quietly = TRUE)) {
#'   x <- example_draws("eight_schools")
#'   print(rstar(x))
#'   print(rstar(x, split = FALSE))
#'   print(rstar(x, method = "gbm"))
#'   # can pass additional arguments to methods
#'   print(rstar(x, method = "gbm", verbose = FALSE))
#'
#'   # with uncertainty, returns a vector of R* values
#'   hist(rstar(x, uncertainty = TRUE))
#'   hist(rstar(x, uncertainty = TRUE, nsimulations = 100))
#'
#'   # can use other classification methods in caret library
#'   print(rstar(x, method = "knn"))
#' }
#' }
#' @export
rstar <- function(x, split = TRUE,
                  uncertainty = FALSE,
                  method = "rf",
                  hyperparameters = NULL,
                  training_proportion = 0.7,
                  nsimulations = 1000,
                  ...) {

  # caret requires itself to be attached to the search list (not just loaded).
  # To avoid polluting the user's namespace we manually attach caret and its
  # two hard dependencies (ggplot2 and lattice) if they aren't already attached,
  # and unload any of them we had to attach when this function exits.
  loaded_packages <- character()
  on.exit(for (package in loaded_packages) {
    detach(paste0("package:", package), character.only = TRUE)
  })
  for (package in setdiff(c("ggplot2", "lattice", "caret"), .packages())) {
    if (!suppressPackageStartupMessages(get("require")(package, character.only = TRUE, quietly = TRUE))) {
      stop_no_call("Package '", package, "' is required for 'rstar' to work.")
    }
    # store in reverse order since we'll unload in reverse
    loaded_packages <- c(package, loaded_packages)
  }

  split <- as_one_logical(split)
  uncertainty <- as_one_logical(uncertainty)
  method <- as_one_character(method)
  nsimulations <- as_one_integer(nsimulations)
  if (nsimulations < 1) {
    stop_no_call("'nsimulations' must be greater than or equal to 1.")
  }
  training_proportion <- as_one_numeric(training_proportion)
  if (training_proportion <= 0 || training_proportion >= 1) {
    stop_no_call("'training_proportion' must be greater than 0 and less than 1.")
  }

  # caret requires data.frame like objects
  x <- as_draws_df(x)
  if (split) {
    x <- split_chains(x)
  }

  # caret requires at least two variables to work
  if (nvariables(x) == 1) {
    x$.random <- rnorm(ndraws(x))
  }

  # choose hyperparameters
  if (method == "rf" && is.null(hyperparameters)) {
    caret_grid <- data.frame(mtry = floor(sqrt(nvariables(x))))
  } else if (method == "gbm" && is.null(hyperparameters)) {
    caret_grid <- data.frame(
      interaction.depth = 3,
      n.trees = 50,
      shrinkage = 0.1,
      n.minobsinnode = 10
    )
  } else {
    if (!(is.null(hyperparameters) || is.list(hyperparameters))) {
      stop_no_call("'hyperparameters' must be a list or NULL.")
    }
    caret_grid <- hyperparameters
  }

  # reserved variables should not be used for classification
  reserved_vars <- all_reserved_variables(x)
  reserved_vars <- setdiff(reserved_vars, ".chain")
  x <- remove_variables(x, reserved_vars)
  ndraws <- ndraws(x)
  class(x) <- "data.frame"
  x$.chain <- as.factor(x$.chain)

  # create training / testing sets
  random_draws <- sample(seq_len(ndraws), training_proportion * ndraws)
  training_data <- x[random_draws, ]
  testing_data <- x[-random_draws, ]

  # predict chain index using all other variables
  fit <- caret::train(
    .chain ~ .,
    data = training_data,
    method = method,
    trControl = caret::trainControl(method = 'none'),
    tuneGrid = caret_grid,
    ...
  )

  # calculate classification accuracy and then R*
  # not all chains may be represented in testing_data
  nchains <- length(unique(testing_data$.chain))
  if (uncertainty) {
    probs <- predict(object = fit, newdata = testing_data, type = "prob")
    m_accuracy <- matrix(nrow = nrow(probs), ncol = nsimulations)
    for (j in seq_len(NROW(probs))) {
      vals <- rmultinom(nsimulations, 1, prob = probs[j, ])
      test <- apply(vals, 2, function(v) which(v == 1))
      m_accuracy[j, ] <- ifelse(test == testing_data$.chain[j], 1, 0)
    }
    out <- colMeans(m_accuracy) * nchains
  } else {
    plda <- predict(object = fit, newdata = testing_data)
    res <- data.frame(predicted = plda, actual = testing_data$.chain)
    accuracy <- mean(res$predicted == res$actual)
    out <- accuracy * nchains
  }
  out
}
