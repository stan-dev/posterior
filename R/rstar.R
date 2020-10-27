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
#' @param x A [`draws_df`] object or one coercible to a `draws_df` object.
#'
#' @param split_chains Logical. Indicaters whether to split chains into two
#'   equal halves. Default is `TRUE`.
#'
#' @param uncertainty Logical. Indicates whether to provide a vector of R*
#'   values representing uncertainty in the calculated value (if `TRUE`) or a
#'   single value (if `FALSE`). Default is `TRUE.`
#'
#' @param method The machine learning classifier to use (must be available in
#'   the \pkg{caret} package). The default is `"rf"`, which calls the random
#'   forest classifier.
#'
#' @param hyperparameters A named list of hyperparameter settings passed to the
#'   classifier. Default for `"rf"` classifier is `mtry=floor(sqt(nvariables))`;
#'   default for gradient-based model (`method="gbm"`) is `interaction.depth=3,
#'   n.trees = 50, shrinkage=0.1, n.minobsinnode=10`.
#'
#' @param nsimulations Number of R* values in the returned vector if
#'   `uncertainty` is `TRUE`. Default is 1000.
#'
#' @param training_proportion Proportion of iterations used to train the
#'   classifier. Default is 0.7.
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
#'   result. To minimise the implications of this stochasticity, it is
#'   recommended to repeatedly run this function to calculate a distribution of
#'   R*; alternatively, an approximation to this distribution can be obtained by
#'   setting `uncertainty = TRUE`, although this approximation of uncertainty
#'   will generally have a lower mean.
#'
#'   By default, a random forest classifier is used (`method="rf"`), which tends
#'   to perform best for target distributions of around 4 dimensions and above.
#'   For lower dimensional targets, gradient boosted models (called via
#'   `method="gbm"`) tend to have a higher classification accuracy. On a given
#'   MCMC sample, it is recommended to try both of these classifiers.
#'
#' @return A numeric vector of length 1 (by default) or length `nsimulations`
#'   (if `uncertainty=TRUE`).
#'
#' @references Ben Lambert, Aki Vehtari (2020) R*: A robust MCMC convergence
#'   diagnostic with uncertainty using gradient-boosted machines \emph{arXiv
#'   preprint} \code{arXiv:2003.07900}.
#'
#' @examples
#' x <- example_draws("eight_schools")
#' rstar(x)
#' rstar(x, split_chains = FALSE)
#' rstar(x, method = "gbm")
#' # can pass additional arguments to methods
#' rstar(x, method = "gbm", verbose = FALSE)
#'
#' # with uncertainty, returns a vector of R* values
#' hist(rstar(x, uncertainty = TRUE))
#' hist(rstar(x, uncertainty = TRUE, nsimulations = 100))
#'
#' # can use other classification methods in caret library
#' rstar(x, method = "knn")
#'
#' @export
rstar <-
  function(x,
           split_chains = TRUE,
           uncertainty = FALSE,
           method = "rf",
           hyperparameters = NULL,
           training_proportion = 0.7,
           nsimulations = 1000,
           ...) {

  if (!"caret" %in% (.packages())) {
    stop("Package \"caret\" needed for this function to work. Please load it.")
  }

  nsimulations <- round(nsimulations)
  if (nsimulations < 1) {
    stop("nsimulations must exceed 1.")
  }

  if (training_proportion <= 0 || training_proportion >= 1) {
    stop("training_proportion must be greater than zero and less than 1.")
  }

  x <- as_draws_df(x)
  if (split_chains) {
    x <- split_chains_draws_df(x)
  }

  # if only 1 param, add in a column of random noise
  nvars <- nvariables(x)
  if (nvars==1) {
    x$V_new <- rnorm(nrow(x))
  }

  # create training / testing sets
  x$.chain <- as.factor(x$.chain)
  rand_samples <- sample(1:nrow(x), training_proportion * nrow(x))
  training_data <- x[rand_samples, ]
  testing_data <- x[-rand_samples, ]

  # choose hyperparameters
  if (method=="rf" && is.null(hyperparameters)) {
    caret_grid <- data.frame(mtry=floor(sqrt(nvars)))
  } else if(method=="gbm" && is.null(hyperparameters)) {
    caret_grid <- data.frame(interaction.depth=3,
                             n.trees = 50,
                             shrinkage=0.1,
                             n.minobsinnode=10)
  } else {
    caret_grid <- hyperparameters
  }

  # remove iteration / draws columns and fit classifier
  training_data$.iteration <- training_data$.draw <- NULL
  class(training_data) <- class(training_data)[-(1:2)]
  fit <- caret::train(
    .chain ~ .,
    data = training_data,
    method = method,
    trControl = caret::trainControl(method = 'none'),
    tuneGrid = caret_grid,
    ...
  )

  # calculate classification accuracy then R*
  nchains <- length(unique(testing_data$.chain))
  if (uncertainty) {
    probs <- predict(object=fit, newdata=testing_data, type = "prob")
    m_accuracy <- matrix(nrow = nrow(probs),
                         ncol = nsimulations)
    for (j in seq_len(NROW(probs))) {
      vals <- rmultinom(nsimulations, 1, prob = probs[j, ])
      test <- apply(vals, 2, function(x) which(x == 1))
      m_accuracy[j, ] <- ifelse(test == testing_data$.chain[j], 1, 0)
    }
    return(colMeans(m_accuracy) * nchains)
  } else {
    plda <- predict(object=fit, newdata=testing_data)
    res <- data.frame(predicted=plda, actual=testing_data$.chain)
    accuracy <- mean(res$predicted == res$actual)
    return(accuracy * nchains)
  }
}


# internal ----------------------------------------------------------------

# splits chains into halves within a `draws_df` object
split_chains_draws_df <- function(x) {
  nchain <- nchains(x)
  k <- 1
  for (i in 1:nchain) {
    tmp <- x$.chain[x$.chain == i]
    niter <- length(tmp)
    if(niter > 1) {
      half <- niter / 2
      tmp[1:floor(half)] <- k
      k <- k + 1
      tmp[ceiling(half + 1):niter] <- k
      k <- k + 1
    }
    if (i == 1)
      chains <- tmp
    else
      chains <- c(chains, tmp)
  }
  x$.chain <- chains
  return(x)
}
