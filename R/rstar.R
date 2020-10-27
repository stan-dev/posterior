# Copyright (C) 2012, 2013, 2014, 2015, 2016, 2017, 2018 Trustees of Columbia University
# Copyright (C) 2018, 2019 Aki Vehtari, Paul Bürkner
# See LICENSE.md for more details

#' Calculate R star convergence diagnostic
#'
#' The `rstar` function generates a measure of convergence for MCMC draws based on
#' whether it is possible to determine the Markov chain that generated a draw with
#' probability greater than chance. To do so, it fits a machine learning classifier
#' to a training set of MCMC draws and evalutes its predictive accuracy on a testing
#' set: giving the ratio of accuracy to predicting a chain uniformly at random.
#'
#' @param x a `draws_df` object or one coercible to a `draws_df` object.
#'
#' @param split_chains a Boolean indicating whether to split chains into two equal
#' halves. Default is TRUE.
#'
#' @param uncertainty a Bolean indicating whether to provide a vector of R* values representing
#' uncertainty in the calculated value (if TRUE) or a single value (if FALSE). Default is TRUE.
#'
#' @param method machine learning classifer available in caret R package. Default is
#' \"rf\", calling the random forest classifier.
#'
#' @param hyperparameters hyperparameter settings for classifier given as a named list.
#' Default for rf classifier is `mtry=floor(sqt(nvariables))`; default for gradient-based
#' model (gbm in caret) is `interaction.depth=3, n.trees = 50, shrinkage=0.1, n.minobsinnode=10`.
#'
#' @param nsimulations number of R* values in returned vector if uncertainty is TRUE. Default is 1000.
#'
#' @param training_proportion proportion of iterations used to train classifier. Default is 0.7.
#'
#' @details `rstar` provides a measure of MCMC convergence based on whether it is
#' possible to determine the chain that generated a particular draw with a probability
#' greater than chance. To do so, it fits a machine learning classifier to a subset of
#' the original MCMC draws (the training set) and evaluates its predictive accuracy on
#' the remaining draws (the testing set). If predictive accuracy exceeds chance (i.e.
#' predicting the chain that generated a draw uniformly at random), the diagnostic
#' measure, R* > 1, indicating that convergence has yet to occur. This statistic is
#' recently developed, and it is currently unclear what is a reasonable threshold for
#' diagnosing convergence.
#'
#' The statistic, R*, is stochastic, meaning that each time the test is run, unless
#' the random seed is fixed, it will generally produce a different result. To minimise
#' the implications of this stochasticity, it is recommended to repeatedly run this
#' function to calculate a distribution of R*; alternatively, an approximation to this
#' distribution can be obtained by setting uncertainty = TRUE (note, that this measure
#' of uncertainty will generally have a lower mean).
#'
#' By default, a random forest classifier is used, which tends to perform best for
#' target distribution dimensionalities of around 4 and above. For lower dimensional
#' targets, gradient boosted models (called via `method=\"gbm\"`) tend to have a
#' higher classification accuracy. On a given MCMC sample, it is recommended to
#' try both of these classifiers.
#'
#' @return A single numeric R* value (by default) or a vector of values (if
#' uncertainty is TRUE).
#'
#' @references Ben Lambert, Aki Vehtari (2020) R*: A robust MCMC convergence
#' diagnostic with uncertainty using gradient-boosted machines
#' \emph{arXiv preprint} \code{arXiv:TBD}
#'
#' @examples
#' x <- example_draws("eight_schools")
#' rstar(x)
#' rstar(x, split_chains = F)
#' rstar(x, method = "gbm")
#' # can pass additional arguments to methods
#' rstar(x, method = "gbm", verbose = F)
#'
#' # with uncertainty, returns a vector of R* values
#' hist(rstar(x, uncertainty = T))
#' hist(rstar(x, uncertainty = T, nsimulations = 100))
#'
#' # can use other classification methods in caret library
#' rstar(x, method = "knn")
#'
#' @export
rstar <- function(x, split_chains=TRUE, uncertainty=FALSE, method=NULL, hyperparameters=NULL,
                  training_proportion=0.7, nsimulations=1000, ...){

  if(!requireNamespace("caret", quietly = T))
    stop("Package \"caret\" needed for this function to work. Please install it.")
  require(caret)

  nsimulations <- round(nsimulations)
  if(nsimulations < 1)
    stop("nsimulations must exceed 1.")

  if(training_proportion <= 0 || training_proportion >= 1)
    stop("training_proportion must be greater than zero and less than 1.")

  x <- as_draws_df(x)
  if(split_chains)
    x <- split_chains_draws_df(x)

  # if only 1 param, add in a column of random noise
  nparams <- nvariables(x)
  if(nparams==1)
    x$V_new <- rnorm(nrow(x))

  # create training / testing sets
  x$.chain <- as.factor(x$.chain)
  rand_samples <- sample(1:nrow(x), training_proportion * nrow(x))
  training_data <- x[rand_samples, ]
  testing_data <- x[-rand_samples, ]

  # choose hyperparameters
  if(is.null(method)) {
    method <- "rf"
    caret_grid <- data.frame(mtry=floor(sqrt(nparams)))
  } else if(is.null(hyperparameters) && method=="gbm") {
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
  fit <- train(.chain ~ .,
               data = training_data,
               method = method,
               trControl = trainControl(method = 'none'),
               tuneGrid = caret_grid,
               ...)

  # calculate classification accuracy then R*
  nchains <- length(unique(testing_data$.chain))
  if(uncertainty){
    probs <- predict(object=fit, newdata=testing_data, type = "prob")
    m_accuracy <- matrix(nrow = nrow(probs),
                         ncol = nsimulations)
    for(j in 1:seq_len(NROW(probs)) ){
      vals <- rmultinom(nsimulations, 1, prob = probs[j, ])
      test <- apply(vals, 2, function(x) which(x == 1))
      m_accuracy[j, ] <- ifelse(test == testing_data$.chain[j], 1, 0)
    }
    return(colMeans(m_accuracy) * nchains)
  } else{
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
  for(i in 1:nchain) {
    tmp <- x$.chain[x$.chain == i]
    niter <- length(tmp)
    if(niter > 1) {
      half <- niter / 2
      tmp[1:floor(half)] <- k
      k <- k + 1
      tmp[ceiling(half + 1):niter] <- k
      k <- k + 1
    }
    if(i == 1)
      chains <- tmp
    else
      chains <- c(chains, tmp)
  }
  x$.chain <- chains
  return(x)
}
