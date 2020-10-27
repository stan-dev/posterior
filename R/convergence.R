# Copyright (C) 2012, 2013, 2014, 2015, 2016, 2017, 2018 Trustees of Columbia University
# Copyright (C) 2018, 2019 Aki Vehtari, Paul Bürkner
# See LICENSE.md for more details

#' Basic version of the Rhat convergence diagnostic
#'
#' Compute the basic Rhat convergence diagnostic for a single variable as
#' described in Gelman et al. (2013). For practical applications, we strongly
#' recommend the improved Rhat convergence diagnostic implemented in
#' [rhat()].
#'
#' @family diagnostics
#' @template args-conv
#' @template args-conv-split
#' @template return-conv
#' @template ref-gelman-bda-2013
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' rhat_basic(mu)
#'
#' @export
rhat_basic <- function(x, split = TRUE) {
  split <- as_one_logical(split)
  if (split) {
    x <- split_chains(x)
  }
  .rhat(x)
}

#' Basic version of the effective sample size
#'
#' Compute the basic effective sample size (ESS) estimate for a single variable
#' as described in Gelman et al. (2013). For practical applications, we strongly
#' recommend the improved ESS convergence diagnostics implemented in
#' [ess_bulk()] and [ess_tail()].
#'
#' @family diagnostics
#' @template args-conv
#' @template args-conv-split
#' @template return-conv
#' @template ref-gelman-bda-2013
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' ess_basic(mu)
#'
#' @export
ess_basic <- function(x, split = TRUE) {
  split <- as_one_logical(split)
  if (split) {
    x <- split_chains(x)
  }
  .ess(x)
}

#' Rhat convergence diagnostic
#'
#' Compute Rhat convergence diagnostic as the maximum of rank normalized
#' split-Rhat and rank normalized folded-split-Rhat for a single variable
#' as proposed in Vehtari et al. (2019).
#'
#' @family diagnostics
#' @template args-conv
#' @template return-conv
#' @template ref-vehtari-rhat-2019
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' rhat(mu)
#'
#' @export
rhat <- function(x) {
  rhat_bulk <- .rhat(z_scale(split_chains(x)))
  rhat_tail <- .rhat(z_scale(split_chains(fold_draws(x))))
  max(rhat_bulk, rhat_tail)
}

#' Bulk effective sample size (bulk-ESS)
#'
#' Compute bulk effective sample size estimate (bulk-ESS) for a single variable.
#' Bulk-ESS is useful as a generic diagnostic for the sampling
#' efficiency in the bulk of the posterior. It is defined as the
#' effective sample size for rank normalized values using split chains.
#'
#' @family diagnostics
#' @template args-conv
#' @template return-conv
#' @template ref-vehtari-rhat-2019
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' ess_bulk(mu)
#'
#' @export
ess_bulk <- function(x) {
  .ess(z_scale(split_chains(x)))
}

#' Tail effective sample size (tail-ESS)
#'
#' Compute tail effective sample size estimate (tail-ESS) for a single variable.
#' Tail-ESS is useful for generic diagnostic for the sampling
#' efficiency in the tails of the posterior. It is defined as
#' the minimum of the effective sample sizes for 5% and 95% quantiles.
#'
#' @family diagnostics
#' @template args-conv
#' @template return-conv
#' @template ref-vehtari-rhat-2019
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' ess_tail(mu)
#'
#' @export
ess_tail <- function(x) {
  q05_ess <- ess_quantile(x, 0.05)
  q95_ess <- ess_quantile(x, 0.95)
  min(q05_ess, q95_ess)
}

#' Effective sample sizes for quantiles
#'
#' Compute effective sample size estimates for quantile estimates of
#' a single variable.
#'
#' @family diagnostics
#' @template args-conv
#' @template args-conv-quantile
#' @template return-conv-quantile
#' @template ref-vehtari-rhat-2019
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' ess_quantile(mu, probs = c(0.1, 0.9))
#'
#' @export
ess_quantile <- function(x, probs = c(0.05, 0.95), names = TRUE) {
  probs <- as.numeric(probs)
  if (any(probs < 0 | probs > 1)) {
    stop2("'probs' must contain values between 0 and 1.")
  }
  names <- as_one_logical(names)
  out <- ulapply(probs, .ess_quantile, x = x)
  if (names) {
    names(out) <- paste0("ess_q", probs * 100)
  }
  out
}

#' @rdname ess_quantile
#' @export
ess_median <- function(x) {
  .ess_quantile(x, prob = 0.5)
}

# ESS of a single quantile
.ess_quantile <- function(x, prob) {
  if (should_return_NA(x)) {
    return(NA_real_)
  }
  x <- as.matrix(x)
  I <- x <= quantile(x, prob)
  .ess(split_chains(I))
}

#' Effective sample size for the mean
#'
#' Compute effective sample size estimate for a mean (expectation)
#' estimate of a single variable.
#'
#' @template args-conv
#' @template return-conv
#' @template ref-gelman-bda-2013
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' ess_mean(mu)
#'
#' @export
ess_mean <- function(x) {
  .ess(split_chains(x))
}

#' Effective sample size for the standard deviation
#'
#' Compute an effective sample size estimate for the standard deviation (SD)
#' estimate of a single variable. This is defined as minimum of effective
#' sample size estimate for mean and mean of squared value.
#'
#' @family diagnostics
#' @template args-conv
#' @template return-conv
#' @template ref-vehtari-rhat-2019
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' ess_sd(mu)
#'
#' @export
ess_sd <- function(x) {
  min(.ess(split_chains(x)), .ess(split_chains(x^2)))
}

#' Monte Carlo standard error for quantiles
#'
#' Compute Monte Carlo standard errors for quantile estimates of a
#' single variable.
#'
#' @family diagnostics
#' @template args-conv
#' @template args-conv-quantile
#' @template return-conv-quantile
#' @template ref-vehtari-rhat-2019
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' mcse_quantile(mu, probs = c(0.1, 0.9))
#'
#' @export
mcse_quantile <- function(x, probs = c(0.05, 0.95), names = TRUE) {
  probs <- as.numeric(probs)
  if (any(probs < 0 | probs > 1)) {
    stop2("'probs' must contain values between 0 and 1.")
  }
  names <- as_one_logical(names)
  out <- ulapply(probs, .mcse_quantile, x = x)
  if (names) {
    names(out) <- paste0("mcse_q", probs * 100)
  }
  out
}

#' @rdname mcse_quantile
#' @export
mcse_median <- function(x) {
  .mcse_quantile(x, prob = 0.5)
}

# MCSE of a single quantile
.mcse_quantile <- function(x, prob) {
  ess <- ess_quantile(x, prob)
  p <- c(0.1586553, 0.8413447)
  a <- qbeta(p, ess * prob + 1, ess * (1 - prob) + 1)
  ssims <- sort(x)
  S <- length(ssims)
  th1 <- ssims[max(floor(a[1] * S), 1)]
  th2 <- ssims[min(ceiling(a[2] * S), S)]
  as.vector((th2 - th1) / 2)
}

#' Monte Carlo standard error for the mean
#'
#' Compute Monte Carlo standard error for mean (expectation) of a
#' single variable.
#'
#' @family diagnostics
#' @template args-conv
#' @template return-conv
#' @template ref-gelman-bda-2013
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' mcse_mean(mu)
#'
#' @export
mcse_mean <- function(x) {
  sd(x) / sqrt(ess_mean(x))
}

#' Monte Carlo standard error for the standard deviation
#'
#' Compute Monte Carlo standard error for standard deviation (SD) of a
#' single variable using Stirling's approximation and assuming
#' approximate normality.
#'
#' @family diagnostics
#' @template args-conv
#' @template return-conv
#' @template ref-vehtari-rhat-2019
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' mcse_sd(mu)
#'
#' @export
mcse_sd <- function(x) {
  # assumes normality of x and uses Stirling's approximation
  ess_sd <- ess_sd(x)
  sd(x) * sqrt(exp(1) * (1 - 1 / ess_sd)^(ess_sd - 1) - 1)
}

#' Compute Quantiles
#'
#' Compute quantiles of a sample and return them in a format consistent
#' with other summary functions of the \pkg{posterior} package.
#'
#' @template args-conv
#' @template args-conv-quantile
#' @param ... Further arguments passed to [quantile()].
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' quantile2(mu)
#'
#' @export
quantile2 <- function(x, probs = c(0.05, 0.95), names = TRUE, ...) {
  names <- as_one_logical(names)
  out <- quantile(x, probs = probs, ...)
  if (names) {
    names(out) <- paste0("q", probs * 100)
  } else {
    names(out) <- NULL
  }
  out
}

#' Calculate R star convergence diagnostic
#'
#' The `rstar` function generates a measure of convergence for MCMC based on
#' whether it is possible to determine the Markov chain that generated a draw with
#' probability greater than chance. To do so, it fits a machine learning classifier
#' to a training set of MCMC draws and evalutes its predictive accuracy on a testing
#' set.
#'
#' @param x a `draws_df` object or one coercible to a `draws_df` object.
#'
#' @param split_chains a Boolean indicating whether to split
#' chains into two equal halves.
#'
#' @param uncertainty whether to provide a list of R* values (if true) or a single value (if false).
#'
#' @param method machine learning classifer available in Caret R package.
#'
#' @param hyperparameters hyperparameter settings for ML classifier given as a list.
#'
#' @param nsim number of R* values in returned vector if uncertainty=T.
#'
#' @param training_proportion proportion of iterations used to train GBM model.
#'
#' @details `rstar` provides a measure of MCMC convergence based on whether it is possible
#' to determine the chain that generated a particular draw with a probability greater than
#' chance. To do so, it fits a machine learning classifier to a subset of the original
#' MCMC draws (the training set) and evaluates its predictive accuracy on the remaining
#' draws (the testing set). If predictive accuracy exceeds chance (i.e. predicting a
#' the chain that generated a draw uniformly at random), the diagnostic measure, R* > 1,
#' indicating that convergence has yet to occur. The statistic, R*, is stochastic,
#' meaning that each time the test is run, unless the random seed is fixed, it will
#' generally produce a different result. To minimise the implications of this
#' stochasticity, it is recommended to repeatedly run this function to calculate a
#' distribution of R*; alternatively, an approximation to this distribution can be
#' obtained by setting uncertainty = T.
#'
#' @return A single numeric R* value by default or a vector of values if uncertainty=T.
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
#' rstar(x, method = "gbm", verbose = F)
#'
#' # with uncertainty, returns a vector of R* values.
#' hist(rstar(x, uncertainty = T))
#' hist(rstar(x, uncertainty = T, nsim = 100))
#'
#' # can use other classification methods in Caret library
#' rstar(x, method = "knn")
#'
#' @export
rstar <- function(x, split_chains=T, uncertainty=F, method=NULL, hyperparameters=NULL,
                            training_proportion=0.7, nsim=1000, ...){

  nsim <- round(nsim)
  if(nsim < 1)
    stop("nsim must exceed 1.")

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
    caret_grid <- tibble(mtry=floor(sqrt(nparams)))
  } else if(is.null(hyperparameters) && method=="gbm") {
    caret_grid <- tibble(interaction.depth=c(3),
                         n.trees = 50,
                         shrinkage=c(0.1),
                         n.minobsinnode=10)
  } else {
    caret_grid <- hyperparameters
  }

  # remove iteration / draws columns and fit classifier
  training_data <- training_data[, c(variables(training_data), ".chain")]
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
                         ncol = nsim)
    for(j in 1:nrow(probs)){
      vals <- rmultinom(nsim, 1, prob = probs[j, ])
      test <- apply(vals, 2, function(x) which(x==1))
      m_accuracy[j, ] <- ifelse(test == testing_data$.chain[j], 1, 0)
    }
    return(colMeans(m_accuracy) * nchains)
  } else{
    plda <- predict(object=fit, newdata=testing_data)
    res <- tibble(predicted=plda, actual=testing_data$.chain)
    accuracy <- mean(res$predicted == res$actual)
    return(accuracy * nchains)
  }
}

# internal ----------------------------------------------------------------

#' Find the optimal next size for the FFT so that a minimum number of zeros
#' are padded.
#' @param N length of the sequence over which to apply FFT
#' @return the optimal next step size as a single integer
#' @noRd
fft_next_good_size <- function(N) {
  if (N <= 2)
    return(2)
  while (TRUE) {
    m <- N
    while ((m %% 2) == 0) m <- m / 2
    while ((m %% 3) == 0) m <- m / 3
    while ((m %% 5) == 0) m <- m / 5
    if (m <= 1)
      return(N)
    N <- N + 1
  }
}

#' Autocovariance estimates
#'
#' Compute autocovariance estimates for every lag for the specified
#' input sequence using a fast Fourier transform approach. The estimate
#' for lag t is scaled by N-t where N is the length of the sequence.
#'
#' @template args-conv-seq
#' @return A numeric vector of autocovariances at every lag (scaled by N-lag).
#' @keywords internal
#' @export
autocovariance <- function(x) {
  N <- length(x)
  M <- fft_next_good_size(N)
  Mt2 <- 2 * M
  yc <- x - mean(x)
  yc <- c(yc, rep.int(0, Mt2 - N))
  transform <- fft(yc)
  ac <- fft(Conj(transform) * transform, inverse = TRUE)
  # use "biased" estimate as recommended by Geyer (1992)
  ac <- Re(ac)[1:N] / (N^2 * 2)
  ac
}

#' Autocorrelation estimates
#'
#' Compute autocorrelation estimates for every lag for the specified
#' input sequence using a fast Fourier transform approach. The estimate
#' for lag t is scaled by N-t where N is the length of the sequence.
#'
#' @template args-conv-seq
#' @return A numeric vector of autocorrelations at every lag (scaled by N-lag).
#' @keywords internal
#' @export
autocorrelation <- function(x) {
  ac <- autocovariance(x)
  ac <- ac / ac[1]
}

#' Rank normalization
#'
#' Compute rank normalization for a numeric array. First replace each
#' value by its rank. Average rank for ties are used to conserve the
#' number of unique values of discrete quantities. Second, normalize
#' ranks via the inverse normal transformation.
#'
#' @template args-scale
#' @return A numeric array of rank normalized values with the same size
#'   and dimension as the input.
#' @keywords internal
#' @export
z_scale <- function(x) {
  r <- rank(as.array(x), ties.method = 'average')
  z <- qnorm(backtransform_ranks(r))
  z[is.na(x)] <- NA
  if (!is.null(dim(x))) {
    # output should have the input dimension
    z <- array(z, dim = dim(x), dimnames = dimnames(x))
  }
  z
}

#' Rank uniformization
#'
#' Compute rank uniformization for a numeric array. First replace each
#' value by its rank. Average rank for ties are used to conserve the
#' number of unique values of discrete quantities. Second, uniformize
#' ranks to scale `[1/(2S), 1-1/(2S)]`, where `S` is the the number
#' of values.
#'
#' @template args-scale
#' @return A numeric array of uniformized values with the same size
#'   and dimension as the input.
#' @keywords internal
#' @export
u_scale <- function(x) {
  r <- rank(as.array(x), ties.method = 'average')
  u <- backtransform_ranks(r)
  u[is.na(x)] <- NA
  if (!is.null(dim(x))) {
    # output should have the input dimension
    u <- array(u, dim = dim(x), dimnames = dimnames(x))
  }
  u
}

#' Rank values
#'
#' Compute ranks for a numeric array. First replace each
#' value by its rank. Average rank for ties are used to conserve the
#' number of unique values of discrete quantities. Second, normalize
#' ranks via the inverse normal transformation.
#'
#' @template args-scale
#' @return A numeric array of ranked values with the same size
#'   and dimension as the input.
#' @keywords internal
#' @export
r_scale <- function(x) {
  r <- rank(as.array(x), ties.method = 'average')
  r[is.na(x)] <- NA
  if (!is.null(dim(x))) {
    # output should have the input dimension
    r <- array(r, dim = dim(x), dimnames = dimnames(x))
  }
  r
}

#' Backtransformation of ranks
#'
#' @param r array of ranks
#' @param c fractional offset; defaults to c = 3/8 as recommend by Blom (1958)
#' @noRd
backtransform_ranks <- function(r, c = 3/8) {
  S <- length(r)
  (r - c) / (S - 2 * c + 1)
}

#' Split Markov chains in half
#' @template args-conv
#' @return A 2D array of draws with split chains.
#' @keywords internal
#' @export
split_chains <- function(x) {
  x <- as.matrix(x)
  niter <- NROW(x)
  if (niter == 1L) {
    return(x)
  }
  half <- niter / 2
  cbind(x[1:floor(half), ], x[ceiling(half + 1):niter, ])
}

#' Fold draws around their median
#' @template args-conv
#' @return An array or vector of folded draws.
#' @keywords internal
#' @export
fold_draws <- function(x) {
  abs(x - median(x))
}

#' Compute the Rhat convergence diagnostic
#' @template args-conv
#' @template return-conv
#' @keywords internal
#' @export
.rhat <- function(x) {
  x <- as.matrix(x)
  if (should_return_NA(x)) {
    return(NA_real_)
  }
  nchains <- NCOL(x)
  niterations <- NROW(x)
  chain_mean <- numeric(nchains)
  chain_var <- numeric(nchains)
  for (i in seq_len(nchains)) {
    chain_mean[i] <- mean(x[, i])
    chain_var[i] <- var(x[, i])
  }
  var_between <- niterations * var(chain_mean)
  var_within <- mean(chain_var)
  sqrt((var_between / var_within + niterations - 1) / niterations)
}

#' Compute the effective sample size
#' @template args-conv
#' @template return-conv
#' @keywords internal
#' @export
.ess <- function(x) {
  x <- as.matrix(x)
  nchains <- NCOL(x)
  niterations <- NROW(x)
  if (niterations < 3L || should_return_NA(x)) {
    return(NA_real_)
  }
  acov_fun <- function(i) autocovariance(x[, i])
  acov <- lapply(seq_len(nchains), acov_fun)
  acov <- do.call(cbind, acov)
  chain_mean <- apply(x, 2, mean)
  mean_var <- mean(acov[1, ]) * niterations / (niterations - 1)
  var_plus <- mean_var * (niterations - 1) / niterations
  if (nchains > 1) {
    var_plus <- var_plus + var(chain_mean)
  }

  # Geyer's initial positive sequence
  rho_hat_t <- rep.int(0, niterations)
  t <- 0
  rho_hat_even <- 1
  rho_hat_t[t + 1] <- rho_hat_even
  rho_hat_odd <- 1 - (mean_var - mean(acov[t + 2, ])) / var_plus
  rho_hat_t[t + 2] <- rho_hat_odd
  while (t < NROW(acov) - 5 && !is.nan(rho_hat_even + rho_hat_odd) &&
         (rho_hat_even + rho_hat_odd > 0)) {
    t <- t + 2
    rho_hat_even = 1 - (mean_var - mean(acov[t + 1, ])) / var_plus
    rho_hat_odd = 1 - (mean_var - mean(acov[t + 2, ])) / var_plus
    if ((rho_hat_even + rho_hat_odd) >= 0) {
      rho_hat_t[t + 1] <- rho_hat_even
      rho_hat_t[t + 2] <- rho_hat_odd
    }
  }
  max_t <- t
  # this is used in the improved estimate
  if (rho_hat_even>0)
    rho_hat_t[max_t + 1] <- rho_hat_even

  # Geyer's initial monotone sequence
  t <- 0
  while (t <= max_t - 4) {
    t <- t + 2
    if (rho_hat_t[t + 1] + rho_hat_t[t + 2] >
        rho_hat_t[t - 1] + rho_hat_t[t]) {
      rho_hat_t[t + 1] = (rho_hat_t[t - 1] + rho_hat_t[t]) / 2;
      rho_hat_t[t + 2] = rho_hat_t[t + 1];
    }
  }
  ess <- nchains * niterations
  # Geyer's truncated estimate
  # tau_hat <- -1 + 2 * sum(rho_hat_t[1:max_t])
  # Improved estimate reduces variance in antithetic case
  tau_hat <- -1 + 2 * sum(rho_hat_t[1:max_t]) + rho_hat_t[max_t+1]
  # Safety check for negative values and with max ess equal to ess*log10(ess)
  tau_hat <- max(tau_hat, 1/log10(ess))
  ess <- ess / tau_hat
  ess
}

# should NA be returned by a convergence diagnostic?
should_return_NA <- function(x) {
  anyNA(x) || checkmate::anyInfinite(x) || is_constant(x)
}

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
