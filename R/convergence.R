# Copyright (C) 2012, 2013, 2014, 2015, 2016, 2017, 2018 Trustees of Columbia University
# Copyright (C) 2018, 2019 Aki Vehtari, Paul Bürkner
# See LICENSE.md for more details

#' List of available convergence diagnostics
#'
#' A list of available diagnostics and links to their individual help pages.
#'
#' @name diagnostics
#' @aliases convergence
#'
#' @details
#'
#' |**Function**|**Description**|
#' |:----------|:---------------|
#' | [ess_basic()] | Basic version of effective sample size |
#' | [ess_bulk()] | Bulk effective sample size |
#' | [ess_tail()] | Tail effective sample size |
#' | [ess_quantile()] | Effective sample sizes for quantiles |
#' | [ess_sd()] | Effective sample sizes for standard deviations |
#' | [mcse_mean()] | Monte Carlo standard error for the mean |
#' | [mcse_quantile()] | Monte Carlo standard error for quantiles |
#' | [mcse_sd()] | Monte Carlo standard error for standard deviations |
#' | [pareto_khat()] | Pareto khat diagnostic for tail(s) |
#' | [pareto_diags()] | Additional diagnostics related to Pareto khat |
#' | [rhat_basic()] | Basic version of Rhat |
#' | [rhat()] | Improved, rank-based version of Rhat |
#' | [rhat_nested()] | Rhat for use with many short chains |
#' | [rstar()] | R* diagnostic |
#'
#' @return
#' See individual functions for a description of return types.
#'
NULL

#' Basic version of the Rhat convergence diagnostic
#'
#' Compute the basic Rhat convergence diagnostic for a single variable as
#' described in Gelman et al. (2013) with some changes according to Vehtari et
#' al. (2021). For practical applications, we strongly recommend the improved
#' Rhat convergence diagnostic implemented in [rhat()].
#'
#' @family diagnostics
#' @template args-conv
#' @template args-conv-split
#' @template args-methods-dots
#' @template return-conv
#' @template ref-gelman-bda-2013
#' @template ref-vehtari-rhat-2021
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' rhat_basic(mu)
#'
#' d <- as_draws_rvars(example_draws("multi_normal"))
#' rhat_basic(d$Sigma)
#'
#' @export
rhat_basic <- function(x, ...) UseMethod("rhat_basic")

#' @rdname rhat_basic
#' @export
rhat_basic.default <- function(x, split = TRUE, ...) {
  split <- as_one_logical(split)
  if (split) {
    x <- .split_chains(x)
  }
  .rhat(x)
}

#' @rdname rhat_basic
#' @export
rhat_basic.rvar <- function(x, split = TRUE, ...) {
  summarise_rvar_by_element_with_chains(x, rhat_basic, split, ...)
}

#' Basic version of the effective sample size
#'
#' Compute the basic effective sample size (ESS) estimate for a single variable
#' as described in Gelman et al. (2013) with some changes according to Vehtari et
#' al. (2021). For practical applications, we strongly
#' recommend the improved ESS convergence diagnostics implemented in
#' [ess_bulk()] and [ess_tail()]. See Vehtari (2021) for an in-depth
#' comparison of different effective sample size estimators.
#'
#' @family diagnostics
#' @template args-conv
#' @template args-conv-split
#' @template args-methods-dots
#' @template return-conv
#' @template ref-gelman-bda-2013
#' @template ref-vehtari-rhat-2021
#' @template ref-vehtari-ess-2021
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' ess_basic(mu)
#'
#' d <- as_draws_rvars(example_draws("multi_normal"))
#' ess_basic(d$Sigma)
#'
#' @export
ess_basic <- function(x, ...) UseMethod("ess_basic")

#' @rdname ess_basic
#' @export
ess_basic.default <- function(x, split = TRUE, ...) {
  split <- as_one_logical(split)
  if (split) {
    x <- .split_chains(x)
  }
  .ess(x)
}

#' @rdname ess_basic
#' @export
ess_basic.rvar <- function(x, split = TRUE, ...) {
  summarise_rvar_by_element_with_chains(x, ess_basic, split, ...)
}

#' Rhat convergence diagnostic
#'
#' Compute the Rhat convergence diagnostic for a single variable as the maximum
#' of rank normalized split-Rhat and rank normalized folded-split-Rhat as
#' proposed in Vehtari et al. (2021).
#'
#' @family diagnostics
#' @template args-conv
#' @template args-methods-dots
#' @template return-conv
#' @template ref-vehtari-rhat-2021
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' rhat(mu)
#'
#' d <- as_draws_rvars(example_draws("multi_normal"))
#' rhat(d$Sigma)
#'
#' @export
rhat <- function(x, ...) UseMethod("rhat")

#' @rdname rhat
#' @export
rhat.default <- function(x, ...) {
  rhat_bulk <- .rhat(z_scale(.split_chains(x)))
  rhat_tail <- .rhat(z_scale(.split_chains(fold_draws(x))))
  max(rhat_bulk, rhat_tail)
}

#' @rdname rhat
#' @export
rhat.rvar <- function(x, ...) {
  summarise_rvar_by_element_with_chains(x, rhat, ...)
}

#' Bulk effective sample size (bulk-ESS)
#'
#' Compute a bulk effective sample size estimate (bulk-ESS) for a single
#' variable. Bulk-ESS is useful as a diagnostic for the sampling efficiency in
#' the bulk of the posterior. It is defined as the effective sample size for
#' rank normalized values using split chains. For the tail effective sample size
#' see [ess_tail()]. See Vehtari (2021) for an in-depth
#' comparison of different effective sample size estimators.
#'
#' @family diagnostics
#' @template args-conv
#' @template args-methods-dots
#' @template return-conv
#' @template ref-vehtari-rhat-2021
#' @template ref-vehtari-ess-2021
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' ess_bulk(mu)
#'
#' d <- as_draws_rvars(example_draws("multi_normal"))
#' ess_bulk(d$Sigma)
#'
#' @export
ess_bulk <- function(x, ...) UseMethod("ess_bulk")

#' @rdname ess_bulk
#' @export
ess_bulk.default <- function(x, ...) {
  .ess(z_scale(.split_chains(x)))
}

#' @rdname ess_bulk
#' @export
ess_bulk.rvar <- function(x, ...) {
  summarise_rvar_by_element_with_chains(x, ess_bulk, ...)
}

#' Tail effective sample size (tail-ESS)
#'
#' Compute a tail effective sample size estimate (tail-ESS) for a single
#' variable. Tail-ESS is useful as a diagnostic for the sampling efficiency in
#' the tails of the posterior. It is defined as the minimum of the effective
#' sample sizes for 5% and 95% quantiles. For the bulk effective sample
#' size see [ess_bulk()]. See Vehtari (2021) for an in-depth
#' comparison of different effective sample size estimators.
#'
#' @family diagnostics
#' @template args-conv
#' @template args-methods-dots
#' @template return-conv
#' @template ref-vehtari-rhat-2021
#' @template ref-vehtari-ess-2021
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' ess_tail(mu)
#'
#' d <- as_draws_rvars(example_draws("multi_normal"))
#' ess_tail(d$Sigma)
#'
#' @export
ess_tail <- function(x, ...) UseMethod("ess_tail")

#' @rdname ess_tail
#' @export
ess_tail.default <- function(x, ...) {
  q05_ess <- ess_quantile(x, 0.05)
  q95_ess <- ess_quantile(x, 0.95)
  min(q05_ess, q95_ess)
}

#' @rdname ess_tail
#' @export
ess_tail.rvar <- function(x, ...) {
  summarise_rvar_by_element_with_chains(x, ess_tail, ...)
}

#' Effective sample sizes for quantiles
#'
#' Compute effective sample size estimates for quantile estimates of a single
#' variable.
#'
#' @family diagnostics
#' @template args-conv
#' @template args-conv-quantile
#' @template args-methods-dots
#' @template return-conv-quantile
#' @template ref-vehtari-rhat-2021
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' ess_quantile(mu, probs = c(0.1, 0.9))
#'
#' d <- as_draws_rvars(example_draws("multi_normal"))
#' ess_quantile(d$mu, probs = c(0.1, 0.9))
#'
#' @export
ess_quantile <- function(x, probs = c(0.05, 0.95), ...) {
  UseMethod("ess_quantile")
}

#' @rdname ess_quantile
#' @export
ess_quantile.default <- function(x, probs = c(0.05, 0.95), names = TRUE, ...) {
  probs <- as.numeric(probs)
  if (any(probs < 0 | probs > 1)) {
    stop_no_call("'probs' must contain values between 0 and 1.")
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
ess_quantile.rvar <- function(x, probs = c(0.05, 0.95), names = TRUE, ...) {
  summarise_rvar_by_element_with_chains(x, ess_quantile, probs, names, ...)
}

#' @rdname ess_quantile
#' @export
ess_median <- function(x, ...) {
  ess_quantile(x, probs = 0.5, names = FALSE, ...)
}

# ESS of a single quantile
.ess_quantile <- function(x, prob) {
  if (should_return_NA(x)) {
    return(NA_real_)
  }
  x <- as.matrix(x)
  if (prob == 1) {
    len <- length(x)
    prob <- (len - 0.5) / len
  }
  I <- x <= quantile(x, prob)
  .ess(.split_chains(I))
}

#' Effective sample size for the mean
#'
#' Compute an effective sample size estimate for a mean (expectation)
#' estimate of a single variable.
#'
#' @template args-conv
#' @template args-methods-dots
#' @template return-conv
#' @template ref-gelman-bda-2013
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' ess_mean(mu)
#'
#' d <- as_draws_rvars(example_draws("multi_normal"))
#' ess_mean(d$Sigma)
#'
#' @export
ess_mean <- function(x, ...) UseMethod("ess_mean")

#' @rdname ess_quantile
#' @export
ess_mean.default <- function(x, ...) {
  .ess(.split_chains(x))
}

#' @rdname ess_mean
#' @export
ess_mean.rvar <- function(x, ...) {
  summarise_rvar_by_element_with_chains(x, ess_mean, ...)
}

#' Effective sample size for the standard deviation
#'
#' Compute an effective sample size estimate for the standard deviation (SD)
#' estimate of a single variable. This is defined as the effective sample size
#' estimate for the absolute deviation from mean.
#'
#' @family diagnostics
#' @template args-conv
#' @template args-methods-dots
#' @template return-conv
#' @template ref-vehtari-rhat-2021
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' ess_sd(mu)
#'
#' d <- as_draws_rvars(example_draws("multi_normal"))
#' ess_sd(d$Sigma)
#'
#' @export
ess_sd <- function(x, ...) UseMethod("ess_sd")

#' @rdname ess_sd
#' @export
ess_sd.default <- function(x, ...) {
  .ess(.split_chains(abs(x-mean(x))))
}

#' @rdname ess_sd
#' @export
ess_sd.rvar <- function(x, ...) {
  summarise_rvar_by_element_with_chains(x, ess_sd, ...)
}

#' Monte Carlo standard error for quantiles
#'
#' Compute Monte Carlo standard errors for quantile estimates of a
#' single variable.
#'
#' @family diagnostics
#' @template args-conv
#' @template args-conv-quantile
#' @template args-methods-dots
#' @template return-conv-quantile
#' @template ref-vehtari-rhat-2021
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' mcse_quantile(mu, probs = c(0.1, 0.9))
#'
#' d <- as_draws_rvars(example_draws("multi_normal"))
#' mcse_quantile(d$mu)
#'
#' @export
mcse_quantile <- function(x, probs = c(0.05, 0.95), ...) {
  UseMethod("mcse_quantile")
}

#' @rdname mcse_quantile
#' @export
mcse_quantile.default <- function(x, probs = c(0.05, 0.95), names = TRUE, ...) {
  probs <- as.numeric(probs)
  if (any(probs < 0 | probs > 1)) {
    stop_no_call("'probs' must contain values between 0 and 1.")
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
mcse_quantile.rvar <- function(x, probs = c(0.05, 0.95), names = TRUE, ...) {
  summarise_rvar_by_element_with_chains(x, mcse_quantile, probs, names, ...)
}

#' @rdname mcse_quantile
#' @export
mcse_median <- function(x, ...) {
  mcse_quantile(x, probs = 0.5, names = FALSE, ...)
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
#' Compute the Monte Carlo standard error for the mean (expectation) of a
#' single variable.
#'
#' @family diagnostics
#' @template args-conv
#' @template args-methods-dots
#' @template return-conv
#' @template ref-gelman-bda-2013
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' mcse_mean(mu)
#'
#' d <- as_draws_rvars(example_draws("multi_normal"))
#' mcse_mean(d$Sigma)
#'
#' @export
mcse_mean <- function(x, ...) UseMethod("mcse_mean")

#' @rdname mcse_mean
#' @export
mcse_mean.default <- function(x, ...) {
  sd(x) / sqrt(ess_mean(x))
}

#' @rdname mcse_mean
#' @export
mcse_mean.rvar <- function(x, ...) {
  summarise_rvar_by_element_with_chains(x, mcse_mean, ...)
}

#' Monte Carlo standard error for the standard deviation
#'
#' Compute the Monte Carlo standard error for the standard deviation (SD) of a
#' single variable without assuming normality using moments of moments and
#' first order Taylor series approximation (Kenney and Keeping, 1951, p. 141).
#'
#' @family diagnostics
#' @template args-conv
#' @template args-methods-dots
#' @template return-conv
#' @template ref-vehtari-rhat-2021
#' @template ref-kenney-stats-1951
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' mcse_sd(mu)
#'
#' d <- as_draws_rvars(example_draws("multi_normal"))
#' mcse_sd(d$Sigma)
#'
#' @export
mcse_sd <- function(x, ...) UseMethod("mcse_sd")

#' @rdname mcse_sd
#' @export
mcse_sd.default <- function(x, ...) {
  # var/sd are not a simple expectation of g(X), e.g. variance
  # has (X-E[X])^2. The following ESS is based on a relevant quantity
  # in the computation and is empirically a good choice.
  sims_c <- x - mean(x)
  ess <- ess_mean((sims_c)^2)
  # Variance of variance estimate by Kenney and Keeping (1951, p. 141),
  # which doesn't assume normality of sims.
  Evar <- mean(sims_c^2)
  varvar <- (mean(sims_c^4) - Evar^2) / ess
  # The first order Taylor series approximation of variance of sd.
  # Kenney and Keeping (1951, p. 141) write "...since fluctuations of
  # any moment are of order N^{-1/2}, squares and higher powers of
  # differentials of the moments can be neglected "
  varsd <- varvar / Evar / 4
  sqrt(varsd)
}

#' @rdname mcse_sd
#' @export
mcse_sd.rvar <- function(x, ...) {
  summarise_rvar_by_element_with_chains(x, mcse_sd, ...)
}

#' Compute Quantiles
#'
#' Compute quantiles of a sample and return them in a format consistent
#' with other summary functions in the \pkg{posterior} package.
#'
#' @template args-conv
#' @template args-conv-quantile
#' @param na.rm (logical) Should `NA` and `NaN` values be removed from `x` prior
#'   to computing quantiles? The default is `FALSE`.
#' @param ... Arguments passed to individual methods (if applicable) and then
#'   on to [stats::quantile()].
#'
#' @return
#' A numeric vector of length `length(probs)`. If `names = TRUE`, it has a
#' [names] attribute with names like `"q5"`, `"q95"`, etc, based on the values
#' of `probs`.
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' quantile2(mu)
#'
#' @export
quantile2 <- function(x, probs = c(0.05, 0.95), na.rm = FALSE, ...) {
  UseMethod("quantile2")
}

#' @rdname quantile2
#' @export
quantile2.default <- function(
  x, probs = c(0.05, 0.95), na.rm = FALSE, names = TRUE, ...
) {
  names <- as_one_logical(names)
  na.rm <- as_one_logical(na.rm)
  if (!na.rm && anyNA(x)) {
    # quantile itself doesn't handle this case (#110)
    out <- rep(NA_real_, length(probs))
  } else {
    out <- quantile(x, probs = probs, na.rm = na.rm, ...)
  }
  if (names) {
    names(out) <- paste0("q", probs * 100)
  } else {
    names(out) <- NULL
  }
  out
}

#' @rdname quantile2
#' @export
quantile2.rvar <- function(
  x, probs = c(0.05, 0.95), na.rm = FALSE, names = TRUE, ...
) {
  summarise_rvar_by_element_with_chains(x, quantile2, probs, na.rm, names, ...)
}

# internal ----------------------------------------------------------------

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
  varx <- var(x)
  if (varx==0) {
    # if variance is 0, then all autocovariances are 0
    return(rep(0, N))
  }
  # zero padding makes fft() much faster when N > 1000
  M <- nextn(N)
  Mt2 <- 2 * M
  yc <- x - mean(x)
  yc <- c(yc, rep.int(0, Mt2 - N))
  # FFT based unnormalized autocovariances
  ac <- Re(fft(abs(fft(yc))^2, inverse = TRUE)[1:N])
  # use "biased" estimate as recommended by Geyer (1992)
  # direct scaling with var(x) avoids need to compute "mask effect"
  ac <- ac / ac[1] * varx * (N - 1) / N
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
#' @template args-frac-offset
#' @return A numeric array of rank normalized values with the same size
#'   and dimension as the input.
#' @keywords internal
#' @export
z_scale <- function(x, c = 3/8) {
  r <- rank(as.array(x), ties.method = 'average')
  z <- qnorm(backtransform_ranks(r, c = c))
  z[is.na(x)] <- NA
  if (!is.null(dim(x))) {
    # output should have the input dimension
    z <- array(z, dim = dim(x), dimnames = dimnames(x))
  }
  z
}

#' Rank uniformization
#'
#' Compute rank uniformization for a numeric array. First replace each value by
#' its rank. Average rank for ties are used to conserve the number of unique
#' values of discrete quantities. Second, uniformize ranks to the scale
#' `[1/(2S), 1-1/(2S)]`, where `S` is the number of values.
#'
#' @template args-scale
#' @template args-frac-offset
#' @return A numeric array of uniformized values with the same size
#'   and dimension as the input.
#' @keywords internal
#' @export
u_scale <- function(x, c = 3/8) {
  r <- rank(as.array(x), ties.method = 'average')
  u <- backtransform_ranks(r, c = c)
  u[is.na(x)] <- NA
  if (!is.null(dim(x))) {
    # output should have the input dimension
    u <- array(u, dim = dim(x), dimnames = dimnames(x))
  }
  u
}

#' Rank values
#'
#' Compute ranks for a numeric array, that is, replace each
#' value by its rank. Average rank for ties are used to conserve the
#' number of unique values of discrete quantities.
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

#' Back-transformation of ranks
#'
#' @param r array of ranks
#' @param c fractional offset; defaults to c = 3/8 as recommend by Blom (1958)
#' @noRd
backtransform_ranks <- function(r, c = 3/8) {
  c <- as_one_numeric(c)
  S <- length(r)
  (r - c) / (S - 2 * c + 1)
}

#' Fold draws around their median
#' @template args-conv
#' @return An array or vector of folded draws.
#' @noRd
fold_draws <- function(x) {
  abs(x - median(x))
}

#' Compute the Rhat convergence diagnostic
#' @template args-conv
#' @template return-conv
#' @noRd
.rhat <- function(x) {
  x <- as.matrix(x)
  if (should_return_NA(x)) {
    return(NA_real_)
  }
  nchains <- NCOL(x)
  niterations <- NROW(x)
  chain_mean <- matrixStats::colMeans2(x)
  chain_var <- matrixStats::colVars(x, center=chain_mean)
  var_between <- niterations * var(chain_mean)
  var_within <- mean(chain_var)
  sqrt((var_between / var_within + niterations - 1) / niterations)
}

#' Compute the effective sample size
#' @template args-conv
#' @template return-conv
#' @noRd
.ess <- function(x) {
  x <- as.matrix(x)
  nchains <- NCOL(x)
  niterations <- NROW(x)
  if (niterations < 3L || should_return_NA(x)) {
    return(NA_real_)
  }
  acov <- apply(x, 2, autocovariance)
  acov_means <- matrixStats::rowMeans2(acov)
  mean_var <- acov_means[1] * niterations / (niterations - 1)
  var_plus <- mean_var * (niterations - 1) / niterations
  if (nchains > 1) {
    var_plus <- var_plus + var(matrixStats::colMeans2(x))
  }

  # Geyer's initial positive sequence
  rho_hat_t <- rep.int(0, niterations)
  t <- 0
  rho_hat_even <- 1
  rho_hat_t[t + 1] <- rho_hat_even
  rho_hat_odd <- 1 - (mean_var - acov_means[t + 2]) / var_plus
  rho_hat_t[t + 2] <- rho_hat_odd
  while (t < NROW(acov) - 5 && !is.nan(rho_hat_even + rho_hat_odd) &&
         (rho_hat_even + rho_hat_odd > 0)) {
    t <- t + 2
    rho_hat_even = 1 - (mean_var - acov_means[t + 1]) / var_plus
    rho_hat_odd = 1 - (mean_var - acov_means[t + 2]) / var_plus
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
  tau_bound <- 1 / log10(ess)
  if (tau_hat < tau_bound) {
    warning_no_call("The ESS has been capped to avoid unstable estimates.")
    tau_hat <- tau_bound
  }
  ess <- ess / tau_hat
  ess
}

# should NA be returned by a convergence diagnostic?
should_return_NA <- function(x, tol = .Machine$double.eps) {
  if (anyNA(x) || checkmate::anyInfinite(x)) {
    return(TRUE)
  }
  # checking for constant input per chain is too conservative for ess_tail
  # so we don't check this for the time being until we find a better solution
  # if (is.matrix(x)) {
  #   # is_constant() vectorized over x columns
  #   if (is.logical(x)) {
  #     return(any(matrixStats::colAnys(x) == matrixStats::colAlls(x)))
  #   } else if (is.integer(x)) {
  #     return(any(matrixStats::rowDiffs(matrixStats::colRanges(x)) == 0L))
  #   } else {
  #     return(any(abs(matrixStats::rowDiffs(matrixStats::colRanges(x))) < tol))
  #   }
  # }
  is_constant(x, tol = tol)
}
