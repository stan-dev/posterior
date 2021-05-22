# Copyright (C) 2012, 2013, 2014, 2015, 2016, 2017, 2018 Trustees of Columbia University
# Copyright (C) 2018, 2019 Aki Vehtari, Paul Bürkner
# See LICENSE.md for more details

#' Convergence diagnostics
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
#' | [rhat_basic()] | Basic version of Rhat |
#' | [rhat()] | Improved, rank-based version of Rhat |
#' | [rstar()] | R* diagnostic |
#'
NULL

#' Basic version of the Rhat convergence diagnostic
#'
#' Compute the basic Rhat convergence diagnostic for a single variable as
#' described in Gelman et al. (2013). For practical applications, we strongly
#' recommend the improved Rhat convergence diagnostic implemented in [rhat()].
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
    x <- .split_chains(x)
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
    x <- .split_chains(x)
  }
  .ess(x)
}

#' Rhat convergence diagnostic
#'
#' Compute the Rhat convergence diagnostic for a single variable as the maximum
#' of rank normalized split-Rhat and rank normalized folded-split-Rhat as
#' proposed in Vehtari et al. (2019).
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
  rhat_bulk <- .rhat(z_scale(.split_chains(x)))
  rhat_tail <- .rhat(z_scale(.split_chains(fold_draws(x))))
  max(rhat_bulk, rhat_tail)
}

#' Bulk effective sample size (bulk-ESS)
#'
#' Compute a bulk effective sample size estimate (bulk-ESS) for a single
#' variable. Bulk-ESS is useful as a diagnostic for the sampling efficiency in
#' the bulk of the posterior. It is defined as the effective sample size for
#' rank normalized values using split chains. For the tail effective sample size
#' see [ess_tail()].
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
ess_bulk <- function(x) UseMethod("ess_bulk")
#' @rdname ess_bulk
#' @export
ess_bulk.default <- function(x) {
  .ess(z_scale(.split_chains(x)))
}
#' @rdname ess_bulk
#' @export
ess_bulk.rvar <- function(x) {
  summarise_rvar_by_element_with_chains(x, ess_bulk)
}

#' Tail effective sample size (tail-ESS)
#'
#' Compute a tail effective sample size estimate (tail-ESS) for a single
#' variable. Tail-ESS is useful as a diagnostic for the sampling efficiency in
#' the tails of the posterior. It is defined as the minimum of the effective
#' sample sizes for 5% and 95% quantiles. For the bulk effective sample
#' size see [ess_bulk()].
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
#' Compute effective sample size estimates for quantile estimates of a single
#' variable.
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
  .ess(.split_chains(I))
}

#' Effective sample size for the mean
#'
#' Compute an effective sample size estimate for a mean (expectation)
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
  .ess(.split_chains(x))
}

#' Effective sample size for the standard deviation
#'
#' Compute an effective sample size estimate for the standard deviation (SD)
#' estimate of a single variable. This is defined as minimum of the effective
#' sample size estimate for the mean and the the effective sample size estimate
#' for the mean of the squared value.
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
  min(.ess(.split_chains(x)), .ess(.split_chains(x^2)))
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
#' Compute the Monte Carlo standard error for the mean (expectation) of a
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
#' Compute the Monte Carlo standard error for the standard deviation (SD) of a
#' single variable using Stirling's approximation and assuming approximate
#' normality.
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
#' with other summary functions in the \pkg{posterior} package.
#'
#' @template args-conv
#' @template args-conv-quantile
#' @param na.rm Logical. If `TRUE`, any `NA` and `NaN`'s are removed from `x`
#'   before the quantiles are computed.
#' @param ... Further arguments passed to [quantile()].
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' quantile2(mu)
#'
#' @export
quantile2 <- function(x, probs = c(0.05, 0.95), names = TRUE, na.rm = FALSE,
                      ...) {
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
#' @noRd
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
