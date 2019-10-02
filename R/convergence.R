# This file is part of posterior
# Copyright (C) 2012, 2013, 2014, 2015, 2016, 2017, 2018 Trustees of Columbia University
# Copyright (C) 2018, 2019 Aki Vehtari, Paul Bürkner
#
# posterior is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the License, or (at your option) any later version.
#
# posterior is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

fft_next_good_size <- function(N) {
  # Find the optimal next size for the FFT so that
  # a minimum number of zeros are padded.
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
#' input sequence using a fast Fourier transform approach. Estimate
#' for lag t is scaled by N-t.
#'
#' @param x A numeric vector forming a sequence of values.
#'
#' @return A numeric vector of autocovariances at every lag (scaled by N-lag).
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
#' input sequence using a fast Fourier transform approach. Estimate
#' for lag t is scaled by N-t.
#'
#' @param x A numeric vector forming a sequence of values.
#'
#' @return A numeric vector of autocorrelations at every lag (scaled by N-lag).
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
#' @param x A numeric array of values.
#'
#' @return A numeric array of rank normalized values with the same
#'     size as input.
z_scale <- function(x) {
  S <- length(x)
  r <- rank(as.array(x), ties.method = 'average')
  z <- qnorm((r - 1 / 2) / S)
  if (!is.null(dim(x))) {
    # output should have the input dimension
    z <- rray(z, dim = dim(x), dim_names = dimnames(x))
  }
  z
}

#' Rank uniformization
#'
#' Compute rank uniformization for a numeric array. First replace each
#' value by its rank. Average rank for ties are used to conserve the
#' number of unique values of discrete quantities. Second, uniformize
#' ranks to scale \code{[1/(2S), 1-1/(2S)]}, where \code{S} is the the number
#' of values.
#'
#' @param x A numeric array of values.
#'
#' @return A numeric array of rank uniformized values with the same
#'     size as input.
#'
u_scale <- function(x) {
  S <- length(x)
  r <- rank(as.array(x), ties.method = 'average')
  u <- (r - 1 / 2) / S
  if (!is.null(dim(x))) {
    # output should have the input dimension
    u <- rray(u, dim = dim(x), dim_names = dimnames(x))
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
#' @param x A numeric array of values.
#'
#' @return A numeric array of ranked values with the same
#'     size as input.
#'
r_scale <- function(x) {
  S <- length(x)
  r <- rank(as.array(x), ties.method = 'average')
  if (!is.null(dim(x))) {
    # output should have the input dimension
    r <- rray(r, dim = dim(x), dim_names = dimnames(x))
  }
  r
}

split_chains <- function(x) {
  # split Markov chains
  # Args:
  #   x: a 2D array of samples (# iter * # chains)
  niter <- dim(x)[1]
  if (niter == 1L) {
    return(x)
  }
  half <- niter / 2
  rray_cbind(x[1:floor(half), ], x[ceiling(half + 1):niter, ])
}

is_constant <- function(x, tol = .Machine$double.eps) {
  abs(max(x) - min(x)) < tol
}

#' Traditional Rhat convergence diagnostic
#'
#' Compute the Rhat convergence diagnostic for a single parameter
#' For split-Rhat, call this with split chains.
#'
#' @param x A 2D array _without_ warmup draws (# iter * # chains).
#'
#' @return A single numeric value for Rhat.
#'
#' @references
#' Aki Vehtari, Andrew Gelman, Daniel Simpson, Bob Carpenter, and
#' Paul-Christian Bürkner (2019). Rank-normalization, folding, and
#' localization: An improved R-hat for assessing convergence of
#' MCMC. \emph{arXiv preprint} \code{arXiv:1903.08008}.
rhat_rfun <- function(x) {
  if (any(!is.finite(x))) {
    return(NaN)
  }
  else if (is_constant(x)) {
    return(1)
  }
  chains <- ncol(x)
  ndraws <- nrow(x)
  chain_mean <- numeric(chains)
  chain_var <- numeric(chains)
  for (i in seq_len(chains)) {
    chain_mean[i] <- mean(x[, i])
    chain_var[i] <- var(x[, i])
  }
  var_between <- ndraws * var(chain_mean)
  var_within <- mean(chain_var)
  sqrt((var_between / var_within + ndraws - 1) / ndraws)
}

#' Effective sample size
#'
#' Compute the effective sample size estimate for a sample of several chains
#' for one parameter. For split-ESS, call this with split chains.
#'
#' @param x A 2D array _without_ warmup draws (# iter * # chains).
#'
#' @return A single numeric value for the effective sample size.
#'
#' @references
#' Aki Vehtari, Andrew Gelman, Daniel Simpson, Bob Carpenter, and
#' Paul-Christian Bürkner (2019). Rank-normalization, folding, and
#' localization: An improved R-hat for assessing convergence of
#' MCMC. \emph{arXiv preprint} \code{arXiv:1903.08008}.
ess_rfun <- function(x) {
  chains <- ncol(x)
  ndraws <- nrow(x)
  if (any(!is.finite(x)) || ndraws < 3L) {
    return(NaN)
  }
  else if (is_constant(x)) {
    return(chains * ndraws)
  }
  .acov_fun <- function(i) autocovariance(rray_extract(x, , i))
  acov <- lapply(seq_len(chains), .acov_fun)
  acov <- do.call(cbind, acov)
  chain_mean <- apply(x, 2, mean)
  mean_var <- mean(acov[1, ]) * ndraws / (ndraws - 1)
  var_plus <- mean_var * (ndraws - 1) / ndraws
  if (chains > 1) {
    var_plus <- var_plus + var(chain_mean)
  }

  # Geyer's initial positive sequence
  rho_hat_t <- rep.int(0, ndraws)
  t <- 0
  rho_hat_even <- 1
  rho_hat_t[t + 1] <- rho_hat_even
  rho_hat_odd <- 1 - (mean_var - mean(acov[t + 2, ])) / var_plus
  rho_hat_t[t + 2] <- rho_hat_odd
  while (t < nrow(acov) - 5 && !is.nan(rho_hat_even + rho_hat_odd) &&
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
  ess <- chains * ndraws
  # Geyer's truncated estimate
  # tau_hat <- -1 + 2 * sum(rho_hat_t[1:max_t])
  # Improved estimate reduces variance in antithetic case
  tau_hat <- -1 + 2 * sum(rho_hat_t[1:max_t]) + rho_hat_t[max_t+1]
  # Safety check for negative values and with max ess equal to ess*log10(ess)
  tau_hat <- max(tau_hat, 1/log10(ess))
  ess <- ess / tau_hat
  ess
}

#' Rhat convergence diagnostic
#'
#' Compute Rhat convergence diagnostic as the maximum of rank normalized
#' split-Rhat and rank normalized folded-split-Rhat for one parameter.
#'
#' @param x A 2D array _without_ warmup draws (# iter * # chains).
#'
#' @return A single numeric value for the effective sample size.
#'
#' @references
#' Aki Vehtari, Andrew Gelman, Daniel Simpson, Bob Carpenter, and
#' Paul-Christian Bürkner (2019). Rank-normalization, folding, and
#' localization: An improved R-hat for assessing convergence of
#' MCMC. \emph{arXiv preprint} \code{arXiv:1903.08008}.
#'
#' @export
Rhat <- function(x) {
  bulk_rhat <- rhat_rfun(z_scale(split_chains(x)))
  sims_folded <- abs(x - median(x))
  tail_rhat <- rhat_rfun(z_scale(split_chains(sims_folded)))
  max(bulk_rhat, tail_rhat)
}

#' Bulk effective sample size (bulk-ESS)
#'
#' Compute bulk effective sample size estimate (bulk-ESS) for one parameter.
#' Bulk-ESS is useful as a generic diagnostic for the sampling
#' efficiency in the bulk of the posterior. It is defined as the
#' effective sample size for rank normalized values using split chains.
#'
#' @param x A 2D array _without_ warmup draws (# iter * # chains).
#'
#' @return A single numeric value for the bulk effective sample size.
#'
#' @references
#' Aki Vehtari, Andrew Gelman, Daniel Simpson, Bob Carpenter, and
#' Paul-Christian Bürkner (2019). Rank-normalization, folding, and
#' localization: An improved R-hat for assessing convergence of
#' MCMC. \emph{arXiv preprint} \code{arXiv:1903.08008}.
#'
#' @export
ess_bulk <- function(x) {
  ess_rfun(z_scale(split_chains(x)))
}

#' Tail effective sample size (tail-ESS)
#'
#' Compute tail effective sample size estimate (tail-ESS) for one parameter.
#' Tail-ESS is useful for generic diagnostic for the sampling
#' efficiency in the tails of the posterior. It is defined as
#' the minimum of the effective sample sizes for 5% and 95% quantiles.
#'
#' @param x A 2D array _without_ warmup draws (# iter * # chains).
#'
#' @return A single numeric value for the tail effective sample size.
#'
#' @references
#' Aki Vehtari, Andrew Gelman, Daniel Simpson, Bob Carpenter, and
#' Paul-Christian Bürkner (2019). Rank-normalization, folding, and
#' localization: An improved R-hat for assessing convergence of
#' MCMC. \emph{arXiv preprint} \code{arXiv:1903.08008}.
#'
#' @export
ess_tail <- function(x) {
  I05 <- x <= quantile(x, 0.05)
  q05_ess <- ess_rfun(split_chains(I05))
  I95 <- x <= quantile(x, 0.95)
  q95_ess <- ess_rfun(split_chains(I95))
  min(q05_ess, q95_ess)
}

#' Quantile effective sample size
#'
#' Compute effective sample size estimate for a quantile estimate of
#' one parameter.
#'
#' @param x A 2D array _without_ warmup draws (# iter * # chains).
#' @param prob A single numeric value of probability.
#'
#' @return A single numeric value for the effective sample size for a
#'     quantile estimate corresponding to the probability.
#'
#' @references
#' Aki Vehtari, Andrew Gelman, Daniel Simpson, Bob Carpenter, and
#' Paul-Christian Bürkner (2019). Rank-normalization, folding, and
#' localization: An improved R-hat for assessing convergence of
#' MCMC. \emph{arXiv preprint} \code{arXiv:1903.08008}.
#'
#' @export
ess_quantile <- function(x, prob) {
  I <- x <= quantile(x, prob)
  ess_rfun(split_chains(I))
}

#' Effective sample size
#'
#' Compute effective sample size estimate for a mean (expectation)
#' estimate of one parameter.
#'
#' @param x A 2D array _without_ warmup draws (# iter * # chains).
#'
#' @return A single numeric value for the effective sample size
#'     estimate for mean estimate.
#'
#' @references
#' Aki Vehtari, Andrew Gelman, Daniel Simpson, Bob Carpenter, and
#' Paul-Christian Bürkner (2019). Rank-normalization, folding, and
#' localization: An improved R-hat for assessing convergence of
#' MCMC. \emph{arXiv preprint} \code{arXiv:1903.08008}.
#'
#' @export
ess_mean <- function(x) {
  ess_rfun(split_chains(x))
}

#' Effective sample size
#'
#' Compute effective sample size estimate for standard deviation (s)
#' estimate of one parameter. This is defined as minimum of effective
#' sample size estimate for mean and mean of squared value.
#'
#' @param x A 2D array _without_ warmup draws (# iter * # chains).
#'
#' @return A single numeric value for the effective sample size
#'     estimate for standard deviation estimate.
#'
#' @references
#' Aki Vehtari, Andrew Gelman, Daniel Simpson, Bob Carpenter, and
#' Paul-Christian Bürkner (2019). Rank-normalization, folding, and
#' localization: An improved R-hat for assessing convergence of
#' MCMC. \emph{arXiv preprint} \code{arXiv:1903.08008}.
#'
#' @export
ess_sd <- function(x) {
  min(ess_rfun(split_chains(x)), ess_rfun(split_chains(x^2)))
}

#' Monte Carlo standard error for a quantile
#'
#' Compute Monte Carlo standard error for a quantile estimate of a
#' single parameter.
#'
#' @param x A 2D array _without_ warmup draws (# iter * # chains).
#' @param prob A single numeric value of probability.
#'
#' @return A single numeric value for Monte Carlo standard error for a
#'     quantile estimate corresponding to the probability.
#'
#' @references
#' Aki Vehtari, Andrew Gelman, Daniel Simpson, Bob Carpenter, and
#' Paul-Christian Bürkner (2019). Rank-normalization, folding, and
#' localization: An improved R-hat for assessing convergence of
#' MCMC. \emph{arXiv preprint} \code{arXiv:1903.08008}.
#'
#' @export
mcse_quantile <- function(x, prob) {
  ess <- ess_quantile(x, prob)
  p <- c(0.1586553, 0.8413447)
  a <- qbeta(p, ess * prob + 1, ess * (1 - prob) + 1)
  ssims <- sort(x)
  S <- length(ssims)
  th1 <- ssims[max(round(a[1] * S), 1)]
  th2 <- ssims[min(round(a[2] * S), S)]
  as.vector((th2 - th1) / 2)
}

#' Monte Carlo standard error for mean
#'
#' Compute Monte Carlo standard error for mean (expectation) of a
#' single parameter.
#'
#' @param x A 2D array _without_ warmup draws (# iter * # chains).
#'
#' @return A single numeric value for Monte Carlo standard error
#'     for mean estimate.
#'
#' @references
#' Aki Vehtari, Andrew Gelman, Daniel Simpson, Bob Carpenter, and
#' Paul-Christian Bürkner (2019). Rank-normalization, folding, and
#' localization: An improved R-hat for assessing convergence of
#' MCMC. \emph{arXiv preprint} \code{arXiv:1903.08008}.
#'
#' @references
#' Aki Vehtari, Andrew Gelman, Daniel Simpson, Bob Carpenter, and
#' Paul-Christian Bürkner (2019). Rank-normalization, folding, and
#' localization: An improved R-hat for assessing convergence of
#' MCMC. \emph{arXiv preprint} \code{arXiv:1903.08008}.
#'
#' @export
mcse_mean <- function(x) {
  sd(x) / sqrt(ess_mean(x))
}

#' Monte Carlo standard error for standard error
#'
#' Compute Monte Carlo standard error for standard deviation (sd) of a
#' single parameter using Stirling's approximation and assuming
#' approximate normality.
#'
#' @param x A 2D array _without_ warmup draws (# iter * # chains).
#'
#' @return A single numeric value for Monte Carlo standard error
#'     for standard deviation estimate.
#'
#' @references
#' Aki Vehtari, Andrew Gelman, Daniel Simpson, Bob Carpenter, and
#' Paul-Christian Bürkner (2019). Rank-normalization, folding, and
#' localization: An improved R-hat for assessing convergence of
#' MCMC. \emph{arXiv preprint} \code{arXiv:1903.08008}.
#'
#' @export
mcse_sd <- function(x) {
  # assumes normality of x and uses Stirling's approximation
  ess_sd <- ess_sd(x)
  sd(x) * sqrt(exp(1) * (1 - 1 / ess_sd)^(ess_sd - 1) - 1)
}
