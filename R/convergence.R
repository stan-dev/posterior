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
    z <- array(z, dim = dim(x), dimnames = dimnames(x))
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
    r <- array(r, dim = dim(x), dimnames = dimnames(x))
  }
  r
}

# split Markov chains in half
# @param x a 2D array of draws (#iterations * #chains)
# @return a 2D array of draws with split chains
split_chains <- function(x) {
  if (is.null(dim(x))) {
    x <- matrix(x)
  }
  niter <- NROW(x)
  if (niter == 1L) {
    return(x)
  }
  half <- niter / 2
  cbind(x[1:floor(half), ], x[ceiling(half + 1):niter, ])
}

# compute the rhat converence diagnostic
# @param x A 2D array of draws (#iterations * #chains).
.rhat <- function(x) {
  if (any(!is.finite(x))) {
    return(NaN)
  }
  else if (is_constant(x)) {
    return(1)
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

.ess <- function(x) {
  nchains <- NCOL(x)
  niterations <- NROW(x)
  if (any(!is.finite(x)) || niterations < 3L) {
    return(NaN)
  }
  else if (is_constant(x)) {
    return(nchains * niterations)
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

#' Basic version of the Rhat convergence diagnostic
#'
#' Compute the basic Rhat convergence diagnostic for a single parameter as
#' described in Gelman et al. (2013). For practical applications, we strongly
#' recommend the improved Rhat convergence diagnostic implemented in
#' \code{\link{Rhat}}.
#'
#' @param x A 2D array _without_ warmup draws (#iterations * #chains).
#' @param split Logical. If \code{TRUE}, compute Rhat on split chains.
#'
#' @return A single numeric value for Rhat.
#'
#' @references
#' Andrew Gelman, John B. Carlin, Hal S. Stern, David B. Dunson, Aki Vehtari and
#' Donald B. Rubin (2013). Bayesian Data Analysis, Third Edition. Chapman and
#' Hall/CRC.
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
#' Compute the basic effective sample size (ESS) estimate for a single parameter
#' as described in Gelman et al. (2013). For practical applications, we strongly
#' recommend the improved ESS convergence diagnostics implemented in
#' \code{\link{ess_bulk}} and \code{\link{ess_tail}}.
#'
#' @param x A 2D array _without_ warmup draws (#iterations * #chains).
#' @param split Logical. If \code{TRUE}, compute ESS on split chains.
#'
#' @return A single numeric value for the effective sample size.
#'
#' @references
#' Andrew Gelman, John B. Carlin, Hal S. Stern, David B. Dunson, Aki Vehtari and
#' Donald B. Rubin (2013). Bayesian Data Analysis, Third Edition. Chapman and
#' Hall/CRC.
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
#' split-Rhat and rank normalized folded-split-Rhat for a single parameter
#' as proposed in Vehtari et al. (2019).
#'
#' @param x A 2D array _without_ warmup draws (#iterations * #chains).
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
rhat <- function(x) {
  rhat_bulk <- .rhat(z_scale(split_chains(x)))
  sims_folded <- abs(x - median(x))
  rhat_tail <- .rhat(z_scale(split_chains(sims_folded)))
  max(rhat_bulk, rhat_tail)
}

#' Bulk effective sample size (bulk-ESS)
#'
#' Compute bulk effective sample size estimate (bulk-ESS) for a single parameter.
#' Bulk-ESS is useful as a generic diagnostic for the sampling
#' efficiency in the bulk of the posterior. It is defined as the
#' effective sample size for rank normalized values using split chains.
#'
#' @param x A 2D array _without_ warmup draws (#iterations * #chains).
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
  .ess(z_scale(split_chains(x)))
}

#' Tail effective sample size (tail-ESS)
#'
#' Compute tail effective sample size estimate (tail-ESS) for a single parameter.
#' Tail-ESS is useful for generic diagnostic for the sampling
#' efficiency in the tails of the posterior. It is defined as
#' the minimum of the effective sample sizes for 5% and 95% quantiles.
#'
#' @param x A 2D array _without_ warmup draws (#iterations * #chains).
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
  q05_ess <- ess_quantile(x, 0.05)
  q95_ess <- ess_quantile(x, 0.95)
  min(q05_ess, q95_ess)
}

#' Effective sample sizes for quantiles
#'
#' Compute effective sample size estimates for quantile estimates of
#' a single parameter.
#'
#' @param x A 2D array _without_ warmup draws (#iterations * #chains).
#' @param probs A numeric vector of probabilities.
#' @param names Logical. If \code{TRUE}, the result has a names
#'   attribute. Set to \code{FALSE} for speedup with many probs.
#'
#' @return A numeric vector of effective sample sizes for quantile estimates.
#'
#' @references
#' Aki Vehtari, Andrew Gelman, Daniel Simpson, Bob Carpenter, and
#' Paul-Christian Bürkner (2019). Rank-normalization, folding, and
#' localization: An improved R-hat for assessing convergence of
#' MCMC. \emph{arXiv preprint} \code{arXiv:1903.08008}.
#'
#' @export
ess_quantile <- function(x, probs, names = TRUE) {
  probs <- as.numeric(probs)
  if (any(probs < 0 | probs > 1)) {
    stop2("'probs' must contain values between 0 and 1.")
  }
  names <- as_one_logical(names)
  out <- ulapply(probs, .ess_quantile, x = x)
  if (names) {
    names(out) <- paste0("q", probs * 100)
  }
  out
}

# ess of a single quantile
.ess_quantile <- function(x, prob) {
  I <- x <= quantile(x, prob)
  .ess(split_chains(I))
}

#' @rdname ess_quantile
#' @export
ess_median <- function(x) {
  .ess_quantile(x, prob = 0.5)
}

#' Effective sample size for the mean
#'
#' Compute effective sample size estimate for a mean (expectation)
#' estimate of a single parameter.
#'
#' @param x A 2D array _without_ warmup draws (#iterations * #chains).
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
  .ess(split_chains(x))
}

#' Effective sample size for the standard deviation
#'
#' Compute an effective sample size estimate for the standard deviation (sd)
#' estimate of a single parameter. This is defined as minimum of effective
#' sample size estimate for mean and mean of squared value.
#'
#' @param x A 2D array _without_ warmup draws (#iterations * #chains).
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
  min(.ess(split_chains(x)), .ess(split_chains(x^2)))
}

#' Monte Carlo standard error for quantiles
#'
#' Compute Monte Carlo standard errors for quantile estimates of a
#' single parameter.
#'
#' @param x A 2D array _without_ warmup draws (#iterations * #chains).
#' @param probs A numeric vector of probabilities.
#' @param names Logical. If \code{TRUE}, the result has a names
#'   attribute. Set to \code{FALSE} for speedup with many probs.
#'
#' @return A numeric vector of Monte Carlo standard errors for quantile
#'   estimates.
#'
#' @references
#' Aki Vehtari, Andrew Gelman, Daniel Simpson, Bob Carpenter, and
#' Paul-Christian Bürkner (2019). Rank-normalization, folding, and
#' localization: An improved R-hat for assessing convergence of
#' MCMC. \emph{arXiv preprint} \code{arXiv:1903.08008}.
#'
#' @export
mcse_quantile <- function(x, probs, names = TRUE) {
  probs <- as.numeric(probs)
  if (any(probs < 0 | probs > 1)) {
    stop2("'probs' must contain values between 0 and 1.")
  }
  names <- as_one_logical(names)
  out <- ulapply(probs, .mcse_quantile, x = x)
  if (names) {
    names(out) <- paste0("q", probs * 100)
  }
  out
}

# mcse of a single quantile
.mcse_quantile <- function(x, prob) {
  ess <- ess_quantile(x, prob)
  p <- c(0.1586553, 0.8413447)
  a <- qbeta(p, ess * prob + 1, ess * (1 - prob) + 1)
  ssims <- sort(x)
  S <- length(ssims)
  th1 <- ssims[max(round(a[1] * S), 1)]
  th2 <- ssims[min(round(a[2] * S), S)]
  as.vector((th2 - th1) / 2)
}

#' @rdname mcse_quantile
#' @export
mcse_median <- function(x) {
  .mcse_quantile(x, prob = 0.5)
}

#' Monte Carlo standard error for the mean
#'
#' Compute Monte Carlo standard error for mean (expectation) of a
#' single parameter.
#'
#' @param x A 2D array _without_ warmup draws (#iterations * #chains).
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

#' Monte Carlo standard error for the standard deviation
#'
#' Compute Monte Carlo standard error for standard deviation (sd) of a
#' single parameter using Stirling's approximation and assuming
#' approximate normality.
#'
#' @param x A 2D array _without_ warmup draws (#iterations * #chains).
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
