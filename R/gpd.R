#' Quantile function for generalized pareto
#' @noRd
qgeneralized_pareto <- function(p, mu = 0, sigma = 1, k = 0, lower.tail = TRUE, log.p = FALSE) {
  stopifnot(length(mu) == 1 && length(sigma) == 1 && length(k) == 1)
  if (is.na(sigma) || sigma <= 0) {
    return(rep(NaN, length(p)))
  }
  if (log.p) {
    p <- exp(p)
  }
  if (!lower.tail) {
    p <- 1 - p
  }
  if (k == 0) {
    q <-  mu - sigma * log1p(-p)
  } else {
    q <- mu + sigma * expm1(-k * log1p(-p)) / k
  }
  q
}

#' Estimate parameters of the Generalized Pareto distribution
#'
#' Given a sample \eqn{x}, Estimate the parameters \eqn{k} and
#' \eqn{\sigma} of the generalized Pareto distribution (GPD), assuming
#' the location parameter is 0. By default the fit uses a prior for
#' \eqn{k} (this is in addition to the prior described by Zhang and
#' Stephens, 2009), which will stabilize estimates for very small
#' sample sizes (and low effective sample sizes in the case of MCMC
#' samples). The weakly informative prior is a Gaussian prior centered
#' at 0.5 (see details in Vehtari et al., 2024). This is used
#' internally but is exported for use by other packages.
#' @family helper-functions
#' @param x A numeric vector. The sample from which to estimate the
#'   parameters.
#' @param wip Logical indicating whether to adjust \eqn{k} based on a
#'   weakly informative Gaussian prior centered on 0.5. Defaults to
#'   `TRUE`.
#' @param min_grid_pts The minimum number of grid points used in the
#'   fitting algorithm. The actual number used is `min_grid_pts +
#'   floor(sqrt(length(x)))`.
#' @param sort_x If `TRUE` (the default), the first step in the
#'   fitting algorithm is to sort the elements of `x`. If `x` is
#'   already sorted in ascending order then `sort_x` can be set to
#'   `FALSE` to skip the initial sorting step.
#' @return A named list with components `k` and `sigma`.
#'
#' @details Here the parameter \eqn{k} is the negative of \eqn{k} in Zhang &
#'   Stephens (2009).
#'
#'
#' @references
#' Zhang, J., and Stephens, M. A. (2009). A new and efficient estimation method
#' for the generalized Pareto distribution. *Technometrics* **51**, 316-325.
#'
#' @noRd
gpdfit <- function(x, wip = TRUE, min_grid_pts = 30, sort_x = TRUE) {
  # see section 4 of Zhang and Stephens (2009)
  if (sort_x) {
    x <- sort.int(x)
  }
  N <- length(x)
  prior <- 3
  M <- min_grid_pts + floor(sqrt(N))
  jj <- seq_len(M)
  xstar <- x[floor(N / 4 + 0.5)] # first quartile of sample
  theta <- 1 / x[N] + (1 - sqrt(M / (jj - 0.5))) / prior / xstar
  k <- matrixStats::rowMeans2(log1p(-theta %o% x))
  l_theta <- N * (log(-theta / k) - k - 1) # profile log-lik
  w_theta <- exp(l_theta - matrixStats::logSumExp(l_theta)) # normalize
  theta_hat <- sum(theta * w_theta)
  k_hat <- mean.default(log1p(-theta_hat * x))
  sigma_hat <- -k_hat / theta_hat

  # adjust k_hat based on weakly informative prior, Gaussian centered on 0.5.
  # this stabilizes estimates for very small Monte Carlo sample sizes and low ess
  # (see Vehtari et al., 2024 for details)
  if (wip) {
    k_hat <- (k_hat * N + 0.5 * 10) / (N + 10)
  }

  if (is.na(k_hat)) {
    k_hat <- Inf
    sigma_hat <- NaN
  }

  list(k = k_hat, sigma = sigma_hat)
}
