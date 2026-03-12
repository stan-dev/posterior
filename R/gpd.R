#' Quantile function for the generalized Pareto distribution
#'
#' Computes the quantile function for a generalized Pareto distribution
#' with location `mu`, scale `sigma`, and shape `k`.
#' 
#' @param p Numeric vector of probabilities.
#' @param mu Location parameter.
#' @param sigma Scale parameter (must be positive).
#' @param k Shape parameter.
#' @param lower.tail Logical; if `TRUE` (default), probabilities are `P[X <= x]`.
#' @param log.p Logical; if `TRUE`, probabilities are given as `log(p)`.
#' @return A numeric vector of quantiles.
#' @keywords internal
#' @export
#' @examples
#' qgeneralized_pareto(p = c(0.1, 0.5, 0.9), mu = 0, sigma = 1, k = 0.2)
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

#' Distribution function for the generalized Pareto distribution
#'
#' Computes the cumulative distribution function (CDF) for a generalized
#' Pareto distribution with location `mu`, scale `sigma`, and shape `k`.
#'
#' @param q Numeric vector of quantiles.
#' @param mu Location parameter.
#' @param sigma Scale parameter (must be positive).
#' @param k Shape parameter.
#' @param lower.tail Logical; if `TRUE` (default), probabilities are `P[X <= x]`.
#' @param log.p Logical; if `TRUE`, probabilities are returned as `log(p)`.
#' @return A numeric vector of probabilities.
#' @keywords internal
#' @export
#' @examples
#' pgeneralized_pareto(q = c(1, 2, 5), mu = 0, sigma = 1, k = 0.2)
pgeneralized_pareto <- function(q, mu = 0, sigma = 1, k = 0, lower.tail = TRUE, log.p = FALSE) {
  stopifnot(length(mu) == 1 && length(sigma) == 1 && length(k) == 1)
  if (is.na(sigma) || sigma <= 0) {
    return(rep(NaN, length(q)))
  }
  z <- (q - mu) / sigma
  if (abs(k) < 1e-15) {
    # for very small values of indistinguishable in floating point accuracy from the case k=0
    p <- -expm1(-z)
  } else {
    # pmax handles values outside the support
    p <- -expm1(log1p(pmax(k * z, -1)) / -k)
  }
  # force to [0, 1] for values outside the support
  p <- pmin(pmax(p, 0), 1)
  if (!lower.tail) {
    p <- 1 - p
  }
  if (log.p) {
    p <- log(p)
  }
  p
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
#' @param weights An optional numeric vector of positive weights the same
#'   length as `x`. If `NULL` (the default), all observations are
#'   weighted equally and the result is identical to the unweighted fit.
#'   Weights are normalized internally to sum to `length(x)`.
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
#' @keywords internal
#' @export
gpdfit <- function(x, wip = TRUE, min_grid_pts = 30, sort_x = TRUE,
                   weights = NULL) {
  # see section 4 of Zhang and Stephens (2009)
  if (sort_x) {
    if (!is.null(weights)) {
      ord <- sort.int(x, index.return = TRUE)
      x <- ord$x
      weights <- weights[ord$ix]
    } else {
      x <- sort.int(x)
    }
  }
  N <- length(x)

  # normalize weights to sum to N so the log-likelihood scale is preserved
  if (!is.null(weights)) {
    weights <- weights / sum(weights) * N
  }

  prior <- 3
  M <- min_grid_pts + floor(sqrt(N))
  jj <- seq_len(M)
  xstar <- x[floor(N / 4 + 0.5)] # first quartile of sample
  if (xstar > x[1])  {
    # first quantile is bigger than the minimum
    theta <- 1 / x[N] + (1 - sqrt(M / (jj - 0.5))) / prior / xstar

    # log1p(-theta %o% x) is M x N matrix
    log1p_mat <- log1p(-theta %o% x)

    if (!is.null(weights)) {
      # weighted mean across observations for each theta value
      k <- drop(log1p_mat %*% weights) / N
    } else {
      k <- matrixStats::rowMeans2(log1p_mat)
    }

    l_theta <- N * (log(-theta / k) - k - 1) # profile log-lik
    w_theta <- exp(l_theta - matrixStats::logSumExp(l_theta)) # normalize
    theta_hat <- sum(theta * w_theta)

    if (!is.null(weights)) {
      k_hat <- sum(weights * log1p(-theta_hat * x)) / N
    } else {
      k_hat <- mean.default(log1p(-theta_hat * x))
    }
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
  } else {
    # first quantile is not bigger than the minimum, which indicates
    # that the distribution is far from a generalized Pareto
    # distribution
    k_hat <- NA
    sigma_hat <- NA
  }

  list(k = k_hat, sigma = sigma_hat)
}
