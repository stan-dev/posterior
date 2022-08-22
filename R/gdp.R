#' The Generalized Pareto Distribution
#'
#' Density, distribution function, quantile function and random generation
#' for the generalized Pareto distribution with location equal to \code{mu},
#' scale equal to \code{sigma}, and shape equal to \code{k}.
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. If \code{length(n) > 1}, the length is taken
#' to be the number required.
#' @param mu scalar location parameter
#' @param sigma scalar, positive scale parameter
#' @param k scalar shape parameter
#' @param log,log.p logical; if TRUE, probabilities p are given as log(p).
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[X \le x]}
#' otherwise, \eqn{P[X > x]}.
#'
#' @name GPD


#' @rdname GPD
#' @export
dgpd <- function(x, mu = 0, sigma = 1, k = 0, log = FALSE) {
  stopifnot(length(mu) == 1 && length(sigma) == 1 && length(k) == 1)
  if (is.na(sigma) || sigma <= 0) {
    return(rep(NaN, length(x)))
  }
  d <- (x - mu) / sigma
  ind <- (d > 0) & ((1 + k * d) > 0)
  ind[is.na(ind)] <- FALSE
  if (k == 0) {
    d[ind] <- -d[ind] - log(sigma)
  } else {
    d[ind] <- log1p(k * d[ind]) * -(1 / k  + 1) - log(sigma)
  }
  d[!ind] <- -Inf
  if (!log) {
    d <- exp(d)
  }
  d
}


#' @rdname GPD
#' @export
pgpd <- function(q, mu = 0, sigma = 1, k = 0, lower.tail = TRUE, log.p = FALSE) {
  stopifnot(length(mu) == 1 && length(sigma) == 1 && length(k) == 1)
  if (is.na(sigma) || sigma <= 0) {
    return(rep(NaN, length(q)))
  }
  q <- pmax(q - mu, 0) / sigma
  if (k == 0) {
    p <- 1 - exp(-q)
  } else {
    p <- -expm1(log(pmax(1 + k * q, 0)) * -(1 / k))
  }
  if (!lower.tail) {
    p <- 1 - p
  }
  if (log.p) {
    p <- log(p)
  }
  p
}

#' @rdname GPD
#' @export
qgpd <- function(p, mu = 0, sigma = 1, k = 0, lower.tail = TRUE, log.p = FALSE) {
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

#' @rdname GPD
#' @export
rgpd <- function(n, mu = 0, sigma = 1, k = 0) {
  stopifnot(
    length(n) == 1 && length(mu) == 1 && length(sigma) == 1 && length(k) == 1
  )
  if (is.na(sigma) || sigma <= 0) {
    return(rep(NaN, n))
  }
  if (k == 0) {
    r <- mu + sigma * rexp(n)
  } else {
    r <- mu + sigma * expm1(-k * log(runif(n))) / k
  }
  r
}


#' Estimate parameters of the Generalized Pareto distribution
#'
#' Given a sample \eqn{x}, Estimate the parameters \eqn{k} and \eqn{\sigma} of
#' the generalized Pareto distribution (GPD), assuming the location parameter is
#' 0. By default the fit uses a prior for \eqn{k}, which will stabilize
#' estimates for very small sample sizes (and low effective sample sizes in the
#' case of MCMC samples). The weakly informative prior is a Gaussian prior
#' centered at 0.5.
#'
#' @export
#' @param x A numeric vector. The sample from which to estimate the parameters.
#' @param wip Logical indicating whether to adjust \eqn{k} based on a weakly
#'   informative Gaussian prior centered on 0.5. Defaults to `TRUE`.
#' @param min_grid_pts The minimum number of grid points used in the fitting
#'   algorithm. The actual number used is `min_grid_pts + floor(sqrt(length(x)))`.
#' @param sort_x If `TRUE` (the default), the first step in the fitting
#'   algorithm is to sort the elements of `x`. If `x` is already
#'   sorted in ascending order then `sort_x` can be set to `FALSE` to
#'   skip the initial sorting step.
#' @return A named list with components `k` and `sigma`.
#'
#' @details Here the parameter \eqn{k} is the negative of \eqn{k} in Zhang &
#'   Stephens (2009).
#'
#' @seealso [psis()], [pareto-k-diagnostic]
#'
#' @references
#' Zhang, J., and Stephens, M. A. (2009). A new and efficient estimation method
#' for the generalized Pareto distribution. *Technometrics* **51**, 316-325.
#'
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
  # this stabilizes estimates for very small Monte Carlo sample sizes and low neff
  if (wip) {
    k_hat <- (k_hat * N + 0.5 * 10) / (N + 10)
  }

  if (is.na(k_hat)) {
    k_hat <- Inf
    sigma_hat <- NaN
  }

  list("k" = k_hat, "sigma" = sigma_hat)
}

# Below is a separate function for the marginal posterior.
# We can extend to have computationally more intensive but more accurate methods.
gpdpost <- function(x, min_grid_pts = 30, sort_x = TRUE) {

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

  # quadrature weights for k are same as for theta
  k_w <- w_theta
  # quadrature weights are just the normalized likelihoods
  # we get the unnormalized posterior by multiplying these by the prior
  k_d <- k_w * dgpd(-theta, mu = -1 / x[N], sigma = 1 / prior / xstar, k = 0.5)
  # normalize using the trapezoidal rule
  Z <- sum((k_d[-M] + k_d[-1]) * (k[-M] - k[-1])) / 2
  k_d <- k_d / Z

  list("k" = k, "k_w" = k_w, "k_d" = k_d)
}
