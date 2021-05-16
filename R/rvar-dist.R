#' Density, CDF, and quantile functions of random variables
#'
#' The probability density function (`density()`), cumulative distribution
#' function (`cdf()`), and quantile function / inverse CDF (`quantile()`) of
#' an [`rvar`].
#'
#' @param x an [`rvar`]
#' @param q,at vector of quantiles.
#' @param probs vector of probabilities
#' @param ... Additional arguments passed onto underlying methods:
#'   - For `density()`, these are passed to [stats::density()].
#'   - For `cdf()`, these are ignored.
#'   - For `quantile()`, these are passed to [stats::quantile()].
#'
#' @return
#'
#' A vector of the same length as the input (`q`, `at`, or `probs`) containing
#' values from the corresponding function of the given [`rvar`].
#'
#' @name rvar-functions
#' @export
density.rvar <- function(x, at, ...) {
  if (length(x) != 1) {
    stop_no_call("density() can currently only be used on scalar rvars")
  }

  d <- density(draws_of(x), cut = 0, ...)
  f <- approxfun(d$x, d$y, yleft = 0, yright = 0)
  f(at)
}

#' @importFrom distributional cdf
#' @export
distributional::cdf

#' @rdname rvar-functions
#' @export
cdf.rvar <- function(x, q, ...) {
  if (length(x) != 1) {
    stop_no_call("cdf() can currently only be used on scalar rvars")
  }

  ecdf(draws_of(x))(q)
}

#' @rdname rvar-functions
#' @export
quantile.rvar <- function(x, probs, ...) {
  quantile(draws_of(x), probs, ...)
}
