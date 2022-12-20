#' Density, CDF, and quantile functions of random variables
#'
#' The probability density function (`density()`), cumulative distribution
#' function (`cdf()`), and quantile function / inverse CDF (`quantile()`) of
#' an [`rvar`].
#'
#' @param x (rvar) An [`rvar`] object.
#' @param q,at (numeric vector) One or more quantiles.
#' @param probs (numeric vector) One or more probabilities in `[0,1]`.
#' @param ... Additional arguments passed onto underlying methods:
#'   - For `density()`, these are passed to [stats::density()].
#'   - For `cdf()`, these are ignored.
#'   - For `quantile()`, these are passed to [stats::quantile()].
#'
#' @return
#'
#' If `x` is a scalar [`rvar`], returns a vector of the same length as the input
#' (`q`, `at`, or `probs`) containing values from the corresponding function
#' of the given [`rvar`].
#'
#' If `x` has length greater than 1, returns an array with dimensions
#' `c(length(y), dim(x))` where `y` is `q`, `at`, or `probs`, where each
#' `result[i,...]` is the value of the corresponding function,`f(y[i])`, for
#' the corresponding cell in the input array, `x[...]`.
#'
#' @examples
#'
#' set.seed(1234)
#' x = rvar(rnorm(100))
#'
#' density(x, seq(-2, 2, length.out = 10))
#' cdf(x, seq(-2, 2, length.out = 10))
#' quantile(x, ppoints(10))
#'
#' x2 = c(rvar(rnorm(100, mean = -0.5)), rvar(rnorm(100, mean = 0.5)))
#' density(x2, seq(-2, 2, length.out = 10))
#' cdf(x2, seq(-2, 2, length.out = 10))
#' quantile(x2, ppoints(10))
#'
#' @name rvar-dist
#' @export
density.rvar <- function(x, at, ...) {
  summarise_rvar_by_element(x, function(draws) {
    d <- density(draws, cut = 0, ...)
    f <- approxfun(d$x, d$y, yleft = 0, yright = 0)
    f(at)
  })
}

#' @rdname rvar-dist
#' @export
density.rvar_factor <- function(x, at, ...) {
  at <- as.numeric(factor(at, levels = levels(x)))
  nbins <- nlevels(x)

  summarise_rvar_by_element(x, function(draws) {
    props <- prop.table(tabulate(draws, nbins = nbins))[at]
    props
  })
}

#' @importFrom distributional cdf
#' @export
distributional::cdf

#' @rdname rvar-dist
#' @export
cdf.rvar <- function(x, q, ...) {
  summarise_rvar_by_element(x, function(draws) {
    ecdf(draws)(q)
  })
}

#' @rdname rvar-dist
#' @export
cdf.rvar_factor <- function(x, q, ...) {
  # CDF is not defined for unordered distributions
  # generate an all-NA array of the appropriate shape
  out <- rep_len(NA, length(x) * length(q))
  if (length(x) > 1) dim(out) <- c(length(q), dim(x))
  out
}

#' @rdname rvar-dist
#' @export
cdf.rvar_ordered <- function(x, q, ...) {
  q <- as.numeric(factor(q, levels = levels(x)))
  cdf.rvar(x, q, ...)
}

#' @rdname rvar-dist
#' @export
quantile.rvar <- function(x, probs, ...) {
  summarise_rvar_by_element_via_matrix(x,
    "quantile",
    function(draws) {
      t(matrixStats::colQuantiles(draws, probs = probs, useNames = TRUE, ...))
    },
    .extra_dim = length(probs),
    .extra_dimnames = list(NULL)
  )
}

#' @rdname rvar-dist
#' @export
quantile.rvar_factor <- function(x, probs, ...) {
  # quantile function is not defined for unordered distributions
  # generate an all-NA array of the appropriate shape (CDF has the same output)
  cdf.rvar_factor(x, probs)
}

#' @rdname rvar-dist
#' @export
quantile.rvar_ordered <- function(x, probs, ...) {
  # `type` must be in 1:3 because x is discrete
  out <- quantile(as_rvar_numeric(x), probs, type = 1, ...)
  while_preserving_dims(function(out) levels(x)[out], out)
}
