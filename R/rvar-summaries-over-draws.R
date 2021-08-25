# Summaries within array elements, over draws  --------------------------

#' Summaries of random variables within array elements, over draws
#'
#' Compute summaries within elements of an [`rvar`] and over draws of each element,
#' producing an array of the same shape as the input random variable (except in
#' the case of `range()`, see **Details**).
#'
#' @param x (rvar) An [`rvar`].
#' @param ... Further arguments passed to underlying functions (e.g.,
#'   `base::mean()` or `base::median()`), such as `na.rm`.
#'
#' @details
#'
#' Summaries include expectations (`E()` or `mean()`), probabilities (`Pr()`),
#' medians (`median()`), spread (`var()`, `variance()`, `sd()`, `mad()`), sums and
#' products (`sum()`, `prod()`), extrema and ranges (`min()`, `max()`, `range()`),
#' logical summaries (`all()`, `any()`), and special value predicates (`is.finite()`,
#' `is.infinite()`, `is.nan()`, `is.na()`).
#'
#' Unless otherwise stated, these functions return a numeric array with the same shape
#' (same dimensions) as the input [`rvar`], `x`.
#'
#' `range(x)` returns an array with dimensions `c(2, dim(x))`, where the last
#' dimension contains the minimum and maximum values.
#'
#' `is.infinite(x)`, `is.nan(x)`, and `is.na(x)` return logical arrays, where each
#' element is `TRUE` if **any** draws in its corresponding element in `x` match
#' the predicate. Each elements in the result of `is.finite(x)` is `TRUE` if
#' **all** draws in the corresponding element in `x` are finite.
#'
#' Both `E()`, `mean()`, and `Pr()` return the means of each element in the input.
#' `Pr()` additionally checks that the provided [`rvar`]
#' is a logical variable (hence, taking its expectation results in a probability).
#'
#' For consistency, `E()` and `Pr()` are also defined for base arrays so that
#' they can be used as summary functions in `summarise_draws()`.
#'
#' @return
#' A numeric or logical vector with the same dimensions as the given random variable, where
#' each entry in the vector is the mean, median, or variance of the corresponding entry in `x`.
#'
#' @examples
#'
#' set.seed(5678)
#' x = rvar_rng(rnorm, 4, mean = 1:4, sd = 2)
#'
#' # These should all be ~= c(1, 2, 3, 4)
#' E(x)
#' mean(x)
#' median(x)
#'
#' # This ...
#' Pr(x < 1.5)
#' # ... should be about the same as this:
#' pnorm(1.5, mean = 1:4, sd = 2)
#'
#' @name rvar-summaries-over-draws
#' @seealso [rvar-summaries-within-draws] for summary functions within draws.
#' [rvar-dist] for density, CDF, and quantile functions of random variables.
#' @family rvar-summaries
#' @export
E <- function(x, ...) {
  mean(x, ...)
}

#' @rdname rvar-summaries-over-draws
#' @export
mean.rvar <- function(x, ...) {
  summarise_rvar_by_element_via_matrix(x, matrixStats::colMeans2, useNames = FALSE, ...)
}

#' @rdname rvar-summaries-over-draws
#' @export
Pr <- function(x, ...) UseMethod("Pr")
#' @rdname rvar-summaries-over-draws
#' @export
Pr.default <- function(x, ...) {
  stop_no_call("Can only use `Pr()` on logical variables.")
}
#' @rdname rvar-summaries-over-draws
#' @export
Pr.logical <- function(x, ...) {
  mean(x, ...)
}
#' @rdname rvar-summaries-over-draws
#' @export
Pr.rvar <- function(x, ...) {
  if (!is.logical(draws_of(x))) {
    stop_no_call("Can only use `Pr()` on logical random variables.")
  }
  mean(x, ...)
}


# numeric summaries -------------------------------------------------------

#' @rdname rvar-summaries-over-draws
#' @export
median.rvar <- function(x, ...) {
  summarise_rvar_by_element_via_matrix(x, matrixStats::colMedians, useNames = FALSE, ...)
}

#' @rdname rvar-summaries-over-draws
#' @export
min.rvar <- function(x, ...) {
  summarise_rvar_by_element_via_matrix(x, matrixStats::colMins, useNames = FALSE, ...)
}

#' @rdname rvar-summaries-over-draws
#' @export
max.rvar <- function(x, ...) {
  summarise_rvar_by_element_via_matrix(x, matrixStats::colMaxs, useNames = FALSE, ...)
}

#' @rdname rvar-summaries-over-draws
#' @export
sum.rvar <- function(x, ...) {
  summarise_rvar_by_element_via_matrix(x, matrixStats::colSums2, useNames = FALSE, ...)
}

#' @rdname rvar-summaries-over-draws
#' @export
prod.rvar <- function(x, ...) {
  summarise_rvar_by_element_via_matrix(x, matrixStats::colProds, useNames = FALSE, ...)
}

#' @rdname rvar-summaries-over-draws
#' @export
all.rvar <- function(x, ...) {
  summarise_rvar_by_element_via_matrix(x, matrixStats::colAlls, useNames = FALSE, ...)
}

#' @rdname rvar-summaries-over-draws
#' @export
any.rvar <- function(x, ...) {
  summarise_rvar_by_element_via_matrix(x, matrixStats::colAnys, useNames = FALSE, ...)
}

#' @rdname rvar-summaries-over-draws
#' @export
Summary.rvar <- function(...) {
  # min, max, sum, prod, all, any, range --- these are all defined by more
  # specific functions to be faster, but I left the generic implementation here
  # on the off chance anything gets added to this group generic in the future
  f <- get(.Generic)
  summarise_rvar_by_element(.f = f, ...)
}


# spread ------------------------------------------------------------------

#' @importFrom distributional variance
#' @export
distributional::variance
#' @rdname rvar-summaries-over-draws
#' @export
variance.rvar <- function(x, ...) {
  summarise_rvar_by_element_via_matrix(x, matrixStats::colVars, useNames = FALSE, ...)
}

#' @rdname rvar-summaries-over-draws
#' @export
var <- function(x, ...) UseMethod("var")
#' @rdname rvar-summaries-over-draws
#' @export
var.default <- function(x, ...) stats::var(x, ...)
#' @rdname rvar-summaries-over-draws
#' @export
var.rvar <- variance.rvar

#' @rdname rvar-summaries-over-draws
#' @export
sd <- function(x, ...) UseMethod("sd")
#' @rdname rvar-summaries-over-draws
#' @export
sd.default <- function(x, ...) stats::sd(x, ...)
#' @rdname rvar-summaries-over-draws
#' @export
sd.rvar <- function(x, ...) {
  summarise_rvar_by_element_via_matrix(x, matrixStats::colSds, useNames = FALSE, ...)
}

#' @rdname rvar-summaries-over-draws
#' @export
mad <- function(x, ...) UseMethod("mad")
#' @rdname rvar-summaries-over-draws
#' @export
mad.default <- function(x, ...) stats::mad(x, ...)
#' @rdname rvar-summaries-over-draws
#' @export
mad.rvar <- function(x, ...) {
  summarise_rvar_by_element_via_matrix(x, matrixStats::colMads, useNames = FALSE, ...)
}


# range -------------------------------------------------------------------

#' @rdname rvar-summaries-over-draws
#' @export
range.rvar <- function(x, ...) {
  summarise_rvar_by_element_via_matrix(x, function(...) t(matrixStats::colRanges(...)),
    useNames = FALSE, .extra_dim = 2, .extra_dimnames = list(NULL), ...
  )
}


# special value predicates ---------------------------------------------------------------

#' @rdname rvar-summaries-over-draws
#' @export
is.finite.rvar <- function(x) {
  summarise_rvar_by_element_via_matrix(x, function(x) matrixStats::colAlls(is.finite(x), useNames = FALSE))
}

#' @rdname rvar-summaries-over-draws
#' @export
is.infinite.rvar <- function(x) {
  summarise_rvar_by_element_via_matrix(x, function(x) matrixStats::colAnys(is.infinite(x), useNames = FALSE))
}

#' @rdname rvar-summaries-over-draws
#' @export
is.nan.rvar <- function(x) {
  summarise_rvar_by_element_via_matrix(x, function(x) matrixStats::colAnys(is.nan(x), useNames = FALSE))
}

#' @rdname rvar-summaries-over-draws
#' @export
is.na.rvar <- function(x) {
  summarise_rvar_by_element_via_matrix(x, matrixStats::colAnyNAs, useNames = FALSE)
}

#' @export
anyNA.rvar <- function(x, ...) anyNA(draws_of(x), ...)
