# Summaries over array elements, within draws ---------------------------------------------------------

#' Summaries of random variables over array elements, within draws
#'
#' Compute summaries of random variables within draws, producing a new random variable.
#'
#' @param x an [`rvar`]
#' @param na.rm Should `NA` values in the random variable be removed before
#' computing summaries?
#' @param ... further arguments passed to underlying functions (e.g., `base::mean()`
#' or `base::median()`).
#'
#' @details
#'
#' These functions compute statistics within each draw of the random variable.
#' For summaries over draws (such as expectations), see [rvar-summaries].
#'
#' Besides `rvar_mean()` and `rvar_median()`, these standard generics are supported:
#'
#' - `all()`, `any()`
#' - `sum()`, `prod()`
#' - `min()`, `max()`
#' - `range()`
#'
#' @return
#' An [`rvar`] of length 1 (or in the case of `range()`, length 2) with the same number
#' of draws as the input rvar(s) containing the summary statistic computed within
#' each draw of the input rvar(s).
#'
#' @examples
#'
#' set.seed(5678)
#' x = rdo(rnorm(4, mean = 1:4, sd = 2))
#'
#' # These will give similar results to mean(1:4),
#' # median(1:4), sum(1:4), prod(1:4), etc
#' rvar_mean(x)
#' rvar_median(x)
#' sum(x)
#' prod(x)
#'
#' @seealso [rvar-summaries] for summary functions across draws (e.g. expectations).
#' [rvar-functions] for density, CDF, and quantile functions of random variables.
#' @name rvar-summaries-by-draw
#' @export
Summary.rvar <- function(..., na.rm = FALSE) {
  f <- get(.Generic)
  .Summary.rvar(f, ..., na.rm = na.rm)
}

#' @rdname rvar-summaries-by-draw
#' @export
range.rvar <- function(..., na.rm = FALSE) {
  .Summary.rvar(base::range, ..., na.rm = na.rm, transpose = TRUE)
}

.Summary.rvar <- function(f, ..., na.rm = FALSE, transpose = FALSE) {
  rvars <- lapply(list(...), function(arg) {
    arg <- as_rvar(arg)
    dim(arg) <- prod(dim(arg))
    arg
  })
  rvars <- conform_rvar_nchains(rvars)

  # bind all args into a single matrix of draws to perform the summary over
  all_draws <- draws_of(do.call(c, rvars))

  # perform summary
  .draws <- apply(all_draws, 1, f, na.rm = na.rm)

  if (transpose) {
    .draws <- t(.draws)
  }
  new_rvar(.draws, .nchains = nchains(rvars[[1]]))
}

#' @rdname rvar-summaries-by-draw
#' @export
rvar_mean <- function(x, ...) summarise_rvar_within_draws(x, mean, ...)

#' @rdname rvar-summaries-by-draw
#' @export
rvar_median <- function(x, ...) summarise_rvar_within_draws(x, median, ...)


#' @export
anyNA.rvar <- function(x, ...) anyNA(draws_of(x, ...))

#' @export
is.finite.rvar <- function(x, ...) rvar_apply_vec_fun(is.finite, x, ...)
#' @export
is.infinite.rvar <- function(x, ...) rvar_apply_vec_fun(is.infinite, x, ...)
#' @export
is.nan.rvar <- function(x, ...) rvar_apply_vec_fun(is.nan, x, ...)
#' @export
is.na.rvar <- function(x, ...) summarise_rvar_by_element(x, function(x) anyNA(x))

