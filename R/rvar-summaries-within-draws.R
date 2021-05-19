# Summaries over array elements, within draws ---------------------------------------------------------

#' Summaries of random variables over array elements, within draws
#'
#' Compute summaries of random variables over array elements and within draws,
#' producing a new random variable of length 1 (except in the case of
#' `rvar_range()`, see 'Details').
#'
#' @param ... [`rvar`]s
#' @template args-rvar-summaries-na.rm
#' @param constant For `rvar_mad()`, a scale factor for computing the median
#' absolute deviation. See the details of `stats::mad()` for the justification
#' for the default value.
#' @param low For `rvar_mad()`, if `TRUE`, compute the 'lo-median', i.e., for
#' even sample size, do not average the two middle values, but take the smaller
#' one. See `stats::mad()`.
#' @param high For `rvar_mad()`, if `TRUE`, compute the 'hi-median', i.e., take
#' the larger of the two middle values for even sample size. See `stats::mad()`.
#'
#' @details
#'
#' These functions compute statistics within each draw of the random variable.
#' For summaries over draws (such as expectations), see [rvar-summaries-over-draws].
#'
#' Each function defined here corresponds to the base function of the same name
#' without the `rvar_` prefix (e.g., `rvar_mean()` calls `mean()` under the hood, etc).
#'
#' @return
#' An [`rvar`] of length 1 (or in the case of `range()`, length 2) with the same number
#' of draws as the input rvar(s) containing the summary statistic computed within
#' each draw of the input rvar(s).
#'
#' @examples
#'
#' set.seed(5678)
#' x = rvar_rng(rnorm, 4, mean = 1:4, sd = 2)
#'
#' # These will give similar results to mean(1:4),
#' # median(1:4), sum(1:4), prod(1:4), etc
#' rvar_mean(x)
#' rvar_median(x)
#' rvar_sum(x)
#' rvar_prod(x)
#' rvar_range(x)
#'
#' @seealso [rvar-summaries-over-draws] for summary functions across draws (e.g. expectations).
#' [rvar-functions] for density, CDF, and quantile functions of random variables.
#' @family rvar-summaries
#' @name rvar-summaries-within-draws
#' @export
rvar_mean <- function(..., na.rm = FALSE) summarise_rvar_within_draws(c(...), mean, na.rm = na.rm,
  .when_empty = stop_no_call("in rvar_mean(): cannot take mean of empty vector")
)

# numeric summaries -------------------------------------------------------

#' @rdname rvar-summaries-within-draws
#' @export
rvar_median <- function(..., na.rm = FALSE) summarise_rvar_within_draws(c(...), median, na.rm = na.rm,
  .when_empty = stop_no_call("in rvar_median(): cannot take median of empty vector")
)

#' @rdname rvar-summaries-within-draws
#' @export
rvar_sum <- function(..., na.rm = FALSE) summarise_rvar_within_draws(c(...), sum, na.rm = na.rm)

#' @rdname rvar-summaries-within-draws
#' @export
rvar_prod <- function(..., na.rm = FALSE) summarise_rvar_within_draws(c(...), prod, na.rm = na.rm)

#' @rdname rvar-summaries-within-draws
#' @export
rvar_min <- function(..., na.rm = FALSE) summarise_rvar_within_draws(c(...), min, na.rm = na.rm)

#' @rdname rvar-summaries-within-draws
#' @export
rvar_max <- function(..., na.rm = FALSE) summarise_rvar_within_draws(c(...), max, na.rm = na.rm)


# spread ------------------------------------------------------------------

#' @rdname rvar-summaries-within-draws
#' @export
rvar_sd <- function(..., na.rm = FALSE) summarise_rvar_within_draws(c(...), sd, na.rm = na.rm,
  .when_empty = stop_no_call("in rvar_sd(): cannot take sd of empty vector")
)

#' @rdname rvar-summaries-within-draws
#' @export
rvar_var <- function(..., na.rm = FALSE) {
  # var() is silly and gives the covariance matrix instead of the variance
  # when dim(x) == 2, so convert to a vector to avoid this
  summarise_rvar_within_draws(c(...), function(x) var(as.vector(x), na.rm = na.rm),
    .when_empty = stop_no_call("in rvar_var(): cannot take variance of empty vector")
  )
}

#' @rdname rvar-summaries-within-draws
#' @export
rvar_mad <- function(..., constant = 1.4826, na.rm = FALSE, low = FALSE, high = FALSE) {
  summarise_rvar_within_draws(c(...), mad, constant = constant, na.rm = na.rm, low = low, high = high,
    .when_empty = stop_no_call("in rvar_mad(): cannot take mad of empty vector")
  )
}


# range -------------------------------------------------------------------

#' @rdname rvar-summaries-within-draws
#' @export
rvar_range <- function(..., na.rm = FALSE) summarise_rvar_within_draws(c(...), range, na.rm = na.rm, .transpose = TRUE)


# logical summaries -------------------------------------------------------

#' @rdname rvar-summaries-within-draws
#' @export
rvar_all <- function(..., na.rm = FALSE) summarise_rvar_within_draws(c(...), all, na.rm = na.rm)

#' @rdname rvar-summaries-within-draws
#' @export
rvar_any <- function(..., na.rm = FALSE) summarise_rvar_within_draws(c(...), any, na.rm = na.rm)


# special value predicates ------------------------------------------------

#' Special value predicates for random variables
#'
#' Compute special value predicates (checking for finite / infinite values, `NaN`, and `NA`)
#' on all draws within a random variable, returning a random variable.
#'
#' @param x An [`rvar`]
#'
#' @details
#'
#' These functions return a new [`rvar`] that is the result of applying
#' `is.finite()`, `is.infinite()`, `is.nan()`, or `is.na()` to every draw
#' in the input random variable.
#'
#' @return
#' A logical [`rvar`] of the same length as the input.
#'
#' @examples
#'
#' x <- rvar(c(1, Inf, -Inf, NaN, NA))
#' x
#'
#' rvar_is_finite(x)
#' rvar_is_infinite(x)
#' rvar_is_nan(x)
#' rvar_is_na(x)
#'
#' @seealso [rvar-summaries-over-draws] for summary functions across draws, including
#' implementations of `is.finite()`, `is.infinite()`, `is.nan()`, and `is.na()` for
#' `rvar`s.
#' @family rvar-summaries
#' @name rvar_is_finite
#' @export
rvar_is_finite <- function(x) rvar_apply_vec_fun(is.finite, x)

#' @rdname rvar_is_finite
#' @export
rvar_is_infinite <- function(x) rvar_apply_vec_fun(is.infinite, x)

#' @rdname rvar_is_finite
#' @export
rvar_is_nan <- function(x) rvar_apply_vec_fun(is.nan, x)

#' @rdname rvar_is_finite
#' @export
rvar_is_na <- function(x) rvar_apply_vec_fun(is.na, x)
