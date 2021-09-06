# Summaries over array elements, within draws ---------------------------------------------------------

#' Summaries of random variables over array elements, within draws
#'
#' Compute summaries of random variables over array elements and within draws,
#' producing a new random variable of length 1 (except in the case of
#' `rvar_range()`, see **Details**).
#'
#' @param ... (rvar) One or more [`rvar`]s.
#' @template args-rvar-summaries-na.rm
#' @param constant (scalar real) For `rvar_mad()`, a scale factor for computing
#'   the median absolute deviation. See the details of `stats::mad()` for the
#'   justification for the default value.
#' @param probs (numeric vector) For `rvar_quantile()`, probabilities in `[0, 1]`.
#' @param names (logical) For `rvar_quantile()`, if `TRUE`, the result has a
#'   `names` attribute.
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
#' An [`rvar`] of length 1 (for `range()`, length 2; for `quantile()`, length
#' equal to `length(probs)`) with the same number
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
#' rvar_quantile(x, probs = c(0.25, 0.5, 0.75), names = TRUE)
#'
#' @seealso [rvar-summaries-over-draws] for summary functions across draws (e.g. expectations).
#' [rvar-dist] for density, CDF, and quantile functions of random variables.
#' @family rvar-summaries
#' @name rvar-summaries-within-draws
#' @export
rvar_mean <- function(..., na.rm = FALSE) {
  summarise_rvar_within_draws_via_matrix(c(...), matrixStats::rowMeans2, na.rm = na.rm)
}

# numeric summaries -------------------------------------------------------

#' @rdname rvar-summaries-within-draws
#' @export
rvar_median <- function(..., na.rm = FALSE) {
  summarise_rvar_within_draws_via_matrix(c(...), matrixStats::rowMedians, na.rm = na.rm)
}

#' @rdname rvar-summaries-within-draws
#' @export
rvar_sum <- function(..., na.rm = FALSE) {
  summarise_rvar_within_draws_via_matrix(c(...), matrixStats::rowSums2, na.rm = na.rm)
}

#' @rdname rvar-summaries-within-draws
#' @export
rvar_prod <- function(..., na.rm = FALSE) {
  summarise_rvar_within_draws_via_matrix(c(...), matrixStats::rowProds, na.rm = na.rm)
}

#' @rdname rvar-summaries-within-draws
#' @export
rvar_min <- function(..., na.rm = FALSE) {
  summarise_rvar_within_draws_via_matrix(c(...), matrixStats::rowMins, na.rm = na.rm)
}

#' @rdname rvar-summaries-within-draws
#' @export
rvar_max <- function(..., na.rm = FALSE) {
  summarise_rvar_within_draws_via_matrix(c(...), matrixStats::rowMaxs, na.rm = na.rm)
}


# spread ------------------------------------------------------------------

#' @rdname rvar-summaries-within-draws
#' @export
rvar_sd <- function(..., na.rm = FALSE) {
  summarise_rvar_within_draws_via_matrix(c(...), matrixStats::rowSds, na.rm = na.rm)
}

#' @rdname rvar-summaries-within-draws
#' @export
rvar_var <- function(..., na.rm = FALSE) {
  summarise_rvar_within_draws_via_matrix(c(...), matrixStats::rowVars, na.rm = na.rm)
}

#' @rdname rvar-summaries-within-draws
#' @export
rvar_mad <- function(..., constant = 1.4826, na.rm = FALSE) {
  summarise_rvar_within_draws_via_matrix(c(...), matrixStats::rowMads, constant = constant, na.rm = na.rm)
}


# range -------------------------------------------------------------------

#' @rdname rvar-summaries-within-draws
#' @export
rvar_range <- function(..., na.rm = FALSE) {
  summarise_rvar_within_draws_via_matrix(c(...), matrixStats::rowRanges, na.rm = na.rm)
}


# quantiles ---------------------------------------------------------------

#' @rdname rvar-summaries-within-draws
#' @export
rvar_quantile <- function(..., probs, names = FALSE, na.rm = FALSE) {
  names <- as_one_logical(names)
  na.rm <- as_one_logical(na.rm)

  out <- summarise_rvar_within_draws_via_matrix(
    c(...), matrixStats::rowQuantiles, probs = probs, na.rm = na.rm, drop = FALSE
  )

  if (!names) {
    dimnames(out) <- NULL
  }

  out
}


# logical summaries -------------------------------------------------------

#' @rdname rvar-summaries-within-draws
#' @export
rvar_all <- function(..., na.rm = FALSE) {
  summarise_rvar_within_draws_via_matrix(c(...), matrixStats::rowAlls, na.rm = na.rm)
}

#' @rdname rvar-summaries-within-draws
#' @export
rvar_any <- function(..., na.rm = FALSE) {
  summarise_rvar_within_draws_via_matrix(c(...), matrixStats::rowAnys, na.rm = na.rm)
}


# special value predicates ------------------------------------------------

#' Special value predicates for random variables
#'
#' Compute special value predicates (checking for finite / infinite values, `NaN`, and `NA`)
#' on all draws within a random variable, returning a random variable.
#'
#' @param x (rvar) An [`rvar`].
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
