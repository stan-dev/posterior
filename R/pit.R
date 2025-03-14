#' Probability integral transfrom
#'
#' The `pit()` function computes the empirical probability integral transform
#' (PIT) of a vector of values with regard to provided draws. If weigths are
#' provided for the draws, a weighted transformation is computed instead.
#'
#' @family diagnostics
#' @param x (draws) A [`draws_df`] object or one coercible to a `draws_df`
#' object.

#' @param y (observations) A 1D vector of observations. Each element of `y`
#' corresponds to a column in `x`.

#' @param lw (log-wieghts) A [`draws_df`] object or one coercible to a
#' `draws_df` object. `lw`` should have the same dimensions as `x`.

#' @template args-conv-split
#'
#' @details The `pit()` function computes the probability integral transform of
#'   `y` using the empirical cumulative distribution computed from the samples
#'   of `x`. These samples can further be provided a log-weights in `lw`, which
#'   enables for example the computation of LOO-PITs.
#'
#'   If `y` and `x` are discrete, randomisation is used to obtain continuous PIT
#'   values. (see, e.g., Czado, C., Gneiting, T., Held, L.: Predictive model
#'   assessment for count data.  Biometrics 65(4), 1254–1261 (2009).)
#'
#' @return A numeric vector of length `length(y)` containing the PIT values.
#'
#' @examples
#' @export
pit <- function(x, ...) UseMethod("pit")

#' @rdname pit
#' @export
pit.default <- function(x, y, loo_weights = NULL, log = TRUE) {
  if (!is.null(loo_weights)) {
    loo_weights <- validate_loo_weights(loo_weights, x, log)
  }
  pit <- vapply(seq_len(ncol(x)), function(j) {
    sel_min <- x[, j] < y[j]
    if (is.null(loo_weights)) {
      pit <- mean(sel_min)
    } else {
      pit <- exp(log_sum_exp(loo_weights[sel_min, j]))
    }

    sel_sup <- x[, j] == y[j]
    if (any(sel_sup)) {
      # randomized PIT for discrete y (see, e.g., Czado, C., Gneiting, T.,
      # Held, L.: Predictive model assessment for count data.
      # Biometrics 65(4), 1254–1261 (2009).)
      if (is.null(loo_weights)) {
        pit_sup <- pit + mean(sel_sup)
      } else {
        pit_sup <- pit + exp(log_sum_exp(loo_weights[sel_sup, j]))
      }

      pit <- runif(1, pit, pit_sup)
    }
    pit
  }, FUN.VALUE = 1.0)

  if (any(pit > 1)) {
    warning(
      cat(
        "Some PIT values larger than 1!",
        "This is usually due to numerical inaccuracies.",
        "Largest value:",
        max(pit),
        "\nRounding PIT > 1 to 1."
      )
    )
  }

  pit
}

# internal ----------------------------------------------------------------

# validate weights and return log weights
validate_loo_weights <- function(loo_weights, draws, log = TRUE) {
  checkmate::assert_numeric(loo_weights, any.missing = FALSE)
  checkmate::assert_flag(log)
  if (dim(loo_weights) != dim(draws)) {
    stop_no_call("Dimension of `loo_weights` must match that of `x`.")
  }
  if (!all(is.finite(loo_weights))) {
    stop_no_call("All weigths in `loo_weights` must be finite.")
  }
  if (!log) {
    if (any(loo_weights < 0)) {
      stop_no_call("`loo-weights` must be non-negative.")
    }
    loo_weights <- log(loo_weights)
  }
  loo_weights
}
