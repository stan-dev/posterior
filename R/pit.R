#' Calculate probability integral transfrom
#'
#' The `pit()` function computes the empirical probability integral transform
#' (PIT) of a vector of values with regard to provided draws. If weigths are
#' provided for the draws are supplied, a weighted transformation is computed
#' instead.
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
pit.default <- function(x, y, lw = NULL) {
  if (!is.null(lw) && !all(is.finite(lw))) {
    stop("All weigths in `lw` need to be finite.")
  }
  vapply(seq_len(ncol(x)), function(j) {
    sel_min <- x[, j] < y[j]
    pit <- ifelse(
      is.null(lw),
      mean(sel_min),
      .exp_log_sum_exp(lw[sel_min, j])
    )
    sel_sup <- x[, j] == y[j]
    if (any(sel_sup)) {
      # randomized PIT for discrete y (see, e.g., Czado, C., Gneiting, T.,
      # Held, L.: Predictive model assessment for count data.
      # Biometrics 65(4), 1254–1261 (2009).)
      pit_sup <- pit + ifelse(
        is.null(lw),
        mean(sel_sup),
        .exp_log_sum_exp(lw[sel_sup, j])
      )
      pit <- runif(1, pit, pit_sup)
    }
    if (any(pit > 1)) {
      warning(
        cat(
          "Some PIT values larger than 1! Largest: ",
          max(pit),
          "\nRounding PIT > 1 to 1."
        )
      )
    }
    pit
  }, FUN.VALUE = 1)
}

# internal ----------------------------------------------------------------

.exp_log_sum_exp <- function(x) {
  m <- max(x)
  exp(m + log(sum(exp(x - m))))
}