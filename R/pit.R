#' Probability integral transform
#'
#' The probability integral transform (PIT). LOO-PIT is achieved with weighted sample.
#'
#' @param x (draws) A [`draws_matrix`] object or one coercible to a
#' `draws_matrix` object.

#' @param y (observations) A 1D vector of observations. Each element of `y`
#' corresponds to a column in `x`.

#' @param loo_weights A [`draws_matrix`] object or one coercible to a
#' `draws_matrix` object. `loo_weights`` should have the same dimensions as `x`.

#' @param log Are the weights provided in `loo_weights` log-weights.

#' @details The `pit()` function computes the probability integral transform of
#'   `y` using the empirical cumulative distribution computed from the samples
#'   of `x`. These samples can further be provided (log-)weights in
#    `loo_weights`, which enables for example the computation of LOO-PITs.
#'
#'   If `y` and `x` are discrete, randomisation is used to obtain continuous PIT
#'   values. (see, e.g., Czado, C., Gneiting, T., Held, L.: Predictive model
#'   assessment for count data.  Biometrics 65(4), 1254–1261 (2009).)
#'
#' @return A numeric vector of length `length(y)` containing the PIT values.
#'
#' @export
pit <- function(x, y, loo_weights = NULL, log = TRUE) UseMethod("pit")

#' @rdname pit
#' @export
pit.default <- function(x, y, loo_weights = NULL, log = TRUE) {
  x <- as_draws_matrix(x)
  if (!is.null(loo_weights)) {
    loo_weights <- as_draws_matrix(loo_weights)
  }
  pit(x, y, loo_weights, log)
}

#' @rdname pit
#' @export
pit.draws_matrix <- function(x, y, loo_weights = NULL, log = TRUE) {
  if (length(y) != ncol(x)) {
    stop_no_call("Length of `y` must match number of columns in `x`")
  }
  if (!is.null(loo_weights)) {
    loo_weights <- validate_loo_weights(loo_weights, x, log)
    loo_weights <- normalize_log_weights(loo_weights)
  }
  pit <- vapply(seq_len(ncol(x)), function(j) {
    sel_min <- x[, j] < y[j]
    if (!any(sel_min)) {
      pit <- 0
    } else {
      if (is.null(loo_weights)) {
        pit <- mean(sel_min)
      } else {
        pit <- exp(log_sum_exp(loo_weights[sel_min, j]))
      }
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
    warning_no_call(
      paste(
        "Some PIT values larger than 1! ",
        "This is usually due to numerical inaccuracies. ",
        "Largest value: ",
        max(pit),
        "\nRounding PIT > 1 to 1.",
        sep = ""
      )
    )
    pit <- pmin(1, pit)
  }

  pit
}

#' @rdname pit
#' @export
pit.rvar <- function(x, y, loo_weights = NULL, log = TRUE) {
  if (any(dim(y) != dim(x))) {
    stop_no_call("Shape of `y` must match that of `x`.")
  }
  if (is.null(loo_weights)) {
    pit <- array(
      runif(length(y), Pr(x < y), Pr(x <= y)),
      dim(x),
      dimnames(x)
    )
  } else {
    pit <- array(
      pit(
        as_draws_matrix(c(x)),
        y,
        as_draws_matrix(c(loo_weights)),
        log
      ),
      dim(x),
      dimnames(x)
    )
  }

  pit
}

# internal ----------------------------------------------------------------

# validate weights and return log weights
validate_loo_weights <- function(loo_weights, draws, log = TRUE) {
  checkmate::assert_numeric(loo_weights, any.missing = FALSE)
  checkmate::assert_flag(log)
  if (!all(dim(loo_weights) == dim(draws))) {
    stop_no_call("Dimension of `loo_weights` must match that of `x`.")
  }
  if (!all(is.finite(loo_weights))) {
    stop_no_call("All weights in `loo_weights` must be finite.")
  }
  if (!log) {
    if (any(loo_weights < 0)) {
      stop_no_call("`loo-weights` must be non-negative when log = FALSE.")
    }
    loo_weights <- log(loo_weights)
  }
  loo_weights
}

normalize_log_weights <- function(log_weights) {
  apply(log_weights, 2, function(col) col - log_sum_exp(col))
}
