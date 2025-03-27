#' Probability integral transform
#'
#' Probability integral transform (PIT). LOO-PIT is given by a weighted sample.
#'
#' @param x (draws) A [`draws_matrix`] object or one coercible to a
#'   `draws_matrix` object, or an [`rvar`] object.
#'
#' @param y (observations) A 1D vector, or an array of dim(x), if x is `rvar`.
#'   Each element of `y` corresponds to a variable in `x`.
#'
#' @param weights A matrix of weights for each draw and variable. `weights`
#'   should have one column per variable in `x`, and `ndraws(x)` rows.
#'
#' @param log (logical) Are the weights passed already on the log scale? The
#'   default is `FALSE`, that is, expecting `weights` to be on the standard
#'   (non-log) scale.
#'
#' @template args-methods-dots
#'
#' @details The `pit()` function computes the probability integral transform of
#'   `y` using the empirical cumulative distribution computed from the samples
#'   in `x`. For continuous valued `y` and `x`, the PIT for the elements of `y`
#'   is computed as the empirical cumulative distribution value:
#'
#'     PIT(y_i) = Pr(x_i < y_i),
#'
#'   where x_i, is the corresponding set of draws in `x`. For `draws` objects,
#'   this corresponds to the draws of the *i*th variable, and for `rvar`
#'   the elements of `y` and `x` are matched.
#'
#'   The draws in `x` can further be provided (log-)weights in
#    `weights`, which enables for example the computation of LOO-PITs.
#'
#'   If `y` and `x` are discrete, randomisation is used to obtain continuous PIT
#'   values. (see, e.g., Czado, C., Gneiting, T., Held, L.: Predictive model
#'   assessment for count data.  Biometrics 65(4), 1254–1261 (2009).)
#'
#' @return A numeric vector of length `length(y)` containing the PIT values, or
#'   an array of shape `dim(y)`, if `x` is an `rvar`.

#' @examples
#' # PIT for a draws object
#' x <- example_draws()
#' # Create a vector of observations
#' y <- rnorm(nvariables(x), 5, 5)
#' pit(x, y)
#'
#' # Compute weighted PIT (for example LOO-PIT)
#' weights <- matrix(runif(length(x) * nvariables(x)), ncol = nvariables(x))
#'
#' pit(x, y, weights)
#'
#' # PIT for an rvar
#' x <- rvar(example_draws())
#' # Create an array of observations with the same dimensions as x.
#' y_arr <- array(rnorm(length(x), 5, 5), dim = dim(x))
#' pit(x, y_arr)
#'
NULL

#' @export
pit <- function(x, y, ...) UseMethod("pit")

#' @rdname pit
#' @export
pit.default <- function(x, y, weights = NULL, log = FALSE, ...) {
  x <- as_draws_matrix(x)
  if (!is.null(weights)) {
    weights <- as_draws_matrix(weights)
  }
  pit(x, y, weights, log)
}

#' @rdname pit
#' @export
pit.draws_matrix <- function(x, y, weights = NULL, log = FALSE, ...) {
  y <- validate_y(y, x)
  if (!is.null(weights)) {
    weights <- sapply(seq_len(nvariables(x)), \(var_idx) {
      validate_weights(weights[, var_idx], x[, var_idx], log)
    })
    weights <- normalize_log_weights(weights)
  }
  pit <- vapply(seq_len(ncol(x)), function(j) {
    sel_min <- x[, j] < y[j]
    if (!any(sel_min)) {
      pit <- 0
    } else {
      if (is.null(weights)) {
        pit <- mean(sel_min)
      } else {
        pit <- exp(log_sum_exp(weights[sel_min, j]))
      }
    }

    sel_sup <- x[, j] == y[j]
    if (any(sel_sup)) {
      # randomized PIT for discrete y (see, e.g., Czado, C., Gneiting, T.,
      # Held, L.: Predictive model assessment for count data.
      # Biometrics 65(4), 1254–1261 (2009).)
      if (is.null(weights)) {
        pit_sup <- pit + mean(sel_sup)
      } else {
        pit_sup <- pit + exp(log_sum_exp(weights[sel_sup, j]))
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

  setNames(pit, variables(x))
}

#' @rdname pit
#' @export
pit.rvar <- function(x, y, weights = NULL, log = FALSE, ...) {
  y <- validate_y(y, x)
  if (is.null(weights)) {
    out <- array(
      runif(length(y), Pr(x < y), Pr(x <= y)),
      dim(x),
      dimnames(x)
    )
  } else {
    out <- array(
      data = pit(
        x = as_draws_matrix(c(x)),
        y = c(y),
        weights = weights,
        log = log
      ),
      dim = dim(x),
      dimnames = dimnames(x)
    )
  }
  out
}

# internal ----------------------------------------------------------------

validate_y <- function(y, x = NULL) {
  if (!is.numeric(y)) {
    stop_no_call("`y` must be numeric.")
  }
  if (anyNA(y)) {
    stop_no_call("NAs not allowed in `y`.")
  }
  if (is_rvar(x)) {
    if (length(x) != length(y) || any(dim(y) != dim(x))) {
      stop_no_call("`dim(y)` must match `dim(x)`.")
    }
  } else if (is_draws(x)) {
    if (!is.vector(y, mode = "numeric") || length(y) != nvariables(x)) {
      stop_no_call("`y` must be a vector of length `nvariables(x)`.")
    }
  }
  y
}

normalize_log_weights <- function(log_weights) {
  apply(log_weights, 2, function(col) col - log_sum_exp(col))
}
