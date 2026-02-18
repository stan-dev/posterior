#' Probability integral transform
#'
#' Probability integral transform (PIT). LOO-PIT is given by a weighted sample.
#'
#' @name pit
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
#'   `y` using the empirical cumulative distribution computed from the draws
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
#' weights <- matrix(runif(length(x)), ncol = nvariables(x))
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

#' @rdname pit
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
    weights <- sapply(seq_len(nvariables(x)), function(var_idx) {
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

  if (any(pit > 1 + 1e-10)) {
    warning_no_call(
      paste(
        "Some PIT values larger than 1. ",
        "This is usually due to numerical inaccuracies. ",
        "Largest value: ",
        max(pit),
        "\nRounding PIT > 1 to 1.",
        sep = ""
      )
    )
  }

  setNames(pmin(1, pit), variables(x))
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

#' Pareto-smoothed probability integral transform
#'
#' Compute PIT values using the empirical CDF, then refine values in
#' the tails by fitting a generalized Pareto distribution (GPD) to
#' the tail draws. This gives smoother, more accurate PIT values in
#' the tails where the ECDF is coarse, and avoids PIT values of 0 and 1.
#' Due to use of generalized Pareto distribution CDF in tails, the
#' PIT values are not anymore rank based and continuous uniformity
#' test is appropriate.
#'
#' @name pareto_pit
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
#' @param ndraws_tail (integer) Number of tail draws to use for GPD
#'   fitting. If `NULL` (the default), computed using [ps_tail_length()].
#'
#' @template args-methods-dots
#'
#' @details The function first computes raw PIT values identically to
#'   [pit()] (including support for weighted draws). It then fits a
#'   GPD to both tails of the draws (using the same approach as
#'   [pareto_smooth()]) and replaces PIT values for observations falling in
#'   the tail regions:
#'
#'   For a right-tail observation \eqn{y_i > c_R} (where \eqn{c_R} is
#'   the right-tail cutoff):
#'
#'   \deqn{PIT(y_i) = 1 - p_{tail}(1 - F_{GPD}(y_i; c_R, \sigma_R, k_R))}
#'
#'   For a left-tail observation \eqn{y_i < c_L}:
#'
#'   \deqn{PIT(y_i) = p_{tail}(1 - F_{GPD}(-y_i; -c_L, \sigma_L, k_L))}
#'
#'   where \eqn{p_{tail}} is the proportion of (weighted) mass in the
#'   tail.
#'
#'   When (log-)weights in in `weights` are provided, they are used for
#'   the raw PIT computation (as in [pit()]) and for GPD fit.
#'
#' @return A numeric vector of length `length(y)` containing the PIT values, or
#'   an array of shape `dim(y)`, if `x` is an `rvar`.
#'
#' @seealso [pit()] for the unsmoothed version, [pareto_smooth()] for
#'   Pareto smoothing of draws.
#'
#' @examples
#' x <- example_draws()
#' y <- rnorm(nvariables(x), 5, 5)
#' pareto_pit(x, y)
#'
NULL

#' @rdname pareto_pit
#' @export
pareto_pit <- function(x, y, ...) UseMethod("pareto_pit")

#' @rdname pareto_pit
#' @export
pareto_pit.default <- function(x, y, weights = NULL, log = FALSE,
                               ndraws_tail = NULL, ...) {
  x <- as_draws_matrix(x)
  if (!is.null(weights)) {
    weights <- as_draws_matrix(weights)
  }
  pareto_pit(x, y, weights = weights, log = log,
             ndraws_tail = ndraws_tail, ...)
}

#' @rdname pareto_pit
#' @export
pareto_pit.draws_matrix <- function(x, y, weights = NULL, log = FALSE,
                                    ndraws_tail = NULL, ...) {
  y <- validate_y(y, x)

  # validate and normalize weights to log scale (same as pit.draws_matrix)
  if (!is.null(weights)) {
    weights <- sapply(seq_len(nvariables(x)), function(var_idx) {
      validate_weights(weights[, var_idx], x[, var_idx], log)
    })
    weights <- normalize_log_weights(weights)
  }

  ndraws <- ndraws(x)

  if (is.null(ndraws_tail)) {
    ndraws_tail <- ps_tail_length(ndraws, 1)
  } else {
    ndraws_tail <- as_one_integer(ndraws_tail)
  }

  # validate ndraws_tail once for all variables
  gpd_ok <- !is.na(ndraws_tail) && ndraws_tail >= 5
  if (gpd_ok && ndraws_tail > ndraws / 2) {
    ndraws_tail <- floor(ndraws / 2)
  }
  if (gpd_ok && ndraws_tail >= ndraws) {
    gpd_ok <- FALSE
  }

  # precompute tail indices (shared across all variables)
  if (gpd_ok) {
    tail_ids <- seq(ndraws - ndraws_tail + 1, ndraws)
  }

  pit_values <- vapply(seq_len(ncol(x)), function(j) {
    draws <- x[, j]

    # --- raw PIT (same logic as pit.draws_matrix) ---
    sel_min <- draws < y[j]
    if (!any(sel_min)) {
      raw_pit <- 0
    } else {
      if (is.null(weights)) {
        raw_pit <- mean(sel_min)
      } else {
        raw_pit <- exp(log_sum_exp(weights[sel_min, j]))
      }
    }

    sel_sup <- draws == y[j]
    if (any(sel_sup)) {
      if (is.null(weights)) {
        pit_sup <- raw_pit + mean(sel_sup)
      } else {
        pit_sup <- raw_pit + exp(log_sum_exp(weights[sel_sup, j]))
      }
      raw_pit <- runif(1, raw_pit, pit_sup)
    }

    # --- GPD tail refinement ---
    if (!gpd_ok || should_return_NA(draws)) {
      return(raw_pit)
    }

    # sort draws and carry weights along
    ord <- sort.int(draws, index.return = TRUE)
    sorted <- ord$x
    log_wt_sorted <- if (!is.null(weights)) weights[ord$ix, j] else NULL

    # tail proportion: sum of (normalized) weights in the tail
    if (!is.null(log_wt_sorted)) {
      tail_proportion <- exp(log_sum_exp(log_wt_sorted[tail_ids]))
    } else {
      tail_proportion <- ndraws_tail / ndraws
    }

    # --- right tail ---
    right_replaced <- FALSE
    right_tail <- sorted[tail_ids]
    if (!is_constant(right_tail)) {
      right_cutoff <- sorted[min(tail_ids) - 1]
      if (right_cutoff == right_tail[1]) {
        right_cutoff <- right_cutoff - .Machine$double.eps
      }
      right_tail_wt <- if (!is.null(log_wt_sorted)) {
        exp(log_wt_sorted[tail_ids])
      } else {
        NULL
      }
      right_fit <- gpdfit(right_tail - right_cutoff, sort_x = FALSE,
                          weights = right_tail_wt)
      if (is.finite(right_fit$k) && !is.na(right_fit$sigma)) {
        if (y[j] > right_cutoff) {
          gpd_cdf <- pgeneralized_pareto(
            y[j], mu = right_cutoff, sigma = right_fit$sigma, k = right_fit$k
          )
          raw_pit <- 1 - tail_proportion * (1 - gpd_cdf)
          right_replaced <- TRUE
        }
      }
    }

    # --- left tail (negate trick, same as ps_tail) ---
    if (!right_replaced) {
      left_draws <- -draws
      left_ord <- sort.int(left_draws, index.return = TRUE)
      left_sorted <- left_ord$x
      log_wt_left_sorted <- if (!is.null(weights)) weights[left_ord$ix, j] else NULL

      left_tail <- left_sorted[tail_ids]
      if (!is_constant(left_tail)) {
        left_cutoff <- left_sorted[min(tail_ids) - 1]
        if (left_cutoff == left_tail[1]) {
          left_cutoff <- left_cutoff - .Machine$double.eps
        }
        left_tail_wt <- if (!is.null(log_wt_left_sorted)) {
          exp(log_wt_left_sorted[tail_ids])
        } else {
          NULL
        }
        left_fit <- gpdfit(left_tail - left_cutoff, sort_x = FALSE,
                           weights = left_tail_wt)
        if (is.finite(left_fit$k) && !is.na(left_fit$sigma)) {
          if (-y[j] > left_cutoff) {
            gpd_cdf <- pgeneralized_pareto(
              -y[j], mu = left_cutoff, sigma = left_fit$sigma, k = left_fit$k
            )
            if (!is.null(log_wt_left_sorted)) {
              left_tail_proportion <- exp(log_sum_exp(log_wt_left_sorted[tail_ids]))
            } else {
              left_tail_proportion <- tail_proportion
            }
            raw_pit <- left_tail_proportion * (1 - gpd_cdf)
          }
        }
      }
    }

    raw_pit
  }, FUN.VALUE = 1.0)

  min_tail_prob <- 1/ndraws/1e4
  pit_values <- pmin(pmax(pit_values, min_tail_prob), 1-min_tail_prob)
  setNames(pit_values, variables(x))
}

#' @rdname pareto_pit
#' @export
pareto_pit.rvar <- function(x, y, weights = NULL, log = FALSE,
                            ndraws_tail = NULL, ...) {
  y <- validate_y(y, x)
  out <- array(
    data = pareto_pit(
      x = as_draws_matrix(c(x)),
      y = c(y),
      weights = weights,
      log = log,
      ndraws_tail = ndraws_tail,
      ...
    ),
    dim = dim(x),
    dimnames = dimnames(x)
  )
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
