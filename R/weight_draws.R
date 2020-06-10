#' Weight Draws Objects
#'
#' Add weights to [`draws`] objects, with one weight per draw, for use in
#' subsequent weighting operations. For reasons of numerical accuracy, weights
#' are stored in the form of unnormalized log-weights (in a variable
#' called `.log_weight`). See [`weights.draws`] for details how to extract weights
#' from `draws` objects.
#'
#' @template args-methods-x
#' @param weights A vector of weights of length equal to the number of
#'   draws in `x` (as obtained by method [`ndraws`]). Weights will be
#'   internally stored on the log scale (in a variable called `.log_weight`)
#'   and will not be normalized, but normalized (non-log) weights can be returned
#'   via the [`weights.draws`] method later.
#' @param log Logical. Indicates if the weights passed via `weights` are on the
#'   log scale already. Defaults to `FALSE`, that is, expecting `weights` to be on
#'   the standard (non-log) scale.
#' @template args-methods-dots
#' @template return-draws
#'
#' @seealso [`weights.draws`], [`resample_draws`]
#'
#' @examples
#' x <- example_draws()
#'
#' # sample some random weights for illustration
#' weights <- rexp(ndraws(x))
#'
#' # add weights
#' x <- weight_draws(x, weights = weights)
#' # extract weights
#' head(weights(x))
#'
#' # add weights which are already on the log scale
#' log_weights <- log(weights)
#' # extract weights
#' x <- weight_draws(x, weights = weights, log = TRUE)
#' head(weights(x))
#'
#' @export
weight_draws <- function(x, weights, ...) {
  UseMethod("weight_draws")
}

#' @rdname weight_draws
#' @export
weight_draws.draws_matrix <- function(x, weights, log = FALSE, ...) {
  log <- as_one_logical(log)
  log_weights <- validate_weights(weights, x, log = log)
  if (".log_weight" %in% colnames(x)) {
    # overwrite existing weights
    x[, ".log_weight"] <- log_weights
  } else {
    # add weights as a new column
    log_weights <- draws_matrix(.log_weight = log_weights)
    x <- bind_draws(x, log_weights)
  }
  x
}

#' @rdname weight_draws
#' @export
weight_draws.draws_array <- function(x, weights, log = FALSE, ...) {
  log <- as_one_logical(log)
  log_weights <- validate_weights(weights, x, log = log)
  if (".log_weight" %in% dimnames(x)[[3]]) {
    # overwrite existing weights
    x[, , ".log_weight"] <- log_weights
  } else {
    # add weights as a new column
    log_weights <- draws_array(.log_weight = log_weights, .nchains = nchains(x))
    x <- bind_draws(x, log_weights)
  }
  x
}

#' @rdname weight_draws
#' @export
weight_draws.draws_df <- function(x, weights, log = FALSE, ...) {
  log <- as_one_logical(log)
  log_weights <- validate_weights(weights, x, log = log)
  x$.log_weight <- log_weights
  x
}

#' @rdname weight_draws
#' @export
weight_draws.draws_list <- function(x, weights, log = FALSE, ...) {
  log <- as_one_logical(log)
  log_weights <- validate_weights(weights, x, log = log)
  niterations <- niterations(x)
  for (i in seq_len(nchains(x))) {
    sel <- (1 + (i - 1) * niterations):(i * niterations)
    x[[i]]$.log_weight <- log_weights[sel]
  }
  x
}

#' Extract Weights from Draws Objects
#'
#' Extract weights from [`draws`] objects, with one weight per draw.
#' See [`weight_draws`] for details how to add weights to [`draws`] objects.
#'
#' @param object A [`draws`] object.
#' @param log Logical. Indicates if weights should be returned on the log scale.
#'   Defaults to `FALSE`.
#' @param normalize Logical. Indicates if weights should be normalized
#'   in order to sum to 1 on the standard scale. Defaults to `TRUE`.
#' @template args-methods-dots
#'
#' @return A vector of weights, with one weight per draw.
#'
#' @seealso [`weight_draws`], [`resample_draws`]
#'
#' @examples
#' x <- example_draws()
#'
#' # sample some random weights for illustration
#' weights <- rexp(ndraws(x))
#'
#' # add weights
#' x <- weight_draws(x, weights = weights)
#'
#' # return normalized weights
#' weights <- weights(x)
#' head(weights)
#'
#' # return normalized log weights
#' log_weights <- weights(x, log = TRUE)
#' head(log_weights)
#'
#' @export
weights.draws <- function(object, log = FALSE, normalize = TRUE, ...) {
  log <- as_one_logical(log)
  normalize <- as_one_logical(normalize)
  if (!".log_weight" %in% variables(object)) {
    stop2("No weights found in the draws object. ",
          "You can add weights via 'weight_draws'.")
  }
  out <- extract_variable(object, ".log_weight")
  if (normalize) {
    out <- out - log_sum_exp(out)
  }
  if (!log) {
    out <- exp(out)
  }
  out
}

# validate weights and return log weights
validate_weights <- function(weights, draws, log = FALSE) {
  checkmate::expect_numeric(weights)
  checkmate::expect_flag(log)
  if (length(weights) != ndraws(draws)) {
    stop2("Number of weights must match the number of draws.")
  }
  if (!log) {
    if (any(weights < 0)) {
      stop2("Weights needs to be non-negative.")
    }
    weights <- log(weights)
  }
  weights
}
