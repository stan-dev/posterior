#' Weight `draws` objects
#'
#' Add weights to [`draws`] objects, with one weight per draw, for use in
#' subsequent weighting operations. For reasons of numerical accuracy, weights
#' are stored in the form of unnormalized log-weights (in a variable called
#' `.log_weight`). See [weights.draws()] for details how to extract weights from
#' `draws` objects.
#'
#' @template args-methods-x
#' @param weights (numeric vector) A vector of weights of length `ndraws(x)`.
#'   Weights will be internally stored on the log scale (in a variable called
#'   `.log_weight`) and will not be normalized, but normalized (non-log) weights
#'   can be returned via the [weights.draws()] method later.
#' @param log (logical) Are the weights passed already on the log scale? The
#'   default is `FALSE`, that is, expecting `weights` to be on the standard
#'   (non-log) scale.
#' @param pareto_smooth (logical) Should the weights be Pareto-smoothed?
#' The default is `FALSE`.
#' @template args-methods-dots
#' @template return-draws
#'
#' @seealso [weights.draws()], [resample_draws()]
#'
#' @examples
#' x <- example_draws()
#'
#' # sample some random weights for illustration
#' wts <- rexp(ndraws(x))
#' head(wts)
#'
#' # add weights
#' x <- weight_draws(x, weights = wts)
#'
#' # extract weights
#' head(weights(x)) # defaults to normalized weights
#' head(weights(x, normalize=FALSE)) # recover original weights
#' head(weights(x, log=TRUE)) # get normalized log-weights
#'
#' # add weights which are already on the log scale
#' log_wts <- log(wts)
#' head(log_wts)
#'
#' x <- weight_draws(x, weights = log_wts, log = TRUE)
#' # extract weights
#' head(weights(x))
#' head(weights(x, log=TRUE, normalize = FALSE)) # recover original log_wts
#'
#' # add weights on log scale and Pareto smooth them
#' x <- weight_draws(x, weights = log_wts, log = TRUE, pareto_smooth = TRUE)
#'
#' @export
weight_draws <- function(x, weights, ...) {
  UseMethod("weight_draws")
}

#' @rdname weight_draws
#' @export
weight_draws.draws_matrix <- function(x, weights, log = FALSE, pareto_smooth = FALSE, ...) {


  pareto_smooth <- as_one_logical(pareto_smooth)
  log <- as_one_logical(log)
  log_weights <- validate_weights(weights, x, log = log)
  if (pareto_smooth) {
    log_weights <- pareto_smooth_log_weights(log_weights)
  }
  if (".log_weight" %in% variables(x, reserved = TRUE)) {
    # overwrite existing weights
    x[, ".log_weight"] <- log_weights
  } else {
    # add weights as a new variable
    log_weights <- draws_matrix(.log_weight = log_weights, .nchains = nchains(x))
    x <- bind_draws(x, log_weights)
  }
  x
}

#' @rdname weight_draws
#' @export
weight_draws.draws_array <- function(x, weights, log = FALSE, pareto_smooth = FALSE, ...) {

  pareto_smooth <- as_one_logical(pareto_smooth)
  log <- as_one_logical(log)
  log_weights <- validate_weights(weights, x, log = log)
  if (pareto_smooth) {
    log_weights <- pareto_smooth_log_weights(log_weights)
  }
  if (".log_weight" %in% variables(x, reserved = TRUE)) {
    # overwrite existing weights
    x[, , ".log_weight"] <- log_weights
  } else {
    # add weights as a new variable
    log_weights <- draws_array(.log_weight = log_weights, .nchains = nchains(x))
    x <- bind_draws(x, log_weights)
  }
  x
}

#' @rdname weight_draws
#' @export
weight_draws.draws_df <- function(x, weights, log = FALSE, pareto_smooth = FALSE, ...) {

  pareto_smooth <- as_one_logical(pareto_smooth)
  log <- as_one_logical(log)
  log_weights <- validate_weights(weights, x, log = log)
  if (pareto_smooth) {
    log_weights <- pareto_smooth_log_weights(log_weights)
  }
  x$.log_weight <- log_weights
  x
}

#' @rdname weight_draws
#' @export
weight_draws.draws_list <- function(x, weights, log = FALSE, pareto_smooth = FALSE, ...) {

  pareto_smooth <- as_one_logical(pareto_smooth)
  log <- as_one_logical(log)
  log_weights <- validate_weights(weights, x, log = log)
  if (pareto_smooth) {
    log_weights <- pareto_smooth_log_weights(log_weights)
  }
  niterations <- niterations(x)
  for (i in seq_len(nchains(x))) {
    sel <- (1 + (i - 1) * niterations):(i * niterations)
    x[[i]]$.log_weight <- log_weights[sel]
  }
  x
}

#' @rdname weight_draws
#' @export
weight_draws.draws_rvars <- function(x, weights, log = FALSE, pareto_smooth = FALSE, ...) {

  pareto_smooth <- as_one_logical(pareto_smooth)
  log <- as_one_logical(log)
  log_weights <- validate_weights(weights, x, log = log)
  if (pareto_smooth) {
    log_weights <- pareto_smooth_log_weights(log_weights)
  }
  x$.log_weight <- rvar(log_weights)
  x
}

#' Extract Weights from Draws Objects
#'
#' Extract weights from [`draws`] objects, with one weight per draw.
#' See [`weight_draws`] for details how to add weights to [`draws`] objects.
#'
#' @param object (draws) A [`draws`] object.
#' @param log (logical) Should the weights be returned on the log scale?
#'   Defaults to `FALSE`.
#' @param normalize (logical) Should the weights be normalized to sum to 1 on
#'   the standard scale? Defaults to `TRUE`.
#' @template args-methods-dots
#'
#' @return A vector of weights, with one weight per draw.
#'
#' @seealso [`weight_draws`], [`resample_draws`]
#'
#' @inherit weight_draws examples
#'
#' @export
weights.draws <- function(object, log = FALSE, normalize = TRUE, ...) {
  log <- as_one_logical(log)
  normalize <- as_one_logical(normalize)
  if (!".log_weight" %in% variables(object, reserved = TRUE)) {
    return(NULL)
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
  checkmate::assert_numeric(weights, any.missing = FALSE)
  checkmate::assert_flag(log)
  if (length(weights) != ndraws(draws)) {
    stop_no_call("Number of weights must match the number of draws.")
  }
  if (!log) {
    if (any(weights < 0)) {
      stop_no_call("Weights must be non-negative.")
    }
    weights <- log(weights)
  }
  weights
}


pareto_smooth_log_weights <- function(log_weights) {
  pareto_smooth(
    log_weights,
    tail = "right",
    return_k = TRUE,
    are_log_weights = TRUE,
    extra_diags = TRUE
  )$x
}
