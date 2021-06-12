#' Resample `draws` objects
#'
#' Resample [`draws`] objects according to provided weights, for example weights
#' obtained through importance sampling.
#'
#' @template args-methods-x
#' @param weights (numeric vector) A vector of positive weights of length
#'   `ndraws(x)`. The weights will be internally normalized. If `weights` is not
#'   specified, an attempt will be made to extract any weights already stored in
#'   the draws object (via [weight_draws()]). How exactly the weights influence
#'   the resampling depends on the `method` argument.
#' @param method (string) The resampling method to use:
#'  * `"simple"`: simple random resampling with replacement
#'  * `"simple_no_replace"`: simple random resampling without replacement
#'  * `"stratified"`: stratified resampling with replacement
#'  * `"deterministic"`: deterministic resampling with replacement
#'
#'  Currently, `"stratified"` is the default as it has comparably low variance
#'  and bias with respect to ideal resampling. The latter would sample perfectly
#'  proportional to the weights, but this is not possible in practice due to the
#'  finite number of draws available. For more details about resampling methods,
#'  see Kitagawa (1996).
#' @param ndraws (positive integer) The number of draws to be returned. By
#'   default `ndraws` is set internally to the total number of draws in `x` if
#'   sensible.
#' @template args-methods-dots
#' @template return-draws
#'
#' @details Upon usage of `resample_draws()`, chains will automatically be merged
#'   due to subsetting of individual draws (see [`subset_draws`] for details).
#'   Also, weights stored in the `draws` object will be removed in the process,
#'   as resampling invalidates existing weights.
#'
#' @references
#' Kitagawa, G., Monte Carlo Filter and Smoother for Non-Gaussian Nonlinear '
#' State Space Models, *Journal of Computational and Graphical Statistics*,
#' 5(1):1-25, 1996.
#'
#' @seealso [resample_draws()]
#'
#' @examples
#' x <- as_draws_df(example_draws())
#'
#' # random weights for justr for demonstration
#' w <- runif(ndraws(x), 0, 10)
#'
#' # use default stratified sampling
#' x_rs <- resample_draws(x, weights = w)
#' summarise_draws(x_rs, default_summary_measures())
#'
#' # use simple random sampling
#' x_rs <- resample_draws(x, weights = w, method = "simple")
#' summarise_draws(x_rs, default_summary_measures())
#'
#' @export
resample_draws <- function(x, ...) {
  UseMethod("resample_draws")
}

#' @rdname resample_draws
#' @export
resample_draws.draws <- function(x, weights = NULL, method = "stratified",
                                 ndraws = NULL, ...) {
  ndraws_total <- ndraws(x)
  assert_numeric(weights, len = ndraws_total, lower = 0, null.ok = TRUE)
  assert_choice(method, supported_resample_methods())
  assert_number(ndraws, null.ok = TRUE, lower = 0)
  if (is.null(weights)) {
    weights <- weights(x, normalize = TRUE)
    if (is.null(weights)) {
      stop_no_call("No weights are provided and none can ",
                   "be found within the draws object.")
    }
    # resampling invalidates stored weights
    x <- remove_variables(x, ".log_weight")
  } else {
    weights <- weights / sum(weights)
  }
  if (is.null(ndraws)) {
    if (grepl("_no_replace$", method)) {
      stop_no_call("Argument 'ndraws' is required when sampling without replacement.")
    }
    ndraws <- length(weights)
  }
  method_fun <- paste0(".resample_", method)
  method_fun <- get(method_fun, asNamespace("posterior"))
  draw_ids <- method_fun(weights = weights, ndraws = ndraws, ...)
  subset_draws(x, draw = draw_ids, unique = FALSE)
}

# simple random resampling with replacement
# @return index vector of length 'ndraws'
.resample_simple <- function(weights, ndraws, ...) {
  out <- seq_along(weights)
  sample(out, ndraws, replace = TRUE, prob = weights)
}


# internal ----------------------------------------------------------------

# simple random resampling without replacement
# @return index vector of length 'ndraws'
.resample_simple_no_replace <- function(weights, ndraws, ...) {
  if (ndraws > length(weights)) {
    stop_no_call("Argument 'ndraws' must be smaller than the total ",
          "number of draws in method 'simple_no_replace'.")
  }
  out <- seq_along(weights)
  sample(out, ndraws, replace = FALSE, prob = weights)
}

# Stratified resampling
#   Kitagawa, G., Monte Carlo Filter and Smoother for Non-Gaussian
#   Nonlinear State Space Models, Journal of Computational and
#   Graphical Statistics, 5(1):1-25, 1996.
# @return index vector of length 'ndraws'
.resample_stratified <- function(weights, ndraws, ...) {
  # expected number of repetitions for each original draw
  w <- ndraws * weights
  out <- integer(ndraws)
  c <- 0
  j <- 0
  for (i in seq_along(w)) {
    c <- c + w[i]
    if (c >= 1) {
      a <- floor(c)
      c <- c - a
      out[j + seq_len(a)] <- i
      j <- j + a
    }
    if (j < ndraws && c >= runif(1)) {
      c <- c - 1
      j <- j + 1
      out[j] <- i
    }
  }
  out
}

# Deterministic resampling
#   Kitagawa, G., Monte Carlo Filter and Smoother for Non-Gaussian
#   Nonlinear State Space Models, Journal of Computational and
#   Graphical Statistics, 5(1):1-25, 1996.
# @return index vector of length 'ndraws'
.resample_deterministic <- function(weights, ndraws, ...) {
  # expected number of repetitions for each original draw
  w <- ndraws * weights
  fw <- floor(w)
  out <- integer(ndraws)
  k <- 0
  c <- 0.5
  for (i in seq_along(w)) {
    if (w[i] >= 1) {
      a <- fw[i]
      w[i] <- w[i] - a
      out[k + seq_len(a)] <- i
      k <-  k + a;
    }
    c <- c + w[i]
    if (c >= 1) {
      k <- k + 1
      out[k] <- i
      c <- c - 1
    }
  }
  out
}

# names of supported resampling methods
supported_resample_methods <- function() {
  c("simple", "simple_no_replace", "stratified", "deterministic")
}
