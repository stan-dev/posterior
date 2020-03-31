resample_draws <- function(x, ...) {
  UseMethod("resample_draws")
}

# proof-of-concept implementation for testing only
#' @export
resample_draws.draws <- function(x, method = "simple", weights = NULL,
                                 ndraws = NULL, replace = TRUE, ...) {
  if (!allow_resample_draws(x)) {
    stop2("'resample_draws' requires formats which can be subsetted",
          "by individual draws, not only by iterations or chains.")
  }
  ndraws_total <- ndraws(x)
  assert_numeric(weights, len = ndraws_total, null.ok = TRUE)
  assert_number(ndraws, null.ok = TRUE, lower = 0)
  replace <- as_one_logical(replace)
  if (is.null(weights)) {
    weights <- rep(1 / ndraws_total, ndraws_total)
  } else {
    weights <- weights / sum(weights)
  }
  if (is.null(ndraws)) {
    if (!replace) {
      stop2("Argument 'ndraws' is required if 'replace' is FALSE.")
    }
    ndraws <- ndraws_total
  }
  if (ndraws > ndraws_total && !replace) {
    stop2("Argument 'ndraws' must be smaller than the total ",
          "number of draws if 'replace' is FALSE.")
  }
  method_fun <- paste0("resample_", method)
  method_fun <- get(method_fun, asNamespace("posterior"))
  sel_draws <- method_fun(
    ndraws = ndraws,
    ndraws_total = ndraws_total,
    weights = weights,
    replace = replace,
    ...
  )
  subset_draws(x, draw = sel_draws, unique = FALSE)
}

# simple random resampling with or without replacement
resample_simple <- function(ndraws, ndraws_total, weights, replace, ...) {
  idx <- seq_len(ndraws_total)
  sample(idx, ndraws, replace = replace, prob = weights)
}

# TODO: add more resample methods

# allow resampling for draws for this format
# not all formats support subsetting of draws
allow_resample_draws <- function(x) {
  is_draws_matrix(x) || is_draws_df(x)
}
