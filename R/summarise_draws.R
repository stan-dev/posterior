#' Summaries of draws objects
#'
#' The `summarise_draws()` (and `summarize_draws()`) methods provide a quick way
#' to get a table of summary statistics and diagnostics. These methods will
#' convert an object to a `draws` object if it isn't already. For convenience, a
#' [summary()][base::summary] method for `draws` objects is also provided as an
#' alias for `summarise_draws()` if the input object is already a `draws`
#' object.
#'
#' @name draws_summary
#'
#' @param x A `draws` object or one coercible to a `draws` object.
#' @param ... Optionally, arguments to pass to specific methods.
#' @param measures A character vector containing the names of summary stats or
#'   diagnostics to include. The convenience functions with names `default_*`
#'   return character vectors with the names of the measures included by
#'   default.
#' @param probs A numeric vector of probabilities used to compute quantiles. In
#'   the output any columns corresponding to quantiles will have names starting
#'   with lowercase `"q"` (e.g. `"q95"`).
#'
#' @return
#' The `summarise_draws()` methods return a [tibble][tibble::tibble] data frame.
#' The first column, `"variable"`, contains the variable names and the remaining
#' columns contain summary statistics and diagnostics.
#'
#' The functions `default_summary_measures()`, `default_convergence_measures()`,
#' and `default_mcse_measures()` return character vectors of names of the
#' default measures included.
#'
#' @examples
#' x <- draws_eight_schools
#' class(x)
#' str(x)
#'
#' summarise_draws(x) # default output
#' summarise_draws(x, "quantile", probs = c(0.1, 0.9))
#' summarise_draws(x, c("mean", "median"))
#'
NULL

#' @rdname draws_summary
#' @export
summarise_draws <- function(x, ...) {
  UseMethod("summarise_draws")
}

#' @rdname draws_summary
#' @export
summarize_draws <- summarise_draws


#' @export
summarise_draws.default <- function(x, ...) {
  x <- as_draws(x)
  summarise_draws.default(x, ...)
}

#' @rdname draws_summary
#' @export
summarise_draws.draws <- function(x,
                                  measures = NULL,
                                  ...,
                                  probs = c(0.05, 0.95)) {
  variables <- variables(x)
  if (is.null(measures)) {
    measures <- c(
      default_summary_measures(),
      default_convergence_measures()
    )
  }
  measures <- as.character(measures)
  if ("quantile" %in% measures) {
    # ensure correct format for quantiles
    .quantile <- function(x, ...) quantile2(x, probs, ...)
    measures[measures == "quantile"] <- ".quantile"
  }
  if ("mcse_quantile" %in% measures) {
    .mcse_quantile <- function(x, ...) mcse_quantile2(x, probs, ...)
    measures[measures == "mcse_quantile"] <- ".mcse_quantile"
  }
  if ("ess_quantile" %in% measures) {
    .ess_quantile <- function(x, ...) ess_quantile2(x, probs, ...)
    measures[measures == "ess_quantile"] <- ".ess_quantile"
  }
  out <- named_list(variables, value = list(named_list(measures)))
  funs <- lapply(measures, get, environment())
  names(funs) <- measures
  for (v in variables) {
    draws <- extract_one_variable_matrix(x, variable = v)
    for (m in measures) {
      out[[v]][[m]] <- funs[[m]](draws, ...)
    }
    out[[v]] <- do_call(cbind, out[[v]])
  }
  out <- tibble::as_tibble(do_call(rbind, out))
  if (any(names(out) == "variable")) {
    stop2("Name 'variable' is reserved in 'summarise_draws'.")
  }
  out$variable <- variables
  out <- move_to_start(out, "variable")
  out
}

#' @rdname draws_summary
#' @export
summary.draws <- function(x, ...) {
  summarise_draws(x, ...)
}

#' @rdname draws_summary
#' @export
default_summary_measures <- function() {
  c("mean", "median", "sd", "mad", "quantile")
}

#' @rdname draws_summary
#' @export
default_convergence_measures <- function() {
  c("rhat", "ess_bulk", "ess_tail")
}

#' @rdname draws_summary
#' @export
default_mcse_measures <- function() {
  c("mcse_mean", "mcse_median", "mcse_sd", "mcse_quantile")
}

# ensure quantiles are returned in the right format
quantile2 <- function(x, probs, ...) {
  out <- matrix(quantile(x, probs = probs, ...), nrow = 1L)
  colnames(out) <- paste0("q", probs * 100)
  out
}

# ensure MCSE of quantiles are returned in the right format
mcse_quantile2 <- function(x, probs, ...) {
  out <- matrix(mcse_quantile(x, probs = probs, ...), nrow = 1L)
  colnames(out) <- paste0("mcse_q", probs * 100)
  out
}

# ensure ESS of quantiles are returned in the right format
ess_quantile2 <- function(x, probs, ...) {
  out <- matrix(ess_quantile(x, probs = probs, ...), nrow = 1L)
  colnames(out) <- paste0("ess_q", probs * 100)
  out
}
