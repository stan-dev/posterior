#' @export
summarise_draws <- function(x, ...) {
  UseMethod("summarise_draws")
}

#' @export
summarise_draws.default <- function(x, measures = NULL,
                                    probs = c(0.05, 0.95), ...) {
  x <- as_closest_draws_format(x)
  variables <- variables(x)
  if (is.null(measures)) {
    measures <- c(all_summary_measures(), all_convergence_measures())
  }
  measures <- as.character(measures)
  if ("quantile" %in% measures) {
    # ensure correct format for quantiles
    .quantile <- function(x, ...) quantile2(x, probs, ...)
    measures[measures == "quantile"] <- ".quantile"
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
  out <- as_tibble(do_call(rbind, out))
  out$.variable <- variables
  out <- move_to_start(out, ".variable")
  out
}

#' @export
all_summary_measures <- function() {
  # TODO: add more?
  c("mean", "median", "sd", "mad", "quantile")
}

#' @export
all_convergence_measures <- function() {
  # TODO: Add mcse functions
  c("Rhat", "ess_bulk", "ess_tail")
}

# ensure quantiles are returned in the right format
quantile2 <- function(x, probs, ...) {
  out <- matrix(quantile(x, probs = probs, ...), nrow = 1L)
  colnames(out) <- paste0("Q", probs * 100)
  out
}
