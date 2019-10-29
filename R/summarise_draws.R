#' @export
summarise_draws <- function(x, ...) {
  UseMethod("summarise_draws")
}

#' @export
summarise_draws.default <- function(x, ...) {
  x <- as_draws(x)
  summarise_draws.default(x, ...)
}

#' @export
summarise_draws.draws <- function(x, measures = NULL,
                                  probs = c(0.05, 0.95), ...) {
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
  out <- as_tibble(do_call(rbind, out))
  if (any(names(out) == "variable")) {
    stop2("Name 'variable' is reserved in 'summarise_draws'.")
  }
  out$variable <- variables
  out <- move_to_start(out, "variable")
  out
}

#' @export
summary.draws <- function(x, ...) {
  summarise_draws(x, ...)
}

#' @export
default_summary_measures <- function() {
  c("mean", "median", "sd", "mad", "quantile")
}

#' @export
default_convergence_measures <- function() {
  c("rhat", "ess_bulk", "ess_tail")
}

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
