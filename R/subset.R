#' @export
subset_draws <- function(x, draws, ...) {
  # TODO: add default method?
  UseMethod("subset_draws")
}

subset_draws.default <- function(x, draws, ...) {
  x <- as_posterior(x)
  subset_draws(x, draws, ...)
}

#' @export
subset_draws.vctrs_rray <- function(x, draws, ...) {
  # TODO: add class on top of those provided by rray
  # currently they are dropped my all math operations
  if (is_posterior_matrix(x)) {
    out <- x[draws, ]
  } else if (is_posterior_array(x)) {
    out <- x[draws, , ]
  } else {
    x <- as_posterior(x)
    out <- subset_draws(x, draws, ...)
  }
  out
}

#' @export
subset_pars <- function(x, pars, ...) {
  UseMethod("subset_pars")
}

subset_pars.default <- function(x, pars, ...) {
  x <- as_posterior(x)
  subset_pars(x, pars, ...)
}

#' @export
subset_pars.vctrs_rray <- function(x, pars, ...) {
  x <- as_posterior(x)
  if (is_posterior_matrix(x)) {
    out <- x[, pars]
  } else if (is_posterior_array(x)) {
    out <- x[, , pars]
  } else {
    x <- as_posterior(x)
    out <- subset_pars(x, pars, ...)
  }
  out
}

#' Extract posterior matrix of a single parameter
#'
#' extract a draws x chain matrix for a single parameter
#' required for various convergence diagnostics.
#'
#' @param x A posterior object.
#' @param par A parameter name to extract draws for.
#'
#' @export
extract_one_par_matrix <- function(x, par) {
  par <- as_one_character(par)
  out <- subset_pars(x, par)
  out <- as_posterior_array(out)
  rray_squeeze(out, axes = 3L)
}
