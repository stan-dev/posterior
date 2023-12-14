#' Thin `draws` objects
#'
#' Thin [`draws`] objects to reduce their size and autocorrelation in the chains.
#'
#' @aliases thin
#' @template args-methods-x
#' @param thin (positive integer) The period for selecting draws. If
#'   omitted, this will be automatically calculated based on bulk-ESS
#'   and tail-ESS as suggested by Säilynoja et al. (2022).
#' @template args-methods-dots
#' @template ref-sailynoja-ecdf-2022
#' @template return-draws
#'
#' @examples
#' x <- example_draws()
#' niterations(x)
#'
#' x <- thin_draws(x, thin = 5)
#' niterations(x)
#'
#' @export
thin_draws <- function(x, thin = NULL, ...) {
  UseMethod("thin_draws")
}

#' @rdname thin_draws
#' @export
thin_draws.draws <- function(x, thin = NULL, ...) {
  if (is.null(thin)) {
    thin <- ess_based_thinning_all_vars(x)
    message("Automatically thinned by ", round(thin, 1), " based on ESS.")
  }

  thin <- as_one_numeric(thin)
  if (thin == 1L) {
    # no thinning requested
    return(x)
  }
  if (thin <= 0L) {
    stop_no_call("'thin' must be a positive integer.")
  }
  niterations <- niterations(x)
  if (thin > niterations) {
    stop_no_call("'thin' must be smaller than the total number of iterations.")
  }
  iteration_ids <- round(seq(1, niterations, by = thin))
  subset_draws(x, iteration = iteration_ids)
}

#' @rdname thin_draws
#' @export
thin_draws.rvar <- function(x, thin = NULL, ...) {
  thin_draws(draws_rvars(x = x), thin, ...)$x
}

ess_based_thinning_all_vars <- function(x, ...) {
  max(summarise_draws(x, thin = ess_based_thinning)$thin)
}

ess_based_thinning <- function(x, ...) {
  # thin based on mean (over chains) of minimum of tail and bulk ess
  x <- as.matrix(x)
  ess_tailbulk_chains <- apply(x,
    MARGIN = 2,
    FUN = function(x) min(SW(ess_tail(x)), SW(ess_bulk(x)))
    )
  nrow(x) / mean(ess_tailbulk_chains)
}
