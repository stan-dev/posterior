#' Thin `draws` objects
#'
#' Thin [`draws`] objects to reduce their size and autocorrelation in
#' the chains.
#'
#' @aliases thin
#' @template args-methods-x
#' @param thin (positive numeric) The period for selecting draws. Must
#'   be between 1 and the number of iterations. If the value is not an
#'   integer, the draws will be selected such that the number of draws
#'   returned is equal to round(ndraws(x) / thin). Intervals between
#'   selected draws will be either ceiling(thin) or floor(thin), such
#'   that the average interval will be close to the thin value. If
#'   `NULL`, it will be automatically calculated based on bulk and
#'   tail effective sample size as suggested by Säilynoja et
#'   al. (2022).
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
  if (thin <= 1L) {
    stop_no_call("'thin' must be greater than or equal to 1")
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
