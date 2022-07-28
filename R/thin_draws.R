#' Thin `draws` objects
#'
#' Thin [`draws`] objects to reduce their size and autocorrelation in the chains.
#'
#' @aliases thin
#' @template args-methods-x
#' @param thin (positive integer) The period for selecting draws.
#' @template args-methods-dots
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
thin_draws <- function(x, thin, ...) {
  UseMethod("thin_draws")
}

#' @rdname thin_draws
#' @export
thin_draws.draws <- function(x, thin, ...) {
  thin <- as_one_integer(thin)
  if (thin == 1L) {
    # no thinning requested
    return(x)
  }
  if (thin <= 0L) {
    stop_no_call("'thin' must be a positive integer.")
  }
  niterations <- niterations(x)
  if (thin > niterations ) {
    stop_no_call("'thin' must be smaller than the total number of iterations.")
  }
  iteration_ids <- seq(1, niterations, by = thin)
  subset_draws(x, iteration = iteration_ids)
}

#' @rdname thin_draws
#' @export
thin_draws.rvar <- function(x, thin, ...) {
  thin_draws(draws_rvars(x = x), thin, ...)$x
}
