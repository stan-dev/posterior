#' Order `draws` objects
#'
#' Order [`draws`] objects according to iteration and chain number. By default,
#' draws objects are ordered but subsetting or extracting parts of them may
#' leave them in an unordered state.
#'
#' @template args-methods-x
#' @template args-methods-dots
#'
#' @examples
#' x <- as_draws_array(example_draws())
#' # manually select some iterations and chains
#' (x <- x[10:5, 4:3, ])
#' # reorder iteration and chain indices
#' order_draws(x)
#'
#' @export
order_draws <- function(x, ...) {
  UseMethod("order_draws")
}

#' @rdname order_draws
#' @export
order_draws.draws_matrix <- function(x, ...) {
  iteration_order <- order(iteration_ids(x))
  if (needs_ordering(iteration_order)) {
    x <- x[iteration_order, ]
  }
  x
}

#' @rdname order_draws
#' @export
order_draws.draws_array <- function(x, ...) {
  iteration_order <- order(iteration_ids(x))
  chain_order <- order(chain_ids(x))
  if (needs_ordering(iteration_order, chain_order)) {
    x <- x[iteration_order, chain_order, ]
  }
  x
}

#' @rdname order_draws
#' @export
order_draws.draws_df <- function(x, ...) {
  row_order <- order(x$.chain, x$.iteration)
  if (needs_ordering(row_order)) {
    x <- x[row_order, ]
  }
  x
}

#' @rdname order_draws
#' @export
order_draws.draws_list <- function(x, ...) {
  chain_order <- order(chain_ids(x))
  if (needs_ordering(chain_order)) {
    x <- x[chain_order]
  }
  x
}

#' Order draws if told to do so
#' @param x draws object to be ordered
#' @param order should the draws object be ordered?
#' @return potentially ordered draws object
#' @noRd
do_ordering <- function(x, order, ...) {
  order <- as_one_logical(order)
  if (order) {
    x <- order_draws(x, ...)
  }
  x
}

# are vectors in an unordered state?
needs_ordering <- function(...) {
  dots <- list(...)
  .needs_ordering <- function(x) {
    any(x != seq_along(x))
  }
  any(ulapply(dots, .needs_ordering))
}
