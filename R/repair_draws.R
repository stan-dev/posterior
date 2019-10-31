#' Repair Indices of Draws Objects
#'
#' Repair indices of draws objects so that iterations, chains, and draws
#' are continuously and consistently numbered.
#'
#' @template args-methods-x
#' @template args-methods-dots
#' @param order Logical; Indicates if draws should be ordered (via
#'   `\link{order_draws}`) before reparing indices. Defaults to `TRUE`.
#'
#' @export
repair_draws <- function(x, order = TRUE, ...) {
  UseMethod("repair_draws")
}

#' @export
repair_draws.draws_matrix <- function(x, order = TRUE, ...) {
  x <- do_ordering(x, order)
  rownames(x) <- as.character(seq_rows(x))
  x
}

#' @export
repair_draws.draws_array <- function(x, order = TRUE, ...) {
  x <- do_ordering(x, order)
  rownames(x) <- as.character(seq_rows(x))
  colnames(x) <- as.character(seq_cols(x))
  x
}

#' @export
repair_draws.draws_df <- function(x, order = TRUE, ...) {
  x <- do_ordering(x, order)
  x$.chain <- repair_chain_indices(x$.chain)
  x$.iteration <- repair_iteration_indices(x$.iteration, x$.chain)
  x$.draw <- compute_draw_indices(x$.iteration, x$.chain)
  x
}

#' @export
repair_draws.draws_list <- function(x, order = TRUE, ...) {
  x <- do_ordering(x, order)
  names(x) <- seq_along(x)
  x
}

#' Repair iteration indices
#' @param iterations A vector of iteration indices
#' @param chains A vector of chain indices
#' @noRd
repair_iteration_indices <- function(iterations, chains = NULL) {
  .repair_iteration_indices <- function(x) {
    match(seq_along(x), order(x))
  }
  if (is.null(chains)) {
    out <- .repair_iteration_indices(iterations)
  } else {
    check_true(length(iterations) == length(chains))
    unique_chains <- unique(chains)
    out <- rep(NA, length(iterations))
    for (u in unique(chains)) {
      sel <- chains == u
      out[sel] <- .repair_iteration_indices(iterations[sel])
    }
  }
  out
}

#' Repair chain indices
#' @param chains A vector of chain indices
#' @noRd
repair_chain_indices <- function(chains) {
  as.integer(factor(chains))
}

#' Compute draw indices from iterations and chains
#' @param iterations A vector of iteration indices
#' @param chains A vector of chain indices
#' @noRd
compute_draw_indices <- function(iterations, chains) {
  niter <- max(iterations)
  (chains - 1) * niter + iterations
}
