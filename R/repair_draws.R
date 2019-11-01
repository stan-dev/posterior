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
  x$.chain <- repair_chain_ids(x$.chain)
  x$.iteration <- repair_iteration_ids(x$.iteration, x$.chain)
  x$.draw <- compute_draw_ids(x$.iteration, x$.chain)
  x
}

#' @export
repair_draws.draws_list <- function(x, order = TRUE, ...) {
  x <- do_ordering(x, order)
  names(x) <- seq_along(x)
  x
}

#' Repair iteration indices
#' @param iteration_ids A vector of iteration indices
#' @param chain_ids A vector of chain indices
#' @noRd
repair_iteration_ids <- function(iteration_ids, chain_ids = NULL) {
  .repair_iteration_ids <- function(x) {
    match(seq_along(x), order(x))
  }
  if (is.null(chain_ids)) {
    out <- .repair_iteration_ids(iteration_ids)
  } else {
    check_true(length(iteration_ids) == length(chain_ids))
    unique_chain_ids <- unique(chain_ids)
    out <- rep(NA, length(iteration_ids))
    for (u in unique(chain_ids)) {
      sel <- chain_ids == u
      out[sel] <- .repair_iteration_ids(iteration_ids[sel])
    }
  }
  out
}

#' Repair chain indices
#' @param chain_ids A vector of chain indices
#' @noRd
repair_chain_ids <- function(chain_ids) {
  as.integer(factor(chain_ids))
}

#' Compute draw indices from iteration and chain indices
#' @param iteration_ids A vector of iteration indices
#' @param chain_ids A vector of chain indices
#' @noRd
compute_draw_ids <- function(iteration_ids, chain_ids) {
  niterations <- max(iteration_ids)
  (chain_ids - 1) * niterations + iteration_ids
}
