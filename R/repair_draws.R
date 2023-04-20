#' Repair indices of `draws` objects
#'
#' Repair indices of `draws` objects so that iterations, chains, and draws
#' are continuously and consistently numbered.
#'
#' @template args-methods-x
#' @template args-methods-dots
#' @param order (logical) Should draws be ordered (via [`order_draws()`]) before
#'   repairing indices? Defaults to `TRUE`.
#' @template return-draws
#' @seealso [order_draws()]
#' @examples
#' x <- as_draws_array(example_draws())
#' (x <- x[10:5, 3:4, ])
#' repair_draws(x)
#'
#' @export
repair_draws <- function(x, order = TRUE, ...) {
  UseMethod("repair_draws")
}

#' @rdname repair_draws
#' @export
repair_draws.draws_matrix <- function(x, order = TRUE, ...) {
  # ensure integer instead of character ordering
  rownames(x) <- repair_ids(rownames(x))
  x <- do_ordering(x, order)
  rownames(x) <- as.character(seq_rows(x))
  x
}

#' @rdname repair_draws
#' @export
repair_draws.draws_array <- function(x, order = TRUE, ...) {
  # ensure integer instead of character ordering
  rownames(x) <- repair_ids(rownames(x))
  colnames(x) <- repair_ids(colnames(x))
  x <- do_ordering(x, order)
  rownames(x) <- as.character(seq_rows(x))
  colnames(x) <- as.character(seq_cols(x))
  x
}

#' @rdname repair_draws
#' @export
repair_draws.draws_df <- function(x, order = TRUE, ...) {
  x <- do_ordering(x, order)
  x$.chain <- repair_chain_ids(x$.chain)
  x$.iteration <- repair_iteration_ids(x$.iteration, x$.chain)
  x$.draw <- compute_draw_ids(x$.chain, x$.iteration)
  x
}

#' @rdname repair_draws
#' @export
repair_draws.draws_list <- function(x, order = TRUE, ...) {
  # ensure integer instead of character ordering
  names(x) <- repair_ids(names(x))
  x <- do_ordering(x, order)
  names(x) <- seq_along(x)
  x
}

#' @rdname repair_draws
#' @export
repair_draws.draws_rvars <- function(x, order = TRUE, ...) {
  for (i in seq_along(x)) {
    x[[i]] <- repair_draws(x[[i]], order = order, ...)
  }
  x
}

#' @rdname repair_draws
#' @export
repair_draws.rvar <- function(x, order = TRUE, ...) {
  rownames(draws_of(x)) <- repair_ids(rownames(draws_of(x)))
  order <- as_one_logical(order)
  if (order) {
    x <- order_draws(x)
  } else if (nchains(x) > 1) {
    # even if we aren't asked to order draws, still need to check on the order
    # because if the draws are out of order we have to drop chain info (it would
    # no longer be guaranteed to be correct)
    draw_order <- order(draw_ids(x))
    if (needs_ordering(draw_order)) {
      x <- merge_chains(x)
    }
  }
  rownames(draws_of(x)) <- seq_rows(draws_of(x))
  x
}


# internal ----------------------------------------------------------------

#' Repair indices to be continuously numbered integers starting from one
#' @param x vector of values
#' @noRd
repair_ids <- function(x) {
  out <- SW(as.integer(x))
  if (anyNA(out)) {
    # use character instead of integer ordering if
    # some values cannot be converted to integers
    out <- factor(x)
  } else {
    out <- factor(out)
  }
  as.integer(out)
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
  as.integer(out)
}

#' Repair chain indices
#' @param chain_ids A vector of chain indices
#' @noRd
repair_chain_ids <- function(chain_ids) {
  repair_ids(chain_ids)
}

#' Compute draw indices from iteration and chain indices
#' @param chain_ids A vector of chain indices
#' @param iteration_ids A vector of iteration indices
#' @noRd
compute_draw_ids <- function(chain_ids, iteration_ids) {
  assert_true(length(chain_ids) == length(iteration_ids))
  niterations <- SW(max(iteration_ids))
  out <- (chain_ids - 1L) * niterations + iteration_ids
  as.integer(out)
}
