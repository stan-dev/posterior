#' Repair Indices of Draws Objects
#'
#' Repair indices of draws objects so that iterations, chains, and draws
#' are continuously and consistently numbered.
#'
#' @param x An \R object.
#' @param ... Arguments passed to or from other methods.
#'
#' @export
repair_indices <- function(x, ...) {
  UseMethod("repair_indices")
}

#' @export
repair_indices.draws_matrix <- function(x, ...) {
  rownames(x) <- as.character(seq_rows(x))
  x
}

#' @export
repair_indices.draws_array <- function(x, ...) {
  rownames(x) <- as.character(seq_rows(x))
  colnames(x) <- as.character(seq_cols(x))
  x
}

#' @export
repair_indices.draws_df <- function(x, ...) {
  x$.chain <- index_continuously(x$.chain)
  x$.iteration <- index_continuously(x$.iteration)
  x$.draw <- compute_draw_indices(x$.iteration, x$.chain)
  x
}

#' @export
repair_indices.draws_list <- function(x, ...) {
  names(x) <- seq_along(x)
  x
}

#' Create continuous indices
#' @param x A vector to be converted to indices.
#' @param g An optional grouping vector. In the most common use case,
#'   `x` is the iteration number and `g` is the chain number.
#' @param order Order values in ascending order (`TRUE`)
#'   or keep their order of appearence (`FALSE`)
#' @noRd
index_continuously <- function(x, g = NULL, order = TRUE) {
  assert_flag(order)
  # internal function actually doing the indexing
  .index_continuously <- function(x) {
    if (order) {
      x <- as.integer(factor(x))
    } else {
      x <- as.integer(factor(x, levels = unique(x)))
    }
    return(x)
  }
  if (is.null(g)) {
    out <- .index_continuously(x)
  } else {
    check_true(length(x) == length(g))
    unique_y <- unique(g)
    out <- rep(NA, length(x))
    for (u in unique_y) {
      sel <- g == u
      out[sel] <- .index_continuously(x[sel])
    }
  }
  out
}
