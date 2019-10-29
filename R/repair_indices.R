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

# create continuous indices from 1 to length(unique(x))
# @param x a vector
# @param order order values in ascending order (TRUE)
#   or keep their order of appearence (FALSE)
index_continuously <- function(x, order = TRUE) {
  check_logical(order, len = 1L)
  if (order) {
    out <- as.integer(factor(x))
  } else {
    out <- as.integer(factor(x, levels = unique(x)))
  }
  out
}

