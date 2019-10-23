#' Repair Indices of Draws Objects
#' @export
repair_indices <- function(x, ...) {
  # TODO: decide whether to sort indices rather than just renaming them
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
repair_indices.draws_data_frame <- function(x, ...) {
  x$.chain <- index_continuously(x$.chain)
  x$.iterations <- index_continuously(x$.iteration)
  x$.draw <- compute_draw_indices(x$.iteration, x$.chain)
  x
}

#' @export
repair_indices.draws_list <- function(x, ...) {
  names(x) <- seq_along(x)
  x
}

# create continuous indices from 1 to length(unique(x))
index_continuously <- function(x) {
  # TODO: use the order of appearence?
  as.integer(factor(x))
}

