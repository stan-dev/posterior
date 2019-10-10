#' @export
is_draws_data_frame <- function(x) {
  inherits(x, "draws_data_frame")
}

# is an object looking like a 'draws_data_frame' object?
is_draws_data_frame_like <- function(x) {
  is.data.frame(x)
}

# meta column names
# @param x a named object from which to extract existing meta column names
meta_columns <- function(x = NULL) {
  out <- c(".variable", ".iteration", ".chain", ".draw")
  if (!is.null(x)) {
    out <- intersect(out, names(x))
  }
  out
}

# remove meta columns
remove_meta_columns <- function(x) {
  assert_true(is.list(x))
  for (col in meta_columns()) {
    x[[col]] <- NULL
  }
  x
}

# move meta columns to the start of a list or data frame
move_meta_columns_first <- function(x) {
  assert_true(is.list(x))
  meta_columns <- intersect(names(x), meta_columns())
  if (!length(meta_columns)) {
    return(x)
  }
  x[c(meta_columns, setdiff(names(x), meta_columns))]
}

# compute index over draws from iteration and chain indices
compute_draw_indices <- function(iterations, chains) {
  niter <- max(iterations)
  (chains - 1) * niter + iterations
}
