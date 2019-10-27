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
  out <- c(".iteration", ".chain", ".draw")
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

# compute index over draws from iteration and chain indices
compute_draw_indices <- function(iterations, chains) {
  niter <- max(iterations)
  (chains - 1) * niter + iterations
}
