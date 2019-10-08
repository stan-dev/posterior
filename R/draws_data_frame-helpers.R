#' @export
is_draws_data_frame <- function(x) {
  inherits(x, "draws_data_frame")
}

# is an object looking like a 'draws_data_frame' object?
is_draws_data_frame_like <- function(x) {
  is.data.frame(x)
}

# meta column names
meta_columns <- function() {
  c(".iteration", ".chains", ".draw")
}

# remove meta columns
remove_meta_columns <- function(x) {
  assert_class(x, "draws_data_frame")
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
