# detect the supported format closest to the format of the input
closest_draws_format <- function(x) {
  if (is_draws_matrix_like(x)) {
    out <- "matrix"
  } else if (is_draws_array_like(x)) {
    out <- "array"
  } else if (is_draws_data_frame_like(x)) {
    out <- "data_frame"
  } else {
    stop2("Don't know how to transform an object of class ",
          "'", class(x)[1L], "' to any supported draws format.")
  }
  paste0("draws_", out)
}

# transform an object to the closest supported draws format
as_closest_draws_format <- function(x) {
  format <- closest_draws_format(x)
  fun <- get(paste0(".as_", format), asNamespace("posterior"))
  fun(x)
}

# TODO: move these functions to separate files?
# get iteration indices
.iterations <- function(x) {
  UseMethod(".iterations")
}

#' @export
.iterations.draws_matrix <- function(x) {
  as.integer(rownames(x))
}

#' @export
.iterations.draws_array <- function(x) {
  as.integer(rownames(x))
}

#' @export
.iterations.draws_data_frame <- function(x) {
  as.integer(unique(x$.iteration))
}

# get chain indices
.chains <- function(x) {
  UseMethod(".chains")
}

#' @export
.chains.draws_matrix <- function(x) {
  1L
}

#' @export
.chains.draws_array <- function(x) {
  as.integer(colnames(x))
}

#' @export
.chains.draws_data_frame <- function(x) {
  as.integer(unique(x$.chain))
}
