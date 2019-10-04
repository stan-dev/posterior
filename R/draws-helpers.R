# detect the supported format closest to the format of the input
closest_draws_format <- function(x) {
  if (is_draws_matrix_like(x)) {
    out <- "matrix"
  } else if (is_draws_array_like(x)) {
    out <- "array"
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

# is an object looking like a 'draws_matrix' object?
is_draws_matrix_like <- function(x) {
  is.matrix(x) || is.array(x) && length(dim(x)) == 2L
}

# is an object looking like a 'draws_array' object?
is_draws_array_like <- function(x) {
  is.array(x) && length(dim(x)) == 3L
}

#' @export
'[.draws_matrix' <- function(x, ..., drop = FALSE) {
  # TODO: warn that drop = TRUE will be ignored
  old_class <- class(x)
  class(x) <- "matrix"
  x <- x[..., drop = FALSE]
  class(x) <- old_class
  x
}

#' @export
'[.draws_array' <- function(x, ..., drop = FALSE) {
  # TODO: warn that drop = TRUE will be ignored
  old_class <- class(x)
  class(x) <- "array"
  x <- x[..., drop = FALSE]
  class(x) <- old_class
  x
}

# TODO: do we want to support these '$' methods at all?
# select draws of a single parameter
#' @export
'$.draws_matrix' <- function(x, name) {
  class(x) <- "matrix"
  out <- x[, name, drop = FALSE]
  # selectively drop the second dimension
  dim(out) <- dim(x)[1]
  dimnames(out) <- dimnames(x)[1]
  out
}

# select draws of a single parameter
#' @export
'$.draws_array' <- function(x, name) {
  class(x) <- "array"
  out <- x[, , name, drop = FALSE]
  # selectively drop the third dimension
  dim(out) <- dim(x)[1:2]
  dimnames(out) <- dimnames(x)[1:2]
  out
}
