#' @export
is_draws_matrix <- function(x) {
  inherits(x, "draws_matrix")
}

# is an object looking like a 'draws_matrix' object?
is_draws_matrix_like <- function(x) {
  is.matrix(x) || is.array(x) && length(dim(x)) == 2L
}

# TODO: do we want to support this '$' method at all?
# select draws of a single parameter
#' @export
`$.draws_matrix` <- function(x, name) {
  class(x) <- "matrix"
  out <- x[, name, drop = FALSE]
  # selectively drop the second dimension
  dim(out) <- dim(x)[1]
  dimnames(out) <- dimnames(x)[1]
  out
}

#' @export
`[.draws_matrix` <- function(x, i, j, ..., drop = FALSE) {
  # TODO: add a warning that 'drop' is ignored?
  out <- NextMethod("[", drop = FALSE)
  class(out) <- class(x)
  out
}
