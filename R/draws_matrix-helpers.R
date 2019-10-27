#' @export
is_draws_matrix <- function(x) {
  inherits(x, "draws_matrix")
}

# is an object looking like a 'draws_matrix' object?
is_draws_matrix_like <- function(x) {
  is.matrix(x) || is.array(x) && length(dim(x)) == 2L
}

#' @export
`[.draws_matrix` <- function(x, i, j, ..., drop = FALSE) {
  # TODO: add a warning that 'drop' is ignored?
  out <- NextMethod("[", drop = FALSE)
  class(out) <- class(x)
  out
}
