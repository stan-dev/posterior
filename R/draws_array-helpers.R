# is an object looking like a 'draws_array' object?
is_draws_array_like <- function(x) {
  is.array(x) && length(dim(x)) == 3L
}

#' @export
`[.draws_array` <- function(x, i, j, ..., drop = FALSE) {
  # TODO: add a warning that 'drop' is ignored?
  out <- NextMethod("[", drop = FALSE)
  class(out) <- class(x)
  out
}

# TODO: do we want to support this '$' method at all?
# select draws of a single parameter
#' @export
`$.draws_array` <- function(x, name) {
  class(x) <- "array"
  out <- x[, , name, drop = FALSE]
  # selectively drop the third dimension
  dim(out) <- dim(x)[1:2]
  dimnames(out) <- dimnames(x)[1:2]
  out
}

