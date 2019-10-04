#' @export
extract_one_variable_matrix <- function(x, variable, ...) {
  UseMethod("extract_one_variable_matrix")
}

#' @export
extract_one_variable_matrix.default <- function(x, variable, ...) {
  x <- as_detected_draws_format(x)
  extract_one_variable_matrix(x, variable, ...)
}

#' @export
extract_one_variable_matrix.draws_matrix <- function(x, variable, ...) {
  out <- x[, variable]
  dimnames(out) <- c(dimnames(x)[1], list(chain = "1"))
  class(out) <- "matrix"
  out
}

#' @export
extract_one_variable_matrix.draws_array <- function(x, variable, ...) {
  out <- x[, , variable]
  # selectively drop the third dimension
  dim(out) <- dim(x)[1:2]
  dimnames(out) <- dimnames(x)[1:2]
  class(out) <- "matrix"
  out
}
