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
extract_one_variable_matrix.draws <- function(x, variable, ...) {
  variable <- as_one_character(variable)
  out <- subset(x, variable = variable)
  out <- as_draws_array(out)
  out <- drop_dims(out, dims = 3)
  class(out) <- "matrix"
  out
}
