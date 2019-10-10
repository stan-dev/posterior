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
  variable <- as_one_character(variable)
  out <- x[, variable]
  dimnames(out) <- c(dimnames(x)[1], list(chain = "1"))
  class(out) <- "matrix"
  out
}

#' @export
extract_one_variable_matrix.draws_array <- function(x, variable, ...) {
  variable <- as_one_character(variable)
  out <- drop_dims(x[, , variable], dims = 3)
  class(out) <- "matrix"
  out
}

#' @export
extract_one_variable_matrix.draws_data_frame <- function(x, variable, ...) {
  variable <- as_one_character(variable)
  out <- subset(x, variables = variable)
  out <- as_draws_array(out)
  extract_one_variable_matrix(out, variable)
}
