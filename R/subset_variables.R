#' @export
subset_variables <- function(x, variables, ...) {
  UseMethod("subset_variables")
}

#' @export
subset_variables.default <- function(x, variables, ...) {
  x <- as_detected_draws_format(x)
  subset_variables(x, variables, ...)
}

#' @export
subset_variables.draws_matrix <- function(x, variables, ...) {
  x[, variables]
}

#' @export
subset_variables.draws_array <- function(x, variables, ...) {
  x[, , variables]
}
