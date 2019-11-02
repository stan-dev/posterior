#' Extract Draws Matrix of one Variable
#'
#' Extract a matrix of draws of a single variable with dimension
#' #iterations x #chains primarily for use in convergence functions
#' such as `\link{rhat}`.
#'
#' @template args-methods-x
#' @param variable Name of a single variable to extract draws for.
#' @template args-methods-dots
#' @return An `matrix` with dimension #iterations x #chains.
#'
#' @examples
#' x <- example_draws()
#' mu <- extract_one_variable_matrix(x, variable = "mu")
#' rhat(mu)
#'
#' @export
extract_one_variable_matrix <- function(x, variable, ...) {
  UseMethod("extract_one_variable_matrix")
}

#' @rdname extract_one_variable_matrix
#' @export
extract_one_variable_matrix.default <- function(x, variable, ...) {
  x <- as_draws(x)
  extract_one_variable_matrix(x, variable, ...)
}

#' @rdname extract_one_variable_matrix
#' @export
extract_one_variable_matrix.draws <- function(x, variable, ...) {
  variable <- as_one_character(variable)
  out <- subset(x, variable = variable)
  out <- as_draws_array(out)
  out <- drop_dims(out, dims = 3)
  class(out) <- "matrix"
  out
}
