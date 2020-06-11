#' Extract matrix of a single variable
#'
#' Extract a matrix of draws of a single variable with dimension
#' #iterations x #chains, primarily for use in convergence functions
#' such as [rhat()].
#'
#' @template args-methods-x
#' @param variable Name of a single variable to extract draws for.
#' @template args-methods-dots
#' @return An `matrix` with dimension #iterations x #chains.
#'
#' @examples
#' x <- example_draws()
#' mu <- extract_variable_matrix(x, variable = "mu")
#' rhat(mu)
#'
#' @export
extract_variable_matrix <- function(x, variable, ...) {
  UseMethod("extract_variable_matrix")
}

#' @rdname extract_variable_matrix
#' @export
extract_variable_matrix.default <- function(x, variable, ...) {
  x <- as_draws(x)
  extract_variable_matrix(x, variable, ...)
}

#' @rdname extract_variable_matrix
#' @export
extract_variable_matrix.draws <- function(x, variable, ...) {
  variable <- as_one_character(variable)
  out <- subset_draws(x, variable = variable)
  out <- as_draws_array(out)
  out <- drop_dims(out, dims = 3)
  class(out) <- "matrix"
  out
}
