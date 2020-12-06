#' Extract draws of a single variable
#'
#' Extract a vector of draws of a single variable.
#'
#' @template args-methods-x
#' @param variable Name of a single variable to extract draws for.
#' @template args-methods-dots
#' @return A numeric vector of length #draws.
#'
#' @examples
#' x <- example_draws()
#' mu <- extract_variable(x, variable = "mu")
#' str(mu)
#'
#' @export
extract_variable <- function(x, variable, ...) {
  UseMethod("extract_variable")
}

#' @rdname extract_variable
#' @export
extract_variable.default <- function(x, variable, ...) {
  x <- as_draws(x)
  extract_variable(x, variable, ...)
}

#' @rdname extract_variable
#' @export
extract_variable.draws <- function(x, variable, ...) {
  variable <- as_one_character(variable)
  out <- .subset_draws(x, variable = variable, reserved = FALSE)
  out <- as_draws_matrix(out)
  as.vector(out)
}
