#' Name variables in `draws` objects
#'
#' Name all variables in a [`draws`] object. Useful when using pipe
#' operators.
#'
#' @param .x (draws) A [`draws`] object.
#' @param variables (character) new variable names.
#' @template args-methods-dots
#'
#' @return Returns a [`draws`] object of the same format as `.x`, with
#'   variables named as specified.
#'
#' @seealso [`variables`]
#' @examples
#' x <- as_draws(matrix(rnorm(100), ncol = 2))
#' variables(x)
#' 
#' x |> set_variables(c("theta[1]", "theta[2]")) |> variables()
#'
#' # this is equivalent to
#' variables(x) <- c("theta[1]", "theta[2]")
#' variables(x)
#' 
#' @export
set_variables <- function(.x, ...) {
  UseMethod("set_variables")
}

#' @rdname set_variables
#' @export
set_variables.draws <- function(.x, variables, ...) {
  variables(.x) <- variables
  return(.x)
}
