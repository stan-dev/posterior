#' Extract array of a single (possibly indexed) variable
#'
#' Extract an array of draws of a single variable, including any dimensions of
#' variables with indices.
#'
#' @template args-methods-x
#' @param variable (string) The name of the variable to extract. To extract all
#' dimensions from variables with indices (e.g. `"x[1]"`), provide the base
#' variable name (e.g. `"x"`).
#' @template args-methods-dots
#' @returns
#' An `array` with dimension `niterations(x)` x `nchains(x)` x any remaining
#' dimensions determined by the indices of the variable `x`.
#' @family variable extraction methods
#' @examples
#' x <- example_draws(example = "multi_normal")
#'
#' mu <- extract_variable_array(x, variable = "mu")
#' str(mu)
#'
#' mu1 <- extract_variable_array(x, variable = "mu[1]")
#' str(mu1)
#'
#' Sigma <- extract_variable_array(x, variable = "Sigma")
#' str(Sigma)
#'
#' @export
extract_variable_array <- function(x, variable, ...) {
  UseMethod("extract_variable_array")
}

#' @rdname extract_variable_array
#' @export
extract_variable_array.default <- function(x, variable, ...) {
  x <- as_draws(x)
  extract_variable_array(x, variable, ...)
}

#' @rdname extract_variable_array
#' @export
extract_variable_array.draws <- function(x, variable, ...) {
  variable <- as_one_character(variable)

  if (isTRUE(nzchar(split_variable_names(variable)$indices))) {
    # indices provided => scalar => equivalent to extract_variable_matrix
    out <- extract_variable_matrix(x, variable, ...)
    dim(out) <- c(dim(out), 1)
    dimnames(out) <- list(NULL)
  } else {
    x <- subset_draws(x, variable = variable, reserved = FALSE)
    x <- as_draws_rvars(x)
    out <- draws_of(x[[variable]], with_chains = TRUE)
  }

  out
}
