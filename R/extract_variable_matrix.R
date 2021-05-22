#' Extract matrix of a single variable
#'
#' Extract an iterations x chains matrix of draws of a single variable.
#' This is primarily used for convergence diagnostic functions such as [rhat()].
#'
#' @template args-methods-x
#' @param variable (string) The name of the variable to extract.
#' @template args-methods-dots
#' @return A `matrix` with dimension iterations x chains.
#'
#' @examples
#' x <- example_draws()
#' mu <- extract_variable_matrix(x, variable = "mu")
#' dim(mu)
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
  out <- .subset_draws(x, variable = variable, reserved = FALSE)
  out <- as_draws_array(out)
  out <- drop_dims_or_classes(out, dims = 3, reset_class = TRUE)
  class(out) <- "matrix"
  out
}

#' @rdname extract_variable_matrix
#' @export
extract_variable_matrix.draws_rvars <- function(x, variable, ...) {
  variable <- as_one_character(variable)
  variable_regex <- regexec("^(.*)\\[.*\\]$", variable)
  if (!isTRUE(variable_regex[[1]] == -1)) {
    # regex match => variable with indices in the name ("x[1]", etc), which
    # can't be subset from draws_rvars directly, so we'll convert to a
    # draws_array first. root_variable is "x" when variable is "x[...]"
    root_variable <- regmatches(variable, variable_regex)[[1]][[2]]
    extract_variable_matrix(as_draws_array(x[root_variable]), variable, ...)
  } else if (length(x[[variable]]) > 1) {
    stop_no_call(
      'Cannot extract non-scalar value using extract_variable_matrix():\n',
      '  "', variable, '" has dimensions: [', paste0(dim(x[[variable]]), collapse = ","), ']\n',
      '  Try including brackets ("[]") and indices in the variable name to extract a scalar value.'
    )
  } else {
    NextMethod()
  }
}
