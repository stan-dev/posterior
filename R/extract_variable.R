#' Extract draws of a single variable
#'
#' Extract a vector of draws of a single variable.
#'
#' @template args-methods-x
#' @param variable (string) The name of the variable to extract.
#' @template args-methods-dots
#' @return A numeric vector of length equal to the number of draws.
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

#' @rdname extract_variable
#' @export
extract_variable.draws_rvars <- function(x, variable, ...) {
  variable <- as_one_character(variable)
  variable_regex <- regexec("^(.*)\\[.*\\]$", variable)
  if (!isTRUE(variable_regex[[1]] == -1)) {
    # regex match => variable with indices in the name ("x[1]", etc), which
    # can't be subset from draws_rvars directly, so we'll convert to a
    # draws_array first. root_variable is "x" when variable is "x[...]"
    root_variable <- regmatches(variable, variable_regex)[[1]][[2]]
    out <- extract_variable(as_draws_array(x[root_variable]), variable, ...)
  } else if (length(x[[variable]]) > 1) {
    stop_no_call(
      'Cannot extract non-scalar value using extract_variable():\n',
      '  "', variable, '" has dimensions: [', paste0(dim(x[[variable]]), collapse = ","), ']\n',
      '  Try including brackets ("[]") and indices in the variable name to extract a scalar value.'
    )
  } else {
    out <- NextMethod()
  }
  out
}
