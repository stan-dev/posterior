#' Extract draws of a single variable
#'
#' Extract a vector of draws of a single variable.
#'
#' @template args-methods-x
#' @template args-extract-variable
#' @template args-methods-dots
#' @return A vector of length equal to the number of draws.
#' @family variable extraction methods
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
extract_variable.draws_df <- function(x, variable, ...) {
  variable <- as_one_character(variable)
  out <- .subset_draws(x, variable = variable, reserved = FALSE)
  out[[variable]]
}

#' @rdname extract_variable
#' @export
extract_variable.draws_list <- function(x, variable, ...) {
  variable <- as_one_character(variable)
  out <- .subset_draws(x, variable = variable, reserved = FALSE)
  out <- as_draws_df(out)
  out[[variable]]
}

#' @rdname extract_variable
#' @export
extract_variable.draws_rvars <- function(x, variable, ...) {
  variable <- as_one_character(variable)
  parts <- split_variable_names(variable)
  if (isTRUE(nzchar(parts$indices))) {
    # variable with indices in the name ("x[1]", etc), which can't be subset
    # from draws_rvars directly, so we'll convert to a draws_df first.
    out <- extract_variable(as_draws_df(x[parts$base_name]), variable = variable, ...)
  } else if (length(x[[variable]]) > 1) {
    stop_no_call(
      'Cannot extract non-scalar value using extract_variable():\n',
      '  "', variable, '" has dimensions: [', paste0(dim(x[[variable]]), collapse = ","), ']\n',
      '  Try including brackets ("[]") and indices in the variable name to extract a scalar value.'
    )
  } else {
    # scalar
    out <- unname(drop(draws_of(x[[variable]])))
  }
  out
}
