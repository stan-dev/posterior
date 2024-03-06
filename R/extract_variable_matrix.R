#' Extract matrix of a single variable
#'
#' Extract an iterations x chains matrix of draws of a single variable.
#' This is primarily used for convergence diagnostic functions such as [rhat()].
#'
#' @template args-methods-x
#' @template args-extract-variable
#' @template args-methods-dots
#' @return A `matrix` with dimension iterations x chains.
#' @family variable extraction methods
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
extract_variable_matrix.draws_df <- function(x, variable, ...) {
  variable <- as_one_character(variable)
  if (is.factor(x[[variable]])) {
    x_variable_factor <- x[[variable]]
    x[[variable]] <- unclass(x[[variable]])
    out <- copy_levels(x_variable_factor, NextMethod())
  } else {
    out <- NextMethod()
  }
  out
}

#' @rdname extract_variable_matrix
#' @export
extract_variable_matrix.draws_list <- function(x, variable, ...) {
  variable <- as_one_character(variable)
  if (is.factor(x[[1]][[variable]])) {
    x_variable_factor <- x[[1]][[variable]]
    for (i in seq_along(x)) {
      x[[i]][[variable]] <- unclass(x[[i]][[variable]])
    }
    out <- copy_levels(x_variable_factor, NextMethod())
  } else {
    out <- NextMethod()
  }
  out
}

#' @rdname extract_variable_matrix
#' @export
extract_variable_matrix.draws_rvars <- function(x, variable, ...) {
  variable <- as_one_character(variable)
  parts <- split_variable_names(variable)

  .draws <- draws_of(x[[parts$base_name]])
  if (is.factor(.draws)) {
    # if x is a factor rvar, convert it to numeric before extracting and
    # then we'll add levels back at the end
    x[[parts$base_name]] <- as_rvar_integer(x[[parts$base_name]])
  }

  if (isTRUE(nzchar(parts$indices))) {
    # variable with indices in the name ("x[1]", etc) can't be subset from
    # draws_rvars directly, so we'll convert to a draws_array first
    out <- extract_variable_matrix(as_draws_array(x[parts$base_name]), variable = variable, ...)
  } else if (length(x[[variable]]) > 1) {
    stop_no_call(
      'Cannot extract non-scalar value using extract_variable_matrix():\n',
      '  "', variable, '" has dimensions: [', paste0(dim(x[[variable]]), collapse = ","), ']\n',
      '  Try including brackets ("[]") and indices in the variable name to extract a scalar value.'
    )
  } else {
    out <- NextMethod()
  }

  copy_levels(.draws, out)
}
