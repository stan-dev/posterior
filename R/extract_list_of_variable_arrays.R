#' Extract arrays of multiple variables
#'
#' Extract arrays of draws for multiple variables, returning them as a named list
#' of arrays. Each array has the same structure as returned by 
#' \code{\link{extract_variable_array}}.
#'
#' @template args-methods-x
#' @param variables A character vector of variable names to extract, or NULL to
#'   extract all variables. To extract all dimensions from variables with 
#'   indices (e.g. \code{"x[1]"}), provide the base variable names 
#'   (e.g. \code{"x"}).
#' @template args-methods-dots
#' @returns
#' A named list of arrays, where each array has dimension
#' `niterations(x)` x `nchains(x)` x any remaining
#' dimensions determined by the indices of the variable `x`
#' (if `with_chains = TRUE`) or dimension
#' `niterations(x) * nchains(x)` x any remaining dimensions
#' (if `with_chains = FALSE`).
#' @family variable extraction methods
#' @examples
#' x <- example_draws(example = "multi_normal")
#'
#' # Extract multiple variables at once
#' vars <- extract_list_of_variable_arrays(x, c("mu", "Sigma"))
#' str(vars)
#'
#' # Extract all variables (uses base variable names)
#' all_vars <- extract_list_of_variable_arrays(x)
#' str(all_vars)
#'
#' # Extract specific indexed variables
#' vars2 <- extract_list_of_variable_arrays(x, c("mu[1]", "mu[2]"))
#' str(vars2)
#'
#' @export
extract_list_of_variable_arrays <- function(x, variables = NULL, ...) {
  UseMethod("extract_list_of_variable_arrays")
}

#' @rdname extract_list_of_variable_arrays
#' @export
extract_list_of_variable_arrays.default <- function(x, variables = NULL, ...) {
  x <- as_draws(x)
  extract_list_of_variable_arrays(x, variables, ...)
}

#' @rdname extract_list_of_variable_arrays
#' @export
extract_list_of_variable_arrays.draws <- function(x, variables = NULL, ...) {
  if (is.null(variables)) {
    # Get unique base variable names (without indices) when variables = NULL
    all_vars <- variables(x)
    variables <- unique(split_variable_names(all_vars)$base_name)
  }
  
  if (!is.character(variables)) {
    stop_no_call("'variables' must be a character vector.")
  }
  
  if (length(variables) == 0) {
    return(list())
  }
  
  out <- lapply(variables, function(var) {
    extract_variable_array(x, variable = var, ...)
  })
  
  names(out) <- variables
  out
}
