#' Transform to `draws` objects
#'
#' Try to transform an \R object to a format supported by the \pkg{posterior}
#' package.
#'
#' @name draws
#' @family formats
#'
#' @template args-methods-x
#' @template args-methods-dots
#'
#' @details The class `"draws"` is the parent class of all supported formats,
#'   which also have their own subclasses of the form `"draws_{format}"` (e.g.
#'   `"draws_array"`).
#'
#' @return If possible, a `draws` object in the closest supported format to `x`.
#'   The formats are linked to in the **See Also** section below.
#'
#' @examples
#' # create some random draws
#' x <- matrix(rnorm(30), nrow = 10)
#' colnames(x) <- c("a", "b", "c")
#' str(x)
#'
#' # transform to a draws object
#' y <- as_draws(x)
#' str(y)
#'
#' # remove the draws classes from the object
#' class(y) <- class(y)[-(1:2)]
#' str(y)
#'
NULL

#' @rdname draws
#' @export
as_draws <- function(x, ...) {
  UseMethod("as_draws")
}

#' @export
as_draws.draws <- function(x, ...) {
  x
}

#' @export
as_draws.default <- function(x, ...) {
  # transform an object to the closest supported draws format
  format <- closest_draws_format(x)
  fun_name <- paste0("as_", format)
  if (!has_s3_method(fun_name, class(x))) {
    # if there is no implementation of as_draws_XXX() for this class, then
    # we can't call as_draws_XXX() here as it will end up in as_draws_XXX.default()
    # which will call back to as_draws.default(), creating an infinite loop. So
    # we call down to .as_draws_XXX() instead.
    fun_name <- paste0(".", fun_name)
  }
  fun <- get(fun_name, asNamespace("posterior"))
  fun(x, ...)
}

#' @export
as_draws.rvar <- function(x, ...) {
  as_draws_rvars(x, ...)
}

# detect the supported format closest to the format of the input
closest_draws_format <- function(x) {
  if (is_draws_matrix_like(x)) {
    out <- "matrix"
  } else if (is_draws_array_like(x)) {
    out <- "array"
  } else if (is_draws_df_like(x)) {
    out <- "df"
  } else if (is_draws_rvars_like(x)) {
    out <- "rvars"
  } else if (is_draws_list_like(x)) {
    out <- "list"
  } else {
    stop_no_call(
      "Don't know how to transform an object of class '",
      class(x)[1L], "' to any supported draws format."
    )
  }
  paste0("draws_", out)
}

#' @rdname draws
#' @export
is_draws <- function(x) {
  inherits(x, "draws")
}

# check if an object is supported by the posterior package
# the name 'check_draws' is already in use for checking
# the validity of the 'draw' argument in 'subset'
check_draws_object <- function(x) {
  if (!is_draws(x)) {
    stop_no_call("The object is not in a format supported by posterior.")
  }
  x
}

#' check all variables in an object are numeric, converting non-numeric
#' to numeric with a warning. Used when converting to draws_array or
#' draws_matrix formats (which don't support non-numeric variables).
#' @param x A draws_df, draws_list, or draws_rvars object
#' @param is_non_numeric function that checks if a variable is non-numeric
#' @param convert convert non-numeric variables to numeric?
#' @noRd
check_variables_are_numeric <- function(
  x, to = "draws_array",
  is_non_numeric = function(x_i) !is.numeric(x_i) && !is.logical(x_i),
  convert = TRUE
) {

  non_numeric_cols <- vapply(x, is_non_numeric, logical(1))
  if (any(non_numeric_cols)) {
    warning_no_call(
      to,
      " does not support non-numeric variables (e.g., factors). Converting non-numeric variables to numeric."
    )
  }
  if (convert) {
    x[, non_numeric_cols] <- lapply(unclass(x)[non_numeric_cols], as.numeric)
  }
  x
}

# define default variable names
# use the 'unique' naming strategy of tibble
# @param nvariables number of variables
default_variables <- function(nvariables) {
  sprintf("...%s", seq_len(nvariables))
}

# validate draws vectors per variable
# @param ... Named arguments containing numeric vector
# @return a named list of numeric vectors
validate_draws_per_variable <- function(...) {
  out <- list(...)
  if (!rlang::is_named(out)) {
    stop_no_call("All variables must be named.")
  }
  if (".nchains" %in% names(out)) {
    # '.nchains' is an additional argument in chain supporting formats
    stop_no_call("'.nchains' is not supported for this format.")
  }
  out <- lapply(out, as.numeric)
  ndraws_per_variable <- lengths(out)
  ndraws <- max(ndraws_per_variable)
  if (!all(ndraws_per_variable %in% c(1, ndraws))) {
    stop_no_call("Number of draws per variable needs to be 1 or ", ndraws, ".")
  }
  for (i in which(ndraws_per_variable == 1)) {
    out[[i]] <- rep(out[[i]], ndraws)
  }
  out
}
