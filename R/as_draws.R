#' Transform to `draws` objects
#'
#' Try to transform an \R object to a `draws` object
#' supported by the \pkg{posterior} package.
#'
#' @param x An \R object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @details The `draws` class is the parent class of
#'   all draws formats supported by the \pkg{posterior} package.
#'
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
  fun <- get(paste0(".as_", format), asNamespace("posterior"))
  fun(x, ...)
}

# detect the supported format closest to the format of the input
closest_draws_format <- function(x) {
  if (is_draws_matrix_like(x)) {
    out <- "matrix"
  } else if (is_draws_array_like(x)) {
    out <- "array"
  } else if (is_draws_df_like(x)) {
    out <- "df"
  } else if (is_draws_list_like(x)) {
    out <- "list"
  }
  else {
    stop2("Don't know how to transform an object of class ",
          "'", class(x)[1L], "' to any supported draws format.")
  }
  paste0("draws_", out)
}

#' @rdname as_draws
#' @export
is_draws <- function(x) {
  inherits(x, "draws")
}

# check if an object is supported by the posterior package
# the name 'check_draws' is already in use for checking
# the validity of the 'draw' argument in 'subset'
check_draws_object <- function(x) {
  if (!is_draws(x)) {
    stop2("The object is not in a format supported by posterior.")
  }
  x
}

# define default variable names
# use the 'unique' naming strategy of tibble
# @param nvariables number of variables
default_variables <- function(nvariables) {
  paste0("...", seq_len(nvariables))
}
