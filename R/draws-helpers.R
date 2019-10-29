# detect the supported format closest to the format of the input
closest_draws_format <- function(x) {
  if (is_draws_matrix_like(x)) {
    out <- "matrix"
  } else if (is_draws_array_like(x)) {
    out <- "array"
  } else if (is_draws_data_frame_like(x)) {
    out <- "data_frame"
  } else if (is_draws_list_like(x)) {
    out <- "list"
  }
  else {
    stop2("Don't know how to transform an object of class ",
          "'", class(x)[1L], "' to any supported draws format.")
  }
  paste0("draws_", out)
}

# transform an object to the closest supported draws format
as_closest_draws_format <- function(x) {
  if (is_draws(x)) {
    # 'x' is already in a supported format
    return(x)
  }
  format <- closest_draws_format(x)
  fun <- get(paste0(".as_", format), asNamespace("posterior"))
  fun(x)
}

#' Check if argument is a \code{draws} object
#'
#' Check if argument is a \code{draws} object, which constitutes the
#' parent class of all draws formats supported by \pkg{posterior}.
#'
#' @param x An \R object.
#' @export
is_draws <- function(x) {
  inherits(x, "draws")
}

# check if an object is supported by the posterior package
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
