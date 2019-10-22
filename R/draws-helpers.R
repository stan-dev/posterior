# detect the supported format closest to the format of the input
closest_draws_format <- function(x) {
  if (is_draws_matrix_like(x)) {
    out <- "matrix"
  } else if (is_draws_array_like(x)) {
    out <- "array"
  } else if (is_draws_data_frame_like(x)) {
    out <- "data_frame"
  } else {
    stop2("Don't know how to transform an object of class ",
          "'", class(x)[1L], "' to any supported draws format.")
  }
  paste0("draws_", out)
}

# transform an object to the closest supported draws format
as_closest_draws_format <- function(x) {
  if (is_draws_object(x)) {
    return(x)
  }
  format <- closest_draws_format(x)
  fun <- get(paste0(".as_", format), asNamespace("posterior"))
  fun(x)
}

# is an object in one of the supported draws classes?
is_draws_object <- function(x) {
  is_draws_matrix(x) || is_draws_array(x) || is_draws_data_frame(x)
}

# check if an object is supported by the posterior package
check_draws_object <- function(x) {
  if (!is_draws_object(x)) {
    stop2("'x' is not in a format supported by posterior.")
  }
  x
}
