.subset_draws <- function(x, draws, format = NULL, ...) {
  format <- format %||% detect_draws_format(format)
  if (format == "matrix") {
    out <- x[draws, ]
  } else if (format == "array") {
    # use iteration and draws interchangably here?
    out <- x[draws, , ]
  } else {
    stop2("Format '", format, "' is not supported.")
  }
  out
}

.subset_variables <- function(x, variables, format = NULL, ...) {
  format <- format %||% detect_draws_format(format)
  if (format == "matrix") {
    out <- x[, variables]
  } else if (format == "array") {
    out <- x[, , variables]
  } else {
    stop2("Format '", format, "' is not supported.")
  }
  out
}

# Extract posterior matrix of a single parameter
#
# extract a draws x chain matrix for a single parameter
# required for various convergence diagnostics.
#
# @param x A posterior object.
# @param variable A parameter name to extract draws for.
#
# @export
.extract_one_variable_matrix <- function(x, variable, format = NULL, ...) {
  variable <- as_one_character(variable)
  format <- format %||% detect_draws_format(format)
  out <- .subset_variables(x, variable, format = format)
  out <- transform_posterior_draws(out, from = format, to = "array")
  rray_squeeze(out, axes = 3L)
}
