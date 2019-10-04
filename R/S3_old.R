# file kept for reference; should eventually be removed
# create a new posterior_draws object
new_posterior_draws <- function(x) {
  format <- detect_posterior_format(x)
  if (isNA(format)) {
    format <- forecast_posterior_format(x)
    draws <- as_posterior_format(x, format = format)
  } else {
    # 'x' is already in a supported format
    draws <- x
  }
  # TODO: what else to store in the object?
  # TODO: if we store iterations and chains, we will not loose this information
  # even if we transform to a format that does not have that information anymore
  # such as the 'matrix' format
  out <- nlist(draws, format)
  # TODO: name class 'posterior_draws' or 'posterior'?
  class(out) <- "posterior_draws"
  out
}

#' Attempt to transform to a posterior_draws object
#'
#' @param x An \R object to be transformed.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
as_posterior_draws <- function(x, ...) {
  UseMethod("as_posterior_draws")
}

#' @export
as_posterior_draws.default <- function(x, ...) {
  new_posterior_draws(x)
}

#' @export
as_posterior_draws.posterior_draws <- function(x, ...) {
  x
}

# transform a posterior_draws object to another format
transform_posterior_draws <- function(x, format, ...) {
  assert_class(x, "posterior_draws")
  assert_choice(format, all_posterior_formats())
  if (format != x$format) {
    x$draws <- transform_posterior_format(x$draws, from = x$format, to = format)
    x$format <- format
  }
  x
}

#' @export
to_posterior_matrix <- function(x, ...) {
  transform_posterior_draws(x, format = "matrix", ...)
}

#' @export
to_posterior_array <- function(x, ...) {
  transform_posterior_draws(x, format = "array", ...)
}

#' @export
print.posterior_draws <- function(x, ...) {
  cat0("Format: ", x$format, "\n\n")
  # TODO: add pretty printing for draws
  print(x$draws)
  invisible(x)
}

# detect the format in which posterior draws are stored
detect_posterior_format <- function(x) {
  out <- NA
  if (is_posterior_matrix(x)) {
    out <- "matrix"
  } else if (is_posterior_array(x)) {
    out <- "array"
  }
  out
}

# forecast the format to which the input can likely be transformed
forecast_posterior_format <- function(x) {
  if (is_matrix_like(x)) {
    out <- "matrix"
  } else if (is_3d_array_like(x)) {
    out <- "array"
  } else {
    stop2("Don't know how to transform an object of class ",
          "'", class(x)[1L], "' to any supported posterior format.")
  }
  out
}

# try to convert any R object to a suitable posterior draws format
#' @import rray
as_posterior_format <- function(x, format = NULL) {
  format <- format %||% forecast_draws_format(x)
  if (format == "matrix") {
    message("Converting to a posterior matrix.")
    x <- as_rray(x)
    new_dimnames <- list(draw = NULL, variable = NULL)
    if (!is.null(dimnames(x)[[2]])) {
      new_dimnames[[2]] <- dimnames(x)[[2]]
    } else {
      # TODO: how format call variables by default?
      new_dimnames[[2]] <- paste0("variable", seq_cols(x))
    }
    # TODO: use existing row names in any way?
    new_dimnames[[1]] <- as.character(seq_rows(x))
    dimnames(x) <- new_dimnames
  } else if (format == "array") {
    message("Converting to a posterior array.")
    x <- as_rray(x)
    new_dimnames <- list(iteration = NULL, chain = NULL, variable = NULL)
    if (!is.null(dimnames(x)[[3]])) {
      new_dimnames[[3]] <- dimnames(x)[[3]]
    } else {
      # TODO: how format call parameters by default?
      new_dimnames[[3]] <- paste0("variable", seq_dim(x, 3))
    }
    # TODO: use existing row/col names in any way?
    new_dimnames[[1]] <- as.character(seq_rows(x))
    new_dimnames[[2]] <- as.character(seq_cols(x))
    dimnames(x) <- new_dimnames
  } else {
    stop2("Format '", format, "' is not supported.")
  }
  x
}

# all possible posterior represenations
all_posterior_formats <- function() {
  c("matrix", "array")
}

#' @export
is_posterior_matrix <- function(x) {
  is_rray(x) && is_equal(names(dimnames(x)), c("draw", "variable"))
}

#' @export
is_posterior_array <- function(x) {
  is_rray(x) && is_equal(names(dimnames(x)), c("iteration", "chain", "variable"))
}

is_matrix_like <- function(x) {
  is.matrix(x) || is_rray(x) && length(dim(x)) == 2L
}

is_3d_array_like <- function(x) {
  (is.array(x) || is_rray(x)) && length(dim(x)) == 3L
}

# tranform posterior object to another representation
transform_posterior_format <- function(x, to, from = NULL) {
  from <- from %||% detect_draws_format(x)
  assert_choice(to, all_posterior_formats())
  assert_choice(from, all_posterior_formats())
  fun <- paste0("posterior_", from, "_to_", to)
  fun <- get(fun, pos = asNamespace("posterior"))
  fun(x)
}

posterior_matrix_to_matrix <- function(x) {
  x
}

posterior_matrix_to_array <- function(x) {
  old_dim <- dim(x)
  old_dimnames <- dimnames(x)
  x <- rray_reshape(x, dim = c(old_dim[1], 1, old_dim[2]))
  dimnames(x) <- list(
    iteration = as.character(seq_rows(x)),
    chain = "1",
    variable = old_dimnames[[2]]
  )
  x
}

posterior_array_to_matrix <- function(x) {
  old_dim <- dim(x)
  old_dimnames <- dimnames(x)
  x <- rray_reshape(x, dim = c(old_dim[1] * old_dim[2], old_dim[3]))
  dimnames(x) <- list(
    draw = as.character(seq_rows(x)),
    variable = old_dimnames[[3]]
  )
  x
}

posterior_array_to_array <- function(x) {
  x
}

# @export
# subset_draws <- function(x, draws, ...) {
#   assert_class(x, "posterior_draws")
#   x$draws <- .subset_draws(x$draws, draws, format = x$format, ...)
#   x
# }

# helper function to subset draws for any posterior format
# .subset_draws <- function(x, draws, format = NULL, ...) {
#   format <- format %||% detect_draws_format(format)
#   if (format == "matrix") {
#     out <- x[draws, ]
#   } else if (format == "array") {
#     # use iteration and draws interchangably here?
#     out <- x[draws, , ]
#   } else {
#     stop2("Format '", format, "' is not supported.")
#   }
#   out
# }

# @export
# subset_variables <- function(x, variables, ...) {
#   assert_class(x, "posterior_draws")
#   x$draws <- .subset_variables(x$draws, variables, format = x$format, ...)
#   x
# }

# helper function to subset variables for any posterior format
# .subset_variables <- function(x, variables, format = NULL, ...) {
#   format <- format %||% detect_draws_format(format)
#   if (format == "matrix") {
#     out <- x[, variables]
#   } else if (format == "array") {
#     out <- x[, , variables]
#   } else {
#     stop2("Format '", format, "' is not supported.")
#   }
#   out
# }

# Extract posterior matrix of a single parameter
#
# extract a iteration x chain matrix for a single parameter
# required for various convergence diagnostics.
#
# @param x A posterior_draws object.
# @param variable A parameter name to extract draws for.
#
# @export
# extract_one_variable_matrix <- function(x, variable, ...) {
#   .extract_one_variable_matrix(x$draws, variable, format = x$format, ...)
# }

# helper function to extract draws for a single variable
# .extract_one_variable_matrix <- function(x, variable, format = NULL, ...) {
#   variable <- as_one_character(variable)
#   format <- format %||% detect_draws_format(format)
#   out <- .subset_variables(x, variable, format = format)
#   out <- transform_posterior_format(out, from = format, to = "array")
#   rray_squeeze(out, axes = 3L)
# }

