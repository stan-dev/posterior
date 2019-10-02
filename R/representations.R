#' @export
posterior <- R6::R6Class(
  classname = "posterior",
  public = list(
    initialize = function(x) {
      private$ps_format <- detect_draws_format(x)
      if (private$ps_format == "unknown") {
        private$ps_format <- forecast_draws_format(x)
        private$ps_draws <- as_posterior_draws(x, to = private$ps_format)
      } else {
        # 'x' is already in a supported format
        private$ps_draws <- x
      }
    },
    # extractor functions
    draws = function() {
      private$ps_draws
    },
    format = function() {
      private$ps_format
    },
    extract_one_variable_matrix = function(variable) {
      .extract_one_variable_matrix(
        private$ps_draws, variable = variable,
        format = private$ps_format
      )
    },
    # tranformation functions
    to_matrix = function() {
      private$ps_draws <- transform_posterior_draws(
        private$ps_draws, from = private$ps_format, to = "matrix"
      )
      private$ps_format <- "matrix"
      invisible(self)
    },
    to_array = function() {
      private$ps_draws <- transform_posterior_draws(
        private$ps_draws, from = private$ps_format, to = "array"
      )
      private$ps_format <- "array"
      invisible(self)
    },
    # subsetting functions
    subset_draws = function(draws) {
      private$ps_draws <- .subset_draws(
        private$ps_draws, draws, format = private$ps_format
      )
      invisible(self)
    },
    subset_variables = function(variables) {
      private$ps_draws <- .subset_variables(
        private$ps_draws, variables, format = private$ps_format
      )
      invisible(self)
    }
  ),
  private = list(
    ps_draws = NULL,
    ps_format = NULL
  )
)

# detect the format in which posterior draws are stored
detect_draws_format <- function(x) {
  if (is_posterior_matrix(x)) {
    out <- "matrix"
  } else if (is_posterior_array(x)) {
    out <- "array"
  } else {
    out <- "unknown"
  }
  out
}

# forecast the format to which the input can likely be transformed
forecast_draws_format <- function(x) {
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

# Attempt to transform to a posterior object
#
# @param x An \R object to be converted.
# @param class optional posterior class to transform to.
#   If \code{NULL} (the default), the class most similar to the
#   structure of \code{x} is chosen.
#
# @export
# as_posterior_draws <- function(x, class = NULL) {
#   assert_choice(class, all_posterior_classes(), null.ok = TRUE)
#   if (!is_posterior(x)) {
#     x <- .as_posterior(x)
#   }
#   if (!is.null(class)) {
#     x <- posterior_transform(x, class = class)
#   }
#   x
# }

# try to convert any R object to a suitable posterior draws format
#' @import rray
as_posterior_draws <- function(x, to = NULL) {
  to <- to %||% forecast_draws_format(x)
  if (to == "matrix") {
    message("Converting to a posterior matrix.")
    x <- as_rray(x)
    new_dimnames <- list(draw = NULL, variable = NULL)
    if (!is.null(dimnames(x)[[2]])) {
      new_dimnames[[2]] <- dimnames(x)[[2]]
    } else {
      # TODO: how to call variables by default?
      new_dimnames[[2]] <- paste0("variable", seq_cols(x))
    }
    # TODO: use existing row names in any way?
    new_dimnames[[1]] <- as.character(seq_rows(x))
    dimnames(x) <- new_dimnames
  } else if (to == "array") {
    message("Converting to a posterior array.")
    x <- as_rray(x)
    new_dimnames <- list(iteration = NULL, chain = NULL, variable = NULL)
    if (!is.null(dimnames(x)[[3]])) {
      new_dimnames[[3]] <- dimnames(x)[[3]]
    } else {
      # TODO: how to call parameters by default?
      new_dimnames[[3]] <- paste0("variable", seq_dim(x, 3))
    }
    # TODO: use existing row/col names in any way?
    new_dimnames[[1]] <- as.character(seq_rows(x))
    new_dimnames[[2]] <- as.character(seq_cols(x))
    dimnames(x) <- new_dimnames
  } else {
    stop2("Format '", to, "' is not supported.")
  }
  x
}

#' @export
# as_posterior_matrix <- function(x) {
#   as_posterior_draws(x, "matrix")
# }

#' @export
# as_posterior_array <- function(x) {
#   as_posterior_draws(x, "array")
# }

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
