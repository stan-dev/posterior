# file kept for reference; should eventually be removed
#' @export
posterior <- R6::R6Class(
  classname = "posterior",
  public = list(
    initialize = function(x) {
      private$ps_format <- detect_posterior_format(x)
      if (isNA(private$ps_format)) {
        private$ps_format <- forecast_posterior_format(x)
        private$ps_draws <- as_posterior_format(x, private$ps_format)
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
      private$ps_draws <- transform_posterior_format(
        private$ps_draws, from = private$ps_format, to = "matrix"
      )
      private$ps_format <- "matrix"
      invisible(self)
    },
    to_array = function() {
      private$ps_draws <- transform_posterior_format(
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
