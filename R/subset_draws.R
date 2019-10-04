#' @export
subset_draws <- function(x, draws, ...) {
  UseMethod("subset_draws")
}

#' @export
subset_draws.default <- function(x, draws, ...) {
  x <- as_detected_draws_format(x)
  subset_draws(x, draws, ...)
}

#' @export
subset_draws.draws_matrix <- function(x, draws, ...) {
  x[draws, ]
}

#' @export
subset_draws.draws_array <- function(x, draws, ...) {
  x[draws, , ]
}
