#' @export
gradients <- function(x, ...) {
  UseMethod("gradients")
}

#' @rdname gradients
#' @export
gradients.draws <- function(x, ...) {
  vars <- regex_reserved_variables(".gradient")
  vars <- check_existing_variables(vars, x, regex = TRUE)
  out <- subset_draws(x, variable = vars, reserved = FALSE)
  out <- as_draws_matrix(out)
  class(out) <- "matrix"
  out
}
