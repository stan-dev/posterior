#' @export
is_draws_list <- function(x) {
  inherits(x, "draws_list")
}

# is an object looking like a 'draws_list' object?
is_draws_list_like <- function(x) {
  # TODO: add more sophisticated checks
  is.list(x)
}
