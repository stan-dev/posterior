#' @export
as_draws_array <- function(x, ...) {
  UseMethod("as_draws_array")
}

#' @export
as_draws_array.default <- function(x, ...) {
  x <- as_closest_draws_format(x)
  as_draws_array(x, ...)
}

#' @export
as_draws_array.draws_matrix <- function(x, ...) {
  old_dim <- dim(x)
  old_dimnames <- dimnames(x)
  dim(x) <- c(old_dim[1], 1, old_dim[2])
  dimnames(x) <- list(
    iteration = as.character(seq_rows(x)),
    chain = "1",
    variable = old_dimnames[[2]]
  )
  class(x) <- c("draws_array", "matrix")
  x
}

#' @export
as_draws_array.draws_array <- function(x, ...) {
  x
}

.as_draws_array <- function(x, ...) {
  message("Converting to a posterior array.")
  x <- as.array(x)
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
  class(x) <- c("draws_array", "array")
  x
}
