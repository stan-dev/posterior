#' @export
as_draws_matrix <- function(x, ...) {
  UseMethod("as_draws_matrix")
}

#' @export
as_draws_matrix.default <- function(x, ...) {
  x <- as_closest_draws_format(x)
  as_draws_matrix(x, ...)
}

#' @export
as_draws_matrix.draws_matrix <- function(x, ...) {
  x
}

#' @export
as_draws_matrix.draws_array <- function(x, ...) {
  old_dim <- dim(x)
  old_dimnames <- dimnames(x)
  dim(x) <- c(old_dim[1] * old_dim[2], old_dim[3])
  dimnames(x) <- list(
    draw = as.character(seq_rows(x)),
    variable = old_dimnames[[3]]
  )
  class(x) <- c("draws_matrix", "matrix")
  x
}

# try to convert any R object into a 'draws_matrix' object
.as_draws_matrix <- function(x, ...) {
  x <- as.matrix(x)
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
  class(x) <- c("draws_matrix", "matrix")
  x
}

