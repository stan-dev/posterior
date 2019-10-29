#' The `draws_matrix` format
#'
#' @name draws_matrix
#' @family formats
#'
#' @templateVar draws_format draws_matrix
#' @templateVar base_class "matrix"
#' @template draws_format-skeleton
#'
#' @details Objects of class `"draws_matrix"` are matrices (2-D arrays) with
#'   dimensions `"draw"` and `"variable"`. This format does not store any
#'   information about which MCMC chain the draws are from and so we simply
#'   refer to the rows as draws and not iterations (unlike for the
#'   [`draws_array`] format). See **Examples**.
#'
NULL

#' @rdname draws_matrix
#' @export
as_draws_matrix <- function(x, ...) {
  UseMethod("as_draws_matrix")
}

#' @rdname draws_matrix
#' @export
as_draws_matrix.default <- function(x, ...) {
  x <- as_draws(x)
  as_draws_matrix(x, ...)
}

#' @rdname draws_matrix
#' @export
as_draws_matrix.draws_matrix <- function(x, ...) {
  x
}

#' @rdname draws_matrix
#' @export
as_draws_matrix.draws_array <- function(x, ...) {
  old_dim <- dim(x)
  old_dimnames <- dimnames(x)
  dim(x) <- c(old_dim[1] * old_dim[2], old_dim[3])
  dimnames(x) <- list(
    draw = as.character(seq_rows(x)),
    variable = old_dimnames[[3]]
  )
  class(x) <- class_draws_matrix()
  x
}

#' @rdname draws_matrix
#' @export
as_draws_matrix.draws_df <- function(x, ...) {
  draws <- x$.draw
  x <- remove_meta_columns(x)
  class(x) <- class(x)[-1L]
  x <- .as_draws_matrix(x)
  rownames(x) <- draws
  x
}

#' @rdname draws_matrix
#' @export
as_draws_matrix.draws_list <- function(x, ...) {
  x <- as_draws_df(x)
  as_draws_matrix(x, ...)
}

# try to convert any R object into a 'draws_matrix' object
.as_draws_matrix <- function(x) {
  x <- as.matrix(x)
  new_dimnames <- list(draw = NULL, variable = NULL)
  if (!is.null(dimnames(x)[[2]])) {
    new_dimnames[[2]] <- dimnames(x)[[2]]
  } else {
    new_dimnames[[2]] <- default_variables(NCOL(x))
  }
  check_reserved_variables(new_dimnames[[2]])
  new_dimnames[[1]] <- as.character(seq_rows(x))
  dimnames(x) <- new_dimnames
  class(x) <- class_draws_matrix()
  x
}

class_draws_matrix <- function() {
  c("draws_matrix", "draws", "matrix")
}
