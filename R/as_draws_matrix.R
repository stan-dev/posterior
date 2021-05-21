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
as_draws_matrix.data.frame <- function(x, ...) {
  x <- as_draws_df(x)
  if (ndraws(x) == 0) {
    return(empty_draws_matrix(variables(x)))
  }
  x[c(".chain", ".iteration", ".draw")] <- NULL
  as_draws_matrix.matrix(as.matrix(x))
}

#' @rdname draws_matrix
#' @export
as_draws_matrix.default <- function(x, ...) {
  x <- as_draws_df(x)
  as_draws_matrix.data.frame(x, ...)
}

#' @rdname draws_matrix
#' @export
as_draws_matrix.draws_matrix <- function(x, ...) {
  x
}

#' @rdname draws_matrix
#' @export
as_draws_matrix.matrix <- function(x, ...) {
  new_dimnames <- list(draw = seq_len(nrow(x)), variable = dimnames(x)[[2]])
  if (is.null(new_dimnames[[2]])) {
    new_dimnames[[2]] <- default_variables(ncol(x))
  }
  check_new_variables(new_dimnames[[2]])
  dimnames(x) <- new_dimnames
  class(x) <- class_draws_matrix()
  x
}

#' @rdname draws_matrix
#' @export
draws_matrix <- function(...) {
  out <- validate_draws_per_variable(...)
  as_draws_matrix(do.call(cbind, out))
}

class_draws_matrix <- function() {
  c("draws_matrix", "draws", "matrix")
}

#' @rdname draws_matrix
#' @export
is_draws_matrix <- function(x) {
  inherits(x, "draws_matrix")
}

#' @export
`[.draws_matrix` <- function(x, i, j, ..., drop = FALSE) {
  # TODO: add a warning that 'drop' can lead to also dropping the class?
  # TODO: allow for argument 'reserved' as in '[.draws_df'
  #   right now this fails because NextMethod() cannot ignore arguments
  out <- NextMethod("[", drop = drop)
  if (length(dim(out)) == length(dim(x))) {
    class(out) <- class(x)
  }
  out
}

# create an empty draws_matrix object
empty_draws_matrix <- function(variables = character(0), ndraws = 0) {
  assert_character(variables, null.ok = TRUE)
  assert_number(ndraws, lower = 0)
  out <- matrix(
    numeric(0),
    nrow = ndraws,
    ncol = length(variables),
    dimnames = list(
      draw = seq_len(ndraws),
      variable = variables
    )
  )
  class(out) <- class_draws_matrix()
  out
}
