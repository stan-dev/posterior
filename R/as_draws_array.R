#' The `draws_array` format
#'
#' @name draws_array
#' @family formats
#'
#' @templateVar draws_format draws_array
#' @templateVar base_class "array"
#' @template draws_format-skeleton
#' @template args-format-nchains
#'
#' @details Objects of class `"draws_array"` are 3-D arrays with dimensions
#'   `"iteration"`, `"chain"`, and `"variable"`. See **Examples**.
#'
NULL

#' @rdname draws_array
#' @export
as_draws_array <- function(x, ...) {
  UseMethod("as_draws_array")
}

#' @rdname draws_array
#' @export
as_draws_array.array <- function(x, ...) {
  if (length(dim(x)) != 3) {
    stop_no_call("Don't know how to transform an array with other than 3 ",
                 "dimensions to any supported draws format.")
  }
  dimnames(x) <- list(
    "iteration" = seq_len(dim(x)[1]),
    "chain" = seq_len(dim(x)[2]),
    "variable" = dimnames(x)[[3]] %||% default_variables(dim(x)[3])
  )
  class(x) <- class_draws_array()
  if (ndraws(x) == 0) {
    return(empty_draws_array(variables(x)))
  }
  x
}

#' @rdname draws_array
#' @export
as_draws_array.data.frame <- function(x, ...) {
  x <- as_draws_df(x)
  if (ndraws(x) == 0) {
    return(empty_draws_array(variables(x)))
  }
  class(x) <- "data.frame"
  .chain <- x[[".chain"]]
  x[c(".chain", ".iteration", ".draw")] <- NULL
  x <- split(x, .chain)
  x <- abind::abind(x, along = 3L)
  x <- aperm(x, c(1, 3, 2))
  names(dimnames(x)) <- c("iteration", "chain", "variable")
  dimnames(x)[["iteration"]] <- seq_len(dim(x)[1])
  class(x) <- class_draws_array()
  x
}

#' @rdname draws_array
#' @export
as_draws_array.default <- function(x, ...) {
  x <- as_draws_df(x)
  as_draws_array.data.frame(x, ...)
}

#' @rdname draws_array
#' @export
as_draws_array.draws_array <- function(x, ...) {
  x
}

#' @rdname draws_array
#' @export
as_draws_array.matrix <- function(x, ...) {
  x <- as_draws_df(x)
  as_draws_array.data.frame(x, ...)
}

#' @rdname draws_array
#' @export
draws_array <- function(..., .nchains = 1) {
  out <- validate_draws_per_variable(...)
  .nchains <- as_one_integer(.nchains)
  if (.nchains < 1) {
    stop_no_call("Number of chains must be positive.")
  }
  ndraws <- length(out[[1]])
  if (ndraws %% .nchains != 0) {
    stop_no_call("Number of chains does not divide the number of draws.")
  }
  niterations <- ndraws %/% .nchains
  variables <- names(out)
  out <- unlist(out)
  out <- array(out, dim = c(niterations, .nchains, length(variables)))
  dimnames(out) <- list(NULL, NULL, variables)
  as_draws_array(out)
}

class_draws_array <- function() {
  c("draws_array", "draws", "array")
}

#' @rdname draws_array
#' @export
is_draws_array <- function(x) {
  inherits(x, "draws_array")
}

#' @export
`[.draws_array` <- function(x, i, j, ..., drop = FALSE) {
  # TODO: add a warning that 'drop' can lead to also dropping the class?
  # TODO: allow for argument 'reserved' as in '[.draws_df'
  #   right now this fails because NextMethod() cannot ignore arguments
  out <- NextMethod("[", drop = drop)
  if (length(dim(out)) == length(dim(x))) {
    class(out) <- class(x)
  }
  out
}

# create an empty draws_array object
empty_draws_array <- function(variables = NULL, nchains = 0,
                              niterations = 0) {
  assert_character(variables, null.ok = TRUE)
  assert_number(nchains, lower = 0)
  assert_number(niterations, lower = 0)
  out <- array(
    numeric(0),
    dim = c(niterations, nchains, length(variables)),
    dimnames = list(
      iteration = seq_len(niterations),
      chain = seq_len(nchains),
      variable = variables %||% character(0)
    )
  )
  class(out) <- class_draws_array()
  out
}
