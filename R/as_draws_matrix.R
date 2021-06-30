#' The `draws_matrix` format
#'
#' @name draws_matrix
#' @family formats
#'
#' @templateVar draws_format draws_matrix
#' @templateVar base_class "matrix"
#' @template draws_format-skeleton
#' @template args-format-nchains
#'
#' @details Objects of class `"draws_matrix"` are matrices (2-D arrays) with
#'   dimensions `"draw"` and `"variable"`. See **Examples**.
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
  if (ndraws(x) == 0) {
    return(empty_draws_matrix(variables(x), niterations(x)))
  }
  nchains <- nchains(x)
  old_dim <- dim(x)
  old_dimnames <- dimnames(x)
  dim(x) <- c(old_dim[1] * old_dim[2], old_dim[3])
  dimnames(x) <- list(
    draw = as.character(seq_rows(x)),
    variable = old_dimnames[[3]]
  )
  class(x) <- class_draws_matrix()
  attr(x, "nchains") <- nchains
  x
}

#' @rdname draws_matrix
#' @export
as_draws_matrix.draws_df <- function(x, ...) {
  if (ndraws(x) == 0) {
    return(empty_draws_matrix(variables(x)))
  }
  nchains <- nchains(x)
  draws <- x$.draw
  x <- remove_reserved_df_variables(x)
  class(x) <- class(x)[-1L]
  x <- .as_draws_matrix(x)
  rownames(x) <- draws
  attr(x, "nchains") <- nchains
  x
}

#' @rdname draws_matrix
#' @export
as_draws_matrix.draws_list <- function(x, ...) {
  x <- as_draws_df(x)
  as_draws_matrix(x, ...)
}

#' @rdname draws_matrix
#' @export
as_draws_matrix.draws_rvars <- function(x, ...) {
  if (ndraws(x) == 0) {
    return(empty_draws_matrix(variables(x)))
  }
  out <- do.call(cbind, lapply(seq_along(x), function(i) {
    # flatten each rvar so it only has two dimensions: draws and variables
    # this also collapses indices into variable names in the format "var[i,j,k,...]"
    x_i <- flatten_array(x[[i]], names(x)[[i]])
    draws_of(x_i)
  }))
  out <- as_draws_matrix(out, ...)
  attr(out, "nchains") <- nchains(x)
  out
}

#' @rdname draws_matrix
#' @export
as_draws_matrix.mcmc <- function(x, ...) {
  class(x) <- "matrix"
  attributes(x)[c("title", "mcpar")] <- NULL
  .as_draws_matrix(x)
}

#' @rdname draws_matrix
#' @export
as_draws_matrix.mcmc.list <- function(x, ...) {
  as_draws_matrix(as_draws_array(x), ...)
}

# try to convert any R object into a 'draws_matrix' object
.as_draws_matrix <- function(x) {
  .nchains <- attr(x, "nchains") %||% 1L
  x <- as.matrix(x)
  new_dimnames <- list(draw = NULL, variable = NULL)
  if (!is.null(dimnames(x)[[2]])) {
    new_dimnames[[2]] <- dimnames(x)[[2]]
  } else {
    new_dimnames[[2]] <- default_variables(NCOL(x))
  }
  check_new_variables(new_dimnames[[2]])
  new_dimnames[[1]] <- as.character(seq_rows(x))
  dimnames(x) <- new_dimnames
  class(x) <- class_draws_matrix()
  attr(x, "nchains") <- .nchains
  x
}

#' @rdname draws_matrix
#' @export
draws_matrix <- function(..., .nchains = 1) {
  out <- validate_draws_per_variable(...)
  .nchains <- as_one_integer(.nchains)
  if (.nchains < 1) {
    stop_no_call("Number of chains must be positive.")
  }
  ndraws <- length(out[[1]])
  if (ndraws %% .nchains != 0) {
    stop_no_call("Number of chains does not divide the number of draws.")
  }
  out <- do.call(cbind, out)
  attr(out, "nchains") <- .nchains
  as_draws_matrix(out)
}

class_draws_matrix <- function() {
  c("draws_matrix", "draws", "matrix")
}

#' @rdname draws_matrix
#' @export
is_draws_matrix <- function(x) {
  inherits(x, "draws_matrix")
}

# is an object looking like a 'draws_matrix' object?
is_draws_matrix_like <- function(x) {
  is.matrix(x) || is.array(x) && length(dim(x)) == 2L
}

#' Extract parts of a `draws_matrix` object
#'
#' Extract parts of a `draws_matrix` object. They are strictly defined as
#' matrices (draws x variable) so dropping any of the
#' dimensions breaks the expected structure of the object. Accordingly, no
#' dropping of dimensions is done by default even if the extracted slices are of
#' length 1. If `drop` is manually set to `TRUE` and any of the dimensions is
#' actually dropped, this will lead to dropping the `"draws_matrix"` class as
#' well.
#'
#' @param x,i,j,...,drop Same as in the default extraction method but with
#'   `drop` being set to `FALSE` by default.
#'
#' @return An object of class `"draws_matrix"` unless any of the dimensions
#' was dropped during the extraction.
#'
#' @export
`[.draws_matrix` <- function(x, i, j, ..., drop = FALSE) {
  # TODO: allow for argument 'reserved' as in '[.draws_df'
  #   right now this fails because NextMethod() cannot ignore arguments
  out <- NextMethod("[", drop = drop)
  if (length(dim(out)) == length(dim(x))) {
    class(out) <- class(x)
    .nchains <- nchains(x)
    if (missing(i)) {
      attr(out, "nchains") <- .nchains
    } else if (.nchains > 1L) {
      warn_merge_chains("index")
    }
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
