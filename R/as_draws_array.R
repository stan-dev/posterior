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
as_draws_array.default <- function(x, ...) {
  x <- as_draws(x)
  as_draws_array(x, ...)
}

#' @rdname draws_array
#' @export
as_draws_array.draws_array <- function(x, ...) {
  x
}

#' @rdname draws_array
#' @export
as_draws_array.draws_matrix <- function(x, ...) {
  old_dim <- dim(x)
  old_dimnames <- dimnames(x)
  iteration_ids <- iteration_ids(x)
  chain_ids <- chain_ids(x)
  dim(x) <- c(niterations(x), nchains(x), old_dim[2])
  dimnames(x) <- list(
    iteration = iteration_ids,
    chain = chain_ids,
    variable = old_dimnames[[2]]
  )
  class(x) <- class_draws_array()
  attr(x, "nchains") <- NULL
  x
}

#' @rdname draws_array
#' @export
as_draws_array.draws_df <- function(x, ...) {
  if (ndraws(x) == 0) {
    return(empty_draws_array(variables(x)))
  }
  iterations <- iteration_ids(x)
  chains <- chain_ids(x)
  x <- check_variables_are_numeric(x, to = "draws_array")
  out <- vector("list", length(chains))
  for (i in seq_along(out)) {
    if (length(chains) == 1) {
      out[[i]] <- x
    } else {
      out[[i]] <- x[x$.chain == i, ]
    }
    out[[i]] <- remove_reserved_df_variables(out[[i]])
    out[[i]] <- as.matrix(out[[i]])
  }
  out <- as_array_matrix_list(out)
  dimnames(out) <- list(
    iteration = iterations,
    chain = chains,
    variable = dimnames(out)[[3]]
  )
  class(out) <- class_draws_array()
  out
}

#' @rdname draws_array
#' @export
as_draws_array.draws_list <- function(x, ...) {
  x <- as_draws_df(x)
  as_draws_array(x, ...)
}

#' @rdname draws_array
#' @export
as_draws_array.draws_rvars <- function(x, ...) {
  if (ndraws(x) == 0) {
    return(empty_draws_array(variables(x)))
  }

  x <- check_variables_are_numeric(
    x, to = "draws_array", is_non_numeric = is_rvar_factor, convert = FALSE
  )

  # cbind discards class information when applied to vectors, which converts
  # the underlying factors to numeric
  draws <- do.call(cbind, lapply(seq_along(x), function(i) {
    # flatten each rvar so it only has two dimensions: draws and variables
    # this also collapses indices into variable names in the format "var[i,j,k,...]"
    x_i <- flatten_array(x[[i]], names(x)[[i]])
    draws_of(x_i)
  }))

  # add chain info back into the draws array
  # ([draws, variables] -> [iterations, chains, variables])
  .dimnames <- dimnames(draws)
  dim(draws) <- c(niterations(x), nchains(x), dim(draws)[-1])
  dimnames(draws) <- c(list(NULL, NULL), .dimnames[-1])

  as_draws_array(draws, ...)
}

#' @rdname draws_array
#' @export
as_draws_array.mcmc <- function(x, ...) {
  as_draws_array(as_draws_matrix(x), ...)
}

#' @rdname draws_array
#' @export
as_draws_array.mcmc.list <- function(x, ...) {
  class(x) <- "list"
  .as_draws_array(as_array_matrix_list(x))
}

# try to convert any R object into a 'draws_array' object
.as_draws_array <- function(x) {
  x <- as.array(x)
  new_dimnames <- list(iteration = NULL, chain = NULL, variable = NULL)
  if (!is.null(dimnames(x)[[3]])) {
    new_dimnames[[3]] <- dimnames(x)[[3]]
  } else {
    new_dimnames[[3]] <- default_variables(dim(x)[3])
  }
  check_new_variables(new_dimnames[[3]])
  new_dimnames[[1]] <- as.character(seq_rows(x))
  new_dimnames[[2]] <- as.character(seq_cols(x))
  dimnames(x) <- new_dimnames
  class(x) <- class_draws_array()
  x
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
  niterations <- ndraws / .nchains
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

# is an object looking like a 'draws_array' object?
is_draws_array_like <- function(x) {
  is.array(x) && length(dim(x)) == 3L
}

#' Extract parts of a `draws_array` object
#'
#' Extract parts of a `draws_array` object. They are strictly defined as arrays
#' of 3 dimensions (iteration x chain x variable) so dropping any of the
#' dimensions breaks the expected structure of the object. Accordingly, no
#' dropping of dimensions is done by default even if the extracted slices are of
#' length 1. If `drop` is manually set to `TRUE` and any of the dimensions is
#' actually dropped, this will lead to dropping the `"draws_array"` class as
#' well.
#'
#' @param x,i,j,...,drop Same as in the default extraction method but with
#'   `drop` being set to `FALSE` by default.
#'
#' @return An object of class `"draws_array"` unless any of the dimensions
#' was dropped during the extraction.
#'
#' @keywords internal
#' @export
`[.draws_array` <- function(x, i, j, ..., drop = FALSE) {
  # TODO: allow for argument 'reserved' as in '[.draws_df'
  #   right now this fails because NextMethod() cannot ignore arguments
  out <- NextMethod("[", drop = drop)
  if (length(dim(out)) == length(dim(x))) {
    class(out) <- class(x)
  }
  out
}

#' @export
variance.draws_array <- function(x, ...) {
  var(as.vector(x))
}

# convert a list of matrices to an array
as_array_matrix_list <- function(x) {
  stopifnot(is.list(x))
  if (length(x) == 1) {
    tmp <- dimnames(x[[1]])
    x <- x[[1]]
    dim(x) <- c(dim(x), 1)
    dimnames(x) <- tmp
  } else {
    x <- abind::abind(x, along = 3L)
  }
  x <- aperm(x, c(1, 3, 2))
}

# create an empty draws_array object
empty_draws_array <- function(variables = character(0), nchains = 0,
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
      variable = variables
    )
  )
  class(out) <- class_draws_array()
  out
}

