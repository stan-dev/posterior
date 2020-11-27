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
  dim(x) <- c(old_dim[1], 1, old_dim[2])
  dimnames(x) <- list(
    iteration = as.character(seq_rows(x)),
    chain = "1",
    variable = old_dimnames[[2]]
  )
  class(x) <- class_draws_array()
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
  # TODO: use existing row/col names in any way?
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
    stop2("Number of chains must be positive.")
  }
  ndraws <- length(out[[1]])
  if (ndraws %% .nchains != 0) {
    stop2("Number of chains does not divide the number of draws.")
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

# is an object looking like a 'draws_array' object?
is_draws_array_like <- function(x) {
  is.array(x) && length(dim(x)) == 3L
}

#' @export
`[.draws_array` <- function(x, i, j, ..., drop = FALSE) {
  # TODO: add a warning that 'drop' can lead to also dropping the class?
  # TODO: allow for argument 'reserved' as in '[.draws_df'
  #   right now this fails because NextMethod() cannot ignore arguments
  out <- NextMethod("[", drop = drop)
  if (length(dim(out)) == 3) {
    class(out) <- class(x)
  }
  out
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

