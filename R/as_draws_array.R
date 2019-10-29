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
as_draws_array.draws_array <- function(x, ...) {
  x
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
  class(x) <- class_draws_array()
  x
}

#' @importFrom abind abind
#' @export
as_draws_array.draws_df <- function(x, ...) {
  iterations <- iterations(x)
  chains <- chains(x)
  variables <- setdiff(names(x), meta_columns())
  out <- vector("list", length(chains))
  for (i in seq_along(out)) {
    out[[i]] <- x[x$.chain == i, ]
    out[[i]] <- remove_meta_columns(out[[i]])
    out[[i]] <- as.matrix(out[[i]])
  }
  # TODO: make the two lines below more efficient?
  out <- abind(out, along = 3L)
  out <- aperm(out, c(1, 3, 2))
  dimnames(out) <- list(
    iteration = iterations,
    chain = chains,
    variable = variables
  )
  class(out) <- class_draws_array()
  out
}

#' @export
as_draws_array.draws_list <- function(x, ...) {
  x <- as_draws_df(x)
  as_draws_array(x, ...)
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
  check_reserved_variables(new_dimnames[[3]])
  # TODO: use existing row/col names in any way?
  new_dimnames[[1]] <- as.character(seq_rows(x))
  new_dimnames[[2]] <- as.character(seq_cols(x))
  dimnames(x) <- new_dimnames
  class(x) <- class_draws_array()
  x
}

class_draws_array <- function() {
  c("draws_array", "draws", "array")
}
