#' @importFrom rlang eval_tidy is_missing missing_arg dots_list
#' @export
`[[.rvar` <- function(x, i, ...) {
  index <- check_rvar_yank_index(x, i, ...)

  if (length(index) == 1) {
    # single element selection => collapse the dims so we can select directly using i
    .dim = dim(x)
    if (length(.dim) != 1) {
      # we only collapse dims if necessary since this will drop dimnames (which
      # would prevent single-element by-name selection for 1d rvars)
      dim(x) <- prod(.dim)
    }
    .draws <- draws_of(x)[, i, drop = FALSE]
    dimnames(.draws) <- NULL
    out <- new_rvar(.draws, .nchains = nchains(x))
  } else if (length(index) == length(dim(x))) {
    # multiple element selection => must have exactly the right number of dims
    .draws <- eval_tidy(expr(draws_of(x)[, !!!index, drop = FALSE]))
    # must do drop manually in case the draws dimension has only 1 draw
    dim(.draws) <- c(ndraws(x), 1)
    out <- new_rvar(.draws, .nchains = nchains(x))
  } else {
    stop2("subscript out of bounds")
  }
  out
}

#' @export
`[[<-.rvar` <- function(x, i, ..., value) {
  value <- vec_cast(value, x)
  c(x, value) %<-% conform_rvar_ndraws_nchains(list(x, value))
  value <- check_rvar_dims_first(value, new_rvar(0))
  index <- check_rvar_yank_index(x, i, ...)

  if (length(index) == 1) {
    .dim = dim(x)

    if (length(.dim) == 1 && i > length(x)) {
      # unidimensional indexing allows array extension; extend the array
      # then do the assignment
      x <- x[seq_len(max(i, na.rm = TRUE))]
      draws_of(x)[, i] <- draws_of(value)
    } else {
      # single element selection => collapse the dims so we can select directly using i
      .dimnames = dimnames(draws_of(x)) # to restore later
      if (length(.dim) != 1) {
        # we only collapse dims if necessary since this will drop dimnames (which
        # would prevent single-element by-name selection for 1d rvars)
        dim(x) <- prod(.dim)
      }
      draws_of(x)[, i] <- draws_of(value)
      dim(x) <- .dim
      dimnames(draws_of(x)) <- .dimnames
    }
  } else if (length(index) == length(dim(x))) {
    # multiple element selection => must have exactly the right number of dims
    x <- eval_tidy(expr({
      draws_of(x)[, !!!index] <- draws_of(value)
      x
    }))
  } else {
    stop2("subscript out of bounds")
  }
  x
}

#' @importFrom rlang enquos eval_tidy quo is_missing missing_arg expr
#' @export
`[.rvar` <- function(x, ..., drop = FALSE) {
  check_rvar_subset_indices(x, ...)
  .draws = draws_of(x)
  .dim = dim(.draws)

  index = as.list(enquos(...))
  for (i in seq_along(index)) {
    if (is_missing(quo_get_expr(index[[i]]))) {
      index[[i]] <- missing_arg()
    } else {
      index_i <- eval_tidy(index[[i]])

      if (length(index_i) == 0) {
        # need to do this in a second step just in case index_i evaluate to
        # NULL (which would cause assignment to this list to remove the
        # index at i rather than setting it to NULL)
        index[[i]] <- integer(0)
      } else {
        index[[i]] <- index_i

        if (is.numeric(index[[i]])) {
          # numeric indices outside the range of the corresponding dimension
          # should create NAs; but array indexing doesn't do this (it throws
          # an error), so we adjust the indices to do so.
          index[[i]][index[[i]] > .dim[[i + 1]]] <- NA_integer_
        }
      }
    }
  }

  if (length(index) < length(dim(.draws)) - 1) {
    if (length(index) == 1 && is.logical(index[[1]]) && length(index[[1]]) == length(x)) {
      # logical index over entire array: flatten array so that the logical index
      # can be applied along all the elements of the array
      dim(.draws) <- c(dim(.draws)[[1]], length(x))
    } else {
      # fill in final indices with missing arguments
      index[seq(length(index) + 1, length(dim(.draws)) - 1)] = list(missing_arg())
    }
  }

  x <- eval_tidy(expr(
    new_rvar(.draws[, !!!index, drop = FALSE], .nchains = nchains(x))
  ))

  if (drop) {
    drop_(x)
  } else {
    x
  }
}

#' @export
`[<-.rvar` <- function(x, i, ..., value) {
  if (missing(i)) i = missing_arg()
  if (length(dim(x)) == 1 && !missing(i) && any(i > length(x), na.rm = TRUE)) {
    # unidimensional indexing allows array extension; extend the array
    # before we do the assignment
    x <- x[seq_len(max(i, na.rm = TRUE))]
  }

  value <- vec_cast(value, x)
  c(x, value) %<-% conform_rvar_ndraws_nchains(list(x, value))

  if (missing(...) && is.logical(i) && length(i) == length(x)) {
    # logical index over entire array: flatten array so that the logical index
    # can be applied along all the elements of the array, then invert after assignment
    original_dim <- dim(draws_of(x))
    original_dimnames <- dimnames(draws_of(x))
    dim(x) <- length(x)
    draws_of(x)[,i] <- draws_of(value)
    dim(draws_of(x)) <- original_dim
    dimnames(draws_of(x)) <- original_dimnames
  } else {
    draws_of(x)[,i,...] <- draws_of(value)
  }

  x
}
