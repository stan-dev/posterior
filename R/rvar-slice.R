#' Random variable slicing
#'
#' Operations for slicing [`rvar`]s and replacing parts of [`rvar`]s.
#'
#' @param x an [`rvar`].
#' @param i,... indices; see *Details*.
#' @param drop (logical) Should singular dimensions be dropped when slicing
#' array [`rvar`]s? Unlike base array slicing operations, defaults to `FALSE`.
#' @param value (`rvar` or coercable to `rvar`) Value to insert into
#' `x` at the location determined by the indices.
#'
#' @details
#' The [`rvar`] slicing operators (`[` and `[[`) attempt to implement the same
#' semantics as the [base array slicing operators][base::Extract]. There are some
#' exceptions; most notably, [`rvar`] slicing defaults to `drop = FALSE` instead
#' of `drop = TRUE`.
#'
#' @section Extracting or replacing single elements with `[[`:
#'
#' The `[[` operator extracts (or replaces) single elements. It always
#' returns (or replaces) a scalar (length-1) [`rvar`].
#'
#' The `x[[i,...]]` operator can be used as follows:
#'
#' - `x[[<numeric>]]` for scalar numeric `i`: gives the `i`th element of `x`. If `x` is
#' multidimensional (i.e. `length(dim(x)) > 1`), extra dimensions are ignored
#' when indexing. For example, if `x` is a \eqn{6 \times 2} [`rvar`] array, the
#' 7th element, `x[[7]]`, will be the first element of the second column, `x[1,2]`.
#'
#' - `x[[<numeric rvar>]]` for scalar numeric [`rvar`] `i`: a generalization of indexing when
#' `i` is a scalar numeric. Within each draw of `x`, selects the element
#' corresponding to the value of `i` within that same draw.
#'
#' - `x[[<character>]]` for scalar character `i`: gives the element of `x` with name
#' equal to `i`. **Unlike with base arrays**, does not work with
#' multidimensional [`rvar`]s.
#'
#' - `x[[i_1,i_2,...,i_n]]` for scalar numeric or character `i_1`, `i_2`, etc.
#' Must provide exactly the same number of indices as dimensions in `x`. Selects
#' the element at the corresponding position in the [`rvar`] by number and/or
#' dimname (as a string).
#'
#' @section Extracting or replacing multiple elements with `[`:
#'
#' The `[` operator extracts (or replaces) multiple elements. It always returns
#' (or replaces) a possibly-multidimensional [`rvar`].
#'
#' The `x[i,...]` operator can be used as follows:
#'
#' - `x[<logical>]` for vector logical `i`: `i` is recycled to the same length as `x`,
#' ignoring multiple dimensions in `x`, then an [`rvar`] vector is returned
#' containing the elements in `x` where `i` is `TRUE`.
#'
#' - `x[<logical rvar>]` for scalar logical [`rvar`] `i`: returns an [`rvar`] the same shape
#' as `x` containing only those draws where `i` is `TRUE`.
#'
#' - `x[<numeric>]` for vector numeric `i`: an [`rvar`] vector is returned
#' containing the `i`th elements of `x`, ignoring dimensions.
#'
#' - `x[<matrix>]` for numeric matrix `i`, where `ncol(i) == length(dim(x))`: each row
#' of `i` should give the multidimensional index for a single element in `x`. The
#' result is an [`rvar`] vector of length `nrow(i)` containing elements of `x`
#' selected by each row of `i`.
#'
#' - `x[i_1,i_2,...,i_n]` for vector numeric, character, or logical `i_1`,
#' `i_2`, etc. Returns a slice of `x` containing all elements from the dimensions
#' specified in `i_1`, `i_2`, etc. If an argument is left empty, all elements
#' from that dimension are included. Unlike base arrays, trailing dimensions
#' can be omitted entirely and will still be selected; for example, if `x` has
#' three dimensions, both `x[1,,]` and `x[1,]` can be used to create a
#' slice that includes all elements from the last two dimensions. Unlike base
#' arrays, `[` defaults to `drop = FALSE`, so results retain the same number of
#' dimensions as `x`.
#'
#' @examples
#' x <- rvar(array(1:24, dim = c(4,2,3)))
#' dimnames(x) <- list(c("a","b"), c("d","e","f"))
#' x
#'
#' ## Slicing single elements
#' # x[[<numeric>]]
#' x[[2]]
#'
#' # x[[<numeric rvar>]]
#' # notice the draws of x[1:4]...
#' draws_of(x[1:4])
#' x[[rvar(c(1,3,4,4))]]
#' # ... x[[rvar(c(1,3,4,4))]] creates a mixures of those draws
#' draws_of(x[[rvar(c(1,3,4,4))]])
#'
#' # x[[i_1,i_2,...]]
#' x[[2,"e"]]
#'
#'
#' ## Slicing multiple elements
#' # x[<logical>]
#' x[c(TRUE,TRUE,FALSE)]
#'
#' # x[<logical rvar>]
#' # select every other draw
#' x[rvar(c(TRUE,FALSE,TRUE,FALSE))]
#'
#' # x[<numeric>]
#' x[1:3]
#'
#' # x[<matrix>]
#' x[rbind(
#'   c(1,2),
#'   c(1,3),
#'   c(2,2)
#' )]
#'
#' # x[i_1,i_2,...,i_n]
#' x[1,]
#' x[1,2:3]
#' x[,2:3]
#' @name rvar-slice
NULL

#' @rdname rvar-slice
#' @importFrom rlang inject is_missing missing_arg dots_list
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
    if (is_rvar(index[[1]])) {
      # x[[i]] where i is a scalar numeric rvar
      i_rvar <- index[[1]]
      c(i, x) %<-% scalar_numeric_rvar_to_index(i_rvar, x)
      .draws <- draws_of(x)[i]
    } else {
      # x[[i]] where i is a scalar numeric or character
      .draws <- draws_of(x)[, i, drop = FALSE]
    }
    dimnames(.draws) <- NULL
    out <- new_rvar(.draws, .nchains = nchains(x))
  } else if (length(index) == length(dim(x))) {
    # multiple element selection => must have exactly the right number of dims
    .draws <- inject(draws_of(x)[, !!!index, drop = FALSE])
    # must do drop manually in case the draws dimension has only 1 draw
    dim(.draws) <- c(ndraws(x), 1)
    out <- new_rvar(.draws, .nchains = nchains(x))
  } else {
    stop_no_call("subscript out of bounds")
  }
  out
}

#' @rdname rvar-slice
#' @export
`[[<-.rvar` <- function(x, i, ..., value) {
  value <- vec_cast(value, x)
  c(x, value) %<-% conform_rvar_ndraws_nchains(list(x, value))
  value <- check_rvar_dims_first(value, new_rvar(0))
  index <- check_rvar_yank_index(x, i, ...)

  if (length(index) == 1) {
    .dim = dim(x)

    if (length(.dim) == 1 && isTRUE(i > length(x))) {
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
      if (is_rvar(i)) {
        # x[[i]] <- value where i is a scalar numeric rvar
        c(i, x, value) %<-% scalar_numeric_rvar_to_index(i, x, value)
        draws_of(x)[i] <- draws_of(value)
      } else {
        # x[[i]] <- value where i is a scalar numeric or character
        draws_of(x)[, i] <- draws_of(value)
      }
      dim(x) <- .dim
      dimnames(draws_of(x)) <- .dimnames
    }
  } else if (length(index) == length(dim(x))) {
    # multiple element selection => must have exactly the right number of dims
    x <- inject({
      draws_of(x)[, !!!index] <- draws_of(value)
      x
    })
  } else {
    stop_no_call("subscript out of bounds")
  }
  x
}

#' @rdname rvar-slice
#' @importFrom rlang dots_list inject is_missing missing_arg expr
#' @export
`[.rvar` <- function(x, ..., drop = FALSE) {
  check_rvar_subset_indices(x, ...)
  index <- dots_list(..., .ignore_empty = "none", .preserve_empty = TRUE)

  # check for indexing of draws
  if (length(index) == 1 && is_rvar(index[[1]])) {
    i <- index[[1]]
    if (!is.logical(draws_of(i)) || length(i) != 1) {
      stop_no_call("`x[i]` for rvars `x` and `i` is only supported when `i` is a scalar logical rvar.")
    }

    # indexing by a scalar rvar, use it as a draws index
    # this kind of indexing must ignore chains
    nchains_rvar(x) <- 1L
    nchains_rvar(i) <- 1L
    c(x, i) %<-% conform_rvar_ndraws(list(x, i))
    index <- list()
    draws_index <- list(draws_of(i))
  } else {
    # not indexing draws, so draws_index is empty
    # must store draws_index as a list to safely pass around the missing argument
    draws_index <- list(missing_arg())
  }

  .draws <- draws_of(x)
  .dim <- dim(.draws)

  # clean up the indices: because we have to index using the multi-index
  # notation x[,...] (to account for draws) and this notation has slightly
  # different semantics for NAs from the way that x[i] works, have to do a bit
  # of translation here
  for (i in seq_along(index)) {
    if (is.numeric(index[[i]])) {
      # numeric indices outside the range of the corresponding dimension
      # should create NAs; but array indexing doesn't do this (it throws
      # an error), so we adjust the indices to do so.

      dim_i_length <- if (i == 1 && length(index) == 1) {
        # for x[i] style indexing of multidimensional arrays we will flatten
        # the array before indexing, so the max of the dim length will be
        # the length of x
        length(x)
      } else {
        .dim[[i + 1]]
      }

      index[[i]][index[[i]] > dim_i_length] <- NA_integer_
    }
  }

  if (length(index) == 1) {
    # indexing by a single dimension, (could be numerical indexing along one
    # dimension --- even for multidimensional arrays; logical indexing; or matrix indexing)

    if (is.matrix(index[[1]]) && ncol(index[[1]]) == length(.dim) - 1) {
      # matrix-based indexing, like x[cbind(2,1,2)]
      # => translate matrix-based indices into unidimensional indices
      index[[1]] <- matrix_to_index(index[[1]], .dim[-1])
    }

    if (is_missing(index[[1]])) {
      # if we only have one index and it is missing entirely, the call must
      # have been for x[], which actually means no index at all
      index <- list()
    } else if (length(dim(.draws)) > 2) {
      # draws have > 2 dims => array has > 1 dimension => must flatten array so
      # that the index can be applied along all the elements of the array
      dim(.draws) <- c(dim(.draws)[[1]], length(x))
    }
  } else if (length(index) < length(dim(.draws)) - 1) {
    # fill in final indices with missing arguments
    index[seq(length(index) + 1, length(dim(.draws)) - 1)] = list(missing_arg())
  }

  .draws <- inject(.draws[!!!draws_index, !!!index, drop = FALSE])

  if (!is_missing(draws_index[[1]])) {
    # if we subsetted draws, replace draw ids with sequential ids
    rownames(.draws) <- seq_len(NROW(.draws))
  }

  x <- new_rvar(.draws, .nchains = nchains(x))

  if (drop) {
    x <- drop(x)
  }

  x
}

#' @rdname rvar-slice
#' @export
`[<-.rvar` <- function(x, i, ..., value) {
  if (missing(i)) i = missing_arg()
  if (length(dim(x)) == 1 && !missing(i) && is.numeric(i) && any(i > length(x), na.rm = TRUE)) {
    # unidimensional indexing allows array extension; extend the array
    # before we do the assignment
    x <- x[seq_len(max(i, na.rm = TRUE))]
  }

  value <- vec_cast(value, x)

  # check to see if we're doing any draw indexing
  if (!missing(i) && is_rvar(i)) {
    if (!is.logical(draws_of(i)) || length(i) != 1L || !missing(...)) {
      stop_no_call("`x[i]` for rvars `x` and `i` is only supported when `i` is a single, scalar logical rvar.")
    }

    # for the purposes of this kind of assignment, we check draws only, not chains,
    # as chain information is irrelevant when subsetting by draw
    c(x, i) %<-% conform_rvar_ndraws(list(x, i))
    draws_index <- draws_of(i)

    # necessary number of draws in `value` is determined by whether or not
    # we're doing logical indexing
    value_ndraws <- sum(draws_index, na.rm = TRUE)
    draws_of(value) <- broadcast_array(draws_of(value), c(value_ndraws, dim(x)), broadcast_scalars = FALSE)
    i <- missing_arg()
  } else {
    c(x, value) %<-% conform_rvar_ndraws_nchains(list(x, value))
    draws_index <- missing_arg()
  }

  if (missing(...)) {
    # index over entire array: flatten array so that the  index
    # can be applied along all the elements of the array, then invert after assignment
    original_dim <- dim(draws_of(x))
    original_dimnames <- dimnames(draws_of(x))

    if (!missing(i) && is.matrix(i) && ncol(i) == length(original_dim) - 1) {
      # matrix-based indexing, like x[cbind(2,1,2)] <- y
      # => translate matrix-based indices into unidimensional indices
      i <- matrix_to_index(i, original_dim[-1])
    }

    #flatten and assign
    dim(x) <- length(x)
    draws_of(x)[draws_index,i] <- draws_of(value)

    # unflatten and restore dimnames
    dim(draws_of(x)) <- original_dim
    dimnames(draws_of(x)) <- original_dimnames
  } else {
    draws_of(x)[,i,...] <- draws_of(value)
  }

  x
}



# slicing helpers ---------------------------------------------------------

# Given m, a matrix with ncol(m) == length(dim), where each row specifies the
# index of a single cell from an array of shape == dim, return a vector of
# length nrow(m) giving the unidimensional indices of that array corresponding
# to the cells specified by each row in m
# e.g. if we have an array x with dim(x) = c(2,3,4), we might want to
# translate an index of style x[cbind(2,1,2)] into x[i] so that we can index
# index x using the single index i instead of cbind(2,1,2).
# matrix_to_index(cbind(2,1,2), dim(x)) does that translation.
matrix_to_index <- function(m, dim) {
  cumdim <- cumprod(c(1, dim[-length(dim)]))
  as.vector((m - 1) %*% cumdim + 1)
}

# Given i_rvar, a scalar numeric rvar acting as an index on x, return a list
# whose first element is i, an index on draws_of(x) that corresponds to i_rvar.
# remaining elements are x and any other rvars in `...`, all of which will
# have chains and draws conformed to each other.
scalar_numeric_rvar_to_index <- function(i_rvar, x, ...) {
  if (!is.numeric(draws_of(i_rvar)) || length(i_rvar) != 1) {
    stop_no_call("`x[[i]]` for rvars `x` and `i` is only supported when `i` is a scalar numeric rvar.")
  }
  out <- conform_rvar_ndraws_nchains(list(i_rvar, x, ...))
  c(i_rvar, x) %<-% out[1:2]
  out[[1]] <- matrix_to_index(cbind(seq_len(ndraws(x)), draws_of(i_rvar)), c(ndraws(x), length(x)))
  out
}

