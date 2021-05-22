#' Random variables of arbitrary dimension
#'
#' Random variables backed by arrays of arbitrary dimension
#'
#' @name rvar
#'
#' @param x A vector or array where the first dimension represents draws from
#' a distribution. The resulting [`rvar`] will have dimension `dim(x)[-1]`; that is,
#' everything except the first dimension is used for the shape of the variable, and the
#' first dimension is used to index draws from the distribution. Optionally,
#' if `with_chains == TRUE`, the first dimension indexes the iteration and the
#' second dimension indexes the chain (see `with_chains`).
#' @template args-rvar-dim
#' @template args-rvar-dimnames
#' @param nchains Number of chains (default is `1`).
#' @param with_chains Does `x` include a dimension for chains?
#' If `FALSE` (the default), chains are not included, the first dimension of
#' the input array should index draws, and the `nchains` argument can be
#' used to determine the number of chains. If `TRUE`, the `nchains` argument
#' is ignored and the second dimension of `x` is used to index chains.
#' Internally, the array will be converted to a format without the chain index.
#'
#' @details
#'
#' The `"rvar"` class internally represents random variables as arrays of arbitrary
#' dimension, where the first dimension is used to index draws from the distribution.
#
#' Most mathematical operators and functions are supported, including efficient matrix
#' multiplication and vector and array-style indexing. The intent is that an `rvar`
#' works as closely as possible to how a base vector/matrix/array does, with a few
#' differences:
#'
#' - The default behavior when subsetting is not to drop extra dimensions (i.e.
#'   the default `drop` argument for `[` is `FALSE`, not `TRUE`).
#' - Rather than base R-style recycling, `rvar`s use a limited form of broadcasting:
#'   if an operation is being performed on two vectors with different size of the same
#'   dimension, the smaller vector will be recycled up to the size of the larger one
#'   along that dimension so long as it has size 1.
#'
#' For functions that expect base numeric arrays and for which `rvar`s cannot be
#' used directly as arguments, you can use [rfun()] or [rdo()] to translate your
#' code into code that executes across draws from one or more random variables
#' and returns a random variable as output. Typically [rdo()] offers the most
#' straightforward translation.
#'
#' As [rfun()] and [rdo()] incur some performance cost, you can also operate directly
#' on the underlying array using the [draws_of()] function. To re-use existing
#' random number generator functions to efficiently create `rvar`s, use [rvar_rng()].
#'
#' @seealso [as_rvar()] to convert objects to `rvar`s. See [rdo()], [rfun()], and
#' [rvar_rng()] for higher-level interfaces for creating `rvar`s.
#'
#' @return An object of class `"rvar"` representing a random variable.
#'
#' @examples
#'
#' # To create a "scalar" `rvar`, pass a one-dimensional array or a vector
#' # whose length (here `4000`) is the desired number of draws:
#' x <- rvar(rnorm(4000, mean = 1, sd = 1))
#' x
#'
#' # Create random vectors by adding an additional dimension:
#' n <- 4   # length of output vector
#' x <- rvar(array(rnorm(4000 * n, mean = rep(1:n, each = 4000), sd = 1), dim = c(4000, n)))
#' x
#'
#' # Create a random matrix:
#' rows <- 4
#' cols <- 3
#' x <- rvar(array(rnorm(4000 * rows * cols, mean = 1, sd = 1), dim = c(4000, rows, cols)))
#' x
#'
#' @export
rvar <- function(x = double(), dim = NULL, dimnames = NULL, nchains = 1L, with_chains = FALSE) {
  with_chains <- as_one_logical(with_chains)
  if (with_chains) {
    nchains <- dim(x)[[2]] %||% 1L
    x <- drop_chain_dim(x)
  }

  out <- new_rvar(x, .nchains = nchains)

  if (!is.null(dim)) {
    dim(out) <- dim
  }
  if (!is.null(dimnames)) {
    dimnames(out) <- dimnames
  }

  out
}

#' @importFrom vctrs new_vctr
new_rvar <- function(x = double(), .nchains = 1L) {
  .dim <- dim(x)
  if (length(x) == 0) {
    x <- double()
  }
  x <- as.array(x)

  if (length(x) == 0) {
    # canonical NULL rvar is 1 draw of nothing
    # this ensures that (e.g.) extending a null rvar
    # with x[1] = something works.
    dim(x) <- c(1, 0)
  }
  else if (is.null(.dim) || length(.dim) == 1) {
    # 1d vectors get treated as a single variable
    dim(x) <- c(length(x), 1)
  }

  # ensure dimnames is set (makes comparison easier for tests)
  if (length(dimnames(x)) == 0) {
    dimnames(x) <- list(NULL)
  }

  .ndraws <- dim(x)[[1]]
  .nchains <- as_one_integer(.nchains)
  check_nchains_compat_with_ndraws(.nchains, .ndraws)

  # ensure we have an index for draws
  if (length(rownames(x)) == 0) {
    rownames(x) <- as.character(seq_rows(x))
  }

  structure(
    list(),
    draws = x,
    nchains = .nchains,
    class = c("rvar", "vctrs_vctr", "list")
  )
}


# manipulating raw draws array --------------------------------------------

#' Get/set array of draws underlying a random variable
#'
#' Gets/sets the array-representation that backs an [`rvar`]. Should be used rarely.
#'
#' @param x An [`rvar`]
#' @param value An array
#' @param with_chains Should the array of draws include a dimension for chains?
#' If `FALSE` (the default), chains are not included and the array has dimension
#' `c(ndraws(x), dim(x))`. If `TRUE`, chains are included and the array has
#' dimension `c(niterations(x), nchains(x), dim(x))`.
#'
#' @details
#'
#' While [`rvar`]s implement fast versions of basic math operations (including
#' [matrix multiplication][rvar-matmult]), sometimes you may need to bypass
#' the [`rvar`] abstraction to do what you need to do more efficiently.
#' `draws_of()` allows you to get / set the underlying array of draws in
#' order to do that.
#'
#' [`rvar`]s represent draws internally using arrays of arbitrary dimension, which
#' is returned by `draws_of(x)` and can be set using `draws_of(x) <- value`.
#' The **first** dimension of these arrays is the index of the draws. If
#' `with_chains = TRUE`, then the dimensions of the returned array are modified
#' so that the first dimension is the index of the iterations and the second
#' dimension is the index of the chains.
#'
#' @export
draws_of <- function(x, with_chains = FALSE) {
  with_chains <- as_one_logical(with_chains)
  draws <- attr(x, "draws")

  if (with_chains) {
    x_dim <- dim(x)
    dim(draws) <- c(niterations(x), nchains(x), x_dim)
    x_dim_i <- seq_along(x_dim)
    draws <- copy_dimnames(x, x_dim_i, draws, x_dim_i + 2)
  }

  draws
}

#' @rdname draws_of
#' @export
`draws_of<-` <- function(x, value, with_chains = FALSE) {
  with_chains <- as_one_logical(with_chains)

  if (with_chains) {
    draws <- drop_chain_dim(value)
    rownames(draws) <- as.character(seq_rows(draws))
    attr(x, "draws") <- draws
    attr(x, "nchains") <- dim(value)[[2]] %||% 1L
  } else {
    attr(x, "draws") <- value
  }

  x
}


# misc standard methods --------------------------------------------------

#' @export
levels.rvar <- function(x) {
  # TODO: implement for factor-like rvars
  NULL
}

#' @export
rep.rvar <- function(x, times = 1, length.out = NA, each = 1, ...) {
  # flatten before rep()ing
  dim(x) <- length(x)

  if (each != 1) {
    x = vec_restore(rep(vec_proxy(x), each = each), x)
  }

  draws = draws_of(x)
  if (is.na(length.out)) {
    # use `times`
    rep_draws = rep(draws, times)
    dim = dim(draws)
    dim[[2]] = dim[[2]] * times
    dim(rep_draws) = dim
    out <- new_rvar(rep_draws, .nchains = nchains(x))
  } else {
    # use `length.out`
    rep_draws = rep_len(draws, length.out * ndraws(x))
    dim(rep_draws) = c(ndraws(x), length(rep_draws) / ndraws(x))
    out <- new_rvar(rep_draws, .nchains = nchains(x))
  }
  out
}

#' @method rep.int rvar
#' @export
rep.int.rvar <- function(x, times) {
  rep(x, times = times)
}

#' @method rep_len rvar
#' @export
rep_len.rvar <- function(x, length.out) {
  rep(x, length.out = length.out)
}

#' @export
unique.rvar <- function(x, incomparables = FALSE, MARGIN = 1, ...) {
  draws_margin <- check_rvar_margin(x, MARGIN)
  draws_of(x) <- unique(draws_of(x), incomparables = incomparables, MARGIN = draws_margin, ...)
  x
}

#' @export
duplicated.rvar <- function(x, incomparables = FALSE, MARGIN = 1, ...) {
  draws_margin <- check_rvar_margin(x, MARGIN)
  duplicated(draws_of(x), incomparables = incomparables, MARGIN = draws_margin, ...)
}

#' @export
anyDuplicated.rvar <- function(x, incomparables = FALSE, MARGIN = 1, ...) {
  draws_margin <- check_rvar_margin(x, MARGIN)
  anyDuplicated(draws_of(x), incomparables = incomparables, MARGIN = draws_margin, ...)
}

# check that MARGIN is a valid margin for the dimensions of rvar x
# then return the corresponding margin for draws_of(x)
check_rvar_margin <- function(x, MARGIN) {
  if (!(1 <= MARGIN && MARGIN <= length(dim(x)))) {
    stop_no_call("MARGIN = ", MARGIN, " is invalid for dim = ", paste0(dim(x), collapse = ","))
  }
  MARGIN + 1
}

#' @export
all.equal.rvar <- function(target, current, ...) {
  result <- NULL

  if (data.class(target) != data.class(current)) {
    result <- c(result, paste0(
      "target is ", data.class(target),
      ", current is ", data.class(current)
    ))
  }

  object_result <- all.equal(unclass(target), unclass(current), ...)
  if (!isTRUE(object_result)) {
    result = c(result, object_result)
  }

  if (is.null(result)) TRUE else result
}


# helpers: validation -----------------------------------------------------------------

# Check the passed yank index (for x[[...]]) is valid
check_rvar_yank_index = function(x, i, ...) {
  index <- dots_list(i, ..., .preserve_empty = TRUE, .ignore_empty = "none")

  if (any(lengths(index)) > 1) {
    stop_no_call("Cannot select more than one element per index with `[[` in an rvar.")
  } else if (any(sapply(index, function(x) is_missing(x) || is.na(x)))) {
    stop_no_call("Missing indices not allowed with `[[` in an rvar.")
  } else if (any(sapply(index, is.logical))) {
    stop_no_call("Logical indices not allowed with `[[` in an rvar.")
  } else if (any(sapply(index, function(x) x < 0))) {
    stop_no_call("subscript out of bounds")
  }

  index
}

# Check the passed subset indices (for x[...]) do not go beyond the end
# of the valid dimensions
check_rvar_subset_indices = function(x, ...) {
  ndim = max(length(dim(x)), 1)
  if (length(substitute(list(...))) - 1 > ndim) {
    stop_no_call("Cannot index past dimension ", ndim, ".")
  }
}

# find common ndraws for the ndraws of two rvars to be broadcast to
ndraws2_common <- function(ndraws_x, ndraws_y) {
  if (ndraws_x == 1) {
    ndraws_y
  } else if (ndraws_y == 1) {
    ndraws_x
  } else if (ndraws_x == ndraws_y) {
    ndraws_x
  } else {
    stop_no_call(
      "Random variables have different number of draws (", ndraws_x,
      " and ", ndraws_y, ") and cannot be used together."
    )
  }
}

# find common nchains for the nchains of two rvars to be set to
# nchains_x or nchains_y may be NULL to indicate they are constants
# and take the nchains of the other
nchains2_common <- function(nchains_x, nchains_y) {
  # constants should give nchains of NULL for input to this function
  # so they are treated as having any number of chains
  if (is.null(nchains_x)) {
    nchains_y
  } else if (is.null(nchains_y)) {
    nchains_x
  } else if (nchains_x == nchains_y) {
    nchains_x
  } else {
    warning_no_call(
      "Random variables do not have the same number of chains (", nchains_x, " and ", nchains_y, "),\n",
      "so chains were dropped.\n",
      "Use merge_chains() to collapse chains before combining rvars with\n",
      "a different number of chains to avoid this warning."
    )
    1L
  }
}

# check that the given number of chains is compatible with the given number of draws
check_nchains_compat_with_ndraws <- function(nchains, ndraws) {
  # except with constants, nchains must divide the number of draws
  if (ndraws != 1 && isTRUE(ndraws %% nchains != 0)) {
    stop_no_call("Number of chains does not divide the number of draws.")
  }
  if (nchains < 1) {
    stop_no_call("Number of chains must be >= 1")
  }
}

# given two rvars, conform their number of chains
# so they can be used together (or throw an error if they can't be)
conform_rvar_nchains <- function(rvars) {
  # find the number of chains to use, treating constants as having any number of chains
  nchains_or_null <- lapply(rvars, function(x) if (ndraws(x) == 1) NULL else nchains(x))
  .nchains <- Reduce(nchains2_common, nchains_or_null) %||% 1L

  for (i in seq_along(rvars)) {
    attr(rvars[[i]], "nchains") <- .nchains
  }

  rvars
}

# given two rvars, conform their number of draws and chains
# so they can be used together (or throw an error if they can't be)
# @param keep_constants keep constants as 1-draw rvars
conform_rvar_ndraws_nchains <- function(rvars, keep_constants = FALSE) {
  rvars <- conform_rvar_nchains(rvars)

  # broadcast to a common number of chains. If keep_constants = TRUE,
  # constants will not be broadcast.
  .ndraws = Reduce(ndraws2_common, lapply(rvars, ndraws))
  for (i in seq_along(rvars)) {
    rvars[[i]] <- broadcast_draws(rvars[[i]], .ndraws, keep_constants)
  }

  rvars
}

# Check that the first rvar can be conformed to the dimensions of the second,
# ignoring 1s
check_rvar_dims_first <- function(x, y) {
  x_dim <- dim(x)
  x_dim_dropped <- as.integer(x_dim[x_dim != 1])
  y_dim <- dim(y)
  y_dim_dropped <- as.integer(y_dim[y_dim != 1])

  if (length(x_dim_dropped) == 0) {
    # x can be treated as scalar, do so
    dim(x) <- rep(1, length(dim(y)))
  } else if (identical(x_dim_dropped, y_dim_dropped)) {
    dim(x) <- dim(y)
  } else {
    stop_no_call("Cannot assign an rvar with dimension ", paste0(x_dim, collapse = ","),
      " to an rvar with dimension ", paste0(y_dim, collapse = ","))
  }

  x
}


# helpers: arrays/lists -----------------------------------------------------------------

# convert into a list of draws for applying a function draw-wise
list_of_draws <- function(x) {
  lapply(apply(draws_of(x), 1, list), `[[`, 1)
}

dim2_common <- function(dim_x, dim_y) {
  # find common dim for two arrays to be broadcast to
  ndim_x <- length(dim_x)
  ndim_y <- length(dim_y)

  if (ndim_x < ndim_y) {
    dim_x <- c(dim_x, rep(1, ndim_y - ndim_x))
  } else {
    dim_y <- c(dim_y, rep(1, ndim_x - ndim_y))
  }

  pmax(dim_x, dim_y)
}

dim_common <- function(dims) {
  Reduce(dim2_common, dims)
}

broadcast_array  <- function(x, dim, broadcast_scalars = TRUE) {
  if (!broadcast_scalars && length(x) == 1) {
    # quick exit: not broadcasting scalars; return them as vectors
    dim(x) <- NULL
    return(x)
  }

  current_dim = dim(x)

  if (length(current_dim) < length(dim)) {
    # add dimensions of size 1 as necessary so we can broadcast those
    current_dim[seq(length(current_dim) + 1, length(dim))] = 1
    dim(x) = current_dim
  } else if (length(current_dim) > length(dim)) {
    stop_no_call(
      "Cannot broadcast array of shape [", paste(current_dim, collapse = ","), "] ",
      "to array of shape [", paste(dim, collapse = ","), "]:\n",
      "Desired shape has fewer dimensions than existing array."
    )
  }

  dim_to_broadcast = which(current_dim != dim)

  if (length(dim_to_broadcast) == 0) {
    # quick exit: already has desired dim or just needed extra dims on the end
    return(x)
  }

  if (any(current_dim[dim_to_broadcast] != 1)) {
    stop_no_call(
      "Cannot broadcast array of shape [", paste(current_dim, collapse = ","), "] ",
      "to array of shape [", paste(dim, collapse = ","), "]:\n",
      "All dimensions must be 1 or equal."
    )
  }

  # move the dims we aren't broadcasting to the front so they are recycled properly
  perm = c(seq_along(dim)[-dim_to_broadcast], dim_to_broadcast)

  # broadcast the other dims
  x = array(aperm(x, perm), dim[perm])

  # move dims back to their original order
  aperm(x, order(perm))
}

# broadcast the draws dimension of an rvar to the requested size
broadcast_draws <- function(x, .ndraws, keep_constants = FALSE) {
  ndraws_x = ndraws(x)
  if (
    (ndraws_x == 1 && keep_constants) ||
    (ndraws_x == .ndraws)
  ) {
    x
  } else {
    draws <- draws_of(x)
    new_dim <- dim(draws)
    new_dim[1] <- .ndraws

    new_rvar(broadcast_array(draws, new_dim), .nchains = nchains(x))
  }
}

# flatten dimensions and names of an array
flatten_array = function(x, x_name = NULL) {
  # determine new dimension names in the form x,y,z
  # start with numeric names
  dimname_lists = lapply(dim(x), seq_len)
  .dimnames = dimnames(x)
  if (!is.null(.dimnames)) {
    # where character names are provided, use those instead of the numeric names
    dimname_lists = lapply(seq_along(dimname_lists), function(i) .dimnames[[i]] %||% dimname_lists[[i]])
  }
  # expand out the dimname lists into the appropriate combinations and assemble into new names
  dimname_grid <- expand.grid(dimname_lists)
  new_names <- apply(dimname_grid, 1, paste0, collapse = ",")

  dim(x) <- prod(dim(x))

  # update variable names
  if (is.null(x_name)) {
    # no base name for x provided, just use index names
    names(x) <- new_names
  } else if (length(x) > 1) {
    # rename the variables with their indices in brackets
    names(x) <- paste0(x_name, "[", new_names %||% seq_along(x), "]")
  } else {
    # just one variable, use the provided base name
    names(x) <- x_name
  }

  x
}

#' copy the dimension names (and name of the dimension) from dimension src_i
#' in array src to dimension dst_i in array dst
#' @noRd
copy_dimnames <- function(src, src_i, dst, dst_i) {
  if (is.null(dimnames(dst))) {
    if (is.null(dimnames(src))) {
      return(dst)
    }
    dimnames(dst) <- list(NULL)
  }

  if (is.null(dimnames(src))) {
    dimnames(src) <- list(NULL)
  }
  if (is.null(names(dimnames(src)))) {
    names(dimnames(src)) <- rep("", length(dim(src)))
  }

  dimnames(dst)[dst_i] <- dimnames(src)[src_i]
  names(dimnames(dst))[dst_i] <- names(dimnames(src))[src_i]
  names(dimnames(dst))[is.na(names(dimnames(dst)))] <- ""
  if (all(names(dimnames(dst)) == "")) {
    names(dimnames(dst)) <- NULL
  }

  dst
}

#' Drop the chain dimension from an array (presumed to be the second dim)
#' @noRd
drop_chain_dim <- function(x) {
  if (length(x) == 0) {
    # quick exit: NULL input
    dim(x) <- c(1,0)
    return(x)
  }

  x_dim <- dim(x)
  if (length(x_dim) < 3) {
    stop_no_call("Cannot use an array of dimension less than 3 when with_chains equals TRUE")
  }

  out <- x
  dim(out) <- c(x_dim[[1]] * x_dim[[2]], x_dim[-c(1,2)])
  x_dim_i <- seq_along(x_dim)[-c(1,2)]
  out <- copy_dimnames(x, x_dim_i, out, x_dim_i - 1)
  out
}

# helpers: applying functions over rvars ----------------------------------

# apply a summary function within each draw of the rvar (dropping other dimensions)
summarise_rvar_within_draws <- function(x, .f, ..., .transpose = FALSE, .when_empty = .f()) {
  draws <- draws_of(x)
  dim <- dim(draws)
  if (!length(dim)) {
    # x is a NULL rvar, need to return base value for this summary function
    as_rvar(.when_empty)
  } else {
    draws <- apply(draws, 1, .f, ...)
    if (.transpose) draws <- t(draws)
    new_rvar(draws, .nchains = nchains(x))
  }
}

# apply vectorized function to an rvar's draws
rvar_apply_vec_fun <- function(.f, x, ...) {
  draws_of(x) <- .f(draws_of(x), ...)
  x
}

# apply a summary function across draws of the rvar (i.e., by each element)
summarise_rvar_by_element <- function(x, .f, ...) {
  draws <- draws_of(x)
  dim <- dim(draws)
  apply(draws, seq_along(dim)[-1], .f, ...)
}
