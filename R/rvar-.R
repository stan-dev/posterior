#' Random variables of arbitrary dimension
#'
#' Random variables backed by arrays of arbitrary dimension
#'
#' @name rvar
#'
#' @param x (multiple options) The object to convert to an `rvar`:
#'   * A vector of draws from a distribution.
#'   * An array where the first dimension represents draws from a distribution.
#'     The resulting [`rvar`] will have dimension `dim(x)[-1]`; that is,
#'     everything except the first dimension is used for the shape of the
#'     variable, and the first dimension is used to index draws from the
#'     distribution (see **Examples**). Optionally,
#'     if `with_chains == TRUE`, the first dimension indexes the iteration and the
#'     second dimension indexes the chain (see `with_chains`).
#'   * An `rvar`.
#' @template args-rvar-dim
#' @template args-rvar-dimnames
#' @param nchains (positive integer) The number of chains. The if `NULL` (the default),
#' `1` is used unless `x` is already an [`rvar`], in which case the number of
#' chains it has is used.
#' @param with_chains (logical) Does `x` include a dimension for chains?
#' If `FALSE` (the default), chains are not included, the first dimension of
#' the input array should index draws, and the `nchains` argument can be
#' used to determine the number of chains. If `TRUE`, the `nchains` argument
#' is ignored and the second dimension of `x` is used to index chains.
#' Internally, the array will be converted to a format without the chain index.
#' Ignored when `x` is already an [`rvar`].
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
#' set.seed(1234)
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
#' # If the input sample comes from multiple chains, we can indicate that using the
#' # nchains argument (here, 1000 draws each from 4 chains):
#' x <- rvar(rnorm(4000, mean = 1, sd = 1), nchains = 4)
#' x
#'
#' # Or if the input sample has chain information as its second dimension, we can
#' # use with_chains to create the rvar
#' x <- rvar(array(rnorm(4000, mean = 1, sd = 1), dim = c(1000, 4)), with_chains = TRUE)
#' x
#'
#' @export
rvar <- function(x = double(), dim = NULL, dimnames = NULL, nchains = NULL, with_chains = FALSE) {
  if (is_rvar(x)) {
    nchains <- nchains %||% nchains(x)
    with_chains = FALSE
    x <- draws_of(x)
  }

  with_chains <- as_one_logical(with_chains)
  if (with_chains) {
    nchains <- dim(x)[[2]] %||% 1L
    x <- drop_chain_dim(x)
  } else {
    nchains <- nchains %||% 1L
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
  if (is.null(x)) {
    x <- double()
  }

  x <- cleanup_rvar_draws(x)

  .ndraws <- dim(x)[[1]]
  .nchains <- as_one_integer(.nchains)
  check_nchains_compat_with_ndraws(.nchains, .ndraws)

  structure(
    list(),
    draws = x,
    nchains = .nchains,
    class = get_rvar_class(x),
    cache = new.env(parent = emptyenv())
  )
}


# manipulating raw draws array --------------------------------------------

#' Get/set array of draws underlying a random variable
#'
#' Gets/sets the array-representation that backs an [`rvar`]. Should be used rarely.
#'
#' @param x (rvar) An [`rvar`] object.
#' @param value (array) An array of values to use as the backing array of `x`.
#' @param with_chains (logical) Should the array of draws include a dimension for chains?
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
#' @return
#'
#' If `with_chains = FALSE`, an array with dimensions `c(ndraws(x), dim(x))`.
#'
#' If `with_chains = TRUE`, an array with dimensions
#' `c(niterations(x), nchains(x), dim(x))`.
#'
#'
#' @examples
#'
#' x <- rvar(1:10, nchains = 2)
#' x
#'
#' # draws_of() without arguments will return the array of draws without
#' # chain information (first dimension is draw)
#' draws_of(x)
#'
#' # draws_of() with with_chains = TRUE will reshape the returned array to
#' # include chain information in the second dimension
#' draws_of(x, with_chains = TRUE)
#'
#' # you can also set draws using draws_of(). When with_chains = FALSE the
#' # existing chain information will be retained ...
#' draws_of(x) <- 2:11
#' x
#'
#' # when with_chains = TRUE the chain information will be set by the
#' # second dimension of the assigned array
#' draws_of(x, with_chains = TRUE) <- array(2:11, dim = c(2,5))
#' x
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
`draws_of<-` <- function(x, with_chains = FALSE, value) {
  with_chains <- as_one_logical(with_chains)

  if (with_chains) {
    draws <- drop_chain_dim(value)
    nchains_rvar(x) <- dim(value)[[2]] %||% 1L
  } else {
    draws <- value
  }

  draws <- cleanup_rvar_draws(draws)
  attr(x, "draws") <- draws
  class(x) <- get_rvar_class(draws)

  x <- invalidate_rvar_cache(x)
  x
}


# misc standard methods --------------------------------------------------

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

#' @rawNamespace S3method(rep.int,rvar,rep_int_rvar)
rep_int_rvar <- function(x, times, ...) {
  rep(x, times = times, ...)
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
    stop_no_call("MARGIN = ", MARGIN, " is invalid for length(dim(x)) = ", length(dim(x)))
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

  # ignore cache in comparison
  .target <- unclass(target)
  attr(.target, "cache") <- NULL
  .current <- unclass(current)
  attr(.current, "cache") <- NULL

  object_result <- all.equal(.target, .current, ...)
  if (!isTRUE(object_result)) {
    result = c(result, object_result)
  }

  if (is.null(result)) TRUE else result
}


# match and %in% ----------------------------------------------------------

#' Value Matching
#'
#' Generic version of [base::match()]. For base vectors, returns a vector of the
#' positions of (first) matches of its first argument in its second. For [rvar]s,
#' returns an [rvar] of the matches.
#'
#' @inheritDotParams base::match
#' @param x (multiple options) the values to be matched. Can be:
#'  - A base vector: see [base::match()]
#'  - An [rvar]
#' @param table (vector) the values to be matched against.
#'
#' @details
#' For more information on how match behaves with base vectors, see [base::match()].
#'
#' When `x` is an [rvar], the draws of `x` are matched against `table` using
#' [base::match()], and the result is returned as an [rvar].
#'
#' The implementation of `%in%` here is identical to \code{base::%in%}, except
#' it uses the generic version of `match()` so that non-base vectors (such
#' as [rvar]s) are supported.
#'
#' @returns
#' When `x` is a base vector, a vector of the same length as `x`.
#'
#' When `x` is an [rvar], an [rvar] the same shape as `x`.
#' @examples
#' x <- rvar(c("a","b","b","c","d"))
#' x %in% c("b","d")
#'
#' # for additional examples, see base::match()
#' @export
match <- function(x, table, ...) UseMethod("match")

#' @rdname match
#' @export
match.default <- function(x, ...) base::match(x, ...)

#' @rdname match
#' @export
match.rvar <- function(x, ...) {
  draws_of(x) <- while_preserving_dims(base::match, draws_of(x), ...)
  x
}

#' @rdname match
#' @export
`%in%` <- function(x, table) {
  match(x, table, nomatch = 0L) > 0L
}


# ggplot2::scale_type -----------------------------------------------------

# This generic is not exported here as {ggplot2} is only in Suggests, so
# we must export it in .onLoad() for compatibility with R < 3.6.
scale_type.rvar <- function(x) {
  # ensure that rvars used in ggplot2 aesthetics (as in e.g. ggdist) are passed
  # through without modification and without raising a warning
  "identity"
}


# helpers: classes --------------------------------------------------------

get_rvar_class <- function(x) {
  UseMethod("get_rvar_class")
}

#' @export
get_rvar_class.default <- function(x) {
  c("rvar", "vctrs_vctr")
}

#' @export
get_rvar_class.factor <- function(x) {
  c("rvar_factor", NextMethod())
}

#' @export
get_rvar_class.ordered <- function(x) {
  c("rvar_ordered", NextMethod())
}

#' @importFrom methods setOldClass
setOldClass(get_rvar_class(numeric()))
setOldClass(get_rvar_class(factor()))
setOldClass(get_rvar_class(ordered(NULL)))


# helpers: validation -----------------------------------------------------------------

# Check the passed yank index (for x[[...]]) is valid
check_rvar_yank_index = function(x, i, ...) {
  index <- dots_list(i, ..., .preserve_empty = TRUE, .ignore_empty = "none")

  index_lengths <- lengths(index)
  if (any(index_lengths == 0)) {
    stop_no_call("Cannot select zero elements with `[[` in an rvar.")
  } else if (any(index_lengths > 1)) {
    stop_no_call("Cannot select more than one element per index with `[[` in an rvar.")
  } else if (any(vapply(index, function(x) is_missing(x) || is.na(x), logical(1)))) {
    stop_no_call("Missing indices not allowed with `[[` in an rvar.")
  } else if (any(vapply(index, is.logical, logical(1)))) {
    stop_no_call("Logical indices not allowed with `[[` in an rvar.")
  } else if (any(vapply(index, function(x) x < 0, logical(1)))) {
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
    warn_merge_chains("match")
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
    nchains_rvar(rvars[[i]]) <- .nchains
  }

  rvars
}

# given two rvars, conform their number of draws
# so they can be used together (or throw an error if they can't be)
# @param keep_constants keep constants as 1-draw rvars
conform_rvar_ndraws <- function(rvars, keep_constants = FALSE) {
  # broadcast to a common number of chains. If keep_constants = TRUE,
  # constants will not be broadcast.
  .ndraws = Reduce(ndraws2_common, lapply(rvars, ndraws))
  for (i in seq_along(rvars)) {
    rvars[[i]] <- broadcast_draws(rvars[[i]], .ndraws, keep_constants)
  }

  rvars
}

# given two rvars, conform their number of draws and chains
# so they can be used together (or throw an error if they can't be)
# @param keep_constants keep constants as 1-draw rvars
conform_rvar_ndraws_nchains <- function(rvars, keep_constants = FALSE) {
  rvars <- conform_rvar_nchains(rvars)
  rvars <- conform_rvar_ndraws(rvars)
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

#' Get one draw from a draws_rvars as a list of base-R variables
#' @param x a draws_rvars
#' @param i a draw index
#' @returns a named list of vectors and arrays
#' @noRd
get_variables_from_one_draw <- function(x, i) {
  lapply(x, function(variable) {
    draws <- draws_of(variable)
    .dim <- dim(variable)
    ndim <- length(.dim)

    if (ndim <= 1) {
      # treat 0- and 1-dimensional arrays as vectors
      draws <- draws[i, ]
      dim(draws) <- NULL
      names(draws) <- names(variable)
    } else {
      dim(draws) <- c(NROW(draws), length(variable))
      draws <- draws[i, ]
      dim(draws) <- .dim
      dimnames(draws) <- dimnames(variable)
    }

    draws
  })
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

  current_dim <- dim(x)
  current_dimnames <- dimnames(x)
  current_levels <- levels(x)
  current_class <- oldClass(x)

  if (length(current_dim) < length(dim)) {
    # add dimensions of size 1 as necessary so we can broadcast those
    new_dim <- seq(length(current_dim) + 1, length(dim))
    current_dim[new_dim] <- 1
    dim(x) <- current_dim
    if (!is.null(current_dimnames)) {
      current_dimnames[new_dim] <- list(NULL)
      dimnames(x) <- current_dimnames
    }
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
  perm <- c(seq_along(dim)[-dim_to_broadcast], dim_to_broadcast)

  # broadcast the other dims
  x <- array(aperm(x, perm), dim[perm])

  # move dims back to their original order
  x <- aperm(x, order(perm))

  if (!is.null(current_dimnames)) {
    # restore any dimnames that we did not have to broadcast
    dim_to_restore <- current_dim == dim
    dimnames(x)[dim_to_restore] <- current_dimnames[dim_to_restore]
  }

  # restore class and levels
  levels(x) <- current_levels
  oldClass(x) <- current_class

  x
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
  dimname_lists <- lapply(dim(x), seq_len)
  .dimnames <- dimnames(x)
  if (!is.null(.dimnames)) {
    # where character names are provided, use those instead of the numeric names
    dimname_lists = lapply(seq_along(dimname_lists), function(i) .dimnames[[i]] %||% dimname_lists[[i]])
  }
  # expand out the dimname lists into the appropriate combinations and assemble into new names
  dimname_grid <- expand.grid(dimname_lists)
  new_names <- apply(dimname_grid, 1, paste0, collapse = ",")

  .length <- length(x)
  old_dim <- dim(x)
  dim(x) <- .length

  # update variable names
  if (is.null(x_name)) {
    # no base name for x provided, just use index names
    names(x) <- new_names
  } else if (.length == 1 && (isTRUE(old_dim == 1) || length(old_dim) == 0)) {
    # scalar, use the provided base name
    names(x) <- x_name
  } else if (.length >= 1) {
    # rename the variables with their indices in brackets
    names(x) <- paste0(x_name, "[", new_names %||% seq_along(x), "]")
  }

  x
}

#' Fast conversion of rvar draws to a flattened data frame. Equivalent to
#' as.data.frame(draws_of(flatten_array(x, name))) except it works with
#' factor rvars (as.data.frame does not work on array-like factors)
#' @noRd
flatten_rvar_draws_to_df <- function(x, x_name = NULL) {
  if (length(x) == 0) {
    out <- data.frame(row.names = draw_ids(x))
  } else {
    draws <- draws_of(flatten_array(x, x_name))
    cols <- lapply(seq_len(ncol(draws)), function(i) unname(draws[, i]))
    names(cols) <- colnames(draws)
    out <- vctrs::new_data_frame(cols)
  }
  out
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

  dst_dimnames <- names(dimnames(dst))
  empty_names <- is.na(dst_dimnames) | dst_dimnames == ""
  if (all(empty_names)) {
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
  if (length(x_dim) < 2) {
    stop_no_call("Cannot use an array of dimension less than 2 when with_chains equals TRUE")
  }

  out <- x
  dim(out) <- c(x_dim[[1]] * x_dim[[2]], x_dim[-c(1,2)])
  x_dim_i <- seq_along(x_dim)[-c(1,2)]
  out <- copy_dimnames(x, x_dim_i, out, x_dim_i - 1)
  out
}

#' Clean up the data type and dimensions of an array of draws intended to back an rvar.
#' Ensures that dim and dimnames are set, that the array has at least two dimensions
#' (first one is draws), etc.
#' @noRd
cleanup_rvar_draws <- function(x) {
  if (length(x) == 0) {
    # canonical NULL rvar is at least 1 draw of nothing
    # this ensures that (e.g.) extending a null rvar
    # with x[1] = something works.
    ndraws <- max(NROW(x), 1)
    dim(x) <- c(ndraws, 0)
  }
  else if (length(dim(x)) <= 1) {
    # 1d vectors get treated as a single variable
    dim(x) <- c(length(x), 1)
  }

  # ensure dimnames is set (makes comparison easier for tests)
  if (length(dimnames(x)) == 0) {
    dimnames(x) <- list(NULL)
  }

  # ensure we have an index for draws
  if (length(rownames(x)) == 0) {
    rownames(x) <- as.character(seq_rows(x))
  }

  # if x is a character array, make it a factor
  if (is.character(x)) {
    x <- while_preserving_dims(factor, x)
  }

  x
}

#' Execute x <- f(x, ...) but preserve dimensions and dimension names of x.
#' Useful for functions that do not change the length of x but which drop
#' dimensions.
#' @noRd
while_preserving_dims <- function(f, x, ...) {
  .dim <- dim(x)
  .dimnames <- dimnames(x)
  x <- f(x, ...)
  dim(x) <- .dim
  dimnames(x) <- .dimnames
  x
}

#' Execute x <- f(x, ...) but preserve class and levels of x.
#' Useful for functions that do not change the length of x but which levels.
#' @noRd
while_preserving_levels <- function(f, x, ...) {
  .class <- oldClass(x)
  .levels <- levels(x)
  x <- f(x, ...)
  oldClass(x) <- .class
  levels(x) <- .levels
  x
}

#' a version of apply() that works on factor-like arrays
#' apply() strips classes of slices of x before passing them to FUN, which
#' breaks apply() when used with factor-like arrays; this variant ensures
#' levels and classes are preserved for slices of factors passed to FUN
#' @noRd
.apply_factor <- function(X, MARGIN, FUN, ...) {
  if (is.factor(X)) {
    .class <- oldClass(X)
    .levels <- levels(X)
    FUN <- match.fun(FUN)
    .f <- function(x, ...) {
      oldClass(x) <- .class
      levels(x) <- .levels
      FUN(x, ...)
    }
  } else {
    .f <- FUN
  }
  apply(X, MARGIN, .f, ...)
}


# helpers: applying functions over rvars ----------------------------------

# apply a summary function within each draw of the rvar (dropping other dimensions)
# summarise_rvar_within_draws_via_matrix should be used instead of this function
# if a faster implementation of .f is available in matrix form (e.g. functions
# from matrixStats); otherwise this function can be used.
summarise_rvar_within_draws <- function(x, .f, ..., .transpose = FALSE, .when_empty = .f(numeric(0))) {
  draws <- draws_of(x)
  dim <- dim(draws)
  if (!length(x)) {
    # x is a NULL rvar, need to return base value for this summary function
    as_rvar(.when_empty)
  } else {
    draws <- apply(draws, 1, .f, ...)
    if (.transpose) draws <- t(draws)
    new_rvar(draws, .nchains = nchains(x))
  }
}

#' apply a summary function within each draw of the rvar (dropping other dimensions)
#' by first collapsing dimensions into columns of the draws matrix
#' (so that .f can be a rowXXX() function)
#' @param x an rvar
#' @param name function name to use for error messages
#' @param .f a function that takes a matrix and summarises its rows, like rowMeans
#' @param ... arguments passed to `.f`
#' @param .ordered_okay can this function be applied to rvar_ordereds?
#' @noRd
summarise_rvar_within_draws_via_matrix <- function(x, .name, .f, ..., .ordered_okay = FALSE) {
  .length <- length(x)
  if (!.length) {
    x <- rvar()
  }

  dim(x) <- .length

  if (.ordered_okay && is_rvar_ordered(x)) {
    .levels <- levels(x)
    .draws <- .f(draws_of(as_rvar_numeric(x)), ...)
    .draws <- while_preserving_dims(function(.draws) ordered(.levels[round(.draws)], .levels), .draws)
  } else if (is_rvar_factor(x)) {
    stop_no_call("Cannot apply `", .name, "` function to rvar_factor objects.")
  } else {
    .draws <- .f(draws_of(x), ...)
  }

  new_rvar(.draws, .nchains = nchains(x))
}

# apply vectorized function to an rvar's draws
rvar_apply_vec_fun <- function(.f, x, ...) {
  draws_of(x) <- .f(draws_of(x), ...)
  x
}

# apply a summary function across draws of the rvar (i.e., by each element)
summarise_rvar_by_element <- function(x, .f, ...) {
  if (length(x) == 1) {
    # this ensures that scalar rvars are summarized to vectors rather than
    # to matrices with one column
    .f(draws_of(x), ...)
  } else {
    draws <- draws_of(x)
    dim <- dim(draws)
    .apply_factor(draws, seq_along(dim)[-1], .f, ...)
  }
}

#' apply a summary function across draws of the rvar (i.e., by each element)
#' by first collapsing dimensions into columns of the draws matrix, applying the
#' function, then restoring dimensions (so that .f can be a colXXX() function)
#' @param x an rvar
#' @param name function name to use for error messages
#' @param .f a function that takes a matrix and summarises its columns, like colMeans
#' @param .extra_dim extra dims added by `.f` to the output, e.g. in the case of
#' matrixStats::colRanges this is `2`
#' @param .extra_dimnames extra dimension names for dims added by `.f` to the output
#' @param .ordered_okay can this function be applied to rvar_ordereds?
#' @param .factor_okay can this function be applied to rvar_factors?
#' @param ... arguments passed to `.f`
#' @noRd
summarise_rvar_by_element_via_matrix <- function(
  x, .name, .f, .extra_dim = NULL, .extra_dimnames = NULL, .ordered_okay = TRUE, .factor_okay = FALSE, ...
) {
  .dim <- dim(x)
  .dimnames <- dimnames(x)
  .length <- length(x)
  dim(x) <- .length

  if (!is_rvar_factor(x) || .factor_okay) {
    x <- .f(draws_of(x), ...)
  } else if (.ordered_okay && is_rvar_ordered(x)) {
    .levels <- levels(x)
    x <- .f(draws_of(as_rvar_numeric(x)), ...)
    x <- ordered(.levels[round(x)], .levels)
  } else {
    stop_no_call("Cannot apply `", .name, "` function to rvar_factor objects.")
  }

  if (is.null(.extra_dim) && length(.dim) <= 1) {
    # this ensures that vector rvars are summarized to vectors rather than
    # to arrays with one dimension
    dim(x) <- NULL
    names(x) <- .dimnames[[1]]
  } else if (isTRUE(.dim == 1)) {
    # scalars with extra dimensions should just return vectors
    dim(x) <- NULL
    names(x) <- .extra_dimnames[[1]]
  } else {
    dim(x) <- c(.extra_dim, .dim)
    dimnames(x) <- c(.extra_dimnames, .dimnames)
  }

  x
}

# apply a summary function across draws of the rvar (i.e., by each element)
# including a chain dimension in the array passed to .f
summarise_rvar_by_element_with_chains <- function(x, .f, ...) {
  draws <- draws_of(x, with_chains = TRUE)
  dim <- dim(draws)
  apply(draws, seq_along(dim)[-c(1,2)], .f, ...)
}
