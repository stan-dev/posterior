#' Random variables of arbitrary dimension
#'
#' Random variables backed by arrays of arbitrary dimension
#'
#' @name rvar
#'
#' @param x A vector or array where the last dimension is draws from
#' a distribution. The resulting [rvar] will have dimension `dim(x)[-length(dim(x))]`; that is,
#' everything up to the last dimension is used for the shape of the variable, and the
#' last dimension is used to index draws from the distribution.
#' @template args-rvar-dim
#'
#' @details The `"rvar"` class represents random variables as arrays of arbitrary
#' dimension, where the last dimension is used to index draws from the distribution.
#
#' `new_rvar()` is a low-level constructor; generally speaking you should use `rvar()`
#' in most code. To convert other objects to `rvar`s or to easily create constants,
#' use [as_rvar()].
#
#' Most mathmetical operators and functions are supported, including efficient matrix
#' multiplication and vector and array-style indexing. The intent is that an `rvar`
#' works as closely as possible to how a base vector/matrix/array does.
#'
#' For functions that expect base numeric arrays and for which `rvar`s cannot be
#' used directly as arguments, you can use [rfun()] or [rdo()] to translate your
#' code into code that executes across draws from one or more random variables
#' and returns a random variable as output. Typically [rdo()] offers the most
#' straightforward translation.
#'
#' For faster operation than is possible with [rfun()] or [rdo()] (if those functions
#' are not sufficiently performant for your use case), you can also operate directly
#' on the underlying array using the [draws_of()] function.
#'
#' @seealso [as_rvar()] to convert objects to `rvar`s.
#'
#' @return An object of class `"rvar"` representing a random variable.
#'
NULL

#' @rdname rvar
#' @importFrom vctrs new_vctr
new_rvar <- function(x = double()) {
  # TODO: decide on supported types and cast to them in here
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

  structure(list(), draws = x, class = c("rvar", "vctrs_vctr", "list"))
}

#' @rdname rvar
#' @export
rvar <- function(x = double(), dim = NULL) {
  x <- new_rvar(x)

  if (!is.null(dim)) {
    dim(x) <- dim
  }
  x
}


#' Is `x` a random variables?
#'
#' Test if `x` is an [rvar].
#'
#' @param x An object
#'
#' @seealso [as_rvar()] to convert objects to `rvar`s.
#'
#' @return `TRUE` if `x` is an [rvar], `FALSE` otherwise.
#'
#' @export
is_rvar <- function(x) {
  inherits(x, "rvar")
}


# length and dimensions ---------------------------------------------------

#' @export
length.rvar <- function(x) {
  prod(dim(x))
}

#' @export
dim.rvar <- function(x) {
  dim(draws_of(x))[-1]
}

#' @export
`dim<-.rvar` <- function(x, value) {
  if (length(value) == 0) {
    # vectors have NULL dim; for us that means
    # dim of c(ndraws(x), length(x))
    value = length(x)
  }
  dim(draws_of(x)) <- c(ndraws(x), value)
  x
}

#' @export
dimnames.rvar <- function(x) {
  .dimnames <- dimnames(draws_of(x))
  .dimnames[-1]
}

#' @export
`dimnames<-.rvar` <- function(x, value) {
  dimnames(draws_of(x)) <- c(list(NULL), value)
  x
}

#' @export
names.rvar <- function(x) {
  .dimnames <- dimnames(draws_of(x))
  .dimnames[[2]]
}

#' @export
`names<-.rvar` <- function(x, value) {
  dimnames(draws_of(x))[[2]] <- value
  x
}

# other standard methods --------------------------------------------------

#' @export
is.matrix.rvar <- function(x) {
  length(dim(draws_of(x))) == 3
}

#' @export
is.array.rvar <- function(x) {
  length(dim(draws_of(x))) > 0
}

#' @export
levels.rvar <- function(x) {
  # TODO: for factor-like rvars
  NULL
}

#' @export
rep.rvar <- function(x, times = 1, length.out = NA, each = 1, ...) {
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
    new_rvar(rep_draws)
  } else {
    # use `length.out`
    rep_draws = rep_len(draws, length.out * ndraws(x))
    dim(rep_draws) = c(ndraws(x), length(rep_draws) / ndraws(x))
    new_rvar(rep_draws)
  }
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
    stop2("MARGIN = ", MARGIN, " is invalid for dim = ", paste0(dim(x), collapse = ","))
  }
  MARGIN + 1
}

#' @export
all.equal.rvar <- function(target, current, ...) {
  if (!inherits(target, "rvar")) {
    return("'target' is not an rvar")
  }
  if (!inherits(current, "rvar")) {
    return("'current' is not a rvar")
  }
  result = NULL

  class_result = all.equal(class(current), class(target), ...)
  if (!isTRUE(class_result)) {
    result = c(result, paste("Class: <", class_result, ">"))
  }

  object_result = all.equal(unclass(target), unclass(current), ...)
  if (!isTRUE(object_result)) {
    result = c(result, object_result)
  }

  if (is.null(result)) TRUE else result
}

#' @export
as.vector.rvar <- function(x, mode = "any") {
  x
}

#' @export
as.list.rvar <- function(x, ...) {
  apply(draws_of(x), 2, new_rvar)
}


# indexing ----------------------------------------------------------------

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
    new_rvar(.draws)
  } else if (length(index) == length(dim(x))) {
    # multiple element selection => must have exactly the right number of dims
    .draws <- eval_tidy(expr(draws_of(x)[, !!!index, drop = FALSE]))
    # must do drop manually in case the draws dimension has only 1 draw
    dim(.draws) <- c(ndraws(x), 1)
    new_rvar(.draws)
  } else {
    stop("subscript out of bounds")
  }
}

#' @export
`[[<-.rvar` <- function(x, i, ..., value) {
  value <- vec_cast(value, x)
  check_rvar_ndraws_first(value, x)
  value <- check_rvar_dims_first(value, new_rvar(0))
  index <- check_rvar_yank_index(x, i, ...)

  if (length(index) == 1) {
    .dim = dim(x)

    if (length(.dim) == 1 && i > length(x)) {
      # unidimensional indexing allows array extension; extend the array
      # then do the assignment
      x <- x[seq_len(max(i, na.rm = TRUE))]
      draws_of(x)[, i] <- draws_of(value)
      x
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
      x
    }
  } else if (length(index) == length(dim(x))) {
    # multiple element selection => must have exactly the right number of dims
    eval_tidy(expr({
      draws_of(x)[, !!!index] <- draws_of(value)
      x
    }))
  } else {
    stop("subscript out of bounds")
  }
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

  # fill in final indices with missing arguments
  if (length(index) < length(dim(.draws)) - 1) {
    index[seq(length(index) + 1, length(dim(.draws)) - 1)] = list(missing_arg())
  }

  x = eval_tidy(expr(new_rvar(.draws[, !!!index, drop = FALSE])))

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
  x <- check_rvar_ndraws_first(value, x)
  #TODO: reinstate
#  value <- check_rvar_dims_first(value, x[i, ...])

  draws_of(x)[,i,...] <- draws_of(value)
  x
}


# manipulating raw draws array --------------------------------------------

#' Get/set array of draws underlying a random variable
#'
#' Gets/sets the array-representation that backs an [rvar]
#'
#' @param x An [rvar]
#' @param value An array
#'
#' @details
#'
#' While [rvar]s implements fast versions of basic math operations (including
#' [matrix multiplication][rvar-matmult]), sometimes you may need to bypass
#' the [rvar] abstraction to do what you need to do more efficiently.
#' `draws_of()` allows you to get / set the underlying array of draws in
#' order to do that.
#'
#' [rvar]s represent draws internally using arrays of arbitrary dimension, which
#' is returned by `draws_of(x)` and can be set using `draws_of(x) <- value`.
#' The **last** dimension of these arrays is the index of the draws.
#'
#' @export
draws_of <- function(x) {
  attr(x, "draws")
}

#' @rdname draws_of
#' @export
`draws_of<-` <- function(x, value) {
  attr(x, "draws") <- value
  x
}


# vctrs stuff -------------------------------------------------------------

#' @importFrom vctrs vec_proxy vec_chop
#' @export
vec_proxy.rvar = function(x, ...) {
  # TODO: probably could do something more efficient here and for restore
  .draws = draws_of(x)
  vec_chop(aperm(.draws, c(2, 1, seq_along(dim(.draws))[c(-1,-2)])))
}

#' @importFrom vctrs vec_restore
#' @export
vec_restore.rvar <- function(x, ...) {
  if (length(x) > 0) {
    # need to handle the case of creating NAs from NULL entries so that
    # vec_init() works properly: vec_init requires vec_slice(x, NA_integer_)
    # to give you back NA values, but this breaks because we use lists as proxies.
    # When using a list as a proxy, a proxy entry in `x` that is equal to NULL
    # actually corresponds to an NA value due to the way that list indexing
    # works: when you do something like list()[c(NA_integer_,NA_integer_)]
    # you get back list(NULL, NULL), but when you do something like
    # double()[c(NA_integer_,NA_integer_)] you get back c(NA, NA).
    # So we have to make the NULL values be NA values to mimic vector indexing.

    # N.B. could potentially do this with vec_cast as well (as long as the first
    # dimension is the slicing index)
    x[sapply(x, is.null)] <- list(array(NA, dim = c(1,1)))

  }
  # broadcast dimensions and bind together
  new_dim <- dim_common(lapply(x, dim))
  .draws <- abind(lapply(x, broadcast_array, new_dim), along = 1)
  # move draws dimension back to the front
  if (!is.null(.draws)) {
    .draws <- aperm(.draws, c(2, 1, seq_along(dim(.draws))[c(-1,-2)]))
  }
  new_rvar(.draws)
}


# distributional stuff ----------------------------------------------------

#' @importFrom distributional cdf
#' @export
distributional::cdf

#' @export
cdf.rvar <- function(x, q, ...) {
  if (length(x) != 1) {
    stop("cdf() can currently only be used on single rvars")
  }

  ecdf(draws_of(x))(q)
}

#' @export
quantile.rvar <- function(x, probs, ...) {
  quantile(draws_of(x), probs, ...)
}

#' @export
density.rvar <- function(x, at, ...) {
  if (length(x) != 1) {
    stop("density() can currently only be used on single rvars")
  }

  d <- density(draws_of(x), cut = 0, ...)
  f <- approxfun(d$x, d$y, yleft = 0, yright = 0)
  f(at)
}

# concatenation -----------------------------------------------------------

#' @export
c.rvar <- function(...) {
  combine_rvar(c, list(...))
}

#' @export
rbind.rvar <- function(...) {
  bind_rvar(rbind, list(...))
}

#' @export
cbind.rvar <- function(...) {
  bind_rvar(cbind, list(...), .axis = 3)
}

bind_rvar <- function(.f, args, .axis = 2) {
  if (is.null(dim(args[[1]]))) {
    dim(args[[1]]) <- c(length(args[[1]]), 1)
  }
  if (length(args) == 1) {
    return(args[[1]])
  }

  if (is.data.frame(args[[2]])) {
    # TODO
    stop2("not implemented")
  }

  args[[2]] <- as_rvar(args[[2]])
  if (is.null(dim(args[[2]]))) {
    dim(args[[2]]) <- c(length(args[[2]]), 1)
  }

  combine_rvar(.f, args, .axis = .axis)
}

combine_rvar <- function(.f, args, .axis = 2) {
  if (length(args) == 1) {
    return(args[[1]])
  }

  # broadcast each array to the desired dimensions
  # (except along the axis we are binding along)
  draws1 <- draws_of(args[[1]])
  draws2 <- draws_of(as_rvar(args[[2]]))
  new_dim <- dim2_common(dim(draws1), dim(draws2))

  new_dim[.axis] <- dim(draws1)[.axis]
  draws1 <- broadcast_array(draws1, new_dim)

  new_dim[.axis] <- dim(draws2)[.axis]
  draws2 <- broadcast_array(draws2, new_dim)

  # bind along desired axis
  result <- new_rvar(abind(draws1, draws2, along = .axis))

  if (length(args) > 2) {
    args[[1]] <- result
    args[[2]] <- NULL
    do.call(.f, args)
  } else {
    result
  }
}


# chain / iteration / draw info -------------------------------------------

#' @export
ndraws.rvar <- function(x) {
  dim(draws_of(x))[1]
}


# helpers -----------------------------------------------------------------

# convert into a list of draws for applying a function draw-wise
list_of_draws <- function(x) {
  lapply(apply(draws_of(x), 1, list), `[[`, 1)
}


# Check the passed yank index (for x[[...]]) is valid
check_rvar_yank_index = function(x, i, ...) {
  index <- dots_list(i, ..., .preserve_empty = TRUE, .ignore_empty = "none")

  if (any(lengths(index)) > 1) {
    stop("Cannot select more than one element per index with `[[` in an rvar.")
  } else if (any(sapply(index, function(x) is_missing(x) || is.na(x)))) {
    stop("Missing indices not allowed with `[[` in an rvar.")
  } else if (any(sapply(index, is.logical))) {
    stop("Logical indices not allowed with `[[` in an rvar.")
  } else if (any(sapply(index, function(x) x < 0))) {
    stop("subscript out of bounds")
  }

  index
}

# Check the passed subset indices (for x[...]) do not go beyond the end
# of the valid dimensions
check_rvar_subset_indices = function(x, ...) {
  ndim = max(length(dim(x)), 1)
  if (length(substitute(list(...))) - 1 > ndim) {
    stop2("Cannot index past dimension ", ndim, ".")
  }
}

# Check that the first rvar has a compatible number of draws to be used
# with the second. Returns a (possibly modified) form of `y` if
# the number of draws needs to be conformed (when ndraws(y) == 0)
check_rvar_ndraws_first <- function(x, y) {
  ndraws_x <- ndraws(x)
  ndraws_y <- ndraws(y)

  if (ndraws_x == 1 || ndraws_x == ndraws_y) {
    # ndraws_x == 1 => assigning a constant, which is fine
    # ndraws_y
    y
  } else if (ndraws_y == 0) {
    # ndraws_y == 0 => assigning to an empty vector, use ndraws_x
    draws_y = draws_of(y)
    new_dim = dim(draws_y)
    new_dim[length(new_dim)] <- ndraws_x
    draws_of(y) <- array(vec_ptype(draws_y), dim = new_dim)
    y
  } else {
    stop(
      "Random variables have different number of draws (", ndraws_x,
      " and ", ndraws_y, ") and cannot be used together."
    )
  }
}

# Check that two rvars have a compatible number of draws and
# return an appropriate number of draws that both objects could be broadcasted
# to, or throw an error if there is no such number of draws.
check_rvar_ndraws_both <- function(x, y) {
  ndraws_x <- ndraws(x)
  ndraws_y <- ndraws(y)

  if (ndraws_x == 1) {
    ndraws_y
  } else if (ndraws_y == 1) {
    ndraws_x
  } else if (ndraws_x == ndraws_y) {
    ndraws_x
  } else {
    stop(
      "Random variables have different number of draws (", ndraws_x,
      " and ", ndraws_y, ") and cannot be used together."
    )
  }
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
  } else if (y_dim == 0) {
    # y_dim == 0 => assigning to an empty vector, can leave it as is
    # TODO: this is a hack for assignment to empty vectors and needs to be fixed
  } else {
    stop2("Cannot assign an rvar with dimension ", paste0(x_dim, collapse = ","),
      " to an rvar with dimension ", paste0(y_dim, collapse = ","))
  }

  x
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

broadcast_array  <- function(x, dim) {
  current_dim = dim(x)

  if (length(current_dim) < length(dim)) {
    # add dimensions of size 1 as necessary so we can broadcast those
    current_dim[seq(length(current_dim) + 1, length(dim))] = 1
    dim(x) = current_dim
  } else if (length(current_dim) > length(dim)) {
    stop(
      "Cannot broadcast array of shape [", paste(current_dim, collapse = ","), "]",
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
    stop(
      "Cannot broadcast array of shape [", paste(current_dim, collapse = ","), "]",
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
broadcast_draws <- function(x, .ndraws) {
  if (.ndraws == ndraws(x)) {
    x
  } else {
    draws <- draws_of(x)
    new_dim <- dim(draws)
    new_dim[1] <- .ndraws
    new_rvar(broadcast_array(draws, new_dim))
  }
}

drop_ <- function(x) {
  .dim <- dim(x)

  if (!all(.dim == 1)) {
    # with exactly 1 element left we don't want to drop anything
    # (otherwise names get lost), so only do this with > 1 element
    .dimnames <- dimnames(x)
    dim(x) <- .dim[.dim != 1]
    dimnames(x) <- .dimnames[.dim != 1]
  }

  x
}

# apply a summary function within each draw of the rvar (dropping other dimensions)
summarise_rvar_within_draws <- function(x, .f, ...) {
  draws <- draws_of(x)
  dim <- dim(draws)
  new_rvar(apply(draws, 1, .f, ...))
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
