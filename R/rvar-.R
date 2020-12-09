#' Random variables of arbitrary dimension
#'
#' Random variables backed by arrays of arbitrary dimension
#'
#' @name rvar
#'
#' @param x A vector or array where the first dimension represents draws from
#' a distribution. The resulting [`rvar`] will have dimension `dim(x)[-1]`; that is,
#' everything except the first dimension is used for the shape of the variable, and the
#' first dimension is used to index draws from the distribution.
#' @template args-rvar-dim
#' @template args-format-nchains
#'
#' @details
#'
#' The `"rvar"` class internally represents random variables as arrays of arbitrary
#' dimension, where the first dimension is used to index draws from the distribution.
#
#' Most mathmetical operators and functions are supported, including efficient matrix
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
#' on the underlying array using the [draws_of()] function.
#'
#' @seealso [as_rvar()] to convert objects to `rvar`s.
#'
#' @return An object of class `"rvar"` representing a random variable.
#'
#' @export
rvar <- function(x = double(), dim = NULL, .nchains = 1L) {
  x <- new_rvar(x, .nchains = .nchains)

  if (!is.null(dim)) {
    dim(x) <- dim
  }
  x
}

#' @importFrom vctrs new_vctr
new_rvar <- function(x = double(), .nchains = 1L) {
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

  # except with constants, .nchains must divide the number of draws
  .ndraws <- dim(x)[[1]]
  .nchains <- as_one_integer(.nchains)
  if (.ndraws != 1 && .ndraws %% .nchains != 0) {
    stop2("Number of chains does not divide the number of draws.")
  }
  if (.nchains < 1) {
    stop2("Number of chains must be >= 1")
  }

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

#' Is `x` a random variable?
#'
#' Test if `x` is an [`rvar`].
#'
#' @param x An object
#'
#' @seealso [as_rvar()] to convert objects to `rvar`s.
#'
#' @return `TRUE` if `x` is an [`rvar`], `FALSE` otherwise.
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
  # must keep old rowname around and restore them, since changing dim will drop them
  old_rownames <- rownames(draws_of(x))
  dim(draws_of(x)) <- c(ndraws(x), value)
  rownames(draws_of(x)) <- old_rownames
  x
}

#' @export
dimnames.rvar <- function(x) {
  .dimnames <- dimnames(draws_of(x))
  .dimnames[-1]
}

#' @export
`dimnames<-.rvar` <- function(x, value) {
  dimnames(draws_of(x)) <- c(list(rownames(draws_of(x))), value)
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
  stop("Factor support in rvars is not yet implemented.")
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
    new_rvar(rep_draws, .nchains = nchains(x))
  } else {
    # use `length.out`
    rep_draws = rep_len(draws, length.out * ndraws(x))
    dim(rep_draws) = c(ndraws(x), length(rep_draws) / ndraws(x))
    new_rvar(rep_draws, .nchains = nchains(x))
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

#' @export
as.vector.rvar <- function(x, mode = "any") {
  dim(x) <- NULL
  names(x) <- NULL
  x
}

#' @export
as.list.rvar <- function(x, ...) {
  apply(draws_of(x), 2, new_rvar, .nchains = nchains(x))
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
    new_rvar(.draws, .nchains = nchains(x))
  } else if (length(index) == length(dim(x))) {
    # multiple element selection => must have exactly the right number of dims
    .draws <- eval_tidy(expr(draws_of(x)[, !!!index, drop = FALSE]))
    # must do drop manually in case the draws dimension has only 1 draw
    dim(.draws) <- c(ndraws(x), 1)
    new_rvar(.draws, .nchains = nchains(x))
  } else {
    stop2("subscript out of bounds")
  }
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
    stop2("subscript out of bounds")
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

  x = eval_tidy(expr(
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
  #TODO: reinstate
#  value <- check_rvar_dims_first(value, x[i, ...])

  draws_of(x)[,i,...] <- draws_of(value)
  x
}


# manipulating raw draws array --------------------------------------------

#' Get/set array of draws underlying a random variable
#'
#' Gets/sets the array-representation that backs an [`rvar`]. Should be used rarely.
#'
#' @param x An [`rvar`]
#' @param value An array
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
#' The **first** dimension of these arrays is the index of the draws.
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
  out <- vec_chop(aperm(.draws, c(2, 1, seq_along(dim(.draws))[c(-1,-2)])))
  for (i in seq_along(out)) {
    attr(out[[i]], "nchains") <- nchains(x)
  }
  out
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
  # determine the number of chains
  nchains_or_null <- lapply(x, function(x) if (dim(x)[[2]] %||% 1 == 1) NULL else attr(x, "nchains"))
  .nchains <- Reduce(nchains2_common, nchains_or_null) %||% 1L

  new_rvar(.draws, .nchains = .nchains)
}


# distributional stuff ----------------------------------------------------

#' Density, CDF, and quantile functions of random variables
#'
#' The probability density function (`density()`), cumulative distribution
#' function (`cdf()`), and quantile function / inverse CDF (`quantile()`) of
#' an [`rvar`].
#'
#' @param x an [`rvar`]
#' @param q,at vector of quantiles.
#' @param probs vector of probabilities
#' @param ... Additional arguments passed onto underlying methods:
#'   - For `density()`, these are passed to [stats::density()].
#'   - For `cdf()`, these are ignored.
#'   - For `quantile()`, these are passed to [stats::quantile()].
#'
#' @return
#'
#' A vector of the same length as the input (`q`, `at`, or `probs`) containing
#' values from the corresponding function of the given [`rvar`].
#'
#' @name rvar-functions
#' @export
density.rvar <- function(x, at, ...) {
  if (length(x) != 1) {
    stop2("density() can currently only be used on scalar rvars")
  }

  d <- density(draws_of(x), cut = 0, ...)
  f <- approxfun(d$x, d$y, yleft = 0, yright = 0)
  f(at)
}

#' @importFrom distributional cdf
#' @export
distributional::cdf

#' @rdname rvar-functions
#' @export
cdf.rvar <- function(x, q, ...) {
  if (length(x) != 1) {
    stop2("cdf() can currently only be used on scalar rvars")
  }

  ecdf(draws_of(x))(q)
}

#' @rdname rvar-functions
#' @export
quantile.rvar <- function(x, probs, ...) {
  quantile(draws_of(x), probs, ...)
}


# concatenation -----------------------------------------------------------

#' @export
c.rvar <- function(...) {
  combine_rvar(c, list(...))
}

#' @export
rbind.rvar <- function(...) {
  # not sure why deparse.level is not passed here correctly...
  deparse.level <- rlang::caller_env()$deparse.level %||% 1
  bind_rvar("rbind", list(...), as.list(substitute(list(...))[-1]), deparse.level)
}

#' @export
cbind.rvar <- function(...) {
  # not sure why deparse.level is not passed here correctly...
  deparse.level <- rlang::caller_env()$deparse.level %||% 1
  bind_rvar("cbind", list(...), as.list(substitute(list(...))[-1]), deparse.level, .axis = 3)
}

#' @importFrom utils getS3method
bind_rvar <- function(.f_name, args, arg_exprs, deparse.level = 1, .axis = 2) {
  args = deparse_names(args, arg_exprs, deparse.level)

  # ensure first argument has dimensions (for binding)
  if (is.null(dim(args[[1]]))) {
    dim(args[[1]]) <- c(length(args[[1]]), 1)
  }
  if (length(args) == 1) {
    return(args[[1]])
  }

  if (is.data.frame(args[[2]])) {
    # for data frames, need to deparse arg names at level 2
    args = deparse_names(args, arg_exprs, deparse.level = 2)
    return(do.call(getS3method(.f_name, "data.frame"), args))
  }

  args[[2]] <- as_rvar(args[[2]])
  if (is.null(dim(args[[2]]))) {
    dim(args[[2]]) <- c(length(args[[2]]), 1)
  }

  combine_rvar(get(.f_name), args, .axis = .axis)
}

#' @importFrom rlang as_name as_label
deparse_names <- function(args, arg_exprs, deparse.level) {
  # give arguments names if needed
  if (deparse.level > 0) {
    if (is.null(names(args))) {
      names(args) <- rep("", length(args))
    }
    for (i in seq_along(arg_exprs)) {
      arg_name <- names(args)[[i]]
      arg_expr <- arg_exprs[[i]]
      if (!isTRUE(nchar(arg_name) > 0)) {
        if (deparse.level == 1 && is.name(arg_expr)) {
          names(args)[[i]] <- as_name(arg_expr)
        } else if (deparse.level > 1) {
          names(args)[[i]] <- as_label(arg_expr)
        }
      }
    }
    if (all(names(args) == "")) {
      names(args) = NULL
    }
  }
  args
}

combine_rvar <- function(.f, args, .axis = 2) {
  if (length(args) == 1) {
    return(args[[1]])
  }

  x <- args[[1]]
  y <- as_rvar(args[[2]])

  # conform nchains
  # (don't need to do draws here since that's part of the broadcast below)
  c(x, y) %<-% conform_rvar_nchains(list(x, y))

  # broadcast each array to the desired dimensions
  # (except along the axis we are binding along)
  draws_x <- draws_of(x)
  draws_y <- draws_of(y)
  new_dim <- dim2_common(dim(draws_x), dim(draws_y))

  .broadcast = function(draws, arg_name) {
    new_dim[.axis] <- dim(draws)[.axis]
    draws <- broadcast_array(draws, new_dim)
    if (length(arg_name) && nchar(arg_name) > 0 && is.na(new_dim[.axis])) {
      # cbind/rbind were called with a name for this var and draws being a vector
      # (which will result in new_dim[.axis] being NA), which implies we can
      # replace the existing dimension name with the provided name
      # as in something like cbind(a = x, ...) where x is a vector-like rvar
      dimnames(draws)[[.axis]] = arg_name
    }
    draws
  }
  draws_x <- .broadcast(draws_x, names(args)[[1]])
  draws_y <- .broadcast(draws_y, names(args)[[2]])

  # bind along desired axis
  result <- new_rvar(abind(draws_x, draws_y, along = .axis), .nchains = nchains(x))

  if (length(args) > 2) {
    args[[1]] <- result
    args[[2]] <- NULL
    do.call(.f, args)
  } else {
    result
  }
}


# helpers: validation -----------------------------------------------------------------

# Check the passed yank index (for x[[...]]) is valid
check_rvar_yank_index = function(x, i, ...) {
  index <- dots_list(i, ..., .preserve_empty = TRUE, .ignore_empty = "none")

  if (any(lengths(index)) > 1) {
    stop2("Cannot select more than one element per index with `[[` in an rvar.")
  } else if (any(sapply(index, function(x) is_missing(x) || is.na(x)))) {
    stop2("Missing indices not allowed with `[[` in an rvar.")
  } else if (any(sapply(index, is.logical))) {
    stop2("Logical indices not allowed with `[[` in an rvar.")
  } else if (any(sapply(index, function(x) x < 0))) {
    stop2("subscript out of bounds")
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

# find common ndraws for the ndraws of two rvars to be broadcast to
ndraws2_common <- function(ndraws_x, ndraws_y) {
  if (ndraws_x == 1) {
    ndraws_y
  } else if (ndraws_y == 1) {
    ndraws_x
  } else if (ndraws_x == ndraws_y) {
    ndraws_x
  } else {
    stop2(
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
    warning2(
      "Random variables do not have the same number of chains (", nchains_x, " and ", nchains_y, "),\n",
      "so chains were dropped.\n",
      "Use merge_chains() to collapse chains before combining rvars with\n",
      "a different number of chains to avoid this warning."
    )
    1L
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
  } else if (y_dim == 0) {
    # y_dim == 0 => assigning to an empty vector, can leave it as is
    # TODO: this is a hack for assignment to empty vectors and needs to be fixed
  } else {
    stop2("Cannot assign an rvar with dimension ", paste0(x_dim, collapse = ","),
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

broadcast_array  <- function(x, dim) {
  current_dim = dim(x)

  if (length(current_dim) < length(dim)) {
    # add dimensions of size 1 as necessary so we can broadcast those
    current_dim[seq(length(current_dim) + 1, length(dim))] = 1
    dim(x) = current_dim
  } else if (length(current_dim) > length(dim)) {
    stop2(
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
    stop2(
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


# helpers: applying functions over rvars ----------------------------------

# apply a summary function within each draw of the rvar (dropping other dimensions)
summarise_rvar_within_draws <- function(x, .f, ...) {
  draws <- draws_of(x)
  dim <- dim(draws)
  new_rvar(apply(draws, 1, .f, ...), .nchains = nchains(x))
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
