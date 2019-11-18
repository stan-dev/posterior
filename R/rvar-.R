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
  if (length(x) == 0) {
    x <- double()
  }
  x <- as.array(x)

  # ensure dimnames is set (makes comparison easier for tests)
  if (is.null(dimnames(x))) {
    dimnames(x) <- list(NULL)
  }

  .dim <- dim(x)
  if (length(x) == 0) {
    if (is.null(.dim)) {
      dim(x) <- c(0, 0)
    } else {
      dim(x) <- c(.dim, 0)
    }
  }
  else if (is.null(.dim) || length(.dim) == 1) {
    # 1d vectors get treated as a single variable
    dim(x) <- c(1, length(x))
  }

  # setting the S4 flag on the object allows us to dispatch matrix
  # multiplication correctly --- %*% does its own dispatching where
  # it will not dispatch to S3 objects even if they have an S4 class
  # defined through setOldClass, they *must* also have isS4() return
  # TRUE, hence the need to set that flag here.
  asS4(structure(rep.int(NA, NROW(x)), draws = x, class = c("rvar")))
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

#' @importFrom methods setOldClass
setOldClass(c("rvar"))

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
  prod(diml(x))
}

#' @export
dim.rvar <- function(x) {
  .dim <- dim(draws_of(x))
  ndim <- length(.dim)

  if (ndim == 2) {
    # just a vector
    NULL
  } else {
    # everything except the draws dimension
    .dim[-ndim]
  }
}

#' @export
`dim<-.rvar` <- function(x, value) {
  if (length(value) == 0) {
    # vectors have NULL dim; for us that means
    # dim of c(length(x), ndraws(x))
    value = length(x)
  }
  dim(draws_of(x)) <- c(value, ndraws(x))
  x
}

#' @export
dimnames.rvar <- function(x) {
  .dimnames <- dimnames(draws_of(x))
  .dimnames[-length(.dimnames)]
}

#' @export
`dimnames<-.rvar` <- function(x, value) {
  dimnames(draws_of(x)) <- value
  x
}

#' @export
names.rvar <- function(x) {
  .dimnames <- dimnames(draws_of(x))
  .dimnames[[1]]
}

#' @export
`names<-.rvar` <- function(x, value) {
  dimnames(draws_of(x))[[1]] <- value
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
  NULL
}

#' @export
rep.rvar <- function(x, ..., times = 1, length.out = NA, each = 1) {
  if (each != 1) {
    # TODO: implement
    stop2("rep(each = ) is not yet implemented for `rvar`s.")
  }

  draws = draws_of(x)
  if (is.na(length.out)) {
    # use `times`
    rep_draws = rep(draws, times)
    dim = dim(draws)
    dim[[1]] = dim[[1]] * times
    dim(rep_draws) = dim
    new_rvar(rep_draws)
  } else {
    # use `length.out`
    rep_draws = rep_len(draws, length.out * ndraws(x))
    dim(rep_draws) = c(length(rep_draws) / ndraws(x), ndraws(x))
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
  check_rvar_margin(x, MARGIN)
  draws_of(x) <- unique(draws_of(x), incomparables = incomparables, MARGIN = MARGIN, ...)
  x
}

#' @export
duplicated.rvar <- function(x, incomparables = FALSE, MARGIN = 1, ...) {
  check_rvar_margin(x, MARGIN)
  duplicated(draws_of(x), incomparables = incomparables, MARGIN = MARGIN, ...)
}

#' @export
anyDuplicated.rvar <- function(x, incomparables = FALSE, MARGIN = 1, ...) {
  check_rvar_margin(x, MARGIN)
  anyDuplicated(draws_of(x), incomparables = incomparables, MARGIN = MARGIN, ...)
}

check_rvar_margin <- function(x, MARGIN) {
  if (!(1 <= MARGIN && MARGIN <= length(diml(x)))) {
    stop2("MARGIN = ", MARGIN, " is invalid for dim = ", paste0(diml(x), collapse = ","))
  }
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
  .draws = draws_of(x)
  lapply(apply(.draws, length(dim(.draws)), list), function(x) new_rvar(x[[1]]))
}


# indexing ----------------------------------------------------------------

#' @importFrom rray rray_slice
#' @export
`[[.rvar` <- function(x, i, ...) {
  check_rvar_yank_index(x, i, ...)

  x <- new_rvar(rray_slice(draws_of(x), i, 1))
  dimnames(x) <- NULL
  x
}

#' @importFrom rray rray_slice<-
#' @export
`[[<-.rvar` <- function(x, i, ..., value) {
  value <- vec_cast(value, x)
  check_rvar_ndraws_first(value, x)
  value <- check_rvar_dims_first(value, x[[i, ...]])

  rray_slice(draws_of(x), i, 1) <- draws_of(value)
  x
}

#' @importFrom rray rray_subset
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
          index[[i]][index[[i]] > .dim[[i]]] <- NA_integer_
        }
      }
    }
  }

  # fill in final indices with missing arguments
  index[seq(length(index) + 1, length(dim(.draws)))] = list(missing_arg())

  x = eval_tidy(expr(new_rvar(.draws[!!!index, drop = FALSE])))

  #print(expr(.draws[!!!indices]))

  #x = new_rvar(rray_subset(draws_of(x), ...))

  if (drop) {
    drop_(x)
  } else {
    x
  }
}

#' @importFrom rray rray_subset<-
#' @export
`[<-.rvar` <- function(x, i, ..., value) {
  if (length(diml(x) == 1) && any(i > length(x), na.rm = TRUE)) {
    # unidimensional indexing allows array extension; extend the array
    # before we do the assignment
    x <- x[seq_len(max(i, na.rm = TRUE))]
  }

  value <- vec_cast(value, x)
  check_rvar_ndraws_first(value, x)
  value <- check_rvar_dims_first(value, x[i, ...])

  rray_subset(draws_of(x), i, ...) <- draws_of(value)
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
  # TODO: fix stopgap or at least make attrs stay
  new_rvar(value)
  # attr(x, "draws") <- value
  # x
}


# vctrs stuff -------------------------------------------------------------

# #' @importFrom vctrs vec_proxy
# #' @importFrom rray rray_split
# #' @export
# vec_proxy.rvar <- function(x, ...) {
#   draws_of(x)
# }
#
# vec_restore.rvar <- function(x, ...) {
#   new_rvar(x)
# }

# #' @importFrom vctrs vec_proxy
# #' @importFrom rray rray_split
# #' @export
# vec_proxy.rvar <- function(x, ...) {
#   .draws <- draws_of(x)
#
#   if (is.null(.draws)) {
#     list()
#   } else {
#     # decompose into a list of lists by the first index
#     rray_split(.draws, 1)
#   }
# }
#
#
# #' @importFrom vctrs vec_restore
# #' @importFrom rray rray_rbind
# #' @export
# vec_restore.rvar <- function(x, ...) {
#   if (length(x) > 0) {
#     # need to handle the case of creating NAs from NULL entries so that
#     # vec_init() works properly: vec_init requires vec_slice(x, NA_integer_)
#     # to give you back NA values, but this breaks because we use lists as proxies.
#     # When using a list as a proxy, a proxy entry in `x` that is equal to NULL
#     # actually corresponds to an NA value due to the way that list indexing
#     # works: when you do something like list()[c(NA_integer_,NA_integer_)]
#     # you get back list(NULL, NULL), but when you do something like
#     # double()[c(NA_integer_,NA_integer_)] you get back c(NA, NA).
#     # So we have to make the NULL values be NA values to mimic vector indexing.
#
#     # N.B. could potentially do this with vec_cast as well (as long as the first
#     # dimension is the slicing index)
#     x[sapply(x, is.null)] <- list(array(NA, dim = c(1,1)))
#
#   }
#   x_array <- do.call(rray_rbind, x)
#   new_rvar(x_array)
# }



# concatenation -----------------------------------------------------------

#' @export
#' @importFrom rray rray_bind
c.rvar <- function(...) {
  combine_rvar(c, list(...), .axis = 1)
}

#' @export
#' @importFrom rray rray_bind
rbind.rvar <- function(...) {
  bind_rvar(rbind, list(...), .axis = 1)
}

#' @export
#' @importFrom rray rray_bind
cbind.rvar <- function(...) {
  bind_rvar(cbind, list(...), .axis = 2)
}

bind_rvar <- function(.f, args, .axis = 1) {
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

combine_rvar <- function(.f, args, .axis = 1) {
  if (length(args) == 1) {
    return(args[[1]])
  }

  draws1 <- draws_of(args[[1]])
  draws2 <- draws_of(as_rvar(args[[2]]))
  result <- new_rvar(rray_bind(draws1, draws2, .axis = .axis))

  if (length(args) > 2) {
    args[[1]] <- result
    args[[2]] <- NULL
    do.call(.f, args)
  } else {
    result
  }
}


# chain / iteration / draw info -------------------------------------------

ndraws.rvar <- function(x) {
  .dim <- dim(draws_of(x))
  .dim[length(.dim)]
}


# helpers -----------------------------------------------------------------

# dim or length: nevers returns NULL except in cases where rvar is NULL
# (unlike dim which will return NULL on single-dimensional vector)
diml <- function(x) {
  .dim <- dim(draws_of(x))
  ndim <- length(.dim)
  .dim[-ndim]
}


# convert into a list of draws for applying a function draw-wise
list_of_draws <- function(x) {
  .draws <- draws_of(x)

  lapply(apply(.draws, length(dim(.draws)), list), `[[`, 1)
}


# Check the passed yank index (for x[[...]]) is valid
check_rvar_yank_index = function(x, i, ...) {
  if (length(i) != 1 || length(list(...)) != 0) {
    stop2("You can only select one element with `[[` on rvar objects.")
  }
  if (is.logical(i)) {
    stop2("logical indices are not supported with `[[` on rvar objects.")
  }
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
# with the second.
check_rvar_ndraws_first <- function(x, y) {
  ndraws_x <- ndraws(x)
  ndraws_y <- ndraws(y)

  if (ndraws_x == 1 || ndraws_x == ndraws_y) {
    ndraws_y
  } else {
    stop(
      "Random variables have different number of draws (", ndraws_x,
      " and ", ndraws_y, ") can cannot be used together."
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
      " and ", ndraws_y, ") can cannot be used together."
    )
  }
}

# Check that the first rvar can be conformed to the dimensions of the second,
# ignoring 1s
check_rvar_dims_first <- function(x, y) {
  x_dim <- diml(x)
  x_dim_dropped <- as.integer(x_dim[x_dim != 1])
  y_dim <- diml(y)
  y_dim_dropped <- as.integer(y_dim[y_dim != 1])

  if (length(x_dim_dropped) == 0) {
    # x can be treated as scalar, do so
    dim(x) <- rep(1, length(dim(y)))
  } else if (identical(x_dim_dropped, y_dim_dropped)) {
    dim(x) <- dim(y)
  } else {
    stop2("Cannot assign an rvar with dimension ", paste0(x_dim, collapse = ","),
      " to an rvar with dimension ", paste0(y_dim, collapse = ","))
  }

  x
}

# broadcast the draws dimension of an rvar to the requested size
broadcast_draws <- function(x, .ndraws) {
  if (.ndraws == ndraws(x)) {
    x
  } else {
    draws <- draws_of(x)
    new_dim <- dim(draws)
    new_dim[length(new_dim)] <- .ndraws
    new_rvar(rray::rray_broadcast(draws, new_dim))
  }
}

drop_ <- function(x) {
  .diml <- diml(x)

  if (!isTRUE(all.equal(.diml, 1))) {
    # with exactly 1 element left we don't want to drop anything
    # (otherwise names get lost), so only do this with > 1 element
    .dimnames <- dimnames(x)
    dim(x) <- .diml[.diml != 1]
    dimnames(x) <- .dimnames[.diml != 1]
  }

  x
}

# apply a summary function within each draw of the rvar (dropping other dimensions)
summarise_rvar_within_draws <- function(x, .f, ...) {
  draws <- draws_of(x)
  dim <- dim(draws)
  new_rvar(apply(draws, length(dim), .f, ...))
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
  apply(draws, seq_len(length(dim) - 1), .f, ...)
}
