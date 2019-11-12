#' Random variables of arbitrary dimension
#'
#' Random variables backed by arrays of arbitrary dimension
#'
#' @name rvar
#'
#' @param x A vector or array where the last dimension is draws from a distribution
#'
#' @details The `"rvar"` class represents random variables as arrays of arbitrary
#' dimension, where the last dimension is used to index draws from the distribution.
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
  .dim <- dim(x)

  if (length(x) == 0) {
    if (is.null(.dim)) {
      dim(x) <- c(0, 0)
      dim_1_length <- 0
    } else {
      dim(x) <- c(.dim, 0)
      dim_1_length <- .dim[[1]]
    }
  }
  else if (is.null(dim(x)) || length(dim(x)) == 1) {
    # 1d vectors get treated as a single variable
    dim(x) <- c(1, length(x))
    dim_1_length <- 1
  } else {
    dim_1_length <- dim(x)[[1]]
  }

  # setting the S4 flag on the object allows us to dispatch matrix
  # multiplication correctly --- %*% does its own dispatching where
  # it will not dispatch to S3 objects even if they have an S4 class
  # defined through setOldClass, they *must* also have isS4() return
  # TRUE, hence the need to set that flag here.

  # we also wrap a dummy list with same length as the first
  # dimension so that code that relies on the internal length of
  # this vector (such as dplyr::mutate()) behaves correctly.
  asS4(new_vctr(vector("list", dim_1_length), draws = x, class = "rvar"))
}

#' @rdname rvar
#' @export
rvar <- function(x = double()) {
  new_rvar(x)
}

#' @importFrom methods setOldClass
setOldClass(c("rvar", "vctrs_vctr"))

#' @rdname rvar
#' @export
is_rvar <- function(x) {
  inherits(x, "rvar")
}


# length and dimensions ---------------------------------------------------

#' @export
length.rvar <- function(x) {
  .draws <- draws_of(x)

  if (is.null(.draws)) {
    0
  } else {
    dim(.draws)[[1]]
  }
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
levels.rvar <- function(x) {
  NULL
}

# indexing ----------------------------------------------------------------

#' @importFrom rray rray_slice
#' @export
`[[.rvar` <- function(x, i, ...) {
  check_rvar_yank_index(x, i, ...)

  new_rvar(rray_slice(draws_of(x), i, 1))
}

#' @importFrom rray rray_slice<-
#' @export
`[[<-.rvar` <- function(x, i, ..., value) {
  value <- vec_cast(value, x)
  check_rvar_ndraws_first(value, x)
  value <- check_rvar_dims_first(value, x[[i, ...]])

  rray_slice(draws_of(x), i, 1) <- draws_of(value)
  update_rvar_length(x)
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
      index[[i]] <- eval_tidy(index[[i]])

      if (is.numeric(index[[i]])) {
        # numeric indices outside the range of the corresponding dimension
        # should create NAs; but array indexing doesn't do this (it throws
        # an error), so we adjust the indices to do so.
        index[[i]][index[[i]] > .dim[[i]]] <- NA_integer_
      }
    }
  }

  # fill in final indices with missing arguments
  index[seq(length(index) + 1, length(dim(.draws)))] = list(missing_arg())

  x = eval_tidy(expr(new_rvar(.draws[!!!index])))

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
  if (is.null(dim(x)) && any(i > length(x), na.rm = TRUE)) {
    # unidimensional indexing allows array extension; extend the array
    # before we do the assignment
    x <- x[seq_len(max(i, na.rm = TRUE))]
  }

  value <- vec_cast(value, x)
  check_rvar_ndraws_first(value, x)
  value <- check_rvar_dims_first(value, x[i, ...])

  rray_subset(draws_of(x), i, ...) <- draws_of(value)
  update_rvar_length(x)
}

# #' @export
# #' @importFrom rlang missing_arg
# #' @importFrom rlang enquos eval_bare
# `[.rvar` <- function(x, ..., drop <- FALSE) {
#   draws <- field(x, 1)
#   args <- enexprs(...)
#
#   if (length(args) == length(dim(draws))) {
#     # can't index into the draws dimension
#     args[[length(dim(draws))]] <- NULL
#   }
#   eval_bare(expr(draws[!!!args, drop = drop]))
#
#   # args <- c(list(x = draws), args, list(drop = drop))
#   # print(str(args))
#   # new_rvar(do.call(`[`, args))
#   # n_args <- length(substitute(list(...))[-1])
#   # if (n_args == length(dim(x$draws))) {
#   #   new_rvar(x$draws[..., drop = drop])
#   # } else {
#   #   new_rvar(x$draws[..., , drop = drop])
#   # }
#
# }


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
  # workaround to ensure internal length of wrapped vector matches correct length
  attr(x, "draws") <- value
  update_rvar_length(x)
}


# vctrs stuff -------------------------------------------------------------

#' @importFrom vctrs vec_proxy
#' @importFrom rray rray_split
#' @export
vec_proxy.rvar <- function(x, ...) {
  .draws <- draws_of(x)

  if (is.null(.draws)) {
    list()
  } else {
    # decompose into a list of lists by the first index
    rray_split(.draws, 1)
  }
}


#' @importFrom vctrs vec_restore
#' @importFrom rray rray_rbind
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
  x_array <- do.call(rray_rbind, x)
  new_rvar(x_array)
}



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



# setMethod("rbind2", c(x = "rvar", y = "rvar"), function(x, y) {
#   if (is.null(dim(y))) {
#     dim(y) <- c(length(y), 1)
#   }
#   result <- c(x, y)
#   rownames(result) <- NULL #c(rownames(x), rownames(y))
#   result
# })


# chain / iteration / draw info -------------------------------------------

ndraws.rvar <- function(x) {
  .dim <- dim(draws_of(x))
  .dim[length(.dim)]
}


# helpers -----------------------------------------------------------------

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
  x_dim <- dim(x) %||% length(x)
  x_dim_dropped <- as.integer(x_dim[x_dim != 1])
  y_dim <- dim(y) %||% length(y)
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

update_rvar_length = function(x) {
  # TODO: make this faster, save attributes
  new_rvar(draws_of(x))
  # class. = attr(x, "class")
  # x = unclass(x)
  # length(x) = dim(attr(x, "draws"))[[1]]
  # attr(x, "class") = class.
  # x
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
  .dim = dim(x)
  dim(x) <- .dim[.dim != 1]
  x
}
