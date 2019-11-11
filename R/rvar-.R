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
  if (is.null(x)) {
    x <- double()
  }
  x <- as.array(x)
  .dim <- dim(x)

  if (length(x) == 0) {
    if (is.null(.dim)) {
      dim(x) <- c(0, 0)
    } else {
      dim(x) <- c(.dim, 0)
    }
  }
  else if (is.null(dim(x)) || length(dim(x)) == 1) {
    # 1d vectors get treated as a single variable
    dim(x) <- c(1, length(x))
  }

  # setting the S4 flag on the object allows us to dispatch matrix
  # multiplication correctly --- %*% does its own dispatching where
  # it will not dispatch to S3 objects even if they have an S4 class
  # defined through setOldClass, they *must* also have isS4() return
  # TRUE, hence the need to set that flag here.
  asS4(new_vctr(list(), draws = x, class = "rvar"))
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

#' @export
is.matrix.rvar <- function(x) {
  length(dim(draws_of(x))) == 3
}


# indexing ----------------------------------------------------------------

# #' @export
# `[[.rvar` <- function(x, i) {
#   args <- c(list(x$draws, i), replicate(length(dim(x$draws)) - 1, missing_arg()), list(drop = FALSE))
#   new_rvar(do.call(`[`, args))
# }

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

#' @export
draws_of <- function(x) {
  attr(x, "draws")
}

#' @export
`draws_of<-` <- function(x, value) {
  attr(x, "draws") <- value
  x
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

# vec_proxy.rvar <- function(x, ...) {
#   # decompose into a list of arrays by the first index
#
#   extra_args <- c(
#     list(x$draws, NA),
#     replicate(length(dim(x$draws)) - 1, missing_arg()),
#     list(drop = FALSE)
#   )
#   lapply(seq_len(nrow(x$draws)), function(i) {
#     extra_args[[2]] <- i
#     do.call(`[`, extra_args)
#   })
# }


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

# #' @export
# #' @importFrom rray rray_bind
# c.rvar <- function(...) {
#   args <- list(...)
#   if (length(args) == 1) {
#     return(args[[1]])
#   }
#
#   draws1 <- args[[1]]$draws
#   if (is_rvar(args[[2]])) {
#     draws2 <- args[[2]]$draws
#     result <- new_rvar(rray_bind(draws1, draws2, .axis = 1))
#   } else {
#     result <- new_rvar(rray_bind(draws1, args[[2]], .axis = 1))
#   }
#
#   if (length(args) > 2) {
#     args[[1]] <- result
#     args[[2]] <- NULL
#     do.call(c, args)
#   } else {
#     result
#   }
# }


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

# Check that two rvars have a compatible number of draws and
# return an appropriate number of draws that both objects could be broadcasted
# to, or throw an error if there is no such number of draws.
check_ndraws2 <- function(x, y) {
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
