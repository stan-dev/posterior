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
#' @importFrom vctrs field new_vctr
new_rvar = function(x = double()) {
  # TODO: decide on supported types and cast to them in here
  if (is.null(x)) {
    x = double()
  }
  x = as.array(x)
  .dim = dim(x)

  if (length(x) == 0) {
    if (is.null(.dim)) {
      dim(x) = c(0, 0)
    } else {
      dim(x) = c(.dim, 0)
    }
  }
  else if (is.null(dim(x)) || length(dim(x)) == 1) {
    # 1d vectors get treated as a single variable
    dim(x) = c(1, length(x))
  }

  new_vctr(list(x), class = "rvar")
}

#' @rdname rvar
#' @export
rvar = function(x = double()) {
  new_rvar(x)
}

#' @export
is_rvar = function(x) {
  inherits(x, "rvar")
}


# length and dimensions ---------------------------------------------------

#' @export
length.rvar = function(x) {
  .draws = field(x, 1)

  if (is.null(.draws)) {
    0
  } else {
    dim(.draws)[[1]]
  }
}


#' @export
dim.rvar = function(x) {
  .draws = field(x, 1)
  .dim = dim(.draws)
  ndim = length(.dim)

  if (ndim == 2) {
    # just a vector
    NULL
  } else {
    # everything except the draws dimension
    .dim[-ndim]
  }
}

#' @export
`dim<-.rvar` = function(x, value) {
  .ndraws = ndraws(x)

  dim(x$draws) = c(value, .ndraws)
  x
}

# #' @export
# dimnames.rvar = function(x) {
#   .dimnames = dimnames(x$draws)
#   .dimnames[-length(.dimnames)]
# }

# #' @export
# `dimnames<-.rvar` = function(x, value) {
#   dimnames(x$draws) = value
#   x
# }

#' @export
is.matrix.rvar = function(x) {
  .draws = field(x, 1)

  length(dim(.draws)) == 3
}

# #' @export
# names.rvar = function(x) {
#   NULL
# }


# indexing ----------------------------------------------------------------

# #' @export
# `[[.rvar` = function(x, i) {
#   args = c(list(x$draws, i), replicate(length(dim(x$draws)) - 1, missing_arg()), list(drop = FALSE))
#   new_rvar(do.call(`[`, args))
# }

# #' @export
# #' @importFrom rlang missing_arg
# #' @importFrom rlang enquos eval_bare
# `[.rvar` = function(x, ..., drop = FALSE) {
#   draws = field(x, 1)
#   args = enexprs(...)
#
#   if (length(args) == length(dim(draws))) {
#     # can't index into the draws dimension
#     args[[length(dim(draws))]] = NULL
#   }
#   eval_bare(expr(draws[!!!args, drop = drop]))
#
#   # args = c(list(x = draws), args, list(drop = drop))
#   # print(str(args))
#   # new_rvar(do.call(`[`, args))
#   # n_args = length(substitute(list(...))[-1])
#   # if (n_args == length(dim(x$draws))) {
#   #   new_rvar(x$draws[..., drop = drop])
#   # } else {
#   #   new_rvar(x$draws[..., , drop = drop])
#   # }
#
# }



# manipulating raw draws array --------------------------------------------

#' @export
draws_of = function(x) {
  field(x, 1)
}

#' @export
`draws_of<-` = function(x, value) {
  field(x, 1) = value
}

# vctrs stuff -------------------------------------------------------------

#' @importFrom vctrs vec_proxy
#' @importFrom rray rray_split
#' @export
vec_proxy.rvar = function(x, ...) {
  .draws = field(x, 1)

  if (is.null(.draws)) {
    list()
  } else {
    # decompose into a list of lists by the first index
    rray_split(.draws, 1)
  }
}

# vec_proxy.rvar = function(x, ...) {
#   # decompose into a list of arrays by the first index
#
#   extra_args = c(
#     list(x$draws, NA),
#     replicate(length(dim(x$draws)) - 1, missing_arg()),
#     list(drop = FALSE)
#   )
#   lapply(seq_len(nrow(x$draws)), function(i) {
#     extra_args[[2]] = i
#     do.call(`[`, extra_args)
#   })
# }


#' @importFrom vctrs vec_restore
#' @importFrom rray rray_rbind
#' @export
vec_restore.rvar = function(x, ...) {
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
    x[sapply(x, is.null)] = list(array(NA, dim = c(1,1)))

  }
  x_array = do.call(rray_rbind, x)
  new_rvar(x_array)
}



# concatenation -----------------------------------------------------------

# #' @export
# #' @importFrom rray rray_bind
# c.rvar = function(...) {
#   args = list(...)
#   if (length(args) == 1) {
#     return(args[[1]])
#   }
#
#   draws1 = args[[1]]$draws
#   if (is_rvar(args[[2]])) {
#     draws2 = args[[2]]$draws
#     result = new_rvar(rray_bind(draws1, draws2, .axis = 1))
#   } else {
#     result = new_rvar(rray_bind(draws1, args[[2]], .axis = 1))
#   }
#
#   if (length(args) > 2) {
#     args[[1]] = result
#     args[[2]] = NULL
#     do.call(c, args)
#   } else {
#     result
#   }
# }



# chain / iteration / draw info -------------------------------------------

ndraws.rvar = function(x) {
  .draws = field(x, 1)
  .dim = dim(.draws)
  .dim[length(.dim)]
}




# helpers -----------------------------------------------------------------

# convert into a list of draws for applying a function draw-wise
list_of_draws = function(x) {
  .draws = draws_of(x)

  lapply(apply(.draws, length(dim(.draws)), list), `[[`, 1)
}


