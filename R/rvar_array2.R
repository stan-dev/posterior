#' Random variables
#'
#' Random variables backed by arrays
#'
#' @name rvar_array2
#'
#' @param x A vector or array where the last dimension is draws from a distribution
#'
#' @details The `"rvar_array2"` class represents random variables as arrays.
#'
#' @return An object of class `"rvar_array2"` representing a random variable.
#'
NULL

#' @rdname rvar_array2
#' @export
#' @importFrom vctrs field
rvar_array2 = function(x) {
  if (!is.null(x)) {
    x = as.array(x)

    if (is.null(dim(x)) || length(dim(x)) == 1) {
      dim(x) = c(1, length(x))
    }
  }

  structure(list(
    x
  ),
    class = c("rvar_array2", "vctrs_vctr")
  )
}

#' @export
is_rvar_array2 = function(x) {
  inherits(x, "rvar_array2")
}


# length and dimensions ---------------------------------------------------

#' @export
length.rvar_array2 = function(x) {
  draws = field(x, 1)
  if (is.null(draws)) {
    0
  } else {
    .dim = dim(draws)
    length(draws) / .dim[length(.dim)]
  }
}


# #' @export
# dim.rvar_array2 = function(x) {
#   .dim = dim(x$draws)
#   ndim = length(.dim)
#   if (ndim == 2) {
#     # just a vector
#     NULL
#   } else {
#     .dim[-ndim]
#   }
# }

# #' @export
# `dim<-.rvar_array2` = function(x, value) {
#   .ndraws = ndraws(x)
#   dim(x$draws) = c(value, .ndraws)
#   x
# }

# #' @export
# dimnames.rvar_array2 = function(x) {
#   .dimnames = dimnames(x$draws)
#   .dimnames[-length(.dimnames)]
# }

# #' @export
# `dimnames<-.rvar_array2` = function(x, value) {
#   dimnames(x$draws) = value
#   x
# }

# #' @export
# is.matrix.rvar_array2 = function(x) {
#   length(dim(x$draws)) == 3
# }

# #' @export
# names.rvar_array2 = function(x) {
#   NULL
# }


# indexing ----------------------------------------------------------------

# #' @export
# `[[.rvar_array2` = function(x, i) {
#   args = c(list(x$draws, i), replicate(length(dim(x$draws)) - 1, missing_arg()), list(drop = FALSE))
#   rvar_array2(do.call(`[`, args))
# }

# #' @export
# `[.rvar_array2` = function(x, ..., drop = FALSE) {
#   n_args = length(substitute(list(...))[-1])
#   if (n_args == length(dim(x$draws))) {
#     rvar_array2(x$draws[..., drop = FALSE])
#   } else {
#     rvar_array2(x$draws[..., , drop = FALSE])
#   }
#
# }


# vctrs stuff -------------------------------------------------------------

#' @importFrom vctrs vec_proxy
#' @export
vec_proxy.rvar_array2 = function(x, ...) {
  # decompose into a list of lists by the first index
  # extra_args = c(
  #   list(x$draws, NA),
  #   replicate(length(dim(x$draws)) - 1, missing_arg()),
  #   list(drop = FALSE)
  # )
  # lapply(seq_len(nrow(x$draws)), function(i) {
  #   extra_args[[2]] = i
  #   list(do.call(`[`, extra_args))
  # })
  draws = field(x, 1)
  if (is.null(draws)) {
    list()
  } else {
    apply(draws, 1, list)
  }
}

# vec_proxy.rvar_array2 = function(x, ...) {
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
#' @export
vec_restore.rvar_array2 = function(x, ...) {
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
    x[sapply(x, is.null)] = NA
  }
  x_array = do.call(rbind, lapply(x, `[[`, 1))
  rvar_array2(x_array)
}



# concatenation -----------------------------------------------------------

# #' @export
# #' @importFrom rray rray_bind
# c.rvar_array2 = function(...) {
#   args = list(...)
#   if (length(args) == 1) {
#     return(args[[1]])
#   }
#
#   draws1 = args[[1]]$draws
#   if (is_rvar_array2(args[[2]])) {
#     draws2 = args[[2]]$draws
#     result = rvar_array2(rray_bind(draws1, draws2, .axis = 1))
#   } else {
#     result = rvar_array2(rray_bind(draws1, args[[2]], .axis = 1))
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

# ndraws.rvar_array2 = function(x) {
#   .dim = dim(x$draws)
#   .dim[length(.dim)]
# }

#' @export
new_a2 <- function(x) {
  new_vctr(list(x), class = "a2")
}

#' @export
vec_proxy.a2 = function(x) {
  as.list(field(x, 1))
}

#' @export
vec_restore.a2 = function(x, ...) {
  new_a2(unlist(x))
}

#' @export
length.a2 = function(x, ...) {
  length(field(x, 1))
}


# a1 ----------------------------------------------------------------------


#' @export
new_a1 <- function(x) {
  new_vctr(list(x), class = "a1")
}

#' @export
vec_proxy.a1 = function(x) {
  proxy = field(x, 1)
  attr(proxy, "foo") = "bar"
  proxy
}

#' @export
vec_restore.a1 = function(x, ...) {
  print(str(x))
  print(list(...))
  new_a1(x)
}

#' @export
length.a1 = function(x, ...) {
  length(field(x, 1))
}
