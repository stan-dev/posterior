#' Random variables
#'
#' Random variables backed by arrays
#'
#' @name rvar_array
#'
#' @param x A vector or array where the last dimension is draws from a distribution
#'
#' @details The `"rvar_array"` class represents random variables as arrays.
#'
#' @return An object of class `"rvar_array"` representing a random variable.
#'
NULL

#' @rdname rvar_array
#' @export
rvar_array = function(x) {
  if (!is.null(x)) {
    x = as.array(x)

    if (is.null(dim(x)) || length(dim(x)) == 1) {
      dim(x) = c(1, length(x))
    }
  }

  structure(list(
    draws = x
  ),
    class = "rvar_array"
  )
}

#' @export
is_rvar_array = function(x) {
  inherits(x, "rvar_array")
}


# length and dimensions ---------------------------------------------------

#' @export
length.rvar_array = function(x) {
  if (is.null(x$draws)) {
    0
  } else {
    .dim = dim(x$draws)
    length(x$draws) / .dim[length(.dim)]
  }
}

#' @export
dim.rvar_array = function(x) {
  .dim = dim(x$draws)
  ndim = length(.dim)
  if (ndim == 2) {
    # just a vector
    NULL
  } else {
    .dim[-ndim]
  }
}

#' @export
`dim<-.rvar_array` = function(x, value) {
  .ndraws = ndraws(x)
  dim(x$draws) = c(value, .ndraws)
  x
}

#' @export
dimnames.rvar_array = function(x) {
  .dimnames = dimnames(x$draws)
  .dimnames[-length(.dimnames)]
}

#' @export
`dimnames<-.rvar_array` = function(x, value) {
  dimnames(x$draws) = value
  x
}

#' @export
is.matrix.rvar_array = function(x) {
  length(dim(x$draws)) == 3
}

#' @export
names.rvar_array = function(x) {
  NULL
}


# indexing ----------------------------------------------------------------

#' @export
`[[.rvar_array` = function(x, i) {
  args = c(list(x$draws, i), replicate(length(dim(x$draws)) - 1, missing_arg()), list(drop = FALSE))
  rvar_array(do.call(`[`, args))
}

#' @export
`[.rvar_array` = function(x, ..., drop = FALSE) {
  n_args = length(substitute(list(...))[-1])
  if (n_args == length(dim(x$draws))) {
    rvar_array(x$draws[..., drop = FALSE])
  } else {
    rvar_array(x$draws[..., , drop = FALSE])
  }

}


# vctrs stuff -------------------------------------------------------------

#' @importFrom vctrs vec_proxy
#' @export
vec_proxy.rvar_array = function(x, ...) {
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
  if (is.null(x$draws)) {
    list()
  } else {
    apply(x$draws, 1, list)
  }
}

# vec_proxy.rvar_array = function(x, ...) {
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
vec_restore.rvar_array = function(x, ...) {
  # x_array = do.call(rbind, x)
  x_array = do.call(rbind, lapply(x, `[[`, 1))
  rvar_array(x_array)
}



# concatenation -----------------------------------------------------------

#' @export
#' @importFrom rray rray_bind
c.rvar_array = function(...) {
  args = list(...)
  if (length(args) == 1) {
    return(args[[1]])
  }

  draws1 = args[[1]]$draws
  if (is_rvar_array(args[[2]])) {
    draws2 = args[[2]]$draws
    result = rvar_array(rray_bind(draws1, draws2, .axis = 1))
  } else {
    result = rvar_array(rray_bind(draws1, args[[2]], .axis = 1))
  }

  if (length(args) > 2) {
    args[[1]] = result
    args[[2]] = NULL
    do.call(c, args)
  } else {
    result
  }
}



# chain / iteration / draw info -------------------------------------------

ndraws.rvar_array = function(x) {
  .dim = dim(x$draws)
  .dim[length(.dim)]
}

