# Summary operations ---------------------------------------------------------

#' @export
Summary.rvar <- function(..., na.rm = FALSE) {
  args <- lapply(list(...), function(arg) {
    arg <- as_rvar(arg)
    dim(arg) <- prod(diml(arg))
    arg
  })

  # bind all args into a single matrix of draws to perform the summary over
  all_draws <- draws_of(do.call(c, args))

  f <- get(.Generic)
  new_rvar(apply(all_draws, 2, f))
}

#' @export
mean.rvar <- function(x, ...) summarise_rvar_within_draws(x, mean, ...)
#' @export
median.rvar <- function(x, ...) summarise_rvar_within_draws(x, median, ...)
#' @export
anyNA.rvar <- function(x, ...) summarise_rvar_within_draws(x, anyNA, ...)

#' @export
is.finite.rvar <- function(x, ...) rvar_apply_vec_fun(is.finite, x, ...)
#' @export
is.infinite.rvar <- function(x, ...) rvar_apply_vec_fun(is.infinite, x, ...)
#' @export
is.nan.rvar <- function(x, ...) rvar_apply_vec_fun(is.nan, x, ...)
#' @export
is.na.rvar <- function(x, ...) summarise_rvar_by_element(x, function(x) any(is.na(x)))


# Expectations and summaries of random variables --------------------------

E <- function(x, na.rm = FALSE) {
  summarise_rvar_by_element(x, mean, na.rm = na.rm)
}

Pr <- function(x, na.rm = FALSE) {
  if (!all(is.logical(draws_of(x)))) {
    stop2("Can only use `Pr(...)` on logical random variables.")
  }
  summarise_rvar_by_element(x, mean, na.rm = na.rm)
}


# Ops: math operators ---------------------------------------------------

#' @export
Ops.rvar <- function(e1, e2) {
  e1 <- as_rvar(e1)
  f <- get(.Generic)

  if (missing(e2)) {
    # unary operators
    return(rvar_apply_vec_fun(f, e1))
  }

  draws_x <- draws_of(as_rvar(e1))
  draws_y <- draws_of(as_rvar(e2))

  # broadcast draws to common dimension
  # TODO: skip broadcast for scalars
  new_dim <- dim2_common(dim(draws_x), dim(draws_y))
  draws_x <- broadcast_array(draws_x, new_dim)
  draws_y <- broadcast_array(draws_y, new_dim)

  new_rvar(f(draws_x, draws_y))
}

#' @export
Math.rvar <- function(x, ...) {
  f <- get(.Generic)

  new_rvar(f(draws_of(x)))
}


# matrix multiplication ---------------------------------------------------

#' Matrix multiplication of random variables
#'
#' Efficient matrix multiplication of random variables.
#'
#' @name rvar-matmult
#' @aliases %*%
#' @param x An [rvar], [numeric], or [logical]. Must be 1 or 2-dimensional. If it is a vector,
#' it is treated as a row vector.
#' @param y An [rvar], [numeric], or [logical]. Must be 1 or 2-dimensional. If it is a vector,
#' it is treated as a column vector.
#'
#' @details
#' If `x` or `y` are vectors, they are converted into matrices prior to multiplication, with `x`
#' converted to a row vector and `y` to a column vector. Numerics and logicals can be multiplied
#' by [rvar]s and are broadcasted across all draws of the [rvar] argument. Tensor multiplication
#' is used to efficiently multiply matrices across draws, so if either `x` or `y` is an [rvar],
#' `x %*% y` will be much faster than `rdo(x %*% y)`.
#'
#' @return An [rvar] representing the matrix product of `x` and `y`.
#'
#' @importFrom tensorA mul.tensor as.tensor
#' @method %*% rvar
#' @export
`%*%.rvar` <- function(x, y) {
  # TODO: get someone else to double-check this
  # Fast version of rdo(x %*% y)

  # ensure everything is a matrix by adding dimensions as necessary to make `x`
  # a row vector and `y` a column vector
  ndiml_x <- length(diml(x))
  if (ndiml_x == 1) {
    dim(x) <- c(1, diml(x))
  } else if (ndiml_x != 2) {
    stop("First argument (`x`) is not a vector or matrix, cannot matrix-multiply")
  }

  ndiml_y <- length(diml(y))
  if (ndiml_y == 1) {
    dim(y) <- c(diml(y), 1)
  } else if (ndiml_y != 2) {
    stop("Second argument (`y`) is not a vector or matrix, cannot matrix-multiply")
  }

  # convert both objects into rvars if they aren't already (this will give us
  # a 3d draws array for each variable)
  x <- as_rvar(x)
  y <- as_rvar(y)

  # conform the draws dimension in both variables
  .ndraws <- check_rvar_ndraws_both(x, y)
  x <- broadcast_draws(x, .ndraws)
  y <- broadcast_draws(y, .ndraws)

  # do a tensor multiplication equivalent of the requested matrix multiplication
  result <- mul.tensor(as.tensor(draws_of(x)), 3, as.tensor(draws_of(y)), 2, by = 1)

  # restore names (as.tensor adds dummy names to dimensions)
  names(result) <- names(dimnames(draws_of(x)))

  # move draws dimension back to the front
  result <- aperm(result, c(3,1,2))

  new_rvar(unclass(result))
}

#' @rdname rvar-matmult
#' @export
setMethod("%*%", c(x = "rvar", y = "rvar"), `%*%.rvar`)
#' @rdname rvar-matmult
#' @export
setMethod("%*%", c(x = "rvar"), `%*%.rvar`)
#' @rdname rvar-matmult
#' @export
setMethod("%*%", c(y = "rvar"), `%*%.rvar`)



# transpose and permutation -----------------------------------------------

#' @export
t.rvar = function(x) {
  .draws = draws_of(x)
  ndim = length(dim(.draws))

  if (length(x) != 0 && ndim == 2) {
    # vector
    dim(.draws) = c(dim(.draws)[1], 1, dim(.draws)[2])
    new_rvar(.draws)
  } else if (ndim == 3) {
    .draws <- aperm(.draws, c(1, 3, 2))
    new_rvar(.draws)
  } else {
    stop("argument is not a random vector or matrix")
  }
}
