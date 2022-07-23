# Ops: math operators ---------------------------------------------------

#' @export
Ops.rvar <- function(e1, e2) {
  e1 <- as_rvar(e1)
  f <- get(.Generic)

  if (missing(e2)) {
    # unary operators
    return(rvar_apply_vec_fun(f, e1))
  }

  c(e1, e2) %<-% conform_rvar_nchains(list(as_rvar(e1), as_rvar(e2)))
  draws_x <- draws_of(e1)
  draws_y <- draws_of(e2)

  # broadcast draws to common dimension
  new_dim <- dim2_common(dim(draws_x), dim(draws_y))
  # Most of the time we don't broadcast scalars (constant rvars of length 1).
  # With broadcast_scalars = FALSE broadcast_array will return a vector (no dims)
  # version of the input, which works unless *both* x and y are constants
  # (because then the correct output shape is lost; in this case we do need to
  # broadcast both x and y in case their dimensions are not equal; e.g. if x is
  # 1x1 and y is 1x1x1x1 we must broadcast both to 1x1x1x1)
  broadcast_scalars = length(draws_x) == 1 && length(draws_y) == 1
  draws_x <- broadcast_array(draws_x, new_dim, broadcast_scalars = broadcast_scalars)
  draws_y <- broadcast_array(draws_y, new_dim, broadcast_scalars = broadcast_scalars)

  new_rvar(f(draws_x, draws_y), .nchains = nchains(e1))
}

#' @export
Math.rvar <- function(x, ...) {
  f <- get(.Generic)

  if (.Generic %in% c("cumsum", "cumprod", "cummax", "cummin")) {
    # cumulative functions need to be handled differently
    # from other functions in this generic
    new_rvar(t(apply(draws_of(x), 1, f)), .nchains = nchains(x))
  } else {
    new_rvar(f(draws_of(x), ...), .nchains = nchains(x))
  }
}


# matrix stuff ---------------------------------------------------

#' Matrix multiplication of random variables
#'
#' Matrix multiplication of random variables.
#'
#' @name rvar-matmult
#' @aliases %**%
#' @param x (multiple options) The object to be postmultiplied by `y`:
#'   * An [`rvar`]
#'   * A [`numeric`] vector or matrix
#'   * A [`logical`] vector or matrix
#'
#'   If a vector is used, it is treated as a *row* vector.
#'
#' @param y (multiple options) The object to be premultiplied by `x`:
#'   * An [`rvar`]
#'   * A [`numeric`] vector or matrix
#'   * A [`logical`] vector or matrix
#'
#'   If a vector is used, it is treated as a *column* vector.
#'
#' @details
#' If `x` or `y` are vectors, they are converted into matrices prior to multiplication, with `x`
#' converted to a row vector and `y` to a column vector. Numerics and logicals can be multiplied
#' by [`rvar`]s and are broadcasted across all draws of the [`rvar`] argument. Tensor multiplication
#' is used to efficiently multiply matrices across draws, so if either `x` or `y` is an [`rvar`],
#' `x %**% y` will be much faster than `rdo(x %*% y)`.
#'
#' Because [`rvar`] is an S3 class and S3 classes cannot properly override `%*%`, [`rvar`]s use
#' `%**%` for matrix multiplication.
#'
#' @return An [`rvar`] representing the matrix product of `x` and `y`.
#'
#' @examples
#'
#' # d has mu (mean vector of length 3) and Sigma (3x3 covariance matrix)
#' d <- as_draws_rvars(example_draws("multi_normal"))
#' d$Sigma
#'
#' # trivial example: multiplication by a non-random matrix
#' d$Sigma %**% diag(1:3)
#'
#' # Decompose Sigma into R s.t. R'R = Sigma ...
#' R <- chol(d$Sigma)
#' # ... and recreate Sigma using matrix multiplication
#' t(R) %**% R
#'
#' @importFrom tensorA mul.tensor as.tensor
#' @export
`%**%` <- function(x, y) {
  # Fast version of rdo(x %*% y)

  # convert both objects into rvars if they aren't already (this will ensure
  # we have a 3d draws array for each variable)
  x <- as_rvar(x)
  y <- as_rvar(y)

  # ensure everything is a matrix by adding dimensions as necessary to make `x`
  # a row vector and `y` a column vector
  ndim_x <- length(dim(x))
  if (ndim_x == 1) {
    dim(x) <- c(1, dim(x))
  } else if (ndim_x != 2) {
    stop_no_call("First argument (`x`) is not a vector or matrix, cannot matrix-multiply")
  }

  ndim_y <- length(dim(y))
  if (ndim_y == 1) {
    dim(y) <- c(dim(y), 1)
  } else if (ndim_y != 2) {
    stop_no_call("Second argument (`y`) is not a vector or matrix, cannot matrix-multiply")
  }

  # conform the draws dimension in both variables
  c(x, y) %<-% conform_rvar_ndraws_nchains(list(x, y))

  # drop the names of the dimensions (mul.tensor gets uppity if dimension names
  # are duplicated, but we don't care about that)
  x_tensor <- as.tensor(draws_of(x))
  y_tensor <- as.tensor(draws_of(y))
  names(dim(x_tensor)) <- NULL
  names(dim(y_tensor)) <- NULL

  # do a tensor multiplication equivalent of the requested matrix multiplication
  result <- unclass(mul.tensor(x_tensor, 3, y_tensor, 2, by = 1))

  # move draws dimension back to the front
  result <- aperm(result, c(3,1,2))

  # restore dimension names (as.tensor adds dummy names to dimensions)
  names(dim(result)) <- NULL
  result <- copy_dimnames(draws_of(x), 1:2, result, 1:2)
  result <- copy_dimnames(draws_of(y), 3, result, 3)

  new_rvar(result, .nchains = nchains(x))
}

#' Cholesky decomposition of random matrix
#'
#' Cholesky decomposition of an [`rvar`] containing a matrix.
#'
#' @param x (rvar) A 2-dimensional [`rvar`].
#' @param ... Additional parameters passed on to `chol.tensor()`
#'
#' @return An [`rvar`] containing the upper triangular factor of the Cholesky
#' decomposition, i.e., the matrix \eqn{R} such that \eqn{R'R = x}.
#'
#' @importFrom tensorA chol.tensor as.tensor
#' @export
chol.rvar <- function(x, ...) {
  # ensure x is a matrix
  if (length(dim(x)) != 2) {
    stop_no_call("`x` must be a random matrix")
  }

  # must re-order draws dimension to the end, as chol.tensor expects it there
  x_tensor <- as.tensor(aperm(draws_of(x), c(2,3,1)))

  # do the cholesky decomp
  result <- unclass(chol.tensor(x_tensor, 1, 2, ...))

  # move draws dimension back to the front
  result <- aperm(result, c(3,1,2))

  # drop dimension names (chol.tensor screws them around)
  names(dim(result)) <- NULL

  new_rvar(result, .nchains = nchains(x))
}

#' @importFrom methods setGeneric
#' @export
setGeneric("diag")

#' Matrix diagonals (including for random variables)
#'
#' Extract the diagonal of a matrix or construct a matrix, including random
#' matrices (2-dimensional [`rvar`]s). Makes [`base::diag()`] generic.
#'
#' @inheritParams base::diag
#' @param x (numeric,rvar) a matrix, vector, 1D array, missing, or a 1- or
#' 2-dimensional [`rvar`].
#'
#' @details
#' Makes [`base::diag()`] into a generic function. See that function's documentation
#' for usage with [`numeric`]s and for usage of [`diag<-`], which is also supported
#' by [`rvar`].
#'
#' @return
#'
#' For [`rvar`]s, has two modes:
#'
#' 1. `x` is a matrix-like [`rvar`]: it returns the diagonal as a vector-like [`rvar`]
#' 2. `x` is a vector-like [`rvar`]: it returns a matrix-like [`rvar`] with `x` as
#' the diagonal and zero for off-diagonal entries.
#'
#' @seealso [`base::diag()`]
#'
#' @examples
#'
#' # Sigma is a 3x3 covariance matrix
#' Sigma <- as_draws_rvars(example_draws("multi_normal"))$Sigma
#' Sigma
#'
#' diag(Sigma)
#'
#' diag(Sigma) <- 1:3
#' Sigma
#'
#' diag(as_rvar(1:3))
#'
#' @importFrom methods setMethod callNextMethod
#' @export
setMethod("diag", signature(x = "rvar"), function(x = 1, nrow, ncol, names = TRUE) {
  if (length(dim(x)) > 1) {
    # base implementation of diag() works on rvars except when x is a vector
    callNextMethod()
  } else {
    if (missing(nrow)) {
      nrow <- length(x)
    }
    if (missing(ncol)) {
      ncol <- nrow
    }
    out <- as_rvar(matrix(rep(0, nrow * ncol), nrow = nrow, ncol = ncol))
    n <- min(nrow, ncol)
    x <- rep_len(x, n)
    i <- seq_len(n)
    out[cbind(i, i)] <- x
    out
  }
})


# transpose and permutation -----------------------------------------------

#' @export
t.rvar = function(x) {
  .draws = draws_of(x)
  ndim = length(dim(.draws))

  if (length(x) != 0 && ndim == 2) {
    # vector
    .dimnames = dimnames(.draws)
    dim(.draws) = c(dim(.draws)[1], 1, dim(.draws)[2])
    dimnames(.draws) = c(.dimnames[1], list(NULL), .dimnames[2])
    result <- new_rvar(.draws, .nchains = nchains(x))
  } else if (ndim == 3) {
    .draws <- aperm(.draws, c(1, 3, 2))
    result <- new_rvar(.draws, .nchains = nchains(x))
  } else {
    stop_no_call("argument is not a random vector or matrix")
  }
  result
}

#' @export
aperm.rvar = function(a, perm, ...) {
  .draws <- aperm(draws_of(a), c(1, perm + 1), ...)
  new_rvar(.draws, .nchains = nchains(a))
}
