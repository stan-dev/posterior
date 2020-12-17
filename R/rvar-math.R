# Expectations and summaries of random variables --------------------------

#' Summaries, expectations, and variances of random variables
#'
#' Compute expectations (`E()` or `mean()`), probabilities (`Pr()`),
#' medians (`median()`), and variances (`variance()`) from a random variable.
#'
#' @param x an [`rvar`]
#' @param na.rm Should `NA` values in the random variable be removed before
#' computing summaries?
#' @param ... further arguments passed to underlying functions (e.g., `base::mean()`
#' or `base::median()`).
#'
#' Both `E()`, `mean()`, and `Pr()` take means over the draws dimension of the provided
#' random variable. `Pr()` additionally checks that the provided [`rvar`]
#' is a logical variable (hence, taking its expectation results in a probability).
#' `median()` takes medians, and `variance()` takes variances.
#'
#' @return
#' A numeric vector with the same dimensions as the given random variable, where
#' each entry in the vector is the mean, median, or variance of the corresponding entry in `x`.
#'
#' @examples
#'
#' set.seed(5678)
#' x = rdo(rnorm(4, mean = 1:4, sd = 2))
#'
#' # These should all be ~= c(1, 2, 3, 4)
#' E(x)
#' mean(x)
#' median(x)
#'
#' # This ...
#' Pr(x < 1.5)
#' # ... should be about the same as this:
#' pnorm(1.5, mean = 1:4, sd = 2)
#'
#' @seealso [Summary.rvar] for summary functions within draws.
#' [rvar-functions] for density, CDF, and quantile functions of random variables.
#' @export
E <- function(x, na.rm = FALSE) {
  summarise_rvar_by_element(x, mean, na.rm = na.rm)
}

#' @rdname E
#' @export
mean.rvar <- function(x, ...) {
  summarise_rvar_by_element(x, mean, ...)
}

#' @rdname E
#' @export
Pr <- function(x, na.rm = FALSE) {
  if (!all(is.logical(draws_of(x)))) {
    stop2("Can only use `Pr(...)` on logical random variables.")
  }
  summarise_rvar_by_element(x, mean, na.rm = na.rm)
}

#' @rdname E
#' @export
median.rvar <- function(x, ...) {
  summarise_rvar_by_element(x, median, ...)
}

#' @importFrom distributional variance
#' @export
distributional::variance

#' @rdname E
#' @export
variance.rvar <- function(x, ...) {
  summarise_rvar_by_element(x, var, ...)
}



# Summary operations ---------------------------------------------------------

#' Within-draw summaries of random variables
#'
#' Compute summaries of random variables within draws, producing a new random variable.
#'
#' @param x an [`rvar`]
#' @param na.rm Should `NA` values in the random variable be removed before
#' computing summaries?
#' @param ... further arguments passed to underlying functions (e.g., `base::mean()`
#' or `base::median()`).
#'
#' @details
#'
#' These functions compute statistics within each draw of the random variable.
#' For summaries over draws (such as expectations), see [E()].
#'
#' Besides `rvar_mean()` and `rvar_median()`, these standard generics are supported:
#'
#' - `all()`, `any()`
#' - `sum()`, `prod()`
#' - `min()`, `max()`
#' - `range()`
#'
#' @return
#' An [`rvar`] of length 1 (or in the case of `range()`, length 2) with the same number
#' of draws as the input rvar(s) containing the summary statistic computed within
#' each draw of the input rvar(s).
#'
#' @examples
#'
#' set.seed(5678)
#' x = rdo(rnorm(4, mean = 1:4, sd = 2))
#'
#' # These will give similar results to mean(1:4),
#' # median(1:4), sum(1:4), prod(1:4), etc
#' rvar_mean(x)
#' rvar_median(x)
#' sum(x)
#' prod(x)
#'
#' @seealso [E()] for summary functions across draws (e.g. expectations).
#' [rvar-functions] for density, CDF, and quantile functions of random variables.
#' @export
Summary.rvar <- function(..., na.rm = FALSE) {
  f <- get(.Generic)
  .Summary.rvar(f, ..., na.rm = na.rm)
}

#' @rdname Summary.rvar
#' @export
range.rvar <- function(..., na.rm = FALSE) {
  .Summary.rvar(base::range, ..., na.rm = na.rm, transpose = TRUE)
}
.Summary.rvar <- function(f, ..., na.rm = FALSE, transpose = FALSE) {
  rvars <- lapply(list(...), function(arg) {
    arg <- as_rvar(arg)
    dim(arg) <- prod(dim(arg))
    arg
  })
  rvars <- conform_rvar_nchains(rvars)

  # bind all args into a single matrix of draws to perform the summary over
  all_draws <- draws_of(do.call(c, rvars))

  # perform summary
  .draws <- apply(all_draws, 1, f, na.rm = na.rm)

  if (transpose) {
    .draws <- t(.draws)
  }
  new_rvar(.draws, .nchains = nchains(rvars[[1]]))
}

#' @rdname Summary.rvar
#' @export
rvar_mean <- function(x, ...) summarise_rvar_within_draws(x, mean, ...)

#' @rdname Summary.rvar
#' @export
rvar_median <- function(x, ...) summarise_rvar_within_draws(x, median, ...)


#' @export
anyNA.rvar <- function(x, ...) anyNA(draws_of(x, ...))

#' @export
is.finite.rvar <- function(x, ...) rvar_apply_vec_fun(is.finite, x, ...)
#' @export
is.infinite.rvar <- function(x, ...) rvar_apply_vec_fun(is.infinite, x, ...)
#' @export
is.nan.rvar <- function(x, ...) rvar_apply_vec_fun(is.nan, x, ...)
#' @export
is.na.rvar <- function(x, ...) summarise_rvar_by_element(x, function(x) anyNA(x))


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
  # TODO: skip broadcast for scalars (for speed)
  new_dim <- dim2_common(dim(draws_x), dim(draws_y))
  draws_x <- broadcast_array(draws_x, new_dim)
  draws_y <- broadcast_array(draws_y, new_dim)

  new_rvar(f(draws_x, draws_y), .nchains = nchains(e1))
}

#' @export
Math.rvar <- function(x, ...) {
  f <- get(.Generic)

  new_rvar(f(draws_of(x)), .nchains = nchains(x))
}


# matrix stuff ---------------------------------------------------

#' Matrix multiplication of random variables
#'
#' Matrix multiplication of random variables.
#'
#' @name rvar-matmult
#' @aliases %**%
#' @param x An [`rvar`], [`numeric`], or [`logical`]. Must be 1 or 2-dimensional. If it is a vector,
#' it is treated as a row vector.
#' @param y An [`rvar`], [`numeric`], or [`logical`]. Must be 1 or 2-dimensional. If it is a vector,
#' it is treated as a column vector.
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
#' @importFrom tensorA mul.tensor as.tensor
#' @export
`%**%` <- function(x, y) {
  # Fast version of rdo(x %*% y)

  # ensure everything is a matrix by adding dimensions as necessary to make `x`
  # a row vector and `y` a column vector
  ndim_x <- length(dim(x))
  if (ndim_x == 1) {
    dim(x) <- c(1, dim(x))
  } else if (ndim_x != 2) {
    stop2("First argument (`x`) is not a vector or matrix, cannot matrix-multiply")
  }

  ndim_y <- length(dim(y))
  if (ndim_y == 1) {
    dim(y) <- c(dim(y), 1)
  } else if (ndim_y != 2) {
    stop2("Second argument (`y`) is not a vector or matrix, cannot matrix-multiply")
  }

  # convert both objects into rvars if they aren't already (this will give us
  # a 3d draws array for each variable)
  x <- as_rvar(x)
  y <- as_rvar(y)

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
#' @param x An [`rvar`].
#' @param ... Additional parameters passed on to `chol.tensor()`
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
#' @return An [`rvar`] containing the upper triangular factor of the Cholesky
#' decomposition, i.e., the matrix $R$ such that $R'R = x$
#'
#' @importFrom tensorA chol.tensor as.tensor
#' @export
chol.rvar <- function(x, ...) {
  # ensure x is a matrix
  if (length(dim(x)) != 2) {
    stop2("`x` must be a random matrix")
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

# transpose and permutation -----------------------------------------------

#' @export
t.rvar = function(x) {
  .draws = draws_of(x)
  ndim = length(dim(.draws))

  if (length(x) != 0 && ndim == 2) {
    # vector
    dim(.draws) = c(dim(.draws)[1], 1, dim(.draws)[2])
    new_rvar(.draws, .nchains = nchains(x))
  } else if (ndim == 3) {
    .draws <- aperm(.draws, c(1, 3, 2))
    new_rvar(.draws, .nchains = nchains(x))
  } else {
    stop2("argument is not a random vector or matrix")
  }
}
