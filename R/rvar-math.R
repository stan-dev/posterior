# double-dispatch boilerplate ---------------------------------------------
# the extra roxygen @method and @export bits in here are necessary for
# S3 double dispatch. See vignette("s3-vector").

#' @importFrom vctrs vec_arith
#' @method vec_arith rvar
#' @export
#' @export vec_arith.rvar
vec_arith.rvar <- function(op, x, y, ...) {
  UseMethod("vec_arith.rvar", y)
}
#' @method vec_arith.rvar default
#' @export
#' @importFrom vctrs stop_incompatible_op
vec_arith.rvar.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}


# (rvar) operations ---------------------------------------------------------

#' @importFrom vctrs vec_math
#' @export
vec_math.rvar <- function(.fn, .x, ...) {
  .fn_obj <- getExportedValue("base", .fn)
  .draws <- draws_of(.x)

  switch(.fn,
    prod = ,
    sum = ,
    any = ,
    all = ,
    mean = new_rvar(apply(.draws, length(dim(.draws)), .fn_obj)),
    new_rvar(.fn_obj(.draws, ...))
  )
}


# (rvar, rvar) operations ---------------------------------------------------

#' @method vec_arith.rvar rvar
#' @export
vec_arith.rvar.rvar <- function(op, x, y, ...) {
  draws_x <- draws_of(x)
  draws_y <- draws_of(y)

  # if number of dimensions is not equal, pad with 1s before the
  # draws dimension so that broadcasting works with rray
  ndim_x <- length(dim(draws_x))
  ndim_y <- length(dim(draws_y))
  if (ndim_x < ndim_y) {
    dim(draws_x) <- c(dim(draws_x)[-ndim_x], rep(1, ndim_y - ndim_x), dim(draws_x)[ndim_x])
  } else if (ndim_y < ndim_x) {
    dim(draws_y) <- c(dim(draws_y)[-ndim_y], rep(1, ndim_x - ndim_y), dim(draws_y)[ndim_y])
  }

  op_fun <- switch(op,
    `+` = rray::rray_add,
    `-` = rray::rray_subtract,
    `/` = rray::rray_divide,
    `*` = rray::rray_multiply,
    `^` = rray::rray_pow,
    # `%%` = TODO,
    # `%/%` = TODO,
    `&` = rray::rray_logical_and,
    `|` = rray::rray_logical_or,

    `==` = rray::rray_equal,
    `!=` = rray::rray_not_equal,
    `<` = rray::rray_lesser,
    `<=` = rray::rray_lesser_equal,
    `>=` = rray::rray_greater_equal,
    `>` = rray::rray_greater,

    stop_incompatible_op(op, x, y)
  )

  new_rvar(op_fun(draws_x, draws_y))
}


# (rvar, numeric) and (rvar, logical) operations ---------------------------

#' @importFrom vctrs vec_arith.numeric
#' @method vec_arith.numeric rvar
#' @export
vec_arith.numeric.rvar <- function(op, x, y, ...) {
  vec_arith.rvar.rvar(op, as_rvar(x), y, ...)
}
#' @method vec_arith.rvar numeric
#' @export
vec_arith.rvar.numeric <- function(op, x, y, ...) {
  vec_arith.rvar.rvar(op, x, as_rvar(y), ...)
}

#' @importFrom vctrs vec_arith.logical
#' @method vec_arith.logical rvar
#' @export
vec_arith.logical.rvar <- function(op, x, y, ...) {
  vec_arith.rvar.rvar(op, as_rvar(x), y, ...)
}
#' @method vec_arith.rvar logical
#' @export
vec_arith.rvar.logical <- function(op, x, y, ...) {
  vec_arith.rvar.rvar(op, x, as_rvar(y), ...)
}


# comparison operations ---------------------------------------------------

Ops.rvar <- function(e1, e2) {
  if (.Generic %in% c("==", "!=", "<", "<=", ">=", ">")) {
    vec_arith.rvar.rvar(.Generic, as_rvar(e1), as_rvar(e2))
  } else {
    NextMethod()
  }
}


# matrix multiplication ---------------------------------------------------

#' @importFrom tensorA mul.tensor as.tensor
`%*%.rvar` <- function(x, y) {
  # TODO: get someone else to double-check this
  # rdo(x %*% y)

  # ensure everything is a matrix by adding dimensions as necessary to make `x`
  # a row vector and `y` a column vector
  ndim_x <- length(dim(x))
  if (ndim_x == 0) {
    dim(x) <- c(1, length(x))
  } else if (ndim_x == 1) {
    dim(x) <- c(1, dim(x))
  } else if (ndim_x != 2) {
    stop("First argument (`x`) is not a vector or matrix, cannot matrix-multiply")
  }

  ndim_y <- length(dim(y))
  if (ndim_y == 0) {
    dim(y) <- c(length(y), 1)
  } else if (ndim_y == 1) {
    dim(y) <- c(dim(y), 1)
  } else if (ndim_y != 2) {
    stop("Second argument (`y`) is not a vector or matrix, cannot matrix-multiply")
  }

  # convert both objects into rvars if they aren't already (this will give us
  # a 3d draws array for each variable)
  x <- as_rvar(x)
  y <- as_rvar(y)

  # conform the draws dimension in both variables
  .ndraws <- check_ndraws2(x, y)
  x <- broadcast_draws(x, .ndraws)
  y <- broadcast_draws(y, .ndraws)

  # do a tensor multiplication equivalent of the requested matrix multiplication
  result <- mul.tensor(as.tensor(draws_of(x)), 2, as.tensor(draws_of(y)), 1, by = 3)

  # restore names (as.tensor adds dummy names to dimensions)
  names(result) <- names(dimnames(draws_of(x)))
  new_rvar(unclass(result))
}

setMethod("%*%", c(x = "rvar", y = "rvar"), `%*%.rvar`)
setMethod("%*%", c(x = "rvar"), `%*%.rvar`)
setMethod("%*%", c(y = "rvar"), `%*%.rvar`)
