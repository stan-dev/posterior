#' Coerce to a random variable
#'
#' Convert `x` to an [rvar()] object.
#'
#' @param x An object that can be converted to an [rvar], such as a vector or array
#' (or an [rvar] itself).
#' @template args-rvar-dim
#'
#' @details For objects that are already [rvar]s, returns them (with modified dimensions
#' if `dim` is not `NULL`).
#'
#' For numeric or logical vectors or arrays, returns an [rvar] with a single draw and
#' the same dimensions as `x`. This is in contrast to the [rvar()] constructor, which
#' treats the first dimension of `x` as the draws dimension. As a result, `as_rvar()`
#' is useful for creating constants.
#'
#' @seealso [rvar()] to construct [rvar]s directly.
#'
#' @return An object of class `"rvar"` representing a random variable.
#'
as_rvar <- function(x, dim = NULL) {
  x <-
    if (is_rvar(x)) x
  else vec_cast(x, new_rvar())

  if (!is.null(dim)) {
    dim(x) <- dim
  }

  x
}


# double-dispatch boilerplate ---------------------------------------------
# the extra roxygen @method and @export bits in here are necessary for
# S3 double dispatch. See vignette("s3-vector").

#' rvar vctrs compatibility
#'
#' These functions implement compatibility with [vctrs-package] for [rvar]s.
#'
#' @name vctrs-compat
#'
#' @param x,y Vectors
#' @param to Type to cast to
#' @param ... Further arguments passed to other functions
#' @param x_arg,y_arg Argument names for `x` and `y`
#'
#' @details
#'
#' See the corresponding functions in [vctrs-package] for more information.
#'
#' @seealso [vctrs::vec_cast()],[vctrs::vec_ptype2()],[vctrs::vec_arith()],
#' [vctrs::vec_math()]
#'
#' @importFrom vctrs vec_ptype vec_ptype2 vec_default_ptype2
#' @method vec_ptype2 rvar
#' @export
#' @export vec_ptype2.rvar
vec_ptype2.rvar <- function(x, y, ...) UseMethod("vec_ptype2.rvar", y)
#' @rdname vctrs-compat
#' @method vec_ptype2.rvar default
#' @export
vec_ptype2.rvar.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @rdname vctrs-compat
#' @importFrom vctrs vec_cast vec_default_cast
#' @method vec_cast rvar
#' @export
#' @export vec_cast.rvar
vec_cast.rvar <- function(x, to, ...) UseMethod("vec_cast.rvar")
#' @rdname vctrs-compat
#' @method vec_cast.rvar default
#' @export
vec_cast.rvar.default <- function(x, to, ...) vec_default_cast(x, to)


# identity cast -----------------------------------------------------------

#' @rdname vctrs-compat
#' @method vec_ptype2.rvar rvar
#' @export
vec_ptype2.rvar.rvar <- function(x, y, ...) new_rvar()

#' @rdname vctrs-compat
#' @method vec_cast.rvar rvar
#' @export
vec_cast.rvar.rvar <- function(x, to, ...) x


# numeric and logical casts -----------------------------------------------

#' @rdname vctrs-compat
#' @importFrom vctrs vec_ptype2.double
#' @method vec_ptype2.double rvar
#' @export
vec_ptype2.double.rvar <- function(x, y, ...) new_rvar()
#' @rdname vctrs-compat
#' @method vec_ptype2.rvar double
#' @export
vec_ptype2.rvar.double <- function(x, y, ...) new_rvar()
#' @rdname vctrs-compat
#' @method vec_cast.rvar double
#' @export
vec_cast.rvar.double <- function(x, to, ...) rdo(x, ndraws = 1)

#' @rdname vctrs-compat
#' @importFrom vctrs vec_ptype2.integer
#' @method vec_ptype2.integer rvar
#' @export
vec_ptype2.integer.rvar <- function(x, y, ...) new_rvar()
#' @rdname vctrs-compat
#' @method vec_ptype2.rvar integer
#' @export
vec_ptype2.rvar.integer <- function(x, y, ...) new_rvar()
#' @rdname vctrs-compat
#' @method vec_cast.rvar integer
#' @export
vec_cast.rvar.integer <- function(x, to, ...) rdo(x, ndraws = 1)

#' @rdname vctrs-compat
#' @importFrom vctrs vec_ptype2.logical
#' @method vec_ptype2.logical rvar
#' @export
vec_ptype2.logical.rvar <- function(x, y, ...) new_rvar()
#' @rdname vctrs-compat
#' @method vec_ptype2.rvar logical
#' @export
vec_ptype2.rvar.logical <- function(x, y, ...) new_rvar()
#' @rdname vctrs-compat
#' @method vec_cast.rvar logical
#' @export
vec_cast.rvar.logical <- function(x, to, ...) rdo(x, ndraws = 1)
