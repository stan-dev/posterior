
# double-dispatch boilerplate ---------------------------------------------
# the extra roxygen @method and @export bits in here are necessary for
# S3 double dispatch. See vignette("s3-vector").

#' @importFrom vctrs vec_ptype2 vec_default_ptype2
#' @method vec_ptype2 rvar
#' @export
#' @export vec_ptype2.rvar
vec_ptype2.rvar <- function(x, y, ...) UseMethod("vec_ptype2.rvar", y)
#' @method vec_ptype2.rvar default
#' @export
vec_ptype2.rvar.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @importFrom vctrs vec_cast vec_default_cast
#' @method vec_cast rvar
#' @export
#' @export vec_cast.rvar
vec_cast.rvar <- function(x, to, ...) UseMethod("vec_cast.rvar")
#' @method vec_cast.rvar default
#' @export
vec_cast.rvar.default <- function(x, to, ...) vec_default_cast(x, to)

#' @rdname rvar
#' @export
as_rvar <- function(x) {
  if (is_rvar(x)) x
  else vec_cast(x, new_rvar())
}


# identity cast -----------------------------------------------------------

#' @method vec_ptype2.rvar rvar
#' @export
vec_ptype2.rvar.rvar <- function(x, y, ...) new_rvar()

#' @method vec_cast.rvar rvar
#' @export
vec_cast.rvar.rvar <- function(x, to, ...) x


# numeric and logical casts -----------------------------------------------

#' @importFrom vctrs vec_ptype2.double
#' @method vec_ptype2.double rvar
#' @export
vec_ptype2.double.rvar <- function(x, y, ...) new_rvar()
#' @method vec_ptype2.rvar double
#' @export
vec_ptype2.rvar.double <- function(x, y, ...) new_rvar()
#' @method vec_cast.rvar double
#' @export
vec_cast.rvar.double <- function(x, to, ...) rdo(x, ndraws = 1)

#' @importFrom vctrs vec_ptype2.integer
#' @method vec_ptype2.integer rvar
#' @export
vec_ptype2.integer.rvar <- function(x, y, ...) new_rvar()
#' @method vec_ptype2.rvar integer
#' @export
vec_ptype2.rvar.integer <- function(x, y, ...) new_rvar()
#' @method vec_cast.rvar integer
#' @export
vec_cast.rvar.integer <- function(x, to, ...) rdo(x, ndraws = 1)

#' @importFrom vctrs vec_ptype2.logical
#' @method vec_ptype2.logical rvar
#' @export
vec_ptype2.logical.rvar <- function(x, y, ...) new_rvar()
#' @method vec_ptype2.rvar logical
#' @export
vec_ptype2.rvar.logical <- function(x, y, ...) new_rvar()
#' @method vec_cast.rvar logical
#' @export
vec_cast.rvar.logical <- function(x, to, ...) rdo(x, ndraws = 1)
