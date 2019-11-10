
# double-dispatch boilerplate ---------------------------------------------

#' @importFrom vctrs vec_ptype2
vec_ptype2.rvar <- function(x, y, ...) UseMethod("vec_ptype2.rvar", y)
vec_ptype2.rvar.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @importFrom vctrs vec_cast
vec_cast.rvar <- function(x, to, ...) UseMethod("vec_cast.rvar")
vec_cast.rvar.default <- function(x, to, ...) vec_default_cast(x, to)

#' @export
as_rvar <- function(x) {
  vec_cast(x, new_rvar())
}

# identity cast -----------------------------------------------------------

vec_ptype2.rvar.rvar <- function(x, y, ...) new_rvar()

vec_cast.rvar.rvar <- function(x, to, ...) x


# numeric and logical casts -----------------------------------------------

vec_ptype2.numeric.rvar <- function(x, y, ...) new_rvar()
vec_ptype2.rvar.numeric <- function(x, y, ...) new_rvar()
vec_cast.rvar.numeric <- function(x, to, ...) rdo(x, .ndraws = 1)

vec_ptype2.logical.rvar <- function(x, y, ...) new_rvar()
vec_ptype2.rvar.logical <- function(x, y, ...) new_rvar()
vec_cast.rvar.logical <- function(x, to, ...) rdo(x, .ndraws = 1)
