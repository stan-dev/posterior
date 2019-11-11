vec_math.rvar <- function(.fn, .x, ...) {
  .fn_obj = getExportedValue("base", .fn)
  .draws = draws_of(.x)

  switch(.fn,
    prod = ,
    sum = ,
    any = ,
    all = ,
    mean = new_rvar(apply(.draws, length(dim(.draws)), .fn_obj)),
    new_rvar(.fn_obj(.draws, ...))
  )
}


# double dispatch boilerplate ---------------------------------------------

vec_arith.rvar <- function(op, x, y, ...) {
  UseMethod("vec_arith.rvar", y)
}
vec_arith.rvar.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}


# [rvar, rvar] operations ---------------------------------------------------

vec_arith.rvar.rvar <- function(op, x, y, ...) {
  draws_x = draws_of(x)
  draws_y = draws_of(y)

  # if number of dimensions is not equal, pad with 1s before the
  # draws dimension so that broadcasting works with rray
  ndim_x = length(dim(draws_x))
  ndim_y = length(dim(draws_y))
  if (ndim_x < ndim_y) {
    dim(draws_x) = c(dim(draws_x)[-ndim_x], rep(1, ndim_y - ndim_x), dim(draws_x)[ndim_x])
  } else if (ndim_y < ndim_x) {
    dim(draws_y) = c(dim(draws_y)[-ndim_y], rep(1, ndim_x - ndim_y), dim(draws_y)[ndim_y])
  }

  op_fun = switch(op,
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


# [rvar, numeric] and [rvar, logical] operations ---------------------------------------------------

vec_arith.rvar.numeric <- function(op, x, y, ...) {
  vec_arith.rvar.rvar(op, x, as_rvar(y), ...)
}

vec_arith.numeric.rvar <- function(op, x, y, ...) {
  vec_arith.rvar.rvar(op, as_rvar(x), y, ...)
}

vec_arith.rvar.logical = vec_arith.rvar.numeric
vec_arith.logical.rvar = vec_arith.rvar.logical



# comparison operators ----------------------------------------------------

Ops.rvar = function(e1, e2) {
  if (.Generic %in% c("==", "!=", "<", "<=", ">=", ">")) {
    vec_arith.rvar.rvar(.Generic, as_rvar(e1), as_rvar(e2))
  } else {
    NextMethod()
  }
}






