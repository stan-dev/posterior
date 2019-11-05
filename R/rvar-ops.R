#' Operations on random variables
#'
#' Basic math operations on [rvar]s
#'
#' @name rvar-ops
NULL

#' @rdname rvar-ops
#' @export
Math.rvar = function(x) {
  rvar_map(x$draws, .Generic)
}

#' @rdname rvar-ops
#' @export
Ops.rvar = function(e1, e2) {
  if (is_rvar(e1)){
    draws1 = e1$draws
    if (is_rvar(e2)) {
      draws2 = e2$draws
      rvar_map2(draws1, draws2, .Generic)
    } else {
      rvar_map(draws1, .Generic, e2 = e2)
    }
  } else {
    draws2 = e2$draws
    rvar_map(draws2, .Generic, e1 = e1)
  }
}

#' @export
c.rvar = function(...) {
  args = list(...)
  if (length(args) == 1) {
    return(args[[1]])
  }

  draws1 = args[[1]]$draws
  if (is_rvar(args[[2]])) {
    draws2 = args[[2]]$draws
    result = rvar_map2(draws1, draws2, c)
  } else {
    result = rvar_map(draws1, c, args[[2]])
  }

  if (length(args) > 2) {
    args[[1]] = result
    args[[2]] = NULL
    do.call(c, args)
  } else {
    result
  }
}



# helpers -----------------------------------------------------------------

# map a function over var
rvar_map = function(.x, .f, ...) {
  rvar(lapply(.x, .f, ...))
}

# map a function over .x and .y in parallel.
rvar_map2 = function(.x, .y, .f, ...) {
  rvar(mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE, USE.NAMES = FALSE))
}

