
# factor-like rvar methods ------------------------------------------------

#' @export
levels.rvar <- function(x) {
  levels(draws_of(x))
}

#' @export
`levels<-.rvar` <- function(x, value) {
  levels(draws_of(x)) <- value
}


# type predicates and casting ---------------------------------------------------------

#' @export
is_rvar_factor <- function(x) {
  inherits(x, "rvar_factor")
}

#' @export
is_rvar_ordered <- function(x) {
  inherits(x, "rvar_ordered")
}

#' @export
as_rvar_factor <- function(x) {
  vec_cast(x, new_rvar(factor()))
}

#' @export
as_rvar_ordered <- function(x) {
  vec_cast(x, new_rvar(ordered(NULL)))
}
