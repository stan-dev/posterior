
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
  is.factor(draws_of(x))
}

#' @export
is_rvar_ordered <- function(x) {
  is.ordered(draws_of(x))
}

#' @export
as_rvar_factor <- function(x) UseMethod("as_rvar_factor")

#' @export
as_rvar_factor.default <- function(x) {
  as_rvar_factor(as_rvar(x))
}

#' @export
as_rvar_factor.rvar <- function(x) {
  if (is_rvar_factor(x)) {
    x
  } else {
    .as_rvar_factor_rvar(x)
  }
}

.as_rvar_factor_rvar <- function(x, as_factor = as.factor) {
  .draws <- draws_of(x)
  factor_draws <- as_factor(.draws)
  dim(factor_draws) <- dim(.draws)
  dimnames(factor_draws) <- dimnames(.draws)
  draws_of(x) <- factor_draws
  x
}

#' @export
as_rvar_ordered <- function(x) UseMethod("as_rvar_ordered")

#' @export
as_rvar_ordered.default <- function(x) {
  as_rvar_ordered(as_rvar(x))
}

#' @export
as_rvar_ordered.rvar <- function(x) {
  if (is_rvar_ordered(x)) {
    x
  } else {
    .as_rvar_factor_rvar(x, as.ordered)
  }
}
