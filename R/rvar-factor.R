
# factor-like rvar methods ------------------------------------------------

#' @export
levels.rvar <- function(x) {
  levels(draws_of(x))
}

#' @export
`levels<-.rvar` <- function(x, value) {
  levels(draws_of(x)) <- value
  x
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


# misc standard methods ---------------------------------------------------

#' @export
unique.rvar_factor <- function(x, incomparables = FALSE, MARGIN = 1, ...) {
  draws_margin <- check_rvar_margin(x, MARGIN)
  .draws <- draws_of(x)
  .levels <- levels(.draws)
  .class <- oldClass(.draws)

  .draws <- unique(unclass(.draws), incomparables = incomparables, MARGIN = draws_margin, ...)

  levels(.draws) <- .levels
  oldClass(.draws) <- .class
  draws_of(x) <- .draws

  x
}

#' @export
duplicated.rvar_factor <- function(x, incomparables = FALSE, MARGIN = 1, ...) {
  draws_margin <- check_rvar_margin(x, MARGIN)
  duplicated(unclass(draws_of(x)), incomparables = incomparables, MARGIN = draws_margin, ...)
}

#' @export
anyDuplicated.rvar_factor <- function(x, incomparables = FALSE, MARGIN = 1, ...) {
  draws_margin <- check_rvar_margin(x, MARGIN)
  anyDuplicated(unclass(draws_of(x)), incomparables = incomparables, MARGIN = draws_margin, ...)
}


# helpers for factor levels -----------------------------------------------

#' Given a list of levels and an output rvar (which is derived from rvars with those levels
#' bound together in some way, e.g. with c() or vec_c() or whatever),
#' combine the factor levels together appropriately and update out as needed
#' @param x an rvar_factor
#' @param list_of_levels a list of character vectors, each a set of levels
#' @param ordered should the output be an ordered factor if possible?
#' @noRd
combine_rvar_factor_levels <- function(x, list_of_levels, ordered = FALSE) {
  .draws <- draws_of(x)

  unique_levels <- unique(list_of_levels)
  if (length(unique_levels) == 1) {
    # levels are the same in all variables, so preserve level order when binding
    .levels <- unique_levels[[1]]
    if (!identical(.levels, levels(x))) {
      .draws <- while_preserving_dims(factor, .draws, .levels)
    }
    if (ordered) {
      # only keep the "ordered" class when the levels were all the same (this
      # mimics base-R, which demotes to unordered factor when combining ordered
      # factors with different levels)
      oldClass(.draws) <- c("ordered", "factor")
    }
  } else {
    # levels are not the same in all variables, so preserve any old levels by
    # merging them together
    .levels <- Reduce(union, list_of_levels)
    .draws <- while_preserving_dims(factor, .draws, .levels)
  }
  if (!is.factor(.draws)) {
    .draws <- while_preserving_dims(factor, .draws)
  }
  draws_of(x) <- .draws

  x
}
