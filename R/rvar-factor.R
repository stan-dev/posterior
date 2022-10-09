#' Factor random variables of arbitrary dimension
#'
#' Random variables backed by [factor]-like arrays of arbitrary dimension.
#'
#' @name rvar_factor
#'
#' @inheritDotParams rvar
#' @param x (multiple options) The object to convert to an `rvar`:
#'   * A vector of draws from a distribution.
#'   * An array where the first dimension represents draws from a distribution.
#'     The resulting [`rvar`] will have dimension `dim(x)[-1]`; that is,
#'     everything except the first dimension is used for the shape of the
#'     variable, and the first dimension is used to index draws from the
#'     distribution (see **Examples**). Optionally,
#'     if `with_chains == TRUE`, the first dimension indexes the iteration and the
#'     second dimension indexes the chain (see `with_chains`).
#'
#' @details
#'
#' A subtype of [rvar()] that represents a (possibly multidimensional) sample of
#' a [factor] or an [ordered] factor. It is otherwise very similar to the basic [rvar()]:
#' it is backed by a multidimensional array with draws as the first dimension.
#' The primary difference is that the backing array has class `"factor"` (for [rvar_factor()])
#' or `c("ordered", "factor")` (for [rvar_ordered()]). If you
#' pass a [factor] or [ordered] factor to [rvar()] it will automatically return
#' an object with the classes `"rvar_factor"` or `c("rvar_ordered", "rvar_factor")`.
#'
#' See [rvar()] for more details on the internals of the random variable datatype.
#'
#' @seealso [as_rvar_factor()] to convert objects to `rvar_factor`s. See [rdo()], [rfun()], and
#' [rvar_rng()] for higher-level interfaces for creating `rvar`s.
#'
#' @return An object of class `"rvar_factor"` representing a `factor`-like random variable.
#'
#' @examples
#'
#' # TODO: update examples
#'
#' set.seed(1234)
#'
#' # To create a "scalar" `rvar`, pass a one-dimensional array or a vector
#' # whose length (here `4000`) is the desired number of draws:
#' x <- rvar(rnorm(4000, mean = 1, sd = 1))
#' x
#'
#' # Create random vectors by adding an additional dimension:
#' n <- 4   # length of output vector
#' x <- rvar(array(rnorm(4000 * n, mean = rep(1:n, each = 4000), sd = 1), dim = c(4000, n)))
#' x
#'
#' # Create a random matrix:
#' rows <- 4
#' cols <- 3
#' x <- rvar(array(rnorm(4000 * rows * cols, mean = 1, sd = 1), dim = c(4000, rows, cols)))
#' x
#'
#' # If the input sample comes from multiple chains, we can indicate that using the
#' # nchains argument (here, 1000 draws each from 4 chains):
#' x <- rvar(rnorm(4000, mean = 1, sd = 1), nchains = 4)
#' x
#'
#' # Or if the input sample has chain information as its second dimension, we can
#' # use with_chains to create the rvar
#' x <- rvar(array(rnorm(4000, mean = 1, sd = 1), dim = c(1000, 4)), with_chains = TRUE)
#' x
#'
#' @export
rvar_factor <- function(x = factor(), ...) {
  out <- rvar(x, ...)
  if (!is_rvar_factor(out)) {
    # rvar() will automatically convert to a factor if the input is factor-like;
    # so we only need to handle the case here where input is numeric.
    out <- .rvar_to_rvar_factor(out)
  }
  out
}

#' @rdname rvar_factor
rvar_ordered <- function(x = ordered(NULL), ...) {
  out <- rvar(x, ...)
  if (!is_rvar_ordered(out)) {
    if (is_rvar_factor(out)) {
      # factor but not ordered => just update the class (instead of using
      # as.ordered()) so that levels / etc are preserved
      oldClass(draws_of(out)) <- c("ordered", "factor")
    } else {
      out <- .rvar_to_rvar_factor(out, as.ordered)
    }
  }
  out
}

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
