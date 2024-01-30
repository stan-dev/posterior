#' Factor random variables of arbitrary dimension
#'
#' Random variables backed by [factor]-like arrays of arbitrary dimension.
#'
#' @name rvar_factor
#'
#' @inheritParams rvar
#' @inheritDotParams base::factor
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
#' set.seed(1234)
#'
#' # To create a "scalar" `rvar_factor`, pass a one-dimensional array or a vector
#' # whose length (here `4000`) is the desired number of draws:
#' x <- rvar(sample(c("a","a","a","b","c"), 4000, replace = TRUE))
#' x
#'
#' # Create random vectors by adding an additional dimension:
#' x_array <- array(c(
#'     sample(c("a","a","a","b","c"), 4000, replace = TRUE),
#'     sample(c("a","a","b","c","c"), 4000, replace = TRUE),
#'     sample(c("b","b","b","b","c"), 4000, replace = TRUE),
#'     sample(c("d","d","b","b","c"), 4000, replace = TRUE)
#'   ), dim = c(4000, 4))
#' rvar_factor(x_array)
#'
#' # You can also create ordered factors
#' rvar_ordered(x_array)
#'
#' # arguments of factor() and ordered() are passed down by the constructor
#' # e.g. we can reorder levels of an ordered factor:
#' rvar_ordered(x_array, levels = c("d","c","b","a"))
#'
#' # Unlike base factors, rvar factors can be matrices or arrays:
#' rvar_factor(x_array, dim = c(2, 2))
#'
#' # If the input to rvar_factor() is an array with a `"levels"` attribute, it
#' # will use those as the levels of the factor
#' y_array <- t(array(rbinom(3000, 1, c(0.1, 0.5, 0.9)) + 1, dim = c(3, 1000)))
#' rvar(y_array)
#' # with levels
#' attr(y_array, "levels") = c("a", "b")
#' rvar_factor(y_array)
#'
#' @export
rvar_factor <- function(
  x = factor(), dim = NULL, dimnames = NULL, nchains = NULL, with_chains = FALSE, ...
) {

  # to ensure we pick up levels already attached to x (if there are any), we
  # need to convert x to a factor here if it has levels
  if (!is.factor(x) && !is.null(attr(x, "levels"))) {
    x <- copy_dims(x, factor(x, labels = attr(x, "levels")))
  }

  out <- rvar(
    x, dim = dim, dimnames = dimnames, nchains = nchains, with_chains = with_chains
  )
  .rvar_to_rvar_factor(out, ...)
}

#' @rdname rvar_factor
#' @export
rvar_ordered <- function(
  x = ordered(NULL), dim = NULL, dimnames = NULL, nchains = NULL, with_chains = FALSE, ...
) {

  rvar_factor(
    x, dim = dim, dimnames = dimnames, nchains = nchains, with_chains = with_chains, ordered = TRUE, ...
  )
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

#' Is `x` a factor random variable?
#'
#' Test if `x` is an [`rvar_factor`] or [`rvar_ordered`].
#'
#' @inheritParams is_rvar
#'
#' @seealso [as_rvar_factor()] and [as_rvar_ordered()] to convert objects to
#' `rvar_factor`s and `rvar_ordered`s.
#'
#' @return `TRUE` if `x` is an [`rvar_factor`] or [`rvar_ordered`], `FALSE` otherwise.
#'
#' @export
is_rvar_factor <- function(x) {
  inherits(x, "rvar_factor")
}

#' @rdname is_rvar_factor
#' @export
is_rvar_ordered <- function(x) {
  inherits(x, "rvar_ordered")
}

#' Coerce to a factor random variable
#'
#' Convert `x` to an [`rvar_factor`] or [`rvar_ordered`] object.
#'
#' @inheritParams as_rvar
#' @inheritDotParams base::factor
#'
#' @details For objects that are already [`rvar`]s, returns them (with modified dimensions
#' if `dim` is not `NULL`), possibly adding levels using the unique values of the draws of
#' the `rvar` (if the object is not already factor-like).
#'
#' For numeric, logical, factor, or character vectors or arrays, returns an [`rvar_factor`]
#' or [`rvar_ordered`] with a single draw and the same dimensions as `x`. This is in contrast
#' to the [rvar_factor()] and [rvar_ordered()] constructors, which treats the first dimension
#' of `x` as the draws dimension. As a result, `as_rvar_factor()` and `as_rvar_ordered()`
#' are useful for creating constants.
#'
#' @seealso [rvar()], [rvar_factor()], and [rvar_ordered()] to construct [`rvar`]s directly.
#' See [rdo()], [rfun()], and [rvar_rng()] for higher-level interfaces for creating `rvar`s.
#'
#' @return An object of class `"rvar_factor"` or `"rvar_ordered"` representing a random variable.
#'
#' @examples
#'
#' # You can use as_rvar_factor() to create "constant" rvars (having only one draw):
#' x <- as_rvar_factor("a")
#' x
#'
#' # Such constants can be of arbitrary shape:
#' as_rvar_factor(letters[1:4])
#' as_rvar_ordered(matrix(letters[1:10], nrow = 5))
#' as_rvar_factor(array(letters[1:12], dim = c(2, 3, 2)))
#'
#' @export
as_rvar_factor <- function(x, dim = NULL, dimnames = NULL, nchains = NULL, ...) {
  out <- .as_rvar(
    x, dim = dim, dimnames = dimnames, nchains = nchains, ptype = new_rvar(factor())
  )
  .rvar_to_rvar_factor(out, ...)
}

#' @rdname as_rvar_factor
#' @export
as_rvar_ordered <- function(x, dim = NULL, dimnames = NULL, nchains = NULL, ...) {
  out <- .as_rvar(
    x, dim = dim, dimnames = dimnames, nchains = nchains, ptype = new_rvar(ordered(NULL))
  )
  .rvar_to_rvar_factor(out, ordered = TRUE, ...)
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
  new_levels <- levels(.draws) %||% unique(as.character(.draws))

  unique_levels <- unique(list_of_levels)
  # zero-length levels lists don't count (since can only come from factors with only missing values)
  unique_levels <- unique_levels[lengths(unique_levels) > 0]
  if (length(unique_levels) <= 1 && all(new_levels %in% unique_levels[1][[1]])) {
    # levels are the same in all variables, so preserve level order when binding
    .levels <- unique_levels[1][[1]]
    # We only keep the "ordered" class when the levels were all the same (this
    # mimics base-R, which demotes to unordered factor when combining ordered
    # factors with different levels)
    .draws <- copy_dims(.draws, factor(.draws, .levels, ordered = ordered))
  } else {
    # levels are not the same in all variables, so preserve any old levels by
    # merging them together, but do not apply the "ordered" class
    all_levels <- unlist(list_of_levels, recursive = FALSE, use.names = FALSE)
    .levels <- unique(c(all_levels, new_levels))
    .draws <- copy_dims(.draws, factor(.draws, .levels))
  }
  draws_of(x) <- .draws

  x
}
