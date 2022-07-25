#' Coerce to a random variable
#'
#' Convert `x` to an [`rvar`] object.
#'
#' @param x (multiple options) An object that can be converted to an [`rvar`],
#'   such as a vector, array, or an [`rvar`] itself.
#' @template args-rvar-dim
#' @template args-rvar-dimnames
#' @param nchains (positive integer) The number of chains. The default is `1`.
#'
#' @details For objects that are already [`rvar`]s, returns them (with modified dimensions
#' if `dim` is not `NULL`).
#'
#' For numeric or logical vectors or arrays, returns an [`rvar`] with a single draw and
#' the same dimensions as `x`. This is in contrast to the [rvar()] constructor, which
#' treats the first dimension of `x` as the draws dimension. As a result, `as_rvar()`
#' is useful for creating constants.
#'
#' @seealso [rvar()] to construct [`rvar`]s directly.  See [rdo()], [rfun()], and
#' [rvar_rng()] for higher-level interfaces for creating `rvar`s.
#'
#' @return An object of class `"rvar"` representing a random variable.
#'
#' @examples
#'
#' # You can use as_rvar() to create "constant" rvars (having only one draw):
#' x <- as_rvar(1)
#' x
#'
#' # Such constants can be of arbitrary shape:
#' as_rvar(1:4)
#' as_rvar(matrix(1:10, nrow = 5))
#' as_rvar(array(1:12, dim = c(2, 3, 2)))
#'
#' @export
as_rvar <- function(x, dim = NULL, dimnames = NULL, nchains = NULL) {
  out <- x

  if (!is_rvar(out)) {
    out <- vec_cast(out, new_rvar())
  }
  if (is.null(out)) {
    out <- rvar()
  }

  if (!is.null(dim)) {
    dim(out) <- dim
  } else if (is.null(dimnames) && is.vector(x)) {
    # for non-vector-like input (matrices, arrays, etc), vec_cast should
    # have already copied over the dimnames correctly. For vector-like input,
    # it doesn't; so as long as the `dim` argument isn't set we can copy
    # the name over
    names(out) <- names(x)
  }
  if (!is.null(dimnames)) {
    dimnames(out) <- dimnames
  }

  if (!is.null(nchains)) {
    .ndraws <- ndraws(out)
    nchains <- as_one_integer(nchains)
    check_nchains_compat_with_ndraws(nchains, .ndraws)
    nchains_rvar(out) <- nchains
  }

  out
}


# type predicates --------------------------------------------------

#' Is `x` a random variable?
#'
#' Test if `x` is an [`rvar`].
#'
#' @param x (any object) An object to test.
#'
#' @seealso [as_rvar()] to convert objects to `rvar`s.
#'
#' @return `TRUE` if `x` is an [`rvar`], `FALSE` otherwise.
#'
#' @export
is_rvar <- function(x) {
  inherits(x, "rvar")
}

#' @export
is.matrix.rvar <- function(x) {
  length(dim(draws_of(x))) == 3
}

#' @export
is.array.rvar <- function(x) {
  length(dim(draws_of(x))) > 0
}


# type conversion ---------------------------------------------------------

#' @export
as.vector.rvar <- function(x, mode = "any") {
  dim(x) <- NULL
  names(x) <- NULL
  x
}

#' @export
as.list.rvar <- function(x, ...) {
  apply(draws_of(x), 2, new_rvar, .nchains = nchains(x))
}

#' @importFrom rlang as_label
#' @export
as.data.frame.rvar <- function(x, ..., optional = FALSE) {
  out <- as.data.frame.array(x, ..., optional = optional)
  if (length(dim(x)) <= 1 && !optional) {
    names(out) <- as_label(substitute(x))
  }
  out
}

#' @importFrom tibble as_tibble
#' @export
as_tibble.rvar <- function(x, ...) {
  #default name for vectors is `value` with as_tibble
  value <- x
  as_tibble(as.data.frame(value, optional = FALSE), ...)
}


# vctrs proxy / restore --------------------------------------------------------

invalidate_rvar_cache = function(x) {
  attr(x, "cache") <- new.env(parent = emptyenv())
  x
}

#' @importFrom vctrs vec_proxy vec_chop
#' @export
vec_proxy.rvar = function(x, ...) {
  # TODO: probably could do something more efficient here and for restore
  # In the meantime, using caching to help with algorithms that call vec_proxy
  # repeatedly. See https://github.com/r-lib/vctrs/issues/1411

  out <- attr(x, "cache")$vec_proxy
  if (is.null(out)) {
    # proxy is not in the cache, calculate it and store it in the cache
    out <- make_rvar_proxy(x)
    attr(x, "cache")$vec_proxy <- out
  }

  out
}

#' Make a cacheable proxy for vec_proxy.rvar
#' @noRd
make_rvar_proxy = function(x) UseMethod("make_rvar_proxy")

#' @export
make_rvar_proxy.rvar = function(x) {
  .draws = draws_of(x)
  out <- vec_chop(aperm(.draws, c(2, 1, seq_along(dim(.draws))[c(-1,-2)])))

  .nchains <- nchains(x)
  for (i in seq_along(out)) {
    attr(out[[i]], "nchains") <- .nchains
  }

  out
}

#' @export
make_rvar_proxy.rvar_factor = function(x) {
  out <- NextMethod()

  .class <- c(if (is_rvar_ordered(x)) "ordered", "factor")
  .levels <- levels(x)
  for (i in seq_along(out)) {
    attr(out[[i]], "levels") <- .levels
    attr(out[[i]], "class") <- .class
  }

  out
}


#' @importFrom vctrs vec_restore
#' @export
vec_restore.rvar <- function(x, ...) {
  if (length(x) > 0) {
    # need to handle the case of creating NAs from NULL entries so that
    # vec_init() works properly: vec_init requires vec_slice(x, NA_integer_)
    # to give you back NA values, but this breaks because we use lists as proxies.
    # When using a list as a proxy, a proxy entry in `x` that is equal to NULL
    # actually corresponds to an NA value due to the way that list indexing
    # works: when you do something like list()[c(NA_integer_,NA_integer_)]
    # you get back list(NULL, NULL), but when you do something like
    # double()[c(NA_integer_,NA_integer_)] you get back c(NA, NA).
    # So we have to make the NULL values be NA values to mimic vector indexing.

    # N.B. could potentially do this with vec_cast as well (as long as the first
    # dimension is the slicing index)
    x[lengths(x) == 0] <- list(array(NA, dim = c(1,1)))
  }
  # broadcast dimensions and bind together
  new_dim <- dim_common(lapply(x, dim))
  .draws <- abind(lapply(x, broadcast_array, new_dim), along = 1)
  # move draws dimension back to the front
  if (!is.null(.draws)) {
    .draws <- aperm(.draws, c(2, 1, seq_along(dim(.draws))[c(-1,-2)]))
  }

  # determine the number of chains
  nchains_or_null <- lapply(x, function(x) if (dim(x)[[2]] %||% 1 == 1) NULL else attr(x, "nchains"))
  .nchains <- Reduce(nchains2_common, nchains_or_null) %||% 1L

  out <- new_rvar(.draws, .nchains = .nchains)

  # since we've already spent time calculating it, save the proxy in the cache
  attr(out, "cache")$vec_proxy <- x

  out
}

#' @export
vec_restore.rvar_factor = function(x, to, ...) {
  # first, bind the factors as character vectors into an rvar_factor
  x_character <- lapply(x, while_preserving_dims, f = as.character)
  out <- vec_restore.rvar(x_character, ...)
  combine_rvar_factor_levels(out, lapply(x, levels), is_rvar_ordered(to))
  # .draws <- draws_of(out)
  #
  # unique_levels <- unique(lapply(x, levels))
  # if (length(unique_levels) == 1) {
  #   # levels are the same in all variables, so preserve level order when binding
  #   .levels <- unique_levels[[1]]
  #   if (!identical(.levels, levels(out))) {
  #     .draws <- while_preserving_dims(factor, .draws, .levels)
  #   }
  #   if (is_rvar_ordered(to)) {
  #     # only keep the "ordered" class when the levels were all the same (this
  #     # mimics base-R, which demotes to unordered factor when combining ordered
  #     # factors with different levels)
  #     oldClass(.draws) <- c("ordered", "factor")
  #   }
  # }
  # if (!is.factor(.draws)) {
  #   .draws <- while_preserving_dims(factor, .draws)
  # }
  # draws_of(out) <- .draws
  #
  # out
}


# vec_ptype performance generics -------------------------------------------

#' @importFrom vctrs vec_ptype
#' @export
vec_ptype.rvar <- function(x, ..., x_arg = "") new_rvar()
#' @export
vec_ptype.rvar_factor <- function(x, ..., x_arg = "") new_rvar(factor())
#' @export
vec_ptype.rvar_ordered <- function(x, ..., x_arg = "") new_rvar(ordered(NULL))


# identity casts -----------------------------------------------------------

#' @importFrom vctrs vec_ptype2
#' @importFrom vctrs vec_cast
#' @export
vec_ptype2.rvar.rvar <- function(x, y, ...) new_rvar()
#' @export
vec_cast.rvar.rvar <- function(x, to, ...) x

#' @export
vec_ptype2.rvar_factor.rvar_factor <- function(x, y, ...) new_rvar(factor())
#' @export
vec_cast.rvar_factor.rvar_factor <- function(x, to, ...) x

#' @export
vec_ptype2.rvar_ordered.rvar_ordered <- function(x, y, ...) new_rvar(ordered(NULL))
#' @export
vec_cast.rvar_ordered.rvar_ordered <- function(x, to, ...) x


# numeric and logical casts -----------------------------------------------

# double <-> rvar
#' @export
vec_ptype2.double.rvar <- function(x, y, ...) new_rvar()
#' @export
vec_ptype2.rvar.double <- function(x, y, ...) new_rvar()
#' @export
vec_cast.rvar.double <- function(x, to, ...) new_constant_rvar(x)

# integer <-> rvar
#' @export
vec_ptype2.integer.rvar <- function(x, y, ...) new_rvar()
#' @export
vec_ptype2.rvar.integer <- function(x, y, ...) new_rvar()
#' @export
vec_cast.rvar.integer <- function(x, to, ...) new_constant_rvar(x)

# logical <-> rvar
#' @export
vec_ptype2.logical.rvar <- function(x, y, ...) new_rvar()
#' @export
vec_ptype2.rvar.logical <- function(x, y, ...) new_rvar()
#' @export
vec_cast.rvar.logical <- function(x, to, ...) new_constant_rvar(x)

# logical <-> rvar_factor
#' @export
vec_ptype2.logical.rvar_factor <- function(x, y, ...) new_rvar(factor())
#' @export
vec_ptype2.rvar_factor.logical <- function(x, y, ...) new_rvar(factor())
#' @export
vec_cast.rvar_factor.logical <- function(x, to, ...) new_constant_rvar(as.factor(x))


# character casts ---------------------------------------------------------

# rvar -> character
#' @export
vec_cast.character.rvar <- function(x, to, ...) format(x)
#' @export
vec_cast.character.rvar_factor <- function(x, to, ...) format(x)
#' @export
vec_cast.character.rvar_ordered <- function(x, to, ...) format(x)

# character -> rvar
#' @export
vec_cast.rvar.character <- function(x, to, ...) new_constant_rvar(as.factor(x))
#' @export
vec_cast.rvar_factor.character <- function(x, to, ...) new_constant_rvar(as.factor(x))
#' @export
vec_cast.rvar_ordered.character <- function(x, to, ...) new_constant_rvar(as.ordered(x))


# factor casts ---------------------------------------------------------

# factor <-> rvar
#' @export
vec_ptype2.factor.rvar <- function(x, y, ...) new_rvar(factor())
#' @export
vec_ptype2.rvar.factor <- function(x, y, ...) new_rvar(factor())
#' @export
vec_cast.rvar.factor <- function(x, to, ...) new_constant_rvar(x)

# factor <-> rvar_factor
#' @export
vec_ptype2.factor.rvar_factor <- function(x, y, ...) new_rvar(factor())
#' @export
vec_ptype2.rvar_factor.factor <- function(x, y, ...) new_rvar(factor())
#' @export
vec_cast.rvar_factor.factor <- function(x, to, ...) new_constant_rvar(x)

# factor <-> rvar_ordered
#' @export
vec_ptype2.factor.rvar_ordered <- function(x, y, ...) new_rvar(factor())
#' @export
vec_ptype2.rvar_ordered.factor <- function(x, y, ...) new_rvar(factor())
#' @export
vec_cast.rvar_ordered.factor <- function(x, to, ...) new_constant_rvar(x)

# ordered <-> rvar
#' @export
vec_ptype2.ordered.rvar <- function(x, y, ...) new_rvar(ordered())
#' @export
vec_ptype2.rvar.ordered <- function(x, y, ...) new_rvar(ordered())
#' @export
vec_cast.rvar.ordered <- function(x, to, ...) new_constant_rvar(x)

# ordered <-> rvar_factor
#' @export
vec_ptype2.ordered.rvar_factor <- function(x, y, ...) new_rvar(ordered())
#' @export
vec_ptype2.rvar_factor.ordered <- function(x, y, ...) new_rvar(ordered())
#' @export
vec_cast.rvar_factor.ordered <- function(x, to, ...) new_constant_rvar(x)

# ordered <-> rvar_ordered
#' @export
vec_ptype2.ordered.rvar_ordered <- function(x, y, ...) new_rvar(ordered())
#' @export
vec_ptype2.rvar_ordered.ordered <- function(x, y, ...) new_rvar(ordered())
#' @export
vec_cast.rvar_ordered.ordered <- function(x, to, ...) new_constant_rvar(x)


# subtype casts -----------------------------------------------------------

#' @export
vec_cast.rvar_factor.rvar <- function(x, to, ...) .as_rvar_factor_rvar(x)
#' @export
vec_cast.rvar_ordered.rvar <- function(x, to, ...) .as_rvar_factor_rvar(x, as.ordered)
#' @export
vec_cast.rvar_factor.rvar_ordered <- function(x, to, ...) .as_rvar_factor_rvar(x)
#' @export
vec_cast.rvar_ordered.rvar_factor <- function(x, to, ...) .as_rvar_factor_rvar(x, as.ordered)
#' @export
vec_cast.rvar.rvar_ordered <- function(x, to, ...) x
#' @export
vec_cast.rvar.rvar_factor <- function(x, to, ...) x

.as_rvar_factor_rvar <- function(x, as_factor = as.factor) {
  .draws <- draws_of(x)
  factor_draws <- as_factor(.draws)
  dim(factor_draws) <- dim(.draws)
  dimnames(factor_draws) <- dimnames(.draws)
  draws_of(x) <- factor_draws
  x
}


# casting between rvar and distribution objects ---------------------------

#' @export
vec_ptype2.distribution.rvar <- function(x, y, ...) x

#' @export
vec_ptype2.rvar.distribution <- function(x, y, ...) x

#' @export
vec_cast.rvar.distribution <- function(x, to, ..., x_arg = "", to_arg = "") {
  x_list <- vctrs::vec_data(x)
  if (length(dim(to)) > 1 || !is_dist_sample_list(x_list)) {
    vctrs::stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
  }
  x_rvar_list <- lapply(x_list, function(x) rvar(vctrs::field(x, 1)))
  do.call(c, x_rvar_list)
}

#' @export
vec_cast.distribution.rvar <- function(x, to, ..., x_arg = "", to_arg = "") {
  if (length(dim(x)) > 1) {
    vctrs::stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
  }
  x_vector_list <- lapply(vec_proxy(x), as.vector)
  names(x_vector_list) <- names(x)
  distributional::dist_sample(x_vector_list)
}


# helpers: casting --------------------------------------------------------

# create a constant rvar based on x (a double, logical, or integer)
new_constant_rvar <- function(x) {
  out <- x
  dim_x <- dim(x)
  if (length(dim_x) == 0) {
    dim(out) <- c(1, length(x))
  } else {
    dim(out) <- c(1, dim_x)
    dim_i <- seq_along(dim_x)
    out <- copy_dimnames(x, dim_i, out, dim_i + 1)
  }
  new_rvar(out)
}

# is this a list of dist_sample()s?
is_dist_sample_list <- function(x) {
  all(vapply(x, inherits, logical(1), "dist_sample"))
}
