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
#' While `as_rvar()` attempts to pick the most suitable subtype of [`rvar`] based on the
#' type of `x` (possibly returning an [`rvar_factor`] or [`rvar_ordered`]),
#' `as_rvar_numeric()`, `as_rvar_integer()`, and `as_rvar_logical()` always coerce
#' the draws of the output [`rvar`] to be [`numeric`], [`integer`], or [`logical`]
#' (respectively), and always return a base [`rvar`], never a subtype.
#'
#' @seealso [rvar()] to construct [`rvar`]s directly.  See [rdo()], [rfun()], and
#' [rvar_rng()] for higher-level interfaces for creating `rvar`s.
#'
#' @return An object of class `"rvar"` (or one of its subtypes) representing a random variable.
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
#' # as_rvar_numeric() coerces subtypes of rvar to the base rvar type
#' y <- as_rvar_factor(c("a", "b", "c"))
#' y
#' as_rvar_numeric(y)
#'
#' @export
as_rvar <- function(x, dim = NULL, dimnames = NULL, nchains = NULL) {
  .as_rvar(x, dim = dim, dimnames = dimnames, nchains = nchains)
}
.as_rvar <- function(x, dim = NULL, dimnames = NULL, nchains = NULL, ptype = new_rvar()) {
  out <- x

  if (is.null(out)) {
    out <- rvar()
  } else {
    out <- vec_cast(out, ptype)
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

#' @rdname as_rvar
#' @export
as_rvar_numeric <- function(x, dim = NULL, dimnames = NULL, nchains = NULL) {
  out <- as_rvar(x, dim = dim, dimnames = dimnames, nchains = nchains)
  draws_of(out) <- while_preserving_dims(as.numeric, draws_of(out))
  out
}

#' @rdname as_rvar
#' @export
as_rvar_integer <- function(x, dim = NULL, dimnames = NULL, nchains = NULL) {
  out <- as_rvar(x, dim = dim, dimnames = dimnames, nchains = nchains)
  draws_of(out) <- while_preserving_dims(as.integer, draws_of(out))
  out
}

#' @rdname as_rvar
#' @export
as_rvar_logical <- function(x, dim = NULL, dimnames = NULL, nchains = NULL) {
  out <- as_rvar(x, dim = dim, dimnames = dimnames, nchains = nchains)
  draws_of(out) <- while_preserving_dims(as.logical, draws_of(out))
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
  x_dim <- dim(x)

  if (length(x_dim) >= 2) {
    is <- seq_len(x_dim[[1]])
    names(is) <- dimnames(x)[[1]]
    out <- lapply(is, function(i) {
      out_i <- x[i,]
      .dim <- dim(out_i)
      .dimnames <- dimnames(out_i)
      dim(out_i) <- .dim[-1]
      dimnames(out_i) <- .dimnames[-1]
      out_i
    })
  } else {
    is <- seq_along(x)
    names(is) <- dimnames(x)[[1]]
    out <- lapply(is, function(i) x[[i]])
  }
  out
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

#' @importFrom vctrs vec_proxy
#' @export
vec_proxy.rvar = function(x, ...) {
  # Using caching to help with algorithms that call vec_proxy
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
make_rvar_proxy = function(x) {
  nchains <- nchains(x)
  draws <- draws_of(x)
  is <- seq_len(NROW(x))
  names(is) <- rownames(x)
  lapply(is, function(i) {
    list(
      index = i,
      nchains = nchains,
      draws = draws
    )
  })
}


#' @importFrom vctrs vec_restore
#' @export
vec_restore.rvar <- function(x, ...) {
  if (length(x) == 0) return(rvar())

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
  x[lengths(x) == 0] <- make_rvar_proxy(new_rvar(NA_real_))

  # find runs where the same underlying draws are in the proxy
  different_draws_from_previous <- vapply(seq_along(x)[-1], FUN.VALUE = logical(1), function(i) {
    !identical(x[[i]]$draws, x[[i - 1]]$draws) || !identical(x[[i]]$nchains, x[[i - 1]]$nchains)
  })
  draws_groups <- cumsum(c(TRUE, different_draws_from_previous))

  # convert each run into a slice on an rvar and bind the resulting rvars together
  groups <- split(x, draws_groups)
  rvars <- lapply(groups, function(x) {
    i <- vapply(x, `[[`, "index", FUN.VALUE = numeric(1))
    rvar <- new_rvar(x[[1]]$draws, .nchains = x[[1]]$nchains)
    if (length(dim(rvar)) > 1) {
      rvar[i, ]
    } else {
      rvar <- rvar[i]
      .dimnames <- dimnames(rvar)
      dim(rvar) <- c(length(rvar), 1)
      dimnames(rvar) <- c(.dimnames, NULL)
      rvar
    }
  })
  out <- bind_rvars(rvars, arg_exprs = NULL, deparse.level = 0, axis = 1)

  if (all(lengths(lapply(groups, function(x) dim(x[[1]]$draws))) <= 2)) {
    # input was a bunch of vectors, ensure output is also a vector
    .dimnames <- dimnames(out)
    dim(out) <- length(out)
    dimnames(out) <- .dimnames[1]
  }

  # since we've already spent time calculating it, save the proxy in the cache
  # - but only if the proxy only has one group (else we'd have to recalculate
  # the bind above again, which is usually more expensive than generating the
  # proxy itself)
  if (length(groups) == 1) {
    attr(out, "cache")$vec_proxy <- x
  }

  out
}

#' @export
vec_restore.rvar_factor = function(x, to, ...) {
  x[lengths(x) == 0] <- make_rvar_proxy(rvar_factor(NA_integer_))
  vec_restore.rvar(x, ...)
}

#' @export
vec_restore.rvar_ordered = function(x, to, ...) {
  x[lengths(x) == 0] <- make_rvar_proxy(rvar_ordered(NA_integer_))
  vec_restore.rvar(x, ...)
}


# vctrs comparison proxies ------------------------------------------------

#' @importFrom vctrs vec_proxy_equal
#' @export
vec_proxy_equal.rvar = function(x, ...) {
  # Using caching to help with algorithms that call vec_proxy_equal
  # repeatedly. See https://github.com/r-lib/vctrs/issues/1411

  out <- attr(x, "cache")$vec_proxy_equal
  if (is.null(out)) {
    # proxy is not in the cache, calculate it and store it in the cache
    out <- make_rvar_proxy_equal(x)
    attr(x, "cache")$vec_proxy_equal <- out
  }

  out
}

#' Make a cacheable proxy for vec_proxy_equal.rvar
#' @noRd
make_rvar_proxy_equal = function(x) {
  lapply(as.list(x), function(x) list(
    nchains = nchains(x),
    draws = draws_of(x)
  ))
}

#' @importFrom vctrs vec_proxy_compare
#' @export
vec_proxy_compare.rvar = function(x, ...) {
  stop_no_call("rvar does not support vctrs::vec_compare()")
}

#' @importFrom vctrs vec_proxy_order
#' @export
vec_proxy_order.rvar = function(x, ...) {
  stop_no_call("rvar does not support vctrs::vec_order()")
}


# vec_ptype performance generics -------------------------------------------

#' @importFrom vctrs vec_ptype
#' @export
vec_ptype.rvar <- function(x, ..., x_arg = "") new_rvar()
#' @export
vec_ptype.rvar_factor <- function(x, ..., x_arg = "") new_rvar(factor())
#' @export
vec_ptype.rvar_ordered <- function(x, ..., x_arg = "") new_rvar(ordered(NULL, levels = levels(x)))


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

# double -> rvar
#' @export
vec_ptype2.double.rvar <- function(x, y, ...) new_rvar()
#' @export
vec_ptype2.rvar.double <- function(x, y, ...) new_rvar()
#' @export
vec_cast.rvar.double <- function(x, to, ...) new_constant_rvar(x)

# double -> rvar_factor
#' @export
vec_cast.rvar_factor.double <- function(x, to, ...) new_constant_rvar(while_preserving_dims(as.factor, x))

# double -> rvar_ordered
#' @export
vec_cast.rvar_ordered.double <- function(x, to, ...) new_constant_rvar(while_preserving_dims(as.ordered, x))

# integer -> rvar
#' @export
vec_ptype2.integer.rvar <- function(x, y, ...) new_rvar()
#' @export
vec_ptype2.rvar.integer <- function(x, y, ...) new_rvar()
#' @export
vec_cast.rvar.integer <- function(x, to, ...) new_constant_rvar(x)

# integer -> rvar_factor
#' @export
vec_cast.rvar_factor.integer <- function(x, to, ...) new_constant_rvar(while_preserving_dims(as.factor, x))

# integer -> rvar_ordered
#' @export
vec_cast.rvar_ordered.integer <- function(x, to, ...) new_constant_rvar(while_preserving_dims(as.ordered, x))

# logical -> rvar
#' @export
vec_ptype2.logical.rvar <- function(x, y, ...) new_rvar()
#' @export
vec_ptype2.rvar.logical <- function(x, y, ...) new_rvar()
#' @export
vec_cast.rvar.logical <- function(x, to, ...) new_constant_rvar(x)

# logical -> rvar_factor
#' @export
vec_cast.rvar_factor.logical <- function(x, to, ...) new_constant_rvar(while_preserving_dims(as.factor, x))

# logical -> rvar_ordered
#' @export
vec_cast.rvar_ordered.logical <- function(x, to, ...) new_constant_rvar(while_preserving_dims(as.ordered, x))


# character casts ---------------------------------------------------------

# rvar_[factor|ordered] -> character
#' @export
vec_cast.character.rvar <- function(x, to, ...) format(x)
#' @export
vec_cast.character.rvar_factor <- function(x, to, ...) format(x)
#' @export
vec_cast.character.rvar_ordered <- function(x, to, ...) format(x)

# character -> rvar
#' @export
vec_cast.rvar.character <- function(x, to, ...) new_constant_rvar(while_preserving_dims(as.factor, x))

# character -> rvar_factor
#' @export
vec_ptype2.character.rvar_factor <- function(x, y, ...) new_rvar(factor())
#' @export
vec_ptype2.rvar_factor.character <- function(x, y, ...) new_rvar(factor())
#' @export
vec_cast.rvar_factor.character <- function(x, to, ...) new_constant_rvar(while_preserving_dims(as.factor, x))

# character -> rvar_ordered
#' @export
vec_ptype2.character.rvar_ordered <- function(x, y, ...) rvar_ordered(levels = levels(y))
#' @export
vec_ptype2.rvar_ordered.character <- function(x, y, ...) rvar_ordered(levels = levels(x))
#' @export
vec_cast.rvar_ordered.character <- function(x, to, ...) {
  old_levels <- levels(to)
  new_levels <- sort(setdiff(x, levels(to)))
  levels <- c(old_levels, new_levels)
  ordered <- length(new_levels) == 0
  new_constant_rvar(while_preserving_dims(factor, x, levels = levels, ordered = ordered))
}


# factor casts ---------------------------------------------------------

# factor -> rvar
#' @export
vec_cast.rvar.factor <- function(x, to, ...) new_constant_rvar(x)

# factor -> rvar_factor
#' @export
vec_ptype2.factor.rvar_factor <- function(x, y, ...) new_rvar(factor())
#' @export
vec_ptype2.rvar_factor.factor <- function(x, y, ...) new_rvar(factor())
#' @export
vec_cast.rvar_factor.factor <- function(x, to, ...) new_constant_rvar(x)

# factor -> rvar_ordered
#' @export
vec_ptype2.factor.rvar_ordered <- function(x, y, ...) new_rvar(factor())
#' @export
vec_ptype2.rvar_ordered.factor <- function(x, y, ...) new_rvar(factor())
#' @export
vec_cast.rvar_ordered.factor <- function(x, to, ...) new_constant_rvar(x)

# ordered -> rvar
#' @export
vec_cast.rvar.ordered <- function(x, to, ...) new_constant_rvar(x)

# ordered -> rvar_factor
#' @export
vec_ptype2.ordered.rvar_factor <- function(x, y, ...) new_rvar(ordered(NULL))
#' @export
vec_ptype2.rvar_factor.ordered <- function(x, y, ...) new_rvar(ordered(NULL))
#' @export
vec_cast.rvar_factor.ordered <- function(x, to, ...) new_constant_rvar(x)

# ordered -> rvar_ordered
#' @export
vec_ptype2.ordered.rvar_ordered <- function(x, y, ...) new_rvar(ordered(NULL))
#' @export
vec_ptype2.rvar_ordered.ordered <- function(x, y, ...) new_rvar(ordered(NULL))
#' @export
vec_cast.rvar_ordered.ordered <- function(x, to, ...) new_constant_rvar(x)


# subtype casts -----------------------------------------------------------

#' @export
vec_cast.rvar_factor.rvar <- function(x, to, ...) .rvar_to_rvar_factor(x)
#' @export
vec_cast.rvar_ordered.rvar <- function(x, to, ...) .rvar_to_rvar_factor(x, ordered = TRUE)
#' @export
vec_cast.rvar_factor.rvar_ordered <- function(x, to, ...) .rvar_to_rvar_factor(x)
#' @export
vec_cast.rvar_ordered.rvar_factor <- function(x, to, ...) .rvar_to_rvar_factor(x, ordered = TRUE)
#' @export
vec_cast.rvar.rvar_ordered <- function(x, to, ...) x
#' @export
vec_cast.rvar.rvar_factor <- function(x, to, ...) x

.rvar_to_rvar_factor <- function(x, ordered = FALSE, ...) {
  if (
    ...length() == 0 &&
    ((ordered && is_rvar_ordered(x)) || (!ordered && is_rvar_factor(x)))
  ) {
    # already correct type and nothing is being passed to factor() to change it
    return(x)
  }

  draws_of(x) <- while_preserving_dims(factor, draws_of(x), ordered = ordered, ...)
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
  .draws <- draws_of(x)
  x_array_list <- vctrs::vec_chop(aperm(.draws, c(2, 1, seq_along(dim(.draws))[c(-1,-2)])))
  x_vector_list <- lapply(x_array_list, as.vector)
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
