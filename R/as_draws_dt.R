# set this to TRUE so that data.table:::cedta() will treat {posterior} as a
# data.table-aware package, then re-implement methods as necessary to use
# data.table-style indexing with `[`
.datatable.aware = FALSE

#' The `draws_dt` format
#'
#' @name draws_dt
#' @family formats
#'
#' @templateVar draws_format draws_dt
#' @templateVar base_class class(data.table::data.table())
#' @template draws_format-skeleton
#' @template args-format-nchains
#'
#' @details Objects of class `"draws_dt"` are [data.table][data.table::data.table]s.
#'   They have one column per variable as well as additional metadata
#'   columns `".iteration"`, `".chain"`, and `".draw"`. The difference between
#'   the `".iteration"` and `".draw"` columns is that the former is relative to
#'   the MCMC chain while the latter ignores the chain information and has all
#'   unique values. See **Examples**.
#'
#'   If a `data.table`-like object is supplied to `as_draws_dt` that contains
#'   columns named `".iteration"` or `".chain"`, they will be treated as
#'   iteration and chain indices, respectively. See **Examples**.
#'
#' @examples
#'
#' # the difference between iteration and draw is clearer when contrasting
#' # the head and tail of the data frame
#' print(head(x1), reserved = TRUE, max_variables = 2)
#' print(tail(x1), reserved = TRUE, max_variables = 2)
#'
#' # manually supply chain information
#' xnew <- data.table(mu = rnorm(10), .chain = rep(1:2, each = 5))
#' xnew <- as_draws_dt(xnew)
#' print(xnew)
#'
NULL


#' @rdname draws_dt
#' @export
as_draws_dt <- function(x, ...) {
  UseMethod("as_draws_dt")
}

#' @rdname draws_dt
#' @export
as_draws_dt.default <- function(x, ...) {
  x <- as_draws(x)
  as_draws_dt(x, ...)
}

#' @rdname draws_dt
#' @export
as_draws_dt.data.frame <- function(x, ...) {
  .as_draws_dt(x)
}

#' @rdname draws_dt
#' @export
as_draws_dt.draws_dt <- function(x, ...) {
  x
}

#' @rdname draws_dt
#' @export
as_draws_dt.draws_df <- function(x, ...) {
  class(x) <- class_draws_dt()
  x
}

#' @rdname draws_dt
#' @export
as_draws_dt.draws_matrix <- function(x, ...) {
  as_draws_dt(as_draws_df(x), ...)
}

#' @rdname draws_dt
#' @export
as_draws_dt.draws_array <- function(x, ...) {
  as_draws_dt(as_draws_df(x), ...)
}

#' @rdname draws_dt
#' @export
as_draws_dt.draws_list <- function(x, ...) {
  as_draws_dt(as_draws_df(x), ...)
}

#' @rdname draws_dt
#' @export
as_draws_dt.draws_rvars <- function(x, ...) {
  as_draws_dt(as_draws_df(x), ...)
}

#' @rdname draws_dt
#' @export
as_draws_dt.mcmc <- function(x, ...) {
  as_draws_dt(as_draws_matrix(x), ...)
}

#' @rdname draws_df
#' @export
as_draws_df.mcmc.list <- function(x, ...) {
  as_draws_dt(as_draws_array(x), ...)
}

#' Convert any \R object into a `draws_dt` object
#' @param x An \R object.
#' @noRd
.as_draws_dt <- function(x) {
  x <- .as_draws_df(x)
  class(x) <- class_draws_dt()
  x
}

#' @rdname draws_dt
#' @export
draws_dt <- function(..., .nchains = 1) {
  as_draws_dt(draws_df(..., .nchains = .nchains))
}

class_draws_dt <- function() {
  c("draws_dt", "draws_df", "draws", "data.table", "data.frame")
}

#' @rdname draws_dt
#' @export
is_draws_dt <- function(x) {
  inherits(x, "draws_dt")
}

# is an object looking like a 'draws_dt' object?
is_draws_dt_like <- function(x) {
  is.data.table(x)
}

#' @export
`[.draws_dt` <- function(x, ..., drop = FALSE, reserved = FALSE) {
  reserved <- as_one_logical(reserved)
  # data.table uses heuristics to pick if a symbol is evaluated in the calling
  # context or in the data frame context; thus we have to reconstruct the
  # calling expression (but without non-data.table arguments like `reserved`)
  # and evaluate it in the calling context (data.table ignores `drop`)
  subset_expr = substitute(data.table::`[.data.table`(x, ...))
  out <- eval(subset_expr, envir = parent.frame())
  if (reserved) {
    reserved_vars <- all_reserved_variables(x)
    reserved_vars <- setdiff(reserved_vars, names(out))
    out[, reserved_vars] <- data.table::`[.data.table`(x, , reserved_vars)
  } else {
    out <- drop_draws_class_if_metadata_removed(out, warn = TRUE)
  }
  out
}

# create an empty draws_dt object
empty_draws_dt <- function(variables = character(0)) {
  as_draws_dt(empty_draws_df(variables))
}
