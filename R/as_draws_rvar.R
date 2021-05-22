#' The `draws_rvar` format
#'
#' @name draws_rvar
#' @family formats
#'
#' @templateVar draws_format draws_rvar
#' @templateVar base_class "list"
#' @template draws_format-skeleton
#' @template args-format-nchains
#'
#' @details Objects of class `"draws_rvar"` are lists of [`rvar`] objects.
#' See **Examples**.
#'
NULL

#' @rdname draws_rvar
#' @export
as_draws_rvar <- function(x, ...) {
  UseMethod("as_draws_rvar")
}

#' @rdname draws_array
#' @export
as_draws_rvar.data.frame <- function(x, ...) {
  x <- as_draws_df(x)
  n_chains <- nchains(x)
  vars <- gsub(pattern = "\\[.+\\]$", replacement = "", variables(x))
  x <- unclass(x)
  x[c(".chain", ".iteration", ".draw")] <- NULL
  x <- split.default(x, vars)
  x <- lapply(x, function(v) {
    r_var <- rvar(unname(do.call(cbind, v)))
    attr(r_var, "nchains") <- n_chains
    r_var
  })
  class(x) <- class_draws_rvar()
  x
}

#' @rdname draws_rvar
#' @export
as_draws_rvar.default <- function(x, ...) {
  x <- as_draws_df(x)
  as_draws_rvar.data.frame(x, ...)
}

#' @rdname draws_rvar
#' @export
as_draws_rvar.draws_rvar <- function(x, ...) {
  x
}

#' @rdname draws_rvar
#' @export
as_draws_rvar.rvar <- function(x, ...) {
  x <- as.list(x)
  if (is.null(names(x))) {
    names(x) <- default_variables(length(x))
  }
  class(x) <- class_draws_rvar()
  x
}

#' @rdname draws_rvar
#' @export
draws_rvar <- function(..., .nchains = 1) {
  x <- lapply(list(...), function(x) {
    if (is_rvar(x)) x
    else rvar(x, nchains = .nchains)
  })
  if (!rlang::is_named(x)) {
    stop_no_call("All variables must be named.")
  }
  x <- conform_rvar_ndraws_nchains(x)
  class(x) <- class_draws_rvar()
  x
}

class_draws_rvar <- function() {
  c("draws_rvar", "draws", "list")
}

#' @rdname draws_rvar
#' @export
is_draws_rvar <- function(x) {
  inherits(x, "draws_rvar")
}

#' @export
`[.draws_rvar` <- function(x, i, j, ..., drop = FALSE) {
  out <- NextMethod("[")
  class(out) <- class(x)
  out
}

# create an empty draws_rvar object
empty_draws_rvar <- function(variables = NULL, nchains = 0) {
  assert_character(variables, null.ok = TRUE)
  assert_number(nchains, lower = 0)
  out <- named_list(variables %||% character(0), rvar())
  class(out) <- class_draws_rvar()
  out
}
