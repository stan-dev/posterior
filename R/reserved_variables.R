#' Reserved variables
#'
#' Get names of reserved variables from objects in the \pkg{posterior} package.
#'
#' @name reserved_variables
#' @template args-methods-x
#' @template args-methods-dots
#' @details
#'
#' `reserved_variables()` returns the names of reserved variables in use by
#' an object.
#'
#' The following variables names are currently reserved for special use cases
#' in all [`draws`] formats:
#' * `.log_weight`: Log weights per draw (see [`weight_draws`]).
#'
#' Further, specific for the [`draws_df`] format, there are three additional
#' reserved variables:
#' * `.chain`: Chain index per draw
#' * `.iteration`: Iteration index within each chain
#' * `.draw`: Draw index across chains
#'
#' More reserved variables may be added in the future.
#'
#' @return
#'
#' A character vector of reserved variables used in `x`.
#'
#' @examples
#'
#' x <- example_draws()
#' reserved_variables(x)
#'
#' # if we add weights, the `.log_weight` reserved variable is used
#' x <- weight_draws(x, rexp(ndraws(x)))
#' reserved_variables(x)
#'
#' @export
reserved_variables <- function(x, ...) {
  UseMethod("reserved_variables")
}

#' @rdname reserved_variables
#' @export
reserved_variables.default <- function(x, ...) {
  c(".log_weight")
}

#' @rdname reserved_variables
#' @export
reserved_variables.draws_matrix <- function(x, ...) {
  intersect(reserved_variables(), colnames(x))
}

#' @rdname reserved_variables
#' @export
reserved_variables.draws_array <- function(x, ...) {
  intersect(reserved_variables(), dimnames(x)[[3]])
}

#' @rdname reserved_variables
#' @export
reserved_variables.draws_df <- function(x, ...) {
  intersect(reserved_variables(), names(x))
}

#' @rdname reserved_variables
#' @export
reserved_variables.draws_list <- function(x, ...) {
  intersect(reserved_variables(), names(x[[1]]))
}

#' @rdname reserved_variables
#' @export
reserved_variables.draws_rvars <- function(x, ...) {
  intersect(reserved_variables(), names(x))
}


# internal ----------------------------------------------------------------

# reserved variables specific for the 'draws_df' format
reserved_df_variables <- function() {
  c(".chain", ".iteration", ".draw")
}

all_reserved_variables <- function(x = NULL) {
  c(reserved_variables(x), reserved_df_variables())
}
