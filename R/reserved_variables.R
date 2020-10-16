#' Reserved variables
#'
#' Names of reserved variables in the \pkg{posterior} package.
#'
#' @name reserved-variables
#'
#' @details
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
#'
NULL

# extract names of reserved variables
reserved_variables <- function(x, ...) {
  UseMethod("reserved_variables")
}

#' @export
reserved_variables.default <- function(x, ...) {
  c(".log_weight")
}

#' @export
reserved_variables.draws_matrix <- function(x, ...) {
  intersect(reserved_variables(), colnames(x))
}

#' @export
reserved_variables.draws_array <- function(x, ...) {
  intersect(reserved_variables(), dimnames(x)[[3]])
}

#' @export
reserved_variables.draws_df <- function(x, ...) {
  intersect(reserved_variables(), names(x))
}

#' @export
reserved_variables.draws_list <- function(x, ...) {
  intersect(reserved_variables(), names(x[[1]]))
}

# reserved variables specific for the 'draws_df' format
reserved_df_variables <- function() {
  c(".chain", ".iteration", ".draw")
}

all_reserved_variables <- function(x = NULL) {
  c(reserved_variables(x), reserved_df_variables())
}

