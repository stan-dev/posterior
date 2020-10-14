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

# remove reserved variables from an object
remove_reserved_variables <- function(x, ...) {
  UseMethod("remove_reserved_variables")
}

#' @export
remove_reserved_variables.list <- function(x, reserved = NULL, ...) {
  if (is.null(reserved)) {
    reserved <- reserved_variables()
  }
  reserved  <- intersect(reserved, names(x))
  for (v in reserved) {
    x[[v]] <- NULL
  }
  x
}

#' @export
remove_reserved_variables.draws_matrix <- function(x, reserved = NULL, ...) {
  if (is.null(reserved)) {
    reserved <- reserved_variables(x)
  }
  if (!length(reserved)) {
    return(x)
  }
  x[, !colnames(x) %in% reserved]
}

#' @export
remove_reserved_variables.draws_array <- function(x, reserved = NULL, ...) {
  if (is.null(reserved)) {
    reserved <- reserved_variables(x)
  }
  if (!length(reserved)) {
    return(x)
  }
  x[, , !dimnames(x)[[3]] %in% reserved]
}

#' @export
remove_reserved_variables.draws_df <- function(x, reserved = NULL, ...) {
  if (is.null(reserved)) {
    reserved <- reserved_variables(x)
  }
  for (v in reserved) {
    x[[v]] <- NULL
  }
  x
}

#' @export
remove_reserved_variables.draws_list <- function(x, reserved = NULL, ...) {
  if (is.null(reserved)) {
    reserved <- reserved_variables(x)
  }
  if (!length(reserved)) {
    return(x)
  }
  for (i in seq_along(x)) {
    for (v in reserved) {
      x[[i]][[v]] <- NULL
    }
  }
  x
}

all_reserved_variables <- function(x = NULL) {
  c(reserved_variables(x), reserved_df_variables())
}

# reserved variables specific for the 'draws_df' format
reserved_df_variables <- function() {
  c(".chain", ".iteration", ".draw")
}

# remove reserved variables specific for the 'draws_df' format
remove_reserved_df_variables <- function(x, ...) {
  remove_reserved_variables(x, reserved_df_variables())
}
