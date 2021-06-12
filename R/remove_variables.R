# remove variables from a draws object
remove_variables <- function(x, ...) {
  UseMethod("remove_variables")
}

#' @export
remove_variables.list <- function(x, variables, ...) {
  variables <- as.character(variables)
  variables  <- intersect(variables, names(x))
  for (v in variables) {
    x[[v]] <- NULL
  }
  x
}

#' @export
remove_variables.draws_matrix <- function(x, variables, ...) {
  variables <- as.character(variables)
  if (!length(variables)) {
    return(x)
  }
  x[, !colnames(x) %in% variables]
}

#' @export
remove_variables.draws_array <- function(x, variables, ...) {
  variables <- as.character(variables)
  if (!length(variables)) {
    return(x)
  }
  x[, , !dimnames(x)[[3]] %in% variables]
}

#' @export
remove_variables.draws_df <- function(x, variables, ...) {
  variables <- as.character(variables)
  for (v in variables) {
    x[[v]] <- NULL
  }
  x
}

#' @export
remove_variables.draws_list <- function(x, variables, ...) {
  variables <- as.character(variables)
  if (!length(variables)) {
    return(x)
  }
  for (i in seq_along(x)) {
    for (v in variables) {
      x[[i]][[v]] <- NULL
    }
  }
  x
}

#' @export
remove_variables.draws_rvars <- function(x, variables, ...) {
  variables <- as.character(variables)
  if (!length(variables)) {
    return(x)
  }
  x[!names(x) %in% variables]
}


# internal ----------------------------------------------------------------

# remove all reserved variables
remove_reserved_variables <- function(x, ...) {
  remove_variables(x, reserved_variables(x))
}

# remove reserved variables specific for the 'draws_df' format
remove_reserved_df_variables <- function(x, ...) {
  remove_variables(x, reserved_df_variables())
}

