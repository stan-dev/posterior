#' @export
as_draws_list <- function(x, ...) {
  UseMethod("as_draws_list")
}

#' @export
as_draws_list.default <- function(x, ...) {
  x <- as_closest_draws_format(x)
  as_draws_list(x, ...)
}

#' @export
as_draws_list.draws_list <- function(x, ...) {
  x
}

#' @export
as_draws_list.draws_matrix <- function(x, ...) {
  x <- as_draws_data_frame(x)
  as_draws_list(x, ...)
}

#' @export
as_draws_list.draws_array <- function(x, ...) {
  x <- as_draws_data_frame(x)
  as_draws_list(x, ...)
}

#' @export
as_draws_list.draws_data_frame <- function(x, ...) {
  out <- named_list(chains(x))
  x <- x[order(x$.draw), ]
  for (i in seq_along(out)) {
    out[[i]] <- subset(x, chains = i)
    out[[i]] <- remove_meta_columns(out[[i]])
    out[[i]] <- as.list(out[[i]])
  }
  class(out) <- class_draws_list()
  out
}

# try to convert any R object into a 'draws_list' object
.as_draws_list <- function(x) {
  x <- as.list(x)
  # check heuristically if a list of a single chain is supplied
  if (is.numeric(x[[1]])) {
    x <- list(x)
  }
  if (any(!ulapply(x, is.list))) {
    stop2("All list elements must be lists themselves.")
  }
  if (length(unique(lengths(x))) != 1L) {
    stop2("All list elements must have the same length.")
  }
  if (is.null(names(x[[1]]))) {
    # no variable names provided; using default names
    variables <- default_variables(length(x[[1]]))
    for (i in seq_along(x)) {
      names(x[[i]]) <- variables
    }
  }
  variables <- names(x[[1]])
  check_reserved_variables(variables)
  niterations <- length(x[[1]][[1]])
  for (i in seq_along(x)) {
    if (!all(names(x[[i]]) == variables)) {
      stop2("Variables in all chains must have the same names.")
    }
    for (j in seq_along(x[[i]])) {
      if (length(x[[i]][[j]]) != niterations) {
        stop2("All variables in all chains must have the same length.")
      }
    }
  }
  names(x) <- as.character(seq_along(x))
  class(x) <- class_draws_list()
  x
}

class_draws_list <- function() {
  c("draws_list", "draws", "list")
}
