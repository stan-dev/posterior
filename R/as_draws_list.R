#' The `draws_list` format
#'
#' @name draws_list
#' @family formats
#'
#' @templateVar draws_format draws_list
#' @templateVar base_class "list"
#' @template draws_format-skeleton
#' @template args-format-nchains
#'
#' @details Objects of class `"draws_list"` are lists with one element per MCMC
#'   chain. Each of these elements is itself a named list of numeric vectors
#'   with one vector per variable. The length of each vector is equal to the
#'   number of saved iterations per chain. See **Examples**.
#'
NULL

#' @rdname draws_list
#' @export
as_draws_list <- function(x, ...) {
  UseMethod("as_draws_list")
}

#' @rdname draws_list
#' @export
as_draws_list.default <- function(x, ...) {
  x <- as_draws(x)
  as_draws_list(x, ...)
}

#' @rdname draws_list
#' @export
as_draws_list.draws_list <- function(x, ...) {
  x
}

#' @rdname draws_list
#' @export
as_draws_list.draws_matrix <- function(x, ...) {
  x <- as_draws_df(x)
  as_draws_list(x, ...)
}

#' @rdname draws_list
#' @export
as_draws_list.draws_array <- function(x, ...) {
  x <- as_draws_df(x)
  as_draws_list(x, ...)
}

#' @rdname draws_list
#' @export
as_draws_list.draws_df <- function(x, ...) {
  if (ndraws(x) == 0) {
    return(empty_draws_list(variables(x)))
  }
  out <- named_list(chain_ids(x))
  x <- x[order(x$.draw), ]
  for (i in seq_along(out)) {
    out[[i]] <- subset(x, chain = i)
    out[[i]] <- remove_reserved_df_variables(out[[i]])
    out[[i]] <- as.list(out[[i]])
  }
  class(out) <- class_draws_list()
  out
}

#' @rdname draws_list
#' @export
as_draws_list.draws_rvars <- function(x, ...) {
  as_draws_list(as_draws_array(x), ...)
}

#' @rdname draws_list
#' @export
as_draws_list.mcmc <- function(x, ...) {
  as_draws_list(as_draws_matrix(x), ...)
}

#' @rdname draws_list
#' @export
as_draws_list.mcmc.list <- function(x, ...) {
  as_draws_list(as_draws_array(x), ...)
}

# try to convert any R object into a 'draws_list' object
.as_draws_list <- function(x) {
  x <- as.list(x)
  # check heuristically if a list of a single chain is supplied
  if (is.numeric(x[[1]])) {
    x <- list(x)
  }
  if (any(!ulapply(x, is.list))) {
    stop_no_call("All list elements must be lists themselves.")
  }
  if (length(unique(lengths(x))) != 1L) {
    stop_no_call("All list elements must have the same length.")
  }
  if (is.null(names(x[[1]]))) {
    # no variable names provided; using default names
    variables <- default_variables(length(x[[1]]))
    for (i in seq_along(x)) {
      names(x[[i]]) <- variables
    }
  }
  variables <- names(x[[1]])
  check_new_variables(variables)
  niterations <- length(x[[1]][[1]])
  for (i in seq_along(x)) {
    if (!all(names(x[[i]]) == variables)) {
      stop_no_call("Variables in all chains must have the same names.")
    }
    for (j in seq_along(x[[i]])) {
      if (length(x[[i]][[j]]) != niterations) {
        stop_no_call("All variables in all chains must have the same length.")
      }
    }
    # data.frames pass as lists but may not have the same behavior later on
    if (is.data.frame(x[[i]])) {
      x[[i]] <- as.list(x[[i]])
    }
  }
  names(x) <- as.character(seq_along(x))
  class(x) <- class_draws_list()
  x
}

#' @rdname draws_list
#' @export
draws_list <- function(..., .nchains = 1) {
  out <- draws_df(..., .nchains = .nchains)
  as_draws_list(out)
}

class_draws_list <- function() {
  c("draws_list", "draws", "list")
}

#' @rdname draws_list
#' @export
is_draws_list <- function(x) {
  inherits(x, "draws_list")
}

# is an object looking like a 'draws_list' object?
is_draws_list_like <- function(x) {
  # TODO: add more sophisticated checks
  is.list(x)
}

#' @export
`[.draws_list` <- function(x, i) {
  out <- NextMethod("[")
  class(out) <- class(x)
  out
}

# create an empty draws_list object
empty_draws_list <- function(variables = character(0), nchains = 0) {
  assert_character(variables, null.ok = TRUE)
  assert_number(nchains, lower = 0)
  out <- named_list(seq_len(nchains))
  for (i in seq_along(out)) {
    out[[i]] <- named_list(variables, numeric(0))
  }
  class(out) <- class_draws_list()
  out
}
