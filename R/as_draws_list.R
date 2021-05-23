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
as_draws_list.data.frame <- function(x, ...) {
  x <- as_draws_df(x)
  if (ndraws(x) == 0) {
    return(empty_draws_list(variables(x)))
  }
  class(x) <- "data.frame"
  .chain <- x[[".chain"]]
  x[c(".chain", ".iteration", ".draw")] <- NULL
  x <- split(x, .chain)
  x <- lapply(x, as.list)
  class(x) <- class_draws_list()
  x
}

#' @rdname draws_list
#' @export
as_draws_list.default <- function(x, ...) {
  x <- as_draws_df(x)
  as_draws_list.data.frame(x, ...)
}

#' @rdname draws_list
#' @export
as_draws_list.draws_list <- function(x, ...) {
  x
}

#' @rdname draws_list
#' @export
as_draws_list.draws_rvars <- function(x, ...) {
  x <- as_draws_df(x)
  as_draws_list.data.frame(x, ...)
}

#' @rdname draws_list
#' @export
as_draws_list.list <- function(x, ...) {
  if (is.numeric(x[[1]])) {
    x <- list(x)
  }
  x <- lapply(x, function(chain) {
    if (is.null(names(chain))) {
      names(chain) <- default_variables(length(chain))
    }
    chain
  })
  class(x) <- class_draws_list()
  x
}

#' @rdname draws_list
#' @export
as_draws_list.vector <- function(x, ...) {
  x <- list(list(x))
  x <- lapply(x, function(chain) {
    if (is.null(names(chain))) {
      names(chain) <- default_variables(length(chain))
    }
    chain
  })
  class(x) <- class_draws_list()
  x
}

#' @rdname draws_list
#' @export
as_draws_list.rvar <- function(x, ...) {
  x <- as_draws_rvars.rvar(x)
  as_draws_list.draws_rvars(x)
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

#' @export
`[.draws_list` <- function(x, i) {
  out <- NextMethod("[")
  class(out) <- class(x)
  out
}

# create an empty draws_list object
empty_draws_list <- function(variables = NULL,
                             nchains = 0) {
  assert_character(variables, null.ok = TRUE)
  assert_number(nchains, lower = 0)
  out <- named_list(seq_len(nchains))
  for (i in seq_along(out)) {
    out[[i]] <- named_list(variables %||% character(0), numeric(0))
  }
  class(out) <- class_draws_list()
  out
}
