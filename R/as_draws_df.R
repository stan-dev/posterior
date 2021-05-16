#' The `draws_df` format
#'
#' @name draws_df
#' @family formats
#'
#' @templateVar draws_format draws_df
#' @templateVar base_class class(tibble::tibble())
#' @template draws_format-skeleton
#' @param .iteration Optional name of a column in the supplied `data.frame`
#' that contain iteration indices. If `NULL` (the default), the `.iteration`
#' column is used if it exists. Otherwise, the input is treated as being
#' index continuously from the first to the last row.
#' @param .chain Optional name of a column in the supplied `data.frame`
#' that contain chain indices. If `NULL` (the default), the `.chain`
#' column is used if it exists. Otherwise, the input is treated as belonging
#' to a single chain.
#' @template args-format-nchains
#'
#' @details Objects of class `"draws_df"` are [tibble][tibble::tibble] data
#'   frames. They have one column per variable as well as additional metadata
#'   columns `".iteration"`, `".chain"`, and `".draw"`. The difference between
#'   the `".iteration"` and `".draw"` columns is that the former is relative to
#'   the MCMC chain while the latter ignores the chain information and has all
#'   unique values. See **Examples**.
#'
#' @examples
#'
#' # the difference between iteration and draw is clearer when contrasting
#' # the head and tail of the data frame
#' print(head(x1), reserved = TRUE, max_variables = 2)
#' print(tail(x1), reserved = TRUE, max_variables = 2)
#'
NULL


#' @rdname draws_df
#' @export
as_draws_df <- function(x, ...) {
  UseMethod("as_draws_df")
}

#' @rdname draws_df
#' @export
as_draws_df.default <- function(x, ...) {
  x <- as_draws(x)
  as_draws_df(x, ...)
}

#' @rdname draws_df
#' @export
as_draws_df.data.frame <- function(x, .iteration = NULL, .chain = NULL, ...) {
  .as_draws_df(x, .iteration = .iteration, .chain = .chain)
}

#' @rdname draws_df
#' @export
as_draws_df.draws_df <- function(x, ...) {
  x
}

#' @rdname draws_df
#' @export
as_draws_df.draws_matrix <- function(x, ...) {
  class(x) <- "matrix"
  draws <- as.integer(rownames(x))
  rownames(x) <- NULL
  x <- tibble::as_tibble(x)
  x$.chain <- rep(1L, nrow(x))
  x$.iteration <- draws
  x$.draw <- draws
  class(x) <- class_draws_df()
  x
}

#' @rdname draws_df
#' @export
as_draws_df.draws_array <- function(x, ...) {
  if (ndraws(x) == 0) {
    return(empty_draws_df(variables(x)))
  }
  iteration_ids <- iteration_ids(x)
  chain_ids <- chain_ids(x)
  rownames(x) <- NULL
  out <- named_list(chain_ids)
  for (i in seq_along(out)) {
    out[[i]] <- drop2(x[, i, ], dims = 2, reset_class = TRUE)
    class(out[[i]]) <- "matrix"
    out[[i]] <- tibble::as_tibble(out[[i]])
    out[[i]]$.chain <- chain_ids[i]
    out[[i]]$.iteration <- iteration_ids
    out[[i]]$.draw <- compute_draw_ids(chain_ids[i], iteration_ids)
  }
  out <- do.call(rbind, out)
  class(out) <- class_draws_df()
  out
}

#' @rdname draws_df
#' @export
as_draws_df.draws_list <- function(x, ...) {
  if (ndraws(x) == 0) {
    return(empty_draws_df(variables(x)))
  }
  iteration_ids <- iteration_ids(x)
  chain_ids <- chain_ids(x)
  out <- named_list(chain_ids)
  for (i in seq_along(out)) {
    out[[i]] <- tibble::as_tibble(x[[i]])
    out[[i]]$.chain <- chain_ids[i]
    out[[i]]$.iteration <- iteration_ids
    out[[i]]$.draw <- compute_draw_ids(chain_ids[i], iteration_ids)
  }
  out <- do.call(rbind, out)
  class(out) <- class_draws_df()
  out
}

#' @rdname draws_df
#' @export
as_draws_df.draws_rvars <- function(x, ...) {
  as_draws_df(as_draws_array(x), ...)
}

#' @rdname draws_df
#' @export
as_draws_df.mcmc <- function(x, ...) {
  as_draws_df(as_draws_matrix(x), ...)
}

#' @rdname draws_df
#' @export
as_draws_df.mcmc.list <- function(x, ...) {
  as_draws_df(as_draws_array(x), ...)
}

#' Convert any \R object into a \code{draws_df} object
#' @param x An \R object.
#' @param .iteration optional name of the column containing iteration indices
#' @param .chain optional name of the column containing chain indices
#' @noRd
.as_draws_df <- function(x, .iteration = NULL, .chain = NULL) {
  x <- tibble::as_tibble(x, .name_repair = "unique")

  # prepare iteration indices
  if (!is.null(.iteration)) {
    .iteration <- as_one_character(.iteration)
    has_iteration_column <- .iteration %in% names(x)
    if (!has_iteration_column) {
      stop2("Iteration indicator '", .iteration, "' cannot be found.")
    }
  } else {
    .iteration <- ".iteration"
    has_iteration_column <- .iteration %in% names(x)
  }
  if (has_iteration_column) {
    iteration_ids <- x[[.iteration]]
    x[[.iteration]] <- NULL
  } else {
    iteration_ids <- seq_len(NROW(x))
  }
  # prepare chain indices
  if (!is.null(.chain)) {
    .chain <- as_one_character(.chain)
    has_chain_column <- .chain %in% names(x)
    if (!has_chain_column) {
      stop2("Chain indicator '", .chain, "' cannot be found.")
    }
  } else {
    .chain <- ".chain"
    has_chain_column <- .chain %in% names(x)
  }
  if (has_chain_column) {
    chain_ids <- x[[.chain]]
    x[[.chain]] <- NULL
  } else {
    chain_ids <- rep(1L, NROW(x))
  }
  # prepare draw indices --- i.e. drop them, since they are regenerated below
  x[[".draw"]] <- NULL

  # add reserved variables to the data
  check_new_variables(names(x))
  x$.chain <- chain_ids
  x$.iteration <- iteration_ids
  if (has_iteration_column || has_chain_column) {
    x$.chain <- repair_chain_ids(x$.chain)
    x$.iteration <- repair_iteration_ids(x$.iteration, x$.chain)
  }
  x$.draw <- compute_draw_ids(x$.chain, x$.iteration)
  class(x) <- class_draws_df()
  x
}

#' @rdname draws_df
#' @export
draws_df <- function(..., .nchains = 1) {
  out <- validate_draws_per_variable(...)
  .nchains <- as_one_integer(.nchains)
  if (.nchains < 1) {
    stop2("Number of chains must be positive.")
  }
  ndraws <- length(out[[1]])
  if (ndraws %% .nchains != 0) {
    stop2("Number of chains does not divide the number of draws.")
  }
  niterations <- ndraws %/% .nchains
  out <- as.data.frame(out, optional = TRUE)
  out$.iteration <- rep(1L:niterations, .nchains)
  out$.chain <- rep(1L:.nchains, each = niterations)
  as_draws_df(out)
}

class_draws_df <- function() {
  # inherits for tibbles
  c("draws_df", "draws", "tbl_df", "tbl", "data.frame")
}

#' @rdname draws_df
#' @export
is_draws_df <- function(x) {
  inherits(x, "draws_df")
}

# is an object looking like a 'draws_df' object?
is_draws_df_like <- function(x) {
  is.data.frame(x)
}

#' @export
`[.draws_df` <- function(x, i, j, drop = FALSE, ..., reserved = FALSE) {
  reserved <- as_one_logical(reserved)
  # draws_df is a tibble so drop = FALSE by default anyway
  out <- NextMethod("[")
  if (reserved) {
    reserved_vars <- all_reserved_variables(x)
    reserved_vars <- setdiff(reserved_vars, names(out))
    out[, reserved_vars] <- NextMethod("[", j = reserved_vars, drop = FALSE)
  } else if (!all(reserved_df_variables() %in% names(out))) {
    warning2("Dropping 'draws_df' class as required metadata was removed.")
    class(out) <- setdiff(class(out), c("draws_df", "draws"))
  }
  out
}

# create an empty draws_df object
empty_draws_df <- function(variables = character(0)) {
  assert_character(variables, null.ok = TRUE)
  out <- tibble::tibble()
  for (v in variables) {
    out[[v]] <- numeric(0)
  }
  out$.chain <- integer(0)
  out$.iteration <- integer(0)
  out$.draw <- integer(0)
  class(out) <- class_draws_df()
  out
}
