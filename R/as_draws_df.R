#' The `draws_df` format
#'
#' @name draws_df
#' @family formats
#'
#' @templateVar draws_format draws_df
#' @templateVar base_class class(tibble::tibble())
#' @template draws_format-skeleton
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
#' x <- as_draws_df(draws_eight_schools)
#' head(x[, c(".iteration", ".chain", ".draw")])
#' tail(x[, c(".iteration", ".chain", ".draw")])
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
as_draws_df.draws_df <- function(x, ...) {
  x
}

#' @rdname draws_df
#' @export
as_draws_df.draws_matrix <- function(x, ...) {
  class(x) <- "matrix"
  draws <- as.integer(rownames(x))
  rownames(x) <- NULL
  x <- as_tibble(x)
  x$.iteration <- draws
  x$.chain <- 1L
  x$.draw <- draws
  x <- move_to_start(x, meta_columns())
  class(x) <- class_draws_df()
  x
}

#' @rdname draws_df
#' @export
as_draws_df.draws_array <- function(x, ...) {
  iterations <- iterations(x)
  chains <- chains(x)
  rownames(x) <- NULL
  out <- named_list(chains)
  for (i in seq_along(out)) {
    out[[i]] <- drop_dims(x[, i, ], dims = 2)
    class(out[[i]]) <- "matrix"
    out[[i]] <- as_tibble(out[[i]])
    out[[i]]$.iteration <- iterations
    out[[i]]$.chain <- chains[i]
    out[[i]]$.draw <- compute_draw_indices(iterations, chains[i])
  }
  out <- do_call(rbind, out)
  out <- move_to_start(out, meta_columns())
  class(out) <- class_draws_df()
  out
}

#' @rdname draws_df
#' @export
as_draws_df.draws_list <- function(x, ...) {
  iterations <- iterations(x)
  chains <- chains(x)
  out <- named_list(chains)
  for (i in seq_along(out)) {
    out[[i]] <- as_tibble(x[[i]])
    out[[i]]$.iteration <- iterations
    out[[i]]$.chain <- chains[i]
    out[[i]]$.draw <- compute_draw_indices(iterations, chains[i])
  }
  out <- do_call(rbind, out)
  out <- move_to_start(out, meta_columns())
  class(out) <- class_draws_df()
  out
}

# try to convert any R object into a 'draws_df' object
.as_draws_df <- function(x) {
  x <- as_tibble(x, .name_repair = "unique")
  check_reserved_variables(names(x))
  # TODO: validate and use existing .iteration and .chain columns
  x$.iteration <- seq_len(NROW(x))
  x$.chain <- 1L
  x$.draw <- compute_draw_indices(x$.iteration, x$.chain)
  x <- move_to_start(x, meta_columns())
  class(x) <- class_draws_df()
  x
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

# meta column names
# @param x a named object from which to extract existing meta column names
meta_columns <- function(x = NULL) {
  out <- c(".iteration", ".chain", ".draw")
  if (!is.null(x)) {
    out <- intersect(out, names(x))
  }
  out
}

# remove meta columns
remove_meta_columns <- function(x) {
  assert_true(is.list(x))
  for (col in meta_columns()) {
    x[[col]] <- NULL
  }
  x
}

# compute index over draws from iteration and chain indices
compute_draw_indices <- function(iterations, chains) {
  niter <- max(iterations)
  (chains - 1) * niter + iterations
}

