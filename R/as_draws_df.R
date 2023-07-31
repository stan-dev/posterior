#' The `draws_df` format
#'
#' @name draws_df
#' @family formats
#'
#' @templateVar draws_format draws_df
#' @templateVar base_class class(tibble::tibble())
#' @template draws_format-skeleton
#' @template args-format-nchains
#'
#' @details Objects of class `"draws_df"` are [tibble][tibble::tibble] data
#'   frames. They have one column per variable as well as additional metadata
#'   columns `".iteration"`, `".chain"`, and `".draw"`. The difference between
#'   the `".iteration"` and `".draw"` columns is that the former is relative to
#'   the MCMC chain while the latter ignores the chain information and has all
#'   unique values. See **Examples**.
#'
#'   If a `data.frame`-like object is supplied to `as_draws_df` that contains
#'   columns named `".iteration"` or `".chain"`, they will be treated as
#'   iteration and chain indices, respectively. See **Examples**.
#'
#' @examples
#'
#' # the difference between iteration and draw is clearer when contrasting
#' # the head and tail of the data frame
#' print(head(x1), reserved = TRUE, max_variables = 2)
#' print(tail(x1), reserved = TRUE, max_variables = 2)
#'
#' # manually supply chain information
#' xnew <- data.frame(mu = rnorm(10), .chain = rep(1:2, each = 5))
#' xnew <- as_draws_df(xnew)
#' print(xnew)
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
as_draws_df.data.frame <- function(x, ...) {
  .as_draws_df(x)
}

#' @rdname draws_df
#' @export
as_draws_df.draws_df <- function(x, ...) {
  x
}

#' @rdname draws_df
#' @export
as_draws_df.draws_matrix <- function(x, ...) {
  if (ndraws(x) == 0L) {
    return(empty_draws_df(variables(x)))
  }
  iteration_ids <- iteration_ids(x)
  chain_ids <- chain_ids(x)
  attr(x, "nchains") <- NULL
  x <- tibble::as_tibble(unclass(x))
  x[[".chain"]] <- rep(chain_ids, each = length(iteration_ids))
  x[[".iteration"]] <- rep(iteration_ids, length(chain_ids))
  x[[".draw"]] <- seq_len(nrow(x))
  class(x) <- class_draws_df()
  x
}

#' @rdname draws_df
#' @export
as_draws_df.draws_array <- function(x, ...) {
  if (ndraws(x) == 0L) {
    return(empty_draws_df(variables(x)))
  }
  x <- as_draws_matrix(x)
  as_draws_df(x)
}

#' @rdname draws_df
#' @export
as_draws_df.draws_list <- function(x, ...) {
  if (ndraws(x) == 0L) {
    return(empty_draws_df(variables(x)))
  }
  iteration_ids <- iteration_ids(x)
  chain_ids <- chain_ids(x)
  vars <- names(x[[1L]])
  x <- do.call(vctrs::vec_rbind, lapply(x, vctrs::new_data_frame))
  x <- tibble::as_tibble(x)
  x[[".chain"]] <- rep(chain_ids, each = length(iteration_ids))
  x[[".iteration"]] <- rep(iteration_ids, length(chain_ids))
  x[[".draw"]] <- seq_len(nrow(x))
  class(x) <- class_draws_df()
  x
}

#' @rdname draws_df
#' @export
as_draws_df.draws_rvars <- function(x, ...) {
  if (ndraws(x) == 0L) {
    return(empty_draws_df(variables(x)))
  }
  out <- do.call(cbind, lapply(seq_along(x), function(i) {
    # flatten each rvar so it only has two dimensions: draws and variables
    # this also collapses indices into variable names in the format "var[i,j,k,...]"
    flatten_rvar_draws_to_df(x[[i]], names(x)[[i]])
  }))
  iteration_ids <- iteration_ids(x)
  chain_ids <- chain_ids(x)
  out[[".chain"]] <- rep(chain_ids, each = length(iteration_ids))
  out[[".iteration"]] <- rep(iteration_ids, length(chain_ids))
  out[[".draw"]] <- draw_ids(x)
  class(out) <- class_draws_df()
  out
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

#' Convert any \R object into a `draws_df` object
#' @param x An \R object.
#' @noRd
.as_draws_df <- function(x) {
  x <- tibble::as_tibble(x, .name_repair = "unique")

  # prepare iteration and chain indices
  has_iteration_column <- ".iteration" %in% names(x)
  if (has_iteration_column) {
    iteration_ids <- x[[".iteration"]]
    x[[".iteration"]] <- NULL
  } else {
    iteration_ids <- seq_len(nrow(x))
  }
  has_chain_column <- ".chain" %in% names(x)
  if (has_chain_column) {
    chain_ids <- x[[".chain"]]
    x[[".chain"]] <- NULL
  } else {
    chain_ids <- rep(1L, nrow(x))
  }
  # drop draw indices since they are regenerated below
  x[[".draw"]] <- NULL

  # add reserved variables to the data
  check_new_variables(names(x))
  x[[".chain"]] <- chain_ids
  x[[".iteration"]] <- iteration_ids
  if (has_iteration_column || has_chain_column) {
    x[[".chain"]] <- repair_chain_ids(x[[".chain"]])
    x[[".iteration"]] <- repair_iteration_ids(x[[".iteration"]], x[[".chain"]])
  }
  x[[".draw"]] <- compute_draw_ids(x[[".chain"]], x[[".iteration"]])
  class(x) <- class_draws_df()
  x
}

#' @rdname draws_df
#' @export
draws_df <- function(..., .nchains = 1) {
  out <- validate_draws_per_variable(...)
  .nchains <- as_one_integer(.nchains)
  if (.nchains < 1) {
    stop_no_call("Number of chains must be positive.")
  }
  ndraws <- length(out[[1]])
  if (ndraws %% .nchains != 0) {
    stop_no_call("Number of chains does not divide the number of draws.")
  }
  niterations <- ndraws %/% .nchains
  out <- as.data.frame(out, optional = TRUE)
  out[[".iteration"]] <- rep(seq_len(niterations), .nchains)
  out[[".chain"]] <- rep(seq_len(.nchains), each = niterations)
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
  } else {
    out <- drop_draws_class_if_metadata_removed(out, warn = TRUE)
  }
  out
}

# This generic is not exported here as {dplyr} is only in Suggests, so
# we must export it in .onLoad() for compatibility with R < 3.6.
dplyr_reconstruct.draws_df <- function(data, template) {
  data <- NextMethod("dplyr_reconstruct")
  data <- drop_draws_class_if_metadata_removed(data, warn = FALSE)
  data
}

# drop "draws_df" and "draws" classes if metadata columns were removed
# from the data frame
drop_draws_class_if_metadata_removed <- function(x, warn = TRUE) {
  if (!all(reserved_df_variables() %in% names(x))) {
    if (warn) warning_no_call("Dropping 'draws_df' class as required metadata was removed.")
    class(x) <- setdiff(class(x), c("draws_df", "draws"))
  }
  x
}

# create an empty draws_df object
empty_draws_df <- function(variables = character(0)) {
  assert_character(variables, null.ok = TRUE)
  x <- tibble::tibble()
  x[variables] <- numeric(0)
  x[c(".chain", ".iteration", ".draw")] <- integer(0)
  class(x) <- class_draws_df()
  x
}
