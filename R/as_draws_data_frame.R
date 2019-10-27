#' @import tibble
#' @export
as_draws_data_frame <- function(x, ...) {
  UseMethod("as_draws_data_frame")
}

#' @export
as_draws_data_frame.default <- function(x, ...) {
  x <- as_closest_draws_format(x)
  as_draws_data_frame(x, ...)
}

#' @export
as_draws_data_frame.draws_data_frame <- function(x, ...) {
  x
}

#' @export
as_draws_data_frame.draws_matrix <- function(x, ...) {
  class(x) <- "matrix"
  draws <- as.integer(rownames(x))
  rownames(x) <- NULL
  x <- as_tibble(x)
  x$.iteration <- draws
  x$.chain <- 1L
  x$.draw <- draws
  x <- move_to_start(x, meta_columns())
  class(x) <- c("draws_data_frame", class(x))
  x
}

#' @export
as_draws_data_frame.draws_array <- function(x, ...) {
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
  class(out) <- c("draws_data_frame", class(out))
  out
}

#' @export
as_draws_data_frame.draws_list <- function(x, ...) {
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
  class(out) <- c("draws_data_frame", class(out))
  out
}

# try to convert any R object into a 'draws_data_frame' object
.as_draws_data_frame <- function(x) {
  x <- as_tibble(x, .name_repair = "unique")
  check_reserved_variables(names(x))
  # TODO: validate and use existing .iteration and .chain columns
  x$.iteration <- seq_len(NROW(x))
  x$.chain <- 1L
  x$.draw <- compute_draw_indices(x$.iteration, x$.chain)
  x <- move_to_start(x, meta_columns())
  class(x) <- c("draws_data_frame", class(x))
  x
}
