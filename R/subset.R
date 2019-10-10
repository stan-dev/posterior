#' @export
subset.draws_matrix <- function(x, variables = NULL, iterations = NULL,
                                chains = NULL, draws = NULL, ...) {
  if (!is.null(chains)) {
    stop2("Cannot subset 'chains' in 'draws_matrix' objects.")
  }
  if (!is.null(iterations)) {
    if (!is.null(draws)) {
      stop2("Cannot subset 'iterations' and 'draws' at the same time.")
    }
    draws <- iterations
  }
  variables <- unique(variables)
  x <- subset_dims(x, draws, variables)
  rownames(x) <- as.character(seq_rows(x))
  x
}

#' @export
subset.draws_array <- function(x, variables = NULL, iterations = NULL,
                               chains = NULL, draws = NULL, ...) {
  if (!is.null(draws)) {
    stop2("Cannot subset 'draws' in 'draws_array' objects.")
  }
  variables <- unique(variables)
  x <- subset_dims(x, iterations, chains, variables)
  rownames(x) <- as.character(seq_rows(x))
  colnames(x) <- as.character(seq_cols(x))
  x
}

#' @export
subset.draws_data_frame <- function(x, variables = NULL, iterations = NULL,
                                    chains = NULL, draws = NULL, ...) {
  if (!is.null(draws)) {
    if (!is.null(iterations)) {
      stop2("Cannot subset 'iterations' and 'draws' at the same time.")
    }
    if (!is.null(chains)) {
      stop2("Cannot subset 'chains' and 'draws' at the same time.")
    }
  }
  if (!is.null(variables)) {
    variables <- unique(variables)
    x <- x[, c(meta_columns(x), variables)]
  }
  if (!is.null(draws)) {
    x <- x[x$.draw %in% draws, ]
    # subsetting draws invalidates iterations and chains
    x$.draw <- index_continuously(x$.draw)
    x$.iteration <- x$.draw
    x$.chain <- 1L
  } else {
    if (!is.null(chains)) {
      x <- x[x$.chain %in% chains, ]
      x$.chain <- index_continuously(x$.chain)
    }
    if (!is.null(iterations)) {
      x <- x[x$.iteration %in% iterations, ]
      x$.iteration <- index_continuously(x$.iteration)
    }
    if (!is.null(chains) || !is.null(iterations)) {
      x$.draw <- compute_draw_indices(x$.iteration, x$.chain)
    }
  }
  x
}

#' @export
subset_variables <- function(x, variables) {
  check_draws_object(x)
  subset(x, variables = variables)
}

#' @export
subset_iterations <- function(x, iterations) {
  check_draws_object(x)
  subset(x, iterations = iterations)
}

#' @export
subset_chains <- function(x, chains) {
  check_draws_object(x)
  subset(x, chains = chains)
}

#' @export
subset_draws <- function(x, draws) {
  check_draws_object(x)
  subset(x, draws = draws)
}

# subset specified non-NULL dimensions
# @param x an object to be subsetted
# @param ... arguments containing indixes for subsetting a dimension
#   NULL is treated as not subsetting that dimension
subset_dims <- function(x, ...) {
  dots <- list(...)
  if (!length(dots)) {
    return(x)
  }
  dim_x <- max(length(dim(x)), 1L)
  if (length(dots) > dim_x) {
    stop2("'x' has only ", dim_x, " dimensions.")
  }
  if (length(dots) < dim_x) {
    dots <- c(dots, repl(NULL, dim_x - length(dots)))
  }
  names(dots) <- paste0("i", seq_along(dots))
  args <- rep("", length(dots))
  for (i in seq_along(dots)) {
    if (!is.null(dots[[i]])) {
      args[i] <- names(dots)[i]
    }
  }
  args <- paste0(args, collapse = ", ")
  call <- paste0("x[", args, "]")
  dots$x <- x
  eval2(call, dots)
}
