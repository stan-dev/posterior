#' @export
subset.draws_matrix <- function(x, variables = NULL, iterations = NULL,
                                chains = NULL, draws = NULL, ...) {
  x <- repair_indices(x)
  variables <- check_variables(variables, x)
  iterations <- check_iterations(iterations, x)
  draws <- check_draws(draws, x)
  if (!is.null(chains)) {
    stop2("Cannot subset 'chains' in 'draws_matrix' objects.")
  }
  if (!is.null(iterations)) {
    if (!is.null(draws)) {
      stop2("Cannot subset 'iterations' and 'draws' at the same time.")
    }
    draws <- iterations
  }
  x <- subset_dims(x, draws, variables)
  x <- repair_indices(x)
  x
}

#' @export
subset.draws_array <- function(x, variables = NULL, iterations = NULL,
                               chains = NULL, draws = NULL, ...) {
  x <- repair_indices(x)
  variables <- check_variables(variables, x)
  iterations <- check_iterations(iterations, x)
  chains <- check_chains(chains, x)
  if (!is.null(draws)) {
    stop2("Cannot subset 'draws' in 'draws_array' objects.")
  }
  x <- subset_dims(x, iterations, chains, variables)
  x <- repair_indices(x)
  x
}

#' @export
subset.draws_data_frame <- function(x, variables = NULL, iterations = NULL,
                                    chains = NULL, draws = NULL, ...) {
  x <- repair_indices(x)
  variables <- check_variables(variables, x)
  iterations <- check_iterations(iterations, x)
  chains <- check_chains(chains, x)
  draws <- check_draws(draws, x)
  if (!is.null(draws)) {
    if (!is.null(iterations)) {
      stop2("Cannot subset 'iterations' and 'draws' at the same time.")
    }
    if (!is.null(chains)) {
      stop2("Cannot subset 'chains' and 'draws' at the same time.")
    }
  }
  if (!is.null(variables)) {
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
    }
    if (!is.null(iterations)) {
      x <- x[x$.iteration %in% iterations, ]
    }
    if (!is.null(chains) || !is.null(iterations)) {
      x <- repair_indices(x)
    }
  }
  x
}

#' @export
subset.draws_list <- function(x, variables = NULL, iterations = NULL,
                              chains = NULL, draws = NULL, ...) {
  x <- repair_indices(x)
  variables <- check_variables(variables, x)
  iterations <- check_iterations(iterations, x)
  chains <- check_chains(chains, x)
  if (!is.null(draws)) {
    stop2("Cannot subset 'draws' in 'draws_array' objects.")
  }
  if (!is.null(chains)) {
    x <- x[chains]
  }
  if (!is.null(variables)) {
    for (i in seq_along(x)) {
      x[[i]] <- x[[i]][variables]
    }
  }
  if (!is.null(iterations)) {
    for (i in seq_along(x)) {
      for (j in seq_along(x[[i]])) {
        x[[i]][[j]] <- x[[i]][[j]][[iterations]]
      }
    }
  }
  x <- repair_indices(x)
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
# @param ... arguments containing indices for subsetting a dimension
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
