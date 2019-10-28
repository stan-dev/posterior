#' @export
subset.draws_matrix <- function(x, variable = NULL, iteration = NULL,
                                chain = NULL, draw = NULL, ...) {
  x <- repair_indices(x)
  variable <- check_variables(variable, x)
  iteration <- check_iterations(iteration, x)
  draw <- check_draws(draw, x)
  if (!is.null(chain)) {
    stop2("Cannot subset 'chain' in 'draws_matrix' objects.")
  }
  if (!is.null(iteration)) {
    if (!is.null(draw)) {
      stop2("Cannot subset 'iteration' and 'draw' at the same time.")
    }
    draw <- iteration
  }
  x <- subset_dims(x, draw, variable)
  x <- repair_indices(x)
  x
}

#' @export
subset.draws_array <- function(x, variable = NULL, iteration = NULL,
                               chain = NULL, draw = NULL, ...) {
  x <- repair_indices(x)
  variable <- check_variables(variable, x)
  iteration <- check_iterations(iteration, x)
  chain <- check_chains(chain, x)
  if (!is.null(draw)) {
    stop2("Cannot subset 'draw' in 'draws_array' objects.")
  }
  x <- subset_dims(x, iteration, chain, variable)
  x <- repair_indices(x)
  x
}

#' @export
subset.draws_data_frame <- function(x, variable = NULL, iteration = NULL,
                                    chain = NULL, draw = NULL, ...) {
  x <- repair_indices(x)
  variable <- check_variables(variable, x)
  iteration <- check_iterations(iteration, x)
  chain <- check_chains(chain, x)
  draw <- check_draws(draw, x)
  if (!is.null(draw)) {
    if (!is.null(iteration)) {
      stop2("Cannot subset 'iteration' and 'draw' at the same time.")
    }
    if (!is.null(chain)) {
      stop2("Cannot subset 'chain' and 'draw' at the same time.")
    }
  }
  if (!is.null(variable)) {
    x <- x[, c(meta_columns(x), variable)]
  }
  if (!is.null(draw)) {
    x <- x[x$.draw %in% draw, ]
    # subsetting draw invalidates iteration and chain
    x$.draw <- index_continuously(x$.draw)
    x$.iteration <- x$.draw
    x$.chain <- 1L
  } else {
    if (!is.null(chain)) {
      x <- x[x$.chain %in% chain, ]
    }
    if (!is.null(iteration)) {
      x <- x[x$.iteration %in% iteration, ]
    }
    if (!is.null(chain) || !is.null(iteration)) {
      x <- repair_indices(x)
    }
  }
  x
}

#' @export
subset.draws_list <- function(x, variable = NULL, iteration = NULL,
                              chain = NULL, draw = NULL, ...) {
  x <- repair_indices(x)
  variable <- check_variables(variable, x)
  iteration <- check_iterations(iteration, x)
  chain <- check_chains(chain, x)
  if (!is.null(draw)) {
    stop2("Cannot subset 'draw' in 'draws_array' objects.")
  }
  if (!is.null(chain)) {
    x <- x[chain]
  }
  if (!is.null(variable)) {
    for (i in seq_along(x)) {
      x[[i]] <- x[[i]][variable]
    }
  }
  if (!is.null(iteration)) {
    for (i in seq_along(x)) {
      for (j in seq_along(x[[i]])) {
        x[[i]][[j]] <- x[[i]][[j]][[iteration]]
      }
    }
  }
  x <- repair_indices(x)
  x
}

#' @export
subset_variables <- function(x, variable) {
  check_draws_object(x)
  subset(x, variable = variable)
}

#' @export
subset_iterations <- function(x, iteration) {
  check_draws_object(x)
  subset(x, iteration = iteration)
}

#' @export
subset_chains <- function(x, chain) {
  check_draws_object(x)
  subset(x, chain = chain)
}

#' @export
subset_draws <- function(x, draw) {
  check_draws_object(x)
  subset(x, draw = draw)
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
