#' Subset Draws Objects
#'
#' Subset `draws` objects after variables, iteration, chains, and draws
#' indices.
#'
#' @name subset-draws
#' @aliases subset.draws
#' @template args-methods-x
#' @param variable Character vector of variable names to be selected.
#' @param iteration Numeric vector of iteration indices to be selected.
#' @param chain Numeric vector of chain indices to be selected.
#' @param draw Numeric vector of draw indices to be selected.
#' @template args-methods-dots
#' @template return-draws
#'
#' @details
#' To ensure that multiple consequtive subsetting operations are working
#' correctly, `subset` repairs `draws` objects before and after subsetting
#' via `\link{repair_draws}`.
#'
#' @examples
#' x <- example_draws()
#' subset(x, variable = c("mu", "tau"))
#' subset(x, chain = 2)
#' subset(x, iteration = 5:10, chain = 3:4)
#'
NULL

#' @rdname subset-draws
#' @export
subset.draws_matrix <- function(x, variable = NULL, iteration = NULL,
                                chain = NULL, draw = NULL, ...) {
  x <- repair_draws(x)
  variable <- check_variables(variable, x)
  iteration <- check_iteration_ids(iteration, x)
  draw <- check_draw_ids(draw, x)
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
  x <- repair_draws(x, order = FALSE)
  x
}

#' @rdname subset-draws
#' @export
subset.draws_array <- function(x, variable = NULL, iteration = NULL,
                               chain = NULL, draw = NULL, ...) {
  x <- repair_draws(x)
  variable <- check_variables(variable, x)
  iteration <- check_iteration_ids(iteration, x)
  chain <- check_chain_ids(chain, x)
  if (!is.null(draw)) {
    stop2("Cannot subset 'draw' in 'draws_array' objects.")
  }
  x <- subset_dims(x, iteration, chain, variable)
  x <- repair_draws(x, order = FALSE)
  x
}

#' @rdname subset-draws
#' @export
subset.draws_df <- function(x, variable = NULL, iteration = NULL,
                            chain = NULL, draw = NULL, ...) {
  x <- repair_draws(x)
  variable <- check_variables(variable, x)
  iteration <- check_iteration_ids(iteration, x)
  chain <- check_chain_ids(chain, x)
  draw <- check_draw_ids(draw, x)
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
    x$.draw <- repair_iteration_ids(x$.draw)
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
      x <- repair_draws(x, order = FALSE)
    }
  }
  x
}

#' @rdname subset-draws
#' @export
subset.draws_list <- function(x, variable = NULL, iteration = NULL,
                              chain = NULL, draw = NULL, ...) {
  x <- repair_draws(x)
  variable <- check_variables(variable, x)
  iteration <- check_iteration_ids(iteration, x)
  chain <- check_chain_ids(chain, x)
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
  x <- repair_draws(x, order = FALSE)
  x
}

#' subset specified non-NULL dimensions
#' @param x an object to be subsetted
#' @param ... arguments containing indices for subsetting a dimension
#'   NULL is treated as not subsetting that dimension
#' @noRd
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
