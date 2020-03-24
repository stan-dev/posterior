#' Subset `draws` objects
#'
#' Subset [`draws`] objects by variables, iterations, chains, and draws indices.
#'
#' @template args-methods-x
#' @param variable Character vector of variable names to be selected.
#' @param iteration Numeric vector of iteration indices to be selected.
#' @param chain Numeric vector of chain indices to be selected.
#' @param draw Numeric vector of draw indices to be selected.
#' @param regex Logical. Indicates whether `variable` should be treated as a
#'   (vector of) regular expressions. Any variable in `x` matching at least one
#'   of the regular expressions will be selected.
#' @param unique Logical. Indicates whether duplicated selection of chains,
#'   iterations, or draws is possible. If `TRUE` (the default) only
#'   unique chains, iterations, and draws are selected regardless of how
#'   often they appear in the respective selecting arguments.
#'
#' @template args-methods-dots
#' @template return-draws
#'
#' @details
#' To ensure that multiple consecutive subsetting operations work correctly,
#' `subset()` *[repairs][repair_draws]* the `draws` object before and after
#' subsetting.
#'
#' @examples
#' x <- example_draws()
#' subset_draws(x, variable = c("mu", "tau"))
#' subset_draws(x, chain = 2)
#' subset_draws(x, iteration = 5:10, chain = 3:4)
#'
#' # extract the first chain twice
#' subset_draws(x, chain = c(1, 1), unique = FALSE)
#'
#' @export
subset_draws <- function(x, ...) {
  UseMethod("subset_draws")
}

#' @rdname subset_draws
#' @export
subset_draws.draws_matrix <- function(x, variable = NULL, iteration = NULL,
                                      chain = NULL, draw = NULL, regex = FALSE,
                                      unique = TRUE, ...) {
  x <- repair_draws(x)
  variable <- check_existing_variables(variable, x, regex = regex)
  iteration <- check_iteration_ids(iteration, x, unique = unique)
  draw <- check_draw_ids(draw, x, unique = unique)
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
  if (!is.null(draw)) {
    x <- repair_draws(x, order = FALSE)
  }
  x
}

#' @rdname subset_draws
#' @export
subset_draws.draws_array <- function(x, variable = NULL, iteration = NULL,
                                     chain = NULL, draw = NULL, regex = FALSE,
                                     unique = TRUE, ...) {
  x <- repair_draws(x)
  variable <- check_existing_variables(variable, x, regex = regex)
  iteration <- check_iteration_ids(iteration, x, unique = unique)
  chain <- check_chain_ids(chain, x, unique = unique)
  if (!is.null(draw)) {
    stop2("Cannot subset 'draw' in 'draws_array' objects.")
  }
  x <- subset_dims(x, iteration, chain, variable)
  if (!is.null(chain) || !is.null(iteration)) {
    x <- repair_draws(x, order = FALSE)
  }
  x
}

#' @rdname subset_draws
#' @export
subset_draws.draws_df <- function(x, variable = NULL, iteration = NULL,
                                  chain = NULL, draw = NULL, regex = FALSE,
                                  unique = TRUE, ...) {
  x <- repair_draws(x)
  unique <- as_one_logical(unique)
  variable <- check_existing_variables(variable, x, regex = regex)
  iteration <- check_iteration_ids(iteration, x, unique = unique)
  chain <- check_chain_ids(chain, x, unique = unique)
  draw <- check_draw_ids(draw, x, unique = unique)
  if (!is.null(draw)) {
    if (!is.null(iteration)) {
      stop2("Cannot subset 'iteration' and 'draw' at the same time.")
    }
    if (!is.null(chain)) {
      stop2("Cannot subset 'chain' and 'draw' at the same time.")
    }
  }
  if (!is.null(variable)) {
    x <- x[, c(variable, meta_columns(x))]
  }
  if (!is.null(draw)) {
    # each x$.draw is unique so using 'match' is valid here
    x <- x[match(draw, x$.draw), ]
    # subsetting draw invalidates iteration and chain
    x$.draw <- repair_iteration_ids(x$.draw)
    x$.iteration <- x$.draw
    x$.chain <- 1L
  } else if (!is.null(chain) || !is.null(iteration)) {
    if (unique) {
      if (!is.null(chain)) {
        x <- x[x$.chain %in% chain, ]
      }
      if (!is.null(iteration)) {
        x <- x[x$.iteration %in% iteration, ]
      }
      x <- repair_draws(x, order = FALSE)
    } else {
      # non-unique subsetting is conceptually easier in 'draws_list' objects
      # TODO: perform non-unique subsetting directly in the 'draws_df' object
      x <- as_draws_list(x)
      x <- subset_draws(x, chain = chain, iteration = iteration, unique = FALSE)
      x <- as_draws_df(x)
    }
  }
  x
}

#' @rdname subset_draws
#' @export
subset_draws.draws_list <- function(x, variable = NULL, iteration = NULL,
                                    chain = NULL, draw = NULL, regex = FALSE,
                                    unique = TRUE, ...) {
  x <- repair_draws(x)
  variable <- check_existing_variables(variable, x, regex = regex)
  iteration <- check_iteration_ids(iteration, x, unique = unique)
  chain <- check_chain_ids(chain, x, unique = unique)
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
        x[[i]][[j]] <- x[[i]][[j]][iteration]
      }
    }
  }
  if (!is.null(chain) || !is.null(iteration)) {
    x <- repair_draws(x, order = FALSE)
  }
  x
}
#' @rdname subset_draws
#' @export
subset.draws <- function(x, ...) {
  subset_draws(x, ...)
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
