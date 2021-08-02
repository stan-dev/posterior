#' Subset `draws` objects
#'
#' Subset [`draws`] objects by variables, iterations, chains, and draws indices.
#'
#' @template args-methods-x
#' @param variable (character vector) The variables to select. All elements of
#'   non-scalar variables can be selected at once.
#' @param iteration (integer vector) The iteration indices to select.
#' @param chain (integer vector) The chain indices to select.
#' @param draw (integer vector) The draw indices to be select. Subsetting draw
#'   indices will lead to an automatic merging of chains via [`merge_chains`].
#' @param regex (logical) Should `variable` should be treated as a
#'   (vector of) regular expressions? Any variable in `x` matching at least one
#'   of the regular expressions will be selected. Defaults to `FALSE`.
#' @param unique (logical) Should duplicated selection of chains, iterations, or
#'   draws be allowed? If `TRUE` (the default) only unique chains, iterations,
#'   and draws are selected regardless of how often they appear in the
#'   respective selecting arguments.
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
#' # extract all elements of 'theta'
#' subset_draws(x, variable = "theta")
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
  if (all_null(variable, iteration, chain, draw)) {
    return(x)
  }
  x <- repair_draws(x)
  variable <- check_existing_variables(variable, x, regex = regex)
  iteration <- check_iteration_ids(iteration, x, unique = unique)
  chain <- check_chain_ids(chain, x, unique = unique)
  draw <- check_draw_ids(draw, x, unique = unique)
  x <- prepare_subsetting(x, iteration, chain, draw)
  x <- .subset_draws(x, iteration, chain, draw, variable, reserved = TRUE)
  if (!is.null(chain) || !is.null(iteration)) {
    x <- repair_draws(x, order = FALSE)
  }
  x
}

#' @rdname subset_draws
#' @export
subset_draws.draws_array <- function(x, variable = NULL, iteration = NULL,
                                     chain = NULL, draw = NULL, regex = FALSE,
                                     unique = TRUE, ...) {
  if (all_null(variable, iteration, chain, draw)) {
    return(x)
  }
  x <- repair_draws(x)
  variable <- check_existing_variables(variable, x, regex = regex)
  iteration <- check_iteration_ids(iteration, x, unique = unique)
  chain <- check_chain_ids(chain, x, unique = unique)
  draw <- check_draw_ids(draw, x, unique = unique)
  x <- prepare_subsetting(x, iteration, chain, draw)
  if (!is.null(draw)) {
    iteration <- draw
  }
  x <- .subset_draws(x, iteration, chain, variable, reserved = TRUE)
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
  if (all_null(variable, iteration, chain, draw)) {
    return(x)
  }
  x <- repair_draws(x)
  unique <- as_one_logical(unique)
  variable <- check_existing_variables(variable, x, regex = regex)
  iteration <- check_iteration_ids(iteration, x, unique = unique)
  chain <- check_chain_ids(chain, x, unique = unique)
  draw <- check_draw_ids(draw, x, unique = unique)
  x <- prepare_subsetting(x, iteration, chain, draw)
  x <- .subset_draws(
    x, iteration, chain, draw, variable,
    unique = unique, reserved = TRUE
  )
  x
}

#' @rdname subset_draws
#' @export
subset_draws.draws_list <- function(x, variable = NULL, iteration = NULL,
                                    chain = NULL, draw = NULL, regex = FALSE,
                                    unique = TRUE, ...) {
  if (all_null(variable, iteration, chain, draw)) {
    return(x)
  }
  x <- repair_draws(x)
  variable <- check_existing_variables(variable, x, regex = regex)
  iteration <- check_iteration_ids(iteration, x, unique = unique)
  chain <- check_chain_ids(chain, x, unique = unique)
  draw <- check_draw_ids(draw, x, unique = unique)
  x <- prepare_subsetting(x, iteration, chain, draw)
  if (!is.null(draw)) {
    iteration <- draw
  }
  x <- .subset_draws(x, iteration, chain, variable, reserved = TRUE)
  if (!is.null(chain) || !is.null(iteration)) {
    x <- repair_draws(x, order = FALSE)
  }
  x
}

#' @rdname subset_draws
#' @export
subset_draws.draws_rvars <- function(x, variable = NULL, iteration = NULL,
                                     chain = NULL, draw = NULL, regex = FALSE,
                                     unique = TRUE, ...) {
  if (all_null(variable, iteration, chain, draw)) {
    return(x)
  }
  x <- repair_draws(x)
  variable <- check_existing_variables(variable, x, regex = regex)
  iteration <- check_iteration_ids(iteration, x, unique = unique)
  chain <- check_chain_ids(chain, x, unique = unique)
  draw <- check_draw_ids(draw, x, unique = unique)
  x <- prepare_subsetting(x, iteration, chain, draw)
  if (!is.null(draw)) {
    iteration <- draw
  }
  x <- .subset_draws(x, iteration, chain, variable, reserved = TRUE)
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
    stop_no_call("'x' has only ", dim_x, " dimensions.")
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

# internal method for subset_draws
.subset_draws <- function(x, ...) {
  UseMethod(".subset_draws")
}

#' @export
.subset_draws.draws_matrix <- function(x, iteration = NULL, chain = NULL,
                                       draw = NULL, variable = NULL,
                                       reserved = FALSE, ...) {
  out <- subset_dims(x, NULL, variable)
  if (!is.null(draw)) {
    out <- out[draw, ]
  } else {
    if (!is.null(chain)) {
      chain <- unique(chain)
      nchains <- length(chain)
      chain_ids <- rep(chain_ids(out), each = niterations(out))
      slice_index <- chain_ids %in% chain
      out <- out[slice_index, ]
      attr(out, "nchains") <- nchains
    }
    if (!is.null(iteration)) {
      niterations <- length(iteration)
      slice_index <- iteration +
        (rep(chain_ids(out), each = niterations) - 1) * niterations(out)
      nchains <- nchains(out)
      out <- out[slice_index, ]
      attr(out, "nchains") <- nchains
    }
  }
  if (reserved && !is.null(variable)) {
    new_vars <- variables(out, reserved = TRUE)
    reserved_vars <- setdiff(reserved_variables(x), new_vars)
    if (length(reserved_vars)) {
      x_reserved <- subset_dims(x, draw, reserved_vars)
      out <- cbind(out, x_reserved)
      class(out) <- class(x)
    }
  }
  out
}

#' @export
.subset_draws.draws_array <- function(x, iteration = NULL, chain = NULL,
                                      variable = NULL, reserved = FALSE, ...) {
  out <- subset_dims(x, iteration, chain, variable)
  if (reserved && !is.null(variable)) {
    new_vars <- variables(out, reserved = TRUE)
    reserved_vars <- setdiff(reserved_variables(x), new_vars)
    if (length(reserved_vars)) {
      x_reserved <- subset_dims(x, iteration, chain, reserved_vars)
      out <- abind(out, x_reserved, along = 3L)
      class(out) <- class(x)
    }
  }
  out
}

#' @export
.subset_draws.draws_df <- function(x, iteration = NULL, chain = NULL,
                                   draw = NULL, variable = NULL,
                                   unique = TRUE, reserved = FALSE, ...) {
  if (!is.null(variable)) {
    x <- x[, variable, reserved = TRUE]
  }
  if (!is.null(draw)) {
    # each x$.draw is unique so using 'match' is valid here
    x <- x[match(draw, x$.draw), ]
    x$.draw <- repair_iteration_ids(x$.draw)
    x$.iteration <- x$.draw
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
      if (!is.null(chain)) {
        x <- x[unlist(lapply(chain, function(c) which(x$.chain == c))), ]
        x$.chain <- rep(seq_along(chain), each = nrow(x) / length(chain))
      }
      if (!is.null(iteration)) {
        x <- x[unlist(lapply(iteration, function(i) which(x$.iteration == i))), ]
        x$.iteration <- rep(seq_along(iteration), each = nrow(x) / length(iteration))
      }
      x <- repair_draws(x)
    }
  }
  if (!reserved && !is.null(variable)) {
    # remove reserved variables which were not selected
    reserved_vars <- setdiff(reserved_variables(x), variable)
    x <- remove_variables(x, reserved_vars)
  }
  x
}

#' @export
.subset_draws.draws_list <- function(x, iteration = NULL, chain = NULL,
                                     variable = NULL, reserved = FALSE, ...) {
  if (!is.null(chain)) {
    x <- x[chain]
  }
  if (!is.null(variable)) {
    if (reserved) {
      z <- x
    }
    for (i in seq_along(x)) {
      x[[i]] <- x[[i]][variable]
    }
    if (reserved) {
      new_vars <- variables(x, reserved = TRUE)
      reserved_vars <- setdiff(reserved_variables(z), new_vars)
      if (length(reserved_vars)) {
        for (i in seq_along(x)) {
          x[[i]] <- c(x[[i]], z[[i]][reserved_vars])
        }
      }
      remove(z)
    }
  }
  if (!is.null(iteration)) {
    for (i in seq_along(x)) {
      for (j in seq_along(x[[i]])) {
        x[[i]][[j]] <- x[[i]][[j]][iteration]
      }
    }
  }
  x
}

#' @importFrom vctrs vec_slice
#' @export
.subset_draws.draws_rvars <- function(x, iteration = NULL, chain = NULL,
                                      variable = NULL, reserved = FALSE, ...) {
  if (!is.null(variable)) {
    if (reserved) {
      z <- x
    }
    x <- x[variable]
    if (reserved) {
      new_vars <- variables(x, reserved = TRUE)
      reserved_vars <- setdiff(reserved_variables(z), new_vars)
      x <- c(x, z[reserved_vars])
      # c() currently removes the 'draws' classes
      class(x) <- class_draws_rvars()
      remove(z)
    }
  }
  if (!is.null(chain)) {
    chain <- unique(chain)
    nchains <- length(chain)
    chain_ids <- rep(chain_ids(x), each = niterations(x))
    slice_index <- chain_ids %in% chain
    for (i in seq_along(x)) {
      draws_of(x[[i]]) <- vec_slice(draws_of(x[[i]]), slice_index)
      nchains_rvar(x[[i]]) <- nchains
    }
  }
  if (!is.null(iteration)) {
    niterations <- length(iteration)
    slice_index <- iteration +
      (rep(chain_ids(x), each = niterations) - 1) * niterations(x)
    for (i in seq_along(x)) {
      draws_of(x[[i]]) <- vec_slice(draws_of(x[[i]]), slice_index)
    }
  }
  x
}

# prepare object to be subsetted via 'subset_draws'
prepare_subsetting <- function(x, iteration = NULL, chain = NULL,
                               draw = NULL) {
  if (!is.null(draw)) {
    if (!is.null(iteration)) {
      stop_no_call("Cannot subset 'iteration' and 'draw' at the same time.")
    }
    if (!is.null(chain)) {
      stop_no_call("Cannot subset 'chain' and 'draw' at the same time.")
    }
    if (nchains(x) > 1L) {
      message("Merging chains in order to subset via 'draw'.")
      x <- merge_chains(x)
    }
  }
  x
}
