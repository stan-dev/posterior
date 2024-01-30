#' Index `draws` objects
#'
#' Index iterations, chains, and draws of [`draws`] objects.
#'
#' @name draws-index
#' @template args-methods-x
#'
#' @details
#' The methods `iteration_ids()`, `chain_ids()`, and `draw_ids()` return
#' vectors of all iterations, chains, and draws, respectively. In
#' contrast, the methods `niterations()`, `nchains()`, and
#' `ndraws()` return the number of variables, iterations, chains, and draws,
#' respectively.
#'
#' @return
#'
#' For `iteration_ids()`, `chain_ids()`, and `draw_ids()`, an integer vector.
#'
#' For `niterations()`, `nchains()`, and `ndraws()`, a scalar integer.
#'
#' @seealso [`variables`], [`rename_variables`]
#'
#' @examples
#' x <- example_draws()
#'
#' iteration_ids(x)
#' niterations(x)
#'
#' chain_ids(x)
#' nchains(x)
#'
#' draw_ids(x)
#' ndraws(x)
#'
NULL

#' @rdname draws-index
#' @export
iteration_ids <- function(x) {
  UseMethod("iteration_ids")
}

#' @export
iteration_ids.NULL <- function(x) {
  NULL
}

#' @export
iteration_ids.draws_matrix <- function(x) {
  if (nchains(x) > 1) {
    out <- seq_len(niterations(x))
  } else {
    out <- draw_ids(x)
  }
  out
}

#' @export
iteration_ids.draws_array <- function(x) {
  out <- rownames(x) %||% seq_rows(x)
  as.integer(out)
}

#' @export
iteration_ids.draws_df <- function(x) {
  as.integer(unique(x$.iteration))
}

#' @export
iteration_ids.draws_list <- function(x) {
  seq_along(x[[1]][[1]])
}

#' @export
iteration_ids.draws_rvars <- function(x) {
  iteration_ids(x[[1]])
}

#' @export
iteration_ids.rvar <- function(x) {
  if (nchains(x) > 1) {
    out <- seq_len(niterations(x))
  } else {
    out <- draw_ids(x)
  }
  out
}

#' @rdname draws-index
#' @export
chain_ids <- function(x) {
  UseMethod("chain_ids")
}

#' @export
chain_ids.NULL <- function(x) {
  NULL
}

#' @export
chain_ids.draws_matrix <- function(x) {
  seq_len(nchains(x))
}

#' @export
chain_ids.draws_array <- function(x) {
  out <- colnames(x) %||% seq_cols(x)
  as.integer(out)
}

#' @export
chain_ids.draws_df <- function(x) {
  as.integer(unique(x$.chain))
}

#' @export
chain_ids.draws_list <- function(x) {
  out <- names(x) %||% seq_rows(x)
  as.integer(out)
}

#' @export
chain_ids.draws_rvars <- function(x) {
  chain_ids(x[[1]])
}

#' @export
chain_ids.draws_rvars <- function(x) {
  seq_len(nchains(x))
}

#' @rdname draws-index
#' @export
draw_ids <- function(x) {
  UseMethod("draw_ids")
}

#' @export
draw_ids.NULL <- function(x) {
  NULL
}

#' @export
draw_ids.draws_matrix <- function(x) {
  as.integer(rownames(x) %||% seq_rows(x))
}

#' @export
draw_ids.draws_array <- function(x) {
  iteration_ids <- iteration_ids(x)
  niterations <- niterations(x)
  chain_ids <- chain_ids(x)
  ulapply(chain_ids, function(c) niterations * (c - 1L) + iteration_ids)
}

#' @export
draw_ids.draws_df <- function(x) {
  as.integer(unique(x$.draw))
}

#' @export
draw_ids.draws_list <- function(x) {
  iteration_ids <- iteration_ids(x)
  niterations <- niterations(x)
  chain_ids <- chain_ids(x)
  ulapply(chain_ids, function(c) niterations * (c - 1L) + iteration_ids)
}

#' @export
draw_ids.draws_rvars <- function(x) {
  draw_ids(x[[1]])
}

#' @export
draw_ids.rvar <- function(x) {
  draws <- draws_of(x)
  out <- rownames(draws) %||% seq_rows(draws)
  as.integer(out)
}

#' @rdname draws-index
#' @export
niterations <- function(x) {
  UseMethod("niterations")
}

#' @export
niterations.NULL <- function(x) {
  0
}

#' @export
niterations.draws_matrix <- function(x) {
  NROW(x) / nchains(x)
}

#' @export
niterations.draws_array <- function(x) {
  NROW(x)
}

#' @export
niterations.draws_df <- function(x) {
  length(iteration_ids(x))
}

#' @export
niterations.draws_list <- function(x) {
  if (!length(x) || !length(x[[1]])) {
    return(0)
  }
  length(x[[1]][[1]])
}

#' @export
niterations.draws_rvars <- function(x) {
  if (!length(x)) 0 else niterations(x[[1]])
}

#' @export
niterations.rvar <- function(x) {
  ndraws(x) / nchains(x)
}

#' @rdname draws-index
#' @export
nchains <- function(x) {
  UseMethod("nchains")
}

#' @export
nchains.NULL <- function(x) {
  0
}

#' @export
nchains.draws_matrix <- function(x) {
  attr(x, "nchains") %||% 1L
}

#' @export
nchains.draws_array <- function(x) {
  NCOL(x)
}

#' @export
nchains.draws_df <- function(x) {
  length(chain_ids(x))
}

#' @export
nchains.draws_list <- function(x) {
  length(x)
}

#' @export
nchains.draws_rvars <- function(x) {
  if (!length(x)) 0 else nchains(x[[1]])
}

#' @export
nchains.rvar <- function(x) {
  attr(x, "nchains") %||% 1L
}
# for internal use only currently: if you are setting the nchains
# attribute on an rvar, ALWAYS use this function so that the proxy
# cache is invalidated
`nchains_rvar<-` <- function(x, value) {
  attr(x, "nchains") <- value
  invalidate_rvar_cache(x)
}


#' @rdname draws-index
#' @export
ndraws <- function(x) {
  UseMethod("ndraws")
}

#' @export
ndraws.NULL <- function(x) {
  0
}

#' @export
ndraws.draws_matrix <- function(x) {
  NROW(x)
}

#' @export
ndraws.draws_array <- function(x) {
  niterations(x) * nchains(x)
}

#' @export
ndraws.draws_df <- function(x) {
  NROW(x)
}

#' @export
ndraws.draws_list <- function(x) {
  niterations(x) * nchains(x)
}

#' @export
ndraws.draws_rvars <- function(x) {
  if (!length(x)) 0 else ndraws(x[[1]])
}

#' @export
ndraws.rvar <- function(x) {
  # as.vector() to drop names in case there are index names
  as.vector(NROW(draws_of(x)))
}


# internal ----------------------------------------------------------------

# check validity of iteration indices
# @param unique should the returned IDs be unique?
check_iteration_ids <- function(iteration_ids, x, unique = TRUE, exclude = FALSE) {
  check_draws_object(x)
  if (is.null(iteration_ids)) {
    return(NULL)
  }
  unique <- as_one_logical(unique)
  exclude <- as_one_logical(exclude)
  iteration_ids <- as.integer(iteration_ids)
  if (unique) {
    iteration_ids <- unique(iteration_ids)
  }
  iteration_ids <- sort(iteration_ids)
  if (any(iteration_ids < 1L)) {
    stop_no_call("Iteration indices should be positive.")
  }
  niterations <- niterations(x)
  max_iteration <- SW(max(iteration_ids))
  if (max_iteration > niterations) {
    stop_no_call("Tried to subset iterations up to '", max_iteration, "' ",
          "but the object only has '", niterations, "' iterations.")
  }

  # handle exclude iterations in subset_draws
  if (exclude) {
    iteration_ids <- setdiff(iteration_ids(x), iteration_ids)
  }

  invisible(iteration_ids)
}

# check validity of chain indices
# @param unique should the returned IDs be unique?
check_chain_ids <- function(chain_ids, x, unique = TRUE, exclude = FALSE) {
  check_draws_object(x)
  if (is.null(chain_ids)) {
    return(NULL)
  }
  unique <- as_one_logical(unique)
  exclude <- as_one_logical(exclude)
  chain_ids <- as.integer(chain_ids)
  if (unique) {
    chain_ids <- unique(chain_ids)
  }
  chain_ids <- sort(chain_ids)
  if (any(chain_ids < 1L)) {
    stop_no_call("Chain indices should be positive.")
  }
  nchains <- nchains(x)
  max_chain <- SW(max(chain_ids))
  if (max_chain > nchains) {
    stop_no_call("Tried to subset chains up to '", max_chain, "' ",
          "but the object only has '", nchains, "' chains.")
  }

  if (exclude) {
    chain_ids <- setdiff(chain_ids(x), chain_ids)
  }

  invisible(chain_ids)
}

# check validity of draw indices
# @param unique should the returned IDs be unique?
check_draw_ids <- function(draw_ids, x, unique = TRUE, exclude = FALSE) {
  check_draws_object(x)
  if (is.null(draw_ids)) {
    return(NULL)
  }
  unique <- as_one_logical(unique)
  exclude <- as_one_logical(exclude)
  draw_ids <- as.integer(draw_ids)
  if (unique) {
    draw_ids <- unique(draw_ids)
  }
  draw_ids <- sort(draw_ids)
  if (any(draw_ids < 1L)) {
    stop_no_call("Draw indices should be positive.")
  }
  ndraws <- ndraws(x)
  max_draw <- SW(max(draw_ids))
  if (max_draw > ndraws) {
    stop_no_call("Tried to subset draws up to '", max_draw, "' ",
          "but the object only has '", ndraws, "' draws.")
  }

  if (exclude) {
    draw_ids <- setdiff(draw_ids(x), draw_ids)
  }

  invisible(draw_ids)
}
