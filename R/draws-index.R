#' Index Draws Objects
#'
#' Index variables, iterations, chains, and draws.
#'
#' @name draws-index
#' @template args-methods-x
#' @param value For `variables(x) <- value`, a character vector of new variable names.
#'
#' @details
#' The methods `variables()`, `iteration_ids()`, `chain_ids()`, and `draw_ids()` return
#' vectors of all variables, iterations, chains, and draws, respectively. In
#' contrast, the methods `nvariables()`, `niterations()`, `nchains()`, and
#' `ndraws()` return the number of variables, iterations, chains, and draws,
#' respectively.
#'
#' `variables(x) <- value` allows you to modify the vector of variable names,
#' similar to how `names(x) <- value` works for vectors and lists. For renaming
#' specific variables, [rename_variables()] may offer a more convenient approach.
#'
#' @examples
#' x <- example_draws()
#'
#' variables(x)
#' nvariables(x)
#' variables(x) <- letters[1:nvariables(x)]
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
variables <- function(x) {
  UseMethod("variables")
}

#' @export
variables.draws_matrix <- function(x) {
  colnames(x)
}

#' @export
variables.draws_array <- function(x) {
  dimnames(x)[[3L]]
}

#' @export
variables.draws_df <- function(x) {
  # can't use setdiff() here as in the corner case where someone
  # manually creates duplicate columns it will give incorrect results
  names(x)[!names(x) %in% meta_columns(x)]
}

#' @export
variables.draws_list <- function(x) {
  if (!length(x)) {
    return(character(0))
  }
  names(x[[1]])
}

#' @rdname draws-index
#' @export
`variables<-` <- function(x, value) {
  UseMethod("variables<-")
}

#' @export
`variables<-.draws_matrix` <- function(x, value) {
  check_new_variables(value)
  colnames(x) <- value
  x
}

#' @export
`variables<-.draws_array` <- function(x, value) {
  check_new_variables(value)
  dimnames(x)[[3L]] <- value
  x
}

#' @export
`variables<-.draws_df` <- function(x, value) {
  check_new_variables(value)
  names(x)[!names(x) %in% meta_columns(x)] <- value
  x
}

#' @export
`variables<-.draws_list` <- function(x, value) {
  check_new_variables(value)
  for (i in seq_along(x)) {
    names(x[[i]]) <- value
  }
  x
}

#' @rdname draws-index
#' @export
iteration_ids <- function(x) {
  UseMethod("iteration_ids")
}

#' @export
iteration_ids.draws_matrix <- function(x) {
  out <- rownames(x) %||% seq_rows(x)
  as.integer(out)
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

#' @rdname draws-index
#' @export
chain_ids <- function(x) {
  UseMethod("chain_ids")
}

#' @export
chain_ids.draws_matrix <- function(x) {
  1L
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

#' @rdname draws-index
#' @export
draw_ids <- function(x) {
  UseMethod("draw_ids")
}

#' @export
draw_ids.draws_matrix <- function(x) {
  iteration_ids(x)
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

#' @rdname draws-index
#' @export
nvariables <- function(x) {
  UseMethod("nvariables")
}

#' @export
nvariables.draws_matrix <- function(x) {
  NCOL(x)
}

#' @export
nvariables.draws_array <- function(x) {
  dim(x)[3]
}

#' @export
nvariables.draws_df <- function(x) {
  length(variables(x))
}

#' @export
nvariables.draws_list <- function(x) {
  if (!length(x)) {
    return(0)
  }
  length(x[[1]])
}

#' @rdname draws-index
#' @export
niterations <- function(x) {
  UseMethod("niterations")
}

#' @export
niterations.draws_matrix <- function(x) {
  NROW(x)
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

#' @rdname draws-index
#' @export
nchains <- function(x) {
  UseMethod("nchains")
}

#' @export
nchains.draws_matrix <- function(x) {
  1L
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

#' @rdname draws-index
#' @export
ndraws <- function(x) {
  UseMethod("ndraws")
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

# check validity of existing variable names: e.g., that
# all `variables` exist in `x` and that no `variables`
# are reserved words
# @param regex should 'variables' be treated as regular expressions?
# @param scalar should only scalar variables be matched?
check_existing_variables <- function(variables, x, regex = FALSE,
                                     scalar = TRUE) {
  check_draws_object(x)
  if (is.null(variables)) {
    return(NULL)
  }
  regex <- as_one_logical(regex)
  scalar <- as_one_logical(scalar)
  variables <- unique(as.character(variables))
  all_variables <- variables(x)
  if (regex) {
    tmp <- named_list(variables)
    for (i in seq_along(variables)) {
      tmp[[i]] <- all_variables[grepl(variables[i], all_variables)]
    }
    # regular expressions are not required to match anything
    missing_variables <- NULL
    variables <- as.character(unique(unlist(tmp)))
  } else if (!scalar) {
    tmp <- named_list(variables)
    escaped_variables <- escape_all(variables)
    missing <- rep(NA, length(variables))
    for (i in seq_along(variables)) {
      v_regex <- paste0("^", escaped_variables[i], "(\\[[^\\]*])?$")
      tmp[[i]] <- all_variables[grepl(v_regex, all_variables)]
      missing[i] <- !length(tmp[[i]])
    }
    missing_variables <- variables[missing]
    variables <- as.character(unique(unlist(tmp)))
  } else {
    missing_variables <- setdiff(variables, all_variables)
  }
  variables <- check_reserved_variables(variables)
  if (length(missing_variables)) {
    stop2("The following variables are missing in the draws object: ",
          comma(missing_variables))
  }
  variables
}

# check validity of new variables: e.g., that there are
# no duplicates in `variables` and that they do not use
# reserved words
check_new_variables <- function(variables) {
  # use anyDuplicated() for the check since it is faster than any(duplicated(x)) and
  # we shouldn't expect to take this branch often (since it is an error)
  if (anyDuplicated(variables)) {
    duplicates = unique(variables[duplicated(variables)])
    stop2(
      "Duplicate variable names are not allowed in draws objects.\n",
      "The following variable names are duplicates:\n",
      comma(duplicates)
    )
  }
  check_reserved_variables(variables)
}

# check variables do not make use of reserved words
check_reserved_variables <- function(variables) {
  assert_character(variables)
  used_meta_columns <- intersect(meta_columns(), variables)
  if (length(used_meta_columns)) {
    stop2("Variable names ", comma(used_meta_columns), " are reserved.")
  }
  variables
}

# check validity of iteration indices
# @param unique should the returned IDs be unique?
check_iteration_ids <- function(iteration_ids, x, unique = TRUE) {
  check_draws_object(x)
  if (is.null(iteration_ids)) {
    return(NULL)
  }
  unique <- as_one_logical(unique)
  iteration_ids <- as.integer(iteration_ids)
  if (unique) {
    iteration_ids <- unique(iteration_ids)
  }
  iteration_ids <- sort(iteration_ids)
  if (any(iteration_ids < 1L)) {
    stop2("Iteration indices should be positive.")
  }
  niterations <- niterations(x)
  max_iteration <- SW(max(iteration_ids))
  if (max_iteration > niterations) {
    stop2("Tried to subset iterations up to '", max_iteration, "' ",
          "but the object only has '", niterations, "' iterations.")
  }
  iteration_ids
}

# check validity of chain indices
# @param unique should the returned IDs be unique?
check_chain_ids <- function(chain_ids, x, unique = TRUE) {
  check_draws_object(x)
  if (is.null(chain_ids)) {
    return(NULL)
  }
  unique <- as_one_logical(unique)
  chain_ids <- as.integer(chain_ids)
  if (unique) {
    chain_ids <- unique(chain_ids)
  }
  chain_ids <- sort(chain_ids)
  if (any(chain_ids < 1L)) {
    stop2("Chain indices should be positive.")
  }
  nchains <- nchains(x)
  max_chain <- SW(max(chain_ids))
  if (max_chain > nchains) {
    stop2("Tried to subset chains up to '", max_chain, "' ",
          "but the object only has '", nchains, "' chains.")
  }
  chain_ids
}

# check validity of draw indices
# @param unique should the returned IDs be unique?
check_draw_ids <- function(draw_ids, x, unique = TRUE) {
  check_draws_object(x)
  if (is.null(draw_ids)) {
    return(NULL)
  }
  unique <- as_one_logical(unique)
  draw_ids <- as.integer(draw_ids)
  if (unique) {
    draw_ids <- unique(draw_ids)
  }
  draw_ids <- sort(draw_ids)
  if (any(draw_ids < 1L)) {
    stop2("Draw indices should be positive.")
  }
  ndraws <- ndraws(x)
  max_draw <- SW(max(draw_ids))
  if (max_draw > ndraws) {
    stop2("Tried to subset draws up to '", max_draw, "' ",
          "but the object only has '", ndraws, "' draws.")
  }
  draw_ids
}
