#' Index Draws Objects
#'
#' Index variables, iterations, chains, and draws.
#'
#' @name draws-index
#' @template args-methods-x
#' @param value For `variables(x) <- value`, a character vector of new variable names.
#'
#' @details
#' The methods `variables()`, `iterations()`, `chains()`, and `draws()` return
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
#' iterations(x)
#' niterations(x)
#'
#' chains(x)
#' nchains(x)
#'
#' draws(x)
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
iterations <- function(x) {
  UseMethod("iterations")
}

#' @export
iterations.draws_matrix <- function(x) {
  as.integer(rownames(x))
}

#' @export
iterations.draws_array <- function(x) {
  as.integer(rownames(x))
}

#' @export
iterations.draws_df <- function(x) {
  as.integer(unique(x$.iteration))
}

#' @export
iterations.draws_list <- function(x) {
  seq_along(x[[1]][[1]])
}

#' @rdname draws-index
#' @export
chains <- function(x) {
  UseMethod("chains")
}

#' @export
chains.draws_matrix <- function(x) {
  1L
}

#' @export
chains.draws_array <- function(x) {
  as.integer(colnames(x))
}

#' @export
chains.draws_df <- function(x) {
  as.integer(unique(x$.chain))
}

#' @export
chains.draws_list <- function(x) {
  as.integer(names(x))
}

#' @rdname draws-index
#' @export
draws <- function(x) {
  UseMethod("draws")
}

#' @export
draws.draws_matrix <- function(x) {
  as.integer(rownames(x))
}

#' @export
draws.draws_array <- function(x) {
  iterations <- iterations(x)
  niterations <- niterations(x)
  chains <- chains(x)
  ulapply(chains, function(c) niterations * (c - 1) + iterations)
}

#' @export
draws.draws_df <- function(x) {
  as.integer(unique(x$.draw))
}

#' @export
draws.draws_list <- function(x) {
  iterations <- iterations(x)
  niterations <- niterations(x)
  chains <- chains(x)
  ulapply(chains, function(c) niterations * (c - 1) + iterations)
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
  length(iterations(x))
}

#' @export
niterations.draws_list <- function(x) {
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
  length(chains(x))
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
check_existing_variables <- function(variables, x) {
  check_draws_object(x)
  if (!is.null(variables)) {
    variables <- unique(as.character(variables))
    variables <- check_reserved_variables(variables)
    missing_variables <- setdiff(variables, variables(x))
    if (length(missing_variables)) {
      stop2("The following variables are missing in the draws object: ",
            comma(missing_variables))
    }
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
check_iterations <- function(iterations, x) {
  check_draws_object(x)
  if (!is.null(iterations)) {
    iterations <- sort(unique(as.integer(iterations)))
    if (any(iterations < 1L)) {
      stop2("Iteration indices should be positive.")
    }
    niterations <- niterations(x)
    max_iterations <- max(iterations)
    if (max_iterations > niterations) {
      stop2("Tried to subset iterations up to '", max_iterations, "' ",
            "but the object only has '", niterations, "' iterations.")
    }
  }
  iterations
}

# check validity of chain indices
check_chains <- function(chains, x) {
  check_draws_object(x)
  if (!is.null(chains)) {
    chains <- sort(unique(as.integer(chains)))
    if (any(chains < 1L)) {
      stop2("Chain indices should be positive.")
    }
    nchains <- nchains(x)
    max_chains <- max(chains)
    if (max_chains > nchains) {
      stop2("Tried to subset chains up to '", max_chains, "' ",
            "but the object only has '", nchains, "' chains.")
    }
  }
  chains
}

# check validity of draw indices
check_draws <- function(draws, x) {
  check_draws_object(x)
  if (!is.null(draws)) {
    draws <- sort(unique(as.integer(draws)))
    if (any(draws < 1L)) {
      stop2("Draw indices should be positive.")
    }
    ndraws <- ndraws(x)
    max_draws <- max(draws)
    if (max_draws > ndraws) {
      stop2("Tried to subset draws up to '", max_draws, "' ",
            "but the object only has '", ndraws, "' draws.")
    }
  }
  draws
}
