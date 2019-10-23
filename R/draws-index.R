#' Index Draws Objects
#'
#' Index variables, iterations, chains, and draws.
#'
#' @name draws-index
#' @param x An \R object.
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
variables.draws_data_frame <- function(x) {
  setdiff(names(x), meta_columns())
}

#' @export
variables.draws_list <- function(x) {
  names(x[[1]])
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
iterations.draws_data_frame <- function(x) {
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
chains.draws_data_frame <- function(x) {
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
draws.draws_data_frame <- function(x) {
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
nvariables.draws_data_frame <- function(x) {
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
niterations.draws_data_frame <- function(x) {
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
nchains.draws_data_frame <- function(x) {
  length(chains(x))
}

#' @export
nchains.draws_data_list <- function(x) {
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
ndraws.draws_data_frame <- function(x) {
  NROW(x)
}

#' @export
ndraws.draws_list <- function(x) {
  niterations(x) * nchains(x)
}

# check validity of variable names
check_variables <- function(variables, x) {
  check_draws_object(x)
  if (!is.null(variables)) {
    variables <- unique(as.character(variables))
    missing_variables <- setdiff(variables, variables(x))
    if (length(missing_variables)) {
      stop2("The following variables are missing in the draws object: ",
            comma(missing_variables))
    }
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
