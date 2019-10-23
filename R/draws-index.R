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

check_variables <- function(variables, x) {
  check_draws_object(x)
  if (!is.null(variables)) {
    variables <- as.character(variables)
    if (any(duplicated(variables))) {
      stop2("Subsetted variables should be unique.")
    }
    missing_variables <- setdiff(variables, variables(x))
    if (length(missing_variables)) {
      stop2("The following variables are missing in the draws object: ",
            comma(missing_variables))
    }
  }
  variables
}

check_iterations <- function(iterations, x) {
  check_draws_object(x)
  if (!is.null(iterations)) {
    iterations <- as.integer(iterations)
    # TODO: allow for non-unique subsetting?
    if (any(duplicated(iterations))) {
      stop2("Subsetted iterations should be unique.")
    }
    niterations <- niterations(x)
    oos_iterations <-
      iterations[iterations > niterations | iterations < - niterations]
    if (length(oos_iterations)) {
      stop2("Tried to subset iterations up to '", max(abs(oos_iterations)), "' ",
            "but the object only has '", niterations, "' iterations.")
    }
  }
  iterations
}

check_chains <- function(chains, x) {
  check_draws_object(x)
  if (!is.null(chains)) {
    chains <- as.integer(chains)
    if (any(duplicated(chains))) {
      stop2("Subsetted chains should be unique.")
    }
    nchains <- nchains(x)
    oos_chains <- chains[chains > nchains | chains < - nchains]
    if (length(oos_chains)) {
      stop2("Tried to subset chains up to '", max(abs(oos_chains)), "' ",
            "but the object only has '", nchains, "' chains.")
    }
  }
  chains
}

check_draws <- function(draws, x) {
  check_draws_object(x)
  if (!is.null(draws)) {
    draws <- as.integer(draws)
    if (any(duplicated(draws))) {
      stop2("Subsetted draws should be unique.")
    }
    ndraws <- ndraws(x)
    oos_draws <- draws[draws > ndraws | draws < - ndraws]
    if (length(oos_draws)) {
      stop2("Tried to subset draws up to '", max(abs(oos_draws)), "' ",
            "but the object only has '", ndraws, "' draws.")
    }
  }
  draws
}
