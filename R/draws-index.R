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
