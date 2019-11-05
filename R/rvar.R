#' Random variables
#'
#' Random variables of arbitrary objects
#'
#' @name rvar
#'
#' @param x A list or vector where each entry represents a draw from a distribution
#'
#' @details The `"rvar"` class represents random variables of arbitrary objects.
#'
#' @return An object of class `"rvar"` representing a random variable.
#'
NULL

#' @rdname rvar
#' @export
rvar = function(x) {
  structure(list(
    draws = x
  ),
    class = "rvar"
  )
}

#' @export
is_rvar = function(x) {
  inherits(x, "rvar")
}


# length and dimensions ---------------------------------------------------

#' @export
length.rvar = function(x) {
  length(x$draws[[1]])
}

#' @export
dim.rvar = function(x) {
  dim(x$draws[[1]])
}

#' @export
`dim<-.rvar` = function(x, value) {
  for (i in seq_along(x$draws)) {
    dim(x$draws[[i]]) <- value
  }
  x
}

#' @export
dimnames.rvar = function(x) {
  dimnames(x$draws[[1]])
}

#' @export
`dimnames<-.rvar` = function(x, value) {
  for (i in seq_along(x$draws)) {
    dimnames(x$draws[[i]]) <- value
  }
  x
}

#' @export
is.matrix.rvar = function(x) {
  is.matrix(x$draws[[1]])
}


# indexing ----------------------------------------------------------------

#' @export
`[[.rvar` = function(x, i) {
  rvar(lapply(x$draws, `[[`, i))
}

#' @export
`[.rvar` = function(x, ...) {
  rvar(lapply(x$draws, `[`, ...))
}




# chain / iteration / draw info -------------------------------------------

ndraws.rvar = function(x) {
  length(x$draws)
}

