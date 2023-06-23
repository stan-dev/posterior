#' @export
length.rvar <- function(x) {
  prod(dim(x))
}

#' @export
dim.rvar <- function(x) {
  dim(draws_of(x))[-1]
}

#' @export
`dim<-.rvar` <- function(x, value) {
  if (length(value) == 0) {
    # vectors have NULL dim; for us that means
    # dim of c(ndraws(x), length(x))
    value = length(x)
  }
  # must keep old rowname around and restore them, since changing dim will drop them
  old_rownames <- rownames(draws_of(x))
  dim(draws_of(x)) <- c(ndraws(x), value)
  rownames(draws_of(x)) <- old_rownames
  x
}

#' @export
dimnames.rvar <- function(x) {
  .dimnames <- dimnames(draws_of(x))
  .dimnames[-1]
}

#' @export
`dimnames<-.rvar` <- function(x, value) {
  .draws <- draws_of(x)

  dimnames(.draws) <- c(list(dimnames(.draws)[[1]]), value)

  draws_of(x) <- .draws
  x
}

#' @export
names.rvar <- function(x) {
  .dimnames <- dimnames(draws_of(x))
  .dimnames[[2]]
}

#' @export
`names<-.rvar` <- function(x, value) {
  dimnames(draws_of(x))[2] <- list(value)
  x
}

#' @importFrom methods setGeneric
#' @export
setGeneric("drop")

#' Drop redundant dimensions
#'
#' Delete the dimensions of an [`rvar`] which are of size one. See [`base::drop()`]
#'
#' @param x (rvar) an [`rvar`].
#'
#' @return
#' An [`rvar`] with the same length as `x`, but where any entry equal to `1`
#' in `dim(x)` has been removed. The exception is if `dim(x) == 1`, in which
#' case `dim(drop(x)) == 1` as well (this is because [`rvar`]s, unlike [`numeric`]s,
#' never have `NULL` dimensions).
#'
#' @examples
#' # Sigma is a 3x3 covariance matrix
#' Sigma <- as_draws_rvars(example_draws("multi_normal"))$Sigma
#' Sigma
#'
#' Sigma[1, ]
#'
#' drop(Sigma[1, ])
#'
#' # equivalently ...
#' Sigma[1, drop = TRUE]
#'
#' @importFrom methods setMethod
#' @export
setMethod("drop", signature(x = "rvar"), function(x) {
  .dim <- dim(x)

  if (length(.dim) > 1) {
    # with exactly 1 dimension left we don't want to drop anything
    # (otherwise names get lost), so only do this with > 1 dimension
    keep_dim <- .dim != 1
    .dimnames <- dimnames(x)
    dim(x) <- .dim[keep_dim]
    # for comparison / testing, ensure if no dimnames have names that we
    # actually have those names be NULL (rather than just empty strings)
    new_dimnames <- .dimnames[keep_dim]
    if (all(names(new_dimnames) == "")) names(new_dimnames) <- NULL
    dimnames(x) <- new_dimnames
  }

  x
})
