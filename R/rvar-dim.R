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
  dimnames(draws_of(x)) <- c(list(rownames(draws_of(x))), value)
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
