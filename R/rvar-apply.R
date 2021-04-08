#' Random variable resulting from a function applied over margins of an array or random variable
#'
#' Returns an [`rvar`] obtained by applying a function to margins of an array or [`rvar`].
#' Acts like `apply()`, except that the function supplied (`.f`) should return an [`rvar`],
#' and the final result is always an [`rvar`].
#'
#' @param .x An array or an [`rvar`].
#' @param .margin A vector giving the subscripts which the function will be applied over.
#' E.g., for a matrix `1` indicates rows, `2` indicates columns, `c(1, 2)` indicates rows
#' and columns. If `.x` has named dimensions, this can be a character vector selecting
#' dimension names.
#' @param .f the function to be applied. This must be a function that returns an [`rvar`].
#' The dimensions of the result of `.f` applied to each margin of `.x` must be the same as
#' each other, otherwise the resulting [`rvar`] cannot be simplified. See 'Details'.
#' @param ... optional arguments to `.f`.
#'
#' @details
#'
#' This function acts much like `apply()`, except that the function passed to it (`.f`)
#' must return [`rvar`]s, and the result is simplified into an [`rvar`]. Unlike
#' `apply()`, it also keeps the dimensions of the returned values along each margin,
#' rather than simplifying each margin to a vector.
#'
#' If you wish to apply functions over [`rvar`]s where the result is not intended to
#' be simplified into an [`rvar`], you can use the standard `apply()`, `lapply()`,
#' `sapply()`, or `vapply()` functions.
#'
#' @seealso [as_rvar()] to convert objects to `rvar`s. See [rdo()], [rfun()], and
#' [rvar_rng()] for higher-level interfaces for creating `rvar`s.
#'
#' @return An [`rvar`].
#'
#' If each call to `.f` returns an [`rvar`] of dimension `d`, then `rvar_apply()`
#' returns an [`rvar`] of dimension `c(d, dim(.x)[.margin])` if `d != 1`.
#' If `d` equals `1`, `rvar_apply()` returns an [`rvar`] of dimension `dim(.x)[.margin]`
#' otherwise. If `d` is `0`, the result has length `0` but not necessarily the 'correct' dimension.
#'
#' The calls to `.f` must return [`rvar`]s of the same shape; otherwise `rvar_apply()`
#' returns an error.
#'
#' @examples
#'
#' set.seed(3456)
#' x <- rvar_rng(rnorm, 24, mean = 1:24)
#' dim(x) <- c(2,3,4)
#'
#' # we can find the distributions of marginal means of the above array
#' # using rvar_mean along with rvar_apply
#' rvar_apply(x, 1, rvar_mean)
#' rvar_apply(x, 2:3, rvar_mean)
#'
#' @export
rvar_apply <- function(.x, .margin, .f, ...) {
  # this should return a list of rvars
  out <- apply(.x, .margin, .f, ...)
  if (!is.list(out) || !is_rvar(out[[1]])) {
    stop2("The function passed to rvar_apply() must return rvars.")
  }

  # get the dimensions of the results in each cell
  check_same_fun_output(out, dim)
  cell_dim <- dim(out[[1]])

  # get the dimensions from the original array that are being kept
  original_dim <- dim(out) %||% length(out)

  # bind the output into an rvar and move the original dimensions (which are
  # currently flattened into a single dimension at the end) to the front
  # (this could probably be more efficient)
  out <- bind_rvars(out, rep("", length(out)), axis = length(cell_dim) + 1)
  n_dim <- length(dim(out))
  out <- aperm(out, c(n_dim, seq_len(n_dim - 1)))

  # restore the shape of the original dimensions
  if (isTRUE(cell_dim == 1)) {
    # if the dimensions of the value in each cell are 1, we won't add it as
    # a new dimension
    dim(out) <- original_dim
  } else {
    dim(out) <- c(original_dim, cell_dim)
  }
  dimnames(out)[seq_along(original_dim)] <- dimnames(.x)[.margin]

  out
}
