#' Random variable resulting from a function applied over margins of an array or random variable
#'
#' Returns an [`rvar`] obtained by applying a function to margins of an array or [`rvar`].
#' Acts like `apply()`, except that the function supplied (`.f`) should return an [`rvar`],
#' and the final result is always an [`rvar`].
#'
#' @param .x An array or an [`rvar`].
#' @param .margin (multiple options) The subscripts which the function will be applied over:
#'   * An integer vector. E.g., for a matrix `1` indicates rows, `2` indicates
#'     columns, `c(1, 2)` indicates rows and columns.
#'   * A character vector of dimension names if `.x` has named dimensions.
#' @param .f (function) The function to be applied. The function `.f` must
#'   return an [`rvar`] and the dimensions of the result of `.f` applied to each
#'   margin of `.x` must be able to be broadcasted to a common shape (otherwise
#'   the resulting [`rvar`] cannot be simplified). See **Details**.
#' @param ... Optional arguments passed to `.f`.
#'
#' @details
#'
#' This function acts much like `apply()`, except that the function passed to it (`.f`)
#' must return [`rvar`]s, and the result is simplified into an [`rvar`]. Unlike
#' `apply()`, it also keeps the dimensions of the returned values along each margin,
#' rather than simplifying each margin to a vector, and if the results of `.f` do
#' not all have the same dimensions, it applies the [`rvar`] broadcasting rules to
#' bind results together rather than using vector recycling.
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
#' If the result of each call to `.f` returns an [`rvar`] of dimension `d` after
#' being broadcast to a common shape, then `rvar_apply()` returns an [`rvar`] of
#' dimension `c(d, dim(.x)[.margin])`. If the last dimension of the result would
#' be `1`, it is dropped (other dimensions equal to `1` are retained). If `d` is
#' `0`, the result has length `0` but not necessarily the 'correct' dimension.
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
  rvar_list <- apply(.x, .margin, .f, ...)
  if (!is.list(rvar_list) || !all(sapply(rvar_list, is_rvar))) {
    stop_no_call("The function passed to rvar_apply() must return rvars.")
  }

  # get the dimensions of the results in each cell
  cell_dim <- dim(rvar_list[[1]])
  # get the dimensions from the original array that are being kept
  marginal_dim <- dim(rvar_list) %||% length(rvar_list)

  # bind the output into an rvar along the first dimension (which the
  # marginal dimensions flattened into a single dim)
  out <- rvar_list[[1]]
  .dimnames <- dimnames(out)
  dim(out) <- c(1, dim(out))
  if (length(out)) dimnames(out) <- c(list(NULL), .dimnames)
  # process remaining rvars in succession, binding them to the output
  for (i in seq_along(rvar_list)[-1]) {
    rvar_i <- rvar_list[[i]]
    .dimnames <- dimnames(rvar_i)
    dim(rvar_i) <- c(1, dim(rvar_i))
    if (length(rvar_i)) dimnames(rvar_i) <- c(list(NULL), .dimnames)
    out <- broadcast_and_bind_rvars(out, rvar_i, 1)
  }

  if (length(out) > 0) {
    # restore the shape of the marginal dimensions
    .dimnames <- dimnames(out)
    dim(out) <- c(marginal_dim, dim(out)[-1])
    dimnames(out) <- c(rep(list(NULL), length(marginal_dim)), .dimnames[-1])
    # if the last dimension is 1, drop it
    n_dim <- length(dim(out))
    if (dim(out)[[n_dim]] == 1) {
      dim(out) <- dim(out)[-n_dim]
    }
    # restore marginal dimnames
    marginal_dim_i <- seq_along(marginal_dim)
    out <- copy_dimnames(.x, .margin, out, marginal_dim_i)
  }

  out
}
