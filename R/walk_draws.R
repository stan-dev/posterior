#' Loop ("walk") over draws
#'
#' Executes an expression once for every draw in a `draws` object.
#'
#' @template args-methods-x
#' @param expr (expression) A bare expression that can contain references to
#' variables in `x` by name. This expression will be executed once per draw
#' of `x`, where references to variables in `x` resolve to the value of that
#' variable in that draw. The expression supports [quasiquotation].
#'
#' @details
#' If `x` is not in the [`draws_rvars`] format, it is first converted to that
#' format. This allows the variables in `x` to include their dimensions (i.e,
#' to act as R vectors and arrays) when being referred to in `expr`.
#'
#' Within `expr`, use `.draw` to refer to the draw index, which will be a value
#' between 1 and `ndraws(x)`.
#' @importFrom rlang eval_tidy
#' @export
walk_draws <- function(x, expr) {
  x <- as_draws_rvars(x)
  expr <- enquo(expr)

  for (i in seq_len(ndraws(x))) {
    variables <- lapply(x, function(variable) {
      draws <- draws_of(variable)[i, ]

      # need to restore dimensions in case subsetting
      # dropped any other than the first ...
      if (identical(dim(variable), 1L)) {
        # ... but don't do this for length-1 arrays, which we'll leave
        # as is so they are treated as scalars
        draws <- drop(draws)
      } else {
        dim(draws) <- dim(variable)
      }

      draws
    })
    variables$.draw = i
    eval_tidy(expr, data = variables)
  }
}
