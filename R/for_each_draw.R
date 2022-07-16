#' Loop over draws
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
#' between 1 and `ndraws(x)`. `expr` is executed in the calling environment of
#' `for_each_draw()`, so it can use variables in that environment (however, due
#' to the use of data masking, to modify variables in that environment, one
#' must use `<<-`.)
#' @importFrom rlang eval_tidy
#' @export
for_each_draw <- function(x, expr) {
  x <- as_draws_rvars(x)
  expr <- enquo(expr)
  env <- parent.frame()

  for (i in seq_len(ndraws(x))) {
    variables <- lapply(x, function(variable) {
      draws <- draws_of(variable)
      .dim <- dim(variable)
      ndim <- length(.dim)

      if (ndim <= 1) {
        # treat 0- and 1-dimensional arrays as vectors
        draws <- draws[i, ]
        dim(draws) <- NULL
        names(draws) <- names(variable)
      } else {
        dim(draws) <- c(NROW(draws), length(variable))
        draws <- draws[i, ]
        dim(draws) <- .dim
        dimnames(draws) <- dimnames(variable)
      }

      draws
    })
    variables$.draw = i
    eval_tidy(expr, data = variables, env = env)
  }
}
