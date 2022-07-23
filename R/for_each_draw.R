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
#'
#' @examples
#' eight_schools <- as_draws_rvars(example_draws())
#'
#' plot(1, type = "n",
#'   xlim = c(1, length(eight_schools$theta)),
#'   ylim = range(range(eight_schools$theta)),
#'   xlab = "school", ylab = "theta"
#' )
#'
#' # use for_each_draw() to make a parallel coordinates plot of all draws
#' # of eight_schools$theta. Use resample_draws(eight_schools, n = ...)
#' # in place of eight_schools if a smaller sample is desired for the plot.
#' for_each_draw(eight_schools, {
#'   lines(seq_along(theta), theta, col = rgb(1, 0, 0, 0.05))
#' })
#'
#' # add means and 90% intervals
#' lines(seq_along(eight_schools$theta), mean(eight_schools$theta))
#' with(summarise_draws(eight_schools$theta),
#'   segments(seq_along(eight_schools$theta), y0 = q5, y1 = q95)
#' )
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
