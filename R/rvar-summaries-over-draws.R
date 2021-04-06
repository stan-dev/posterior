# Summaries within array elements, over draws  --------------------------

#' Summaries of random variables within array elements, over draws
#'
#' Compute expectations (`E()` or `mean()`), probabilities (`Pr()`),
#' medians (`median()`), and variances (`variance()`) from a random variable.
#'
#' Both `E()`, `mean()`, and `Pr()` take means over the draws dimension of the provided
#' random variable. `Pr()` additionally checks that the provided [`rvar`]
#' is a logical variable (hence, taking its expectation results in a probability).
#' `median()` takes medians, and `variance()` takes variances.
#'
#' For consistency, `E()` and `Pr()` are also defined for base arrays so that
#' they can be used as summary functions in `summarise_draws()`.
#'
#' @param x an [`rvar`]
#' @param ... further arguments passed to underlying functions (e.g., `base::mean()`
#' or `base::median()`), such as `na.rm`.
#'
#' @return
#' A numeric vector with the same dimensions as the given random variable, where
#' each entry in the vector is the mean, median, or variance of the corresponding entry in `x`.
#'
#' @examples
#'
#' set.seed(5678)
#' x = rdo(rnorm(4, mean = 1:4, sd = 2))
#'
#' # These should all be ~= c(1, 2, 3, 4)
#' E(x)
#' mean(x)
#' median(x)
#'
#' # This ...
#' Pr(x < 1.5)
#' # ... should be about the same as this:
#' pnorm(1.5, mean = 1:4, sd = 2)
#'
#' @name rvar-summaries-over-draws
#' @seealso [rvar-summaries-within-draws] for summary functions within draws.
#' [rvar-functions] for density, CDF, and quantile functions of random variables.
#' @family rvar-summaries
#' @export
E <- function(x, ...) {
  mean(x, ...)
}

#' @rdname rvar-summaries-over-draws
#' @export
mean.rvar <- function(x, ...) {
  summarise_rvar_by_element(x, mean, ...)
}

#' @rdname rvar-summaries-over-draws
#' @export
Pr <- function(x, ...) UseMethod("Pr")

#' @rdname rvar-summaries-over-draws
#' @export
Pr.default <- function(x, ...) {
  stop2("Can only use `Pr()` on logical variables.")
}

#' @rdname rvar-summaries-over-draws
#' @export
Pr.logical <- function(x, ...) {
  mean(x, ...)
}

#' @rdname rvar-summaries-over-draws
#' @export
Pr.rvar <- function(x, ...) {
  if (!is.logical(draws_of(x))) {
    stop2("Can only use `Pr()` on logical random variables.")
  }
  mean(x, ...)
}

#' @rdname rvar-summaries-over-draws
#' @export
median.rvar <- function(x, ...) {
  summarise_rvar_by_element(x, median, ...)
}

#' @importFrom distributional variance
#' @export
distributional::variance

#' @rdname rvar-summaries-over-draws
#' @export
variance.rvar <- function(x, ...) {
  summarise_rvar_by_element(x, var, ...)
}





# stuff to be converted ---------------------------------------------------------------

#' @export
is.finite.rvar <- function(x) rvar_apply_vec_fun(is.finite, x)
#' @export
is.infinite.rvar <- function(x) rvar_apply_vec_fun(is.infinite, x)
#' @export
is.nan.rvar <- function(x) rvar_apply_vec_fun(is.nan, x)
#' @export
is.na.rvar <- function(x) summarise_rvar_by_element(x, function(x) anyNA(x))
#' @export
anyNA.rvar <- function(x, ...) anyNA(draws_of(x, ...))

#' @rdname rvar-summaries-within-draws
#' @export
Summary.rvar <- function(..., na.rm = FALSE) {
  f <- get(.Generic)
  .Summary.rvar(f, ..., na.rm = na.rm)
}

#' @rdname rvar-summaries-within-draws
#' @export
range.rvar <- function(..., na.rm = FALSE) {
  .Summary.rvar(base::range, ..., na.rm = na.rm, transpose = TRUE)
}

.Summary.rvar <- function(f, ..., na.rm = FALSE, transpose = FALSE) {
  rvars <- lapply(list(...), function(arg) {
    arg <- as_rvar(arg)
    dim(arg) <- prod(dim(arg))
    arg
  })
  rvars <- conform_rvar_nchains(rvars)

  # bind all args into a single matrix of draws to perform the summary over
  all_draws <- draws_of(do.call(c, rvars))

  # perform summary
  .draws <- apply(all_draws, 1, f, na.rm = na.rm)

  if (transpose) {
    .draws <- t(.draws)
  }
  new_rvar(.draws, .nchains = nchains(rvars[[1]]))
}


