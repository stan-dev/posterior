#' @return
#' If the input is an array,
#' returns a numeric vector with one element per quantile. If any of the draws is
#' non-finite, that is, `NA`, `NaN`, `Inf`, or `-Inf`, the returned output will
#' be a vector of (numeric) `NA` values. Also, if all draws of a variable are
#' the same (constant), the returned output will be a vector of (numeric) `NA`
#' values as well. The reason for the latter is that, for constant draws, we
#' cannot distinguish between variables that are supposed to be constant (e.g.,
#' a diagonal element of a correlation matrix is always 1) or variables that
#' just happened to be constant because of a failure of convergence or other
#' problems in the sampling process.
#'
#' If the input is an [`rvar`] and `length(probs) == 1`, returns an array of the
#' same dimensions as the [`rvar`], where each element is equal to the value
#' that would be returned by passing the draws array for that element of the
#' [`rvar`] to this function. If `length(probs) > 1`, the first dimension of the
#' result indexes the input probabilities; i.e. the result has dimension
#' `c(length(probs), dim(x))`.
