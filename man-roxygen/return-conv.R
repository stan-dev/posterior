#' @return
#' If the input is an array,
#' returns a single numeric value. If any of the draws is non-finite, that is, `NA`,
#' `NaN`, `Inf`, or `-Inf`, the returned output will be (numeric) `NA`. Also, if
#' all draws of a variable are the same (constant), the returned output will be
#' (numeric) `NA` as well. The reason for the latter is that, for constant
#' draws, we cannot distinguish between variables that are supposed to be
#' constant (e.g.,  a diagonal element of a correlation matrix is always 1) or
#' variables that just happened to be constant because of a failure of
#' convergence or other problems in the sampling process.
#'
#' If the input is an [`rvar`], returns an array of the same dimensions as the
#' [`rvar`], where each element is equal to the value that would be returned by
#' passing the draws array for that element of the [`rvar`] to this function.

