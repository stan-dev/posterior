#' @return
#' A numeric vector with one element per quantile. If any of the draws is
#' non-finite, that is, `NA`, `NaN`, `Inf`, or `-Inf`, the returned output will
#' be a vector of (numeric) `NA` values. Also, if all draws of a variable are
#' the same, that is, if draws are constant, the returned output will be a
#' vector of (numeric) `NA` values as well. The reason for the latter is that,
#' for constant draws, we cannot distinguish between variables that are
#' supposed to be constant (e.g., for a diagonal element of a correlation
#' matrix, which will always be 1) or variables that just happened to be
#' constant because of a failure of convergence or other problems in the
#' sampling process.
