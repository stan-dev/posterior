#' Tools for working with posterior (and prior) distributions
#'
#' @docType package
#' @name posterior-package
#' @aliases posterior
#'
#' @import checkmate
#' @import stats
#'
#' @description
#' \if{html}{
#'    \figure{stanlogo.png}{options: width="50"}
#'    \url{https://mc-stan.org/posterior}
#' }
#'
#' The \pkg{posterior} package is intended to provide useful tools
#' for both users and developers of packages for fitting Bayesian models or
#' working with output from Bayesian models. The primary goals of the package
#' are to:
#' * Efficiently convert between many different useful formats of
#' draws (samples) from posterior or prior distributions.
#' * Provide consistent methods for operations commonly performed on draws,
#' for example, subsetting, binding, or mutating draws.
#' * Provide various summaries of draws in convenient formats.
#' * Provide lightweight implementations of state of the art posterior inference
#' diagnostics.
#'
NULL
