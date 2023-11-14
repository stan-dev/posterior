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
#'    \url{https://mc-stan.org/posterior/}
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
#' @section Package options:
#'
#' The following options are used to format and print [`draws`] objects,
#' as in `print.draws_array()`, `print.draws_df()`, `print.draws_list()`,
#' `print.draws_matrix()`, and `print.draws_rvars()`:
#'
#' * `posterior.max_draws`: Maximum number of draws to print.
#' * `posterior.max_iterations`: Maximum number of iterations to print.
#' * `posterior.max_chains`: Maximum number of chains to print.
#' * `posterior.max_variables`: Maximum number of variables to print.
#'
#' The following options are used for formatting the output of
#' [`summarize_draws`]:
#'
#' * `posterior.num_args`: Arguments passed to [num()][tibble::num]
#' for pretty printing of summaries.
#'
#' The following options are used to format and print [`rvar`] objects,
#' as in `print.rvar()` and `print.draws_rvars()`:
#'
#' * `posterior.rvar_summary`: What style of summary to display:
#' `"mean_sd"` displays `mean ± sd`, `"median_mad"` displays `median ± mad`.
#' * `posterior.digits`: How many significant digits are displayed. This
#' defaults to a smaller value (`2`) than `getOption("digits")` because
#' [`rvar`]s print two numbers (point summary and uncertainty) next to
#' each other.
#'
#' The following option is used to construct new [`rvar`] objects,
#' as in `rfun()` and `rdo()`:
#'
#' * `posterior.rvar_ndraws`: The number of draws used to construct
#' new random variables when this number cannot be determined
#' from existing arguments (e.g., other [`rvar`]s passed to a function).
#'
#' The following options are used to control warning messages:
#'
#' * `posterior.warn_on_merge_chains`: (logical) Some operations will
#' trigger an automatic merging of chains, for example, because chains do not
#' match between two objects involved in a binary operation. Whether this
#' causes a warning can be controlled by this option.
#'
NULL
