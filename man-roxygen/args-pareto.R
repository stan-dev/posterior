#' @param x (multiple options) One of:
#'  - A matrix of draws for a single variable (iterations x chains). See
#'    [extract_variable_matrix()].
#'  - An [`rvar`].
#' @param tail (string) The tail to diagnose/smooth:
#'   * `"right"`: diagnose/smooth only the right (upper) tail
#'   * `"left"`: diagnose/smooth only the left (lower) tail
#'   * `"both"`: diagnose/smooth both tails and return the maximum k-hat value
#'
#' The default is `"both"`.
#' @param ndraws_tail (numeric) number of draws for the tail. If
#'   `ndraws_tail` is not specified, it will be calculated as
#'   ceiling(3 * sqrt(length(x) / r_eff)) if length(x) > 225 and
#'   length(x) / 5 otherwise (see Appendix H in Vehtari et al. (2022)).
#' @param r_eff (numeric) relative effective sample size estimate. If
#'   `r_eff` is omitted, it will be calculated assuming the draws are
#'   from MCMC.
#' @param verbose (logical) Should diagnostic messages be printed? If
#'   `TRUE`, messages related to Pareto diagnostics will be
#'   printed. Default is `FALSE`.
