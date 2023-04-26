#' @param x (multiple options) One of:
#'  - A matrix of draws for a single variable (iterations x chains). See
#'    [extract_variable_matrix()].
#'  - An [`rvar`].
#' @param tail (string) The tail to smooth:
#'   * `"right"`: smooth only the right (upper) tail
#'   * `"left"`: smooth only the left (lower) tail
#'   * `"both"`: smooth both tails and return the maximum k value
#'
#' The default is `"both"`.
#' @param ndraws_tail (numeric) number of draws for the tail. If
#'   `ndraws_tail` is not specified, it will be calculated as
#'   max(ceiling(3 * sqrt(length(x))), length(x) / 5).
#' @param r_eff (numeric) relative effective sample size estimate. If
#'   `r_eff` is omitted, it will be calculated assuming the draws are
#'   from MCMC.
#' @param verbose (logical) Should diagnostic messages be printed? If
#'   `TRUE`, messages related to Pareto smoothing diagnostics will be
#'   printed. Default is `FALSE`.
#' @param extra_diags (logical) Should extra Pareto smoothing
#'   diagnostics be included in output? If `TRUE`, `min_ss`,
#'   `khat_threshold` and `convergence_rate` for the calculated k
#'   value will be returned. Default is `FALSE`.
