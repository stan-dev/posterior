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
#'   `ndraws_tail` is not specified, it is calculated with
#'   [ps_tail_length()].
#' @param r_eff Ignored. A value other than `NULL` gives a warning.
#'   Default is `NULL`.
#' @param verbose (logical) Should diagnostic messages be printed? If
#'   `TRUE`, messages related to Pareto diagnostics will be
#'   printed. Default is `FALSE`.
#' @param are_log_weights (logical) Are the draws log weights? Default is
#'   `FALSE`. If `TRUE` computation will take into account that the
#'   draws are log weights, and only right tail will be smoothed.
