#' @param summary (string) The style of summary to display:
#'   - `"mean_sd"` displays `mean ± sd`
#'   - `"median_mad"` displays `median ± mad`
#'   - `"mode_entropy"` displays `mode ± entropy`, and is used automatically for
#'     factor-like [`rvar`]s
#'   - `NULL` uses `getOption("posterior.rvar_summary")` (default `"mean_sd`)
