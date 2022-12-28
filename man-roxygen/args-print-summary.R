#' @param summary (string) The style of summary to display:
#'   - `"mean_sd"` displays `mean ± sd`
#'   - `"median_mad"` displays `median ± mad`
#'   - `"mode_entropy"` displays `mode <entropy>`, and is used automatically for
#'     [`rvar_factor`]s. It shows normalized entropy, which ranges from
#'     0 (all probability in one category) to 1 (uniform). See [entropy()].
#'   - `"mode_dissent"` displays `mode <dissent>`, and is used automatically for
#'     [`rvar_ordered`]s. It shows Tastle and Wierman's (2007) *dissention*
#'     measure, which ranges from 0 (all probability in one category) through
#'     0.5 (uniform) to 1 (bimodal: all probability split equally between the
#'     first and last category). See [dissent()].
#'   - `NULL` uses `getOption("posterior.rvar_summary")` (default `"mean_sd`)
