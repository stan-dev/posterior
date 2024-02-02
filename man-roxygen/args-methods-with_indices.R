#' @param with_indices (logical) Should indices be included in variable
#'   names? For example, if the object includes variables named `"x[1]"` and
#'   `"x[2]"`, if `TRUE`, `c("x[1]", "x[2]")` is returned; if `FALSE`, only `"x"`
#'   is returned. Defaults to `TRUE` for all formats except [draws_rvars()].
