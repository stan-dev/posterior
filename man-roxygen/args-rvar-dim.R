#' @param dim (integer vector) One or more integers giving the maximal indices
#'   in each dimension to override the dimensions of the [`rvar`] to be created
#'   (see [dim()]). If `NULL` (the default), `dim` is determined by the input.
#'   **NOTE:** This argument controls the dimensions of the [`rvar`], not the
#'   underlying array, so you cannot change the number of draws using this
#'   argument.
