#' @param dim Override for the dimensions of the [`rvar`] to be created (see [dim()]): an integer vector of length one
#' or more giving the maximal indices in each dimension. If `NULL` (the default), this is determined by the input.
#' **NOTE:** This argument controls the dimensions of the [`rvar`], not the underlying array, so you cannot
#' change the number of draws using this argument.
