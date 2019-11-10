#' Print or format a random variable
#'
#' Printing and formatting methods for random variables of arbitrary objects
#'
#' @param x A list or vector where each entry represents a draw from a distribution
#' @param color Whether or not to use color when formatting the output
#'
#' @details The `"rvar_array2"` class represents random variables of arbitrary objects.
#'
#' @return An object of class `"rvar_array2"` representing a random variable.
#'
#' @export
print.rvar_array2 = function(x, ...) {
  cat0("rvar_array2<", ndraws(x), ">:\n")
  print(format(x, color = FALSE), quote = FALSE)
  invisible(x)
}

#' @rdname print.rvar_array2
#' @export
format.rvar_array2 = function(x, ..., color = FALSE) {
  draws = field(x, 1)

  if (is.numeric(draws) || is.logical(draws)) {
    summary_dimensions = seq_len(length(dim(draws)) - 1)

    .mean = apply(draws, summary_dimensions, mean)
    .sd = apply(draws, summary_dimensions, sd)
    out = format_mean_sd(.mean, .sd, color = color)

    dim(out) = dim(draws)[summary_dimensions]
    dimnames(out) = dimnames(draws)[summary_dimensions]
    out
  } else {
    pillar::obj_sum(draws[[1]])
  }
}

#' @rdname print.rvar_array2
#' @export
type_sum.rvar_array2 <- function(x) {
  "rvar_array2"
}

#' @rdname print.rvar_array2
#' @export
pillar_shaft.rvar_array2 <- function(x, ...) {
  out = format(x, color = TRUE)
  pillar::new_pillar_shaft_simple(out, align = "right")
}
