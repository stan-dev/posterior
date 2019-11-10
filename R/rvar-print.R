#' Print or format a random variable
#'
#' Printing and formatting methods for random variables of arbitrary objects
#'
#' @param x A list or vector where each entry represents a draw from a distribution
#' @param color Whether or not to use color when formatting the output
#'
#' @details The `"rvar"` class represents random variables of arbitrary objects.
#'
#' @return An object of class `"rvar"` representing a random variable.
#'
#' @export
print.rvar = function(x, ...) {
  cat0("rvar<", ndraws(x), ">:\n")
  print(format(x, color = FALSE), quote = FALSE)
  invisible(x)
}

#' @rdname print.rvar
#' @export
format.rvar = function(x, ..., color = FALSE) {
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

#' @rdname print.rvar
#' @export
type_sum.rvar <- function(x) {
  "rvar"
}

#' @rdname print.rvar
#' @export
pillar_shaft.rvar <- function(x, ...) {
  out = format(x, color = TRUE)
  pillar::new_pillar_shaft_simple(out, align = "right")
}


# formatting helpers ------------------------------------------------------

format_mean = function(x, color = FALSE) {
  format(x, justify = "right", digits = 2, scientific = 2)
}

format_sd = function(x, color = FALSE) {
  sd_string = paste0("±", format(x, justify = "right", digits = 2, scientific = 2))
  if (color) {
    pillar::style_subtle(sd_string)
  } else {
    sd_string
  }
}

format_mean_sd = function(.mean, .sd, color = FALSE) {
  paste0(format_mean(.mean, color = color), format_sd(.sd, color = color))
}
